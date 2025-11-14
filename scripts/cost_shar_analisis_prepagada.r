# ////////////////////////////////////////////////////////////
# Script: cost_shar_analisis_prepagada.R
# Author: Javier Amaya-Nieto
# Date: 19-March-2025
# Description: Use ECVN-DANE to check continuity on 5 MMW for the percentage 
# Of people with prepagada insurance and complementario insurance
#//////////////////////////////////////////////////////////////



# ////////////////////////////////////////////////////////////
# Variables in ECVE workforce - person level dataset 
# P8624 <- income

# ////////////////////////////////////////////////////////////
# Variables in ECVE health - person level dataset 
# P799S2 - prepagada insurance
# P799S3 - complementary insurance


# Environment -------------------------------------------------------------

# Cleaning the environment
rm(list = ls())
cat("\f")

# Loading the packages required for the analysis
library(pacman)
p_load(rio, tidyverse, dplyr, grid, readxl, janitor, readr, skimr, rdrobust) 


# Data paths
data_path <- "../data"
results_path <- "../results"



# Importing raw data ------------------------------------------------------

# Importing data - household characteristics and composition (person level)
comp_hogar <- read_delim(file.path(data_path,'caracteristicas_hogar_2018.CSV'),
                         delim = ";",
                         escape_double = FALSE,  
                         trim_ws = TRUE
                         ,
                         col_select = c('DIRECTORIO', 'SECUENCIA_ENCUESTA','SECUENCIA_P',
                                        'P6020', # sex
                                        'P6040', # age
                         ))
# renaming variables
comp_hogar <- comp_hogar %>% rename(sexo = P6020, age = P6040)

#health dataset
salud <- read_delim(file.path(data_path,'Salud_2018.CSV'),
                    delim = ";",
                    escape_double = FALSE,  
                    trim_ws = TRUE
                    ,
                    col_select = c('DIRECTORIO', 'SECUENCIA_ENCUESTA','SECUENCIA_P',
                                   'P799s2', # prepagada insurance yes/no
                                   'P799s3' # complementary plan yes/no
                    ))
#renaming variables
salud <- salud %>% rename(prepa = P799s2, comp = P799s3)


#workforce dataset
# Importing data - workforce (person level)
labor <- read_delim(file.path(data_path,'Fuerza_trabajo_2018.CSV'),
                    delim = ";",
                    escape_double = FALSE,  
                    trim_ws = TRUE
                    ,
                    col_select = c('DIRECTORIO', 'SECUENCIA_ENCUESTA','SECUENCIA_P',
                                   'P8624', "FEX_C" # income
                    )) 

# Filtering to people with income
labor <- labor %>% filter(P8624 > 0)

# Renaming variables
labor <- labor %>% rename(income = P8624)



# Merging datasets required -----------------------------------------

# Merging datasets leaving only variables in x
df_rd <- merge(labor, comp_hogar, by = c('DIRECTORIO', 'SECUENCIA_ENCUESTA','SECUENCIA_P'), all.x = TRUE)
#summary(df_rd$age)

df_rd <- merge(df_rd, salud, by = c('DIRECTORIO', 'SECUENCIA_ENCUESTA','SECUENCIA_P'), all.x = TRUE)
# table(df_rd$prepa)
# table(df_rd$comp)


# Data cleaning ------------------------------------------------------
# creating a variable with the mmw for 2018
mmw_2018 <- 781242

df <- df_rd

# creating the variable of mmw
df$mmw <- df$income / mmw_2018
summary(df$mmw)

# Filter the data between 3 and 8 in the mmw variable
df_rd_filtered <- df[df$mmw >= 3 & df$mmw <= 8, ]

# preparing variables for RD
# df_rd_filtered <- df_rd_filtered %>%
#   mutate(prepa_fixed = 100 * ifelse(prepa == 2, 0, ifelse(prepa == 1, 1, NA)), 
#          comple_fixed = 100 * ifelse(comp == 2, 0, ifelse(comp == 1, 1, NA)))

df_rd_filtered <- df_rd_filtered %>% 
  dplyr::mutate(
    # Limpieza básica de las dos originales
    prepa_bin = ifelse(prepa == 1, 1,
                       ifelse(prepa == 2, 0, NA)),
    comp_bin  = ifelse(comp  == 1, 1,
                       ifelse(comp  == 2, 0, NA)),
    
    # NUEVA variable: tiene prepagada o complementario
    prepa_fixed = case_when(
      prepa_bin == 1 | comp_bin == 1 ~ 100,   # tiene al menos uno
      prepa_bin == 0 & comp_bin == 0 ~ 0,   # no tiene ninguno
      TRUE ~ NA_real_                      # faltantes / no sabe / no informa
    )
  )


# Regression discontinuity -------------------------------------------------

## Prepagada insurance -----------------------------------------------------

# Run the regression discontinuity with the optimal bandwidth on each side
resultado_prepagada <- rdrobust(
  y = df_rd_filtered$prepa_fixed,  # Outcome variable
  x = df_rd_filtered$mmw,        # Assignment variable
  weights = df_rd_filtered$FEX_C, # usando el factor de expansión
  c = 5,                # Threshold
  bwselect = "msetwo",  # Method to select the bandwidth
  masspoints = "off"    # Disable mass points
)

# View the summary of the results
summary(resultado_prepagada)

# extracting results
estimate_p <- resultado_prepagada$Estimate[1]
ci_p_low <- resultado_prepagada$ci[3,1]
ci_p_upp <- resultado_prepagada$ci[3,2]
pvalue_p <- resultado_prepagada$pv[3]

# Export the regression results
salida_regresion_p <- capture.output(summary(resultado_prepagada))
writeLines(salida_regresion_p, file.path(results_path, "prepagada_RD_regresion.txt"))

# Export the discontinuity plot (PDF)
plot_rd_with_box <- function(preview = TRUE,
                             path = NULL,
                             x = 0.02, y = 0.98,         # esquina sup-izq del recuadro [0,1]
                             cex = 0.9, 
                             poly_order=4) {
  
  if (!preview) {
    stopifnot(!is.null(path))
    png(path, width = 800, height = 600, res = 150)
    on.exit(dev.off(), add = TRUE)
  }
  
  # --- RD plot ---
  rdplot(
    y = df_rd_filtered$prepa_fixed,
    x = df_rd_filtered$mmw,
    c = 5,
    p = poly_order,
    masspoints = "off",
    y.lim = c(0, 60),
    weights = df_rd_filtered$FEX_C,
    title = "",
    x.label = "MMW",
    y.label = "Percentage of Voluntary Insurance",
    nbins = 15,
    col.dots = "blue",
    col.lines = "lightcoral"
  )
  
  # --- Etiqueta y grob de texto ---
  label <- sprintf(
    "RD estimator: %.3f [95%% CI:%.3f to %.3f; p-value: %.3f]",
    estimate_p, ci_p_low, ci_p_upp, pvalue_p
  )
  
  pad <- unit(0.01, "npc") # padding interno
  tg  <- grid::textGrob(
    label,
    x = unit(x, "npc") + pad,
    y = unit(y, "npc") - pad,
    just = c("left", "top"),
    gp = gpar(cex = cex)
  )
  
  # Medir tamaño del texto para autoajustar el rectángulo
  w <- convertWidth(grobWidth(tg) + 2 * pad, "npc", valueOnly = TRUE)
  h <- convertHeight(grobHeight(tg) + 2 * pad, "npc", valueOnly = TRUE)
  
  # Recuadro
  grid::grid.rect(
    x = unit(x, "npc"), y = unit(y, "npc"),
    width  = unit(w, "npc"), height = unit(h, "npc"),
    just = c("left", "top"),
    gp = gpar(fill = "white", col = "black", lwd = 1)
  )
  
  # Texto
  grid::grid.draw(tg)
}

# ---- PREVIEW en el panel de RStudio (ajusta x, y y cex a gusto) ----
plot_rd_with_box(preview = TRUE, x = 0.1, y = 0.85, cex = 0.9, poly_order = 4)

# 2) Guardar EXACTAMENTE esa gráfica como imagen
library(rstudioapi)

rstudioapi::savePlotAsImage(
  file   = file.path(results_path, "Seguro_voluntario_rdplot2.png"),
  format = "png",   # "png", "jpeg", "tiff", "svg", etc.
  width  = 630,    # en píxeles (puedes ajustar)
  height = 500
)


library(rstudioapi)


# --- Loop principal ---
for (i in 1:6) {
  

  # 2. Generar el gráfico en el panel de RStudio 
  plot_rd_with_box(
    preview = TRUE, 
    x = 0.1, 
    y = 0.85, 
    cex = 0.9, 
    poly_order = i
  )

  Sys.sleep(1.0) # Pausa de 1.0 segundos para refresh del sistema
  
  # 4. Crear el nombre de archivo dinámico
  file_name <- sprintf("Seguro_voluntario_rdplot_poly_%d.png", i)
  file_full_path <- file.path(results_path, file_name)
  
  # 5. Guardar el gráfico actual
  rstudioapi::savePlotAsImage(
    file   = file_full_path,
    format = "png",
    width  = 630,
    height = 500
  )
}




 
# # (Opcional) Abrir el PNG en el Viewer de RStudio:
# #rstudioapi::viewer(file.path(results_path, "prepagada_rdplot.png"))
# 
# ## Complementary insurance -----------------------------------------------------
# 
# # Run the regression discontinuity with the optimal bandwidth on each side
# resultado_complementario <- rdrobust(
#   y = df_rd_filtered$comple_fixed,  # Outcome variable
#   x = df_rd_filtered$mmw,        # Assignment variable
#   c = 5,                # Threshold
#   bwselect = "msetwo",  # Method to select the bandwidth
#   masspoints = "off"    # Disable mass points
# )
# 
# # View the summary of the results
# summary(resultado_complementario)
# 
# # extracting results
# estimate_c <- resultado_complementario$Estimate[1]
# ci_c_low <- resultado_complementario$ci[3,1]
# ci_c_upp <- resultado_complementario$ci[3,2]
# pvalue_c <- resultado_complementario$pv[3]
# 
# # Export the regression results
# salida_regresion_c <- capture.output(summary(resultado_complementario))
# writeLines(salida_regresion_c, file.path(results_path, "Complementario_RD_regresion.txt"))
# 
# 
# # Export the discontinuity plot (PDF)
# png(file.path(results_path,"complementario_rdplot.png"), width = 800, height = 600, res = 150)
# # Generate the discontinuity plot
# # --- RD: Plan Complementario ---
# resultado_complementario <- rdrobust(
#   y = df_rd_filtered$comple_fixed,  # Outcome variable
#   x = df_rd_filtered$mmw,           # Assignment variable
#   c = 5,                            # Threshold
#   bwselect = "msetwo",              # Method to select the bandwidth
#   masspoints = "off"                # Disable mass points
# )
# 
# summary(resultado_complementario)
# 
# # extracting results (manteniendo tu misma convención)
# estimate_c <- resultado_complementario$Estimate[1]
# ci_c_low   <- resultado_complementario$ci[3, 1]
# ci_c_upp   <- resultado_complementario$ci[3, 2]
# pvalue_c   <- resultado_complementario$pv[3]
# 
# # Exportar resumen
# salida_regresion_c <- capture.output(summary(resultado_complementario))
# writeLines(salida_regresion_c, file.path(results_path, "Complementario_RD_regresion.txt"))
# 
# # ---- Función de plot para Complementario (misma interfaz que la tuya) ----
# plot_rd_with_box_c <- function(preview = TRUE,
#                                path = NULL,
#                                x = 0.02, y = 0.98,   # esquina sup-izq del recuadro [0,1]
#                                cex = 0.9) {
#   
#   if (!preview) {
#     stopifnot(!is.null(path))
#     png(path, width = 800, height = 600, res = 150)
#     on.exit(dev.off(), add = TRUE)
#   }
#   
#   # --- RD plot ---
#   rdplot(
#     y = df_rd_filtered$comple_fixed,
#     x = df_rd_filtered$mmw,
#     c = 5,
#     masspoints = "off",
#     y.lim = c(0, 45),
#     title = "",
#     x.label = "MMW",
#     y.label = "Percentage of Complementary Plan",
#     binselect = "qs",
#     col.dots = "blue",
#     col.lines = "lightcoral"
#   )
#   
#   # --- Recuadro (grid) ---
#   label <- sprintf(
#     "RD estimator: %.3f [95%% CI: %.3f to %.3f; p-value: %.3f]",
#     estimate_c, ci_c_low, ci_c_upp, pvalue_c
#   )
#   
#   pad <- unit(0.01, "npc") # padding interno
#   tg  <- grid::textGrob(
#     label,
#     x = unit(x, "npc") + pad,
#     y = unit(y, "npc") - pad,
#     just = c("left", "top"),
#     gp = gpar(cex = cex)
#   )
#   
#   w <- convertWidth(grobWidth(tg) + 2 * pad, "npc", valueOnly = TRUE)
#   h <- convertHeight(grobHeight(tg) + 2 * pad, "npc", valueOnly = TRUE)
#   
#   grid::grid.rect(
#     x = unit(x, "npc"), y = unit(y, "npc"),
#     width  = unit(w, "npc"), height = unit(h, "npc"),
#     just = c("left", "top"),
#     gp = gpar(fill = "white", col = "black", lwd = 1)
#   )
#   
#   grid::grid.draw(tg)
# }
# 
# # ---- PREVIEW en el Viewer (ajusta x, y, cex) ----
# plot_rd_with_box_c(preview = TRUE, x = 0.02, y = 0.98, cex = 0.9)
# 
# # ---- GUARDAR a archivo cuando esté listo ----
# plot_rd_with_box_c(
#   preview = FALSE,
#   path = file.path(results_path, "complementario_rdplot.png"),
#   x = 0.30, y = 0.85, cex = 0.6
# )
