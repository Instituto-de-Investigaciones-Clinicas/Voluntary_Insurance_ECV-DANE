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
p_load(rio, tidyverse, dplyr, readxl, janitor, readr, skimr, rdrobust) 

# Data paths
data_path <- "../data"
results_path <- "../results"



# Importing raw data ------------------------------------------------------

# Importing data - household characteristics and composition (person level)
comp_hogar <- read_delim(file.path(data_path,'caracteristicas_hogar_2018.CSV'),
                         delim = ";",
                         escape_double = FALSE,  
                         trim_ws = TRUE,
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
                    trim_ws = TRUE,
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
                    trim_ws = TRUE,
                    col_select = c('DIRECTORIO', 'SECUENCIA_ENCUESTA','SECUENCIA_P',
                                   'P8624' # income
                    )) 

# Filtering to people with income
labor <- labor %>% filter(P8624 > 0)

#renaming variables
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

summary(df$mmw)

# creating the variable of mmw
df$mmw <- df$income / mmw_2018

# Filter the data between 3 and 8 in the mmw variable
df_rd_filtered <- df[df$mmw >= 3 & df$mmw <= 8, ]

# preparing variables for RD
df_rd_filtered <- df_rd_filtered %>%
  mutate(prepa_fixed = 100 * ifelse(prepa == 2, 0, ifelse(prepa == 1, 1, NA)), 
         comple_fixed = 100 * ifelse(comp == 2, 0, ifelse(comp == 1, 1, NA)))

# Regression discontinuity -------------------------------------------------

## Prepagada insurance -----------------------------------------------------

# Run the regression discontinuity with the optimal bandwidth on each side
resultado_prepagada <- rdrobust(
  y = df_rd_filtered$prepa_fixed,  # Outcome variable
  x = df_rd_filtered$mmw,        # Assignment variable
  c = 5,                # Threshold
  bwselect = "msetwo",  # Method to select the bandwidth
  masspoints = "off"    # Disable mass points
)

# View the summary of the results
summary(resultado_prepagada)

# Export the regression results
salida_regresion_p <- capture.output(summary(resultado_prepagada))
writeLines(salida_regresion_p, file.path(results_path, "prepagada_RD_regresion.txt"))



# Export the discontinuity plot (PDF)
png(file.path(results_path,"prepagada_rdplot.png"), width = 800, height = 600, res = 150)
#RD plot
rdplot(
  y = df_rd_filtered$prepa_fixed,
  x = df_rd_filtered$mmw,
  c = 5,
  masspoints = "off",
  y.lim = c(0,15), 
  title = "RD for voluntary insurance",
  x.label = "MMW",
  y.label = "Percentage of Voluntary Insurance (Prepagada insurance)",
  binselect = "qs", 
  col.dots = "blue",  # Color of the dots
  col.lines = "lightcoral"  # Color of the fit lines
  #ci = FALSE,          # Show confidence intervals
)
dev.off()

## Complementary insurance -----------------------------------------------------

# Run the regression discontinuity with the optimal bandwidth on each side
resultado_complementario <- rdrobust(
  y = df_rd_filtered$comple_fixed,  # Outcome variable
  x = df_rd_filtered$mmw,        # Assignment variable
  c = 5,                # Threshold
  bwselect = "msetwo",  # Method to select the bandwidth
  masspoints = "off"    # Disable mass points
)

# View the summary of the results
summary(resultado_complementario)
# Export the regression results
salida_regresion_c <- capture.output(summary(resultado_complementario))
writeLines(salida_regresion_c, file.path(results_path, "Complementario_RD_regresion.txt"))


# Export the discontinuity plot (PDF)
png(file.path(results_path,"complementario_rdplot.png"), width = 800, height = 600, res = 150)
# Generate the discontinuity plot
rdplot(
  y = df_rd_filtered$comple_fixed,
  x = df_rd_filtered$mmw,
  c = 5,
  masspoints = "off",
  y.lim = c(0,45), 
  title = "RD for supplementary voluntary insurance",
  x.label = "MMW",
  y.label = "Percentage of Supplementary Voluntary Insurance",
  nbins = c(10,10),
  binselect = "es", 
  col.dots = "blue",  # Color of the dots
  col.lines = "lightcoral"  # Color of the fit lines
)
dev.off()