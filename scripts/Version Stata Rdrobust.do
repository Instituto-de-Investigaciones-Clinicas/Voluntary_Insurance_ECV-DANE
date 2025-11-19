* Opening the log file
	clear all
	*Ruta bases ecv:
	global bases_ecv D:\Documents_2\Epidatos\Trabajos\Cost_sharing\5_Otras_tareas\7_analisis_prepagada_ecv\Repositorio_github\Voluntary_Insurance_ECV-DANE\data
	*Ruta resultados
	global resultados_ecv D:\Documents_2\Epidatos\Trabajos\Cost_sharing\5_Otras_tareas\7_analisis_prepagada_ecv\Repositorio_github\Voluntary_Insurance_ECV-DANE\results


* Instalar los paquetes (solo una vez)
* ssc install rdrobust

* Cargando los paquetes
use "${bases_ecv}/datos_rd_prepagada_2018.dta", clear

gen PersonaBasicaID = _n



rdrobust prepa_fixed mmw, c(5)


***********************************************************************************
** Opción de gráfica 1 -  fit de línea ajustada a la distribución de los puntos

	cap program drop Make_Graph_RD 
			program Make_Graph_RD 
			args depvar scorevar fraction center y_title 

			generate rate_cell = (floor((`scorevar'+`fraction')/`fraction')) // Create age bins		
			
			bysort rate_cell: egen `depvar'_cell=mean(`depvar')
			bysort rate_cell: gen n=_n
			replace `depvar'_cell=. if n!=1
			
			rdrobust `depvar' mmw [aw = FEX_C], masspoints(off) c(`center')
			
			local e=e(tau_cl)
			local e_l=e(tau_cl)-(1.96*(e(se_tau_cl))) 
			local e_h=e(tau_cl)+(1.96*(e(se_tau_cl)))
			
			local e : di %3.2f `e'
			local e_l : di %3.2f `e_l'
			local e_h : di %3.2f `e_h'
			
			graph twoway (scatter  `depvar'_cell mmw,  mcolor(gs9) ms(oh) fcolor(white)) ///
			(lpoly `depvar' mmw if mmw<`center', deg(1)  ker(tri)  bwidth(`e(h_l)') clcolor(cranberry) clwidth(medthin)) ///
			(lpoly `depvar' mmw if mmw>=`center', deg(1)  ker(tri)  bwidth(`e(h_r)') clcolor(navy) clwidth(medthin)) if mmw<8, ///
			xline(`center', lpattern(dash_dot) lcolor(gray) )  legend(off) title("`depvar'") subtitle("Threshold at 5 MMW")	///
			ytitle("Mean of services") xtitle("Wage (MMW)")  note("RD estimator = `e' [95% CI `e_l' to `e_h']", size(small))
			
			end
			


	********************************
	* Regressions
	********************************

	** RD plots de consumo en 5 MMW

	foreach var in prepa_fixed {
			
	Make_Graph_RD `var' mmw 0.04 5  

	graph save Graph "${resultados_cs}/RD_`var'_c5_cortados_3-8.gph", replace

	drop rate_c rate_c `var'_cell n

	}





* 2. Replicar el análisis rdrobust
* (Nota: 'aw' son analytical weights, equivalentes a 'weights' en R-rdrobust)
rdrobust prepa_fixed mmw [aw = FEX_C], c(5) bwselect(msetwo)

* 3. Replicar el gráfico (ej. con polinomio de orden 4)
* El 'nbins' en R-rdplot se define en Stata con 'binselect(es)'
* y especificando el número de bins a cada lado.
* R-rdplot(nbins=15) -> Stata-rdplot(n(15))
rdplot prepa_fixed mmw [aw = FEX_C], c(5) p(4) nbins(15 15) ///
    yline(0 20 40 60, lcolor(gray*.5)) ///
    xline(3 4 5 6 7 8, lcolor(gray*.5)) ///
    title("") ///
    xlabel("MMW") ///
    ylabel("Percentage of Voluntary Insurance") ///
    graph_options( ///
        msymbol(Oh) mcolor(blue) lcolor(lightcoral) ///
        )