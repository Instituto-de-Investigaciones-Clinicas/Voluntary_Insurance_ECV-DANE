* Opening the log file
	clear all
	*Log_files
	global logs_cs /home/javier/Documentos/Cost_sharing/1_log_files
	*Ruta bases CS:
	global bases_cs /home/javier/Documentos/BASES
	*Ruta resultados
	global resultados_cs /home/javier/Documentos/Cost_sharing/6_Resultados/Impacto/LLR_cataneo/Instantanous

********************************************************************************
** Cost Shating Project-Giancarlo
** Aim: Hacer las gráficas de los análisis de efectos instantaneos para los desenlaces ambulatorios 

//  pgm:        stata 16
//  project:    Cost Sharing Project
//  author:    	GB/JA
//  date: 	April 4 of 2023
********************************************************************************

log using "${logs_cs}/log_RD_plots_months.smcl", replace



*** 5 MMW

use "${bases_cs}/Panel_2011_2018_full_MainOutcomes.dta", clear

keep PersonaBasicaID mmw AMB DRUGS IMAG LAB CONS

keep if mmw>=3 & mmw<=8 

***********************************************************************************
** Opción de gráfica 1 -  fit de línea ajustada a la distribución de los puntos

	cap program drop Make_Graph_RD 
			program Make_Graph_RD 
			args depvar scorevar fraction center y_title 

			generate rate_cell = (floor((`scorevar'+`fraction')/`fraction')) // Create age bins		
			
			bysort rate_cell: egen `depvar'_cell=mean(`depvar')
			bysort rate_cell: gen n=_n
			replace `depvar'_cell=. if n!=1
			
			rdrobust `depvar' mmw, vce(cluster PersonaBasicaID) masspoints(off) c(`center')
			
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

	foreach var in AMB DRUGS IMAG LAB CONS {
			
	Make_Graph_RD `var' mmw 0.04 5  

	graph save Graph "${resultados_cs}/RD_`var'_c5_cortados_3-8.gph", replace

	drop rate_c rate_c `var'_cell n

	}

	
	
*************************************************************************************
** 			Ajustando las gráficas

** Gráfica AMB
	graph use "${resultados_cs}/RD_AMB_c5_cortados_3-8.gph"
	
	gr_edit yaxis1.title.text = {}
	gr_edit yaxis1.title.text.Arrpush Mean of Outpatient Services per Month
	gr_edit yaxis1.title.style.editstyle size(med) editcopy
	gr_edit yaxis1.title.DragBy 0 -.2299402960021445
	gr_edit yaxis1.title.DragBy 0 -.2299402960021436
	gr_edit xaxis1.title.style.editstyle size(med) editcopy
	gr_edit xaxis1.title.text = {}
	gr_edit xaxis1.title.text.Arrpush `"Monthly Earnings (in Monthly Minimum Wages(MMW))"'
	gr_edit xaxis1.title.DragBy -2.299402960021429 0
	gr_edit note.DragBy 58.46352169659149 56.10543222452291
	gr_edit note.style.editstyle drawbox(yes) editcopy
	gr_edit note.text = {}
	gr_edit note.text.Arrpush `"RD estimator = -0.046 [95% CI -0.058 to -0.035]"'
	gr_edit note.style.editstyle fillcolor(white) editcopy
	gr_edit note.style.editstyle horizontal(center) editcopy
	gr_edit note.style.editstyle margin(medsmall) editcopy
	gr_edit note.style.editstyle size(relative3) editcopy
	gr_edit note.style.editstyle margin(small) editcopy
	gr_edit subtitle.text = {}
	gr_edit title.text = {}
	gr_edit yaxis1.title.style.editstyle size(med) editcopy
	gr_edit yaxis1.title.DragBy 1.149701480010698 -.9197611840085729
	gr_edit xaxis1.title.style.editstyle size(med) editcopy
	gr_edit note.DragBy 13.33653716812427 -3.908985032036436
	gr_edit AddTextBox added_text editor .6389253280385754 10.75629835213287
	gr_edit added_text_new = 1
	gr_edit added_text_rec = 3
	gr_edit added_text[1].style.editstyle  angle(default) size( sztype(relative) val(3.4722) allow_pct(1)) color(black) horizontal(left) vertical(middle) margin( gleft( sztype(relative) val(0) allow_pct(1)) gright( sztype(relative) val(0) allow_pct(1)) gtop( sztype(relative) val(0) allow_pct(1)) gbottom( sztype(relative) val(0) allow_pct(1))) linegap( sztype(relative) val(0) allow_pct(1)) drawbox(no) boxmargin( gleft( sztype(relative) val(0) allow_pct(1)) gright( sztype(relative) val(0) allow_pct(1)) gtop( sztype(relative) val(0) allow_pct(1)) gbottom( sztype(relative) val(0) allow_pct(1))) fillcolor(bluishgray) linestyle( width( sztype(relative) val(.2) allow_pct(1)) color(black) pattern(solid) align(inside)) box_alignment(east) editcopy
	gr_edit added_text[1].style.editstyle size(medsmall) editcopy
	gr_edit added_text[1].text = {}
	gr_edit added_text[1].text.Arrpush Price Elasticity: 0.09
	gr_edit added_text[1].style.editstyle color(gray) editcopy
	
	graph rename graph_rd_amb, replace 
	
	graph save "graph_rd_amb" "${resultados_cs}/RD_AMB_c5_cortados_3-8_ajustada.gph", replace	
	graph export "${resultados_cs}/RD_AMB_c5_cortados_3-8_ajustada.png", as(png) name("graph_rd_amb") replace

	
** Gráfica de DRUGS
	graph use "${resultados_cs}/RD_DRUGS_c5_cortados_3-8.gph"
	
	gr_edit title.text = {}
	gr_edit subtitle.text = {}
	gr_edit yaxis1.title.text = {}
	gr_edit yaxis1.title.text.Arrpush Mean of Drugs per Month
	gr_edit yaxis1.title.style.editstyle size(med) editcopy
	gr_edit yaxis1.title.DragBy 0 -.2299402960021396
	gr_edit yaxis1.title.DragBy 0 -.2299402960021445
	gr_edit xaxis1.title.text = {}
	gr_edit xaxis1.title.text.Arrpush `"Monthly Earnings (in Monthly Minimum Wages(MMW))"'
	gr_edit xaxis1.title.style.editstyle size(med) editcopy
	gr_edit note.DragBy 71.05155146466217 57.71501429653789
	gr_edit xaxis1.title.DragBy -4.138925328038569 -2.069462664019295
	gr_edit xaxis1.title.DragBy 2.529343256023573 2.069462664019288
	gr_edit yaxis1.title.DragBy -.6898208880064421 -.6898208880064288
	gr_edit note.text = {}
	gr_edit note.text.Arrpush `"RD estimator = -0.024 [95% CI -0.029 to -0.018]"'
	gr_edit note.style.editstyle drawbox(yes) editcopy
	gr_edit note.style.editstyle fillcolor(white) editcopy
	gr_edit note.style.editstyle horizontal(center) editcopy
	gr_edit note.style.editstyle size(relative3) editcopy
	gr_edit note.style.editstyle margin(small) editcopy
	gr_edit note.DragBy 4.598805920042884 -2.989223848027878
	gr_edit note.DragBy 0 -3.908985032036436
	
	gr_edit AddTextBox added_text editor .1790447360342879 10.52635805613073
	gr_edit added_text_new = 1
	gr_edit added_text_rec = 1
	gr_edit added_text[1].style.editstyle  angle(default) size( sztype(relative) val(3.4722) allow_pct(1)) color(black) horizontal(left) vertical(middle) margin( gleft( sztype(relative) val(0) allow_pct(1)) gright( sztype(relative) val(0) allow_pct(1)) gtop( sztype(relative) val(0) allow_pct(1)) gbottom( sztype(relative) val(0) allow_pct(1))) linegap( sztype(relative) val(0) allow_pct(1)) drawbox(no) boxmargin( gleft( sztype(relative) val(0) allow_pct(1)) gright( sztype(relative) val(0) allow_pct(1)) gtop( sztype(relative) val(0) allow_pct(1)) gbottom( sztype(relative) val(0) allow_pct(1))) fillcolor(bluishgray) linestyle( width( sztype(relative) val(.2) allow_pct(1)) color(black) pattern(solid) align(inside)) box_alignment(east) editcopy
	gr_edit added_text[1].style.editstyle size(medsmall) editcopy
	gr_edit added_text[1].text = {}
	gr_edit added_text[1].text.Arrpush Price Elasticity: 0.11
	gr_edit added_text[1].style.editstyle color(gray) editcopy
	
	graph rename graph_rd_drugs, replace 
	
	graph save "graph_rd_drugs"  "${resultados_cs}/RD_DRUGS_c5_cortados_3-8_ajustada.gph", replace	
	graph export "${resultados_cs}/RD_DRUGS_c5_cortados_3-8_ajustada.png", as(png) name("graph_rd_drugs") replace

	
** Gráfica CONS
	graph use "${resultados_cs}/RD_CONS_c5_cortados_3-8.gph"
	
	gr_edit yaxis1.title.text = {}
	gr_edit yaxis1.title.text.Arrpush Mean of Outpatient Consultations per Month
	gr_edit yaxis1.title.style.editstyle size(med) editcopy
	gr_edit yaxis1.title.DragBy 0 -.2299402960021445
	gr_edit yaxis1.title.DragBy 0 -.2299402960021436
	gr_edit xaxis1.title.style.editstyle size(small) editcopy
	gr_edit xaxis1.title.text = {}
	gr_edit xaxis1.title.text.Arrpush `"Monthly Earnings (in Monthly Minimum Wages(MMW))"'
	gr_edit xaxis1.title.style.editstyle size(med) editcopy
	gr_edit xaxis1.title.DragBy -2.299402960021429 0
	gr_edit note.DragBy 63.46352169659149 56.10543222452291
	gr_edit note.style.editstyle drawbox(yes) editcopy
	gr_edit note.text = {}
	gr_edit note.text.Arrpush `"RD estimator = -0.016 [95% CI -0.020 to -0.01]"'
	gr_edit note.style.editstyle fillcolor(white) editcopy
	gr_edit note.style.editstyle horizontal(center) editcopy
	gr_edit note.style.editstyle size(relative3) editcopy
	gr_edit note.style.editstyle margin(small) editcopy
	gr_edit subtitle.text = {}
	gr_edit title.text = {}
	gr_edit yaxis1.title.DragBy 1.149701480010698 -.9197611840085729
	gr_edit note.DragBy 8 -3.908985032036436

	gr_edit AddTextBox added_text editor .1790447360342879 10.52635805613073
	gr_edit added_text_new = 1
	gr_edit added_text_rec = 1
	gr_edit added_text[1].style.editstyle  angle(default) size( sztype(relative) val(3.4722) allow_pct(1)) color(black) horizontal(left) vertical(middle) margin( gleft( sztype(relative) val(0) allow_pct(1)) gright( sztype(relative) val(0) allow_pct(1)) gtop( sztype(relative) val(0) allow_pct(1)) gbottom( sztype(relative) val(0) allow_pct(1))) linegap( sztype(relative) val(0) allow_pct(1)) drawbox(no) boxmargin( gleft( sztype(relative) val(0) allow_pct(1)) gright( sztype(relative) val(0) allow_pct(1)) gtop( sztype(relative) val(0) allow_pct(1)) gbottom( sztype(relative) val(0) allow_pct(1))) fillcolor(bluishgray) linestyle( width( sztype(relative) val(.2) allow_pct(1)) color(black) pattern(solid) align(inside)) box_alignment(east) editcopy
	gr_edit added_text[1].style.editstyle size(medsmall) editcopy
	gr_edit added_text[1].text = {}
	gr_edit added_text[1].text.Arrpush Price Elasticity: 0.07
	gr_edit added_text[1].style.editstyle color(gray) editcopy
	
	graph rename graph_rd_cons, replace 
	
	graph save "graph_rd_cons" "${resultados_cs}/RD_CONS_c5_cortados_3-8_ajustada.gph", replace	
	graph export "${resultados_cs}/RD_CONS_c5_cortados_3-8_ajustada.png", as(png) name("graph_rd_cons") replace	

		
		
** Gráfica de IMAG
	graph use "${resultados_cs}/RD_IMAG_c5_cortados_3-8.gph"
	
	gr_edit title.text = {}
	gr_edit subtitle.text = {}
	gr_edit xaxis1.title.text = {}
	gr_edit xaxis1.title.text.Arrpush `"Monthly Earnings (in Monthly Minimum Wages(MMW))"'
	gr_edit xaxis1.title.style.editstyle size(med) editcopy
	gr_edit note.DragBy 76.11023797670936 53.57608896849931
	gr_edit yaxis1.title.text = {}
	gr_edit yaxis1.title.text.Arrpush Mean of Diagnostic Images per Month
	gr_edit yaxis1.title.DragBy -1.609582072015 -1.379641776012857
	gr_edit yaxis1.title.style.editstyle size(med) editcopy
	gr_edit xaxis1.title.DragBy -2.989223848027862 -.4598805920042949	
	gr_edit note.style.editstyle margin(medsmall) editcopy
	gr_edit note.text = {}
	gr_edit note.text.Arrpush `"RD estimator = -0.002 [95% CI -0.003 to -0.001]"'  
	gr_edit note.style.editstyle drawbox(yes) editcopy
	gr_edit note.style.editstyle fillcolor(white) editcopy
	gr_edit note.style.editstyle horizontal(center) editcopy
	gr_edit note.style.editstyle size(relative3) editcopy
	gr_edit note.style.editstyle margin(small) editcopy
	gr_edit note.DragBy 0 -1.908985032036436

	gr_edit AddTextBox added_text editor .020 10.52635805613073
	gr_edit added_text_new = 1
	gr_edit added_text_rec = 1
	gr_edit added_text[1].style.editstyle  angle(default) size( sztype(relative) val(3.4722) allow_pct(1)) color(black) horizontal(left) vertical(middle) margin( gleft( sztype(relative) val(0) allow_pct(1)) gright( sztype(relative) val(0) allow_pct(1)) gtop( sztype(relative) val(0) allow_pct(1)) gbottom( sztype(relative) val(0) allow_pct(1))) linegap( sztype(relative) val(0) allow_pct(1)) drawbox(no) boxmargin( gleft( sztype(relative) val(0) allow_pct(1)) gright( sztype(relative) val(0) allow_pct(1)) gtop( sztype(relative) val(0) allow_pct(1)) gbottom( sztype(relative) val(0) allow_pct(1))) fillcolor(bluishgray) linestyle( width( sztype(relative) val(.2) allow_pct(1)) color(black) pattern(solid) align(inside)) box_alignment(east) editcopy
	gr_edit added_text[1].style.editstyle size(medsmall) editcopy
	gr_edit added_text[1].text = {}
	gr_edit added_text[1].text.Arrpush Price Elasticity: 0.09
	gr_edit added_text[1].style.editstyle color(gray) editcopy
	
	graph rename graph_rd_imag, replace 	
	
	graph save "graph_rd_imag" "${resultados_cs}/RD_IMAG_c5_cortados_3-8_ajustada.gph", replace	
	graph export "${resultados_cs}/RD_IMAG_c5_cortados_3-8_ajustada.png", as(png) name("graph_rd_imag") replace		
	
	
** Gráfica de LAB
	graph use "${resultados_cs}/RD_LAB_c5_cortados_3-8.gph"	
	
	gr_edit title.text = {}
	gr_edit subtitle.text = {}
	gr_edit xaxis1.title.text = {}
	gr_edit xaxis1.title.text.Arrpush `"Monthly Earnings (in Monthly Minimum Wages(MMW))"'
	gr_edit xaxis1.title.style.editstyle size(med) editcopy
	gr_edit note.DragBy 76.11023797670936 53.57608896849931
	gr_edit yaxis1.title.text = {}
	gr_edit yaxis1.title.text.Arrpush Mean of Laboratory Tests per Month
	gr_edit yaxis1.title.style.editstyle size(med) editcopy
	gr_edit yaxis1.title.DragBy -1.609582072015 -1.379641776012857
	gr_edit xaxis1.title.DragBy -2.989223848027862 -.4598805920042949	
	gr_edit note.style.editstyle margin(medsmall) editcopy
	gr_edit note.text = {}
	gr_edit note.text.Arrpush `"RD estimator = -0.008 [95% CI -0.011 to -0.006]"'
	gr_edit note.style.editstyle drawbox(yes) editcopy
	gr_edit note.style.editstyle fillcolor(white) editcopy
	gr_edit note.style.editstyle horizontal(center) editcopy
	gr_edit note.style.editstyle size(relative3) editcopy
	gr_edit note.style.editstyle margin(small) editcopy
	gr_edit note.DragBy 0 -1.908985032036436
	
	gr_edit AddTextBox added_text editor .08 10.52635805613073
	gr_edit added_text_new = 1
	gr_edit added_text_rec = 1
	gr_edit added_text[1].style.editstyle  angle(default) size( sztype(relative) val(3.4722) allow_pct(1)) color(black) horizontal(left) vertical(middle) margin( gleft( sztype(relative) val(0) allow_pct(1)) gright( sztype(relative) val(0) allow_pct(1)) gtop( sztype(relative) val(0) allow_pct(1)) gbottom( sztype(relative) val(0) allow_pct(1))) linegap( sztype(relative) val(0) allow_pct(1)) drawbox(no) boxmargin( gleft( sztype(relative) val(0) allow_pct(1)) gright( sztype(relative) val(0) allow_pct(1)) gtop( sztype(relative) val(0) allow_pct(1)) gbottom( sztype(relative) val(0) allow_pct(1))) fillcolor(bluishgray) linestyle( width( sztype(relative) val(.2) allow_pct(1)) color(black) pattern(solid) align(inside)) box_alignment(east) editcopy
	gr_edit added_text[1].style.editstyle size(medsmall) editcopy
	gr_edit added_text[1].text = {}
	gr_edit added_text[1].text.Arrpush Price Elasticity: 0.08
	gr_edit added_text[1].style.editstyle color(gray) editcopy
	
	graph rename graph_rd_lab, replace 	

	
	graph save "graph_rd_lab" "${resultados_cs}/RD_LAB_c5_cortados_3-8_ajustada.gph", replace	
	graph export "${resultados_cs}/RD_LAB_c5_cortados_3-8_ajustada.png", as(png) name("graph_rd_lab") replace	
	
	
		
* Closing the log file
	log close




