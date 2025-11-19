* Opening the log file
	clear all
	*Ruta resultados
	global resultados_cs /home/javier/Documentos/Cost_sharing/7_Otros/14nov25-ECV_analysis

********************************************************************************
** Cost Shating Project-Giancarlo
** Aim: Hacer las gráficas de los análisis de efectos instantaneos afiliacion a prepagada

//  pgm:        stata 17
//  project:    Cost Sharing Project
//  author:    	GB/JA
//  date: 	Noviembre  of 202
********************************************************************************

use "/home/javier/Documentos/Cost_sharing/7_Otros/14nov25-ECV_analysis/datos_rd_prepagada_2018.dta"

gen PersonaBasicaID = _n

***********************************************************************************
** Opción de gráfica 1 -  fit de línea ajustada a la distribución de los puntos

	cap program drop Make_Graph_RD 
			program Make_Graph_RD 
			args depvar scorevar fraction center y_title 

			generate rate_cell = (floor((`scorevar'+`fraction')/`fraction')) // Create age bins		
			
			bysort rate_cell: egen `depvar'_cell=mean(`depvar')
			bysort rate_cell: gen n=_n
			replace `depvar'_cell=. if n!=1
			
			rdrobust `depvar' mmw, /*vce(cluster PersonaBasicaID)*/ masspoints(off) c(`center') weights(FEX_C)
			
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
			ytitle("Percentaje of Voluntary Insurance") xtitle("Wage (MMW)")  note("RD estimator = `e' [95% CI `e_l' to `e_h']", size(small))
			
			end
			


	********************************
	* Regressions
	********************************

	** RD plots de consumo en 5 MMW

	foreach var in prepa_fixed {
			
	Make_Graph_RD `var' mmw 0.15 5  

	graph save Graph "${resultados_cs}/RD_`var'_c5_cortados_3-8.gph", replace

	drop rate_c rate_c `var'_cell n

	}

	

	
	
*************************************************************************************
** 			Ajustando las gráficas

graph use "${resultados_cs}/RD_prepa_fixed_c5_cortados_3-8.gph"

gr_edit style.editstyle boxstyle(shadestyle(color(white))) editcopy
gr_edit note.DragBy 72.84710405820871 3.663907600560803
gr_edit title.text = {}
gr_edit xaxis1.title.text = {}
gr_edit xaxis1.title.text.Arrpush `"Monthly Earnings (in Monthly Minimum Wages (MMW))"'
gr_edit xaxis1.title.DragBy -2.586287718042914 -1.939715788532184
gr_edit yaxis1.title.DragBy 0 -.2155239765035755
gr_edit yaxis1.title.DragBy 0 -.2155239765035745
gr_edit yaxis1.title.DragBy 0 -.4310479530071498
gr_edit yaxis1.title.DragBy 0 -.2155239765035755
gr_edit note.style.editstyle drawbox(yes) editcopy
gr_edit note.style.editstyle fillcolor(white) editcopy
gr_edit note.DragBy 3.583173863434052 .3359225496969756
gr_edit subtitle.text = {}

graph rename graph_rd_prepagada, replace 

graph save "graph_rd_prepagada" "${resultados_cs}/RD_prepagada_c5_cortados_3-8_ajustada.gph", replace	
graph export "${resultados_cs}/RD_prepagada_c5_cortados_3-8_ajustada.png", as(png) name("graph_rd_prepagada") replace






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
