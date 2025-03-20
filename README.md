# Description of the repository
This repository uses data from ECVN-DANE of year 2018 to estimate if there exist a discontinuity in 5 MMW in the usage of "Plan Complementario" and "Medicina Prepagada". Both of them types of voluntary insurance in Colombia. 

# Structure 
Proyect
├───data 
├───results
└───scripts

data: Contains .csv files downloaded from the DANE website, which are publicly available.
results: Contains regression results and RD plots.
scripts: Contains scripts used for the analysis.

To reproduce the analysis, download or clone the repository and then run the R script located in "scripts" using ctrl+alt+R or the buttons in the RStudio interface. Datasets and results must reproduce smotthly. 

Computational enviroment: This repository runs in a Windows system with 4GB of RAM available and 1 GB of storage. It also needs R, Rstudio and pacman library installed (it allows to other pacckages to get installed). 
