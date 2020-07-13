
# Overview

This repository contains the analysis and data processing steps that were carried out for the development of the work: "Data augmentation by means of hybrid modelling for linear actuator condition monitoring".

For better understanding of the code, please read the published work here[]. If you need further details you may contact the main author, Kerman López de Calle Etxabe at kerman.lopezdecalle@tekniker.es.


# Running recommendations

The code was developed in a Windows 10 machine with Rstudio and R version 3.6.1.
Nevertheless, we recommend the use of [Rstudio cloud], tested in windows and (Linux)?
Rstudio needed? Rversion?

# Project and code

The code in this project contains the steps taken from the raw data (available at a different repo) to the final results. The code recreates the final figures and results included in the paper. 

There are three main parts on the code which is stored in the [R folder]:

-  Data_downloading: Contains scripts to download the public data.
-  Pre-processing: Contains the scipts to process cycles and extract features.
-  Analysis: Contains scripts for the analysis and hybridization.

Each folder contains numbered Rmd files that the steps taken for in each phase. The .R files starting with 'aux' contain auxiliar functions used for the development of the code in the Rmd files.

You can either run the main.R script in the R folder to reproduce all the reports and store them in .html format in the Reports folder, or you can go one by one running the Rmd files by yourself. It is precisse that you run them in order.

# Citations

If you want to use any of the code found here or would like to refer to our work consider citing this repository with the following citation.





[Rstudio cloud]: https://rstudio.cloud/

[R folder]: (../R)
