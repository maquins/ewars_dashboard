# ewars_dashboard

ewars dashboard R scripts and files

This repository containers the R scripts needed to run the EWARS dashboard

You can mirror the folder stucture and run the dashboard locally

see  link below on  shapefile management using the QGIS software.
https://gis.stackexchange.com/questions/75563/renaming-attributes-fields-in-shapefile-attribute-table-using-qgis

changes 2021-06-24

- users need to install INLA, no need to upload the zipped INLA package 
- INLA can be installed using the command below:
install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
