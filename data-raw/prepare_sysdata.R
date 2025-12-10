##run this script to update "./R/sysdata.rda"
rm(list=ls())
source("./data-raw/buffers.R")
source("./data-raw/reagents_materials.R")
##
objects <- ls()
do.call(usethis::use_data, c(lapply(objects, as.name), overwrite = TRUE,internal =TRUE))
