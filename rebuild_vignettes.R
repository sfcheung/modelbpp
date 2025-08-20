# Adapted from https://www.kloppenborg.ca/2021/06/long-running-vignettes/

base_dir <- getwd()

setwd("vignettes/")
knitr::knit("modelbpp.Rmd.original", output = "modelbpp.Rmd")

setwd(base_dir)

