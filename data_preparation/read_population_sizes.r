
###### read_population_sizes.r
## Jan Verkade
## Thursday, March 26, 2020
##

##### Preliminaries
library(lubridate)
library(reshape2)
library(dplyr)

rm(list=objects())
source("~/covid19_config.r")

wbdata <- read.csv("API_SP.POP.TOTL_DS2_en_csv_v2_887275.csv",skip=4)
i <- which(names(wbdata) %in% c("Country.Name","Country.Code","X2018")); wbdata <- wbdata[,i]; names(wbdata) <- c("country","code","pop18")

wbdata$country <- NULL
saveRDS(wbdata,file=file.path("covid19_app","wb_pop18.rds"))

