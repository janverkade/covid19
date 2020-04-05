
###### read_population_sizes.r
## Jan Verkade
## Thursday, March 26, 2020
##
## Purpose and mechanics:
## From data supplied by the World Bank, compose two-column dataframe comprising ISO3 country codes and population sizes
## This data is read by the app to compute countries' relative numbers
## This data is computed offline. The .rds file it is stored into, is 'manually' included in the app folder.
##

##### Preliminaries
rm(list=objects())
source("~/covid19_config.r")

##### Read data from file
wbdata <- read.csv(file.path("input","API_SP.POP.TOTL_DS2_en_csv_v2_887275.csv"),skip=4)

##### Purge data that is not required
i <- which(names(wbdata) %in% c("Country.Code","X2018")); wbdata <- wbdata[,i]; names(wbdata) <- c("ISO3","pop18")

##### Save data to file
saveRDS(wbdata,file=file.path(covid19_home,"app","wb_pop18.rds"))
