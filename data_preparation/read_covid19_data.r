
###### read_covid_cases.r
## Jan Verkade
## Saturday, March 21, 2020
##

##### Preliminaries
library(lubridate)
library(reshape2)
library(dplyr)
rm(list=objects())
source("~/covid19_config.r")


##### Read cases from files provided by JHU

## First 10 months of 2020 are pre-prepared. Only read what's more recent than that.
mm <- sprintf("%02.0f",1:10)
myFiles <- list.files(path=jhu_csv_folder,pattern=".csv",full.names = F)
i <- which(substr(myFiles,1,2) %in% mm); myFiles <- myFiles[-i] # Note the MINUS-i

tmp_list <- list()
for (i in seq_along(myFiles)) {
  tmp_list[[i]] <- read.csv(file.path(jhu_csv_folder,myFiles[i]),as.is=T)
  
  cols2remove <- c("Lat","Long_","Lon","Long","Latitude","Longitude","FIPS","Admin2","Last.Update","Last_Update","Combined_Key","Incidence_Rate","Case.Fatality_Ratio")
  n <- which(names(tmp_list[[i]]) %in% cols2remove); if (length(n)>0) { tmp_list[[i]][,n] <- NULL  }
  n <- which(names(tmp_list[[i]]) %in% c("Province_State","Province.State")); if (length(n)>0) {names(tmp_list[[i]])[n] <- "area"  }
  n <- which(names(tmp_list[[i]]) %in% c("Country_Region","Country.Region")); if (length(n)>0) {names(tmp_list[[i]])[n] <- "country"  }
  tmp_list[[i]]$date <- mdy(substr(myFiles[i],1,10))
  if(is.null(tmp_list[[i]]$Active)) { tmp_list[[i]]$Active <- NA }
  print(names(tmp_list[[i]]))
}


##### Post-processing
## Create single dataframe from dataframes in multiple lists; cast as tidy data
cases <- do.call("rbind",tmp_list); rm(tmp_list)
cases <- melt(cases,id.vars=c("area","country","date"))

iso_countries <- read.csv(file.path(covid19_home,"data_preparation","input","iso3_countrynames.csv"),as.is=T)

## Identify countries that are not listed (in the iso3_countrynames.csv file) as 'official' country names
## Subsequently, loop over country names that weren't found. Match unofficial country name (JHU_country) with 'official' name (ISO_country)
i <- which(!cases$country %in% iso_countries$ISO_country)
for ( r in unique(cases$country[i]) ) {
  j <- which(cases$country == r)
  replacement <- iso_countries$ISO_country[iso_countries$JHU_country==r]
  if (length(replacement)==0) { replacement <- "Other"; print(r) } 
  cases$country[cases$country == r] <- replacement
}

## Tidy up some of the columns
cases <- na.omit(cases)
cases$country <- as.factor(cases$country)
cases$area    <- as.factor(cases$area)
cases$Last.Update <- NULL

## Aggregate data by country
cases <- aggregate(cases$value,by=list(cases$country,cases$date,cases$variable),FUN=sum)
names(cases) <- c("country","date","variable","value")

## Match countries with their ISO3 code
cases <- merge(x=cases,y=iso_countries,by.x="country",by.y="ISO_country",no.dups=T); cases$JHU_country <- NULL

## From cumulative data, compute daily numbers
cases <- arrange(unique(cases), country, variable, date)
names(cases)[names(cases)=='value'] <- 'cum'
cases <- cases %>% group_by(country,variable) %>% mutate(daily = cum -lag(cum))
cases <- reshape2::melt(cases,id.vars=c("country","date","variable","ISO3"),variable.name="datatype")

##### Save the data
#write.table(cases,file=file.path(covid19_home,"covid19_data.csv"),quote=F,row.names = F,sep=";")
saveRDS(cases,file=file.path(RDS_file_location,"covid19_data.rds"))
