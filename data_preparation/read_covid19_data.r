
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


##### Read cases from file
myFiles <- list.files(path=jhu_csv_folder,pattern=".csv",full.names = F)
tmp_list <- list()
for (i in seq_along(myFiles)) {
  tmp_list[[i]] <- read.csv(file.path(jhu_csv_folder,myFiles[i]),as.is=T)
  
  cols2remove <- c("Lat","Long_","Lon","Long","Latitude","Longitude","FIPS","Admin2","Last.Update","Last_Update","Combined_Key")
  n <- which(names(tmp_list[[i]]) %in% cols2remove); if (length(n)>0) { tmp_list[[i]][,n] <- NULL  }
  n <- which(names(tmp_list[[i]]) %in% c("Province_State","Province.State")); if (length(n)>0) {names(tmp_list[[i]])[n] <- "area"  }
  n <- which(names(tmp_list[[i]]) %in% c("Country_Region","Country.Region")); if (length(n)>0) {names(tmp_list[[i]])[n] <- "country"  }
  tmp_list[[i]]$date <- mdy(substr(myFiles[i],1,10))
  if(is.null(tmp_list[[i]]$Active)) { tmp_list[[i]]$Active <- NA }
  print(names(tmp_list[[i]]))
}

##### Post-processing
cases <- do.call("rbind",tmp_list); rm(tmp_list)
cases <- melt(cases,id.vars=c("area","country","date"))

iso_countries <- read.csv(file.path(covid19_home,"iso_matching.csv"),as.is=T)
i <- which(!cases$country %in% iso_countries$ISO_country)

for ( r in unique(cases$country[i]) ) {
  j <- which(cases$country == r)
  replacement <- iso_countries$ISO_country[iso_countries$JHU_country==r]
  if (length(replacement)==0) { replacement <- "Other" } 
  cases$country[cases$country == r] <- replacement
}
cases <- na.omit(cases)
cases$country <- as.factor(cases$country)
cases$area    <- as.factor(cases$area)
cases$Last.Update <- NULL
cases <- aggregate(cases$value,by=list(cases$country,cases$date,cases$variable),FUN=sum)
names(cases) <- c("country","date","variable","value")
cases <- merge(x=cases,y=iso_countries,by.x="country",by.y="ISO_country"); cases$JHU_country <- NULL

##### Compute daily data
cases <- arrange(cases, country, variable, date)
names(cases)[names(cases)=='value'] <- 'cum'
cases <- cases %>% group_by(country,variable) %>% mutate(daily = cum -lag(cum))
cases <- reshape2::melt(cases,id.vars=c("country","date","variable","code"),variable.name="datatype")

##### Save the data
write.table(cases,file=file.path(covid19_home,"covid19_data.csv"),quote=F,row.names = F,sep=";")
saveRDS(cases,file=file.path(RDS_file_location,"covid19_data.rds"))
