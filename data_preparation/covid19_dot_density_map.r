
##### covid19_dot_density_map.r
# Jan Verkade
# March 29, 2020
# SHP file from https://thematicmapping.org/downloads/world_borders.php

library(tidyverse)
library(sf)
library(RCurl)

##### Read file from Dropbox
url <- "https://dl.dropboxusercontent.com/s/lpi98yc1tupj9fg/covid19_data.rds?dl=1"
remoteBinFile <- getBinaryURL(url = url)
tmp = tempfile()
writeBin(object=remoteBinFile, con = tmp)
cases <- readRDS(tmp)
unlink(tmp)
names(cases)[names(cases)=="code"] <- "ISO3"
i <- which(is.na(cases$value)); cases <- cases[-i,]

worldmap <- st_read(file.path("shp","TM_WORLD_BORDERS_SIMPL-0.3.shp"),stringsAsFactors = FALSE, quiet = TRUE) %>% select(ISO3)

sf_data <- left_join(cases, worldmap) %>%  st_as_sf() 
#sf_data <- sf_data[,-c("FIPS","ISO2","UN","NAME","AREA","POP2005","REGION","SUBREGION")]

random_round <- function(x) {
  v=as.integer(x)
  r=x-v
  test=runif(length(r), 0.0, 1.0)
  add=rep(as.integer(0),length(r))
  add[r>test] <- as.integer(1)
  value=v+add
  ifelse(is.na(value) | value<0,0,value)
  return(value)
}

my_sf_data <- subset(sf_data,variable == "Confirmed" & datatype == "cum" & country %in% c("Netherlands"))

# data frame of number of dots to plot for each party (1 for every 100 votes)
num_dots <- as.data.frame(my_sf_data) %>% 
  select(value) %>% mutate_all(funs(. / 100)) %>% mutate_all(random_round)
num_dots <- round(num_dots)
num_dots$date <- as.character(my_sf_data$date)

# generates data frame with coordinates for each point + what party it is assiciated with
sf_dots <- map_df(names(num_dots), 
                  ~ st_sample(my_sf_data, size = num_dots[,.x], type = "random") %>%
                    st_cast("POINT") %>%
                    st_coordinates() %>%
                    as_tibble() %>%
                    setNames(c("lon","lat")))


p <- ggplot(data=sf_dots, aes(lon, lat)) +
  #geom_sf(data = my_sf_data, fill = "transparent",colour = "white") +
  geom_point(size=1) +
  #scale_colour_manual(values = pal) +
  coord_sf(crs = 4326, datum = NA) +
  #theme_void(base_family = "Iosevka", base_size = 48) +
  labs(x = NULL, y = NULL,title=NULL,subtitle=NULL,caption=NULL)

anim <- p + transition_states(date) + ggtitle('Now showing {closest_state}',subtitle = 'Frame {frame} of {nframes}')


ggsave("party_points.png", plot = p, dpi = 320, width = 80, height = 70, units = "cm")
