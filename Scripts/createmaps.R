library(sf)
library(terra)
library(tidyverse)

# Paths to open files
smokepath = "~/Dropbox/ColoradoSmokeGrades/ColoradoSmokeGit/Data/SmokeDataforGitHub"
schoolpath = "~/Dropbox/ColoradoSmokeGrades/ColoradoSmokeGit/Data/GradeDataforGitHub"

dates = readRDS(file.path(smokepath, "schooldays.rds"))

SmokeData = readRDS(file.path(smokepath,"SmokewithFPAcount.rds"))
SmokeNoData = readRDS(file.path(smokepath,"SmokenoFPA.rds"))


# Having done this retain the smoke plumes that are only High, Medium and only on the school days

SmokeHeavy = bind_rows(SmokeData, SmokeNoData) %>%
  rename(year=year2,month=month2,day=day2) %>%
  left_join(dates, by=c("year", "month", "day")) %>%
  filter(Density=="Heavy") %>%
  filter(schoolday==1)  %>%
  arrange(year)

SmokeHeavygeo = SmokeHeavy %>%  
  split(.$DOP) %>% 
  lapply(st_union) %>% 
  do.call(c, .) %>% # bind the list element to a single sfc
  st_cast()

SmokeHeavy = SmokeHeavy %>% st_drop_geometry() %>%
  distinct(date,DOP,year,month,day,SchoolYear)
SmokeHeavy$geometry = SmokeHeavygeo
SmokeHeavy = st_sf(SmokeHeavy, crs=5070, sf_column_name = "geometry")
SmokeHeavy = SmokeHeavy %>% mutate(var1 =1)


# open schools and create a polygon
schools = readRDS(file.path(schoolpath, "geocodedSchools.rds"))
schools = schools %>% filter(bad_geocode==0)
schools = schools %>% filter(!(str_detect(address_in, "IL ")))
schools = st_as_sf(schools, coords = c("lon", "lat"), crs=4326)
schools = schools %>% st_transform(5070)
schools = schools %>% st_buffer(dist=1000,nQuadSegs = 2, endCapStyle = "SQUARE") 

# Now rasterize the smoke data for each year 2012 to 2023

raster_sample = rast(file.path(smokepath,"raster_sample.tif"))
for (i in c(2012:2023)){
  rname = paste0("SH",i)
  print(rname)
  raster0 = terra::subst(rasterize(vect(SmokeHeavy %>% 
                                          filter(SchoolYear==i) %>% 
                                          select(var1)), raster_sample, fun = sum), 
                         from = NA, to = 0)
  assign(rname,raster0)
}


# Example plot
plot(SH2019,breaks=c(0,2,5,10,15,20,35), col=hcl.colors(20, "Temps"), main="2019", 
     plg = list(loc = "bottomleft", horiz= FALSE, cex=0.85))
plot(st_geometry(schools), add=TRUE)



