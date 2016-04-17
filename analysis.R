library(dplyr)
library(truncnorm)
library(readr)
library(stringr)
library(tidyr)
load('/data/Sta323/nyc_parking/pluto_data.Rdata')
nyccopy = read_csv("/data/Sta323/nyc_parking/NYParkingViolations.csv")
alternatives=read_csv("/data/Sta323/nyc_parking/altnames.csv")
nyc=nyccopy
names(nyc) = make.names(names(nyc))

nyc_addr = nyc %>%
  select(Issuer.Precinct,House.Number,Street.Name) %>%
  transmute(precinct = Issuer.Precinct, 
            address = paste(House.Number, Street.Name) %>% tolower()) %>%
  filter(precinct >= 1, precinct <= 34)

pluto = pluto_data %>%
  rename(x=longitude,y=latitude)

pluto$address<-pluto$address %>% 
  str_replace(" pkwy"," parkway") %>%
  str_replace(" bway"," broadway") %>%
  str_replace(" blvd"," bl") %>%
  str_replace(" west", " w") %>%
  str_replace(" east"," e") %>%
  str_replace(" street"," st") %>%
  str_replace(" place"," pl") %>%
  str_replace(" avenue"," ave") %>%
  str_replace("([0-9*?])st", "\\1") %>% 
  str_replace("([0-9*?])nd", "\\1") %>% 
  str_replace("([0-9*?])rd", "\\1") %>% 
  str_replace("([0-9*?])th", "\\1")

nyc_addr$address <- nyc_addr$address %>%
  
  str_replace(" pkwy"," parkway") %>%
  str_replace(" bway"," broadway") %>%
  str_replace(" blvd"," bl") %>%
  str_replace(" west", " w") %>%
  str_replace(" east"," e") %>%
  str_replace(" street"," st") %>%
  str_replace(" place"," pl") %>%
  str_replace(" avenue"," ave") %>%
  str_replace("([0-9*?])st", "\\1") %>% 
  str_replace("([0-9*?])nd", "\\1") %>% 
  str_replace("([0-9*?])rd", "\\1") %>% 
  str_replace("([0-9*?])th", "\\1")



precincts = inner_join(nyc_addr, pluto)

#central park
long=mean(precincts$x[precincts$precinct == 22])
lat = mean(precincts$y[precincts$precinct == 22])

R = 3959
to_Radian <- function(x){
  return(x*pi/180)
}
harversine <- function(x1,y1,x2,y2){
  x1<-to_Radian(x1)
  x2<-to_Radian(x2)
  y1<-to_Radian(y1)
  y2<-to_Radian(y2)
  
  dx = x2-x1
  dy = y2-y1
  a = (sin(dx/2))^2 + cos(x1)*cos(x2)*(sin(dy/2))^2
  c = 2*atan2(sqrt(a),sqrt(1-a))
  d = R*c
  return(d)
}

a=c()
for(i in seq(0.00, 0.05, by=0.001)){
  a=c(a, harversine(lat, long, lat+i, long+i))
}
#looks like 3 miles is about 0.08 decimals of lat above the mean and below the mean. while 0.5 miles is about 0.004 decimals of longitude. 

#Central park is about 3 mile long and 0.5 miles wide. WE can use the harversine formula above to calulate the range

n= 5000

cplatitude = rtruncnorm(n, a=lat-0.02, b =lat + 0.02, mean=lat, sd=0.01)
cplongitude = rtruncnorm(n, a=long-0.001, b =long+0.001, mean=long, sd=0.004)
#now make it into a rectangular shap. 

rectanglelat = rep(NA, n)
rectanglelong = rep(NA, n)

for(i in 1:length(cplatitude)){
  rectanglelat[i] = lat  + (cplatitude[i]-lat) + (cplongitude[i]-long)
  rectanglelong[i] = long  + (cplatitude[i]-lat) + (cplongitude[i]-long)
}


cpprecinct = rep(22L, n)
cpaddress = rep(NA, n)

centralpark = data.frame(precinct = cpprecinct, address = cpaddress, x = cplong2 , y = cplat2)

precincts = rbind(precincts, centralpark)


## Visualizing Precincts

good_precincts = c(1,5,6,7,9,10,13,14,17,18,19,20,22,
                   23,24,25,26,28,30,32,33,34)

d = precincts %>% filter(precinct %in% good_precincts)
table(d$precinct)


set.seed(123)
precinct_colors = rep(NA,34)
colors = scales::hue_pal(h = c(0, 360) + 15, c = 100, l = 65, h.start = 0, direction = 1)(22)
colors = sample(colors,length(colors))

precinct_colors[good_precincts] = colors

#distribution of good precincts, geo
plot(d$x, d$y, pch=16, cex=0.5, col=precinct_colors[d$precinct])
legend("topleft",legend=as.character(good_precincts), pch=16, col=colors, 
       ncol = 5, cex=1, x.intersp=0.33, y.intersp=0.33)
#there are intersections and mismatches in the data points, but do not try to fix this manually!!
# multi label models(picking models that pick best reulst from a number of categories: analogy: logistic regression)
## Manhattan Bounday

#these are both basic spatial data managing pckages
## Manhattan Bounday

library(rgdal)
library(raster)

bb = readOGR("/data/Sta323/nyc_parking/nybb/","nybb")
manh = bb[bb$BoroName == "Manhattan",]

r = raster()
extent(r) = bbox(manh)
dim(r) = c(1000,200)

r = rasterize(manh, r)


pred_cells = which(r[] != 0)
pred_locs = xyFromCell(r,pred_cells) %>% as.data.frame()


## Modeling - xgboost

library(xgboost)

xg_data = as.matrix(d[,c("x","y")])
xg_label = as.matrix(d[,"precinct"]) %>% 
  as.factor() %>% 
  as.numeric() - 1

l = xgboost(data=xg_data, label=xg_label, 
            objective="multi:softmax",num_class=length(good_precincts),
            nrounds=20)

p = predict(l, newdata=as.matrix(pred_locs))
pred_lab = good_precincts[p+1]

pred_xg = r
pred_xg[pred_cells] = pred_lab
plot(pred_xg, asp=0)



poly = rasterToPolygons(pred_xg, dissolve = TRUE)
names(poly) = "Precinct"

source("https://raw.githubusercontent.com/Sta323-Sp16/Homework/master/hw5/write_geojson.R")

write_geojson(poly, "precincts.json")
plot(poly)