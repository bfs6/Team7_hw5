##the final JSON data is going to be feature-property

## Merge Data

library(plyr)
library(dplyr)
install.packages("readr")
library(readr)
library(stringr)

load('/data/Sta323/nyc_parking/pluto_data.Rdata')
nyc = read_csv("/data/Sta323/nyc_parking/NYParkingViolations.csv")

names(nyc) = make.names(names(nyc))

nyc_addr = nyc %>%
  select(Issuer.Precinct,House.Number,Street.Name) %>%
  transmute(precinct = Issuer.Precinct, 
            address = paste(House.Number, Street.Name) %>% tolower()) %>%
  filter(precinct >= 1, precinct <= 34)

pluto = pluto_data %>%
  rename(x=longitude,y=latitude)

pluto$address<-pluto$address %>% 
  str_replace("west","w") %>%
  str_replace("east","e") %>%
  str_replace("avenue","ave") %>%
  str_replace("street","st") %>%
  str_replace("bl","blvd") %>%
  str_replace("drive","dr")

nyc_addr$address <- nyc_addr$address %>%
  str_replace("str","st") %>%
  str_replace("pkwy","parkway") %>%
  str_replace("bway","broadway") %>%
  str_replace("west","w") %>%
  str_replace("east","e") %>%
  str_replace("steet","st") %>%
  str_replace("pl","place")

precincts = inner_join(nyc_addr, pluto)
precincts_rep = outer_join(nyc_addr, pluto)
## Visualizing Precincts

##actual precinct numbers in Manhattan
##good precinct? there are 22 of them
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
install.packages("rgdal",dependencies = TRUE)
library(rgdal)
install.packages("raster")
library(raster)

bb = readOGR("/data/Sta323/nyc_parking/nybb/","nybb")
manh = bb[bb$BoroName == "Manhattan",]

r = raster()
extent(r) = bbox(manh) #bbox gives a rectangular boundary of the shape
#get the number of rows, columns, and layers (raster doesn't do mixed layers)
dim(r) = c(1000,200)

#create a raster with all the objecgts in it
r = rasterize(manh, r)

#accessing the actual contents of the rasters
#r[] ---> this would give you a vector
#accessign the nth cell in that cell
#xyFromCell(r,1)

#array type things
plot(r)
plot(r, asp=0)
#see the white space in here? they correspond to the NA values in r[]



## Modeling - Logistic Regression
#start with predicitng one precint
pdata = data.frame(p = (d$precinct == 1), x = d$x, y = d$y)

#set family to binomial--->logistic regression
l = glm(p~poly(x,2)*poly(y,2), data=pdata, family=binomial)

#get the non NA values
pred_cells = which(r[] != 0)
pred_locs = xyFromCell(r,pred_cells) %>% as.data.frame()

pred = r
pred[pred_cells] = predict(l, newdata=pred_locs, type="response")
plot(pred, asp=0)




## Modeling - Logistic Regression across Precinct
pred_cells = which(r[] != 0)
pred_locs = xyFromCell(r,pred_cells) %>% as.data.frame()

res = list()
for(i in seq_along(good_precincts))
{
  pdata = data.frame(p = (d$precinct == good_precincts[i]), x = d$x, y = d$y)
  
  l = glm(p~poly(x,2)*poly(y,2), data=pdata, family=binomial)
  res[[i]] = predict(l, newdata=pred_locs, type="response")
}

#get probability matrix
probs = do.call(cbind, res)
#seep each row and pick out the largest values, each location coordinate has 22 probabilities
#one for each precinct, pick the precint that has the largest probability
pred_prec = good_precincts[apply(probs, 1, which.max)]

#make a copy of r and store results in hte copy not the original file, just to make sure nothing wierd happens there
pred_mlr = r
#how come the pred_prec values are soooo big?
pred_mlr[pred_cells] = pred_prec
plot(pred_mlr, asp=0)


## Modeling - Multinomial
##multi class identification----searches
library(nnet)

l = multinom(precinct~poly(x,2)*poly(y,2), data=d)

pred_mr = r
pred_mr[pred_cells] = predict(l, newdata=pred_locs)
plot(pred_mr, asp=0)


## Modeling - xgboost
install.packages("xgboost")
library(xgboost)

xg_data = as.matrix(d[,c("x","y")])
xg_label = as.matrix(d[,"precinct"]) %>% 
  as.factor() %>% 
  as.numeric() - 1

#softmax, doing a multinomial regression and picking one that has the largest value
l = xgboost(data=xg_data, label=xg_label, 
            objective="multi:softmax",num_class=length(good_precincts),
            nrounds=20)

p = predict(l, newdata=as.matrix(pred_locs))
pred_lab = good_precincts[p+1]


pred_xg = r
pred_xg[pred_cells] = pred_lab
plot(pred_xg, asp=0)


## Modeling - SVM
#take data, fold in a high dimensional space, cut it
#essentially everything in here is multiple logistic regression
library(e1071)

s = svm(as.factor(precinct)~x+y, data=d)

pred_svm = r
pred_svm[pred_cells] = predict(s, newdata=pred_loc)
plot(pred_svm,asp=0)


#writing GEOJSON
d.df<-as.data.frame(d)

precinct.sp<-SpatialPointsDataFrame(d.df[,c(3,4)],d.df[,c(3,4)])
writeOGR(precinct.sp,'precinct.geojson','d.df',driver='GeoJSON',check_exists = FALSE)

library(RJSONIO)
precinct.json<-toJSON(d.df, pretty=TRUE)

#how does the precinct.json file look like