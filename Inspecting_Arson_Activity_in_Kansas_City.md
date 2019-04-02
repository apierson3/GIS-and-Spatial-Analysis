---
title: "Inspecting Arson Activity in Kansas City"
author: "Andrew Pierson"
date: "2/5/2019"
output: 
  html_document:
    keep_md: true
---

Using the quadrat count approach to examine the point pattern of arson crime in KCMO. Here I will try out multiple quadrats and conduct a chi-square test to determine whether the arson events show clustering or not. 


```r
knitr::opts_chunk$set(echo = TRUE)
#Set working directory manually
setwd('E:/Rockhurst University/WS19/A Term/BIA 6313 Spatial and GIS Analytics/Homework 2/Kansas City Neighborhood Boundaries')
#Load package libraries
library(GISTools)
library(spatstat)
library(raster)
library(rgdal)
library(tidyverse)
library(stringr)
library(ggmap)
#Load in the layer files
KCMO <- readOGR(".","geo_export_5a73dbec-f9f2-44ca-ae09-f9c003a2af8b")
```

```
## OGR data source with driver: ESRI Shapefile 
## Source: "E:\Rockhurst University\WS19\A Term\BIA 6313 Spatial and GIS Analytics\Homework 2\Kansas City Neighborhood Boundaries", layer: "geo_export_5a73dbec-f9f2-44ca-ae09-f9c003a2af8b"
## with 246 features
## It has 5 fields
```

```r
#Print out headers for data attached to the shapefile
head(KCMO@data)
```

```
##   nbhid                                    nbhname objectid shape_area
## 0   113                              Eastern 49-63       68   14264150
## 1    75                           Broadway Gillham       28    7817690
## 2   131                              Brown Estates       30   40471689
## 3   160                               Calico Farms       31   34531986
## 4     4                               CBD Downtown       32   17274149
## 5    32 Central Blue Valley And Park Tower Gardens       34   13407465
##   shape_len
## 0  21686.66
## 1  11375.72
## 2  32762.61
## 3  24792.37
## 4  17190.55
## 5  23507.49
```

```r
#Read in the chosen crime data
crimes <- read.csv("E:/Rockhurst University/WS19/A Term/BIA 6313 Spatial and GIS Analytics/Homework 2/KCPD_Crime_Data_2018.csv") 
#Filter to choose a crime type
Arson <- filter(crimes,Description=="Arson")
#Rename "TE" into "TER" since the ggmap package we will use cannot recognize TE
Arson$Address <- str_replace(Arson$Address, "TE", "TER")
#Creating a complete address for geocoding in ggmap package
Arson$Complete.Address <- paste(Arson$Address,",",Arson$City,", MO",Arson$Zip.Code)
register_google(key = "AIzaSyA1jFDGVKkTAPzcOI8x4FHZF5dMwtbpx8Q")
output <- geocode(Arson$Complete.Address, source = "google", output="latlona")
Arson <- cbind(Arson,output) 
Arson <- Arson[complete.cases(Arson$lat),]
#Convert to SpatialPointsDataFrame
coordinates(Arson) <- ~lon+lat
#Identify spatialpoints where the distance is zero. These are the duplicates.
Arson <- remove.duplicates(Arson)
#Convert the SpatialPointsDataFrame to a SpatialPoints object.
Arson.points <- as(Arson,'SpatialPoints')
#Look up the CRS
proj4string(KCMO)
```

```
## [1] "+proj=longlat +ellps=WGS84 +no_defs"
```

```r
#There is no CRS for Arson yet
proj4string(Arson.points)
```

```
## [1] NA
```

```r
#Assign CRS; using WGS84
proj4string(Arson.points) <- CRS("+init=EPSG:4326")
#Plot the map with Arson incidents in the full view
par(mar=c(0,0,1,0))
plot(KCMO, main="Arson in Kansas City, MO") 
plot(Arson.points,add=TRUE, col="red")
```

![](Inspecting_Arson_Activity_in_Kansas_City_files/figure-html/Task B Part 1-1.png)<!-- -->

```r
#Define the events
xy <- coordinates(Arson)
#Create number of grids for raster map
Area <- raster::area(KCMO)
Density <- nrow(xy)/Area
r <- raster(KCMO, ncol = 50, nrow = 50)
r <- rasterize(KCMO, r)
plot(r)
#Create boundaries around each grid
quads <- as(r, 'SpatialPolygons')
par(mar=c(0,0,1,0))
plot(quads, add = TRUE)
#Plot points
points(Arson.points, col = 'red', cex = 0.5)
```

![](Inspecting_Arson_Activity_in_Kansas_City_files/figure-html/Task B Part 1-2.png)<!-- -->

```r
#Count the events
events <- rasterize(coordinates(Arson), r, fun = 'count', background = 0)
#Plot the events
par(mar=c(0,0,1,0))
plot(events)
plot(KCMO, add = TRUE)
```

![](Inspecting_Arson_Activity_in_Kansas_City_files/figure-html/Task B Part 1-3.png)<!-- -->

```r
#Find the frequency
f <- raster::freq(events, useNA = 'no')
head(f, 20)
```

```
##      value count
## [1,]     0  2378
## [2,]     1    86
## [3,]     2    17
## [4,]     3     9
## [5,]     4     5
## [6,]     5     4
## [7,]     8     1
```

```r
#Number of quadrats
quadrats <- sum(f[,2])
#Number of cases
cases <- sum(f[,1] * f[,2])
mu <- cases / quadrats
mu
```

```
## [1] 0.078
```

```r
#Calculating the variance-mean ratio (VMR)
ff <- data.frame(f)
colnames(ff) <- c('K', 'X')
ff$Kmu <- ff$K - mu
ff$Kmu2 <- ff$Kmu^2
ff$XKmu2 <- ff$Kmu2 * ff$X
s2 <- sum(ff$XKmu2) / (sum(ff$X)-1)
VMR <- s2 / mu
VMR
```

```
## [1] 2.379362
```

Creating a kernel density estimate map of arson and interpret the results. The results show a dense concentration of arson activity in the heart of the KCMO area. 


```r
#Calculate the kernel density
Arson.density <- kde.points(Arson, lims = KCMO)
#Create a level plot
level.plot(Arson.density)
#Load the library
library(GISTools)
#proj4string(KCMO)
#Created a map of the boundary of the studied area
masker <- poly.outer(Arson.density, KCMO)
add.masking(masker)
plot(KCMO, add = TRUE)
title("Arson in KCMO")
```

![](Inspecting_Arson_Activity_in_Kansas_City_files/figure-html/Task B Part 2-1.png)<!-- -->


Examining the G, F, and K functions to determine whether the arson events show clustering or not.There is statistically significant clustering of arson activity. 


```r
#Load packages required to complete
library(spatstat)
#Convert to ppp object
Arson.ppp<-as.ppp(Arson)
nnd <- nndist.ppp(Arson.ppp)
summary(nnd)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
##  0.000013  0.002208  0.004709  0.071168  0.008298 12.454037
```

```r
#Calculate the mean nearest-neighbor distance
mnnd <- mean(nnd)
#Compare against expected pattern for IRP/CSR
mnnd
```

```
## [1] 0.0711679
```

```r
#Convert area of study to object in spatstat
#W <- as.owin(blocks) 
#Calculate the expected IRP/CSR mean nearest-neighbor distance
#exp_nnd <- 0.5 / sqrt(breach.ppp$n / area.owin(W)) 
#Calculate ratio of observed/expected #compare to Clark & Evan's R statistic
#mnnd / exp_nnd 
#G-function or the fraction of nearest neighbors < d
Gf <- envelope(Arson.ppp, Gest)
```

```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38,
## 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76,
## 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
```

```r
plot(Gf)
```

![](Inspecting_Arson_Activity_in_Kansas_City_files/figure-html/Task B Part 3-1.png)<!-- -->

```r
#Correction for edge effects 
Gf <- envelope(Arson.ppp, Gest, correction="border")
```

```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38,
## 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76,
## 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
```

```r
#Read the documentation for Gest function
plot(Gf)
```

![](Inspecting_Arson_Activity_in_Kansas_City_files/figure-html/Task B Part 3-2.png)<!-- -->

```r
mad.test(Arson.ppp,Gest)
```

```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38,
## 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76,
## 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
```

```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: G(r)
## 	Reference function: theoretical
## 	Alternative: two.sided
## 	Interval of distance values: [0, 0.425551826236796]
## 	Test statistic: Maximum absolute deviation
## 	Deviation = observed minus theoretical
## 
## data:  Arson.ppp
## mad = 0.92382, rank = 1, p-value = 0.01
```

```r
dclf.test(Arson.ppp,Gest)
```

```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38,
## 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76,
## 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
```

```
## 
## 	Diggle-Cressie-Loosmore-Ford test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: G(r)
## 	Reference function: theoretical
## 	Alternative: two.sided
## 	Interval of distance values: [0, 0.425551826236796]
## 	Test statistic: Integral of squared absolute deviation
## 	Deviation = observed minus theoretical
## 
## data:  Arson.ppp
## u = 0.067408, rank = 1, p-value = 0.01
```

```r
#F-function or fraction of nearest neighbors to random point locations (hazard rate)
Ff <- envelope(Arson.ppp, Fest, correction="border")
```

```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38,
## 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76,
## 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
```

```r
plot(Ff)
```

![](Inspecting_Arson_Activity_in_Kansas_City_files/figure-html/Task B Part 3-3.png)<!-- -->

```r
mad.test(Arson.ppp,Fest)
```

```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38,
## 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76,
## 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
```

```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: F(r)
## 	Reference function: theoretical
## 	Alternative: two.sided
## 	Interval of distance values: [0, 0.426870066601561]
## 	Test statistic: Maximum absolute deviation
## 	Deviation = observed minus theoretical
## 
## data:  Arson.ppp
## mad = 0.96635, rank = 1, p-value = 0.01
```

```r
dclf.test(Arson.ppp,Fest)
```

```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38,
## 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76,
## 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
```

```
## 
## 	Diggle-Cressie-Loosmore-Ford test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: F(r)
## 	Reference function: theoretical
## 	Alternative: two.sided
## 	Interval of distance values: [0, 0.426870066601561]
## 	Test statistic: Integral of squared absolute deviation
## 	Deviation = observed minus theoretical
## 
## data:  Arson.ppp
## u = 0.26425, rank = 1, p-value = 0.01
```

```r
#K-function or fraction of neighbors within a certain radius
Kf <- envelope(Arson.ppp, Kest, correction="border")
```

```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38,
## 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76,
## 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
```

```r
plot(Kf)
```

![](Inspecting_Arson_Activity_in_Kansas_City_files/figure-html/Task B Part 3-4.png)<!-- -->

```r
mad.test(Arson.ppp,Kest)
```

```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38,
## 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76,
## 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
```

```
## 
## 	Maximum absolute deviation test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: K(r)
## 	Reference function: theoretical
## 	Alternative: two.sided
## 	Interval of distance values: [0, 0.190381074999999]
## 	Test statistic: Maximum absolute deviation
## 	Deviation = observed minus theoretical
## 
## data:  Arson.ppp
## mad = 9.6514, rank = 1, p-value = 0.01
```

```r
dclf.test(Arson.ppp,Kest)
```

```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38,
## 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76,
## 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
```

```
## 
## 	Diggle-Cressie-Loosmore-Ford test of CSR
## 	Monte Carlo test based on 99 simulations
## 	Summary function: K(r)
## 	Reference function: theoretical
## 	Alternative: two.sided
## 	Interval of distance values: [0, 0.190381074999999]
## 	Test statistic: Integral of squared absolute deviation
## 	Deviation = observed minus theoretical
## 
## data:  Arson.ppp
## u = 7.5933, rank = 1, p-value = 0.01
```
