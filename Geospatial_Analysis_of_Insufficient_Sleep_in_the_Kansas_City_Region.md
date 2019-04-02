### Geospatial Analysis of Insufficient Sleep in the Kansas City Region
#### Author: Andrew Pierson"
#### Date: 2/16/2019


Creating a census tract map for the greater Kansas City region for analysis.

```r
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(progress = FALSE)
# Load packages
library(tidyverse)
library(tidycensus)
library(tigris)
library(spdep)
# Read in important health indicator data
sleep <- read.csv("C:/Users/Andrew/Desktop/Rockhurst University/WS19/A Term/BIA 6313 Spatial and GIS Analytics/Homework 3/indicator_data_download_20190216.csv")
# Check out the columns
names(sleep)
```

```
##  [1] "Indicator.Name"                    
##  [2] "What.Is.This.Indicator"            
##  [3] "Location.Type"                     
##  [4] "Location"                          
##  [5] "Indicator.Value"                   
##  [6] "Indicator.Value.Units"             
##  [7] "Lower.Confidence.Interval"         
##  [8] "Upper.Confidence.Interval"         
##  [9] "Indicator.Value.Unstable"          
## [10] "Period.of.Measure"                 
## [11] "Data.Source"                       
## [12] "Technical.Note"                    
## [13] "Breakout.Title"                    
## [14] "Breakout.Category"                 
## [15] "Breakout.Subcategory"              
## [16] "Breakout.Value"                    
## [17] "Breakout.Value.Units"              
## [18] "Breakout.Lower.Confidence.Interval"
## [19] "Breakout.Upper.Confidence.Interval"
## [20] "Breakout.Unstable"                 
## [21] "Breakout.Footer"
```

```r
# Filter the data to get a better subset
data <- sleep %>% filter(Period.of.Measure == 2014)
# Select the location and indicator columns
data <- data[, 4:5]
# Rename the column names
colnames(data) <- c("census_tract", "indicator")
# Pull census data from API
# tidycensus::census_api_key('60759d8b7e662b4e41566c69d17ed0199a8a3c16',
# install=TRUE)
readRenviron("~/.Renviron")
# American Community Survey census tract data information
inc.mo <- get_acs(geography = "tract", variable = "B06011_001E", state = "MO", 
    county = c("Jackson", "Cass"), year = 2014, geometry = FALSE)
# American Community Survey census tract data information
inc.ks <- get_acs(geography = "tract", variable = "B06011_001E", state = "KS", 
    county = c("Johnson", "Wyandotte"), year = 2014, geometry = FALSE)
# Row bind
inc.total <- rbind(inc.mo, inc.ks)
# Select the GEOID and estimate columns
inc.total <- inc.total[, c(1, 4)]
# Rename estimate to median income
names(inc.total)[2] <- "median_income"
# Check a summary of the statement
summary(inc.total$median_income)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    9541   20621   29382   30399   37809   74375      14
```

```r
# Set Nan values to zero
inc.total$median_income <- ifelse(is.na(inc.total$median_income), 0, inc.total$median_income)
# Change the data type from numeric to character
data$census_tract <- as.character(data$census_tract)
# Merge the data
health_data <- merge(data, inc.total, by.x = "census_tract", by.y = "GEOID")
# Check out the head of the data frame
head(health_data)
```

```
##   census_tract indicator median_income
## 1  20091050400      29.8         28664
## 2  20091050500      29.3         30052
## 3  20091050600      25.5         38093
## 4  20091051100      28.6         31685
## 5  20091051200      27.5         35065
## 6  20091051300      26.6         34132
```

```r
# Load Packages
library(tigris)
```


```r
# Load Packages
library(tigris)
# Define the counties of interest
mo.cass <- tracts(state = "29", county = "037")
mo.jackson <- tracts(state = "29", county = "095")
ks.johnson <- tracts(state = "20", county = "091")
ks.wyandotte <- tracts(state = "20", county = "209")
```


```r
# Combine the selected counties into a single boundary region
kc <- rbind(mo.cass, mo.jackson, ks.johnson, ks.wyandotte)
# Plot the kc tract boundaries
par(mar = c(0, 0, 0, 0))
plot(kc)
```

![](https://github.com/apierson3/GIS-and-Spatial-Analysis/blob/master/Geospatial_Analysis_of_Insufficient_Sleep_in_the_Kansas_City_Region_files/figure-html/Task%20A%20Part%201-1.png)<!-- -->

```r
# Check the CRS of kc
proj4string(kc)
```

```
## [1] "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
```

```r
# Merge the data
data.sp <- merge(kc, health_data, by.x = "GEOID", by.y = "census_tract")
# Check the CRS of data.sp
proj4string(data.sp)
```

```
## [1] "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
```

```r
# Set CRS and scale
data.sp <- spTransform(data.sp, CRS = "+init=epsg:2817 +units=km")
# Load packages
library(GISTools)
library(tigris)
library(rgdal)
# Save SpatialPolygonDataFrame
setwd("C:/Users/Andrew/Desktop/Rockhurst University/WS19/A Term/BIA 6313 Spatial and GIS Analytics/Homework 3/")
# writeOGR(data.sp, dsn='.', layer='Insufficient_Sleep_KC', driver='ESRI
# Shapefile') Read in the shapefile as a sf object
Sleep_KC <- st_read("Insufficient_Sleep_KC.shp")
```

```
## Reading layer `Insufficient_Sleep_KC' from data source `C:\Users\Andrew\Desktop\Rockhurst University\WS19\A Term\BIA 6313 Spatial and GIS Analytics\Homework 3\Insufficient_Sleep_KC.shp' using driver `ESRI Shapefile'
## Simple feature collection with 419 features and 14 fields
## geometry type:  POLYGON
## dimension:      XY
## bbox:           xmin: 801.6259 ymin: 252.9146 xmax: 887.9682 ymax: 340.792
## epsg (SRID):    NA
## proj4string:    +proj=tmerc +lat_0=36.16666666666666 +lon_0=-94.5 +k=0.999941177 +x_0=850000 +y_0=0 +ellps=GRS80 +units=km +no_defs
```

```r
# Load packages
library(leaflet)
library(stringr)
library(purrr)
library(sf)
# Transform to simple feature
Sleep_KC.sf <- st_as_sf(Sleep_KC)
# Plot simple feature
plot(Sleep_KC.sf["indictr"], main = "Percent with Insufficient Sleep")
```

![](https://github.com/apierson3/GIS-and-Spatial-Analysis/blob/master/Geospatial_Analysis_of_Insufficient_Sleep_in_the_Kansas_City_Region_files/figure-html/Task%20A%20Part%201-2.png)<!-- -->

```r
# Create a color pallete
pal <- colorQuantile(palette = "viridis", domain = Sleep_KC$indictr, n = 10)
# Transform the CRS and plot using leaflet Sleep_KC %>% st_transform(crs =
# '+init=epsg:4326') %>% leaflet(width = '100%') %>%
# addProviderTiles(provider = 'CartoDB.Positron') %>% addPolygons(popup = ~
# str_extract(NAME, '^([^,]*)'), stroke = FALSE, smoothFactor = 0,
# fillOpacity = 0.7, color = ~ pal(indictr)) %>% addLegend('bottomright',
# pal = pal, values = ~ indictr, title = 'Percent with Insufficient Sleep',
# opacity = 1)
```

The global Moran I's value produces a value of 0.434, an indicator of strong autocorrelation among adjacent distances. Alternatively, the local Moran's I plot shows that there are some clusters that are correlated more strongly. These clusters are located more toward the center and then in the southwest and possibly the northeast of Kansas City as well.


```r
# Create legend for plot
add.scale <- function() {
    lines(c(0, 0, 0, 0), c(0, 0, 0, 0))
    text(0, 10, "50 Km")
}
# Load packages
library(rgdal)
# Load in the layer files
Sleep_KC <- readOGR("C:/Users/Andrew/Desktop/Rockhurst University/WS19/A Term/BIA 6313 Spatial and GIS Analytics/Homework 3/Insufficient_Sleep_KC.shp")
```

```
## OGR data source with driver: ESRI Shapefile 
## Source: "C:\Users\Andrew\Desktop\Rockhurst University\WS19\A Term\BIA 6313 Spatial and GIS Analytics\Homework 3\Insufficient_Sleep_KC.shp", layer: "Insufficient_Sleep_KC"
## with 419 features
## It has 14 fields
```

```r
# Remove NAs and associated polygons
Sleep_KC <- Sleep_KC[!is.na(Sleep_KC$indictr), ]
Sleep_KC.indicator <- Sleep_KC$indictr
# Compute the listw object for the KC polygons
Sleep.KC.listw <- nb2listw(poly2nb(Sleep_KC))
Sleep.KC.listw
```

```
## Characteristics of weights list object:
## Neighbour list object:
## Number of regions: 306 
## Number of nonzero links: 1834 
## Percentage nonzero weights: 1.958648 
## Average number of links: 5.993464 
## 
## Weights style: W 
## Weights constants summary:
##     n    nn  S0       S1       S2
## W 306 93636 306 109.9346 1258.037
```

```r
# Prepare numeric vector data
Sleep.indictr <- Sleep_KC$indictr
# Compute the global Moran's I
nc.gI <- moran.test(Sleep.indictr, Sleep.KC.listw)
nc.gI
```

```
## 
## 	Moran I test under randomisation
## 
## data:  Sleep.indictr  
## weights: Sleep.KC.listw    
## 
## Moran I statistic standard deviate = 12.989, p-value < 2.2e-16
## alternative hypothesis: greater
## sample estimates:
## Moran I statistic       Expectation          Variance 
##       0.434510483      -0.003278689       0.001135935
```

```r
# Compute the local Moran's I
nc.lI <- localmoran(Sleep.indictr, Sleep.KC.listw)
head(nc.lI)
```

```
##             Ii         E.Ii    Var.Ii        Z.Ii Pr(z > 0)
## 1  -0.02307255 -0.003278689 0.9801767 -0.01999302 0.5079755
## 7   0.03017913 -0.003278689 0.4885088  0.04786979 0.4809100
## 17 -0.02777843 -0.003278689 0.4885088 -0.03505301 0.5139813
## 20  0.35747888 -0.003278689 0.1607302  0.89984294 0.1841019
## 21 -0.01080795 -0.003278689 0.2426748 -0.01528410 0.5060972
## 23  0.35825311 -0.003278689 0.1935081  0.82185837 0.2055788
```

```r
# Compute a shading scheme
sleep.shade <- auto.shading(c(nc.lI[, 1], -nc.lI[, 1]), cols = brewer.pal(5, 
    "PRGn"))
# Plot the local moran's I values
par(mar = c(0, 0, 0, 0))
choropleth(Sleep_KC, nc.lI[, 1], shading = sleep.shade)
choro.legend(87.9, 85, sleep.shade, fmt = "%6.2f", cex = 0.5)
title("\r\nKC Percent with Insufficient Sleep (Local Moran's I)", cex.main = 1)
add.scale()
```

![](https://github.com/apierson3/GIS-and-Spatial-Analysis/blob/master/Geospatial_Analysis_of_Insufficient_Sleep_in_the_Kansas_City_Region_files/figure-html/Task%20A%20Part%202-1.png)<!-- -->

```r
# Create a shading scheme for p-values
pval.shade <- shading(c(0.01, 0.05, 0.1), cols = rev(brewer.pal(4, "PuRd")))
# Plot the p-values
choropleth(Sleep_KC, nc.lI[, 5], shading = pval.shade)
# Add legends + title + scale
choro.legend(87.9, 85, pval.shade, fmt = "%6.2f", cex = 0.5)
title("\r\nKC Percent with Insufficient Sleep (Local p-value)", cex.main = 1)
add.scale()
```

![](https://github.com/apierson3/GIS-and-Spatial-Analysis/blob/master/Geospatial_Analysis_of_Insufficient_Sleep_in_the_Kansas_City_Region_files/figure-html/Task%20A%20Part%202-2.png)<!-- -->

```r
# Plot the adjusted p-values
choropleth(Sleep_KC, p.adjust(nc.lI[, 5], method = "bonferroni"), shading = pval.shade)
# Add legends + title + scale
choro.legend(87.5, 85, pval.shade, fmt = "%6.2f", cex = 0.5)
title("\r\nKC Percent with Insufficient Sleep (Local Bonferroni Adjusted  p-value)", 
    cex.main = 1)
add.scale()
```

![](https://github.com/apierson3/GIS-and-Spatial-Analysis/blob/master/Geospatial_Analysis_of_Insufficient_Sleep_in_the_Kansas_City_Region_files/figure-html/Task%20A%20Part%202-3.png)<!-- -->

```r
# Create a matrix to place the simulated local Moran's I
sim.I <- matrix(0, 1000, 100)
# Run simulations using local moran's I
for (i in 1:1000) sim.I[i] <- localmoran(sample(Sleep.indictr), Sleep.KC.listw)[, 
    4]
# Monte Carlo simulated p-values
mc.pvals <- (colSums(sweep(sim.I, 2, nc.lI[, 4], ">=")) + 1)/(nrow(sim.I) + 
    1)
# Create a shading scheme for the Monte Carlo simulated p-values
par(mfrow = c(1, 1))
par(mar = c(1, 1, 1, 1))
pval.shade <- shading(c(0.01, 0.05, 0.1), cols = rev(brewer.pal(4, "PuRd")))
# Plot the Monte Carlo simulated p-values using FDR approach
choropleth(Sleep_KC, p.adjust(mc.pvals, method = "fdr"), shading = pval.shade)
# Add legends + title + scale
choro.legend(120.3, 54.9, pval.shade, fmt = "%6.2f")
title("\r\nKC Percent with Insufficient Sleep (Monte Carlo-Simulated FDR Adjusted p-value)", 
    cex.main = 1)
add.scale()
```

![](https://github.com/apierson3/GIS-and-Spatial-Analysis/blob/master/Geospatial_Analysis_of_Insufficient_Sleep_in_the_Kansas_City_Region_files/figure-html/Task%20A%20Part%202-4.png)<!-- -->


```r
knitr::opts_knit$set(progress = FALSE)
# Load packages
library(tigris)
# Define king county in washington state
wa.king <- tracts(state = "53", county = "033")
```



```r
# Read in housing data
house <- read.csv("C:/Users/Andrew/Desktop/Rockhurst University/WS19/A Term/BIA 6313 Spatial and GIS Analytics/Homework 3/kc_house_data/kc_house_data.csv")
# Convert house data to SpatialPointsDataFrame
coordinates(house) <- ~long + lat
# Identify spatialpoints where the distance is zero. These are the
# duplicates.
house <- remove.duplicates(house)
# Convert the SpatialPointsDataFrame to a SpatialPoints object.
house.points <- as(house, "SpatialPoints")
# Look up the CRS
proj4string(house.points)
```

```
## [1] NA
```

```r
# Assign CRS to house; using WGS84
proj4string(house.points) <- CRS("+init=EPSG:26910 +units=mi")
# Assign CRS to wa.king; using WGS84
proj4string(wa.king) <- CRS("+init=EPSG:26910 +units=mi")
# Plot and overlay
plot(wa.king)
plot(house.points, add = TRUE, col = "red")
```

![](https://github.com/apierson3/GIS-and-Spatial-Analysis/blob/master/Geospatial_Analysis_of_Insufficient_Sleep_in_the_Kansas_City_Region_files/figure-html/Task%20B%20Part%201-1.png)<!-- -->


I found the good predictors to include: sqft_Living15, sqft_basement, sqft_above, grade, view, floors, sqft_living, bedrooms, and bathrooms. All of these predictors had extremely low p-values, represented in scientific notation. The predictors I chose all have p-values below a value of 0.001.

The main measure that I used to evaluate the geographically weighted models was the AIC values. Models with relatively lower AIC values leave out less information than one with a large AIC value. This allowed me to narrow my choices to the models with bandwidths of 25, 50, and 75. The next step to distinguish the best model among these three was to inspect their R-squared and residual sum of squares (RSS) values. Ultimately the gwr.res25 model with a bandwidth of 25 was the best because it had the highest and lowest r-squared and RSS values, respectively. This means that the predictors of the model with a bandwidth of  25 have the least amount of error and maintain the highest level of information to explain the variability of house prices.


```r
# Suppress warnings
knitr::opts_knit$set(progress = FALSE)
# Load packages
library(moments)
library(GWmodel)
# Set a random seed start
set.seed(123)
# Define sampling size of house data
sampleSize <- 750
# Gather the random sampling of house data
houseSample <- house[sample(1:length(house), sampleSize), ]
# Global central tendency mean and median
summary(houseSample$price)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   81000  320375  455000  540618  650000 3800000
```

```r
# Standard deviation of house prices
sd(houseSample$price)
```

```
## [1] 357458.8
```

```r
# Range of house prices
range = max(houseSample$price) - min(houseSample$price)
print(range)
```

```
## [1] 3719000
```

```r
# Interquartile range
IQR(houseSample$price)
```

```
## [1] 329625
```

```r
# Dispersion visual
hist(houseSample$price)
```

![](https://github.com/apierson3/GIS-and-Spatial-Analysis/blob/master/Geospatial_Analysis_of_Insufficient_Sleep_in_the_Kansas_City_Region_files/figure-html/Task%20B%20Part%202-1.png)<!-- -->

```r
# Skewness of house sample
skewness(houseSample$price)
```

```
## [1] 3.258327
```

```r
# Determine good predictors
print("bedrooms")
```

```
## [1] "bedrooms"
```

```r
summary(lm(houseSample@data$price ~ houseSample@data$bedrooms))
```

```
## 
## Call:
## lm(formula = houseSample@data$price ~ houseSample@data$bedrooms)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -769116 -205861  -53446  123243 3006969 
## 
## Coefficients:
##                           Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                  37606      48038   0.783    0.434    
## houseSample@data$bedrooms   151085      13960  10.823   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 332600 on 748 degrees of freedom
## Multiple R-squared:  0.1354,	Adjusted R-squared:  0.1342 
## F-statistic: 117.1 on 1 and 748 DF,  p-value: < 2.2e-16
```

```r
print("bathrooms")
```

```
## [1] "bathrooms"
```

```r
summary(lm(houseSample@data$price ~ houseSample@data$bathrooms))
```

```
## 
## Call:
## lm(formula = houseSample@data$price ~ houseSample@data$bathrooms)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -700566 -167268  -20016  115235 2326991 
## 
## Coefficients:
##                            Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                  -25427      28525  -0.891    0.373    
## houseSample@data$bathrooms   272443      12802  21.282   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 282300 on 748 degrees of freedom
## Multiple R-squared:  0.3771,	Adjusted R-squared:  0.3763 
## F-statistic: 452.9 on 1 and 748 DF,  p-value: < 2.2e-16
```

```r
print("sqft_living")
```

```
## [1] "sqft_living"
```

```r
summary(lm(houseSample@data$price ~ houseSample@data$sqft_living))
```

```
## 
## Call:
## lm(formula = houseSample@data$price ~ houseSample@data$sqft_living)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -516665 -145481  -23408  113275 2124874 
## 
## Coefficients:
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                  -41356.650  21976.410  -1.882   0.0602 .  
## houseSample@data$sqft_living    282.602      9.744  29.002   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 245400 on 748 degrees of freedom
## Multiple R-squared:  0.5293,	Adjusted R-squared:  0.5287 
## F-statistic: 841.1 on 1 and 748 DF,  p-value: < 2.2e-16
```

```r
print("sqft_loft")
```

```
## [1] "sqft_loft"
```

```r
summary(lm(houseSample@data$price ~ houseSample@data$sqft_lot))
```

```
## 
## Call:
## lm(formula = houseSample@data$price ~ houseSample@data$sqft_lot)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -611521 -219685  -83184  106076 3237526 
## 
## Coefficients:
##                            Estimate Std. Error t value Pr(>|t|)    
## (Intercept)               5.255e+05  1.389e+04  37.838  < 2e-16 ***
## houseSample@data$sqft_lot 8.640e-01  2.814e-01   3.071  0.00221 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 355500 on 748 degrees of freedom
## Multiple R-squared:  0.01245,	Adjusted R-squared:  0.01113 
## F-statistic: 9.428 on 1 and 748 DF,  p-value: 0.002214
```

```r
print("floors")
```

```
## [1] "floors"
```

```r
summary(lm(houseSample@data$price ~ houseSample@data$floors))
```

```
## 
## Call:
## lm(formula = houseSample@data$price ~ houseSample@data$floors)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -577147 -211396  -75095  114429 3331255 
## 
## Coefficients:
##                         Estimate Std. Error t value Pr(>|t|)    
## (Intercept)               312044      36317   8.592  < 2e-16 ***
## houseSample@data$floors   156701      23329   6.717 3.67e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 347400 on 748 degrees of freedom
## Multiple R-squared:  0.05689,	Adjusted R-squared:  0.05563 
## F-statistic: 45.12 on 1 and 748 DF,  p-value: 3.674e-11
```

```r
print("waterfront")
```

```
## [1] "waterfront"
```

```r
summary(lm(houseSample@data$price ~ houseSample@data$waterfront))
```

```
## 
## Call:
## lm(formula = houseSample@data$price ~ houseSample@data$waterfront)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -458993 -219993  -84993  110007 3260007 
## 
## Coefficients:
##                             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   539993      13071  41.312   <2e-16 ***
## houseSample@data$waterfront   234507     253123   0.926    0.355    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 357500 on 748 degrees of freedom
## Multiple R-squared:  0.001146,	Adjusted R-squared:  -0.0001892 
## F-statistic: 0.8583 on 1 and 748 DF,  p-value: 0.3545
```

```r
print("view")
```

```
## [1] "view"
```

```r
summary(lm(houseSample@data$price ~ houseSample@data$view))
```

```
## 
## Call:
## lm(formula = houseSample@data$price ~ houseSample@data$view)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -537041 -208648  -73319  109115 3126402 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             508598      13139  38.708  < 2e-16 ***
## houseSample@data$view   129815      16076   8.075  2.7e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 343100 on 748 degrees of freedom
## Multiple R-squared:  0.08018,	Adjusted R-squared:  0.07895 
## F-statistic:  65.2 on 1 and 748 DF,  p-value: 2.698e-15
```

```r
print("condition")
```

```
## [1] "condition"
```

```r
summary(lm(houseSample@data$price ~ houseSample@data$condition))
```

```
## 
## Call:
## lm(formula = houseSample@data$price ~ houseSample@data$condition)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -455624 -220712  -86155  111095 3254376 
## 
## Coefficients:
##                            Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                  509870      68694   7.422 3.13e-13 ***
## houseSample@data$condition     8939      19605   0.456    0.649    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 357600 on 748 degrees of freedom
## Multiple R-squared:  0.0002778,	Adjusted R-squared:  -0.001059 
## F-statistic: 0.2079 on 1 and 748 DF,  p-value: 0.6486
```

```r
print("grade")
```

```
## [1] "grade"
```

```r
summary(lm(houseSample@data$price ~ houseSample@data$grade))
```

```
## 
## Call:
## lm(formula = houseSample@data$price ~ houseSample@data$grade)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -593985 -150520  -26627  104195 2185942 
## 
## Coefficients:
##                        Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            -1051235      57857  -18.17   <2e-16 ***
## houseSample@data$grade   208358       7478   27.86   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 250600 on 748 degrees of freedom
## Multiple R-squared:  0.5093,	Adjusted R-squared:  0.5087 
## F-statistic: 776.4 on 1 and 748 DF,  p-value: < 2.2e-16
```

```r
print("sqft_above")
```

```
## [1] "sqft_above"
```

```r
summary(lm(houseSample@data$price ~ houseSample@data$sqft_above))
```

```
## 
## Call:
## lm(formula = houseSample@data$price ~ houseSample@data$sqft_above)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -564515 -168485  -33253  113413 2540989 
## 
## Coefficients:
##                             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                 41293.73   23290.04   1.773   0.0766 .  
## houseSample@data$sqft_above   281.88      11.91  23.673   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 270500 on 748 degrees of freedom
## Multiple R-squared:  0.4283,	Adjusted R-squared:  0.4275 
## F-statistic: 560.4 on 1 and 748 DF,  p-value: < 2.2e-16
```

```r
print("sqft_basement")
```

```
## [1] "sqft_basement"
```

```r
summary(lm(houseSample@data$price ~ houseSample@data$sqft_basement))
```

```
## 
## Call:
## lm(formula = houseSample@data$price ~ houseSample@data$sqft_basement)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -507073 -205763  -76574  106260 3160926 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    474074.12   14940.14  31.732  < 2e-16 ***
## houseSample@data$sqft_basement    231.11      28.33   8.157 1.45e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 342800 on 748 degrees of freedom
## Multiple R-squared:  0.08169,	Adjusted R-squared:  0.08046 
## F-statistic: 66.54 on 1 and 748 DF,  p-value: 1.446e-15
```

```r
print("yr_built")
```

```
## [1] "yr_built"
```

```r
summary(lm(houseSample@data$price ~ houseSample@data$yr_built))
```

```
## 
## Call:
## lm(formula = houseSample@data$price ~ houseSample@data$yr_built)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -449772 -219170  -84109  111520 3248330 
## 
## Coefficients:
##                             Estimate Std. Error t value Pr(>|t|)   
## (Intercept)               -2063241.4   878020.5  -2.350  0.01904 * 
## houseSample@data$yr_built     1322.0      445.7   2.966  0.00311 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 355600 on 748 degrees of freedom
## Multiple R-squared:  0.01162,	Adjusted R-squared:  0.0103 
## F-statistic: 8.797 on 1 and 748 DF,  p-value: 0.003114
```

```r
print("yr_renovated")
```

```
## [1] "yr_renovated"
```

```r
summary(lm(houseSample@data$price ~ houseSample@data$yr_renovated))
```

```
## 
## Call:
## lm(formula = houseSample@data$price ~ houseSample@data$yr_renovated)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -549552 -219811  -83311  114789 3265189 
## 
## Coefficients:
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   534811.33   13277.50  40.280   <2e-16 ***
## houseSample@data$yr_renovated     75.28      33.85   2.224   0.0264 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 356500 on 748 degrees of freedom
## Multiple R-squared:  0.006571,	Adjusted R-squared:  0.005243 
## F-statistic: 4.947 on 1 and 748 DF,  p-value: 0.02643
```

```r
print("zipcode")
```

```
## [1] "zipcode"
```

```r
summary(lm(houseSample@data$price ~ houseSample@data$zipcode))
```

```
## 
## Call:
## lm(formula = houseSample@data$price ~ houseSample@data$zipcode)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -453574 -220148  -76051  116951 3210725 
## 
## Coefficients:
##                            Estimate Std. Error t value Pr(>|t|)   
## (Intercept)              64254123.2 23851259.7   2.694  0.00722 **
## houseSample@data$zipcode     -649.6      243.2  -2.671  0.00772 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 356000 on 748 degrees of freedom
## Multiple R-squared:  0.00945,	Adjusted R-squared:  0.008125 
## F-statistic: 7.136 on 1 and 748 DF,  p-value: 0.00772
```

```r
print("sqft_living15")
```

```
## [1] "sqft_living15"
```

```r
summary(lm(houseSample@data$price ~ houseSample@data$sqft_living15))
```

```
## 
## Call:
## lm(formula = houseSample@data$price ~ houseSample@data$sqft_living15)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -582986 -167246  -39110  104092 2806492 
## 
## Coefficients:
##                                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    -72482.30   32177.60  -2.253   0.0246 *  
## houseSample@data$sqft_living15    309.62      15.36  20.160   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 287900 on 748 degrees of freedom
## Multiple R-squared:  0.3521,	Adjusted R-squared:  0.3512 
## F-statistic: 406.4 on 1 and 748 DF,  p-value: < 2.2e-16
```

```r
print("sqft_loft15")
```

```
## [1] "sqft_loft15"
```

```r
summary(lm(houseSample@data$price ~ houseSample@data$sqft_lot15))
```

```
## 
## Call:
## lm(formula = houseSample@data$price ~ houseSample@data$sqft_lot15)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -455398 -222936  -85816  114288 3254420 
## 
## Coefficients:
##                              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                 5.293e+05  1.403e+04   37.71   <2e-16 ***
## houseSample@data$sqft_lot15 7.936e-01  3.657e-01    2.17   0.0303 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 356600 on 748 degrees of freedom
## Multiple R-squared:  0.006255,	Adjusted R-squared:  0.004927 
## F-statistic: 4.708 on 1 and 748 DF,  p-value: 0.03033
```

```r
# Predict global multiple linear regression with most significant predictors
predictgmlr <- predict(lm(houseSample@data$price ~ houseSample@data$sqft_living15 + 
    houseSample@data$sqft_basement + houseSample@data$sqft_above + houseSample@data$grade + 
    houseSample@data$view + houseSample@data$floors + houseSample@data$sqft_living + 
    houseSample@data$bedrooms + houseSample@data$bathrooms), interval = "prediction")
head(predictgmlr)
```

```
##         fit        lwr       upr
## 1  523697.3   75534.84  971859.8
## 2  580699.2  132707.49 1028690.8
## 3  307761.5 -142014.39  757537.3
## 4  118154.5 -331018.72  567327.8
## 5 1069832.0  620040.62 1519623.4
## 6  394423.0  -54944.30  843790.3
```

```r
# Predict geographically weighted regression with most significant
# predictors using a bandwidth of 10
gwr.res10 <- gwr.basic(houseSample@data$price ~ houseSample@data$sqft_living15 + 
    houseSample@data$sqft_basement + houseSample@data$sqft_above + houseSample@data$grade + 
    houseSample@data$view + houseSample@data$floors + houseSample@data$sqft_living + 
    houseSample@data$bedrooms + houseSample@data$bathrooms, data = houseSample, 
    bw = 10, kernel = "gaussian")
gwr.res10
```

```
##    ***********************************************************************
##    *                       Package   GWmodel                             *
##    ***********************************************************************
##    Program starts at: 2019-04-02 18:04:33 
##    Call:
##    gwr.basic(formula = houseSample@data$price ~ houseSample@data$sqft_living15 + 
##     houseSample@data$sqft_basement + houseSample@data$sqft_above + 
##     houseSample@data$grade + houseSample@data$view + houseSample@data$floors + 
##     houseSample@data$sqft_living + houseSample@data$bedrooms + 
##     houseSample@data$bathrooms, data = houseSample, bw = 10, 
##     kernel = "gaussian")
## 
##    Dependent (y) variable:  houseSample
##    Independent variables:  data price sqft_living15 sqft_basement sqft_above grade view floors sqft_living bedrooms bathrooms
##    Number of data points: 750
##    ***********************************************************************
##    *                    Results of Global Regression                     *
##    ***********************************************************************
## 
##    Call:
##     lm(formula = formula, data = data)
## 
##    Residuals:
##     Min      1Q  Median      3Q     Max 
## -583416 -142410  -18463  104760 1983082 
## 
##    Coefficients: (1 not defined because of singularities)
##                                     Estimate Std. Error t value Pr(>|t|)
##    (Intercept)                    -526906.00   73222.14  -7.196 1.52e-12
##    houseSample@data$sqft_living15     -10.53      19.71  -0.535  0.59315
##    houseSample@data$sqft_basement     145.90      25.28   5.771 1.16e-08
##    houseSample@data$sqft_above        168.02      21.90   7.671 5.36e-14
##    houseSample@data$grade          108194.94   12577.90   8.602  < 2e-16
##    houseSample@data$view            48389.09   11351.64   4.263 2.28e-05
##    houseSample@data$floors         -63861.84   20193.79  -3.162  0.00163
##    houseSample@data$sqft_living           NA         NA      NA       NA
##    houseSample@data$bedrooms       -23135.59   11841.45  -1.954  0.05110
##    houseSample@data$bathrooms       38683.67   18052.67   2.143  0.03245
##                                      
##    (Intercept)                    ***
##    houseSample@data$sqft_living15    
##    houseSample@data$sqft_basement ***
##    houseSample@data$sqft_above    ***
##    houseSample@data$grade         ***
##    houseSample@data$view          ***
##    houseSample@data$floors        ** 
##    houseSample@data$sqft_living      
##    houseSample@data$bedrooms      .  
##    houseSample@data$bathrooms     *  
## 
##    ---Significance stars
##    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
##    Residual standard error: 227600 on 741 degrees of freedom
##    Multiple R-squared: 0.5989
##    Adjusted R-squared: 0.5946 
##    F-statistic: 138.3 on 8 and 741 DF,  p-value: < 2.2e-16 
##    ***Extra Diagnostic information
##    Residual sum of squares: 3.838717e+13
##    Sigma(hat): 226538.6
##    AIC:  20644.41
##    AICc:  20644.77
##    ***********************************************************************
##    *          Results of Geographically Weighted Regression              *
##    ***********************************************************************
## 
##    *********************Model calibration information*********************
##    Kernel function: gaussian 
##    Fixed bandwidth: 10 
##    Regression points: the same locations as observations are used.
##    Distance metric: Euclidean distance metric is used.
## 
##    ****************Summary of GWR coefficient estimates:******************
##                                          Min.     1st Qu.      Median
##    Intercept                      -527004.695 -526959.059 -526943.956
##    houseSample.data.sqft_living15     -10.577     -10.551     -10.546
##    houseSample.data.sqft_basement    -911.740      41.235      41.259
##    houseSample.data.sqft_above       -889.599      63.364      63.391
##    houseSample.data.grade          108184.018  108201.655  108204.036
##    houseSample.data.view            48373.317   48392.479   48395.447
##    houseSample.data.floors         -63916.157  -63878.415  -63872.883
##    houseSample.data.sqft_living      -960.721     104.610     104.645
##    houseSample.data.bedrooms       -23146.957  -23141.274  -23139.733
##    houseSample.data.bathrooms       38659.646   38681.755   38687.001
##                                       3rd Qu.        Max.
##    Intercept                      -526929.122 -526822.921
##    houseSample.data.sqft_living15     -10.539     -10.525
##    houseSample.data.sqft_basement      41.298    1106.620
##    houseSample.data.sqft_above         63.427    1128.768
##    houseSample.data.grade          108205.579  108210.231
##    houseSample.data.view            48398.370   48400.904
##    houseSample.data.floors         -63866.794  -63849.668
##    houseSample.data.sqft_living       104.670    1057.645
##    houseSample.data.bedrooms       -23137.936  -23124.364
##    houseSample.data.bathrooms       38694.668   38758.150
##    ************************Diagnostic information*************************
##    Number of data points: 750 
##    Effective number of parameters (2trace(S) - trace(S'S)): -1.956639e+20 
##    Effective degrees of freedom (n-2trace(S) + trace(S'S)): 1.956639e+20 
##    AICc (GWR book, Fotheringham, et al. 2002, p. 61, eq 2.33): 20643.21 
##    AIC (GWR book, Fotheringham, et al. 2002,GWR p. 96, eq. 4.22): 20631.61 
##    Residual sum of squares: 3.838255e+13 
##    R-square value:  0.5989486 
##    Adjusted R-square value:  1 
## 
##    ***********************************************************************
##    Program stops at: 2019-04-02 18:04:34
```

```r
# Predict geographically weighted regression with most significant
# predictors using a bandwidth of 25
gwr.res25 <- gwr.basic(houseSample@data$price ~ houseSample@data$sqft_living15 + 
    houseSample@data$sqft_basement + houseSample@data$sqft_above + houseSample@data$grade + 
    houseSample@data$view + houseSample@data$floors + houseSample@data$sqft_living + 
    houseSample@data$bedrooms + houseSample@data$bathrooms, data = houseSample, 
    bw = 25, kernel = "gaussian")
gwr.res25
```

```
##    ***********************************************************************
##    *                       Package   GWmodel                             *
##    ***********************************************************************
##    Program starts at: 2019-04-02 18:04:34 
##    Call:
##    gwr.basic(formula = houseSample@data$price ~ houseSample@data$sqft_living15 + 
##     houseSample@data$sqft_basement + houseSample@data$sqft_above + 
##     houseSample@data$grade + houseSample@data$view + houseSample@data$floors + 
##     houseSample@data$sqft_living + houseSample@data$bedrooms + 
##     houseSample@data$bathrooms, data = houseSample, bw = 25, 
##     kernel = "gaussian")
## 
##    Dependent (y) variable:  houseSample
##    Independent variables:  data price sqft_living15 sqft_basement sqft_above grade view floors sqft_living bedrooms bathrooms
##    Number of data points: 750
##    ***********************************************************************
##    *                    Results of Global Regression                     *
##    ***********************************************************************
## 
##    Call:
##     lm(formula = formula, data = data)
## 
##    Residuals:
##     Min      1Q  Median      3Q     Max 
## -583416 -142410  -18463  104760 1983082 
## 
##    Coefficients: (1 not defined because of singularities)
##                                     Estimate Std. Error t value Pr(>|t|)
##    (Intercept)                    -526906.00   73222.14  -7.196 1.52e-12
##    houseSample@data$sqft_living15     -10.53      19.71  -0.535  0.59315
##    houseSample@data$sqft_basement     145.90      25.28   5.771 1.16e-08
##    houseSample@data$sqft_above        168.02      21.90   7.671 5.36e-14
##    houseSample@data$grade          108194.94   12577.90   8.602  < 2e-16
##    houseSample@data$view            48389.09   11351.64   4.263 2.28e-05
##    houseSample@data$floors         -63861.84   20193.79  -3.162  0.00163
##    houseSample@data$sqft_living           NA         NA      NA       NA
##    houseSample@data$bedrooms       -23135.59   11841.45  -1.954  0.05110
##    houseSample@data$bathrooms       38683.67   18052.67   2.143  0.03245
##                                      
##    (Intercept)                    ***
##    houseSample@data$sqft_living15    
##    houseSample@data$sqft_basement ***
##    houseSample@data$sqft_above    ***
##    houseSample@data$grade         ***
##    houseSample@data$view          ***
##    houseSample@data$floors        ** 
##    houseSample@data$sqft_living      
##    houseSample@data$bedrooms      .  
##    houseSample@data$bathrooms     *  
## 
##    ---Significance stars
##    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
##    Residual standard error: 227600 on 741 degrees of freedom
##    Multiple R-squared: 0.5989
##    Adjusted R-squared: 0.5946 
##    F-statistic: 138.3 on 8 and 741 DF,  p-value: < 2.2e-16 
##    ***Extra Diagnostic information
##    Residual sum of squares: 3.838717e+13
##    Sigma(hat): 226538.6
##    AIC:  20644.41
##    AICc:  20644.77
##    ***********************************************************************
##    *          Results of Geographically Weighted Regression              *
##    ***********************************************************************
## 
##    *********************Model calibration information*********************
##    Kernel function: gaussian 
##    Fixed bandwidth: 25 
##    Regression points: the same locations as observations are used.
##    Distance metric: Euclidean distance metric is used.
## 
##    ****************Summary of GWR coefficient estimates:******************
##                                          Min.     1st Qu.      Median
##    Intercept                      -526921.807 -526914.495 -526912.084
##    houseSample.data.sqft_living15     -10.541     -10.537     -10.536
##    houseSample.data.sqft_basement    -604.608      41.233      41.260
##    houseSample.data.sqft_above       -582.487      63.354      63.381
##    houseSample.data.grade          108193.200  108196.019  108196.401
##    houseSample.data.view            48386.570   48389.629   48390.104
##    houseSample.data.floors         -63870.537  -63864.496  -63863.611
##    houseSample.data.sqft_living      -746.398     104.606     104.643
##    houseSample.data.bedrooms       -23137.412  -23136.502  -23136.255
##    houseSample.data.bathrooms       38679.823   38683.362   38684.201
##                                       3rd Qu.        Max.
##    Intercept                      -526909.709 -526892.719
##    houseSample.data.sqft_living15     -10.535     -10.533
##    houseSample.data.sqft_basement      41.298     892.302
##    houseSample.data.sqft_above         63.417     914.423
##    houseSample.data.grade          108196.648  108197.394
##    houseSample.data.view            48390.572   48390.978
##    houseSample.data.floors         -63862.636  -63859.896
##    houseSample.data.sqft_living       104.671     750.511
##    houseSample.data.bedrooms       -23135.967  -23133.797
##    houseSample.data.bathrooms       38685.428   38695.585
##    ************************Diagnostic information*************************
##    Number of data points: 750 
##    Effective number of parameters (2trace(S) - trace(S'S)): -3.765273e+22 
##    Effective degrees of freedom (n-2trace(S) + trace(S'S)): 3.765273e+22 
##    AICc (GWR book, Fotheringham, et al. 2002, p. 61, eq 2.33): 19122.4 
##    AIC (GWR book, Fotheringham, et al. 2002,GWR p. 96, eq. 4.22): -13883125609 
##    Residual sum of squares: 3.838644e+13 
##    R-square value:  0.5989081 
##    Adjusted R-square value:  1 
## 
##    ***********************************************************************
##    Program stops at: 2019-04-02 18:04:35
```

```r
# Predict geographically weighted regression with most significant
# predictors using a bandwidth of 50
gwr.res50 <- gwr.basic(houseSample@data$price ~ houseSample@data$sqft_living15 + 
    houseSample@data$sqft_basement + houseSample@data$sqft_above + houseSample@data$grade + 
    houseSample@data$view + houseSample@data$floors + houseSample@data$sqft_living + 
    houseSample@data$bedrooms + houseSample@data$bathrooms, data = houseSample, 
    bw = 50, kernel = "gaussian")
gwr.res50
```

```
##    ***********************************************************************
##    *                       Package   GWmodel                             *
##    ***********************************************************************
##    Program starts at: 2019-04-02 18:04:35 
##    Call:
##    gwr.basic(formula = houseSample@data$price ~ houseSample@data$sqft_living15 + 
##     houseSample@data$sqft_basement + houseSample@data$sqft_above + 
##     houseSample@data$grade + houseSample@data$view + houseSample@data$floors + 
##     houseSample@data$sqft_living + houseSample@data$bedrooms + 
##     houseSample@data$bathrooms, data = houseSample, bw = 50, 
##     kernel = "gaussian")
## 
##    Dependent (y) variable:  houseSample
##    Independent variables:  data price sqft_living15 sqft_basement sqft_above grade view floors sqft_living bedrooms bathrooms
##    Number of data points: 750
##    ***********************************************************************
##    *                    Results of Global Regression                     *
##    ***********************************************************************
## 
##    Call:
##     lm(formula = formula, data = data)
## 
##    Residuals:
##     Min      1Q  Median      3Q     Max 
## -583416 -142410  -18463  104760 1983082 
## 
##    Coefficients: (1 not defined because of singularities)
##                                     Estimate Std. Error t value Pr(>|t|)
##    (Intercept)                    -526906.00   73222.14  -7.196 1.52e-12
##    houseSample@data$sqft_living15     -10.53      19.71  -0.535  0.59315
##    houseSample@data$sqft_basement     145.90      25.28   5.771 1.16e-08
##    houseSample@data$sqft_above        168.02      21.90   7.671 5.36e-14
##    houseSample@data$grade          108194.94   12577.90   8.602  < 2e-16
##    houseSample@data$view            48389.09   11351.64   4.263 2.28e-05
##    houseSample@data$floors         -63861.84   20193.79  -3.162  0.00163
##    houseSample@data$sqft_living           NA         NA      NA       NA
##    houseSample@data$bedrooms       -23135.59   11841.45  -1.954  0.05110
##    houseSample@data$bathrooms       38683.67   18052.67   2.143  0.03245
##                                      
##    (Intercept)                    ***
##    houseSample@data$sqft_living15    
##    houseSample@data$sqft_basement ***
##    houseSample@data$sqft_above    ***
##    houseSample@data$grade         ***
##    houseSample@data$view          ***
##    houseSample@data$floors        ** 
##    houseSample@data$sqft_living      
##    houseSample@data$bedrooms      .  
##    houseSample@data$bathrooms     *  
## 
##    ---Significance stars
##    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
##    Residual standard error: 227600 on 741 degrees of freedom
##    Multiple R-squared: 0.5989
##    Adjusted R-squared: 0.5946 
##    F-statistic: 138.3 on 8 and 741 DF,  p-value: < 2.2e-16 
##    ***Extra Diagnostic information
##    Residual sum of squares: 3.838717e+13
##    Sigma(hat): 226538.6
##    AIC:  20644.41
##    AICc:  20644.77
##    ***********************************************************************
##    *          Results of Geographically Weighted Regression              *
##    ***********************************************************************
## 
##    *********************Model calibration information*********************
##    Kernel function: gaussian 
##    Fixed bandwidth: 50 
##    Regression points: the same locations as observations are used.
##    Distance metric: Euclidean distance metric is used.
## 
##    ****************Summary of GWR coefficient estimates:******************
##                                          Min.     1st Qu.      Median
##    Intercept                      -526909.953 -526908.125 -526907.522
##    houseSample.data.sqft_living15     -10.536     -10.535     -10.535
##    houseSample.data.sqft_basement   -1029.859      41.234      41.261
##    houseSample.data.sqft_above      -1007.740      63.353      63.380
##    houseSample.data.grade          108194.509  108195.213  108195.309
##    houseSample.data.view            48388.457   48389.222   48389.341
##    houseSample.data.floors         -63864.018  -63862.507  -63862.286
##    houseSample.data.sqft_living      -772.200     104.613     104.642
##    houseSample.data.bedrooms       -23136.047  -23135.820  -23135.758
##    houseSample.data.bathrooms       38682.707   38683.592   38683.801
##                                       3rd Qu.        Max.
##    Intercept                      -526906.928 -526902.681
##    houseSample.data.sqft_living15     -10.534     -10.534
##    houseSample.data.sqft_basement      41.290     918.103
##    houseSample.data.sqft_above         63.410     940.222
##    houseSample.data.grade          108195.370  108195.557
##    houseSample.data.view            48389.458   48389.559
##    houseSample.data.floors         -63862.042  -63861.357
##    houseSample.data.sqft_living       104.669    1175.762
##    houseSample.data.bedrooms       -23135.686  -23135.143
##    houseSample.data.bathrooms       38684.108   38686.647
##    ************************Diagnostic information*************************
##    Number of data points: 750 
##    Effective number of parameters (2trace(S) - trace(S'S)): -3.280886e+20 
##    Effective degrees of freedom (n-2trace(S) + trace(S'S)): 3.280886e+20 
##    AICc (GWR book, Fotheringham, et al. 2002, p. 61, eq 2.33): 19122.41 
##    AIC (GWR book, Fotheringham, et al. 2002,GWR p. 96, eq. 4.22): -1667215209 
##    Residual sum of squares: 3.838699e+13 
##    R-square value:  0.5989023 
##    Adjusted R-square value:  1 
## 
##    ***********************************************************************
##    Program stops at: 2019-04-02 18:04:37
```

```r
# Predict geographically weighted regression with most significant
# predictors using a bandwidth of 75
gwr.res75 <- gwr.basic(houseSample@data$price ~ houseSample@data$sqft_living15 + 
    houseSample@data$sqft_basement + houseSample@data$sqft_above + houseSample@data$grade + 
    houseSample@data$view + houseSample@data$floors + houseSample@data$sqft_living + 
    houseSample@data$bedrooms + houseSample@data$bathrooms, data = houseSample, 
    bw = 75, kernel = "gaussian")
gwr.res75
```

```
##    ***********************************************************************
##    *                       Package   GWmodel                             *
##    ***********************************************************************
##    Program starts at: 2019-04-02 18:04:37 
##    Call:
##    gwr.basic(formula = houseSample@data$price ~ houseSample@data$sqft_living15 + 
##     houseSample@data$sqft_basement + houseSample@data$sqft_above + 
##     houseSample@data$grade + houseSample@data$view + houseSample@data$floors + 
##     houseSample@data$sqft_living + houseSample@data$bedrooms + 
##     houseSample@data$bathrooms, data = houseSample, bw = 75, 
##     kernel = "gaussian")
## 
##    Dependent (y) variable:  houseSample
##    Independent variables:  data price sqft_living15 sqft_basement sqft_above grade view floors sqft_living bedrooms bathrooms
##    Number of data points: 750
##    ***********************************************************************
##    *                    Results of Global Regression                     *
##    ***********************************************************************
## 
##    Call:
##     lm(formula = formula, data = data)
## 
##    Residuals:
##     Min      1Q  Median      3Q     Max 
## -583416 -142410  -18463  104760 1983082 
## 
##    Coefficients: (1 not defined because of singularities)
##                                     Estimate Std. Error t value Pr(>|t|)
##    (Intercept)                    -526906.00   73222.14  -7.196 1.52e-12
##    houseSample@data$sqft_living15     -10.53      19.71  -0.535  0.59315
##    houseSample@data$sqft_basement     145.90      25.28   5.771 1.16e-08
##    houseSample@data$sqft_above        168.02      21.90   7.671 5.36e-14
##    houseSample@data$grade          108194.94   12577.90   8.602  < 2e-16
##    houseSample@data$view            48389.09   11351.64   4.263 2.28e-05
##    houseSample@data$floors         -63861.84   20193.79  -3.162  0.00163
##    houseSample@data$sqft_living           NA         NA      NA       NA
##    houseSample@data$bedrooms       -23135.59   11841.45  -1.954  0.05110
##    houseSample@data$bathrooms       38683.67   18052.67   2.143  0.03245
##                                      
##    (Intercept)                    ***
##    houseSample@data$sqft_living15    
##    houseSample@data$sqft_basement ***
##    houseSample@data$sqft_above    ***
##    houseSample@data$grade         ***
##    houseSample@data$view          ***
##    houseSample@data$floors        ** 
##    houseSample@data$sqft_living      
##    houseSample@data$bedrooms      .  
##    houseSample@data$bathrooms     *  
## 
##    ---Significance stars
##    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
##    Residual standard error: 227600 on 741 degrees of freedom
##    Multiple R-squared: 0.5989
##    Adjusted R-squared: 0.5946 
##    F-statistic: 138.3 on 8 and 741 DF,  p-value: < 2.2e-16 
##    ***Extra Diagnostic information
##    Residual sum of squares: 3.838717e+13
##    Sigma(hat): 226538.6
##    AIC:  20644.41
##    AICc:  20644.77
##    ***********************************************************************
##    *          Results of Geographically Weighted Regression              *
##    ***********************************************************************
## 
##    *********************Model calibration information*********************
##    Kernel function: gaussian 
##    Fixed bandwidth: 75 
##    Regression points: the same locations as observations are used.
##    Distance metric: Euclidean distance metric is used.
## 
##    ****************Summary of GWR coefficient estimates:******************
##                                          Min.     1st Qu.      Median
##    Intercept                      -526907.758 -526906.945 -526906.677
##    houseSample.data.sqft_living15     -10.535     -10.534     -10.534
##    houseSample.data.sqft_basement    -845.206      41.237      41.264
##    houseSample.data.sqft_above       -823.087      63.355      63.383
##    houseSample.data.grade          108194.751  108195.064  108195.106
##    houseSample.data.view            48388.807   48389.146   48389.199
##    houseSample.data.floors         -63862.810  -63862.139  -63862.041
##    houseSample.data.sqft_living      -850.337     104.605     104.639
##    houseSample.data.bedrooms       -23135.794  -23135.693  -23135.666
##    houseSample.data.bathrooms       38683.241   38683.634   38683.727
##                                       3rd Qu.        Max.
##    Intercept                      -526906.413 -526904.526
##    houseSample.data.sqft_living15     -10.534     -10.534
##    houseSample.data.sqft_basement      41.298     996.240
##    houseSample.data.sqft_above         63.416    1018.359
##    houseSample.data.grade          108195.134  108195.217
##    houseSample.data.view            48389.251   48389.296
##    houseSample.data.floors         -63861.932  -63861.628
##    houseSample.data.sqft_living       104.666     991.109
##    houseSample.data.bedrooms       -23135.634  -23135.393
##    houseSample.data.bathrooms       38683.864   38684.992
##    ************************Diagnostic information*************************
##    Number of data points: 750 
##    Effective number of parameters (2trace(S) - trace(S'S)): -3.88637e+22 
##    Effective degrees of freedom (n-2trace(S) + trace(S'S)): 3.88637e+22 
##    AICc (GWR book, Fotheringham, et al. 2002, p. 61, eq 2.33): 19122.41 
##    AIC (GWR book, Fotheringham, et al. 2002,GWR p. 96, eq. 4.22): -4152340329 
##    Residual sum of squares: 3.838709e+13 
##    R-square value:  0.5989012 
##    Adjusted R-square value:  1 
## 
##    ***********************************************************************
##    Program stops at: 2019-04-02 18:04:38
```

```r
# Predict geographically weighted regression with most significant
# predictors using a bandwidth of 10
gwr.res100 <- gwr.basic(houseSample@data$price ~ houseSample@data$sqft_living15 + 
    houseSample@data$sqft_basement + houseSample@data$sqft_above + houseSample@data$grade + 
    houseSample@data$view + houseSample@data$floors + houseSample@data$sqft_living + 
    houseSample@data$bedrooms + houseSample@data$bathrooms, data = houseSample, 
    bw = 100, kernel = "gaussian")
gwr.res100
```

```
##    ***********************************************************************
##    *                       Package   GWmodel                             *
##    ***********************************************************************
##    Program starts at: 2019-04-02 18:04:38 
##    Call:
##    gwr.basic(formula = houseSample@data$price ~ houseSample@data$sqft_living15 + 
##     houseSample@data$sqft_basement + houseSample@data$sqft_above + 
##     houseSample@data$grade + houseSample@data$view + houseSample@data$floors + 
##     houseSample@data$sqft_living + houseSample@data$bedrooms + 
##     houseSample@data$bathrooms, data = houseSample, bw = 100, 
##     kernel = "gaussian")
## 
##    Dependent (y) variable:  houseSample
##    Independent variables:  data price sqft_living15 sqft_basement sqft_above grade view floors sqft_living bedrooms bathrooms
##    Number of data points: 750
##    ***********************************************************************
##    *                    Results of Global Regression                     *
##    ***********************************************************************
## 
##    Call:
##     lm(formula = formula, data = data)
## 
##    Residuals:
##     Min      1Q  Median      3Q     Max 
## -583416 -142410  -18463  104760 1983082 
## 
##    Coefficients: (1 not defined because of singularities)
##                                     Estimate Std. Error t value Pr(>|t|)
##    (Intercept)                    -526906.00   73222.14  -7.196 1.52e-12
##    houseSample@data$sqft_living15     -10.53      19.71  -0.535  0.59315
##    houseSample@data$sqft_basement     145.90      25.28   5.771 1.16e-08
##    houseSample@data$sqft_above        168.02      21.90   7.671 5.36e-14
##    houseSample@data$grade          108194.94   12577.90   8.602  < 2e-16
##    houseSample@data$view            48389.09   11351.64   4.263 2.28e-05
##    houseSample@data$floors         -63861.84   20193.79  -3.162  0.00163
##    houseSample@data$sqft_living           NA         NA      NA       NA
##    houseSample@data$bedrooms       -23135.59   11841.45  -1.954  0.05110
##    houseSample@data$bathrooms       38683.67   18052.67   2.143  0.03245
##                                      
##    (Intercept)                    ***
##    houseSample@data$sqft_living15    
##    houseSample@data$sqft_basement ***
##    houseSample@data$sqft_above    ***
##    houseSample@data$grade         ***
##    houseSample@data$view          ***
##    houseSample@data$floors        ** 
##    houseSample@data$sqft_living      
##    houseSample@data$bedrooms      .  
##    houseSample@data$bathrooms     *  
## 
##    ---Significance stars
##    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
##    Residual standard error: 227600 on 741 degrees of freedom
##    Multiple R-squared: 0.5989
##    Adjusted R-squared: 0.5946 
##    F-statistic: 138.3 on 8 and 741 DF,  p-value: < 2.2e-16 
##    ***Extra Diagnostic information
##    Residual sum of squares: 3.838717e+13
##    Sigma(hat): 226538.6
##    AIC:  20644.41
##    AICc:  20644.77
##    ***********************************************************************
##    *          Results of Geographically Weighted Regression              *
##    ***********************************************************************
## 
##    *********************Model calibration information*********************
##    Kernel function: gaussian 
##    Fixed bandwidth: 100 
##    Regression points: the same locations as observations are used.
##    Distance metric: Euclidean distance metric is used.
## 
##    ****************Summary of GWR coefficient estimates:******************
##                                          Min.     1st Qu.      Median
##    Intercept                      -526906.989 -526906.532 -526906.382
##    houseSample.data.sqft_living15     -10.535     -10.534     -10.534
##    houseSample.data.sqft_basement    -774.698      41.235      41.261
##    houseSample.data.sqft_above       -752.579      63.354      63.380
##    houseSample.data.grade          108194.836  108195.012  108195.036
##    houseSample.data.view            48388.929   48389.120   48389.150
##    houseSample.data.floors         -63862.388  -63862.010  -63861.955
##    houseSample.data.sqft_living     -1126.956     104.602     104.642
##    houseSample.data.bedrooms       -23135.706  -23135.649  -23135.634
##    houseSample.data.bathrooms       38683.428   38683.649   38683.701
##                                       3rd Qu.        Max.
##    Intercept                      -526906.233 -526905.171
##    houseSample.data.sqft_living15     -10.534     -10.534
##    houseSample.data.sqft_basement      41.301    1272.859
##    houseSample.data.sqft_above         63.420    1294.977
##    houseSample.data.grade          108195.051  108195.098
##    houseSample.data.view            48389.179   48389.204
##    houseSample.data.floors         -63861.894  -63861.723
##    houseSample.data.sqft_living       104.668     920.601
##    houseSample.data.bedrooms       -23135.616  -23135.480
##    houseSample.data.bathrooms       38683.778   38684.413
##    ************************Diagnostic information*************************
##    Number of data points: 750 
##    Effective number of parameters (2trace(S) - trace(S'S)): 2.395373 
##    Effective degrees of freedom (n-2trace(S) + trace(S'S)): 747.6046 
##    AICc (GWR book, Fotheringham, et al. 2002, p. 61, eq 2.33): 20642.64 
##    AIC (GWR book, Fotheringham, et al. 2002,GWR p. 96, eq. 4.22): 20631.38 
##    Residual sum of squares: 3.838713e+13 
##    R-square value:  0.5989008 
##    Adjusted R-square value:  0.5976139 
## 
##    ***********************************************************************
##    Program stops at: 2019-04-02 18:04:39
```
