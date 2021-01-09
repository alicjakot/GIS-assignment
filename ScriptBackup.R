library(sf)
library(here)
library(tmap)
library(tidyverse)
library(leaflet)
library(leafgl)
library(mapdeck)
library(RColorBrewer)
library(dplyr)
library(stringr)
library(ggplot2)

### DATA ###

# Reading in data

# Camden outline
LondonBoroughs <- st_read(here("data",
                               "statistical-gis-boundaries-london",
                               "ESRI",
                               "London_Borough_Excluding_MHW.shp"))

# here, we create a CamdenOutline to clip other data into it
CamdenOutline <- LondonBoroughs %>% 
  filter(., NAME=="Camden")
CamdenOutline
qtm(CamdenOutline)

#check the projection
print(CamdenOutline)

# reproject Camden
CamdenOutlineProjected <- CamdenOutline %>%
  st_transform(.,27700)



# Camden streets - pavement width
# reading in file
streetsGJSON <- st_read(here::here("data", "streets3.geojson"))

# check the projection
print(streetsGJSON)

# change the projection
streets <- streetsGJSON %>%
  st_transform(.,27700)
print(streets)

# get rid of the ones outside Camden
pavementsWidth <- streets[CamdenOutlineProjected,]

# see the result
tmap_mode("view")
qtm(pavementsWidth)



# Camden street lights
streetLights <- st_read(here::here("data",
                                   "Camden Street Lighting",
                                   "geo_export_f77a2cc3-40d7-403c-84fe-64be970169bf.shp"))


# Camden public toilets
publicToiletsCSV <- read_csv(here::here("data",
                                        "Public_Conveniences_In_Camden_Map.csv"))

publicToilets <- st_as_sf(publicToiletsCSV,
                          coords = c("Longitude","Latitude"),
                          crs = 4326)
qtm(publicToilets)


# trees in Camden
treesCamdenCSV <- read_csv(here::here("data",
                                      "Trees_In_Camden.csv"))



### DATA CLEARING AND PREPERATION ###


# Pavements Width Data

# check if there are any nulls/nas

# how many
sum(is.na(pavementsWidth))
sum(is.na(pavementsWidth$foW))
sum(is.na(pavementsWidth$DISTNAME))
sum(is.na(pavementsWidth$geometry))

# add a new column to see a proportion of pavement to a street
# and create a new dataframe to store all the data needed for the analysis
completeData <- transform(pavementsWidth, propFowTow = foW/toW)
completeData

# calculate the length of a street and add it to completeData dataset
completeData$streetLength <- st_length(completeData$geometry)

# check whether a new column is numeric
is.numeric(completeData$streetLength)

# says its numeric but its not 
completeData$streetLength <- as.numeric(completeData$streetLength)

# now it is! let's round it up
# rounding the columns
completeData <- completeData %>% 
  mutate_if(is.numeric, ~round(., 2))
completeData

# check for nans
sum(is.na(completeData$streetLength))

# see how it looks like
tm_shape(completeData)+
  tm_lines("propFowTow", 
           palette = "RdYlGn",
           direction=-1)


### Public Toilets Data

publicToilets

# check if there are null values for street name
sum(is.na(publicToilets$Street))

# remove values where the street name is not known
publicToilets <- publicToilets %>% 
  drop_na(Street)

# add public Toilets to complete dataset
# select only columns I want to add
toiletsStreet <-  publicToilets %>% 
  select(Name,Street)

# do the join, not sure if necessary when I'm
# doing the thing below
completeData <- left_join(completeData,toiletsStreet, by= c("DISTNAME"="Street"))

completeData

# add a new column 0-1 if there is a toilet or not
completeData$publicToilet <- ifelse(is.na(completeData$Name),0, 1)
completeData

# add a new column with public toilets per 10 m of the street
completeData$toilets10m <- (completeData$publicToilet*10)/completeData$streetLength



### Street lights data
qtm(streetLights)

# check the projection
print(streetLights)

# change the crs of streetLights
streetLights <- streetLights %>%
  st_transform(.,27700)
print(streetLights)

qtm(streetLights)
# crs seems okay now so can start merging it to our complete data set, the problem is, 
# so many streets have lights? what's important

# HOW MANY STREET LIGHTS ON THE STREET

# extract the unique street names and check if worked
streetNames <-  c(unique(streetLights$street_nam))
streetNames

# check if there are streets with the same names but different ids
library(plyr)
count(completeData,c('id','DISTNAME'))

# how many times the street names appear in streetLights dataset
count(streetLights, c('street_nam'))


# trying to merge street lights and completeData 
# convert it to a df
numberOfStreetLightsDF <- data.frame(count(streetLights, c('street_nam')))

# tried to join the dfs but didnt work out
# so I thought I need to changes names
numberOfStreetLightsDF$street_nam=tolower(numberOfStreetLightsDF$street_nam)
completeData$DISTNAME=tolower(completeData$DISTNAME)

numberOfStreetLightsDF

# trying again
completeData <- completeData %>% 
  left_join(numberOfStreetLightsDF, by = c("DISTNAME" = "street_nam"))

print(completeData)

# check for nas in number of streetLights
sum(is.na(completeData$freq))

# there is a lot of nas in number of street lights, so let's replace
# them with 0
completeData$freq <- completeData$freq %>% 
  replace_na(0)

sum(is.na(completeData$freq))

# I think it worked!!  
# now need to calculate how many street lights are per 10 m of a street

# adding a field - how many street lights per 10 m?
completeData$streetLights10m <- (completeData$freq*10)/completeData$streetLength

completeData$streetLights10m


### TREES

# check for nas
sum(is.na(treesCamdenCSV$Location))

# okay, theres a lot (72) so lets remove them
treesCamdenCSV <- treesCamdenCSV %>%
  drop_na(Location)

treesCamden <- treesCamdenCSV %>%
  st_as_sf(., coords = c("Longitude", "Latitude"),
           crs = 4326) %>%
  st_transform(., 27700)

# qtm(treesCamden)

# let's remove the ones outside Camden
treesCamden <- treesCamden[CamdenOutlineProjected,]

# making a string of street names from completeData
completeStreetNames <-  c(unique(completeData$DISTNAME))
completeStreetNames

# lowering the names of sites in treesCamden
treesCamden$`Site Name`=tolower(treesCamden$`Site Name`)
treesCamden

# select trees only by the streets
# streetTrees <- str_detect(treesCamden$`Site Name`, completeStreetNames)
streetTrees <- data.frame(treesCamden$`Site Name`,treesCamden$`Number Of Trees`)
streetTrees

# aggregating, so know how many trees are there on the street
streetTrees <- aggregate(streetTrees$treesCamden..Number.Of.Trees., by=list(site=streetTrees$treesCamden..Site.Name.), FUN=sum)

# select only rows with street names with compledeDataset street names
streetTrees2 <- streetTrees %>% 
  filter(str_detect(streetTrees$site, paste(completeStreetNames,collapse = '|')))

completeStreetNames
streetTrees2

completeData <- left_join(completeData,streetTrees2, by= c("DISTNAME"="site"))

# change nas in trees to zeroes
completeData$x <- ifelse(is.na(completeData$x),0, completeData$x)

# adding a column with a number of trees per 10m
completeData$trees10m <- (completeData$x*10)/completeData$streetLength


### THE WHOLE DATASET WORK ###

completeData

# renaming the column names so we know what we're looking at
completeData <- completeData %>% 
  dplyr::rename(
    streetName = DISTNAME,
    publicToiletName=Name,
    numberOfStLights=freq,
    numberOfTrees=x
  )

#get rid of the columns we dont need
completeDataNumbers <- subset(completeData, select=-c(id, ROADNUMBER, CLASSIFICA, publicToiletName, publicToilet))

# check if any nas
completeDataNumbers %>% 
  summarize_all(funs(sum(is.na(.))))

# there are nas in street names, but I'm not going to remove 
# them, as they still have geometry
completeDataNumbers

# okay all looks good soo let's just change the name
# and subset data needed for clustering
dataFinal <- subset(completeDataNumbers, select=-c(foW, caW, toW, streetLength,numberOfStLights, numberOfTrees))
dataFinal

class(dataFinal)
typeof(dataFinal)

# check the data types
str(dataFinal)

#all looks good! let's move on tooooo...

### DISTRUBUTIONS!

# rounding the columns
dataFinal <- dataFinal %>% 
  mutate_if(is.numeric, ~round(., 2))
dataFinal

# need to get rid of infinite values,
# again, try to do it within a function
# this is pathetic
sum(!is.finite(dataFinal$streetLights10m))
sum(!is.finite(dataFinal$propFowTow))
sum(!is.finite(dataFinal$trees10m))
sum(!is.finite(dataFinal$toilets10m))

# there is one infinite value here let's remove it

#changing inf to na
dataFinal <- dataFinal %>%
  mutate_if(is.numeric, list(~na_if(., Inf)))

#dropping na
dataFinal<- dataFinal %>%
  drop_na('streetLights10m')

# check if worked
sum(!is.finite(dataFinal$streetLights10m))

# yup, I'm awesome
# let's move on with distributions

# histogram with distribution
propFowTowHist <- ggplot(data=dataFinal, aes(`propFowTow`)) + 
  geom_histogram(
    color="#E69F00",
    fill="white")

propFowTowHist

streetLights10mHist <- ggplot(data=dataFinal, aes(`streetLights10m`)) + 
  geom_histogram(
    color="#E69F00",
    fill="white")

streetLights10mHist

treesHist <- ggplot(data=dataFinal, aes(`trees10m`)) + 
  geom_histogram(
    color="#E69F00",
    fill="white")

treesHist

toiletsHist <- ggplot(data=dataFinal, aes(`toilets10m`)) + 
  geom_histogram(
    color="#E69F00",
    fill="white")

toiletsHist

# to see the histogram next to each other
library(gridExtra)
grid.arrange(propFowTowHist, streetLights10mHist, treesHist, toiletsHist, ncol=2)

# cool distribution plots
library(cluster.datasets)
plot1 <- dataFinal %>% 
  ggplot(aes(x = "streets", y = propFowTow)) + 
  geom_jitter(width = .025, height = 0, size = 2, alpha = .5, color = "blue") +
  labs(x = "", y="proportion of footpath to total street width")
plot1

plot2 <-  dataFinal %>% 
  ggplot(aes(x = "streets", y = streetLights10m)) + 
  geom_jitter(width = .02, height = 0, size = 2, alpha = .6,  color = "orange") +
  labs(x = "", y="street lights per 10m of a street")

plot3 <- dataFinal %>% 
  ggplot(aes(x = 'streets', y = trees10m)) + 
  geom_jitter(width = .02, height = 0, size = 2, alpha = .6,  color = "green") +
  labs(x = "", y="trees per 10m of a street")

plot4 <-  dataFinal %>%
  ggplot(aes(x = 'streets', y = toilets10m)) + 
  geom_jitter(width = .02, height = 0, size = 2, alpha = .6,  color = "red") +
  labs(x = "", y="public toilets per 10m of a street")

grid.arrange(plot1, plot2, plot3, plot4)

# propFowTow seems to be normally distributed, 
# unlike street Lights, trees and toilets

### DATA TRANSFORMATION ###

# log transformation, getting rid of the skewness
streetLights10mLogHist <- ggplot(dataFinal, aes(x=log(streetLights10m))) + 
  geom_histogram(color="#E69F00",
                 fill="white") +
  geom_density()

streetLights10mLogHist

# and trees

treesLogHist <- ggplot(dataFinal, aes(x=log(trees10m))) + 
  geom_histogram(color="#E69F00",
                 fill="white") +
  geom_density()

treesLogHist

propLogHist <- ggplot(dataFinal, aes(x=log(propFowTow))) + 
  geom_histogram(color="#E69F00",
                 fill="white") +
  geom_density()

propLogHist

# clusteringDataset$logStreetLights <- log(clusteringDataset$streetLights10m)

# tukey's ladder
library(car)
symbox(~toilets10m, 
       dataFinal, 
       na.rm=T,
       powers=seq(-3,3,by=.5))

streetLights10mTukeyHist <- ggplot(dataFinal, aes(x=(streetLights10m)^-1)) + 
  geom_histogram()

streetLights10mTukeyHist


# so log is better


### DATA NORMALISATION ###

# a new dataframe with new column with logged streetLights and trees
clusteringDataset <- dataFinal


#trying without a log
clusteringDataset2 <- data.frame(clusteringDataset$propFowTow, clusteringDataset$streetLights10m,
                                 clusteringDataset$trees10m,clusteringDataset$toilets10m)
clusteringDataset2

summary(clusteringDataset2)

# perform normalisation here
clusteringDatasetNormalised <- data.frame(scale(clusteringDataset2, center = TRUE, scale = TRUE))
clusteringDatasetNormalised
summary(clusteringDatasetNormalised)

# check for standard deviation

# correllation matrix - checking if not correlated
# need to work on the graphics here
library(corrplot)
source("http://www.sthda.com/upload/rquery_cormat.r")

rquery.cormat(clusteringDatasetNormalised)

# the variables do not seem correlated, so let's
# proceed with the analysis

clusteringDatasetFinal <- data.frame(dataFinal$streetName,clusteringDatasetNormalised)
clusteringDatasetFinal

# renaming the column names 
clusteringDatasetFinal <- clusteringDatasetFinal %>% 
  dplyr::rename(
    streetName = dataFinal.streetName,
    propFowTow=clusteringDataset.propFowTow,
    toilets10=clusteringDataset.toilets10m,
    streetLights10m=clusteringDataset.streetLights10m,
    trees10m=clusteringDataset.trees10m
  )

### DATA ANALYSIS ###

## K-means ##
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(NbClust) # for a number of clusters

# choosing the right value of k
# Elbow method
fviz_nbclust(clusteringDatasetNormalised, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2) + # add line for better visualisation
  labs(subtitle = "Elbow method") # add subtitle

# the number of clusters suggested here is 5

# Silhouette method
fviz_nbclust(clusteringDatasetNormalised, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")
# according to the silhuette method, the number of clusters should be 9

# NbClust method - provides 30 indices for choosing the best number of clusters
# change the text here, as its all copied
nbclust_out <- NbClust(
  data = clusteringDatasetNormalised,
  distance = "euclidean",
  min.nc = 2, # minimum number of clusters
  max.nc = 10, # maximum number of clusters
  method = "kmeans") # one of: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid", "kmeans"

# this one suggests 5

# create a dataframe of the optimal number of clusters
nbclust_plot <- data.frame(clusters = nbclust_out$Best.nc[1, ])
# select only indices which select between 2 and 5 clusters
nbclust_plot <- subset(nbclust_plot, clusters >= 2 & clusters <= 10)

# create plot
ggplot(nbclust_plot) +
  aes(x = clusters) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  labs(x = "Number of clusters", y = "Frequency among all indices", title = "Optimal number of clusters") +
  theme_minimal()

# suggests 5
# 5 it is then!

### CLUSTERING ###

set.seed(123)
clustered <- kmeans(clusteringDatasetNormalised, 5, nstart = 200)
print(clustered)

fviz_cluster(clustered, clusteringDatasetNormalised)

dataFinal$cluster <- clustered$cluster

# a map showing clusters
tm_shape(dataFinal)+
  tm_lines("cluster", 
           palette = "Oranges",
           direction=1)


# change the projection to use the leaflet package
# https://github.com/rstudio/leaflet/issues/139

dataFinal <- dataFinal %>%
  st_transform(.,4326)

### EXPLORING CLUSTERS ###

library(GGally)
library(plotly)

dataFinal$cluster <- as.factor(clustering$cluster)

p <- ggparcoord(data = dataFinal,
                columns = c(3:6), 
                mapping=aes(color=as.factor(cluster))) +
  scale_color_discrete("cluster") +
  labs(x = "variable", 
       y = "value", 
       title = "Clustering")

ggplotly(p)

meanClusterValues <- dataFinal %>% 
  group_by(cluster) %>% 
  summarise(
    propFowTow = mean(propFowTow),
    streetLights10m=mean(streetLights10m),
    trees10m=mean(trees10m),
    toilets10m=mean(toilets10m))

meanClusterValues

plotMeanProp <- ggplot(meanClusterValues, aes(x = cluster, y = propFowTow)) +
  geom_col(position = "dodge")

plotStreetLights <- ggplot(meanClusterValues, aes(x = cluster, y = streetLights10m)) +
  geom_col(position = "dodge")

plotTrees <- ggplot(meanClusterValues, aes(x = cluster, y = trees10m)) +
  geom_col(position = "dodge")

plotToilets <- ggplot(meanClusterValues, aes(x = cluster, y = toilets10m)) +
  geom_col(position = "dodge")

library(gridExtra)
# grid.arrange(plotMeanProp,plotStreetLights,plotTrees,plotToilets)
# plotsMeanValues <- c(p,plotAll,plotMeanProp,plotStreetLights,plotTrees,plotToilets)

# a different one
plotAll <- ggparcoord(data = meanClusterValues,
                      columns = c(2:5), 
                      mapping=aes(color=as.factor(cluster))) +
  scale_color_discrete("cluster") +
  labs(x = "variable", 
       y = "value", 
       title = "Clustering")
plotAll

ggplotly(plotAll)
grid.arrange(p,plotAll,plotMeanProp,plotStreetLights,plotTrees,plotToilets)


# cluster rank

dataFinal <- dataFinal %>%
  mutate(clusterRank = case_when(cluster==1 ~ 2,
                                 cluster==2 ~ 1,
                                 cluster==3 ~ 5,
                                 cluster==4 ~ 3,
                                 cluster==5 ~ 4))

pal1 <- colorBin(
  palette = "YlOrRd", 
  domain=dataFinal$clusterRank, 
  bins=breaks)

pal <- colorNumeric(
  palette = "Blues",
  domain = dataFinal$clusterRank)

pal2 <- colorFactor(
  palette = c("yellow", "purple","red"),
  domain = dataFinal$clusterRank)



# m <- leaflet(dataFinal) %>% 
#   addTiles() %>% 
#   setView( lng = -0.1433, lat = 51.5406 , zoom = 13 ) %>% 
#   addProviderTiles("Esri.WorldImagery") %>% 
#                    # options = providerTileOptions(opacity = 0.4)) %>% 
#   addProviderTiles("CartoDB.DarkMatter",
#                    options = providerTileOptions(opacity = 0.7)) %>% 
#   #add streets clustered and ranked
#   addPolylines(
#     fillColor = dataFinal$clusterRank,
#     weight = 2.5,
#     opacity = 0.7,
#     color =~pal2(clusterRank)) %>% 
#   # add legend
#   addLegend(pal = pal2, 
#             values = ~`clusterRank`,
#             title = "Street Clusters", 
#             opacity = 1)


m



# tiles
# providers$Stamen.Toner
# Esri.WorldImagery

# gruped by clusters
# clusterGroups <- dataFinal %>% 
#   mutate(group = case_when(cluster==1 ~ "cluster 1",
#                            cluster==2 ~ "cluster 2",
#                            cluster==3 ~ "cluster 3",
#                            # cluster==4 ~ "cluster 4",
#                            TRUE~"Other"))





# mapbox attempt
token <- 'pk.eyJ1IjoiYWxpY2pha290YXJiYSIsImEiOiJja2pteTdoOWU2aGg4MnlsZzIxeWE3YTZpIn0.ViQNik4UnIFZLXHI460uZw'

ms <- mapdeck_style("dark")

mapdeck(style=ms, token = token) %>%
  add_path(
    data = dataFinal,
    stroke_colour = "clusterRank",
    palette = colorRamp(c("#FFB17A", "#B967FF", "#05FFA1","blue","red"))( (1:256)/256 )
  ) %>%
  mapdeck_view(
    location = c(-0.1586, 51.5406),
    zoom = 11.5
  )



### CLUSTERING VALIDATION
# based on this website
# https://towardsdatascience.com/clustering-analysis-in-r-using-k-means-73eca4fb7967
# 
# sil <- silhouette(clustered$cluster, dist(clusteringDatasetNormalised))
# fviz_silhouette(sil)




##### CHECKING FOR CRIME LEVELS AND COMPARING IT TO CLUSTERS ####

# read in crime data
allCrimeCSV <- read_csv(here::here("data", 
                                   "On_Street_Crime_in_Camden.csv"))  
allCrime <- st_as_sf(allCrimeCSV,
                     coords = c("Longitude","Latitude"),
                     crs = 27700)

# filter violance and sexual offences
sexOff <- allCrime %>% 
  filter(str_detect(Category,"Violence and sexual offences"))

sexOff <- data.frame(sexOff$Category,sexOff$`Street Name`)

# add a column with a number, so I can aggregate
sexOff$number <- 1

sexOffCount <- aggregate(sexOff$number, by=list(site=sexOff$sexOff..Street.Name.), FUN=sum) 

# lowering letters so I can smoothly join
sexOffCount$site <- tolower(sexOffCount$site)

# trying to do the join
library(fuzzyjoin)
library(stringr)

sexOffCountJoin <- sexOffCount %>% 
  fuzzy_inner_join(dataFinal, by = c("site" = "streetName"), match_fun = str_detect)

sexOffCountJoin <- subset(sexOffCountJoin, select=-c(site))
sexOffCountJoin

# renaming the crime column
sexOffCountJoin <- sexOffCountJoin %>% 
  dplyr::rename(
    numberOfOffences = x)

# group see how many crimes in each cluster
crimeInClusters <- sexOffCountJoin %>% 
  group_by(cluster) %>% 
  summarise(
    crime = sum(numberOfOffences))
crimeInClusters

sexOffCorr <- subset(sexOffCountJoin, select=-c(fid, streetName,geometry))

# see if there is a correlation between crime and clusters and other variables
library(ggcorrplot)

#correlation matrix
corr <- round(cor(sexOffCorr), 2)
p.mat <- cor_pmat(sexOffCorr)
head(p.mat[, 1:4])
ggcorrplot(corr,hc.order = TRUE, type = "lower",
           outline.col = "white", lab = TRUE,
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))
