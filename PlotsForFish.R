install.packages("RColorBrewer")
install.packages("devtools")
install.packages("flexclust")
devtools::install_github("jaredhuling/jcolors")

#---------------------------------------------------
#calling relevant libraries
#---------------------------------------------------
library(RColorBrewer)
library(jcolors)
library(flexclust)
library(cluster)

#---------------------------------------------------
#loading the .csv file 
#---------------------------------------------------
fish <- read.csv(file.choose(),header=T)
attach(fish)



#---------------------------------------------------
#setting species as a factor
#---------------------------------------------------
species <- as.factor(fish[,2])

#-----------------------------------------------
# Produce a histogram for each of the variables
#-----------------------------------------------
hist(fish[,3],
     main= 'Length from the nose to the beginning of the tail (cm)
     26015580',
     xlab='length (in cm)',
     col='skyblue')

hist(fish[,4],
     main= 'Length from the nose to the notch of the tail (cm)
     26015580',
     xlab='length (in cm)',
     col='deeppink')

hist(fish[,5],
     main='Length from the nose to the end of the tail (cm)
     26015580',
     xlab='length (in cm)',
     col='coral')

hist(fish[,6],
     main='Maximal height, as a percentage of the total length(Len3)
     26015580',
     xlab='Percentage (%)',
     col='forestgreen')

hist(fish[,7],
     main='Maximal width, as a percentage of the total length (Len3)
     26015580',
     xlab='Percentage (%)',
     col='purple')

hist(fish[,9],
     main='Weight of the fish (grams)
     26015580',
     xlab='Weight (grams)',
     col='darkred')

#---------------------------------------------------
#Using Pairs to create a scatter graph for variables
#---------------------------------------------------

pairs(fish[c(3,4,5,6,7,9)], main = "Fish Scatter plot
  26015580",
      pch = 19,col = species, lower.panel = NULL)

#---------------------------------------------------
#show legend for scatter plot
#---------------------------------------------------

legend("bottomleft",  title="Colour Legend",
       c("Bream- Black ","Parkki- Red","Perch- Green","Pike- Blue","Roach-Cyan","Smelt- Purple","Whitewish-Yellow"))
#---------------------------------------------------
#adding the segment plot using the star function
#---------------------------------------------------

fish[,2]<-as.factor(fish[,2])

stars(main = "Fish data 26015580",fish[c(3,4,5,6,7,9)],draw.segments = T,key.loc = c(12, 1.5))

# Create a distance matrix
fish.dist<-dist(fish[,3:7])

# Perform the clustering
fish.hclust<-hclust(fish.dist)

# Produce a default dendrogram
plot(fish.hclust, labels= fish[,2])
