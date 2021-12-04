### Home Assignment HT21 GP ###

library(MASS)
library(permute)
library(smacof)
library(vegan)
library(usethis)
library(tidyverse)

Nations <- read.delim("C:/Users/garll/Downloads/Nations.txt") #import dataset and open
   View(Nations)

country <- data.matrix(Nations, rownames.force = NA) #name every variable
rownames(country) <- c("Brazil", "Congo", "Cuba", "Egypt", "France" , "India", "Israel", "Japan", "China", "UdSSR", "USA", "Yugoslavia")
   
dist_nations <- dist(Nations) #find the distance between the nations
mds <- cmdscale(dist_nations, eig = TRUE, k = 2) #plug it into the cmdscale ()
x <- mds$points[,1]
y <- mds$points[,2]

plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
          main="Nonmetric MDS", type="n") #plot it to find the distance
     text(x, y, labels = row.names(country), cex=.7)

lam<- mds$eig     #find eigenvalues for distance
lam
   
cumsum<-cumsum(abs(lam))/sum(abs(lam)) #the cumulative sum of the columns

cumsum(lam^2)/sum(lam^2)
