### Home Assignment GP Part II HT21 ###

library(devtools) #have all these necessary packages downloaded and ready for use
library(dplyr)
library(factoextra)
library(fdapace)
library(forcats)
library(fpca)
library(FactoMineR)
library(psych)
library(psychtools)
library(readr)
library(tidyverse)

PAQ_Garlli <- read.csv("C:/Users/garll/Downloads/PAQ_Garlli.txt", sep="") #import dataset and read it as a file
View(PAQ_Garlli)

df <- PAQ_Garlli%>%
  pivot_wider(id_cols = id, names_from = var, values_from = value) #divides variables horizontally to make it easier to read

new_df <- df[4:12] #get rid of id, var, sex
new_df <- na.omit(new_df) #get rid of na's to make it a dataset with numerical variables only
view(new_df)

new_df_cov <- cov(new_df) #build a covariance matrix

summary(new_df_cov) #get some summary statistics

plot(new_df_cov) #we plot it (Fig 1)

pca <- princomp(new_df_cov,scores = TRUE, cor = TRUE) #run pca 
pcasum <- summary(pca)
str(pcasum)
summary(pcasum)

pcasum$sdev #obtain SD of each variable
pcasum #allows us to see different components in the data set
loadings(pca)

pca[1:9]

C1 <-loadings(pca)[,1] #the first principal component for every survey answer proportion
pca$scale
pca$center 
screeplot(pca, type = 'line', main = 'Scree Plot') #plot it to explain how much variation is within each principal component (Fig 2)

biplot(pca) #biplot of the first two principal components (Fig 3)

plot(pca$scores) #Plot to show the amount of variance explained on the comp 1 & 2 when PCA has been completed
#Fig 4
predict(pca)[,1] #shows how the first two components account for around 70%
