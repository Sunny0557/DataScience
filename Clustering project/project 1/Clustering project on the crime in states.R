## instaling the packages ##
install.packages("readr")
library(readr) ## Invoking packages##

#### IMPORTING THE FILE INTO R-STUDIO ##
crime <- read.csv(file.choose())

View(crime)   ## view the data imported##

summary(crime) ## Summarizing the values ##

### NORMALIZING THE DATA SET SO THE VALUES SHOULD BE STANDARDIZED AND THE RANGE OF THE VALUES SHOULD BE IN BETWEEN 0-1 ##
crime.norm <- scale(crime[,2:5], center = TRUE, scale = TRUE)
View(crime.norm)      ## NORMALIZED_DATA ##
summary(crime.norm)

## FINDING THE DISTANCE BETWEEN THE VARIABLES WITH THE EULIDEAN DISTANCES  ##
crime.dist <- dist(crime.norm, method = "euclidean")
crime.dist
round(crime.dist, digits = 3) ## the distance calculated values are retrived ##

### After the distances are derived with the function then the data is under process and we retreive the inferences with the distances provided ##
## with the retreived distances we do the hirerichal clustering and then the data is been clustered##
##?hclust##Now we  fit the model ##
 fit_model <- hclust(crime.dist, method = "complete",members = NULL)
fit_model 
plot(fit_model, labels = crime$X) ## plotting the values with the name of states ##
plot(fit_model,labels = crime$X, hang = -1)## alliging them ##

#### NOW TRYING TO CHECK THE MODEL WITH THE AVERAGE LINKAGE METHOD ##
fit_model1 <- hclust(crime.dist, method = "average",members = NULL)
fit_model1 
plot(fit_model1, labels = crime$X)
plot(fit_model1,labels = crime$X, hang = -1)

## NOW trying the model with single linkage ##
fit_model2 <- hclust(crime.dist, method = "single",members = NULL)
fit_model2 
plot(fit_model2, labels = crime$X)
plot(fit_model2,labels = crime$X, hang = -1)

## CLUSTERING MEMBERSHIP ##
## grouping the clusters into 4 groups 
## grouping the data on the complete linkage model ##
member.c <- cutree(fit_model, 4)
## grouping the data on the average linkage model ##
member.a <- cutree(fit_model1,4)
## grouping the data on the single linkage model ##
member.s <-cutree(fit_model2,4)
## ccomparing and contrasting on twoe models ##
table(member.a,member.c)

### NOw we aggregate find the means and infer some conclusions ###
aggregate(crime.norm, list(member.c), mean)
aggregate(crime.norm, list(member.a), mean)
aggregate(crime, list(member.a), mean)
attributes(crime.norm)
### by using screeplot we can check and the plot and infer about the crimerate and those neededn to be cliustered in k number of cluaters###
View(crime.norm)
wss <- (nrow(crime.norm))*sum(apply(crime.norm,2,var))
for(i in 1:5) wss[i] <- sum(kmeans(crime,centers = i,)$withinss)
plot(1:5, wss,type = "b", xlab = "NUMBER OF CLUSTER", ylab = "WITHIN SUM OF SQUARES")

install.packages("kselection")
library(kselection)
 crime.ksel <- kselection(crime.norm, parallel = TRUE, k_threshold = 0.9, max_centers = 4)
View(crime.ksel) 
plot(crime.ksel)  ## the model recommend to proceeed with 2 clusters for the best result###
?kselection


library(parallel)
crime.ksel <- kselection(crime.norm, k_threshold = 0.9, max_centers = 4)
crime.ksel


## K-MEANS CLUSTERING ####
## construcring the k-means clustering ## for the 
crime.clust <- kmeans(crime.norm,4)
crime.clust##to get the  values for the clustering ##
crime.clust$cluster ## To know the cluster value ##
crime.clust$centers ##T know the center values 
plot(Assault~UrbanPop, crime, col=crime.clust$cluster)##  CONTRAST THE PLOT OF THE CRIME ON THE STATES##
plot( Murder~Rape, crime, col=crime.clust$cluster, pos = 2, cex=0.5)##  CONTRAST THE PLOT OF THE CRIME ON THE STATES##


