# Cluster -----------------------------------------------------------------

setwd("~/Dropbox/Project/Code")
load("~/Dropbox/Project/Code/Data.RData")

preload("tidyverse")
preload("readxl")
preload("dplyr")
preload("zoo")
preload("stringr")
preload("cluster")
preload("factoextra")
preload("NbClust")
preload("gplots")
preload("ggplot2")
preload("ggdendro")
preload("car")
preload("RColorBrewer")
preload("ggrepel")
preload("tidyverse")
preload("dendextend")
preload("randomForest")


#Select variables
data.cluster <- all %>%
  select(Category,Allegation,Finding,Outcome,District,officers_involved,DistrictsWorked,
         OfficerGender,OfficerRace,Rank,OfficerAge,officers_witness,WitnessGender,WitnessRace,
         comp_witness,IncidentDaysAfterAppt,allegation_count) %>%
  filter(Finding!="Unknown"&Outcome!="Unknown")

#Check variables
data.cluster %>%
  glimpse()
data.cluster$IncidentDaysAfterAppt <- as.numeric(data.cluster$IncidentDaysAfterAppt)

#Remove NAs
data.cluster <- na.omit(data.cluster)

data.cluster <- data.cluster %>%
  mutate(Category = factor(Category),
         Allegation = factor(Allegation),
         Finding = factor(Finding),
         Outcome = factor(Outcome),
         District = factor(District),
         OfficerGender = factor(OfficerGender),
         OfficerRace = factor(OfficerRace),
         WitnessGender = factor(WitnessGender),
         WitnessRace = factor(WitnessRace))

#Build matrix of frequent levels for allegation
tab <- data.frame(table(data.cluster$Allegation))
names(tab) <- c("Allegation","Cases")
allegation <- tab[,1]
#tab <- data.frame(table(data.cluster$Allegation))
#names(tab) <- c("Allegation","Cases")
matrix <- NULL
for(i in 1:length(allegation)){
  a <- as.character(allegation[i])
  for(j in 1:ncol(data.cluster)){
    if(names(data.cluster)[j]=="Allegation") next
    if(is.numeric(data.cluster[,j])){
      a <- cbind(a,mean(data.cluster[data.cluster$Allegation==allegation[i],j]))
    }
    else{
    t <- as.data.frame(table(data.cluster[data.cluster$Allegation==allegation[i],j]))
    t <- t[order(t[,2],decreasing = T),] %>% mutate(pct = prop.table(Freq))
    t <- cbind(as.character(t[1,1]),t[1,3])
    a <- cbind(a,t)}
  }
  a <- as.data.frame(a)
  names(a) <- c("Allegation","Category","Category.freq",
                "Finding","Finding.freq",
                "Outcome","Outcome.freq",
                "District","District.freq",
                "officers_involved","DistrictsWorked",
                "OfficerGender","OfficerGender.freq",
                "OfficerRace","OfficerRace.freq",
                "Rank","Rank.freq",
                "OfficerAge","officers_witness",
                "WitnessGender","WitnessGender.freq",
                "WitnessRace","WitnessRace.freq",
                "comp_witness","IncidentDaysAfterAppt","allegation_count")
  matrix <- rbind.data.frame(matrix,a)
}
#matrix <- merge(matrix,tab,by="Allegation")
rm(i,j,allegation,t,a,tab)

#matrix <- matrix[-which(matrix$Cases<50),]

#matrix <- matrix[-which(matrix$Allegation=="Miscellaneous"),] #This allegation is weird and might add extra heterogeneity
#matrix <- matrix[-which(matrix$Outcome=="Suspended for 365 Days"),]


glimpse(matrix)

matrix <- matrix %>%
  mutate(#Cases = as.numeric(Cases),
         Category = factor(Category),
         Category.freq = as.numeric(Category.freq),
         Allegation = factor(Allegation),
         Finding = factor(Finding),
         Finding.freq = as.numeric(Finding.freq),
         Outcome = factor(Outcome),
         Outcome.freq = as.numeric(Outcome.freq),
         District = factor(District),
         District.freq = as.numeric(District.freq),
         officers_involved = as.numeric(officers_involved),
         DistrictsWorked = as.numeric(DistrictsWorked),
         OfficerGender = factor(OfficerGender),
         OfficerGender.freq = as.numeric(OfficerGender.freq),
         OfficerRace = factor(OfficerRace),
         OfficerRace.freq = as.numeric(OfficerRace.freq),
         Rank = factor(Rank),
         Rank.freq = as.numeric(Rank.freq),
         OfficerAge = as.numeric(OfficerAge),
         officers_witness = as.numeric(officers_witness),
         WitnessGender = factor(WitnessGender),
         WitnessGender.freq = as.numeric(WitnessGender.freq),
         WitnessRace = factor(WitnessRace),
         WitnessRace.freq = as.numeric(WitnessRace.freq),
         comp_witness = as.numeric(comp_witness),
         IncidentDaysAfterAppt = as.numeric(IncidentDaysAfterAppt),
         allegation_count = as.numeric(allegation_count))

rownames(matrix) <- matrix[,1]
matrix <- matrix[,-1]



weights <- rep(0,ncol(matrix))
weights[which(names(matrix)=="allegation_count"|names(matrix)=="Allegation")] <- .1

weights[which(weights==0)] <- (1-sum(weights))/length(weights[which(weights==0)])

gower.dist <- daisy(matrix,
                    metric = "gower", #weights = weights,
                    type = list(logratio = 3))

#gower.dist <- as.matrix(gower.dist)
#colnames(gower.dist) <- matrix[,1]
#rownames(gower.dist) <- matrix[,1]
#------------ DIVISIVE CLUSTERING ------------#
divisive.clust <- diana(as.matrix(gower.dist), 
                        diss = TRUE, keep.diss = TRUE)
plot(divisive.clust, main = "Divisive",hang = -1)

#------------ AGGLOMERATIVE CLUSTERING ------------#
# I am looking for the most balanced approach
# Complete linkages is the approach that best fits this demand - I will leave only this one here, don't want to get it cluttered
# complete
aggl.clust.c <- hclust(gower.dist, method = "complete")
plot(aggl.clust.c,
     main = "Agglomerative, complete linkages",hang = -1)


# Cluster stats comes out as list while it is more convenient to look at it as a table
# This code below will produce a dataframe with observations in columns and variables in row
# Not quite tidy data, which will require a tweak for plotting, but I prefer this view as an output here as I find it more comprehensive 
library(fpc)
cstats.table <- function(dist, tree, k) {
  clust.assess <- c("cluster.number","n","within.cluster.ss","average.within","average.between",
                    "wb.ratio","dunn2","avg.silwidth")
  clust.size <- c("cluster.size")
  stats.names <- c()
  row.clust <- c()
  output.stats <- matrix(ncol = k, nrow = length(clust.assess))
  cluster.sizes <- matrix(ncol = k, nrow = k)
  for(i in c(1:k)){
    row.clust[i] <- paste("Cluster-", i, " size")
  }
  for(i in c(2:k)){
    stats.names[i] <- paste("Test", i-1)
    
    for(j in seq_along(clust.assess)){
      output.stats[j, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])[j]
      
    }
    
    for(d in 1:k) {
      cluster.sizes[d, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.size])[d]
      dim(cluster.sizes[d, i]) <- c(length(cluster.sizes[i]), 1)
      cluster.sizes[d, i]
      
    }
  }
  output.stats.df <- data.frame(output.stats)
  cluster.sizes <- data.frame(cluster.sizes)
  cluster.sizes[is.na(cluster.sizes)] <- 0
  rows.all <- c(clust.assess, row.clust)
  # rownames(output.stats.df) <- clust.assess
  output <- rbind(output.stats.df, cluster.sizes)[ ,-1]
  colnames(output) <- stats.names[2:k]
  rownames(output) <- rows.all
  is.num <- sapply(output, is.numeric)
  output[is.num] <- lapply(output[is.num], round, 2)
  output
}
# I am capping the maximum ammount of clusters by 7
# I want to choose a reasonable number, based on which I will be able to see basic differences between customer groups as a result
stats.df.divisive <- cstats.table(gower.dist, divisive.clust, 10) #This one is more balanced in sample size
stats.df.divisive

stats.df.aggl <-cstats.table(gower.dist, aggl.clust.c, 12) #complete linkages looks like the most balanced approach
stats.df.aggl


# --------- Choosing the number of clusters ---------#
# Using "Elbow" and "Silhouette" methods to identify the best number of clusters
# to better picture the trend, I will go for more than 7 clusters.
library(ggplot2)
# Elbow
# Divisive clustering
ggplot(data = data.frame(t(cstats.table(gower.dist, divisive.clust, 15))), 
       aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  ggtitle("Divisive clustering") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))
#About 9 clusters


# Agglomerative clustering,provides a more ambiguous picture
ggplot(data = data.frame(t(cstats.table(gower.dist, aggl.clust.c, 15))), 
       aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  ggtitle("Agglomerative clustering") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))
#about 12 clusters



# Silhouette
ggplot(data = data.frame(t(cstats.table(gower.dist, divisive.clust, 15))), 
       aes(x=cluster.number, y=avg.silwidth)) + 
  geom_point()+
  geom_line()+
  ggtitle("Divisive clustering") +
  labs(x = "Num.of clusters", y = "Average silhouette width") +
  theme(plot.title = element_text(hjust = 0.5))
# 4


ggplot(data = data.frame(t(cstats.table(gower.dist, aggl.clust.c, 15))), 
       aes(x=cluster.number, y=avg.silwidth)) + 
  geom_point()+
  geom_line()+
  ggtitle("Agglomerative clustering") +
  labs(x = "Num.of clusters", y = "Average silhouette width") +
  theme(plot.title = element_text(hjust = 0.5))
#4


fviz_dend(divisive.clust, k=8,color_labels_by_k = FALSE,rect = TRUE,horiz = TRUE, main = "",ylab = "",cex = 0.6)+
  scale_color_brewer(palette = "Paired")

fviz_dend(aggl.clust.c, k=8,color_labels_by_k = FALSE,rect = TRUE,horiz = TRUE, main = "",ylab = "",cex = 0.6)+
  scale_color_brewer(palette = "Paired")

groups <- cutree(aggl.clust.c,50)
groups <- as.data.frame(cbind(row.names(matrix),groups))
names(groups) <- c("Allegation","AllegationType")




# Random Forest -----------------------------------------------------------

data.rf <- merge(data.cluster,groups,by="Allegation")
glimpse(data.rf)

data.rf <- data.rf[,-1] %>%
  mutate(AllegationType = factor(AllegationType))


data.frame(table(data.rf$Finding))
#No Cooperation has excessively low freq. I'm gonna nix it.

data.rf <- data.rf %>%
  filter(Finding!="No Cooperation")
data.rf$Finding <- factor(as.character(data.rf$Finding))


# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
set.seed(100)
train <- sample(nrow(data.rf), 0.7*nrow(data.rf), replace = FALSE)
TrainSet <- data.rf[train,]
ValidSet <- data.rf[-train,]
summary(TrainSet)
summary(ValidSet)

#Balance Train set
cat <- as.character(data.frame(table(TrainSet$Finding))$Var1)
num <- min(data.frame(table(TrainSet$Finding))$Freq)

tmp <- NULL
for(i in 1:length(cat)){
  a <- TrainSet[which(TrainSet$Finding==cat[i]),]
  a <- a[sample(nrow(a),num, replace = FALSE),]
  tmp <- rbind(tmp,a)
}
TrainSet <- tmp
rm(tmp,a,cat,num,i)


# Create a Random Forest model with default parameters
model1 <- randomForest(Finding ~ ., data = TrainSet, importance = TRUE)
model1

model2 <- randomForest(Finding ~ ., data = TrainSet, ntree = 500, mtry = 6, importance = TRUE)
model2

model3 <- randomForest(Finding ~ ., data = TrainSet, ntree = 500, mtry = 12, importance = TRUE)
model3

model4 <- randomForest(Finding ~ ., data = TrainSet, ntree = 500, mtry = 16, importance = TRUE)
model4

model5 <- randomForest(Finding ~ ., data = TrainSet, ntree =1000, mtry = 16, importance = TRUE)
model5

model6 <- randomForest(Finding ~ ., data = TrainSet, ntree =2000, importance = TRUE)
model6




