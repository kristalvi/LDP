
#################################################################################################################
#                     LDP Part 2 Code (After Consolidating All Datasets)
#                     Olivia, Fatima, Mayura, Kristal
#                     Machine Learning codes are based on Big Data Class Exercises (Alessandro)
#################################################################################################################

######################################################
######          Step 1: Set-up working directory and library
setwd("###")
.libPaths("C:\\Rlibs")


#######################################################################################
######          Step 2: Load necessary packages
######

library(dplyr)
library(maps)
library(pmap) 
library(purrr)
library("darksky")
library(factoextra)
library(NbClust)
library(FunCluster)
library(rgdal)
library(maps)
library(maptools)
library(OpenStreetMap)
library(raster)
library(gdata)

library(neuralnet)
library(tree)
library(randomForest)
library(caret)
library(ROCR)

library(marray)
library(CluMix)
library(vegan)
library(cluster)
library(dplyr)
library(maps)
library(pmap) 
library(purrr)
library(factoextra)
library(NbClust)
library(FunCluster)
library(rgdal)
library(maps)
library(maptools)
library(OpenStreetMap)
library(raster)
library(gdata)

library(jsonlite)
library(rjson)
library(sparklyr)
library(readr)
library(stringr)
library(plyr)
detach("package:plyr", unload=TRUE)
library(dplyr)

library(SentimentAnalysis)
library(textcat)

library(ISLR)
library(wesanderson)
library(RColorBrewer)
library(ggplot2)
library(fitdistrplus)
library(lattice)
library(MASS)

library(psych)
library(car)
library(devtools)
library(klaR)
library(fpc)


#join the two tables
#TCWAJ3 <- inner_join(TCWA, Agg_Journey, by="joinvalue"); head(TCWAJ3)
#dim(TCWAJ3)

#######################################################################################
######          Step 3:Choose important variables for the analysis (Drop Duplicates from the Merged Sets)
######                and rename column names
######

TCWAJ3 <- read.csv("TCWAJ3.csv"); TCWAJ3 <- TCWAJ3[,-1]

str(TCWAJ3); colnames(TCWAJ3)
TCWAJ3 <- TCWAJ3[,c(1,4,7,8,9,10,12,13,14,15,21,
                      22,24,23,47:57,58:60,61:63,33,43,31,32)]
head(TCWAJ3)

#rename columns
colnames(TCWAJ3)
colnames(TCWAJ3)[2] = "overall_weather"
colnames(TCWAJ3)[3] = "temp"
colnames(TCWAJ3)[4] = "apparentTemp"
colnames(TCWAJ3)[5] = "dewPoint"
colnames(TCWAJ3)[6] = "humidity"
colnames(TCWAJ3)[7] = "windSpeed"
colnames(TCWAJ3)[8] = "windBearing"
colnames(TCWAJ3)[9] = "visibility"
colnames(TCWAJ3)[10] = "cloudCover"
colnames(TCWAJ3)[11] = "date"
colnames(TCWAJ3)[12] = "hour"
colnames(TCWAJ3)[26] = "acc_vehicles"
colnames(TCWAJ3)[27] = "acc_casualties"
colnames(TCWAJ3)[28] = "acc_accidents"
colnames(TCWAJ3)[29] = "jt_speedLimit"
colnames(TCWAJ3)[30] = "jt_speed"
colnames(TCWAJ3)[31] = "jt_journeyTime"
colnames(TCWAJ3)[32] = "road"
colnames(TCWAJ3)[34] = "latitude"
colnames(TCWAJ3)[35] = "longitude"
head(TCWAJ3)

#######################################################################################
#######         Step 4: Handle missing data
#######

#check for missing data
TCWAJ3[is.na(TCWAJ3$acc_vehicles),"acc_vehicles"] = 0
TCWAJ3[is.na(TCWAJ3$acc_casualties),"acc_casualties"] = 0
TCWAJ3[is.na(TCWAJ3$acc_accidents),"acc_accidents"] = 0


NA_TCWAJ3 <- apply(is.na(TCWAJ3), 2, sum)
NA_TCWAJ3 <- round((NA_TCWAJ3 / dim(TCWAJ3)[1]) * 100, digits=2)
NA_TCWAJ3

#speed limit has 85.86% of data. should drop it.
colnames(TCWAJ3)
TCWAJ3 <- TCWAJ3[,-29]

#Accident columns that are NA should be set to 0
TCWAJ3[is.na(TCWAJ3$acc_vehicles),"acc_vehicles"] = 0
TCWAJ3[is.na(TCWAJ3$acc_casualties),"acc_casualties"] = 0
TCWAJ3[is.na(TCWAJ3$acc_accidents),"acc_accidents"] = 0

#Complete cases of JourneyTime
TCWAJ3[is.na(TCWAJ3$jt_speed)==FALSE,]
dim(TCWAJ3) #15060 and 35
str(TCWAJ3)

#Re-declare variable types
TCWAJ3[,"overall_weather"] <- as.factor(TCWAJ3[,"overall_weather"])
TCWAJ3[,"date"] <- as.Date(TCWAJ3[,"date"])
TCWAJ3[,"month"] <- as.integer(TCWAJ3[,"month"])
                  #as.ordered(as.integer(TCWAJ3[,"month"]))
#add derived variables
TCWAJ3$long_lat <- paste(round(TCWAJ3$longitude, digits=1), 
                         round(TCWAJ3$latitude, digits=1))
TCWAJ3$year <- substr(TCWAJ3$date, start=1, stop=4)

TCWAJ3$date <- as.numeric(substr(TCWAJ3$date, start=6, stop=7))
                          
traffic_med_longlat <- as.data.frame(TCWAJ3 %>% 
                                       group_by(year, 
                                                long_lat, add=TRUE) %>%
                                       summarize(yr_speed = median(jt_speed, na.rm=TRUE),
                                                 yr_journeyTime = median(jt_journeyTime, na.rm=TRUE),
                                                 yr_cloudCover = median(cloudCover, na.rm=TRUE)
                                                  ))

traffic_med_longlat$joinvalue2 <- paste(traffic_med_longlat$long_lat,
                                       traffic_med_longlat$year)

TCWAJ3$joinvalue2 <- paste(TCWAJ3$long_lat, TCWAJ3$year)
TCWAJ3 <- left_join(TCWAJ3, traffic_med_longlat, by="joinvalue2")
TCWAJ3$traffic_level <- TCWAJ3$jt_speed

TCWAJ3 <- within(TCWAJ3, cloudCover[is.na(cloudCover)]
                 <- TCWAJ3[is.na(cloudCover),"yr_cloudCover"])

TCWAJ3 <- within(TCWAJ3, traffic_level[jt_speed <= (yr_speed*.60)]
                <- "clear")

TCWAJ3 <- within(TCWAJ3, traffic_level[jt_speed <= (yr_speed*0.90)
                                       & jt_speed > (yr_speed*0.60)]
                 <- "clear to light congestion")

TCWAJ3 <- within(TCWAJ3, traffic_level[jt_speed <= (yr_speed*1.05)
                                       & jt_speed > (yr_speed*0.90)]
                 <- "light to moderate congestion")

TCWAJ3 <- within(TCWAJ3, traffic_level[jt_speed > (yr_speed*1.05)]
                 <- "moderate to heavy congestion")
TCWAJ3$traffic_level <- as.factor(as.character(TCWAJ3$traffic_level))
TCWAJ3 <- TCWAJ3[,c(-37:-42)]; str(TCWAJ3)
colnames(TCWAJ3); 
colnames(TCWAJ3)[35] <- "long_lat"
colnames(TCWAJ3)[36] <- "year"

###for the traffic level get the average sped for the road

#######################################################################################
#######         Step 5: Summary statistics
#######

summary(TCWAJ3); colnames(TCWAJ3)

#Generate visuals
numeric_join <- c(3:13,15:30,33,34) #11 is date, 12 is hour, 13 is month
#pdf(file="Descriptive Analysis for LDP.pdf")
png(file="Descriptive Anaylsis for LDP.png")
psych::pairs.panels(TCWAJ3[,numeric_join])
head(TCWAJ3[,numeric_join])
dev.off()

#Generate boxplots
#histogram and density plot
opar=par()
pdf(file = "Boxplot for LDP Traffic Data.pdf")
par(mfrow=c(3,3))

boxplot(TCWAJ3$temp, col="lavenderblush2",main="Boxplot for \ntemp")
boxplot(TCWAJ3$apparentTemp, col="lavenderblush2",main="Boxplot for apparentTemp")
boxplot(TCWAJ3$dewPoint, col="lavenderblush2",main="Boxplot for \ndewPoint")
boxplot(TCWAJ3$humidity, col="lavenderblush2",main="Boxplot for \nhumidity")
boxplot(TCWAJ3$windSpeed, col="lavenderblush2",main="Boxplot for \nwindSpeed")
boxplot(TCWAJ3$windBearing, col="lavenderblush2",main="Boxplot for windBearing")
boxplot(TCWAJ3$visibility, col="lavenderblush2",main="Boxplot for visibility")
boxplot(TCWAJ3$cloudCover, col="lavenderblush2",main="Boxplot for \ncloudCover")
boxplot(TCWAJ3$CAR, col="lavenderblush2",main="Boxplot for CAR")
boxplot(TCWAJ3$BUS, col="lavenderblush2",main="Boxplot for BUS")
boxplot(TCWAJ3$LGV, col="lavenderblush2",main="Boxplot for LGV")
boxplot(TCWAJ3$HGVR2, col="lavenderblush2",main="Boxplot for HGVR2")
boxplot(TCWAJ3$HGVR3, col="lavenderblush2",main="Boxplot for HGVR3")
boxplot(TCWAJ3$HGVR4, col="lavenderblush2",main="Boxplot for HGVR4")
boxplot(TCWAJ3$HGVA3, col="lavenderblush2",main="Boxplot for HGVA3")
boxplot(TCWAJ3$HGVA5, col="lavenderblush2",main="Boxplot for HGVA5")
boxplot(TCWAJ3$HGVA6, col="lavenderblush2",main="Boxplot for HGVA6")
boxplot(TCWAJ3$AMV , col="lavenderblush2",main="Boxplot for AMV ")
boxplot(TCWAJ3$acc_vehicles, col="lavenderblush2",main="Boxplot for \nacc_vehicles")
boxplot(TCWAJ3$acc_casualties, col="lavenderblush2",main="Boxplot for \nacc_casualties")
boxplot(TCWAJ3$acc_accidents, col="lavenderblush2",main="Boxplot for \nacc_accidents")
boxplot(TCWAJ3$jt_speed , col="lavenderblush2",main="Boxplot for \njt_speed ")
boxplot(TCWAJ3$jt_journeyTime, col="lavenderblush2",main="Boxplot for \njt_journeyTime")

dev.off()

#Generate qq-lines
pdf(file="QQ Plot for LDP Numerical TCWAJ3.pdf")

par(mfrow=c(3,3))
qqnorm(TCWAJ3$temp , col="darkmagenta", main="Normal Q-Q Plot\nfor temp ")
qqline(TCWAJ3$temp)
qqnorm(TCWAJ3$apparentTemp, col="darkmagenta", main="Normal Q-Q Plot\nfor apparentTemp"); 
qqline(TCWAJ3$apparentTemp)
qqnorm(TCWAJ3$dewPoint, col="darkmagenta", main="Normal Q-Q Plot\nfor dewPoint")
qqline(TCWAJ3$dewPoint)
qqnorm(TCWAJ3$humidity, col="darkmagenta", main="Normal Q-Q Plot\nfor humidity"); 
qqline(TCWAJ3$humidity)
qqnorm(TCWAJ3$windSpeed, col="darkmagenta", main="Normal Q-Q Plot\nfor windSpeed");
qqline(TCWAJ3$windSpeed)
qqnorm(TCWAJ3$windBearing, col="darkmagenta", main="Normal Q-Q Plot\nfor windBearing");
qqline(TCWAJ3$windBearing)
qqnorm(TCWAJ3$visibility, col="darkmagenta", main="Normal Q-Q Plot\nfor visibility");
qqline(TCWAJ3$visibility)
qqnorm(TCWAJ3$cloudCover, col="darkmagenta", main="Normal Q-Q Plot\nfor 
       cloudCover");
qqline(TCWAJ3$cloudCover)
qqnorm(TCWAJ3$CAR, col="darkmagenta", main="Normal Q-Q Plot\nfor CAR");
qqline(TCWAJ3$CAR)
qqnorm(TCWAJ3$BUS, col="darkmagenta", main="Normal Q-Q Plot\nfor BUS");
qqline(TCWAJ3$BUS)
qqnorm(TCWAJ3$LGV , col="darkmagenta", main="Normal Q-Q Plot\nfor LGV ");
qqline(TCWAJ3$LGV )
qqnorm(TCWAJ3$HGVR2, col="darkmagenta", main="Normal Q-Q Plot\nfor HGVR2");
qqline(TCWAJ3$HGVR2)
qqnorm(TCWAJ3$HGVR3, col="darkmagenta", main="Normal Q-Q Plot\nfor HGVR3"); 
qqline(TCWAJ3$HGVR3)
qqnorm(TCWAJ3$HGVR4, col="darkmagenta", main="Normal Q-Q Plot\nfor HGVR4"); 
qqline(TCWAJ3$HGVR4)
qqnorm(TCWAJ3$HGVA3, col="darkmagenta", main="Normal Q-Q Plot\nfor HGVA3"); 
qqline(TCWAJ3$HGVA3)
qqnorm(TCWAJ3$HGVA5, col="darkmagenta", main="Normal Q-Q Plot\nfor HGVA5"); 
qqline(TCWAJ3$HGVA5)
qqnorm(TCWAJ3$HGVA6, col="darkmagenta", main="Normal Q-Q Plot\nfor HGVA6"); 
qqline(TCWAJ3$HGVA6)
qqnorm(TCWAJ3$HGV, col="darkmagenta", main="Normal Q-Q Plot\nfor HGV"); 
qqline(TCWAJ3$HGV)
qqnorm(TCWAJ3$AMV, col="darkmagenta", main="Normal Q-Q Plot\nfor AMV"); 
qqline(TCWAJ3$AMV)
qqnorm(TCWAJ3$acc_vehicles, col="darkmagenta", main="Normal Q-Q Plot\nfor acc_vehicles"); 
qqline(TCWAJ3$acc_vehicles)
qqnorm(TCWAJ3$acc_casualties, col="darkmagenta", main="Normal Q-Q Plot\nfor acc_casualties"); 
qqline(TCWAJ3$acc_casualties)
qqnorm(TCWAJ3$acc_accidents, col="darkmagenta", main="Normal Q-Q Plot\nfor acc_accidents"); 
qqline(TCWAJ3$acc_accidents)
qqnorm(TCWAJ3$jt_speed, col="darkmagenta", main="Normal Q-Q Plot\nfor jt_speed"); 
qqline(TCWAJ3$jt_speed)
qqnorm(TCWAJ3$jt_journeyTime, col="darkmagenta", main="Normal Q-Q Plot\nfor jt_journeyTime"); 
qqline(TCWAJ3$jt_journeyTime)

dev.off()

#######################################################################################
#######         Step 6: Transform data based on distribution (Use Log-Normal Distribution)
#######

#Step 3: Transform the Data through Log Normal Distribution based on QQ Plot Interpretation
TCWAJ4 <- TCWAJ3
TCWAJ3$CAR <- TCWAJ3$CAR + 0.1 
TCWAJ3$CAR <- bcPower(TCWAJ3$CAR, powerTransform(TCWAJ3$CAR)$roundlam)

TCWAJ3$BUS <- TCWAJ3$BUS + 0.1 
TCWAJ3$BUS <- bcPower(TCWAJ3$BUS, powerTransform(TCWAJ3$BUS)$roundlam)

TCWAJ3$LGV <- TCWAJ3$LGV + 0.1 
TCWAJ3$LGV <- bcPower(TCWAJ3$LGV, powerTransform(TCWAJ3$LGV)$roundlam)

TCWAJ3$HGVR2 <- TCWAJ3$HGVR2 + 0.1 
TCWAJ3$HGVR2 <- bcPower(TCWAJ3$HGVR2, powerTransform(TCWAJ3$HGVR2)$roundlam)

TCWAJ3$HGVR3 <- TCWAJ3$HGVR3 + 0.1 
TCWAJ3$HGVR3 <- bcPower(TCWAJ3$HGVR3, powerTransform(TCWAJ3$HGVR3)$roundlam)

TCWAJ3$HGVR4 <- TCWAJ3$HGVR4 + 0.1 
TCWAJ3$HGVR4 <- bcPower(TCWAJ3$HGVR4, powerTransform(TCWAJ3$HGVR4)$roundlam)

TCWAJ3$HGVA3 <- TCWAJ3$HGVA3 + 0.1 
TCWAJ3$HGVA3 <- bcPower(TCWAJ3$HGVA3, powerTransform(TCWAJ3$HGVA3)$roundlam)

TCWAJ3$HGVA5 <- TCWAJ3$HGVA5 + 0.1 
TCWAJ3$HGVA5 <- bcPower(TCWAJ3$HGVA5, powerTransform(TCWAJ3$HGVA5)$roundlam)

TCWAJ3$HGVA6 <- TCWAJ3$HGVA6 + 0.1 
TCWAJ3$HGVA6 <- bcPower(TCWAJ3$HGVA6, powerTransform(TCWAJ3$HGVA6)$roundlam)

TCWAJ3$HGV <- TCWAJ3$HGV + 0.1 
TCWAJ3$HGV <- bcPower(TCWAJ3$HGV, powerTransform(TCWAJ3$HGV)$roundlam)

TCWAJ3$AMV <- TCWAJ3$AMV + 0.1 
TCWAJ3$AMV <- bcPower(TCWAJ3$AMV, powerTransform(TCWAJ3$AMV)$roundlam)

TCWAJ3$acc_vehicles <- TCWAJ3$acc_vehicles + 0.1 
TCWAJ3$acc_vehicles <- bcPower(TCWAJ3$acc_vehicles, 
                               powerTransform(TCWAJ3$acc_vehicles)$roundlam)

TCWAJ3$acc_casualties <- TCWAJ3$acc_casualties + 0.1 
TCWAJ3$acc_casualties <- bcPower(TCWAJ3$acc_casualties, 
                                 powerTransform(TCWAJ3$acc_casualties)$roundlam)

TCWAJ3$acc_accidents <- TCWAJ3$acc_accidents + 0.1 
TCWAJ3$acc_accidents <- bcPower(TCWAJ3$acc_accidents, 
                                powerTransform(TCWAJ3$acc_accidents)$roundlam)
TCWAJ3_transformed <- TCWAJ3
TCWAJ3 <- TCWAJ4
#######################################################################################
#######         Step 7: Scatterplot and correlation to identify correlated Xs
#######

png(file="Scatterplot for LDP Numerical Data.png")
pairs(~ temp + apparentTemp + dewPoint + humidity + windSpeed + 
        windBearing + visibility + cloudCover + CAR + BUS + LGV
      + HGVR2 + HGVR3 + HGVR4+ HGVA3 + HGVA5 + HGV + AMV + acc_vehicles
      + acc_casualties + acc_accidents + jt_speed + jt_journeyTime,
      data=TCWAJ3, main="Scatterplot Matrix for Numerical Factors",
      col=rgb(0,100,0,50,maxColorValue=255),panel=panel.smooth)
dev.off()

#run this on the log-normal transformed data
round(cor(TCWAJ3[,c("temp", "apparentTemp", "dewPoint",
        "humidity", "windSpeed", "windBearing", "visibility",
        "cloudCover", "CAR", "BUS", "LGV", "HGVR2", "HGVR3",
        "HGVR4", "HGVA3", "HGVA5", "HGVA6", "HGV", "AMV", "acc_vehicles",
        "acc_casualties", "acc_accidents", "jt_speed", "jt_journeyTime")]),2)

###Conclusion: Eliminate temp and choose apparentTemp
#              Eliminate CAR, BUS, LGV, HGVR2, HGVR3, HGVR4, HGVA6, HGV and retain HGVA3, HGVA5, and AMV
#              Eliminate acc_casualities and acc_vehicles and choose acc_accidents

#######################################################################################
#######         Step 8: Check for outliers, but not necessarily remove them
####### 

#create a function to count outliers
countOutliers <- function(x){
  index.out1 <- x < quantile(x,0.25, na.rm=TRUE)-(1.5*IQR(x, na.rm=TRUE));    x[index.out1]
  index.out2 <- x > quantile(x,0.75, na.rm=TRUE)+(1.5*IQR(x, na.rm=TRUE));    x[index.out2]
  length(x[index.out1]) + length(x[index.out2])
}

#create a function to remove outliers
NoOutliers <- function(x){
  y <- join_transformed[x >= quantile(x,0.25)-(1.5*IQR(x)) & x <= quantile(x,0.75)+(1.5*IQR(x)),]
  return(y)
}

colnames(TCWAJ3); numeric_join <- c(3:11,15:30)
Outliers <- apply(TCWAJ3[,numeric_join],2,countOutliers)
Outliers


#######################################################################################
#######         Step 9: Finalised Variables by removing those that are correlated
#######

colnames(TCWAJ3)
TCWAJ3 <- TCWAJ3[,c(-3,-15:-20,-23:-24,-26,-27)]

#######################################################################################
#######         Step 10. Unsupervised Learning - Clustering
#######         Not needed to split the data, but need to scale them. Not needed to use logtransformed data
#######         !!!!!!!! Perform steps 1-5, skip 6-8, then perform 9 prior running this step
#######

#https://www.r-bloggers.com/clustering-mixed-data-types-in-r/
colnames(TCWAJ3); numeric_join <- c(3:12,14:19)
TCWAJ3[,numeric_join] <- scale(TCWAJ3[,numeric_join]) 
TCWAJ3$cloudCover <- as.numeric(TCWAJ3$cloudCover)
TCWAJ3$date <- as.numeric(TCWAJ3$date)
TCWAJ3$hour <- as.numeric(TCWAJ3$hour)
TCWAJ3$month <- as.numeric(TCWAJ3$month)
TCWAJ3$long_lat <- as.factor(TCWAJ3$long_lat)
TCWAJ3$year <- as.numeric(TCWAJ3$year)
TCWAJ3$iDir <- as.factor(TCWAJ3$iDir)
gower_comp <- daisy(TCWAJ3[,c(-1,-20,-22,-23)], metric="gower") #summary(gower_comp)  #join_value, road, lat, long

#determine k
#Calculate silhouette width for many k using PAM
sil_width <- c(NA)
for(i in 2:10){
  pam_fit <- pam(gower_comp,
                 diss = TRUE,
                 k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}

# Plot sihouette width (higher is better)
pdf(file="Optimal K based on Silhouette Width LDP.pdf")
plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)
dev.off()
#run cluster at k2

pdf(file="Clustering for LDP.pdf")
join_cluster <- pam(gower_comp,
                    diss = TRUE,
                    k = 2)
clusplot(join_cluster, main = "PAM: Cluster of Roads, k = 2",
         color=TRUE)
dev.off()

#######################################################################################
#######         Step 11: Split to Training and Test Data to prepare
#######

TCWAJ3$road <- as.factor(TCWAJ3$road)
TCWAJ3$iDir <- as.factor(TCWAJ3$iDir)
TCWAJ3$long_lat <- as.factor(TCWAJ3$long_lat)
TCWAJ3$borough <- as.factor(as.character(TCWAJ3$borough))

TCWAJ5 <- TCWAJ3; TCWAJ3 <- TCWAJ3[,-18]

index <- sample(nrow(TCWAJ3), nrow(TCWAJ3) * 0.7)
train_pred <- TCWAJ3[index,]
test_pred <- TCWAJ3[-index,]

#######################################################################################
#######         Step 12: Machine Learning - DT and RF for Classification of Traffic Levels
#######         Use log-normal transformed data set for this one
#######         !!!!!!!!! Perform Steps 1-8 prior running this step (may skip boxplots/qqlines)
#######

str(train_pred); colnames(train_pred); table(train_pred$traffic_level)
#define a formula 
formula = traffic_level ~ overall_weather + apparentTemp +
  dewPoint + humidity + windSpeed + windBearing + visibility +
  cloudCover + hour + date + month  +
  HGVA3 + HGVA5 + AMV + acc_accidents +
  jt_journeyTime + borough +
  iDir + long_lat
#consider as predictors: weekday or weekend, public holidays

dt <- tree(formula, data = train_pred)

cv_dt <- cv.tree(dt, FUN=prune.misclass)

cv_table <- data.frame(
  size = cv_dt$size,
  error = cv_dt$dev
)

pruned_tree_size <- cv_table[which.min(cv_table$error), 'size']

pruned_tree <- prune.misclass(dt, best = pruned_tree_size)

rf <- randomForest(formula, ntree=500,
                   na.action=na.omit, importance = T, data=train_pred)

str(test_pred); colnames(test_pred)
pred_pruned <- predict(pruned_tree, test_pred[-25], type = "class")

### 3.2 compute the prediction for the test set with the random forest model
###   note: the income attribute should be excluded from the test data set
pred_rf <- predict(rf, test_pred[,-25], type= "class")

results_table2 <- data.frame(
  actual = test_pred$traffic_level,
  results_tree = pred_pruned,
  results_rf = pred_rf
)
colnames(results_table2)


### 3.4 create a contigency table of the actual VS predicted for each model
cont_table_tree <- table(results_table2[,c('actual','results_tree')])
cont_table_rf <- table(results_table2[,c('actual','results_rf')])

### 3.4 calculate accuracy values from the contigency tables
acc_tree <- sum(diag(cont_table_tree))/sum(cont_table_tree)
acc_rf <- sum(diag(cont_table_rf))/sum(cont_table_rf)

acc_tree
acc_rf

#################################################################3
#Step 9: Machine Learning - KNN

# transform the data using a min-max function
#   note: this will make the data more suitable for use with NN
#     as the attribute values will be on a narrow interval around zero
# first define a MinMax function
MinMax <- function(x){
  tx <- (x - min(x)) / (max(x) - min(x))
  return(tx)
}

# then apply the function to each column of the data set
#   note: the apply function returns a matrix
#colnames(TCWAJ3)
#numeric_join <- c(3:13,15:30,33,34) #11 is date, 12 is hour, 13 is month
TCWAJ3 <- TCWAJ5; colnames(TCWAJ3); numeric_join <- c(3:12,14:19,22,23)
TCWAJ3 <- TCWAJ3[,-16] #remove journeyTime
join_pred_MinMax <- apply(TCWAJ3[,numeric_join], 2, MinMax)
head(join_pred_MinMax); colnames(join_pred_MinMax)

# the matrix needs to be 'cast' into a data frame
#   note: R has an as.data.frame function for this purpose

join_pred_MinMax <- as.data.frame(join_pred_MinMax)
join_pred_MinMax <- join_pred_MinMax[,c("apparentTemp","dewPoint","humidity","windSpeed","windBearing","visibility","cloudCover","hour",
                                     "month","HGVA3","HGVA5","AMV","acc_accidents","date","jt_speed",
                                     "longitude","latitude")]
index <- sample(nrow(join_pred_MinMax), nrow(join_pred_MinMax) * 0.7)

train_pred <- join_pred_MinMax[index,]
test_pred <- join_pred_MinMax[-index,]
#######################################################################################
#######         Step 13. Machine Learning - KNN for Speed
#######         Not needed to use log-transformed data  
#######         !!!!!!!! Perform steps 1-5, skip 6-8, then perform 9 prior running this step
#######

colnames(train_pred); str(train_pred); summary(train_pred)

# define a formula for predicting strength
formula = jt_speed ~ apparentTemp +
  dewPoint + humidity + windSpeed + windBearing + visibility +
  cloudCover + hour + month + HGVA3 + HGVA5 + 
  AMV +  acc_accidents + date + longitude + latitude
#long and lat need to be inputed. but not numerical so????

# train a neural network with 1 hidden node
join_nn_1 <- neuralnet(formula, data = train_pred, stepmax=1e6)

# train a neural network with 5 nodes on one hidden layer
#   note: the number of layers is set with the hidden option parameter
join_nn_5 <- neuralnet(formula, hidden = 5, data = train_pred, stepmax=1e6)

# train a neural network with 5 nodes on each of two hidden layers
join_nn_55 <- neuralnet(formula, hidden = c(5,5), data = train_pred, stepmax=1e6)

# plot the three neural networks and compare their structure
par(mfrow=c(1,2))
pdf(file="KNN Plots for LDP.pdf")
plot(join_nn_1, main='KNN 1 hidden node')
plot(join_nn_5, main='KNN 5 nodes 1 hidden layer')
dev.off()


#13.A Neural network prediction
apparentTemp +
  dewPoint + humidity + windSpeed + windBearing + visibility +
  cloudCover + hour + month + HGVA3 + HGVA5 + 
  AMV +  acc_accidents + date + longitude + latitude
# compute the prediction for each neural network
#   note: remove jt_journey and jt_speed
pred_nn_1 <- neuralnet::compute(join_nn_1, test_pred[,c("apparentTemp","dewPoint","humidity",
                      "windSpeed","windBearing","visibility","cloudCover","hour",
                      "month","HGVA3","HGVA5","AMV","acc_accidents","date",
                      "longitude","latitude")])
pred_nn_5 <- neuralnet::compute(join_nn_5, test_pred[,c(-15)])
pred_nn_55 <- neuralnet::compute(join_nn_55, test_pred[,c(-15)])

# create a table with actual values and the three predictions
#   note: predicted values are stored as net_result attribute of the prediction object
nn_results <- data.frame(
  actual = test_pred$jt_speed,
  nn_1 = pred_nn_1$net.result,
  nn_5 = pred_nn_5$net.result,
  nn_55 = pred_nn_55$net.result
)

# calculate the correlation between actual and predicted values to identify the best predictor
cor(nn_results[,'actual'], nn_results[,c("nn_1")])
cor(nn_results[,'actual'], nn_results[,c("nn_5")]) #0.837 is the best predictor


# plot actual vs predicted values for the worst (blue) and best predictor (orange)
#   note: points is used to add points on a graph
opar <- par()
pdf(file = "KNN for LDP 0.32 and 0.84 correlation.pdf")
plot(
  nn_results$actual,
  nn_results$nn_1,
  col = 'blue',
  xlab = 'actual strength',
  ylab = 'predicted strength',
  xlim = c(0,1),
  ylim = c(0,1)
)
points(
  nn_results$actual,
  nn_results$nn_5,
  col = 'orange'
)
abline(a = 0, b = 1, col = 'red', lty = 'dashed')
legend(
  'topleft',
  c('nn_1','nn_5'),
  pch = 1,
  col = c('blue','orange'),
  bty = 'n'
)
dev.off()
par(opar)

#######################################################################################
#######         Step 14: Performance Evaluation - Compare Model Accuracy
#######         Use log-normal transformed data set for this one
#######         !!!!!!!!! Perform Steps 1-8 prior running this step (may skip boxplots/qqlines)
#######################################################################################
#######         Step 14.A. Decision tree training + tuning
####### 

TCWAJ3 <- TCWAJ3_transformed
TCWAJ3$traffic_level <- as.character(TCWAJ3$traffic_level)
TCWAJ3 <- within(TCWAJ3, traffic_level[traffic_level != "moderate to heavy congestion"]
                 <- "clear to moderate congestion")
TCWAJ3$traffic_level <- as.factor(TCWAJ3$traffic_level)

colnames(TCWAJ3); TCWAJ3 <- TCWAJ3[,c(-1,-18,-20,-22,-23,-25)]; 
#joinvalue, jt_speed, jt_journeytime, road, iDir, lat, long, year
index <- sample(nrow(TCWAJ3), nrow(TCWAJ3) * 0.7)
train_pred <- TCWAJ3[index,]
test_pred <- TCWAJ3[-index,]


str(train_pred)
#define a formula 

# define a formula for predicting churn

formula = traffic_level ~ overall_weather + apparentTemp +
  dewPoint + humidity + windSpeed + windBearing + visibility +
  cloudCover + hour + date + month  +
  HGVA3 + HGVA5 + AMV + acc_accidents +
  jt_journeyTime + borough +
  iDir + long_lat

# set the parameters for tuning to 10-fold CV
ctrl_parameters <- trainControl(method = 'CV', number = 10)

# check the tunable parameter available for rpart
#   note: the algorithm has a parameter for tree complexity
modelLookup('rpart')

# train a decision tree using caret train function
#   note: rpart is a function to build decision trees in R
#     and the training parameters are passed as option trControl
lapply(train_pred, levels) #children has only one value. remove it.

join_tree <- train(formula, 
                   data = train_pred, method = "rpart", trControl = ctrl_parameters)

# inspect the result of the training
#   note: the summary reports the parameter scan results
#     and the value for the best model
join_tree

########################################################################################
#######         Step 14.B. Random forests training + tuning
#######

# check the tunable parameter available for rf
modelLookup('rf')

# train a Random forests model using caret train function
#   note: rf is the algorithm available in the randomForest package
#     and the training parameters are passed as option trControl
join_rf <- train(formula, data = train_pred, method = "rf", trControl = ctrl_parameters)

# inspect the result of the training
join_rf

########################################################################################
#######         Step 14.C. Prediction
#######

# compute prediction with the tree model
#   note: combine actual, predicted and probability prediction in a data frame
#     by using cbind and the 'type' argument in the predict function
tree_predict <-  cbind(
  actual = test_pred$traffic_level,
  predicted = predict(join_tree, test_pred[, -20], type = 'raw'),
  predict(join_tree, test_pred[, -20], type = 'prob')
)

# compute prediction with the Random forests model
#   note: combine actual, predicted and probability prediction in a data frame
#     by using cbind and the 'type' argument in the predict function
rf_predict <-  cbind(
  actual = test_pred$traffic_level,
  predicted = predict(join_rf, test_pred[, -20], type = 'raw'),
  predict(join_rf, test_pred[, -20], type = 'prob')
)

########################################################################################
#######         Step 14.D. Performance evaluation
#######

# generate a confusion matrix for the each predicted model
#   and inspect them: the caret confusionMatrix function
#   returns also Accuracy, Kappa, Sensitivity and Specificity
#     note: the positive class should be explicitely declared
#       with the argument 'positive'
tree_confmat <- confusionMatrix(data = tree_predict$predicted, 
                                reference = tree_predict$actual, positive = "moderate to heavy congestion")
rf_confmat <- confusionMatrix(data = rf_predict$predicted, 
                              reference = rf_predict$actual, positive = "moderate to heavy congestion")
tree_confmat
rf_confmat

# prepare two data frames to generate a ROC curve:
#   a data frame with the probability scores for the prediction of high_grossing
#   a data frame with the actual classes (repeated twice)
models_prob <- data.frame(
  tree = tree_predict[,"moderate to heavy congestion"],
  rf = rf_predict[,"moderate to heavy congestion"]
)
label <- data.frame(
  tree = tree_predict$actual,
  rf = rf_predict$actual
)
# ROCR requires to create a prediction and a performance object
#   note: the performance object can be created for different measures
#     e.g. TPR and FPR in this case
ROC_pred = prediction(models_prob, label)
ROC_perf = performance(ROC_pred, "tpr", "fpr")

pdf(file="ROCR LDP for moderate to heavy congestion.pdf")
# plot the ROC curve for the two methods
opar <- par()
par(pty = 's')
plot(
  ROC_perf,
  col = as.list(c("orange", "blue"))
)
abline(a = 0, b = 1, lty = 2, col = 'red')
legend(
  "bottomright",
  names(models_prob),
  col = c("orange", "blue"),
  lty = 1,
  bty = 'n'
)
par <- opar



dev.off()

########################################################################################