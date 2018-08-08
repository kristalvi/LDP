#Step 1: Set-up working directory and library
setwd("###")
.libPaths("C:\\Rlibs")

#Step 2: Install packages
install.packages("darksky")
install.packages("maps")
install.packages("pmap")
install.packages("purrr")
install.packages("factoextra")
install.packages("NbClust")
install.packages("tibble")

#packages for long lat conversion
install.packages("rgdal")
install.packages("maps")
install.packages("maptools")
install.packages("OpenStreetMap")
install.packages("raster")

install.packages("gdata")

#Step 3: Load necessary packages
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

#Step 4: Load the datasets
#a. Traffic Counts
traffic_MajR <- read.csv("Traffic-Major-Roads.csv")
traffic_MinR <- read.csv("Traffic-Minor-Roads.csv")

        index <- traffic_MajR$Region.Name..GO.=='London'
        london_traffic_MajR <- traffic_MajR[index,]
        head(london_traffic_MajR)
        
        #2015
        index <- london_traffic_MajR$Year=='2015'
        london_traffic_MajR_2015 <- london_traffic_MajR[index,]
        unique(london_traffic_MajR_2015$month)
        
        #2014
        index <- london_traffic_MajR$Year=='2014'
        london_traffic_MajR_2014 <- london_traffic_MajR[index,]
        unique(london_traffic_MajR_2014$month)
        
        #2013
        index <- london_traffic_MajR$Year=='2013'
        london_traffic_MajR_2013 <- london_traffic_MajR[index,]
        unique(london_traffic_MajR_2013$month)
        
        #2012
        index <- london_traffic_MajR$Year=='2012'
        london_traffic_MajR_2012 <- london_traffic_MajR[index,]
        unique(london_traffic_MajR_2012$month)
        
        #2011
        index <- london_traffic_MajR$Year=='2011'
        london_traffic_MajR_2011 <- london_traffic_MajR[index,]
        unique(london_traffic_MajR_2011$month)
        
        #2010
        index <- london_traffic_MajR$Year=='2010'
        london_traffic_MajR_2010 <- london_traffic_MajR[index,]
        unique(london_traffic_MajR_2010$month)
        
        #2009
        index <- london_traffic_MajR$Year=='2009'
        london_traffic_MajR_2009 <- london_traffic_MajR[index,]
        unique(london_traffic_MajR_2009$month)
        
        #Traffic Minor
        
        london_traffic_MinR <- traffic_MinR[traffic_MinR$Region.Name..GO.=="London",]
        london_traffic_MinR$month <- substr(x=london_traffic_MinR$dCount, start=6, stop=7) #get the month 
        
        #2015
        index <- london_traffic_MinR$Year=='2015'
        london_traffic_MinR_2015 <- london_traffic_MinR[index,]
        unique(london_traffic_MinR_2015$month)
        
        #2014
        index <- london_traffic_MinR$Year=='2014'
        london_traffic_MinR_2014 <- london_traffic_MinR[index,]
        unique(london_traffic_MinR_2014$month)
        
        #2013
        index <- london_traffic_MinR$Year=='2013'
        london_traffic_MinR_2013 <- london_traffic_MinR[index,]
        unique(london_traffic_MinR_2013$month)
        
        #2012
        index <- london_traffic_MinR$Year=='2012'
        london_traffic_MinR_2012 <- london_traffic_MinR[index,]
        unique(london_traffic_MinR_2012$month)
        
        #2011
        index <- london_traffic_MinR$Year=='2011'
        london_traffic_MinR_2011 <- london_traffic_MinR[index,]
        unique(london_traffic_MinR_2011$month)
        
        #2010
        index <- london_traffic_MinR$Year=='2010'
        london_traffic_MinR_2010 <- london_traffic_MinR[index,]
        unique(london_traffic_MinR_2010$month)
        
        #2009
        index <- london_traffic_MinR$Year=='2009'
        london_traffic_MinR_2009 <- london_traffic_MinR[index,]
        unique(london_traffic_MinR_2009$month)

#b. Accidents
        Accidents_2009 <- read.csv("Accidents_2009.csv", stringsAsFactors = FALSE)
        Accidents_2010 <- read.csv("Accidents_2010.csv", stringsAsFactors = FALSE)
        Accidents_2011 <- read.csv("Accidents_2011.csv", stringsAsFactors = FALSE)
        Accidents_2012 <- read.csv("Accidents_2012.csv", stringsAsFactors = FALSE)
        Accidents_2013 <- read.csv("Accidents_2013.csv", stringsAsFactors = FALSE)
        Accidents_2014 <- read.csv("Accidents_2014.csv", stringsAsFactors = FALSE)
        Accidents_2015 <- read.csv("Accidents_2015.csv", stringsAsFactors = FALSE)
        Accidents_2016 <- read.csv("Accidents_2016.csv", stringsAsFactors = FALSE)
        
        Accidents <- bind_rows(Accidents_2009, Accidents_2010,
                               Accidents_2011, Accidents_2012,
                               Accidents_2013, Accidents_2014,
                               Accidents_2015, Accidents_2016)
        
#c. Journey Time
setwd("C:\\Users\\Kristal Gazmen\\Desktop\\Kristal\\2. Education\\b. Brunel University London\\4. 2nd Sem\\5. CS5609 Learning Development Project Lab\\Coursework\\Datasets\\Highways Agency network journey time and traffic flow data\\Unzipped")
        
        JourneyTime_Jan09 <- read.table("JourneyTime_Jan09.txt", header=TRUE, sep=",")
        JourneyTime_Jan10 <- read.csv("JourneyTime_Jan10.csv")
        JourneyTime_Jan11 <- read.csv("JourneyTime_Jan11.csv")
        JourneyTime_Jan12 <- read.table("JourneyTime_Jan12.txt", header=TRUE, sep=",")
        JourneyTime_Jan13 <- read.csv("JourneyTime_Jan13.csv")
        JourneyTime_Jan14 <- read.csv("JourneyTime_Jan14.csv")
        
        JourneyTime_Feb09 <- read.table("JourneyTime_Feb09.txt", header=TRUE, sep=",")
        JourneyTime_Feb10 <- read.csv("JourneyTime_Feb10.csv")
        JourneyTime_Feb11 <- read.csv("JourneyTime_Feb11.csv")
        JourneyTime_Feb12 <- read.csv("JourneyTime_Feb12.csv")
        JourneyTime_Feb13 <- read.csv("JourneyTime_Feb13.csv")
        JourneyTime_Feb14 <- read.csv("JourneyTime_Feb14.csv")
        
        JourneyTime_Mar09 <- read.table("JourneyTime_Mar09.txt", header=TRUE, sep=",")
        JourneyTime_Mar10 <- read.csv("JourneyTime_Mar10.csv")
        JourneyTime_Mar11 <- read.csv("JourneyTime_Mar11.csv")
        JourneyTime_Mar12 <- read.csv("JourneyTime_Mar12.csv")
        JourneyTime_Mar13 <- read.table("JourneyTime_Mar13.txt", header=TRUE, sep=",")
        JourneyTime_Mar14 <- read.csv("JourneyTime_Mar14.csv")
        
        JourneyTime_Aor09 <- read.csv("JourneyTime_Apr09.csv")
        JourneyTime_Apr10 <- read.csv("JourneyTime_Apr10.csv")
        JourneyTime_Apr11 <- read.csv("JourneyTime_Apr11.csv")
        JourneyTime_Apr12 <- read.csv("JourneyTime_Apr12.csv")
        JourneyTime_Apr13 <- read.table("JourneyTime_Apr13.txt", header=TRUE, sep=",")
        JourneyTime_Apr14 <- read.csv("JourneyTime_Apr14.csv")
        
        JourneyTime_May09 <- read.csv("JourneyTime_May09.csv")
        JourneyTime_May10 <- read.csv("JourneyTime_May10.csv")
        JourneyTime_May11 <- read.csv("JourneyTime_May11.csv")
        JourneyTime_May12 <- read.csv("JourneyTime_May12.csv")
        JourneyTime_May13 <- read.table("JourneyTime_May13.txt", header=TRUE, sep=",")
        JourneyTime_May14 <- read.csv("JourneyTime_May14.csv")
        
        JourneyTime_Jun09 <- read.csv("JourneyTime_Jun09.csv")
        JourneyTime_Jun10 <- read.csv("JourneyTime_Jun10.csv")
        JourneyTime_Jun11 <- read.csv("JourneyTime_Jun11.csv")
        JourneyTime_Jun12 <- read.csv("JourneyTime_Jun12.csv")
        JourneyTime_Jun13 <- read.table("JourneyTime_Jun13.txt", header=TRUE, sep=",")
        JourneyTime_Jun14 <- read.csv("JourneyTime_Jun14.csv")
        
        JourneyTime_Jul09 <- read.csv("JourneyTime_Jul09.csv")
        JourneyTime_Jul10 <- read.csv("JourneyTime_Jul10.csv")
        JourneyTime_Jul11 <- read.csv("JourneyTime_Jul11.csv")
        JourneyTime_Jul12 <- read.csv("JourneyTime_Jul12.csv")
        JourneyTime_Jul13 <- read.csv("JourneyTime_Jul13.csv")
        JourneyTime_Jul14 <- read.csv("JourneyTime_Jul14.csv")
        
        JourneyTime_Aug09 <- read.csv("JourneyTime_Aug09.csv")
        JourneyTime_Aug10 <- read.csv("JourneyTime_Aug10.csv")
        JourneyTime_Aug11 <- read.csv("JourneyTime_Aug11.csv")
        JourneyTime_Aug12 <- read.csv("JourneyTime_Aug12.csv")
        JourneyTime_Aug13 <- read.csv("JourneyTime_Aug13.csv")
        JourneyTime_Aug14 <- read.csv("JourneyTime_Aug14.csv")
        
        JourneyTime_Sep09 <- read.csv("JourneyTime_Sep09.csv")
        JourneyTime_Sep10 <- read.csv("JourneyTime_Sep10.csv")
        JourneyTime_Sep11 <- read.csv("JourneyTime_Sep11.csv")
        JourneyTime_Sep12 <- read.csv("JourneyTime_Sep12.csv")
        JourneyTime_Sep13 <- read.csv("JourneyTime_Sep13.csv")
        JourneyTime_Sep14 <- read.csv("JourneyTime_Sep14.csv")
        
        JourneyTime_Oct09 <- read.csv("JourneyTime_Oct09.csv")
        JourneyTime_Oct10 <- read.csv("JourneyTime_Oct10.csv")
        JourneyTime_Oct11 <- read.csv("JourneyTime_OCt11.csv")
        JourneyTime_Oct12 <- read.csv("JourneyTime_Oct12.csv")
        JourneyTime_Oct13 <- read.csv("JourneyTime_Oct13.csv")
        JourneyTime_Oct14 <- read.csv("JourneyTime_Oct14.csv")
        
        JourneyTime_Nov09 <- read.csv("JourneyTime_Nov09.csv")
        JourneyTime_Nov10 <- read.csv("JourneyTime_Nov10.csv")
        JourneyTime_Nov11 <- read.csv("JourneyTime_Nov11.csv")
        JourneyTime_Nov12 <- read.csv("JourneyTime_Nov12.csv")
        JourneyTime_Nov13 <- read.csv("JourneyTime_Nov13.csv")
        JourneyTime_Nov14 <- read.csv("JourneyTime_Nov14.csv")
        
        JourneyTime_Dec09 <- read.csv("JourneyTime_Dec09.csv")
        JourneyTime_Dec10 <- read.csv("JourneyTime_Dec10.csv")
        JourneyTime_Dec11 <- read.csv("JourneyTime_Dec11.csv")
        JourneyTime_Dec12 <- read.csv("JourneyTime_Dec12.csv")
        JourneyTime_Dec13 <- read.csv("JourneyTime_Dec13.csv")
        JourneyTime_Dec14 <- read.csv("JourneyTime_Dec14.csv") 
        
        #Joining Tables per Year
        JourneyTime_2009 <- bind_rows(JourneyTime_Jan09, JourneyTime_Feb09, JourneyTime_Mar09,
                                      JourneyTime_Aor09, JourneyTime_May09, JourneyTime_Jun09,
                                      JourneyTime_Jul09, JourneyTime_Aug09, JourneyTime_Sep09,
                                      JourneyTime_Oct09, JourneyTime_Nov09, JourneyTime_Dec09)
        
        JourneyTime_2010 <- bind_rows(JourneyTime_Jan10, JourneyTime_Feb10, JourneyTime_Mar10,
                                      JourneyTime_Apr10, JourneyTime_May10, JourneyTime_Jun10,
                                      JourneyTime_Jul10, JourneyTime_Aug10, JourneyTime_Sep10,
                                      JourneyTime_Oct10, JourneyTime_Nov10, JourneyTime_Dec10)
        
        JourneyTime_2011 <- bind_rows(JourneyTime_Jan11, JourneyTime_Feb11, JourneyTime_Mar11,
                                      JourneyTime_Apr11, JourneyTime_May11, JourneyTime_Jun11,
                                      JourneyTime_Jul11, JourneyTime_Aug11, JourneyTime_Sep11,
                                      JourneyTime_Oct11, JourneyTime_Nov11, JourneyTime_Dec11)
        
        JourneyTime_2012 <- bind_rows(JourneyTime_Jan12, JourneyTime_Feb12, JourneyTime_Mar12,
                                      JourneyTime_Apr12, JourneyTime_May12, JourneyTime_Jun12,
                                      JourneyTime_Jul12, JourneyTime_Aug12, JourneyTime_Sep12,
                                      JourneyTime_Oct12, JourneyTime_Nov12, JourneyTime_Dec12)
        
        JourneyTime_2013 <- bind_rows(JourneyTime_Jan13, JourneyTime_Feb13, JourneyTime_Mar13,
                                      JourneyTime_Apr13, JourneyTime_May13, JourneyTime_Jun13,
                                      JourneyTime_Jul13, JourneyTime_Aug13, JourneyTime_Sep13,
                                      JourneyTime_Oct13, JourneyTime_Nov13, JourneyTime_Dec13)
        
        JourneyTime_2014 <- bind_rows(JourneyTime_Jan14, JourneyTime_Feb14, JourneyTime_Mar14,
                                      JourneyTime_Apr14, JourneyTime_May14, JourneyTime_Jun14,
                                      JourneyTime_Jul14, JourneyTime_Aug14, JourneyTime_Sep14,
                                      JourneyTime_Oct14, JourneyTime_Nov14, JourneyTime_Dec14)
        
        JourneyTime_09to14 <- bind_rows(JourneyTime_2009, JourneyTime_2010, JourneyTime_2011,
                                        JourneyTime_2012, JourneyTime_2013, JourneyTime_2014)
        
        Link_Coordinates <- read.csv("Link Coordinates.csv")
        HourRef <- read.csv("HourRef.csv")
        
        #d. Borough List
        Borough_ref <- read.csv("Borough List.csv")


#Step 5: Download weather data
darksky_api_key(force=TRUE)
#enter API key from the DarkSky website
      
      #5A: Create a dataframe or reference for the long and lat of each borough in London. Concatenante long and lat.
      England_trafMajR <- traffic_MajR[traffic_MajR$Region.Name..GO.=="London"]
      London_longlat <-  as.data.frame(unique(England_trafMajR$ONS.LA.Name)) #Filtering traffic counts for London only
      lat <- c(51.6500,51.499998, 51.5, 51.53622, 51.4333, 51.4951, 51.509865, 51.654827, 51.6,
               51.5, 51.5, 51.55, 51.5833, 51.8038, 51.5, 51.476852, 51.4, 51.45, 51.45,
               51.376495, 51.4333, 51.4667, 51.5672808, 51.583015, 51.5438, 51.4167, 51.5, 50.9167,
               51.3500000, 51.409774, 51.6156, 51.5453, 51.51279)
      long <- c(-0.2000,-0.4499982, -0.0167,-0.10304, 0.15, -0.2061, -0.118092, -0.083599, -0.0333,
                -0.1167, -0.3167, -0.05, -0.0833, -2.4494, -0.0833, -0.000500, 0.05, -0.0167, -0.2,
                -0.100594, -0.3, -0.35, -0.2710568, -0.337820, -0.1399, -0.2833, -0.2333, -1.4667,
                -0.2000000, 	-0.210809, 0.1861, 0.1337, -0.09184)
      London_longlat$borough <- London_longlat[,1]
      London_longlat2 <- cbind(London_longlat, lat, long)
      London_longlat <- London_longlat2[,-1]
      London_longlat$longlat <- paste(London_longlat$long,London_longlat$lat)

      #5B: Create a reference table for each long and lat for each London boroughs, which will be used to extract hourly weather data
      traffic_road_list <- London_longlat
      traffic_road <- traffic_road_list$longlat
      traffic_lat <- traffic_road_list$lat
      traffic_long <- traffic_road_list$long
      
      #5C: Remove scientific notation for long and lat
      options(scipen=999)
         
      
      #Download the weather data for the first date of the time period (1st iteration). Define "extracted_hourly"
      more_than_one <- data.frame(loc=c("London"),
                                  lat=c(51.5074),
                                  lon=c(0.1278),
                                  when=c("2014-01-01T12:00:00-0400"),
                                  stringsAsFactors=FALSE)
      bigger_list <- pmap(list(more_than_one$lat, more_than_one$lon,
                               more_than_one$when),
                          get_forecast_for)
      #darksky_api_key(force = FALSE)
      
      names(bigger_list) <- more_than_one$loc
      London <- bigger_list$London
      extracted_hourly <- London$hourly
      extracted_hourly$lat <- 51.5074
      extracted_hourly$long <- 0.1278
      extracted_hourly3 <- extracted_hourly
      
      #create a function that will append the downloaded data to extracted_hourly
      extract <- function (x) {
        more_than_one <- data.frame(loc=traffic_road,
                                    lat=traffic_lat,
                                    lon=traffic_long,
                                    when=rep(c(x),length(traffic_road)),
                                    stringsAsFactors=FALSE)
        bigger_list <- pmap(list(more_than_one$lat, more_than_one$lon,
                                 more_than_one$when),get_forecast_for)
        names(bigger_list) <- more_than_one$loc
        for (i in 1:length(traffic_road)) {
          subset <- bigger_list[[i]]
          extracted_hourly3 <- subset$hourly
          extracted_hourly3$lat <- traffic_lat[[i]]
          extracted_hourly3$long <- traffic_long[[i]]
          extracted_hourly <- bind_rows(extracted_hourly,extracted_hourly3)
        }
        return(extracted_hourly)
      }
      
      
      #Run the loop for the time period. Start at 2nd iteratio (Jan 2 isntead of Jan 1, 2014).
      for (i in seq(as.Date('2012-12-10 '),as.Date('2012-12-31'),by = 1)) {
        x = paste(as.Date(i,origin="1970-01-01"),"T12:00:00-0400",sep="")
        extracted_hourly3 <- bind_rows(extracted_hourly3, extract(x))
      }
      
          
            #5D-iv: Assign the extract to a new data frame or convert to csv. Please rename 'df'
            borough_weather_201210Dec31Dec <- extracted_hourly3
            darksky_api_key(force=TRUE)
            df <- extracted_hourly3
            write.csv(df, "df.csv")

      #5E: Use cluster analaysis to group roads
            
            #5E-i: Create an "hour" column based on the dCount column
            compiled_extract$time <- substr(x=compiled_extract$time, start=12,stop=13) 
            
            #5E-ii: Create separate tables for each hour
            filtered0 <- filter(compiled_extract,time==0)
            filtered1 <- filter(compiled_extract,time==1)
            filtered2 <- filter(compiled_extract,time==2)
            filtered3 <- filter(compiled_extract,time==3)
            filtered4 <- filter(compiled_extract,time==4)
            filtered5 <- filter(compiled_extract,time==5)
            filtered6 <- filter(compiled_extract,time==6)
            filtered7 <- filter(compiled_extract,time==7)
            filtered8 <- filter(compiled_extract,time==8)
            filtered9 <- filter(compiled_extract,time==9)
            filtered10 <- filter(compiled_extract,time==10)
            filtered11 <- filter(compiled_extract,time==11)
            filtered12 <- filter(compiled_extract,time==12)
            filtered13 <- filter(compiled_extract,time==13)
            filtered14 <- filter(compiled_extract,time==14)
            filtered15 <- filter(compiled_extract,time==15)
            filtered16 <- filter(compiled_extract,time==16)
            filtered17 <- filter(compiled_extract,time==17)
            filtered18 <- filter(compiled_extract,time==18)
            filtered19 <- filter(compiled_extract,time==19)
            filtered20 <- filter(compiled_extract,time==20)
            filtered21 <- filter(compiled_extract,time==21)
            filtered22 <- filter(compiled_extract,time==22)
            filtered23 <- filter(compiled_extract,time==23)
            
            #5E-iii: Append each filtered table as a new column for each road
            filtered <- left_join(filtered0, filtered1, by=c("longlat"="longlat"))
            filtered <- left_join(filtered, filtered2, by=c("longlat"="longlat"))
            filtered <- left_join(filtered, filtered3, by=c("longlat"="longlat"))
            filtered <- left_join(filtered, filtered4, by=c("longlat"="longlat"))
            filtered <- left_join(filtered, filtered5, by=c("longlat"="longlat"))
            filtered <- left_join(filtered, filtered6, by=c("longlat"="longlat"))
            filtered <- left_join(filtered, filtered7, by=c("longlat"="longlat"))
            filtered <- left_join(filtered, filtered8, by=c("longlat"="longlat"))
            filtered <- left_join(filtered, filtered9, by=c("longlat"="longlat"))
            filtered <- left_join(filtered, filtered10, by=c("longlat"="longlat"))
            filtered <- left_join(filtered, filtered11, by=c("longlat"="longlat"))
            filtered <- left_join(filtered, filtered12, by=c("longlat"="longlat"))
            filtered <- left_join(filtered, filtered13, by=c("longlat"="longlat"))
            filtered <- left_join(filtered, filtered14, by=c("longlat"="longlat"))
            filtered <- left_join(filtered, filtered15, by=c("longlat"="longlat"))
            filtered <- left_join(filtered, filtered16, by=c("longlat"="longlat"))
            filtered <- left_join(filtered, filtered17, by=c("longlat"="longlat"))
            filtered <- left_join(filtered, filtered18, by=c("longlat"="longlat"))
            filtered <- left_join(filtered, filtered19, by=c("longlat"="longlat"))
            filtered <- left_join(filtered, filtered20, by=c("longlat"="longlat"))
            filtered <- left_join(filtered, filtered21, by=c("longlat"="longlat"))
            filtered <- left_join(filtered, filtered22, by=c("longlat"="longlat"))
            filtered <- left_join(filtered, filtered23, by=c("longlat"="longlat"))
            
            #5E-iv: Use differnet methods to determine the optimal 'k'
            LDP_MajR <- filtered
            rows_sample <- sample(nrow(LDP_MajR),nrow(LDP_MajR) * 0.70)
            majR_train <- LDP_MajR[rows_sample,]
            majR_test <- LDP_MajR[-rows_sample,]
              # Elbow method
              fviz_nbclust(majR_train, kmeans, method = "wss") +
                geom_vline(xintercept = 4, linetype = 2)+
                labs(subtitle = "Elbow method")
              # Silhouette method
              fviz_nbclust(majR_train, kmeans, method = "silhouette")+
                labs(subtitle = "Silhouette method")
              # Gap statistic
              # nboot = 50 to keep the function speedy. 
              # recommended value: nboot= 500 for your analysis.
              # Use verbose = FALSE to hide computing progression.
              set.seed(123)
              fviz_nbclust(majR_train, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
                labs(subtitle = "Gap statistic method")
              
              #The resutl will shwo 2-4 clusters, which may not be the best method to see the impact of weathre to the traffic per road
              
      #5F: new strategy is to do a per borough extract for LOndon only
            london_weather_2014 <- bind_rows(borough_weather_201406Dec03Jan,borough_weather_201407Nov05Dec,
                                               borough_weather_201409Oct06Nov,borough_weather_201411Sep08Oct,
                                               borough_weather_201413Aug10Sep,borough_weather_201414Jul12Aug,
                                               borough_weather_201415Jun13Jul,borough_weather_201416May14Jun,
                                               borough_weather_201418Apr15May,borough_weather_2014FebApril17,
                                               borough_weather_2014Jan)
            weather$longlat <- paste(weather$long, weather$lat)
              
            weather <- merge(weather, London_longlat2, by="longlat", all.x=TRUE)
            index <- is.na(weather$borough)==FALSE
            weather2 <- weather[index,]
            dim(weather)
    
            
#Step 6: Join Weather and Traffic Count Data for Major and then Minor Roads

      #6-i: Create two new columns --date and hour
            weather$dCount <- substr(x=weather$time, start=1,stop=10) #get the date
            weather$Hour <- substr(x=weather$time, start=12,stop=13) #get the hour
            weather$month <- substr(x=weather$dCount, start=6, stop=7) #get the month 
            weather$Hour <- as.numeric(weather$Hour)
            
      #6-ii: Create a 'join value' by concatenating the region, the borough name, the date, and the hour.
            traffic_MajR$joinvalue <- paste(traffic_MajR$Region.Name..GO., traffic_MajR$ONS.LA.Name, 
                                            traffic_MajR$dCount, traffic_MajR$Hour)
            traffic_MinR$joinvalue <- paste(traffic_MinR$Region.Name..GO., traffic_MinR$ONS.LA.Name, 
                                            traffic_MinR$dCount, traffic_MinR$Hour)
            
      #6-iii: Filter by one city and year
            traffic_MajR <- traffic_MajR[traffic_MajR$Region.Name..GO.=="London",]
            traffic_MajR <- traffic_MajR[traffic_MajR$Year=="2014",]
                        
            traffic_MinR <- traffic_MinR[traffic_MinR$Region.Name..GO.=="London",]
            traffic_MinR <- traffic_MinR[traffic_MinR$Year=="2014",]

      #6-iv: Merging the Traffic Count and Weather Datasets
            weather$joinvalue <- paste("London", weather$borough,
                                    weather$dCount, weather$Hour)
                        
            majorTC_weather <- merge(weather, traffic_MajR, by='joinvalue', all=TRUE)
            minorTC_weather <- merge(weather, traffic_MinR, by='joinvalue', all=TRUE)

      #6-v: Remove the initial "London" entry (not the one by borough. See code at line 126)
            index <- is.na(majorTC_weather$Region.Name..GO.)==FALSE
            majorTC_weather <- majorTC_weather[index,]
          
            index <- is.na(minorTC_weather$Region.Name..GO.)==FALSE
            minorTC_weather <- minorTC_weather[index,]

#Step 7: Join Minor and Major Roads by creating new columns and binding the rows

      #7-i:insert columns on traffic MinR
            
            minorTC_weather$A.Junction <- NA
            minorTC_weather$A.Ref.E <- NA
            minorTC_weather$A.Ref.N <- NA
            minorTC_weather$B.Junction <- NA
            minorTC_weather$B.Ref.E <- NA
            minorTC_weather$B.Ref.N <- NA
            
            colnames(minorTC_weather)
            
            index <- minorTC_weather[,c(1:35)]
            index2 <- minorTC_weather[,c(36:53)]
            index3 <- minorTC_weather[,c(55:60)]
            
            minorTC_weather <- cbind(index,index3,index2)
            colnames(minorTC_weather)
            
      #7-ii: insert columns on traffic MajR
            
            colnames(majorTC_weather)
            majorTC_weather$Road.Name <- NA
            majorTC_weather$CP.Location <- NA
            
            index <- majorTC_weather[,c(1:33)] #until road
            index2 <- majorTC_weather[,c(59:60)]
            index3 <- majorTC_weather[,c(34:57)]
            
            majorTC_weather <- cbind(index,index2,index3)
            colnames(majorTC_weather)
            
            
      #7-iii: Combine major and minor
            TC_weather <- bind_rows(majorTC_weather,minorTC_weather)
            dim(TC_weather)
            head(TC_weather)
            
      #7-iv: Merge accidents and major and minor dataset
            london2014_trafficweather$long_R <- round(london2014_trafficweather$long.x, digits=3)
            london2014_trafficweather$lat_R <- round(london2014_trafficweather$lat.x, digits=3)
            london2014_trafficweather$longlat2 <- paste(london2014_trafficweather$long_R, london2014_trafficweather$lat_R)
            
#Step 8: Data preparation for Accidents data
      #8-i: Group Accidents Data
            Accidents$longlat <- paste(round(Accidents$Longitude, digits=1)
                                       ,round(Accidents$Latitude, digits=1))
            Accidents$Hour <- as.numeric(substr(x=Accidents$Time, 
                                                       start=1,stop=2)) #get the hour
            Accidents$dCount <- as.Date(paste(substr(x=Accidents$Date, start=7,stop=10),"-",
                                             substr(x=Accidents$Date, start=4,stop=5),"-",
                                             substr(x=Accidents$Date, start=1,stop=2),sep=""))

            colnames(Accidents)
            colnames(Accidents)[33] = "Accident_Index2"
            
            Agg_Accidents <- Accidents %>%
              group_by(longlat,dCount,Hour) %>%
              summarize(Total_Vehicles = sum(Number_of_Vehicles),
                        Total_Casualties = sum(Number_of_Casualties),
                        Count_Accidents = n(),
                        Average_SpeedLimit = mean(Speed_limit)
                        )
            
            head(Agg_Accidents)
            
      #8-ii: Create a common identifier   
            Agg_Accidents$joinvalue <- paste(Agg_Accidents$borough, Agg_Accidents$longlat2, 
                                            Agg_Accidents$dCount, Agg_Accidents$Hour)
            #Rename joinvalue and replace borough detail with longlat
            
            london2014_trafficweather$joinvalue <- paste(london2014_trafficweather$borough, london2014_trafficweather$longlat2,
                                                      london2014_trafficweather$dCount.x, london2014_trafficweather$Hour.x)
            
      #8-iii: Merge with London Traffic Count and Weather Data
            London2014_TCWA <- merge(london2014_trafficweather, Agg_Accidents, 
                                     by="joinvalue", all.x=TRUE)
            dim(London2014_TCWA)
            head(London2014_TCWA)
            table(London2014_TCWA$Total_Casualties)
            
            
#Step 9: Data Preparation with Journey Time
            colnames(JourneyTime_2014)
            colnames(Link_Coordinates)
            colnames(Link_Coordinates)[1] <- "LinkRef"
            JourneyTime_v2 <- left_join(JourneyTime_2014, Link_Coordinates_v2, by="LinkRef")
            JourneyTime_v2 <- JourneyTime_v2[,c(-21:-27)]
            JourneyTime_v2$StartXY <- paste(JourneyTime_v2$StartX, JourneyTime_v2$StartY)
            JourneyTime_v2$EndXY <- paste(JourneyTime_v2$EndX, JourneyTime_v2$EndY)
            JourneyTime_v2$StartEnd <- paste(JourneyTime_v2$StartXY, JourneyTime_v2$EndXY,
                                             sep="")
            JourneyTime_v2$longlat <- paste(JourneyTime_v2$long_R, JourneyTime_v2$lat_R)
            
            JourneyTime <- left_join(Journey, HourRef, by="TimePeriod")
            
            London2014_TCWA$StartXY <- paste(London2014_TCWA$A.Ref.E, London2014_TCWA$A.Ref.N)
            London2014_TCWA$EndXY <- paste(London2014_TCWA$B.Ref.E, London2014_TCWA$B.Ref.N)
            London2014_TCWA$StartEnd <- paste(London2014_TCWA$StartXY, London2014_TCWA$EndXY,
                                              sep="")
            
            test <- JourneyTime_v2$longlat %in% London2014_TCWA$longlat
            table(test)
            
            Agg_Journey <- Journey %>%
              group_by(LinkRef,Road,LinkDescription.x,
                       startLong,startLat,endLong,endLatDate,Hour) %>%
              
              summarize(Average_Speed=mean(AverageSpeed,na.rm=FALSE),
                        Average_JT=mean(AverageJT,na.rm=FALSE),
                        Average_Flow=mean(Flow,na.rm=FALSE)
              )
            
    
            Agg_Journey2014$dCount <- substr(x=Agg_Journey2014$Date, start=1,stop=10) #get the date
            Agg_Journey2014$joinvalue <- paste(Agg_Journey2014$longlat,
                                               Agg_Journey2014$dCount, Agg_Journey2014$Hour)
            
            colnames(London2014_TCWA)
            London2014_TCWAv2 <- London2014_TCWA[London2014_TCWA$S.Ref.E!="NA",]
            head(London2014_TCWAv2)
            
            
            Agg_Journey2014$joinvalue <- paste(Agg_Journey2014$StartX, Agg_Journey2014$StartY,
                                               Agg_Journey2014$EndX, Agg_Journey2014$EndY,
                                               Agg_Journey2014$dCount, Agg_Journey2014$Hour)
            
            Agg_Journey2014_v2 <- Agg_Journey2014[Agg_Journey2014$StartEnd!="NA NANA NA",]
            London2014_TCWA_v2 <- London2014_TCWA[London2014_TCWA$StartEnd!="NA NANA NA",]
            
            table(is.na(London2014_TCWA$StartEnd))
            
            
            London2014_TCWAJ <- left_join(London2014_TCWA,Agg_Journey2014,by="joinvalue",all.x=TRUE)
            
            index <- London2014_TCWAJ[is.na(London2014_TCWAJ$Average_JT),]
            index
            
            index <- London2014_TCWAJ[!index,]
            
            index <- London2014_TCWAv2$joinvalue %in% Agg_Journey2014$joinvalue
            table(index)
          
#Scratch. Clearing the directory from old objects.
            rm(list=setdiff(ls(), c("JOurneyTime_2014", "London_longlat", "London_longlat2", "Link_Coordinates",
                                    "Borough_ref", "borough_weather201317Jun17Jul", "borough_weather_201316Aug14Sep",
                                    "borough_weather_201323May16Jun", "borough_weather_201328Apr22May",
                                    "borough_weather_201328Mar27Apr", "borough_weather_2013Mar28",
                                    "borough_weather201317Jun17Jul", "london_weather_2014",
                                    "london_weather_2014_v2", "London2014_TCWA",
                                    "traffic_road_list", "traffic_road_list2",
                                    "Agg_Accidents", "Accidents_2014v2",
                                    "london2014_trafficweather")))

            
#convert easting and northing to long and lat
            colnames(Link_Coordinates)
            head(Link_Coordinates)
            Link_Coordinates2 <- Link_CoordinateS
            Link_Coordinates <- Link_Coordinates[is.na(Link_Coordinates$StartX)==FALSE &
                                                   is.na(Link_Coordinates$StartY)==FALSE &
                                                   Link_Coordinates$StartX!="#N/A" &&
                                                   Link_Coordinates$StartY!="#N/A",]
                                                 
            #Link_Coordinates <- subset(Link_Coordinates, StartX != "NA" | StartY != "NA")
            Link_Coordinates$Link_Coordinates_ID <- 1:nrow(Link_Coordinates)
            coords <- cbind(Easting = as.numeric(as.character(Link_Coordinates$StartX)),
                            Northing = as.numeric(as.character(Link_Coordinates$StartY)))
            dim(coords)
            Link_Coordinates$StartX
            Link_SP <- SpatialPointsDataFrame(coords, data=data.frame(Link_Coordinates$Link_ref,Link_Coordinates$Link_Coordinates_ID),
                                              proj4string=CRS("+init=espg:27700"))
            
            Link_Coordinates_LL <- spTransform(Link_SP, CRS(Latlong))
            
            colnames(Link_Coordinates_LL$coords)[colnames(Link_Coordinates_LL$coords)]
            
            
#more testing
#https://stackoverflow.com/questions/36520915/converting-utms-to-lat-long-in-r
            
            wgs84 = "+init=epsg:4326"
            bng = '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 
            +ellps=airy +datum=OSGB36 +units=m +no_defs'
            
            ConvertCoordinates <- function(easting,northing) {
              out = cbind(easting,northing)
              mask = !is.na(easting)
              sp <-  sp::spTransform(sp::SpatialPoints(list(easting[mask],northing[mask]),proj4string=sp::CRS(bng)),sp::CRS(wgs84))
              out[mask,]=sp@coords
              out
            }
  
            ConvertCoordinates(London2014_TCWA$S.Ref.E, London2014_TCWA$S.Ref.N)
            
#http://blogs.casa.ucl.ac.uk/2013/12/05/british-national-grid-transformation-and-reprojection-in-r/

                      #set the dataframe for conversion
                      colnames(JourneyTime_2014)
                      
                      data(world.cities)
                      uk = world.cities[world.cities$country.etc == 'UK' & world.cities$pop > 100000,]
                      head(uk)
                      
                      # coerce to a spatial object
                      coordinates(uk) = c('long','lat')
                      plot(uk)
            
            
            #Change link coordinates to numeric
            colnames(Link_Coordinates)
            
            Link_Coordinates$StartX <- as.numeric(as.character(Link_Coordinates$StartX))
            Link_Coordinates$EndX <- as.numeric(as.character(Link_Coordinates$EndX))
            Link_Coordinates$StartY <- as.numeric(as.character(Link_Coordinates$StartY))
            Link_Coordinates$EndY <- as.numeric(as.character(Link_Coordinates$EndY))
            index <- complete.cases(Link_Coordinates)
            Link_Coordinates <- Link_Coordinates[index,]
            
            
            #Assign Values
            #start
            uk <- Link_Coordinates[,c(5,6)]
            uk <- as.data.frame(uk)
            uk$StartX <- as.numeric(as.character(uk$StartX))
            uk$StartY <- as.numeric(as.character(uk$StartY))
            dim(uk)
            coordinates(uk) = c('StartX','StartY')
          
            #end
            uk <- Link_Coordinates[,c(7,8)]
            uk <- as.data.frame(uk)
            index <- complete.cases(uk)
            uk <- uk[index,]
            uk$EndX <- as.numeric(as.character(uk$EndX))
            uk$EndY <- as.numeric(as.character(uk$EndY))
            coordinates(uk) = c('EndX','EndY')
            
            # key proj4 transformations for exercise
            wgs84 = '+proj=longlat +datum=WGS84'
            bng = '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs'
            mrc = '+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs'
            
            # useful table of proj4 transformations
            epsg = make_EPSG()
            View(epsg[grep("OSGB", epsg$note),])
            
            # specify coordinate system as WGS84 (typical latlons)
            uk@proj4string   # slot will be empty
            uk@proj4string = CRS(wgs84)
            uk@proj4string = CRS(bng)
            
            # reproject data to Mercator coordinate system
            uk_merc = spTransform(uk, CRS(mrc))
            
            # download Mercator tiles covering the data area
            bbox = uk@bbox
            map = openmap(c(bbox[2,2],bbox[1,1]), c(bbox[2,1],bbox[1,2]), 7, 'osm')
            
            map = openmap(c(62.6886761482925,-7.94873046875),
                          c(40.22949814144951,6.9892578125), 7, 'osm')
            
            plot(map)
            plot(uk_merc, add=T)
            
            # conversion to British National Grid (OSGB36)
            map_bng = projectRaster(raster(openproj(map)), crs = bng)
            uk_bng = spTransform(uk_wgs84, CRS(bng))
            
            coordinates(uk_bng)
            plotRGB(map_bng)
            plot(uk_bng, add=T)
            
            #  BNG to WGS84 conversion
            #uk_wgs84 = spTransform(uk_bng, CRS(wgs84))
            uk_wgs84 = spTransform(uk, CRS(wgs84))
            coordinates(uk_wgs84)
            
            # custom cloudemade map tiles
            plot(openmap(c(bbox[2,2],bbox[1,1]), c(bbox[2,1],bbox[1,2]), 6, 'cloudmade-115496'))

            #assign values  
            coordinates_longlat_start <- coordinates(uk_wgs84)
            coordinates_longlat_start <- as.data.frame(coordinates_longlat_start)
            dim(coordinates_longlat_start)
          
            coordinates_longlat_end <- coordinates(uk_wgs84)
            coordinates_longlat_end <- as.data.frame(coordinates_longlat_end)
            dim(coordinates_longlat_end)
            
            Link_Coordinates_v2 <- Link_Coordinates[complete.cases(Link_Coordinates),]
            Link_Coordinates_v2 <- cbind(Link_Coordinates,
                                         coordinates_longlat_start,
                                         coordinates_longlat_end)
            head(Link_Coordinates_v2)
            dim(Link_Coordinates_v2)
            
            colnames(Link_Coordinates_v2)[10] <- "startLong"
            colnames(Link_Coordinates_v2)[11] <- "startLat"
            colnames(Link_Coordinates_v2)[12] <- "endLong"
            colnames(Link_Coordinates_v2)[13] <- "endLat"
            
            #Round digits up
            
            Link_Coordinates_v2$long_R <- round(as.numeric(Link_Coordinates_v2$StartLong), digits=3)
            Link_Coordinates_v2$lat_R <- round(as.numeric(Link_Coordinates_v2$StartLat), digits=3)
          
            
            colnames(Link_Coordinates_v2)[10] <- "StartLong"
            Link_Coordinates_v2$StartLong <- as.numeric(Link_Coordinates_v2$StartLong)
            colnames(Link_Coordinates_v2)[11] <- "StartLat"
            colnames(Link_Coordinates_v2)[12] <- "EndLong"
            colnames(Link_Coordinates_v2)[13] <- "EndLat"
            
            
            
            #7-iv: Merge accidents and major and minor dataset
            JourneyTime_v2$long_R <- round(JourneyTime_v2$startLong, digits=3)
            JourneyTime_v2$lat_R <- round(JourneyTime_v2$startLat, digits=3)
            JourneyTime_v2$longlat <- paste(JourneyTime_v2$long_R, JourneyTime_v2$lat_R)
            
            head(JourneyTime_v2)
            
            colnames(London2014_TCWA)
            
            
            summary(London2014_TCWA[,c("S.Ref.Longitude","S.Ref.Latitude")])
            
            #Change digit counts to 2 then 1
            #This is the Journey File
            JourneyTime_v2$long_R_start3 <- round(JourneyTime_v2$startLong, digits=3)
            JourneyTime_v2$lat_R_start3 <- round(JourneyTime_v2$startLat, digits=3)
            JourneyTime_v2$longlat_start3 <- paste(JourneyTime_v2$long_R_start3, JourneyTime_v2$lat_R_start3)
            
            JourneyTime_v2$long_R_end3 <- round(JourneyTime_v2$endLong, digits=3)
            JourneyTime_v2$lat_R_end3 <- round(JourneyTime_v2$endLat, digits=3)
            JourneyTime_v2$longlat_end3 <- paste(JourneyTime_v2$long_R_end3, JourneyTime_v2$lat_R_end3)
            
            JourneyTime_v2$long_R_start2 <- round(JourneyTime_v2$startLong, digits=2)
            JourneyTime_v2$lat_R_start2 <- round(JourneyTime_v2$startLat, digits=2)
            JourneyTime_v2$longlat_start2 <- paste(JourneyTime_v2$long_R_start2, JourneyTime_v2$lat_R_start2)
            
            JourneyTime_v2$long_R_end2 <- round(JourneyTime_v2$endLong, digits=2)
            JourneyTime_v2$lat_R_end2 <- round(JourneyTime_v2$endLat, digits=2)
            JourneyTime_v2$longlat_end2 <- paste(JourneyTime_v2$long_R_end2, JourneyTime_v2$lat_R_end2)
             
            JourneyTime_v2$long_R_start1 <- round(JourneyTime_v2$startLong, digits=1)
            JourneyTime_v2$lat_R_start1 <- round(JourneyTime_v2$startLat, digits=1)
            JourneyTime_v2$longlat_start1 <- paste(JourneyTime_v2$long_R_start1, JourneyTime_v2$lat_R_start1)
            
            JourneyTime_v2$long_R_end1 <- round(JourneyTime_v2$endLong, digits=1)
            JourneyTime_v2$lat_R_end1 <- round(JourneyTime_v2$endLat, digits=1)
            JourneyTime_v2$longlat_end1 <- paste(JourneyTime_v2$long_R_end1, JourneyTime_v2$lat_R_end1)
            
            JourneyTime_v2$midpoint_long <- round((JourneyTime_v2$startLong+JourneyTime_v2$endLong)/2, digits=1)
            JourneyTime_v2$midpoint_lat <- round((JourneyTime_v2$startLat+JourneyTime_v2$endLat)/2, digits=1)
            JourneyTime_v2$longlat_mid2 <- paste(JourneyTime_v2$midpoint_long, JourneyTime_v2$midpoint_lang)| 
              
            #Remove all NA
            JourneyTime_v2 <- JourneyTime_v2[complete.cases(JourneyTime_v2),]
            dim(JourneyTime_V2)
            head(JourneyTime_v2)
          
              Agg_Journey <- JourneyTime %>%
              group_by(joinvalue) %>%
              summarize(Average_Speed=mean(AverageSpeed,na.rm=FALSE),
                        Average_JT=mean(AverageJT,na.rm=FALSE),
                        AverageofAve_Flow=mean(Flow,na.rm=FALSE)
              )
            
            Agg_Journey2014$dCount <- substr(x=Agg_Journey2014$Date, start=1,stop=10) #get the date
            Agg_Journey2014$joinvalue <- paste(Agg_Journey2014$longlat_start2,
                                               Agg_Journey2014$dCount, Agg_Journey2014$Hour)
            
            #THis is the meged vehicle count, weather, and accidents dataset
            head(London2014_TCWA)
            London2014_TCWA$long_R <- round(London2014_TCWA$S.Ref.Longitude, digits=1)
            London2014_TCWA$lat_R <- round(London2014_TCWA$S.Ref.Latitude, digits=1)
            London2014_TCWA$longlat <- paste(London2014_TCWA$long_R, London2014_TCWA$lat_R)
            
            #Check if there are long and lats in common
            index <- London2014_TCWA$longlat %in% JourneyTime_v2$longlat_start3
            table(index)
            
            index <- London2014_TCWA$longlat %in% JourneyTime_v2$longlat_end3
            table(index)
            
            index <- London2014_TCWA$longlat %in% JourneyTime_v2$longlat_start2
            table(index)
            
            index <- London2014_TCWA$longlat %in% JourneyTime_v2$longlat_end2
            table(index)
            
            index <- London2014_TCWA$longlat %in% JourneyTime_v2$longlat_mid2
            table(index)
            
            index <- London2014_TCWA$longlat %in% JourneyTime_v2$longlat_start1
            table(index)
            
            index <- London2014_TCWA$longlat %in% JourneyTime_v2$longlat_end1
            table(index) 
            
            
            
            index <- traffic_MajR$longlat %in% JourneyTime_v2$longlat_start3
            table(index)
            
            
            London2014_TCWA$joinvalue <- paste(London2014_TCWA$longlat, 
                                               London2014_TCWA$dCount.x, London2014_TCWA$Hour.x)
            
            
             #Join all four tables or datasets
            London2014_TCWAJ <- left_join(London2014_TCWA,Agg_Journey2014,by="joinvalue",all.x=TRUE)
            
            #echos
            traffic_MajR$lat_3 <- round(traffic_MajR$S.Ref.Latitude, digits=2)
            traffic_MajR$long_3 <- round(traffic_MajR$S.Ref.Longitude, digits=2)
            
            traffic_MajR$longlat <- paste(traffic_MajR$long_3, traffic_MajR$lat_3)
            
            traffic_MajR$lat_3 <- round(traffic_MajR$S.Ref.Latitude, digits=3)
            traffic_MajR$long_3 <- round(traffic_MajR$S.Ref.Longitude, digits=3)
            
            traffic_MajR$longlat <- paste(traffic_MajR$long_3, traffic_MajR$lat_3)
            
            
            index <- (((Journey2013$startLong >= (-0.50335) & Journey2013$startLong <= 0.24767) |
                      (Journey2013$endLong >= (-0.50335) & Journey2013$endLong <= 0.24767)) &
                      ((Journey2013$startLat >= (51.32) & Journey2013$startLat <= 51.68) |
                      (Journey2013$endLat >= (51.32) & Journey2013$endLat <= 51.68)))
            
            table(index)
            JourneyTime_2013 <- Journey2013[index,]
            rm(Journey2013)
            
            index <- (((Journey2012$startLong >= (-0.50335) & Journey2012$startLong <= 0.24767) |
                         (Journey2012$endLong >= (-0.50335) & Journey2012$endLong <= 0.24767)) &
                        ((Journey2012$startLat >= (51.32) & Journey2012$startLat <= 51.68) |
                           (Journey2012$endLat >= (51.32) & Journey2012$endLat <= 51.68)))
            JourneyTime_2012 <- Journey2012[index,]
            
            
            Journey2012 <- left_join(JourneyTime_2012, Link_Coordinates_v2, by="LinkRef")
            Journey2013 <- left_join(JourneyTime_2013, Link_Coordinates_v2, by="LinkRef")
            Journey2014 <- left_join(JourneyTime_2014, Link_Coordinates_v2, by="LinkRef")
            
            
            #TCW
            TCW$longlat <- paste(round(TCW$S.Ref.Longitude,digits=1),
                                 round(TCW$S.Ref.Latitude,digits=1))
  
            TCW$joinvalue <- paste(TCW$longlat, TCW$dCount, TCW$Hour)
            head(TCW)
            
            #Accidents
            index <- ((Accidents$Longitude >= (-0.50335) & Accidents$Longitude <= 0.24767) &
                        (Accidents$Latitude >= (51.32) & Accidents$Latitude <= 51.68))
            table(index)
            Accidents <- Accidents[index,]
            
            
            Agg_Accidents$Hour <- as.integer(Agg_Accidents$Hour)
            Agg_Accidents$joinvalue <- paste(Agg_Accidents$longlat, 
                                            Agg_Accidents$dCount,
                                            Agg_Accidents$Hour)
            
            head(Agg_Accidents)
            Agg_Accidents <- as.data.frame(Agg_Accidents)
            
            head(TCW)
            TCWA <- left_join(TCW, Agg_Accidents, by="joinvalue", all.x=TRUE)
            TCWA <- TCWA[,c(1:57,61:64)]
            
            #journey
            head(JourneyTime)
            
            JourneyTime$dCount <- substr(x=JourneyTime$Date, start=1,stop=10) #get the date
            
            JourneyTime$midLong <- round((JourneyTime$startLong+
                                            JourneyTime$endLong)/2, digits=1)
            JourneyTime$midLat <- round((JourneyTime$startLat+
                                           JourneyTime$endLat)/2, digits=1)
            
            JourneyTime$joinvalue <- paste(JourneyTime$midLong,
                                           JourneyTime$midLat,
                                           JourneyTime$dCount,
                                           JourneyTime$Hour)
            
            colnames(TCWA)
            head(TCWA)
            
            TCWA_Weather_Ref <- TCWA[,c(1,4:15)]
            head(TCWA_Weather_Ref)
            
            Agg_journeyweather <- left_join(Agg_Journey, TCWA_Weather_Ref, by="joinvalue")
            
            
            head(Agg_journeyweather)
            
            TCWAJ <- merge(TCWA, Agg_journeyweather, by="joinvalue", all=TRUE)
            
            colnames(TCWAJ)
            TCWAJ <- TCWAJ[,c(1,16:63)]
            head(TCWAJ)
            
            TCWAJ <- TCWAJ[!duplicated(TCWAJ),]
            
            index <- Agg_Journey$joinvalue %in% TCWA$joinvalue
            table(index)
            
            #for accidents and casualties, replace NAs with 0
            colnames(TCWAJ)
            
            TCWAJ_v2 <- TCWAJ
            TCWAJ_v2[is.na(TCWAJ_v2$Total_Vehicles),"Total_Vehicles"] = 0
            TCWAJ_v2[is.na(TCWAJ_v2$Total_Casualties),"Total_Casualties"] = 0
            TCWAJ_v2[is.na(TCWAJ_v2$Count_Accidents),"Count_Accidents"] = 0
            
            TCWAJ_v2$summary.x %>% replace_na(TCWAJ_v2$summary.y)
            
           is.na(TCWAJ_v2$summary.y)=="FALSE") {TCWAJ_v2$summary.y } else if 
            (is.na(TCWAJ_v2$summary.x)=="FALSE") {TCWAJ_v2$summary.x} else {NA}
            
            TCWAJ_v2[is.na(TCWAJ_v2$summary.x),"summary.x"] = if (is.na(TCWAJ_v2$summary.y)=="FALSE") {TCWAJ_v2$summary.y } else {NA}
            TCWAJ_v2[is.na(TCWAJ_v2$icon.x),"icon.x"] = if (is.na(TCWAJ_v2$icon.y)=="FALSE") {TCWAJ_v2$icon.y} else {NA}
            TCWAJ_v2[is.na(TCWAJ_v2$precipType.x),"precipType.x"] = if (is.na(TCWAJ_v2$precipType.y)=="FALSE") {TCWAJ_v2$precipType.y} else {NA}
            TCWAJ_v2[is.na(TCWAJ_v2$temperature.x),"temperature.x"] = if (is.na(TCWAJ_v2$temperature.y)=="FALSE") {TCWAJ_v2$temperature.y} else {NA}
            TCWAJ_v2[is.na(TCWAJ_v2$dewPoint.x),"dewPoint.x"] = if (is.na(TCWAJ_v2$dewPoint.y)=="FALSE") {TCWAJ_v2$dewPoint.y} else {NA}
            TCWAJ_v2[is.na(TCWAJ_v2$humidity.x),"humidity.x"] = if (is.na(TCWAJ_v2$humidity.y)=="FALSE") {TCWAJ_v2$humidity.y} else {NA}
            TCWAJ_v2[is.na(TCWAJ_v2$pressure.x),"pressure.x"] = if (is.na(TCWAJ_v2$pressure.y)=="FALSE") {TCWAJ_v2$pressure.y} else {NA}
            TCWAJ_v2[is.na(TCWAJ_v2$windSpeed.x),"windSpeed.x"] = if (is.na(TCWAJ_v2$windSpeed.y)=="FALSE") {TCWAJ_v2$windSpeed.y} else {NA}
            TCWAJ_v2[is.na(TCWAJ_v2$windBearing.x),"windBearing.x"] = if (is.na(TCWAJ_v2$windBearing.x)=="FALSE") {TCWAJ_v2$windBearing.x} else {NA}
            TCWAJ_v2[is.na(TCWAJ_v2$visibility.x),"visibility.x"] = if (is.na(TCWAJ_v2$visibility.y)=="FALSE") {TCWAJ_v2$visibility.y} else {NA}
            TCWAJ_v2[is.na(TCWAJ_v2$cloudCover.x),"cloudCover.x"] = if (is.na(TCWAJ_v2$cloudCover.y)=="FALSE") {TCWAJ_v2$cloudCover.y} else {NA}
          
            dfx=TCWAJ_v2$summary.x
            dfy=TCWAJ_v2$summary.y
            
            dfx=TCWAJ_v2$icon.x
            dfy=TCWAJ_v2$icon.y
            
            dfx=TCWAJ_v2$precipType.x
            dfy=TCWAJ_v2$precipType.y
            
            check_NA <- function (dfx,dfy) {
             if (is.na(dfx)=="FALSE") 
                return({dfx}) 
              else if 
                (is.na(dfy)=="FALSE") 
                return({dfy}) 
              else 
                return({NA})
            }
            
              for (i in 1:length(TCWAJ_v2$summary.x)) {
                dfx = check_NA(dfx, dfy)
              }