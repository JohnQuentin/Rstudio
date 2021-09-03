##################################################################
# Te gebruiken libaries
##################################################################

#core Packages
library(tidyverse)
library(mice)
library(psych)
library(dplyr)
library(readr)
library(readxl)
library(weathermetrics)

#Machinelearning
library(caret)
library(rpart)

#Visualisatie
library(ggplot2)
library(Amelia)
library(DataExplorer) #let op: data.table 1.13.4 nodig!
library(rpart.plot())

##################################################################
# 1.Importeren van datasets
##################################################################

#Inladen van CSV bestand crimes in Boston
crime <- read_csv("tmpp17ut6kv.csv") #bewust niet direct als factors vanwegen de complexiteit van de dataset

#[incident_num] 	Internal BPD report number
#[offense_code]Numerical code of offense description
#[Offense_Code_Group_Description] Internal categorization of [offense_description]
#[Offense_Description]	Primary descriptor of incident
#[district] What district the crime was reported in
#[reporting_area] RA number associated with the where the crime was reported from.
#[shooting][char] Indicated a shooting took place.
#[occurred_on] Earliest date and time the incident could have taken place
#[UCR_Part] Universal Crime Reporting Part number (1,2, 3)
#[street] Street name the incident took place

#Inladen van Excel bestand weerstomstandigheden in Boston
weather <- read_excel("2390225Los.xlsx")

#WT11 - High or damaging winds
#WT01 - Fog, ice fog, or freezing fog (may include heavy fog)
#TMAX - Maximum temperature
#WT03 - Thunder
#TMIN - Minimum temperature
#WT04 - Ice pellets, sleet, snow pellets, or small hail"
#TOBS - Temperature at the time of observation
#WT06 - Glaze or rime
#WDMV - Total wind movement 

##################################################################
# 2. Bekijken, opschonen en samenvoegen van datasets
##################################################################

#*******************************
# 2.1. Bekijken
#*******************************

#Overzicht krijgen van de datasets
glimpse(crime)
summary(crime)
describe(crime)

glimpse(weather)
summary(weather)
describe(weather)

#*******************************
# 2.2. Opschonen
#*******************************

#-------------------------------
# 2.2.1 Crime
#-------------------------------

#Controleren op missende waardes
missmap(crime)
md.pattern(crime, rotate.names = TRUE)
plot_missing(crime)

#Hernoemen van districten voor meer duidelijkheid. 
crime$DISTRICT <- recode(crime$DISTRICT,
  A1 = 'Downtown',
  A15= 'Charlestown',
  A7= 'East Boston',
  B2= 'Roxbury',
  B3= 'Mattapan',
  C6= 'South Boston',
  C11= 'Dorchester',
  D4= 'South End',
  D14= 'Brighton',
  E5= 'West Roxbury',
  E13= 'Jamaica Plain',
  E18= 'Hyde Park')

#-------------------------------
# Dataset Shooting 
#-------------------------------

#Dataset Shooting aanmaken en verwijderen van de kolommen welke ik niet ga gebruiken
crime_SH <- select(crime, -c(UCR_PART, OFFENSE_CODE, REPORTING_AREA, Long, Lat, INCIDENT_NUMBER, OFFENSE_CODE,
                                    OFFENSE_DESCRIPTION, Location, OFFENSE_CODE_GROUP))

#Vervangen van alle Y met 1
crime_SH$SHOOTING[crime_SH$SHOOTING=="1"]<-"Y"
crime_SH$SHOOTING[crime_SH$SHOOTING=="0"]<-"N"

#Verwijderen van overgebleven NA's
complete.cases(crime_SH)
crime_SH<-crime_SH[complete.cases(crime_SH),]

#-------------------------------
# Dataset OFFENSE_CODE_GROUP
#-------------------------------

#Dataset aanmaken en verwijderen van de kolommen welke ik niet ga gebruiken
crime_group <- select(crime, -c(INCIDENT_NUMBER, OFFENSE_CODE, OFFENSE_DESCRIPTION, REPORTING_AREA, SHOOTING, 
                                UCR_PART, Lat, Long, Location))

#Verwijderen van overgebleven NA's
complete.cases(crime_group)
crime_group<-crime_group[complete.cases(crime_group),]

#Top 3 bepalen om de dataset te verkleinen
top5_crime <- crime_group %>%
  group_by(OFFENSE_CODE_GROUP) %>%
  summarise(count = n()) %>%
  top_n(n = 5, wt = count)

#Filteren op top 3 crime activiteiten
crime_top5 <- crime_group %>% filter(OFFENSE_CODE_GROUP %in% top5_crime$OFFENSE_CODE_GROUP)

#-------------------------------
# 2.2.2 Weather
#-------------------------------

#Controleren op missende waardes
missmap(weather)
md.pattern(weather, rotate.names = TRUE)
plot_missing(weather)

#NA's veranderen in 0
weather[,11:15][is.na(weather[,11:15])] = 0

#Verwijderen van kolommen die ik niet ga gebruiken
weather <- select(weather, -c(STATION, NAME, LATITUDE, LONGITUDE, ELEVATION, TMAX, TMIN, WDMV))

#Hernoemen van kolommen
weather <- weather %>% 
  rename(
    High.d.wind = WT11,
    Fog.ice = WT01,
    Thunder = WT03,
    Ice.pellets = WT04,
    Temp.obs = TOBS,
    Glaze.rime = WT06
  )

#Omrekenen van Fahrenheit naar Celsius
weather$Temp.obs <- fahrenheit.to.celsius(weather$Temp.obs, round = 0)

#*******************************
# 2.3. Samenvoegen van datasets
#      en toewijzen van datasets
#*******************************

#Datums type gelijk maken of ontrekken zodat het gekoppeld kan worden
crime_top5$DATE <- as.Date(crime_top5$OCCURRED_ON_DATE)
crime_SH$DATE <- as.Date(crime_SH$OCCURRED_ON_DATE)
weather$DATE <- as.Date(weather$DATE)


#-------------------------------
# 2.3.1 Dataset Shooting
#       Decision tree
#       met en zonder weather
#-------------------------------

#Dataset shooting DT zonder weather
D_Sho_DT <- crime_SH

#Dataset shooting DT met weather
D_Sho_W_DT <- merge(x = crime_SH, y = weather , by = "DATE", all.x = TRUE)

#Verwijderen ongewenste kolommen
D_Sho_DT <- select(D_Sho_DT, -c(OCCURRED_ON_DATE, DATE))
D_Sho_W_DT <- select(D_Sho_W_DT, -c(OCCURRED_ON_DATE, DATE))

#-------------------------------
# 2.3.2 Dataset Shooting
#       Decision tree 
#       Randomforest
#       met en zonder weather
#-------------------------------

#Dataset shooting DTR zonder weather
D_Sho_DTR <- crime_SH

#Dataset shooting DTR met weather
D_Sho_W_DTR <- merge(x = crime_SH, y = weather , by = "DATE", all.x = TRUE)

#Verwijderen ongeweste kolommen
D_Sho_DTR <- select(D_Sho_DTR, -c(OCCURRED_ON_DATE, DATE))
D_Sho_W_DTR <- select(D_Sho_W_DTR, -c(OCCURRED_ON_DATE, DATE))

#-------------------------------
# 2.3.3 Dataset Shooting
#       KNN
#       met en zonder weather
#-------------------------------

#Dataset shooting KNN zonder weather
D_Sho_KNN <- crime_SH

#Dataset shooting KNN met weather
D_Sho_W_KNN <- merge(x = crime_SH, y = weather , by = "DATE", all.x = TRUE)

#Verwijderen ongeweste kolommen
D_Sho_KNN <- select(D_Sho_KNN, -c(OCCURRED_ON_DATE, DATE))
D_Sho_W_KNN <- select(D_Sho_W_KNN, -c(OCCURRED_ON_DATE, DATE))

#-------------------------------
# 2.3.4 Dataset Shooting
#       Regression
#       met en zonder weather
#-------------------------------

#Dataset shooting regression zonder weather
D_Sho_R <- crime_SH

#Dataset shooting regression met weather
D_Sho_W_R <- merge(x = crime_SH, y = weather , by = "DATE", all.x = TRUE)

#Verwijderen ongeweste kolommen
D_Sho_R <- select(D_Sho_R, -c(OCCURRED_ON_DATE, DATE))
D_Sho_W_R <- select(D_Sho_W_R, -c(OCCURRED_ON_DATE, DATE))

#-------------------------------
# 2.3.5 Dataset top5
#       Decision tree
#       met en zonder weather
#-------------------------------

#Dataset top5 DT zonder weather
D_top5_DT <- crime_top5

#Dataset top5 DT met weather
D_top5_W_DT <- merge(x = crime_top5, y = weather , by = "DATE", all.x = TRUE)

#Verwijderen ongeweste kolommen
D_top5_DT <- select(D_top5_DT, -c(OCCURRED_ON_DATE, DATE))
D_top5_W_DT <- select(D_top5_W_DT, -c(OCCURRED_ON_DATE, DATE))

#-------------------------------
# 2.3.6 Dataset top5
#       Decision tree 
#       Randomforest
#       met en zonder weather
#-------------------------------

#Dataset top5 DT zonder weather
D_top5_DTR <- crime_top5

#Dataset top5 DT met weather
D_top5_W_DTR <- merge(x = crime_top5, y = weather , by = "DATE", all.x = TRUE)

#Verwijderen ongeweste kolommen
D_top5_DTR <- select(D_top5_DTR, -c(OCCURRED_ON_DATE, DATE))
D_top5_W_DTR <- select(D_top5_W_DTR, -c(OCCURRED_ON_DATE, DATE))

#-------------------------------
# 2.3.7 Dataset top5
#       KNN
#       met en zonder weather
#-------------------------------

#Dataset top5 KNN zonder weather
D_top5_KNN <- crime_top5

#Dataset top5 KNN met weather
D_top5_W_KNN <- merge(x = crime_top5, y = weather , by = "DATE", all.x = TRUE)

#Verwijderen ongeweste kolommen
D_top5_KNN <- select(D_top5_KNN, -c(OCCURRED_ON_DATE, DATE))
D_top5_W_KNN <- select(D_top5_W_KNN, -c(OCCURRED_ON_DATE, DATE))

#-------------------------------
# 2.3.8 Dataset top5
#       Regression
#       met en zonder weather
#-------------------------------

#Dataset top5 KNN zonder weather
D_top5_R <- crime_top5

#Dataset top5 KNN met weather
D_top5_W_R <- merge(x = crime_top5, y = weather , by = "DATE", all.x = TRUE)

#Verwijderen ongeweste kolommen
D_top5_R <- select(D_top5_R, -c(OCCURRED_ON_DATE, DATE))
D_top5_W_R <- select(D_top5_W_R, -c(OCCURRED_ON_DATE, DATE))



##################################################################
# 3. Analyseren
##################################################################

set.seed(1234)

#*******************************
# 3.1. Dataset shooting
#*******************************

#-------------------------------
# 3.1.1 Decision tree 
#       zonder weather
#-------------------------------

#Characters naar factor
D_Sho_DT <- D_Sho_DT%>% mutate_if(is.character,as.factor) 

#Controle op correlatie
hetcor(D_Sho_DT)

#Maken van een training en testing set
D_Sho_DT_inTrain<-createDataPartition(D_Sho_DT$SHOOTING,p=0.75,list=FALSE)
D_Sho_DT_training<-D_Sho_DT[D_Sho_DT_inTrain,]
D_Sho_DT_testing<-D_Sho_DT[-D_Sho_DT_inTrain,]

#Rpart model maken 
D_Sho_DT_fit<-rpart(SHOOTING ~., 
                    data=D_Sho_DT_training,
                    method="class",
                    control=rpart.control(minsplit = 5,cp=0.005))

#Plotten van het model
rpart.plot(D_Sho_DT_fit)

#Prediction testen d.m.v. een confusion Matrix
D_Sho_DT_testing$predictions<-predict(D_Sho_DT_fit,D_Sho_DT_testing,type="class")
confusionMatrix(D_Sho_DT_testing$predictions,D_Sho_DT_testing$SHOOTING)

#-------------------------------
# 3.1.2 Decision tree 
#       met weather
#-------------------------------

#Characters naar factor
D_Sho_W_DT <- D_Sho_W_DT%>% mutate_if(is.character,as.factor) 

#Controle op correlatie
hetcor(D_Sho_W_DT$SHOOTING, D_Sho_W_DT$Temp.obs)
hetcor(D_Sho_W_DT$SHOOTING, D_Sho_W_DT$Fog.ice)
hetcor(D_Sho_W_DT$SHOOTING, D_Sho_W_DT$Thunder)
hetcor(D_Sho_W_DT$SHOOTING, D_Sho_W_DT$Ice.pellets)
hetcor(D_Sho_W_DT$SHOOTING, D_Sho_W_DT$Glaze.rime)
hetcor(D_Sho_W_DT$SHOOTING, D_Sho_W_DT$High.d.wind)

#Maken van een training en testing set
D_Sho_W_DT_inTrain<-createDataPartition(D_Sho_W_DT$SHOOTING,p=0.75,list=FALSE)
D_Sho_W_DT_training<-D_Sho_W_DT[D_Sho_W_DT_inTrain,]
D_Sho_W_DT_testing<-D_Sho_W_DT[-D_Sho_W_DT_inTrain,]

#Rpart model maken 
D_Sho_W_DT_fit<-rpart(SHOOTING ~., 
                    data=D_Sho_W_DT_training,
                    method="class",
                    control=rpart.control(minsplit = 5,cp=0.005))

#Plotten van het model
rpart.plot(D_Sho_W_DT_fit)

#Prediction testen d.m.v. een confusion Matrix
D_Sho_DT_testing$predictions<-predict(D_Sho_DT_fit,D_Sho_DT_testing,type="class")
confusionMatrix(D_Sho_DT_testing$predictions,D_Sho_DT_testing$SHOOTING)

#-------------------------------
# 3.1.3 Decision tree 
#       Randomforest
#       zonder weather
#-------------------------------

#-------------------------------
# 3.1.4 Decision tree
#       Randomforest
#       met weather
#-------------------------------

#-------------------------------
# 3.1.5 KNN
#       zonder weather
#-------------------------------

#-------------------------------
# 3.1.6 KNN
#       met weather
#-------------------------------

#-------------------------------
# 3.1.7 Regression
#       zonder weather
#-------------------------------

#-------------------------------
# 3.1.8 Regression
#       met weather
#-------------------------------

#*******************************
# 3.2. Dataset top5
#*******************************

#-------------------------------
# 3.2.1 Decision tree 
#       zonder weather
#-------------------------------

#-------------------------------
# 3.2.2 Decision tree 
#       met weather
#-------------------------------

#-------------------------------
# 3.2.3 Decision tree 
#       Randomforest
#       zonder weather
#-------------------------------

#-------------------------------
# 3.2.4 Decision tree
#       Randomforest
#       met weather
#-------------------------------

#-------------------------------
# 3.2.5 KNN
#       zonder weather
#-------------------------------

#-------------------------------
# 3.2.6 KNN
#       met weather
#-------------------------------

#-------------------------------
# 3.2.7 Regression
#       zonder weather
#-------------------------------

#-------------------------------
# 3.2.8 Regression
#       met weather
#-------------------------------
