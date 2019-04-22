

# Install the following packages

install.packages("ggplot2")
install.packages("lubridate")
install.packages("ggrepel")
install.packages("ggmap")
install.packages("scales")
install.packages("dplyr")
install.packages("RCurl")
install.packages("RJSONIO")
install.packages("stringi")
install.packages("purrr")
install.packages("tidyr")
install.packages("sos")
install.packages("leaflet")
install.packages("extrafont")
install.packages("htmltools")
install.packages("webshot")
install_phantomjs()
devtools::install_github("dkahle/ggmap")
devtools::install_github("rstudio/leaflet")

#  Import relevent libraries

library(ggplot2)
library(ggmap)
library(ggrepel)
library(knitr)
library(lubridate)
library(dplyr)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(rgdal)
library(scales)
library(RCurl)
library(RJSONIO)
library(stringi)
library(purrr)
library(tidyr)
library(caTools)
library(ElemStatLearn)
library(e1071)
library(leaflet)
library(mapview)
library(webshot)
library(htmltools)
library(webshot)
library(extrafont)

getwd()
setwd("C:/Users/rober/Documents")
getwd()

# Uploading the data into R studio

accidnew <- read.csv(file.choose())
accidnew

## Exploring the data ##

# View first 6 lines of the data

head(accidnew)

# View last 6 lines of the data

tail(accidnew)

# view structure of data

str(accidnew)

# View summary of data

summary(accidnew)

View(accidnew) 
glimpse(accidnew)

# Locating missing data

complete.cases(accidnew)
accid2[!complete.cases(accidnew), ]


# Remove duplicate rows and check number of rows
accidnew %>% distinct() %>% nrow()

# Drop the unwanted columns 

accid2 <- select (accidnew,-c("ID", "DateType", "ResidenceCity", "ResidenceState", "LocationifOther",
                  "InjuryPlace", "InjuryCity", "InjuryCounty", "InjuryState", "OtherSignifican",
                  "InjuryCityGeo"))


glimpse(accid2)

# Breaking the date into three different coloumns for year, month and day
# Converting the date format to Y/M/D

realdate <- as.Date(accid2$Date, format="%m/%d/%Y")

year = as.numeric(format(realdate, "%Y"))

accid2$Year <- year

month = as.numeric(format(realdate, "%m"))

accid2$month <- month

day = as.numeric(format(realdate, "%d"))

accid2$day <- day



# Exploring our data frames

accid2$Date
accid2$Age
accid2$Sex
accid2$Race
accid2$DeathCounty
accid2$Location
accid2$DescriptionofInjury
accid2$InjuryPlace
accid2$Year
accid2$month
accid2$day

str(accid2$Date)
str(accid2$Age)
accid2$Sex
accid2$Race
list(accid2$DeathCounty)
str(accid2$Location)
str(accid2$DescriptionofInjury)
accid2$InjuryPlace
glimpse(accid2$Year)
accid2$month
accid2$day



# Find the all population who is less than age 45

accid2$Age < 45
filter <- accid2$Age < 45
filter
accid2[filter, ]

# Find the all white population 

accidwhite <- accid2[accid2$Race=="White", ]
accidwhite

# Find the all black population

accidblack <- accid2[accid2$Race=="Black", ]
accidblack

# Find the all Hispanic population

accidhispanic <- accid2[accid2$Race=="Hispanic, White", ]
accidhispanic

# Find the all Asian and other population

accidasian <- accid2[accid2$Race=="Asian, Other", ]
accidasian

# Check the summary the race dataframes

summary(accidasian)
summary(accidblack)
summary(accidhispanic)
summary(accidwhite)




# Find all male population

accidmale <- accid2[accid2$Sex=="Male", ]
accidmale

# Find all female population

accidfemale <- accid2[accid2$Sex=="Female", ]
accidfemale

# Check summary for male anf female population

summary(accidmale)
summary(accidfemale)



# Find the all population who is over 65 years old

accid2[accid2$Age > 65, ]

# Find the all population who are over 65 years old and females only

accid2[accid2$Age > 65 & accid2$Sex=="Female" , ]


# Find the all population who are over 65 years old, females and white only 

accid2[accid2$Age > 65 & accid2$Sex=="Female" & accid2$Race=="White" , ]

# Find the all population who are over 70 years old, males and black only

accid2[accid2$Age > 70 & accid2$Sex=="Male" & accid2$Race=="Black", ]



# Check the row counts
nrow(accid2)
nrow(accid2[accid2$Age > 65 & accid2$Sex=="Female" & accid2$Race=="White" , ])
nrow(accid2[accid2$Age > 65 & accid2$Sex=="Female" , ])
nrow(accid2[accid2$Age > 70 & accid2$Sex=="Male" & accid2$Race=="Black", ])
nrow(accid2[accid2$Age > 65, ])



# Check the summaries
# By Sex
summary(accidmale)
summary(accidfemale)


#By Race
summary(accidwhite)
summary(accidasian)
summary(accidblack)
summary(accidhispanic)


# Data visuzalization for Death County for years 2012-2018
# Labeling x and y axes

h <- ggplot(data = accid2, aes(x=Age)) + 
  geom_histogram(binwidth = 1, aes(fill= Race), colour= "Black") + 
  ylab("Count")

h + xlab("Age of Death Person") + 
  ylab("Number of Accidents")

# Label formatiing

h + xlab("Age of Death Person") + 
  ylab("Number of Accidents") + 
  theme(axis.title.x = element_text(colour= "DarkGreen", size=25),
        axis.title.y = element_text(colour= "Red", size= 25),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size = 15))

# Legend formatiing


h + xlab("Age of Death Person") + 
  ylab("Number of Accidents") + 
  theme(axis.title.x = element_text(colour= "DarkGreen", size=25),
        axis.title.y = element_text(colour= "Red", size= 25),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size = 15),  
        legend.title= element_text(size = 15), 
        legend.position = c(1,1),
        legend.justification = c(1,1))

# Adding title


h + xlab("Age of Death Person") + 
  ylab("Number of Accidents") + 
  ggtitle("Accidental Drug Related Deaths in Connecticut 2012-2018 ") +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x = element_text(colour= "DarkGreen", size=15),
        axis.title.y = element_text(colour= "Red", size= 15),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size = 15),  
        legend.title= element_text(size = 15), 
        legend.position = c(1,1),
        legend.justification = c(1,1),
        plot.title = element_text(colour = "Blue",
                                  size = 15,
                                  family = "Times New Roman"))

# Filtering just by sex 
# Male

hmale <- ggplot(data = accidmale, aes(x=Age)) + 
  geom_histogram(binwidth = 1, aes(fill= Race), colour= "Black") + 
  ylab("Count")

hmale + xlab("Age of Death Person") + 
  ylab("Number of Accidents") + 
  ggtitle("Males in All Races") +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x = element_text(colour= "DarkGreen", size=20),
        axis.title.y = element_text(colour= "Red", size= 20),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size = 15),  
        legend.title= element_text(size = 15), 
        legend.position = c(1,1),
        legend.justification = c(1,1),
        plot.title = element_text(colour = "Blue",
                                  size = 20,
                                  family = "Times New Roman"))

# Female


hfemale <- ggplot(data = accidfemale, aes(x=Age)) + 
  geom_histogram(binwidth = 1, aes(fill= Race), colour= "Black") + 
  ylab("Count")

hfemale + xlab("Age of Death Person") + 
  ylab("Number of Accidents") + 
  ggtitle("Females in All Races") +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x = element_text(colour= "DarkGreen", size=20),
        axis.title.y = element_text(colour= "Red", size= 20),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size = 15),  
        legend.title= element_text(size = 15), 
        legend.position = c(1,1),
        legend.justification = c(1,1),
        plot.title = element_text(colour = "Blue",
                                  size = 20,
                                  family = "Corbel")) 


# Visualization with facets

ggplot(data = accid2, aes(x=Age)) + 
  geom_histogram(binwidth = 1, aes(fill= Race), colour= "Black") + 
  ylab("Count") + 
  facet_grid(Race~., scales = "free" )

ggplot(data = accid2, aes(x=Age)) + 
  geom_histogram(binwidth = 1, aes(fill= Race), colour= "Black") + 
  ylab("Count") + 
  facet_grid(.~Race, scales = "free")

# Zoom in and out 

t <- ggplot(data = accid2, aes(x=Age, y= Race, 
                              size= Age,
                              colour= Sex))
t + geom_point() +
  xlab("Age of Death Person") + 
  ylab("Race") + 
  ggtitle("Accidental Drug Related Deaths in Connecticut 2012-2018 ") +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x = element_text(colour= "DarkGreen", size=25),
        axis.title.y = element_text(colour= "Red", size= 25),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size = 15),  
        legend.title= element_text(size = 15), 
        legend.position = c(1,1),
        legend.justification = c(1,1),
        plot.title = element_text(colour = "Blue",
                                  size = 35,
                                  family = "Times New Roman"))

ggplot(data = accid2, aes(x=Age)) + 
  xlab("Age of Death Person") + 
  ylab("Race") + 
  ggtitle("Accidental Drug Related Deaths in Connecticut 2012-2018 ") +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x = element_text(colour= "DarkGreen", size=15),
        axis.title.y = element_text(colour= "Red", size= 15),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size = 10),  
        legend.title= element_text(size = 10), 
        legend.position = c(1,1),
        legend.justification = c(1,1),
        plot.title = element_text(colour = "Blue",
                                  size = 15,
                                  family = "Times New Roman")) +
  geom_histogram(binwidth = 1, aes(fill= Race), colour= "Black") + 
  ylab("Count") + 
  xlim(20,60)


# Year and Sex Correalation 


ggplot(data = accid2, aes(x=Year)) + 
  xlab("Year") + 
  ylab("Count of Accidents") + 
  ggtitle("Accidental Drug Related Deaths in Connecticut 2012-2018 ") +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x = element_text(colour= "DarkGreen", size=15),
        axis.title.y = element_text(colour= "Red", size= 15),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size = 15),  
        legend.title= element_text(size = 15), 
        legend.position = c(1,0),
        legend.justification = c(1,0),
        plot.title = element_text(colour = "Blue",
                                  size = 15,
                                  family = "Times New Roman")) +
  geom_histogram(binwidth = 0.5, aes(fill= Sex), colour= "Black") 


# Statistical Trnsformation- visualization with boxplot 

u <- ggplot(data = accid2, aes(x= Race, y= Age, 
                              colour= Sex, size= 10))



u  + geom_jitter(size=0.2) + geom_boxplot(size=1, alpha= 0.5) +
  ylab("Age of Death Person") + 
  xlab("Race") + 
  ggtitle("Accidental Drug Related Deaths in Connecticut 2012-2018 ") +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x = element_text(colour= "DarkGreen", size=15),
        axis.title.y = element_text(colour= "Red", size= 15),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 15),  
        legend.title= element_text(size = 15), 
        legend.position = c(1,1),
        legend.justification = c(1,1),
        plot.title = element_text(colour = "Blue",
                                  size = 10,
                                  family = "Text New Roman")) 



v <- ggplot(data=accid2, aes(x= Age))

v + geom_histogram(binwidth = 10, aes(fill= Race),colour="Black") + 
  xlab("Age of Death Person") + 
  ylab("Count of Accidents") + 
  ggtitle("Accidental Deaths Related to Age") +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x = element_text(colour= "DarkGreen", size=25),
        axis.title.y = element_text(colour= "Red", size= 25),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size = 15),  
        legend.title= element_text(size = 15), 
        legend.position = c(1,1),
        legend.justification = c(1,1),
        plot.title = element_text(colour = "Blue",
                                  size = 35,
                                  family = "Times New Roman"))


# Year/Cause of Death correalation visualization
# Data visuzalization for Death County for years 2012-2018

DeathCountyRace <- ggplot(data = accid2, aes(x=DeathCounty, y= Race,
                                            colour= Sex, size= Year)) +
  geom_point() +
  xlab("County of Death") 

DeathCountyRace + geom_col(aes(size=0.0001)) +
  geom_point() +
  xlab("County of Death") + 
  ylab("Race") + 
  ggtitle("Race/Death County Correlation") +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x = element_text(colour= "DarkGreen", size=15),
        axis.title.y = element_text(colour= "Red", size= 15),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 15),  
        legend.title= element_text(size = 15), 
        legend.position = c(1,1),
        legend.justification = c(1,1),
        plot.title = element_text(colour = "Blue",
                                  size = 15,
                                  family = "Times New Roman"))



accident <- tbl_df(accid2)
accident

glimpse(accid2)


# Find how many death we have in each year (2012-2018)


filter(accident, Year==2012 )
filter(accident, Year==2013 )
filter(accident, Year==2014 )
filter(accident, Year==2015 )
filter(accident, Year==2016 )
filter(accident, Year==2017 )
filter(accident, Year==2018 )


filter(accident, Year==2012, month==1 )
filter(accident, Year==2012, month==2 )
filter(accident, Year==2012, month==3 )
filter(accident, Year==2012, month==4 )
filter(accident, Year==2012, month==5 )
filter(accident, Year==2012, month==6 )
filter(accident, Year==2012, month==7 )
filter(accident, Year==2012, month==8 )
filter(accident, Year==2012, month==9 )
filter(accident, Year==2012, month==10 )
filter(accident, Year==2012, month==11)
filter(accident, Year==2012, month==12)

filter(accident, Year==2013, month==1 )
filter(accident, Year==2013, month==2 )
filter(accident, Year==2013, month==3 )
filter(accident, Year==2013, month==4 )
filter(accident, Year==2013, month==5 )
filter(accident, Year==2013, month==6 )
filter(accident, Year==2013, month==7 )
filter(accident, Year==2013, month==8 )
filter(accident, Year==2013, month==9 )
filter(accident, Year==2013, month==10 )
filter(accident, Year==2013, month==11)
filter(accident, Year==2013, month==12)

filter(accident, Year==2014, month==1 )
filter(accident, Year==2014, month==2 )
filter(accident, Year==2014, month==3 )
filter(accident, Year==2014, month==4 )
filter(accident, Year==2014, month==5 )
filter(accident, Year==2014, month==6 )
filter(accident, Year==2014, month==7 )
filter(accident, Year==2014, month==8 )
filter(accident, Year==2014, month==9 )
filter(accident, Year==2014, month==10 )
filter(accident, Year==2014, month==11)
filter(accident, Year==2014, month==12)

filter(accident, Year==2015, month==1 )
filter(accident, Year==2015, month==2 )
filter(accident, Year==2015, month==3 )
filter(accident, Year==2015, month==4 )
filter(accident, Year==2015, month==5 )
filter(accident, Year==2015, month==6 )
filter(accident, Year==2015, month==7 )
filter(accident, Year==2015, month==8 )
filter(accident, Year==2015, month==9 )
filter(accident, Year==2015, month==10 )
filter(accident, Year==2015, month==11)
filter(accident, Year==2015, month==12)

filter(accident, Year==2016, month==1 )
filter(accident, Year==2016, month==2 )
filter(accident, Year==2016, month==3 )
filter(accident, Year==2016, month==4 )
filter(accident, Year==2016, month==5 )
filter(accident, Year==2016, month==6 )
filter(accident, Year==2016, month==7 )
filter(accident, Year==2016, month==8 )
filter(accident, Year==2016, month==9 )
filter(accident, Year==2016, month==10 )
filter(accident, Year==2016, month==11)
filter(accident, Year==2016, month==12)

filter(accident, Year==2017, month==1 )
filter(accident, Year==2017, month==2 )
filter(accident, Year==2017, month==3 )
filter(accident, Year==2017, month==4 )
filter(accident, Year==2017, month==5 )
filter(accident, Year==2017, month==6 )
filter(accident, Year==2017, month==7 )
filter(accident, Year==2017, month==8 )
filter(accident, Year==2017, month==9 )
filter(accident, Year==2017, month==10 )
filter(accident, Year==2017, month==11)
filter(accident, Year==2017, month==12)

filter(accident, Year==2018, month==1 )
filter(accident, Year==2018, month==2 )
filter(accident, Year==2018, month==3 )
filter(accident, Year==2018, month==4 )
filter(accident, Year==2018, month==5 )
filter(accident, Year==2018, month==6 )
filter(accident, Year==2018, month==7 )
filter(accident, Year==2018, month==8 )
filter(accident, Year==2018, month==9 )
filter(accident, Year==2018, month==10 )
filter(accident, Year==2018, month==11)
filter(accident, Year==2018, month==12)



#-----------------------------

# Create Age bin 16-35, 36-50, 50 and up

accid2$Age_grp <- accid2$Age

accid2$Age_grp <- ifelse((accid2$Age >= 16 & accid2$Age < 35), '16-35', accid2$Age_grp)
accid2$Age_grp <- ifelse((accid2$Age >= 35 & accid2$Age < 50), '35-50', accid2$Age_grp)
accid2$Age_grp <- ifelse((accid2$Age >= 50 ), '50+', accid2$Age_grp)

accid2$Age_grp

# Filtering by Age_group

Age_grp_first <- filter( accid2, accid2$Age_grp=="16-35")
Age_grp_second <- filter( accid2, accid2$Age_grp=="35-50")
Age_grp_third <- filter( accid2, accid2$Age_grp=="50+")

Age_grp_first
Age_grp_second
Age_grp_third

#---------------------------------------------------------


list(accid$Race)

# Filtering by Race, Age_group and Sex_group
# for Entire Male population

Age_grp_WhiteMalefirst <- filter(accid2, Race == "White", Age_grp == "16-35", Sex == "Male")
Age_grp_WhiteMalesecond <- filter(accid2, Race == "White", Age_grp == "35-50", Sex == "Male")
Age_grp_WhiteMalethird <- filter(accid2, Race == "White", Age_grp == "50+", Sex == "Male")

Age_grp_BlackMalefirst <- filter(accid2, Race == "Black", Age_grp == "16-35", Sex == "Male")
Age_grp_BlackMalesecond <- filter(accid2, Race == "Black", Age_grp == "35-50", Sex == "Male")
Age_grp_BlackMalethird <- filter(accid2, Race == "Black", Age_grp == "50+", Sex == "Male")

Age_grp_HispanicMalefirst <- filter(accid2, Race == "Hispanic, White", Age_grp == "16-35", Sex == "Male")
Age_grp_HispanicMalesecond <- filter(accid2, Race == "Hispanic, White", Age_grp == "35-50", Sex == "Male")
Age_grp_HispanicMalethird <- filter(accid2, Race == "Hispanic, White", Age_grp == "50+", Sex == "Male")

Age_grp_HispanicMalefirst1 <- filter(accid2, Race == "Hispanic, Black", Age_grp == "16-35", Sex == "Male")
Age_grp_HispanicMalesecond1 <- filter(accid2, Race == "Hispanic, Black", Age_grp == "35-50", Sex == "Male")
Age_grp_HispanicMalethird1 <- filter(accid2, Race == "Hispanic, Black", Age_grp == "50+", Sex == "Male")

Age_grp_AsianMalefirst <- filter(accid2, Race == "Asian, Other", Age_grp == "16-35", Sex == "Male")
Age_grp_AsianMalesecond <- filter(accid2, Race == "Asian, Other", Age_grp == "35-50", Sex == "Male")
Age_grp_AsianMalethird <- filter(accid2, Race == "Asian, Other", Age_grp == "50+", Sex == "Male")

# For Entire Female population

Age_grp_WhiteFemalefirst <- filter(accid2, Race == "White", Age_grp == "16-35", Sex == "Female")
Age_grp_WhiteFemalesecond <- filter(accid2, Race == "White", Age_grp == "35-50", Sex == "Female")
Age_grp_WhiteFemalethird <- filter(accid2, Race == "White", Age_grp == "50+", Sex == "Female")

Age_grp_BlackFemalefirst <- filter(accid2, Race == "Black", Age_grp == "16-35", Sex == "Female")
Age_grp_BlackFemalesecond <- filter(accid2, Race == "Black", Age_grp == "35-50", Sex == "Female")
Age_grp_BlackFemalethird <- filter(accid2, Race == "Black", Age_grp == "50+", Sex == "Female")

Age_grp_HispanicFemalefirst <- filter(accid2, Race == "Hispanic, White", Age_grp == "16-35", Sex == "Female")
Age_grp_HispanicFemalesecond <- filter(accid2, Race == "Hispanic, White", Age_grp == "35-50", Sex == "Female")
Age_grp_HispanicFemalethird <- filter(accid2, Race == "Hispanic, White", Age_grp == "50+", Sex == "Female")

Age_grp_HispanicFemalefirst1 <- filter(accid2, Race == "Hispanic, Black", Age_grp == "16-35", Sex == "Female")
Age_grp_HispanicFemalesecond1 <- filter(accid2, Race == "Hispanic, Black", Age_grp == "35-50", Sex == "Female")
Age_grp_HispanicFemalethird1 <- filter(accid2, Race == "Hispanic, Black", Age_grp == "50+", Sex == "Female")

Age_grp_AsianFemalefirst <- filter(accid2, Race == "Asian, Other", Age_grp == "16-35", Sex == "Female")
Age_grp_AsianFemalesecond <- filter(accid2, Race == "Asian, Other", Age_grp == "35-50", Sex == "Female")
Age_grp_AsianFemalethird <- filter(accid2, Race == "Asian, Other", Age_grp == "50+", Sex == "Female")

# Filtering by Sex

accid2$Sex_grp <- accid2$Sex

Sex_grp_Male <- filter(accid2, accid2$Sex_grp=="Male")
Sex_grp_Female <- filter(accid2, accid2$Sex_grp=="Female")

Sex_grp_Male
Sex_grp_Female

# Filtering by Race_group

accid2$Race_grp <- accid2$Race

Race_grp_White <- filter(accid2, accid2$Race_grp=="White")
Race_grp_Black <- filter(accid2, accid2$Race_grp=="Black")
Race_grp_HispanicWhite  <- filter(accid2, accid2$Race_grp=="Hispanic, White")
Race_grp_AsianOther <- filter(accid2, accid2$Race_grp=="Asian, Other")
Race_grp_AsianIndian <- filter(accid2, accid2$Race_grp=="Asian Indian")
Race_grp_HispanicBlack  <- filter(accid2, accid2$Race_grp=="Hispanic, Black")
Race_grp_Hawaiian <- filter(accid2, accid2$Race_grp=="Hawaiian")
Race_grp_Unknown <- filter(accid2, accid2$Race_grp=="Unknown")

Race_grp_White
Race_grp_Black
Race_grp_HispanicWhite
Race_grp_AsianOther
Race_grp_AsianIndian
Race_grp_HispanicBlack
Race_grp_Hawaiian
Race_grp_Unknown

# Filtering Race and Age by age groups 

RaceAge_grp_whitefirst <- filter(accid2, accid2$Race_grp=="White", accid2$Age_grp=="16-35")
RaceAge_grp_whitesecond <- filter(accid2, accid2$Race_grp=="White", accid2$Age_grp=="35-50")
RaceAge_grp_whitethird <- filter(accid2, accid2$Race_grp=="White", accid2$Age_grp=="50+")

RaceAge_grp_whitefirst
RaceAge_grp_whitesecond
RaceAge_grp_whitethird

RaceAge_grp_blackfirst <- filter(accid2, accid2$Race_grp=="Black", accid2$Age_grp=="16-35")
RaceAge_grp_blacksecond <- filter(accid2, accid2$Race_grp=="Black", accid2$Age_grp=="35-50")
RaceAge_grp_blackthird <- filter(accid2, accid2$Race_grp=="Black", accid2$Age_grp=="50+")

RaceAge_grp_blackfirst
RaceAge_grp_blacksecond
RaceAge_grp_blackthird

RaceAge_grp_HispanicWhitefirst <- filter(accid2, accid2$Race_grp=="Hispanic, White", accid2$Age_grp=="16-35")
RaceAge_grp_HispanicWhitesecond <- filter(accid2, accid2$Race_grp=="Hispanic, White", accid2$Age_grp=="35-50")
RaceAge_grp_HispanicWhitethird <- filter(accid2, accid2$Race_grp=="Hispanic, White", accid2$Age_grp=="50+")

RaceAge_grp_HispanicWhitefirst
RaceAge_grp_HispanicWhitesecond 
RaceAge_grp_HispanicWhitethird 

accid2$Race_grp <- accid2$Race

# Filtering by Location of Death

accid2$DeathCounty_grp <- accid2$DeathCounty

DeathCounty_grp_HARTFORD <- filter(accid2, accid2$DeathCounty_grp=="HARTFORD")
DeathCounty_grp_FAIRFIELD <- filter(accid2, accid2$DeathCounty_grp=="FAIRFIELD")
DeathCounty_grp_TOLLAND <- filter(accid2, accid2$DeathCounty_grp=="TOLLAND")
DeathCounty_grp_NEWHAVEN <- filter(accid2, accid2$DeathCounty_grp=="NEW HAVEN")
DeathCounty_grp_WINDHAM <- filter(accid2, accid2$DeathCounty_grp=="WINDHAM")
DeathCounty_grp_MIDDLESEX <- filter(accid2, accid2$DeathCounty_grp=="MIDDLESEX")
DeathCounty_grp_NEWLONDON <- filter(accid2, accid2$DeathCounty_grp=="NEW LONDON")
DeathCounty_grp_LITCHFIELD <- filter(accid2, accid2$DeathCounty_grp=="LITCHFIELD")






#----------------------------------------------
# Uploading the Race and Age percentage csv file for calculating Chi sqr.

# File "chi sqr Race and Age group"


# Chi sqr test for Race and Age group

chisq.test(accid2$Race, accid2$Age_grp)
chisq.test(accid2$Race, accid2$Age_grp)$expected

# We have a high chi-squared value and a p-value of less than 0.05 significance level.
# So we reject the null 
# hypothesis and conclude that carb and cyl have a significant relationship.

# Chi sqr test for Sex and Age group

chisq.test(accid2$Sex, accid2$Age_grp)
chisq.test(accid2$Sex, accid2$Age_grp)$expected

# Chi sqr test for Sex and DeathCounty

chisq.test(accid2$Sex_grp, accid2$DeathCounty)
chisq.test(accid2$Sex_grp, accid2$DeathCounty)$expected

# Chi sqr test for Race Group and DeathCounty

chisq.test(accid2$Race_grp, accid2$DeathCounty)
chisq.test(accid2$Race_grp, accid2$DeathCounty)$expected

# We have a high chi-squared value and a p-value of less than 0.05 significance level.
# So we reject the null 
# hypothesis and conclude that carb and cyl have a significant relationship.


#---------------------------------------------------------------
# Find all death population from cause of cocaine toxicity

accid2$COD <- as.character(accid2$COD)

list(accid2$COD)

grep("Cocaine",accid2$COD)

accid_cocaine <- accid2[grep("Cocaine",accid2$COD),]

accid_cocaine

str(accid_cocaine)

str(accid2$COD)

#  For Opiate toxicity

grep("Opiate",accid2$COD)

accid_Opiate <- accid2[grep("Opiate",accid2$COD),]

accid_Opiate

str(accid_Opiate)

#  For Heroin toxicity

grep("Heroin",accid2$COD)

accid_Heroin <- accid2[grep("Heroin",accid2$COD),]

accid_Heroin

str(accid_Heroin)

#  For Alcohol toxicity

grep("Alcohol",accid2$COD)

accid_Alcohol <- accid2[grep("Alcohol",accid2$COD),]

accid_Alcohol

str(accid_Alcohol)

accid_Acute <- accid2[grep("Acute Fentanyl Intoxication",accid2$COD),]

accid_Acute

str(accid_Acute)


#------------------------------------------------------------------------------------




# Convert to normal data frame to see all the columns

data.frame(head(accident))
accident[accident$Age>65 & accident$Sex=="Male",  ]

# Dplyer approach to do the same thing

filter(accident, Age==35, Race=="White" )
filter(accident, Age<25, Race=="Black", Sex=="Female")

# with OR condition

filter(accident, DescriptionofInjury=="Ingestion" | Race=="Hispanic")

filter(accident, DescriptionofInjury=="Ingestion" | Race=="Hispanic" | Age==50)

# Using %in% operator

filter(accident,DescriptionofInjury %in% c("Drug Use"))

filter(accident,DescriptionofInjury %in% c("Drug Use", "Ingestion", "Multiple Drug Toxicity"))

filter(accident,DescriptionofInjury %in% c("Drug Use", "Ingestion", "Multiple Drug Toxicity", "COD"))

#######################################




#--  Using tally function

accident %>%
  group_by("Date", "Location") %>%
  summarise(DescriptionofInjury = n()) %>%
  tally(sort = TRUE)

##-------------------------------------------------------------------------------------------
#  Groupping  and selecting 

accident %>%
  group_by(Location) %>%
  select(Race) %>%
  table()%>%
  head()

accident %>%
  group_by(Age) %>%
  select(Race) %>%
  table()%>%
  head()

accident %>%
  group_by(Sex) %>%
  select(Race) %>%
  table()%>%
  head()

accident %>%
  group_by(COD) %>%
  select(Race) %>%
  table()%>%
  head()



accident %>%
  group_by(Date) %>%
  tally()%>%
  mutate(change = n - lag(n))

# Applying dplyer to make an structure of an object instead of str(accident)

glimpse(accident)


accident %>%
  select(Race, Date) %>%
  arrange(desc(Date))


accident %>%
  select(Date, Sex, Race, Location) %>%
  arrange(desc(Date))




# Filtering data using which() for non missing data

accident[accident$DeathCounty=="LITCHFIELD", ]

accident[accident$DeathCounty=="FAIRFIELD", ]

# Trying to filter the colomn only in DeathCountywith with LITCHFIELD

which(accident$DeathCounty=="LITCHFIELD")
accident[which(accident$DeathCounty=="LITCHFIELD"),]

accident[accident$Age<= 16,]

accident[which(accident$Age<= 16),]

# Filtering data using is.na() for missing data

head(accident, 100)

is.na(accident$Age)
accident[is.na(accident$Age), ]

accident[is.na(accident$Date), ]
accident[is.na(accident$Sex), ]
accident[is.na(accident$Race), ]
accident[is.na(accident$DeathCounty), ]
accident[is.na(accident$Location), ]
accident[is.na(accident$DescriptionofInjury), ]
accident[is.na(accident$InjuryPlace), ]

list(accident$DeathCounty)



## Encoding categorical data
# Encoding sex_grp to numeric variables

accid2$Sex_grp <- factor(accid2$Sex_grp, 
                        levels = c("Male", "Female"),
                        labels = c(1,2))
str(accid2$Sex_grp)

# Creating linear regression formula to calculate the Coefficients and R-squared and p value

lin_reg = lm(formula = Age ~ Sex_grp , 
             data = accid2)

summary(lin_reg)

# Call:
#   lm(formula = Age ~ Sex_grp, data = accid2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -28.53887 -10.53887   0.24569   9.46113  45.24569 
# 
# Coefficients:
#   Estimate Std. Error   t value Pr(>|t|)    
# (Intercept) 41.754307   0.200760 207.98094  < 2e-16 ***
#   Sex_grp2     0.784561   0.393795   1.99231  0.04639 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 12.3316 on 5096 degrees of freedom
# (7 observations deleted due to missingness)
# Multiple R-squared:  0.000778299,	Adjusted R-squared:  0.000582219 
# F-statistic:  3.9693 on 1 and 5096 DF,  p-value: 0.0463903


## Encoding Age_grp to numberic variables

list(accid2$Age_grp)

accid2$Age_grp <- factor(accid2$Age_grp, 
                        levels = c("16-35", "35-50", "50+"),
                        labels = c(1,2,3))

# Linear regression formula for Year ~ Sex_grp correlation

lin_reg_grp = lm(formula = Year ~ Sex_grp , 
                 data = accid2)


summary(lin_reg_grp)

# Call:
#   lm(formula = Year ~ Sex_grp, data = accid2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -3.716596 -1.551698  0.283404  1.448302  2.448302 
# 
# Coefficients:
#   Estimate   Std. Error     t value   Pr(>|t|)    
# (Intercept) 2015.7165960    0.0302378 66662.23997 < 2.22e-16 ***
#   Sex_grp2      -0.1648979    0.0593060    -2.78046  0.0054482 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.8571 on 5095 degrees of freedom
# (8 observations deleted due to missingness)
# Multiple R-squared:  0.00151506,	Adjusted R-squared:  0.00131909 
# F-statistic: 7.73094 on 1 and 5095 DF,  p-value: 0.00544819



# Create a map where we can visualize the accidental drug related death by county and city

# Extracting the values inDeathCityGeo try to separate the numbers in parentheses as latitudes and longitudes
# using str_exctract and regex function

accid2$geo <- c(str_extract(accid2$DeathCityGeo, regex("(?<=\\().+?(?=\\))")))

p <- strsplit(accid2$geo,",")
accid2 <- accid2 %>% separate(geo, c("lat", "long"),sep = ",")

options(digits=9)
accid2$lat <- as.numeric(accid2$lat)
accid2$long <- as.numeric(accid2$long)


str(accid2)
p


Connecticut <- leaflet(data = accid2) %>% 
  addTiles() %>%
  addProviderTiles(providers$Thunderforest.SpinalMap) %>%
  addMarkers(~long, ~lat,label = ~ htmlEscape(accid2$DeathCityGeo))

Connecticut


######################################



accid$label <- paste("<p>", accid$DeathCityGeo, "</p>" )
accid$label


# Working with new data frame drug_use visualize in a map the population of accidents from drug use

# Creating new data frame by filtering description of injury from drug_use


drug_use <- filter(accid2, DescriptionofInjury == "Drug Use")

drug_use <- mutate(drug_use, Drug_use = paste(DescriptionofInjury))

Con_drug <- leaflet(data = drug_use) %>% 
  addTiles() %>%
  addCircleMarkers(lng = drug_use$long,
                   lat = drug_use$lat,
                   color = "green",
                   weight = 1,
                   radius = 10)
Con_drug

# Creating new data frame by filtering description of injury from Ingestion

ingestion <- filter(accid2, DescriptionofInjury == "Ingestion")

ingestion <- mutate(ingestion, Ingestion = paste(DescriptionofInjury))

Con_ingestion <- leaflet(data = ingestion) %>% 
  addTiles() %>%
  addCircleMarkers(lng = ingestion$long,
                   lat = ingestion$lat,
                   color = "red",
                   weight = 1,
                   radius = 10)

Con_ingestion

# Creating new data frame by filtering description of injury from Substance Abuse


Substance_Abuse <- filter(accid2, DescriptionofInjury == "Substance Abuse")

Substance_Abuse <- mutate(Substance_Abuse, substance_abuse = paste(DescriptionofInjury))


Con_sub_abuse <- leaflet(data = Substance_Abuse) %>% 
  addTiles() %>%
  addCircleMarkers(lng = Substance_Abuse$long,
                   lat = Substance_Abuse$lat,
                   color = "darkblue",
                   weight = 1,
                   radius = 10)

Con_sub_abuse


#################################################################

bins <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
pal <- colorBin("RdYlBu",domain = accid2$Age, bins = bins)



# adding lables. legend using html function


bins1 <- c(15, 25, 35, 45, 55, 65, 75)
pal1 <- colorBin("RdYlBu",domain = drug_use$Age_grp, bins = bins1)

con_map_drug <- leaflet(data = drug_use) %>%
  addMarkers(~long, ~lat) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addCircles(lng = drug_use$long, lat = drug_use$lat,
             color = "#ff0000") %>%
  addLegend(pal = pal1,
            values = drug_use$Year,
            opacity = 5,
            position = "topright")


con_map_drug


labels <- paste("<p>", accid2$DeathCounty, "</p",
                "<p>", "Race" )

con_map <- leaflet(data = accid2) %>%
  setView(~long, ~lat, zoom = 2) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(data = accid2,
              weight = 1,
              smoothFactor = 0.5,
              color = "white",
              fillOpacity = 0.8,
              fillColor = pal(accid$Year),
              highlight = highlightOptions(
                weight = 5,
                fillOpacity = 0.7,
                dashArray = "",
                bringToFront = TRUE
              ),
              label = lapply(labels, HTML)) %>%
  addLegend(pal = pal,
            values = accid2$Year,
            opacity = 0.7,
            position = "topright")

con_map

glimpse(accid2)
str(accid2)




# Try to find which drugs are most common in drug_related accidents by visualizing in ggplot


d_Cocaine <- ggplot(data = accid2, aes(x=Year)) + 
  geom_histogram(binwidth = 1, aes(fill= Cocaine), colour= "Black") + 
  ylab("Count")

d_Cocaine + xlab("Year") + 
  ylab("Number of Accidents")



d_Heroin <- ggplot(data = accid2, aes(x=Year)) + 
  geom_histogram(binwidth = 1, aes(fill= Heroin), colour= "Black") + 
  ylab("Count")

d_Heroin + xlab("Year") + 
  ylab("Number of Accidents")



d_Fentanyl <- ggplot(data = accid2, aes(x=Year)) + 
  geom_histogram(binwidth = 1, aes(fill= Fentanyl), colour= "Black") + 
  ylab("Count")

d_Fentanyl + xlab("Year") + 
  ylab("Number of Accidents")



d_FentanylAnalogue <- ggplot(data = accid2, aes(x=Year)) + 
  geom_histogram(binwidth = 1, aes(fill= FentanylAnalogue), colour= "Black") + 
  ylab("Count")

d_FentanylAnalogue + xlab("Year") + 
  ylab("Number of Accidents")

####################################
dd <- ggplot(data = accid2, aes(x=Year)) + 
  geom_histogram(binwidth = 1, aes(fill= FentanylAnalogue), colour= "blue") + 
  ylab("Count") +
  geom_histogram(binwidth = 1, aes(fill= Heroin), colour= "red") +
  ylab("Count") +
  geom_histogram(binwidth = 1, aes(fill= Fentanyl), colour= "Black") +
  ylab("Count")

dd + xlab("Year") + 
  ylab("Number of Accidents")

dd <- ggplot(data = accid2) +
  geom_point(aes(x = "Yaer", y = "Cocaine", size = 10)) +
  geom_point(aes(x = "Year", y = "Fentanyl", size = 10)) +
  geom_point(aes(x = "Year", y = "FentanylAnalogue", size = 10)) +
  geom_point(aes(x = "Year", y = "Oxycodone", size = 10)) +
  geom_point(aes(x = "Year", y = "Oxymorphone", size = 10)) +
  geom_point(aes(x = "Year", y = "Ethanol", size = 10)) +
  geom_point(aes(x = "Year", y = "Hydrocodone", size = 10)) +
  geom_point(aes(x = "Year", y = "Benzodiazepine", size = 10)) +
  geom_point(aes(x = "Year", y = "Methadone", size = 10)) +
  geom_point(aes(x = "Year", y = "Amphet", size = 10)) +
  geom_point(aes(x = "Year", y = "Tramad", size = 10)) +
  geom_point(aes(x = "Year", y = "Morphine_NotHeroin", size = 10)) +
  geom_point(aes(x = "Year", y = "Other", size = 10)) +
  geom_point(aes(x = "Year", y = "AnyOpioid", size = 10)) 
  
dd + xlab("Year") + 
  ylab("Number of Accidents")



######################################################################














