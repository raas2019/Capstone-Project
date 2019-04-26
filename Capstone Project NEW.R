
################## Accidental Drug-Related Deaths in Connecticut 2012-2018 ######################

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
library(stringr)


getwd()
setwd("C:/Users/rober/Documents")
getwd()

# Uploading the data into R studio

accidnew <- read.csv(file.choose())
accidnew

## Exploring the data 

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

glimpse(accid2)

# Exploring our data frame by columns

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
glimpse(accid2)


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
        legend.position = c(0,1),
        legend.justification = c(0,1),
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
        legend.position = c(0,1),
        legend.justification = c(0,1),
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


# Creating new data frame with rbl_df function 

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



#---------------------------------------------------------

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


list(accid2$Race)

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
# Creating a dataframe for sex-group

accid2$Sex_grp <- accid2$Sex

# Filtering by gender

Sex_grp_Male <- filter(accid2, accid2$Sex_grp=="Male")
Sex_grp_Female <- filter(accid2, accid2$Sex_grp=="Female")

Sex_grp_Male
Sex_grp_Female

# Breaking into two different columns as Male and Female

accid2Male <- accid2[which(accid2$Sex == "Male"), ]
accid2Female <- accid2[which(accid2$Sex == "Female"), ]


accid2$Sex_grp_male <- ifelse(accid2$Sex == "Male", "Male", accid2$Sex )
accid2$Sex_grp_female <- ifelse(accid2$Sex == "Female", "Female", accid2$Sex )


###########################################################################


# Creating new dataframe for male population including year, age, and sex_grp_male

Sex_grp_Male$Male <- ifelse(Sex_grp_Male$Sex == "Male", "Male", Sex_grp_Male$Sex )

logistic_reg_male <- Sex_grp_Male[ ,c("Year","Age", "Male") ]


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


####################################################################

# Chi_Sqr Test
# File "chi sqr Race and Age group"
# Chi sqr test for Race and Age group

chisq.test(accid2$Race, accid2$Age_grp, simulate.p.value = TRUE)
chisq.test(accid2$Race, accid2$Age_grp)$expected

#  data:  accid2$Race and accid2$Age_grp
#  X-squared = 113.41, df = NA, p-value = 0.03448


# We have a high chi-squared value and a p-value of less than 0.05 significance level.
# So we reject the null hypothesis and conclude that 
# race and age_groups are have a significant relationship.

# Chi sqr test for Sex and Age group

chisq.test(accid2$Sex, accid2$Age, simulate.p.value = TRUE)
chisq.test(accid2$Sex, accid2$Age)$expected

# data:  accid2$Sex and accid2$Age_grp
# X-squared = 12.135, df = NA, p-value = 0.09595

# We have a high chi-squared value and a p-value of more than 0.05 significance level.
# So we accept the null hypothesis and conclude that 
# sex and age_groups are have NOT a significant relationship.

# Chi sqr test for Sex and DeathCounty

chisq.test(accid2$Sex_grp, accid2$DeathCounty, simulate.p.value = TRUE)
chisq.test(accid2$Sex_grp, accid2$DeathCounty)$expected

# data:  accid2$Sex_grp and accid2$DeathCounty
# X-squared = 15.012, df = NA, p-value = 0.08646

# We have a high chi-squared value and a p-value of more than 0.05 significance level.
# So we accept the null hypothesis and conclude that 
# sex and DEATH COUNTY are have NOT a significant relationship.

# Chi sqr test for Race Group and DeathCounty

chisq.test(accid2$Race_grp, accid2$DeathCounty, simulate.p.value = TRUE)
chisq.test(accid2$Race_grp, accid2$DeathCounty)$expected

# data:  accid2$Race_grp and accid2$DeathCounty
# X-squared = 202.46, df = NA, p-value = 0.01849

# We have a high chi-squared value and a p-value of less than 0.05 significance level.
# So we reject the null hypothesis and conclude that 
# death county and race_groups are have a significant relationship.


# Chi sqr test for Race, Male and Age 

chisq.test(accidmale$Race, accidmale$Age, simulate.p.value = TRUE)
chisq.test(accidmale$Race, accidmale$Age)$expected

# Pearson's Chi-squared test with simulated p-value (based on 2000 replicates)
# 
# data:  accidmale$Race and accidmale$Age
# X-squared = 960.1, df = NA, p-value = 0.06947

# We have a high chi-squared value and a p-value of more than 0.05 significance level.
# So we accept the null hypothesis and conclude that 
# RACE and Age in male population are have NOT a significant relationship.

# Chi sqr test for Race, Female and Age 

chisq.test(accidfemale$Race, accidfemale$Age, simulate.p.value = TRUE)
chisq.test(accidfemale$Race, accidfemale$Age)$expected

# Pearson's Chi-squared test with simulated p-value (based on 2000 replicates)
# 
# data:  accidfemale$Race and accidfemale$Age
# X-squared = 461.62, df = NA, p-value = 0.5532

# We have a high chi-squared value and a p-value of more than 0.05 significance level.
# So we accept the null hypothesis and conclude that 
# RACE and Age in female population are have NOT a significant relationship.



#############################################################################

## Linear Regression Test

## Encoding categorical data
# Encoding sex_grp to numeric variables


accid2$Sex_grp <- factor(accid2$Sex_grp, 
                        levels = c("Male", "Female"),
                        labels = c(1,2))


str(accid2$Sex_grp)

# Creating linear regression formula to calculate the Coefficients and R-squared and p value
# For sex group overall

lin_reg = lm(formula = Age ~ Sex_grp , 
             data = accid2)

summary(lin_reg)

# lin_reg = lm(formula = Age ~ Sex_grp , 
#              +              data = accid2)
# > 
#   > summary(lin_reg)
# 
# Call:
#   lm(formula = Age ~ Sex_grp, data = accid2)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -28.539 -10.539   0.246   9.461  45.246 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      45.333      7.119   6.368 2.08e-10 ***
#   Sex_grpFemale    -2.794      7.127  -0.392    0.695    
# Sex_grpMale      -3.579      7.122  -0.503    0.615    
# Sex_grpUnknown   20.667     14.238   1.451    0.147    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 12.33 on 5098 degrees of freedom
# (3 observations deleted due to missingness)
# Multiple R-squared:  0.001566,	Adjusted R-squared:  0.0009782 
# F-statistic: 2.665 on 3 and 5098 DF,  p-value: 0.04623

# For Male population particularly

lin_reg_male = lm(formula = Age ~ Sex_grp_male , 
             data = accid2)

summary(lin_reg_male)

# lin_reg_male = lm(formula = Age ~ Sex_grp_male , 
#                   +              data = accid2)
# > 
#   > summary(lin_reg_male)
# 
# Call:
#   lm(formula = Age ~ Sex_grp_male, data = accid2)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -28.539 -10.539   0.246   9.461  45.246 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        45.333      7.119   6.368 2.08e-10 ***
#   Sex_grp_male2      -2.794      7.127  -0.392    0.695    
# Sex_grp_male4      20.667     14.238   1.451    0.147    
# Sex_grp_maleMale   -3.579      7.122  -0.503    0.615    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 12.33 on 5098 degrees of freedom
# (3 observations deleted due to missingness)
# Multiple R-squared:  0.001566,	Adjusted R-squared:  0.0009782 
# F-statistic: 2.665 on 3 and 5098 DF,  p-value: 0.04623

# For Female population particularly

lin_reg_female = lm(formula = Age ~ Sex_grp_female , 
             data = accid2)

summary(lin_reg_female)

# lin_reg_female = lm(formula = Age ~ Sex_grp_female , 
#                     +              data = accid2)
# > 
#   > summary(lin_reg_female)
# 
# Call:
#   lm(formula = Age ~ Sex_grp_female, data = accid2)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -28.539 -10.539   0.246   9.461  45.246 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            45.333      7.119   6.368 2.08e-10 ***
#   Sex_grp_female3        -3.579      7.122  -0.503    0.615    
# Sex_grp_female4        20.667     14.238   1.451    0.147    
# Sex_grp_femaleFemale   -2.794      7.127  -0.392    0.695    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 12.33 on 5098 degrees of freedom
# (3 observations deleted due to missingness)
# Multiple R-squared:  0.001566,	Adjusted R-squared:  0.0009782 
# F-statistic: 2.665 on 3 and 5098 DF,  p-value: 0.04623

#######################
lin_reg2 = lm(formula = Year ~ Age + Sex + Race, 
              data = accid2)

summary(lin_reg2)


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

# accid2$Age_grp <- factor(accid2$Age_grp, 
#                         levels = c("16-35", "35-50", "50+"),
#                         labels = c(1,2,3))

# Linear regression formula for Year ~ Sex_grp correlation

lin_reg_grp = lm(formula = Year ~ Sex_grp , 
                 data = accid2)


summary(lin_reg_grp)

# Call:
#   lm(formula = Year ~ Sex_grp, data = accid2)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.7166 -1.5517  0.2834  1.4483  2.4483 
# 
# Coefficients:
#   Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)    2016.4000     0.8305 2427.916   <2e-16 ***
#   Sex_grpFemale    -0.8483     0.8321   -1.020    0.308    
# Sex_grpMale      -0.6834     0.8311   -0.822    0.411    
# Sex_grpUnknown    1.6000     2.0343    0.787    0.432    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.857 on 5099 degrees of freedom
# (2 observations deleted due to missingness)
# Multiple R-squared:  0.00197,	Adjusted R-squared:  0.001382 
# F-statistic: 3.354 on 3 and 5099 DF,  p-value: 0.01811



##########################################################################################
# Create a map where we can visualize the accidental drug related death by county and city

# Extracting the values inDeathCityGeo try to separate the numbers in parentheses as latitudes and longitudes
# using str_exctract and regex function library()stringr)

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



# Working with new data frame drug_use visualize in a map the population of accidents from drug use

# Creating new data frame by filtering description of injury from drug_use

accid$label <- paste("<p>", accid$DeathCityGeo, "</p>" )
accid$label


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


# Counting some observations

glimpse(accid2)
count(accid2, (Sex))

count(accid2, (Heroin))
count(accid2, (Cocaine))
count(accid2, (Fentanyl))
count(accid2, (FentanylAnalogue))
count(accid2, (Oxycodone))
count(accid2, (Oxymorphone))
count(accid2, (Ethanol))
count(accid2, (Hydrocodone))
count(accid2, (Benzodiazepine))
count(accid2, (Methadone))
count(accid2, (Amphet))
count(accid2, (Tramad))
count(accid2, (Morphine_NotHeroin))
count(accid2, (Hydromorphone))
count(accid2, (Other))
count(accid2, (OpiateNOS))
count(accid2, (AnyOpioid))













