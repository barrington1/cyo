library(ggplot2)
library(lubridate)
library(zoo)
library(dplyr)
library(knitr)
library(plotly)
library(tidyverse)

# Read csv in R
## dd=read.csv("http://www1.maths.leeds.ac.uk/~charles/math5741/crime.csv",header=T)  
#pdx = read.csv("C:/Users/bobby/Documents/Capstone/crime/crime.csv",header = T)
pdx = read.csv("https://cyo.arringtonadventures.com/crime/crime.csv",header = T, stringsAsFactors = FALSE) %>%
        mutate(Text_General_Code = str_to_sentence(Text_General_Code)) 
head(pdx)

# Create a variable count with value 1
pdx$Count <- 1

# Convert Date from factor to date
#pdx$Date <- mdy_hms(pdx$Dispatch_Date_Time)

# Extract year from Dispatch_Date
pdx$Year <- substring(pdx$Dispatch_Date,1,4)

# Extract Day from Dispatch_Date
pdx$Day <- wday(ymd(pdx$Dispatch_Date))

# Rename District from Dc_Dist 
colnames(pdx)[1] <- "District"

# Drop all variables we are not interested in
#select(pdx, -2,-3,-5,-7,-8,-9,-11,-12,-13,-14)

# Group Text_General_Code by categories
pdx$Category[pdx$Text_General_Code == "Thefts" | 
             pdx$Text_General_Code == "Motor vehicle theft" |
             pdx$Text_General_Code == "Theft from vehicle" | 
             pdx$Text_General_Code == "Recovered stolen motor vehicle" |
             pdx$Text_General_Code == "Embezzlement" | 
             pdx$Text_General_Code == "Forgery and counterfeiting" |
             pdx$Text_General_Code == "Receiving stolen property"] <- "Thefts"
pdx$Category[pdx$Text_General_Code == "Vandalism/criminal mischief" | 
             pdx$Text_General_Code == "Fraud"] <- "Criminal Damage"
pdx$Category[pdx$Text_General_Code == "Narcotic / drug law violations"] <- "Narcotics"
pdx$Category[pdx$Text_General_Code == "Other Assaults" | 
             pdx$Text_General_Code == "Aggravated assault firearm"|
             pdx$Text_General_Code == "Aggravated assault no firearm"] <- "Assault"
pdx$Category[pdx$Text_General_Code == "Burglary non-residential" | 
             pdx$Text_General_Code == "Burglary residential"] <- "Burglary"
pdx$Category[pdx$Text_General_Code == "Robbery no firearm" |
             pdx$Text_General_Code == "Robbery Firearm"]  <- "Robbery"
pdx$Category[pdx$Text_General_Code == "Arson" | 
              pdx$Text_General_Code == "Gambling violations" |
              pdx$Text_General_Code == "Liquor law violations" | 
              pdx$Text_General_Code == "Offenses against family and children" |
              pdx$Text_General_Code == "Public drunkenness" | 
              pdx$Text_General_Code == "Vagrancy/loitering" |
              pdx$Text_General_Code == "Disorderly conduct" | 
              pdx$Text_General_Code == "Prostitution and commercialized vice" | 
              pdx$Text_General_Code == "Other sex offenses (not commercialized)" | 
              pdx$Text_General_Code == "Rape" |
              pdx$Text_General_Code == "Weapon violations"| 
              pdx$Text_General_Code == "Homicide - criminal" | 
              pdx$Text_General_Code == "All other offenses" |
              pdx$Text_General_Code == "Homicide - gross negligence" | 
              pdx$Text_General_Code == "Homicide - justifiable" |
              pdx$Text_General_Code == "Disorderly conduct" | 
              pdx$Text_General_Code == "Offenses against family and children" |
              pdx$Text_General_Code == "Other assaults" | 
              pdx$Text_General_Code == "Driving under the influence" |
              pdx$Text_General_Code == "Public Drunkenness" | 
              pdx$Text_General_Code == "Homicide - Criminal" |
              pdx$Text_General_Code == "Burglary non-residential"] <- "Others"


## we clean the dataset of missing values and remove all values from 2016 - this last year is not complete

# Remove NAs
#pdx <- pdx[complete.cases(pdx),]
# Remove 2016 rows
pdx <- pdx[!pdx$Year > 2016,]

pdx <- pdx[, -c(2,3,5,7,8,9,11,12,13,14)] 
# Finally, we show the the dataset ready for exploration.

# Show first 5 records
head(pdx)


## Data exploration
#To answer this question we plot the number of crimes per year from 2006 to 2016.
#The graph shows that crime in the city of Philadelphia has been decreasing year after  
#year, with a continuous decline.

dd_aggr <- aggregate(Count ~ Year, data = pdx, FUN = sum)

# Plot the graph 
ggplot(dd_aggr, aes(x=Year, y= Count, group = 1)) + 
  geom_line(colour = "steelblue") + 
  geom_point(colour = "steelblue") + 
  theme_minimal() + 
  theme(axis.title.x=element_blank()) + 
  theme(axis.title.y=element_blank()) +
  ggtitle("Crimes evolution 2006-2016")


## Figure 2 depicts the annual frequency of crimes per type and their trend. 
## The most common types of crime are Theft and Batery. All types have been    
#falling to a greater or lesser extent.

# Create aggregated object

dd_aggr2 <- aggregate(Count ~ Category + Year, data = pdx, FUN = sum)

# Plot the graph

ggplot(data=dd_aggr2, aes(x=Year, y=Count, group = Category, colour = Category)) +
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  theme(axis.title.x=element_blank()) + 
  theme(axis.title.y=element_blank()) + 
  theme(legend.title=element_blank()) +
  ggtitle("Figures 2. Crimes evolution per type of crime 2006-2016")

## What time of day do most crime occur?
## The following bar graph (Figure 3) shows the number of crimes increases gradually 
## from 06:00 in the morning (the hour with less crimes) until 16:00 in the afternoon 
## (the hour with the most crimes). The hours of 23:00 and 01:00 are exceptionally high,   
## 11:00 to 13:00 are at a similar level as 18:00 to 23:00

ggplot(pdx, aes(x=Hour)) +
  geom_bar(stat="Count", width=0.8, fill = "steelblue") + 
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) + 
  labs(x = "Hour", y = "Number of crimes") + 
  theme_minimal() + 
  theme(axis.title.x=element_blank()) + 
  theme(axis.title.y=element_blank()) +
  ggtitle("Figures 3. Crimes per hour")

## The heat-map in Figure 4 shows the distribution of number of crimes per hour and type. 
## For example, we can see that the peak hours of Theft and Others are at 00:00, 09:00 and 12:00. 
## Narcotics concentrate between 10:00 to 14:00 and 19:00 to 22:00. Other types are more evenly 
## distributed throughout the day.

# Create aggregated object

dd_aggr3 <- aggregate(Count ~ Category + Hour, data = pdx, FUN = sum)

# Plot graph

p1 <- ggplot(data = dd_aggr3, aes(x = Hour, y = Category)) + 
  geom_tile(aes(fill = Count), color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") 

p1 + theme_minimal() + 
  theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 6),
        axis.text.y = element_text(size= 8),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  ggtitle("Figures 4. Type of crime vs hour")


## what day of week do most crime occur?
ggplot(pdx, aes(x=Day)) +
  geom_bar(stat="Count", width=0.8, fill = "steelblue") + 
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) + 
  labs(x = "Day", y = "Number of crimes") + 
  theme_minimal() + 
  theme(axis.title.x=element_blank()) + 
  theme(axis.title.y=element_blank()) +
  ggtitle("Figures 5. Crimes per day")

##  The heat-map show distrbution of crimes per day and type
dd_aggr5 <- aggregate(Count ~ Category + Day, data = pdx, FUN = sum)

# Plot graph

p1 <- ggplot(data = dd_aggr5, aes(x = Day, y = Category)) + 
  geom_tile(aes(fill = Count), color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") 

p1 + theme_minimal() + 
  theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 6),
        axis.text.y = element_text(size= 8),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  ggtitle("Figures 6. Type of crime vs day")


### Which districts are more potentially dangerous?

# We visualise the number of crimes per districts. The most dangerous district seems
# number 15, with more than 150000 records in the 10 years, while district 92 with
# 5000 seems the safest.

pdx$District<- as.factor(pdx$District)
dd_sub <- subset(pdx, District!="21" & District!="31")

# Create aggregated object
dd_aggr6 <- aggregate(Count ~ District, data = dd_sub, FUN = sum)

# Order values
dd_aggr6$District <- factor(dd_aggr6$District, levels = dd_aggr6$District[order(-dd_aggr6$Count)])

# Plot the graph 
ggplot(dd_aggr6, aes(x=District, y = Count)) + 
  theme_minimal() + 
  geom_bar(stat="identity", width=0.7, fill = "steelblue") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = "District", y = "Number of crimes") + 
  theme(axis.title.x=element_blank()) + 
  theme(axis.title.y=element_blank()) +
  ggtitle("Figures 7. Crimes per district")


# There are some interesting findings in the relations between the type of crime and districts
# where they occurred. For example we can see that districts 2,6,9,15 are particularly,
# dangerous in terms of Theft, that 2,14 and 15 stand out in terms of Criminal Damage,
# Narcotics seems to be in 24 and 25.

dd_aggr7 <- aggregate(Count ~ Category + District, data = dd_sub, FUN = sum)

# Plot the graph
p3<-ggplot(data = dd_aggr7, aes(x = District, y = Category)) +
  geom_tile(aes(fill = Count), color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue")  
p3+ theme_minimal()+ theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 6),
        axis.text.y = element_text(size= 8),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  ggtitle("Figures 8. Type of crimes vs district")


