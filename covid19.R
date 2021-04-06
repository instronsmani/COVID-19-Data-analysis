rm(list=ls()) # removes all variables stored previously
library(Hmisc) # import
library(dplyr)
library(ggplot2)

#read the data
data <- read.csv("COVID19.csv")
View(data)

#select particular data
db<-data[c(1:3,6:9,17:18)]

#summary of the data
summary(db)

#coloumns name
names(db)

#find the numeric 
sapply(db,is.numeric)

#To find the number of countries
summary(db$country)
distinct(db,country)

# cleaned up death column
db$death_dummy <- as.integer(db$death != 0)

# death rate
sum(db$death_dummy) / nrow(db)
sum(db$death_dummy)
nrow(db)

# AGE
# claim: people who die are older
dead = subset(db, death_dummy == 1)
alive = subset(db, death_dummy == 0)
View(db$death_dummy)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)

# is this statistically significant?
report<-t.test(alive$age, dead$age, alternative="two.sided", conf.level = 0.99)
View(report)
# normally, if p-value < 0.05, we reject null hypothesis
# here, p-value ~ 0, so we reject the null hypothesis and 
# conclude that this is statistically significant

# GENDER
# claim: gender has no effect
men = subset(db, gender == "male")
women = subset(db, gender == "female")
mean(men$death_dummy, na.rm = TRUE) #6.97%!
summary(men$death_dummy!=0)
mean(women$death_dummy, na.rm = TRUE) #3.7%
summary(women$death_dummy!=0)
# is this statistically significant?
t.test(men$death_dummy, women$death_dummy, alternative="two.sided", conf.level = 0.99)
# 99% confidence: men have from 0.8% to 8.8% higher chance
# of dying.
# p-value = 0.02 < 0.05, so this is statistically
# significant

#classify countries based on continent
library(countrycode)
df <- data.frame(country = c("China","France",
                             "Japan",
                             "Malaysia",
                             "Nepal",
                             "Singapore",
                             "South Korea",
                             "Taiwan",
                             "Thailand",
                             "USA",
                             "Vietnam",
                             "Australia",
                             "Canada",
                             "Cambodia",
                             "Sri Lanka",
                             "Germany",
                             "UAE",
                             "Hong Kong",
                             "Italy",
                             "Russia",
                             "UK",
                             "India",
                             "Philippines",
                             "Finland",
                             "Spain",
                             "Sweden",
                             "Belgium",
                             "Egypt",
                             "Iran",
                             "Israel",
                             "Lebanon",
                             "Kuwait",
                             "Bahrain",
                             "Austria",
                             "Afghanistan",
                             "Algeria",
                             "Croatia",
                             "Switzerland"))
db$continent<-NA
db$continent <- countrycode(sourcevar = db[, "country"],
                            origin = "country.name",
                            destination = "continent")

ff<-NA
ff<-data.frame(table(db$continent))
View(ff)

#number of cases continent wise
ggplot(ff,aes(x=Var1,y=Freq,fill=Var1))+geom_bar(stat = "identity")+ggtitle("COVID-19 REPORT CONTINENT WISE")+ylab("No.of.COVID Cases")+xlab("Continents")
distinct(db,age)
View(db)

#maximum affected age group continent wise
library("ggplot2")
library("ggsci")
# Change area fill color. JCO palette
ggplot(db, aes(continent, age)) +
  geom_boxplot(aes(fill = continent)) +
  scale_fill_jco()+
  theme_classic() +
  theme(legend.position = "right")

#boxplot
box<-ggplot(db,aes(x=continent,y=age,color=age))
box+geom_boxplot()+geom_jitter(aes(color=age),size=1.0)+ggtitle("COVID Cases Continent vs patient age")+coord_flip()+theme(legend.position = "none")

library(rworldmap)
d<-data.frame(country=db$country,value=db$age)
n<-joinCountryData2Map(d,joinCode = "NAME",nameJoinColumn = "country")
mapCountryData(n,nameColumnToPlot = "value",mapTitle = "World Map for COVID-19 patients cases during 15-Jan-2020 to 28-Feb-2020",colourPalette = "terrain")

