library(dplyr)
library(ggplot2)
library(tidyverse)
getwd()
setwd('C:/Users/sshaj/OneDrive/Documents/R dataset')
getwd()

#read the data 
df<- read.csv('wardata.csv')
View(df)

#Column names in the data 
colnames(df)

#head 
Head_df <- head(df,10)
View(Head_df)

#summary
summary(df)

#checking for null values 

sum(is.na(df))
na_counts <- colSums(is.na(df))
na_counts

#mutate the gender and replace the emplty strings with NA 
df <- df %>%
  mutate(
    gender = na_if(gender, ""),
    took_part_in_the_hostilities = na_if(took_part_in_the_hostilities, ""),
    ammunition = na_if(ammunition, ""),
    place_of_residence=na_if(place_of_residence,""),
    place_of_residence_district=na_if(place_of_residence_district,""),
    killed_by=na_if(killed_by,""),
    type_of_injury=na_if(type_of_injury,"")
  )

# mutate age columns with average mean(age has 129 missing values)
age_mean <- mean(df$age, na.rm = TRUE)
df <- df %>%
  mutate(age = ifelse(is.na(age), age_mean, age))
# mutate missing values of gender,type of injury,ammunition,tookpart in hostilities with unknown
df <- df %>%
  mutate(gender = ifelse(is.na(gender), 'Unknown ', gender),
         type_of_injury= ifelse(is.na(type_of_injury),'unknown',type_of_injury),
         ammunition= ifelse(is.na(ammunition),'unknown',ammunition),
         took_part_in_the_hostilities= ifelse(is.na(took_part_in_the_hostilities),'unknown',took_part_in_the_hostilities))

#summary of how people injured in war 
sort(table(df$type_of_injury), decreasing = TRUE)
#grouping data by each tables 
#type of injury

Injurytype <- as.data.frame(sort(table(df$type_of_injury), decreasing = TRUE))
colnames(Injurytype) <- c('injury_type', 'total')

#Gender of the dead
gender <- as.data.frame(sort(table(df$gender), decreasing = TRUE))
names(gender) <- c('sex', 'total')

# Participated in conflicts 
conflicted_person <- as.data.frame(sort(table(df$took_part_in_the_hostilities), decreasing = TRUE))
colnames(conflicted_person) <- c('conflict_participation', 'total')

#CIty of the residence
cityResidence <- as.data.frame(sort(table(df$place_of_residence), decreasing = TRUE))
colnames(cityResidence) <- c('city', 'total')

#ammunition

ammunition <- as.data.frame(sort(table(df$ammunition), decreasing = TRUE))
colnames(ammunition) <- c('ammunition', 'total')

#Killed 
killedBy <- as.data.frame(sort(table(df$killed_by), decreasing = TRUE))
colnames(killedBy) <- c('killed_by', 'total')

#visualisation
##Total deaths by sex
ggplot(gender, aes(x = sex, y = total, fill = sex)) +
  geom_col() +
  labs(x = "Sex", y = "Total deaths", title = "Total deaths by sex") +
  theme_minimal() +
  geom_text(aes(label = total))

# Total deaths by injury type
ggplot(Injurytype, aes(x = injury_type, y = total, fill = injury_type)) +
  geom_col() +
  scale_fill_discrete() +
  labs(x = "Types", y = "Total deaths", title = "total deaths by injury type") +
  theme_minimal() +
  geom_text(aes(label = total),vjust = -0.5, size = 4) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
#conflict participation
ggplot(conflicted_person, aes(x = conflict_participation, y = total, fill = conflict_participation)) +
  geom_col() +
  scale_fill_discrete() +
  labs(x = "'side of conflict", y = "Total", title = "Total deaths by the side of conflict") +
  theme_minimal() +
  geom_text(aes(label = total),vjust = 2, size = 4)
# total deaths as per cities 
top20_city <- cityResidence %>%
  top_n(20, total)
# EDA on total deaths over top 20 city 
ggplot(top20_city, 
       aes(x = city, y = total, fill = city)) +
  geom_col() +
  scale_fill_discrete() +
  labs(x = "death counts as per city ", y = "Total deaths", title = "City of residence") +
  theme_minimal() +
  geom_text(aes(label = total),vjust = 0.5, hjust = 1.25, size = 3, angle = 90) 
  
#ammunition used to kill
ggplot(ammunition, aes(x = ammunition, y = total, fill = ammunition)) +
  geom_col(width=1) +
  scale_fill_discrete() +
  labs(x = "ammunition type", y = "Total deaths", title = "Ammunition used to kill") +
  theme_minimal() +
  geom_text(aes(label = total),vjust = 0.5, hjust = -0.2, size = 3, angle = 90) + 
  guides(x = 'none')
# grops of killer
ggplot(killedBy, aes(x = killed_by, y = total, fill = killed_by)) +
  geom_col() +
  scale_fill_discrete() +
  labs(x = "Killer group", y = "Total deaths", title = "Groups of killerr") +
  theme_minimal() +
  geom_text(aes(label = scales::percent_format()
                (total/sum(total))),vjust = -1, size = 2) +
  scale_y_continuous(labels = scales::percent_format())

