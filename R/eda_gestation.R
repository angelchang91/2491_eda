#       _/_/    _/  _/      _/_/      _/   
#    _/    _/  _/  _/    _/    _/  _/_/    
#       _/    _/_/_/_/    _/_/_/    _/     
#    _/          _/          _/    _/      
# _/_/_/_/      _/    _/_/_/    _/_/_/     

# Exploratory Data Analysis of Gestation data

library(tidyverse)
library(mosaicData)
# if you don't have mosaicData, install it

data(Gestation)

# Activity 1 - Quick look at the data

# number of observations
count(Gestation)

# number of observations per racial group
count(Gestation, race)

# number of observations by racial group and level of mother's education
Gestation_n_race_ed <- count(Gestation, ed, race)


# Activity 2 - Further summary statistics
#create new Gestation df without Na values in age column


count(Gestationc)
# mean age of mothers across all births
# ensure you use a human friendly name for the value you're creating

# calculate both mothers' mean age and babies' mean weight
summarise(Gestation, meanage = mean(age, na.rm= T), meanbweight = mean(wt, na.rm= T))

# Activity 3 - Grouped summaries

# make a new data frame containing only id, age and race variables
gestationnew<- subset(Gestation, select=c(age, id, race))



# calculate the mean age by race
mean_age_by_race <- Gestation %>%
  group_by(race) %>%
  summarise(mean_age = mean(age, na.rm = TRUE))

print(mean_age_by_race)


# Activity 4 - Extensions


# Activity 4a - Correlation

# Calculate the correlation between age and weight across all births
cor(Gestation$age, Gestation$wt, use = "complete.obs")
# Calculate the correlation between age and weight for each race group


# Activity 4b - Multiple summary statistics

# Calculate the sample mean of the ages and weights of the mothers in each race group
mean_age_weight_by_race <- Gestation %>%
  group_by(race) %>%
  summarise(mean_age = mean(age, na.rm = TRUE), mean_wt=mean(wt.1, na.rm=TRUE))
print(mean_age_weight_by_race)


mean_age_weight_byrace_at<- Gestation%>% group_by(race)%>% summarise_at(
  vars(age, wt.1), .funs=list(mean=mean), na.rm=T
)

print(mean_age_weight_byrace_at)

Gestation<- mutate(Gestation, race=str_to_title(race))
# Activity 4c - Pivoting wider

# Make a wide table from the summary data frame calculated in Activity 1 that has the 
#number of observations for each combination of mother's education level and race. 
#Make each row is an education level and each column a race group.

# Hint: Look at the help file for `pivot_wider` for what to do with missing cells 
#(where there is no combination of these variables) and set the argument to be 0.

pivot_wider(Gestation_n_race_ed, names_from="race", values_from="n",values_fill=0)


# Activity 4d - Multiple summary statistics

# Calculate the mean, standard deviation, minimum, maximum and proportion of 
#values missing for the mothers' ages for each race group.
# Hint: you *can* use summarise_at() for this but you could also just summarise()



Gestation %>% group_by(race)%>% 
  summarise(mean=mean(age, na.rm=T), sd=sd(age, na.rm=T), min=min(age, na.rm=T), 
            max=max(age, na.rm=T), n=n(), na_num=sum(is.na(age)))%>%mutate(prop=na_num/n)

 

                                                                                                                               