#       _/_/    _/  _/      _/_/      _/   
#    _/    _/  _/  _/    _/    _/  _/_/    
#       _/    _/_/_/_/    _/_/_/    _/     
#    _/          _/          _/    _/      
# _/_/_/_/      _/    _/_/_/    _/_/_/     

# Exploratory Data Analysis of FEV1 data

library(tidyverse)

# read the data in
fev1 <- read_csv("/Users/angelchang/Desktop/Data Challenge/2491_eda/data/fev1.csv", col_types = list('id' = 'f'))

# sample the data so that we have 20 patients with more than 6 observations

fev1_sampled <- fev1 %>% 
    count(id) %>%
    filter(n > 6) %>%
    slice_sample(n = 20) %>%
    select(id) %>%
    inner_join(fev1)

fev1_sampled

# Activity 5 - A simple scatter plot

# Calculate the correlation between age and FEV1
# (yes, this isn't strictly correct because there's repeated measures)


# Build a plot that shows the relationship between FEV1 and age

fev1_plot <- ggplot(data = fev1_sampled, aes(x = age, y = FEV1)) +
  geom_point(aes(color = as.factor(id), alpha=0.5)) +  # Color points by id
  geom_smooth(method = "lm", se = TRUE, color = "black") +  # Add line of best fit
  labs(title = "FEV1 against Age", x = "Age (years)", y = "FEV1 (L)")
fev1_plot

# Activity 6 - Improving the plot

# Add meaningful labels for the $x$ and $y$ axes, 
#including units, and change the plot's colour theme from the default.

# Add a smooth line of best fit to the plot. 


# Activity 7

# Activity 7a - Showing further structure

# Determine a way to highlight which observations 
#belong to the same individual in your plot


# Activity 7b - How many observations per individual?

# Count the number of times that each `id` is measured 
#and make a bar plot 

fev1_sampled%>%group_by(id)%>%summarise(n=n())%>%ggplot(aes(x=id,y=n))+geom_bar(stat="identity")
# Activity 7c - Incorporating height

# Make a plot that shows both FEV1 and age but also includes height


# Activity 7d - skimr

# Use skimr::skim() to generate a summary table of the data.
# You'll need to install skimr if you don't already have it


# Activity 7e - GGally

# Generate a pairs plot with GGally::ggpairs(), for all 
#columns except id
# You'll need to install GGally if you don't already have it

# Activity 7f - Accounting for repeat measurement

# Build a regression model to look at how FEV1 varies with 
#age, accounting for the
# structure by including a random effect mean for each id 
#and a spline curve for
# the effect of age

