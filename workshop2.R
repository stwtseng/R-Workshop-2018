##### Load libraries #####

library(tidyverse)
library(magrittr)

##### Load the Happiness datasets from last time #####


###########################
##### Review: ggplot ######
###########################

# Plot a histogram of happiness scores in 2017



# Plot Happiness Score of All Countries in 2015


#######################################
##### Combine Datasets with dplyr #####
#######################################

### First, remove and rename variables to match across data sets ###

# Take a look at datasets to see what to remove
# You can use glimpse() or names()



# Let's first remove whiskers from 2017, then rename variables to match 2015 and 2016
# The following is inefficient

happy2017$Whisker.high <- NULL
happy2017$Whisker.low <- NULL
colnames(happy2017)[2] <-  "Happiness Rank"
colnames(happy2017)[3] <- "Happiness Score"
colnames(happy2017)[4] <- "Economy (GDP per Capita)"
# ...etc

# Let's reload 2017 first since we manipulated it already

happy2017 <- read_csv("world-happiness-report/2017.csv")

# The following using dplyr and the %>% function is more efficient and easier to read
# %>% is pronounced "then"

?select()
?rename()

happy2017 %>%
  select(-c(Whisker.high, Whisker.low)) %>%
  rename(`Happiness Rank` = Happiness.Rank,
         `Happiness Score` = Happiness.Score,
         `Economy (GDP per Capita)` = Economy..GDP.per.Capita.,
         `Health (Life Expectancy)` = Health..Life.Expectancy.,
         `Trust (Government Corruption)` = Trust..Government.Corruption.,
         `Dystopia Residual` = Dystopia.Residual)

# Notice that after doing all that, the dataset did not actually change

names(happy2017)

# Instead, use the %<>% function to change dataset
happy2017 %<>%
  select(-c(Whisker.high, Whisker.low)) %>%
  rename(`Happiness Rank` = Happiness.Rank,
         `Happiness Score` = Happiness.Score,
         `Economy (GDP per Capita)` = Economy..GDP.per.Capita.,
         `Health (Life Expectancy)` = Health..Life.Expectancy.,
         `Trust (Government Corruption)` = Trust..Government.Corruption.,
         `Dystopia Residual` = Dystopia.Residual)

names(happy2017)

# Now remove excess from 2016 (confidence intervals) and 2015 (standard error)



# Notice that 2017 does not have Region
# Is there a way to add Region to each Country in 2017 easily?

# First, check `Region` in 2015 and 2016 to make sure they match

?distinct()



# Great! Looks like they match. Let's combine these two data sets into a temp holder

?left_join()

temp <- left_join(happy2015, happy2016, by = c("Country", "Region"), suffix = c(".2015", ".2016"))
glimpse(temp)

# Which countries showed up in 2015 but not in 2016 or vice versa?

temp %>%
  filter(is.na(`Happiness Score.2015`) | is.na(`Happiness Score.2016`)) # Shows missing

temp %>%
  na.omit() # Omits missing

temp %>%
  filter(complete.cases(.)) # Shows complete only

# Alternatively, use inner_join to automatically remove countries that do not show up again

?inner_join()



#Check for missing values



# Now, add 2017 to the mix
# Hint: You'll run into a problem with suffixes

# Work around the suffix problem by renaming



# The below combines 2017 to our temporary holder of 2015/2016 data

inner_join(temp, happy2017, by = c("Country")) %>%
  names()

# You could've actually done all that without needing the intermediary temp holder
# Instead, use a nested join

rm(temp, temp2) # remove temp holders since you really don't need them



# Great! You've successfully combined the datasets
# Turns out you didn't have to worry about "adding" Region to 2017

########################################
##### Reshaping Dataset with tidyr #####
########################################

# Let's make a dataset of happiness rank over the years for the top 10 happiest countries in 2017
# Dataset should have these columns: Country, Region, 2015, 2016, 2017
# Hint: First use filter() to get top 10 countries in 2017

#happy.rank <- happy %>%
  #filter() %>%
  #select(c(, ,
           #"2015" = `Happiness Rank.2015`,
           #"2016" = `Happiness Rank.2016`,
           #"2017" = `Happiness Rank.2017`))

# Use gather() to make data longform
#Dataset should have these columns: Country, Region, Year, Rank

# happy.rank %<>%
#   gather(, , , key = "", value = "")

# Arrange dataset by country instead of year

happy.rank %>%
  arrange(Country)

# Plot change in happiness rank for each country over the years
# Hint: Use a group argument for the plot aesthetics

happy.rank %>%
  ggplot(aes(Year, Rank, group = Country)) +
  geom_point() + geom_line() +
  scale_y_reverse(breaks = c(1:10)) +
  facet_wrap(~Country)

# Plot happiness rank of each country using geom_label() to indicate year



# Next, make dataset wide again using spread()



# Calculate average happiness by region

happy %<>%
  group_by(Region) %>%
  mutate(r.avg15 = mean(`Happiness Score.2015`)) %>%
  mutate(r.avg16 = mean(`Happiness Score.2016`)) %>%
  mutate(r.avg17 = mean(`Happiness Score.2017`)) %>%
  ungroup()

happy %>%
  select(Region, r.avg15, r.avg16, r.avg17)

# Create clean table of average happiness by region



# Make the dataset longform



# Plot average happiness of regions over the years



# Let's go back to the full dataset
# Visualize the relation between freedom and happiness across different economies in 2017



# Let's try and find if anything has changed over the years for any country
# First, it may be helpful to make the entire dataset longform
# Combining everything you've learned today, try it out
# Hint: combine the use of dplyr and tidyr


