##### Load libraries #####

library(tidyverse)
library(magrittr)

##### Load the Happiness datasets from last time #####

happy2017 <- read_csv("world-happiness-report/2017.csv")
glimpse(happy2017)

happy2016 <- read_csv("world-happiness-report/2016.csv")
glimpse(happy2016)

happy2015 <- read_csv("world-happiness-report/2015.csv")
glimpse(happy2015)

##### Review: ggplot ######
# Plot a histogram of happiness scores in 2017

ggplot(happy2017, aes(`Happiness.Score`)) +
  geom_histogram(binwidth = 0.5, color = "dark blue", fill = "light blue")

# Plot Happiness Score of All Countries in 2015

ggplot(happy2015) +
  geom_col(aes(Country, `Happiness Score`, fill = Region)) +
  coord_flip()

ggplot(happy2015) +
  geom_point(aes(Country, `Happiness Score`, color = Region, shape = Region)) +
  scale_shape_manual(values = c(1:10)) +
  coord_flip()

# Save Plot

plot <- ggplot(happy2015) +
  geom_point(aes(Country, `Happiness Score`, color = Region, shape = Region)) +
  scale_shape_manual(values = c(1:10)) +
  coord_flip()

ggsave("happy_plot.png", plot, width = 15, height = 25)

rm(plot)

##### Combine Datasets with dplyr #####

### First, remove and rename variables to match across data sets ###

# Take a look at datasets to see what to remove
# You can use glimpse() or names()

names(happy2017)
names(happy2016)
names(happy2015)

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

happy2016 %<>%
  select(-c(`Lower Confidence Interval`, `Upper Confidence Interval`))

happy2015 %<>%
  select(-`Standard Error`)

# Notice that 2017 does not have Region
# Is there a way to add Region to each Country in 2017 easily?

# First, check `Region` in 2015 and 2016 to make sure they match

?distinct()

happy2015 %>%
  distinct(Region)

happy2016 %>%
  distinct(Region)

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

temp2 <- inner_join(happy2015, happy2016, 
                    by = c("Country", "Region"), suffix = c(".2015", ".2016"))

temp2 %>%
  filter(!complete.cases(.)) # No missing values

# Now, add 2017 to the mix
# Hint: You'll run into a problem with suffixes

# Work around the suffix problem by renaming
names(happy2017)[-1] <- paste0(names(happy2017)[-1], ".2017")
names(happy2017)

# The below combines 2017 to our temporary holder of 2015/2016 data
inner_join(temp, happy2017, by = c("Country")) %>%
  names()

# You could've actually done all that without needing the intermediary temp holder
# Instead, use a nested join

rm(temp, temp2) # remove temp holders since you really don't need them

happy <- inner_join(happy2015, happy2016, by = c("Country", "Region"), suffix = c(".2015", ".2016")) %>%
  inner_join(., happy2017, by = "Country")

glimpse(happy)
View(happy)

# Great! You've successfully combined the datasets
# Turns out you didn't have to worry about "adding" Region to 2017

########################################
##### Reshaping Dataset with tidyr #####
########################################

# Let's make a dataset of happiness rank over the years for the top 10 happiest countries in 2017
# Dataset should have these columns: Country, Region, 2015, 2016, 2017
# Hint: First use filter() to get top 10 countries in 2017
# Hint: Then rename columns using select()
happy.rank <- happy %>%
  filter(`Happiness Rank.2017` <= 10) %>%
  select(c(Country, Region,
          "2015" = `Happiness Rank.2015`,
          "2016" = `Happiness Rank.2016`,
          "2017" = `Happiness Rank.2017`))

# Use gather() to make data longform
#Dataset should have these columns: Country, Region, Year, Rank
happy.rank %<>%
  gather(`2015`, `2016`, `2017`, key = "Year", value = "Rank")

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

happy.rank %>%
  ggplot(aes(Country, Rank)) +
  geom_label(aes(label = Year, color = Year, fontface = "bold"), position = "identity") +
  scale_y_reverse(breaks = c(1:10))

# Next, make dataset wide again using spread()

happy.rank %>%
  spread(Year, Rank)

# Let's try and find if anything has changed over the years for any country
# First, it may be helpful to make the entire dataset longform
# Combining everything you've learned today, try it out
# Hint: combine the use of dplyr and tidyr

  
