##### Load libraries #####

library(tidyverse)
library(magrittr)
library(psych)
library(pastecs)
library(jtools)

##### Load the Happiness datasets from last time #####

happy2017 <- read_csv("world-happiness-report/2017.csv")

happy2016 <- read_csv("world-happiness-report/2016.csv")

happy2015 <- read_csv("world-happiness-report/2015.csv")

####################################
##### Review: Combine Datasets #####
####################################

happy2017 %<>%
  select(-c(Whisker.high, Whisker.low)) %>%
  rename(`Happiness Rank` = Happiness.Rank,
         `Happiness Score` = Happiness.Score,
         `Economy (GDP per Capita)` = Economy..GDP.per.Capita.,
         `Health (Life Expectancy)` = Health..Life.Expectancy.,
         `Trust (Government Corruption)` = Trust..Government.Corruption.,
         `Dystopia Residual` = Dystopia.Residual)

happy2016 %<>%
  select(-c(`Lower Confidence Interval`, `Upper Confidence Interval`))

happy2015 %<>%
  select(-`Standard Error`)

names(happy2017)[-1] <- paste0(names(happy2017)[-1], ".2017")

happy <- inner_join(happy2015, happy2016, by = c("Country", "Region"), suffix = c(".2015", ".2016")) %>%
  inner_join(., happy2017, by = "Country")

glimpse(happy)

#################################
##### Review: Tidy Datasets #####
#################################

happy %<>%
  gather(key, value, -Country, -Region) %>%
  separate(key, into = c("variable", "Year"), sep = "\\.") %>% # regex: separate by "."
  spread(variable, value) %>%
  mutate(Year = as.factor(Year)) %>% # gather converted some variables, so convert them back
  mutate(`Happiness Rank` = as.integer(`Happiness Rank`))
  
glimpse(happy)

#############################
##### Review: Visualize #####
#############################

# Calculate and graph average happiness for each region across years

happy %>%
  group_by(Region, Year) %>%
  select(`Happiness Score`) %>%
  dplyr::summarize(Mean = mean(`Happiness Score`)) %>% # specify dplyr package for command
  ggplot(aes(Year, Mean, fill = Region, color = Region)) +
  geom_point() + geom_line(aes(group = Region)) +
  ylab("Average Happiness")

# Graph average happiness of countries in North America across years
happy %>%
  group_by(Region, Year) %>%
  filter(Region == "Middle East and Northern Africa") %>%
  ggplot(aes(Year, `Happiness Score`, fill = Country, color = Country)) +
  geom_point() + geom_line(aes(group = Country)) +
  ylab("Average Happiness") + # Axes can be deceptive
  ylim(0,10)

##################################
##### Descriptive Statistics #####
##################################

# How many countries per region?

happy %>%
  group_by(Region) %>%
  distinct(Country) %>%
  tally()

# Descriptive stats per region?
# Other packages outside tidyverse have more useful summary functions

by(happy[,4:12], INDICES = c(happy[,2:3]), FUN = pastecs::stat.desc, basic = FALSE) # using pastecs::stat.desc

psych::describeBy(happy, group = c("Region", "Year")) # using psych::describe

########################
##### Correlations ##### 
########################

# Plot and examine the correlation b/t happiness and freedom over the years

cor(happy$Freedom, happy$`Happiness Score`)

happy %>%
  group_by(Year) %>%
  ggplot(aes(Freedom, `Happiness Score`, color = Year)) +
  geom_point() + geom_smooth(method = lm) +
  ylim(0,10)

happy %>%
  group_by(Year) %>%
  dplyr::summarize(cor(Freedom, `Happiness Score`))

# ... happiness and economy over the years

happy %>%
  group_by(Year) %>%
  ggplot(aes(`Economy (GDP per Capita)`,`Happiness Score`, color = Year)) +
  geom_point() + geom_smooth(method = lm) +
  ylim(0,10)

happy %>%
  group_by(Year) %>%
  dplyr::summarize(cor(`Economy (GDP per Capita)`, `Happiness Score`))

# ... and life expectancy

happy %>%
  group_by(Year) %>%
  ggplot(aes(`Health (Life Expectancy)`, `Happiness Score`, color = Year)) +
  geom_point() + geom_smooth(method = lm) +
  ylim(0,10)

happy %>%
  group_by(Year) %>%
  dplyr::summarize(cor(`Health (Life Expectancy)`, `Happiness Score`))

# ... and family

happy %>%
  group_by(Year) %>%
  ggplot(aes(Family, `Happiness Score`, color = Year)) +
  geom_point() + geom_smooth(method = lm) +
  ylim(0,10)

happy %>%
  group_by(Year) %>%
  dplyr::summarize(cor(Family, `Happiness Score`))

# ... and generosity

happy %>%
  group_by(Year) %>%
  ggplot(aes(Generosity, `Happiness Score`, color = Year, group = Year)) +
  geom_point() + geom_smooth(method = lm) +
  ylim(0,10)

happy %>%
  group_by(Year) %>%
  dplyr::summarize(cor(Generosity, `Happiness Score`))

### What about by region? ###

# Plot and examine the correlation b/t happiness and freedom across regions

# freedom

happy %>%
  ggplot(aes(Freedom, `Happiness Score`, color = Region)) +
  geom_point() + geom_smooth(method = lm)

happy %>%
  group_by(Region) %>%
  dplyr::summarize(cor(Freedom, `Happiness Score`))

# economy

happy %>%
  ggplot(aes(`Economy (GDP per Capita)`, `Happiness Score`, color = Region)) +
  geom_point() + geom_smooth(method = lm)

happy %>%
  group_by(Region) %>%
  dplyr::summarize(cor(`Economy (GDP per Capita)`, `Happiness Score`))

# life expectancy

happy %>%
  ggplot(aes(`Health (Life Expectancy)`, `Happiness Score`, color = Region)) +
  geom_point() + geom_smooth(method = lm) +
  ylim(0,10)

happy %>%
  group_by(Region) %>%
  dplyr::summarize(cor(`Health (Life Expectancy)`, `Happiness Score`))

# family

happy %>%
  ggplot(aes(Family, `Happiness Score`, color = Region)) +
  geom_point() + geom_smooth(method = lm)

happy %>%
  group_by(Region) %>%
  dplyr::summarize(cor(Family, `Happiness Score`))

# generosity

happy %>%
  ggplot(aes(Generosity, `Happiness Score`, color = Region)) +
  geom_point() + geom_smooth(method = lm)

happy %>%
  group_by(Region) %>%
  dplyr::summarize(cor(Generosity, `Happiness Score`))

####################################
###### Statistical Tests ###########
####################################

?cor.test

cor.test(happy$Freedom, happy$`Happiness Score`)
cor.test(happy$`Economy (GDP per Capita)`, happy$Freedom)

?lm()

model <- lm(`Happiness Score` ~ Freedom, data = happy)
objects(model)
model$coefficients
model$fitted.values
model$residuals
summary(model)

model2 <- lm(`Happiness Score` ~ `Economy (GDP per Capita)`, data = happy)
summary(model2)

model3 <- lm(`Happiness Score` ~ Freedom + `Economy (GDP per Capita)`, data = happy)
summary(model3)

model4 <- lm(`Happiness Score` ~ Freedom*`Economy (GDP per Capita)`, data = happy)
summary(model4) # significant interaction!

##############################
### Exploring Interactions ###
##############################

# First let's use what we know
# Use case_when to make discrete groups
econ <- happy$`Economy (GDP per Capita)` # assign economy values to new object b/c annoying to type

happy %>%
  mutate(Econ_Group = case_when(econ > mean(econ) + sd(econ) ~ "high",
                                econ < mean(econ) + sd(econ) & econ > mean(econ) - sd(econ) ~ "average",
                                econ < mean(econ) - sd(econ) ~ "low")) %>%
  ggplot(aes(Freedom, `Happiness Score`, color = Econ_Group))


# Check out the super cool jtools package by Jacob Strong at OSU

library(jtools)

# First rename some variables because jtools does not play nice with weird names
happy %<>%
  dplyr::rename(Economy = `Economy (GDP per Capita)`,
                Happiness = `Happiness Score`)

model5 <- lm(Happiness ~ Freedom*Economy, data = as.data.frame(happy))
summ(model5)
summ(model5, scale = TRUE) # mean-centered

interact_plot(model5, pred = Freedom, modx = Economy)

interact_plot(model5, pred = Freedom, modx = Economy, modxvals = "plus-minus")

interact_plot(model5, pred = Freedom, modx = Economy, modxvals = "plus-minus",
              plot.points = TRUE)

interact_plot(model5, pred = Freedom, modx = Economy, modxvals = "plus-minus",
              plot.points = TRUE, interval = TRUE)

interact_plot(model5, pred = Freedom, modx = Economy, modxvals = "plus-minus",
              plot.points = TRUE) + theme_apa()
