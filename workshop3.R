##### Load libraries #####

rm(list = ls())



##### Load the Happiness datasets from last time #####



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

?inner_join

glimpse(happy)
head(happy)
View(happy)

#################################
##### Review: Tidy Datasets #####
#################################

happy %<>%
  arrange(Country)

?gather
?separate
?spread

happy %<>%
  gather(key, value, -Country, -Region) %>%
  separate(key, into = c("variable", "Year"), sep = "\\.") %>%
  spread(variable, value) %>%
  mutate(Year = as.factor(Year)) %>%
  mutate(`Happiness Rank` = as.integer(`Happiness Rank`))

View(happy)

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



##################################
##### Descriptive Statistics #####
##################################

# How many countries per region?

?tally

happy %>%
  group_by(Region) %>%
  distinct(Country) %>%
  tally()

# Descriptive stats per region?
# Other packages outside tidyverse have more useful summary functions

?pastecs::stat.desc
?by
?psych::describe
?psych::describeBy

by(happy[,4:12], INDICES = c(happy[,2:3]), FUN = pastecs::stat.desc, basic = FALSE)

psych::describeBy(happy, group = c("Region", "Year"))


########################
##### Correlations ##### 
########################

# Plot and examine the correlation b/t happiness and freedom over the years

happy %>%
  group_by(Year) %>%
  ggplot(aes(Freedom, `Happiness Score`, color = Year)) +
  geom_point() + geom_smooth(method = lm)

happy %>%
  group_by(Year) %>%
  dplyr::summarize(cor(Freedom, `Happiness Score`))

# ... happiness and economy over the years



# ... and life expectancy



# ... and family



# ... and generosity



### What about by region? ###

# Plot and examine the correlation b/t happiness and freedom across regions

# freedom

happy %>%
  ggplot(aes(Freedom, `Happiness Score`, color = Region)) +
  geom_point() + geom_smooth(method = lm) +
  ylim(0,10)

happy %>%
  group_by(Region) %>%
  dplyr::summarize(cor(Freedom, `Happiness Score`))

# economy



# life expectancy



# family



# generosity




