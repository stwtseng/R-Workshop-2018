######################################################################
########### Workshop 1: Data Wrangling & Visualization ###############
######################################################################

########### Part 1: Occupation, Wages, and Gender ####################

### Load Library###

library('tidyverse')

### Load Dataset ###

list.files()

?read.csv()

df <- read.csv("inc_occ_clean.csv")

### Take a Look at Dataset ###

names(df)

head(df)
tail(df)

df$Occupation

class(df$Occupation)

class(df$All_weekly)

View(df)

### Visualize Dataset ###

# How many Workers in each Category of Occupation?

ggplot(data = df[-1,], mapping = aes(x = Occupation, y = All_workers)) +
  geom_col() +
  coord_flip()

# Bar Graph: Weekly Wage for All Workers by Occupation
ggplot(data = df, mapping = aes(x = Occupation, y = All_weekly)) +
  geom_col() +
  coord_flip()

### Explore Weekly Wages by Gender ###

# First, assign relevant variables to a holder
wage <- df[,c(1,5,7)]

# Next, let's rename the variables
names(wage)[2:3] <- c("Male","Female")

# Use the gather() function from the tidyr package to change the data into longform
wage.long <- gather(wage, "Male", "Female", key = "Gender", value = "Weekly Wage")

# Visualize weekly wage by gender across all occupations in one graph
# Why is [-1,] necessary?
ggplot(wage.long[-1,], aes(Gender, `Weekly Wage`)) +
  geom_col(aes(fill = Gender))

# Visualize weekly wage by gender across each occupation in one graph
ggplot(wage.long, aes(Gender, `Weekly Wage`)) +
  geom_col(aes(fill = Gender), position = "dodge")

ggplot(wage.long, aes(Gender, `Weekly Wage`)) +
  geom_col(aes(fill = Gender)) +
  facet_wrap(~ Occupation, nrow = 5)

ggplot(wage.long, aes(Occupation, `Weekly Wage`)) +
  geom_col(aes(fill = Gender), position = "dodge") +
  coord_flip() +
  xlab("") +
  ylab("Median Income (USD/week)") +
  ggtitle("Income Gap by Industry")

# Let's calculate the gap
gap <- wage$Male - wage$Female
mean(gap)

# You can also make a new column with that variable
wage %>%
  gap <- mutate(gap = wage$M_weekly - wage$F_weekly)



########### Part 2: Happiness ####################

### Load Dataset ###
list.files()

?read_csv()

happy2017 <- read_csv("world-happiness-report/2017.csv")

### Take a look at dataset ###

glimpse(happy2017)

names(happy2017)

length(happy2017$Country)

View(happy2017)

### Visualize the relation between freedom and happiness ###

ggplot(happy2017) +
  geom_point(aes(Freedom, Happiness.Score))

ggplot(happy2017) +
  geom_point(aes(Freedom, Happiness.Score)) +
  geom_smooth(aes(Freedom, Happiness.Score), method = lm)

### Visualize the relation between freedom and happiness across different economies ###

ggplot(happy2017) +
  geom_point(aes(Freedom, Happiness.Score, color = Economy..GDP.per.Capita.))

ggplot(happy2017) +
  geom_point(aes(Freedom, Happiness.Score, size = Economy..GDP.per.Capita.))

ggplot(happy2017) +
  geom_point(aes(Freedom, Happiness.Score, 
                 color = Economy..GDP.per.Capita.,
                 size = Economy..GDP.per.Capita.))



