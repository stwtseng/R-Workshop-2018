######################################################################
########### Workshop 1: Data Wrangling & Visualization ###############
######################################################################

########### Part 1: Occupation, Wages, and Gender ####################

### Load Library###



### Load Dataset ###



### Take a Look at Dataset ###



### Visualize Dataset ###

# How many Workers in each Category of Occupation?

ggplot(data = df[-1,], mapping = aes(x = Occupation, y = All_workers)) +
  geom_col() +
  coord_flip()

# Bar Graph: Weekly Wage for All Workers by Occupation
ggplot(data = df, mapping = aes(x = Occupation, y = All_weekly)) #partial code

### Explore Weekly Wages by Gender ###



# First, assign relevant variables to a holder

wage <- df[,c(1,5,7)]

names(df)
wage <- select(df, Occupation, M_weekly, F_weekly)

# Next, let's rename the variables

names(wage)

rename(wage, M_weekly = Male, F_weekly = Female)

# Use the gather() function from the tidyr package to change the data into longform



# Visualize weekly wage by gender across all occupations in one graph
# Why is [-1,] necessary?
ggplot(wage.long[-1,], aes(Gender, `Weekly Wage`)) + 
  geom_col(aes(fill = Gender))

# Next, visualize weekly wage by gender across each occupation separately
# Only need to add one more line to the following code
ggplot(wage.long, aes(Gender, `Weekly Wage`)) +
  geom_col(aes(fill = Gender))

# Visualize weekly wage by gender across each occupation in one graph
# Try to recreate the graph shown on the board

# Let's calculate the gap
gap <- (wage$Male - wage$Female)
mean(gap)

# You can also make a new column with that variable
wage %>%
  gap <- mutate(gap = wage$M_weekly - wage$F_weekly)




########### Part 2: Happiness ####################


### Load Dataset ###



### Take a look at dataset ###



### Visualize the relation between freedom and happiness ###



### Visualize the relation between freedom and happiness across different economies ###




### Explore basic descriptive statistics ###





########### Part 3: Happiness over the Years ####################


