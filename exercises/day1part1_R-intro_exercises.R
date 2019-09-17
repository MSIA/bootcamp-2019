############ MSiA Boot Camp: Intro to R: Day 1 Exercises #############
### by Kumar Ramanathan, based on materials from Christina Maimone ###

#### REVIEW: BASICS AND DATA TYPES ####

#### Arithmetic ####

# Pick a number; save it as x
x<-17

# Multiply x by 3
x=x*3
x

# Take the log of the above (Hint, you need the function log() here)
x<-log(x)

# Subtract 4 from the above
x<-log(x)-4

# Square the above
x<-x^2

#### Comparisons and Logical Operators ####

# Check if 1 is bigger than 2
1>2

# Check if 1 + 1 is equal to 2
1+1 ==2

# Check if it is true that the strings "eat" and "drink" are not equal to each other
"eat"=="drink"

# Check if it is true that 1 is equal to 1 *AND* 1 is equal to 2 
# (Hint: remember what the operators & and | do)
1==2 & 1==1

# Check if it is true that 1 is equal to 1 *OR* 1 is equal to 2
1==2 | 1==1

#### Packages and Functions ####

# Load the package tidyverse
library("tidyverse")

# Open the help file for the function recode 
# (Hint: remember what ? does)
?recode

#### REVIEW: DATA STRUCTURES ####

#### Vectors ####

# Run this code to generate variables x1 and x2
set.seed(1234)
x1 <- rnorm(5)
x2 <- rnorm(20, mean=0.5)

# Select the 3rd element in x1
x1[3]
x1[1:4]

# Select the elements of x1 that are less than 0
x1
x1[x1<0]

# Select the elements of x2 that are greater than 1
typeof(x1)
class(x1)

# Create x3 containing the first five elements of x2
x3 <- x1[1:5]
x3
# Select all but the third element of x1
x1[-3]

#### Missing values ####

# Generate a vector
vec <- c(1, 8, NA, 7, 3)

# Calculate the mean of vec, excluding the NA value
mean(vec[!is.na(vec)])

# Count the number of missing values in vec
sum(is.na(vec))

#### Factors ####

# See lecture notes and DataCamp for guidance and practice


#### Lists ####

# See lecture notes and DataCamp for guidance and practice


#### Matricies ####

# Generate a matrix
mat <- matrix(c(1:51, rep(NA,4)), ncol=5)

# Select row 4, column 5
mat[4,5]
mat
# Select column 3
mat[,3]

# Bonus: How many NA values are there in this matrix?
sum(is.na(mat))

#### Data frames ####

# Load one of R's example data frames, mtcars
data(mtcars)
mtcars
view(mtcars)
# Identify the number of observations (rows) and number of variables (columns)
dim(mtcars)
summary(mtcars)

# Identify the names of the variables

names(mtcars)
# Select the variable 'mpg'


# Select the 4th row


# Square the value of the 'cyl' variable and store this as a new variable 'cylsq'
mtcars$cylsq <-mtcars$cyl^2
mtcars$cylsq
#### READING FILES ####

# Check your working directory. It should be the root folder where you downloaded the boot camp materials. If that's not the case, set your working directory accordingly.
getwd()

# Read gapminder data with read.csv()
gapminder <- read_csv("data/gapminder5.csv", stringsAsFactors=FALSE)

# Load the readr package

library(readr)

# Read gapminder data with read_csv()
read_csv("data/gapminder5.csv")

#### DATA MANIPULATION ####

#### Exploring data frames ####

# Run summary() on the gapminder data
summary(gapminder)

# Find the mean of the variable pop
mean(gapminder$pop)

# Create a frequency table of the variable 'year'
# Hint: use table()
table(gapminder$year)

# Create a proportion table of the variable 'continent'
# Hint: use prop.table()
prop.table(gapminder$continent)
?prop.table
prop.table(table(gapminder$continent))
?table

#### Subsetting and Sorting ####

# Create a new data frame called gapminder07 contaning only those rows in the gapminder data where year is 2007
gapminder07 <- gapminder[gapminder$year==2007,]
view(gapminder07)
# Created a sorted frequency table of the variable continent in gapminder07
sorted_freq_table <- sort(table(gapminder07$continent))
sorted_freq_table
# Print out the population of Mexico in 2007
print(gapminder07$pop[gapminder07$country=="Mexico"])

# BONUS: Print out the rows represnting the 5 countries with the highest population in 2007
# Hint: Use order(), which we learned about, and head(), which prints out the first 5 rows of a data frame
head(gapminder07[order(gapminder07$pop, decreasing=TRUE),])

#### Adding and removing columns ####

# See lecture notes for more guidance. We will practice this skill in the next section.


#### Recoding variables ####

# Round the values of the variable `lifeExp` using `round()` and store this as a new variable `lifeExp_round`


# Print out the new variable to see what it looks like


# This code creates the new variable 'lifeExp_over70'. Try to understand what it does.
gapminder07$lifeExp_over70 <- NA  # Initialize a variable containing all "NA" values
gapminder07$lifeExp_over70[gapminder07$lifeExp>70] <- "Yes"
gapminder07$lifeExp_over70[gapminder07$lifeExp<70] <- "No"
table(gapminder07$lifeExp_over70)

# Try to create a new variable 'lifeExp_highlow' that has the value 
# "High" when life expectancy is over the mean and the value "Low" 
# when it is below the mean. When you are done, print a frequency table.
names(gapminder07)
gapminder07$lifeExp_highlow <- NA
gapminder07$lifeExp_highlow[gapminder07$lifeExp>mean(gapminder07$lifeExp)] <- "High"
gapminder07$lifeExp_highlow[gapminder07$lifeExp<mean(gapminder07$lifeExp)] <- "Low"
head(gapminder07)
mean(gapminder07$lifeExp)

#### Aggregating ####

# Find the mean of life expectancy in 2007 for each continent
# Hint: use the aggregate() function
?aggregate
aggregate(gapminder07$lifeExp ~ gapminder07$continent, FUN=mean)

# Can also use:
aggregate(lifeExp ~ continent, data = gapminder07, FUN = mean)

#### Statistics, part 1 ####

# Calculate the correlation between 'lifeExp' and 'gdpPercap'.
?cor
cor(gapminder07$lifeExp,gapminder07$gdpPercap)

# Use a t-test to evaluate the difference between 'gdpPercap' in "high" and "low" life expectancy countries. Store the results as t1, and then print out t1.

?t.test
t1 <- t.test(gapminder07$gdpPercap ~ gapminder07$lifeExp_highlow)
t1

#### Statistics, part 2 ####

# Conduct a linear regression predicting 'lifeExp' as a function of 'gdpPercap' and 'pop', and store the results as reg1.
?lm
reg1 <- lm(gapminder07$lifeExp ~ gapminder07$gdpPercap + gapminder07$pop)
print(reg1)
summary(reg1)
?summary
typeof(reg1)
class(reg1)

# Print out reg1.


# Run summary() on reg1.


#### WRITING FILES ####

#### Writing a data file ####

# Save the gapminder07 data frame as a CSV file using write.csv() in the "data" subfolder within the working directory
# Set the argument `row.names = FALSE`.
write.csv(gapminder07, file="data/gapminder07.csv", row.names=FALSE)

#### Save R objects ####

# See lecture notes for guidance and more examples.
?save.image
save.image("My_Environment_Bootcamp_R.RData")
load.image()

#### DATA VISUALIZATION ####

#### Histograms ####

# Create a histogram of the variable 'lifeExp' in gapminder07

hist(gapminder07$lifeExp,main="Distribution of life exectancy across countries in 2007",xlab="Life expectancy", ylab="Frequency")
# Re-create the histogram with a title and axis labels


# Bonus: Change the `breaks = ` argument from its default setting and see what happens.


#### Scatterplots ####

# Create a scatterplot with `lifeExp` on the y-axis and `gdpPercap` on the x-axis.
plot(lifeExp ~ gdpPercap, data=gapminder07, main="Life Expectancy as a function of gdp per capita")
?plot

# Add a title and axis labels.


# Bonus: Add a horizontal line indicating the mean of `lifeExp` onto the plot using `abline()`.
abline(h=mean(gapminder07$lifeExp))
abline(reg1)
?abline
