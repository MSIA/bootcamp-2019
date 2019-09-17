#Day 1 Part 2

#### Load the data ####
# Load the gapminder dataset.Find the factor variables and change them to character vectors.
gapminder$country <- as.character(gapminder$country)
gapminder$continent <- as.character(gapminder$continent)
str(gapminder)

# the mean life expectancy by country?
mean(gapminder$lifeExp[gapminder$country == "Afghanistan"])

# create a vector of values that you want to repeat the function for
obs <- 1:nrow(gapminder)

# initialize the for loop with `for (i in vector)` 
for (i in obs) { # the function to repeat is enclosed in braces {}
    gapminder[i, "gdp"] <- gapminder[i, "pop"] * gapminder[i, "gdpPercap"]
}


# Create a new variable that finds that natural log (log) of the GDP per capita 
# and of population - call them log_gdpPercap and log_pop
obs1 <- 1:nrow(gapminder)
for (i in obs1) { 
    gapminder[i, "log_gdpPercap"] <- log(gapminder[i, "gdpPercap"])
    gapminder[i, "log_pop"] <- log(gapminder[i, "pop"])
}
head(gapminder)

# Avoid when possible, especially when there is a vectorized function you can use
gapminder$vec_log_gdpPercap <- log(gapminder$gdpPercap)
all(gapminder$vec_log_gdpPercap == gapminder$log_gdpPercap)

# Find the mean life expectancy by year
years <- unique(gapminder$year)

for (i in years) {
    mean_le <- mean(gapminder$lifeExp[gapminder$year == i], 
                    na.rm = T)   #including NA value
    print(paste0(i, ": ", mean_le))  #paste: Concatenate vectors after converting to character.
}

# Which continent has the highest mean life expectancy?
conts <- unique(gapminder$continent)

for (i in conts) {
    mean_le <- mean(gapminder$lifeExp[gapminder$continent == i], 
                    na.rm = T)   
    print(paste0(i, ": ", mean_le))  
}

# What is the mean life expectancy for each continent for each year?
conts <- unique(gapminder$continent)

for(i in conts){
    print(paste0("Continents: ",i))
    for ( j in years){
        mean_le <- mean(gapminder$lifeExp[gapminder$continent == i & gapminder$year == j],
                        na.rm = T)

        print(paste0(j, ": ", mean_le))    }
}

# What is the standard deviation (sd) for life expectancy for each continent for each year?
for(i in conts){
    print(paste0("Continents: ",i))
    for ( j in years){
        std_counts <- sd(gapminder$lifeExp[gapminder$continent == i & gapminder$year == j],
                          na.rm = T)
        
        print(paste0(j, ": ", std_counts))    }
}

# apply family: apply, lapply, sapply
vars <- gapminder[, c("lifeExp", "pop", "gdpPercap")]
apply(vars, 2, mean)

# lapply returns a list, sapply returns a simplified list (i.e., a vector)
# there is some inconsistency in how sapply returns results, so always check
lapply(gapminder, mean)

# Add function(x) [function] to the call???x becomes the iterator
sapply(years, function(x) mean(gapminder$lifeExp[gapminder$year == x]))

# Whilw loop
i <-  1952 # define the interator

while (i < 1987) {
    sd_le <- sd(gapminder$lifeExp[gapminder$year == i])
    print(paste0(i, ": ", sd_le)
    )
    i <- i + 5 # increase the iterator by the interval between years
}

# What is the standard deviation for life expectancy for each year 
# between 1987 and 2002 (inclusive)?
i <- 1987

while (i < 2003) {
    sd_le <- sd(gapminder$lifeExp[gapminder$year == i])
    print(paste0(i, ": ", sd_le))
    i <- i + 5
}

#### If Statement ####
# Use an if() statement to print a suitable message reporting whether there are any records from 2002 in the gapminder dataset. Now do the same for 2012.
# Hint: use the any function.

#### Loop and If Statements ####
#Write a script that finds the mean life expectancy by country for countries whose population is below the mean for the dataset

# Write a script that loops through the gapminder data by continent and prints out whether the mean life expectancy is smaller than 50, between 50 and 70, or greater than 70.

#### Exercise: Write Functions ####
#Create a function that given a data frame will print the name of each column and the class of data it contains. Use the gapminder dataset. Hint: Use mode() or class() to get the class of the data in each column. Remember that names() or colnames() returns the name of the columns in a dataset.

# Create a function that given a vector will print the mean and the standard deviation of a vector, it will optionally also print the median. Hint: include an argument that takes a boolean (TRUE/FALSE) operator and then include an if statement.

#### Analyzing the relationship ####
# Use what you???ve learned so far to answer the following questions using the gapminder dataset. Be sure to include some visualizations!
# What is the relationship between GDP per capita and life expectancy? Does this relationship change over time? (Hint: Use the natural log of both variables.)

# Does the relationship between GDP per capita and life expectacy vary by continent? Make sure you divide the Americas into North and South America.