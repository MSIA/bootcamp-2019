#Day 1 Part 2

#### Rmarkdown ####
# See rmd_template.Rmd

#### Load the data ####
# Load the gapminder dataset.Find the factor variables and change them to character vectors.
gapminder <- read.csv(here::here("data/gapminder5.csv"), stringsAsFactors=FALSE) #load
gapminder$country <- as.character(gapminder$country)
gapminder$continent <- as.character(gapminder$continent)
str(gapminder)

#### Loop and If Statements ####
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

# While loop
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
# Which continents have a mean life expectancy greater than 70 years?
threshold <- 70

for(i in unique(gapminder$continent)){
    tmp <- mean(gapminder$lifeExp[gapminder$continent == i])
    if (tmp < threshold){
        print(paste("Mean Life Expectancy in", i, "is less than", threshold))
    }
    else{
        print(paste("Mean Life Expectancy in", i, "is greater than", threshold))
    }
}
# Write a for loop that reports the mean population for years 
# greater than or equal to 1987.    
threshold <- mean(gapminder$pop[gapminder$year == 1987])

for(i in years){
    tmp <- mean(gapminder$pop[gapminder$year == i])
    if (tmp < threshold){
        print(paste("Mean Population for", i, "is less than", threshold))
    }else {
        print(paste("Mean Population for", i,"is", tmp))
    }
}


#### Write Functions ####
# Function that prints the value of a selected variable in the gapminder dataset
get_values <-
    function(df, variable = "continent") {
        vals <- unique(df[[variable]])
        print(paste0(variable, ": ", vals))
    }

# function that prints the mean and standard deviation for life expentancy 
# for a given country in the gapminder dataset
report_mean_sd <- 
    function(df, variable, country) {
        var <- df[[variable]][df$country == country] # df[[variable]] = df[, variable]
        m_le <- mean(var)                            # typeof(gapminder[["pop"]]): double
        sd_le <- sd(var)                             # typeof(gapminder["pop"]): list
        cat("Country:", country, 
            "\nMean Life Expectancy:", m_le,
            "\nSD Life Expectancy:", sd_le)
    }

report_mean_sd(gapminder, "lifeExp", "Bulgaria")

# Write a function that reports the mean, median, minimum, and maximum 
# for life expectancy for a continent in gapminder
report_mean_med_min_max <- 
    function(df, variable, continent) {
        var <- df[[variable]][df$continent == continent] 
        mean_le <- mean(var)                           
        med_le <- median(var)
        min_le <- min(var)
        max_le <- max(var)
        cat("Continent:", continent, 
            "\nMean Life Expectancy:", mean_le,
            "\nMedian Life Expectancy:", med_le,
            "\nMinimum Life Expectancy:", min_le,
            "\nMaximum Life Expectancy:", max_le)
    }

report_mean_med_min_max(gapminder, "lifeExp", "Asia")

# A log-log model relating life expectancy to GDP
viz_lm <-
    function(df, dv, iv, year) {
        dat <- df[df[["year"]] == year, ]
        y <- log(dat[[dv]])
        x <- log(dat[[iv]])
        fit <- lm(y ~ x)
        plot(y ~ x, main = year,
             xlab = iv, ylab = dv)
        lines(x, predict(fit), col = 'blue')
    }

viz_lm(gapminder, "lifeExp", "gdpPercap", 1977)

# Loop it!
for (i in years) {
    viz_lm(gapminder, "lifeExp", "gdpPercap", i)
}

