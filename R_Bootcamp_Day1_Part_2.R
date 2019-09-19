install.packages("here")
library(here)
here()

gapminder <- read.csv(here::here("data","gapminder5.csv"))
str(gapminder)
gapminder$country <- as.character(gapminder$country)
gapminder$continent <- as.character(gapminder$continent)
str(gapminder)

obs <- 1:nrow(gapminder)

for (i in obs) {
    gapminder[i,"gdp"]<- gapminder[i,"pop"] * gapminder[i, "gdpPercap"]
}


for (i in nrow(gapminder)) {
    gapminder$log_gdpPercap <- log(gapminder$gdpPercap)
    gapminder$log_pop <- log(gapminder$pop)
}

gapminder$vec_log_gdpPercap <- log(gapminder$gdpPercap)

#Below we calculate the mean life expectancy by year
years <- unique(gapminder$year)
for (i in years) {
    mean_lifeExp <- mean(gapminder$lifeExp[gapminder$year == i], na.rm = TRUE)
    print(paste0(i,": ", mean_lifeExp))
}
#Below we calculate the mean life expectancy by continent
continents <- unique(gapminder$continent)

for (i in continents) {
    mean_le <- mean(gapminder$lifeExp[gapminder$continent== i], na.rm = TRUE)
    print(paste0(i,": ", mean_le))
}

view(gapminder)
head(gapminder$log_gdpCapita)
str(gapminder$log_gdpCapita)

#Below we nest For loops
# We calculate the mean life expectancy by year and by continent

for (i in continents) {
    print(paste0("Continent: ",i))
    for(j in years) {
        mean_le <- mean(gapminder$lifeExp[gapminder$continent == i & gapminder$year ==j], na.rm=TRUE)
        print(paste0(j, ": ", mean_le))
    }
}


#Has the gap betweenlife expectancy between countries on different continents narrowed over time?
?sd

for (i in continents) {
    print(paste0("Continent: ",i))
    for(j in years) {
        sd_le <- sd(gapminder$lifeExp[gapminder$continent == i & gapminder$year ==j], na.rm=TRUE)
        print(paste0(j, ": ", sd_le))
    }
}

# if you want to store your results from a for loop:

results <- list(NULL)

for (i in seq(years)) {
    results[[i]]<- years[i]
}

# Let's look at the apply functions
# First of all apply:
vars <- gapminder[,c("lifeExp","pop","gdpPercap")]
apply(vars, 2, mean)
?apply

#lapply does the same as apply, but it takes the output and makes it a list
lapply(gapminder, mean)

#sapply -> be careful, sometimes the output is not what you expect
#sapply simply "simplifies" the output of apply

sapply(years, function(x) mean(gapminder$lifeExp[gapminder$year == x]))

#The above is basically used whenever we want 

## While Loops
#
i <- 1987

while (i <2003){
    
    sd_le <- sd(gapminder$lifeExp[gapminder$year==i])
    print(paste0(i,": ", sd_le))
    i=i+5
}

## Conditions, IF, ELSE FUNCTIONS:
set.seed(1)
random_year <- sample(years,1)

if (random_year >1977){
    print(random_year)
}

# Now with an Else
set.seed(1022)
random_year <- sample(years,1)

if (random_year >1977){
    print(random_year)
} else {
    print("sorry, random year is less than 1977")
}

# Combining For with If Else
threshold <- 70

for (i in unique(gapminder$continent)) {
    tmp <- mean(gapminder$year[gapminder$continent == i])
    
    if (tmp < threshold) {
        print(paste("Mean Life Expectancy in", i, "is less than", threshold))
    } else {
        print(paste("Mean Life Expectancy in",i,"is greater than", threshold))
    }
}

#Mean population for years >= 1987. Print a statement if the condition is not met

threshold <- 1987

for (i in unique(gapminder$year)) {
    if (i < threshold) {
        print(paste("Sorry,", i,"is less than",threshold))
    } else {
        tmp <- mean(gapminder$pop[gapminder$year == i ])
        print(paste(i,tmp))
    }
}
### Functions
get_values <-
    function(df, variable = "continent") {
        vals <- unique(df[[variable]])
        print(paste0(variable, ": ", vals))
    }

report_mean_sd <-
    function(df, variable, country) {
        var <- df[[variable]][df$country==country]
        m_le <- mean(var)
        sd_le <- sd(var)
        cat("Country:", country,
            "\nMean Life Expectancy:", m_le,
            "\nSD Life Expectancy:", sd_le)
    }

report_mean_sd(gapminder,"lifeExp","Bulgaria")

# Below is a function which calculates the mean, median, min, and max life expectation for a continent from the gapminder data

report <- 
    function(df, continent) {
        var <- df[["lifeExp"]][df$continent==continent]
        m_le <- mean(var)
        med_le <- median(var)
        min_le <- min(var)
        max_le <- max(var)
        cat("Continent:", continent,
            "\nMean Life Expectancy:", m_le,
            "\nMedian Life Expectancy:", med_le,
            "\nMinimum Life Expectancy:", min_le,
            "\nMaximum Life Expectancy:", max_le)
    }

report(gapminder, "Asia")


