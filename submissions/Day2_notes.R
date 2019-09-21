#### DAY 2 ####

# Reading in the data
generation <- read.csv(here::here("data/ca_energy_generation.csv"), stringsAsFactors = F)
imports <- read.csv(here::here("data/ca_energy_imports.csv"), stringsAsFactors = F)
str(generation)
class(generation$datetime)
class(imports$datetime)

# The best way to deal with date-time data is to use the lubridate package 
# and the as_datetime function
library(lubridate)
generation$datetime <- as_datetime(generation$datetime)
class(generation$datetime)
head(generation$datetime)

#### Reshaping data ####
head(generation)

# Using reshape2
# melt ???> make data long
# dcast ???> make data wide
# recast???> melt then cast data

# melt
library(reshape2)
long_gen <- melt(generation, id.vars = "datetime",
                 variable.name = "source",
                 value.name = "usage")
head(long_gen)
long_gen[order(long_gen$datetime)[1:20], ]

# Merging data
merged_energy <- merge(generation, imports, by = "datetime")
dim(merged_energy)
head(merged_energy)

long_merged_energy <- melt(merged_energy, id.vars = "datetime",
                           variable.name = "source",
                           value.name = "usage")
head(long_merged_energy)

#### dplyr ####

# select ???> subset variables
# filter ???> subset observations based on conditions
# mutate ???> add new variables
# summarize ???> reduce multiple observations to a single value (e.g., find the mean)

# select

# select by name: select(gapminder, continent, pop)
# select by position: select(df, c(1, 3, 10))
# select by range: select(df, country:pop) or select(df, 1:3)
# drop variables with -: select(df, -gdpPercap)

library(tidyverse)
library(dplyr)
tmp <- select(merged_energy, biogas, biomass, geothermal, solar)
names(tmp)

# filter
tmp <- filter(merged_energy, imports > 7000)
nrow(tmp)
head(tmp)

tmp <- filter(merged_energy, imports > 7000, natural_gas < 7000)
nrow(tmp)
head(tmp)

# mutate-----create new variables
tmp <- mutate(long_merged_energy, log_usage = log(usage))
head(tmp)

# summarize
summarize(long_merged_energy, total = sum(usage, na.rm = T))
summarize(long_merged_energy, mean_cons = mean(usage, na.rm = T))

# %>% operator lets you chain together functions
# While piping, the piped dataframe is not changed!
long_merged_energy %>% 
    filter(source == "geothermal") %>% 
    select(-datetime) %>% 
    mutate(log_usage = log(usage)) %>% 
    summarize(mean_log_usage = mean(log_usage, na.rm = T))

merged_energy %>% 
    select(contains("hydro")) %>%   # filter(source == "hydro") %>% can't work her
    mutate(total_hydro = rowSums(., na.rm = T)) %>%
    summarize(mean_hydro = mean(total_hydro, na.rm = T))

# group by
long_merged_energy %>% 
    group_by(source) %>% 
    summarize(sum_usage = sum(usage, na.rm = T))

# comparing to loop
gapminder <- read.csv(here::here("data/gapminder5.csv"))
gapminder %>% 
    group_by(year) %>% 
    summarize(mean_le = mean(lifeExp, na.rm = T),
              sd_lf = sd(lifeExp, na.rm = T))

# Use your knowledge of dplyr to find the mean usage for small hydro, 
# large hydro, biogas, and biomass
long_merged_energy %>% 
    filter(source %in% c("small_hydro","large_hydro", "biogas", "biomass")) %>% 
    group_by(source) %>% 
    summarize(mean_usage = mean(usage, na.rm = T))

# or
merged_energy %>% 
    select(datetime, contains("hydro"), contains("bio")) %>% 
    melt(id.vars = "datetime",
         variable.name = "source",
         value.name = "usage") %>% 
    group_by(source) %>% 
    summarize(mean_usage = mean(usage, na.rm = T))

# join v.s. merge
left_join = merge(all.x = T)
right_join = merge(all.y = T)
full_join = merge(all = T)
inner_join = merge(all = F)

#### Data Table ####
# dt(i, j, by)   i:row, j: col, by: group by

library(data.table)
data_file <- here::here("data", "ca_energy_generation.csv")    
generation_df <- read.csv(data_file, stringsAsFactors = F)
generation_dt <- fread(data_file) # fread similar to read.table but faster and more convenient

class(generation_df) 
class(generation_dt) 
View(generation_df)
View(generation_dt)
generation_df
generation_dt
str(generation_df)
str(generation_dt)

# Row filtering
generation_dt[wind > 4400]
generation_dt[wind > 4400 & mday(datetime) == 7]

# Telect rows for which natural gas generation is less than 
# or equal to 5,000 MW and large hydro generation is greater than 2,000 MW
generation_dt[natural_gas <= 5000 & large_hydro > 2000]

# Select rows for which coal generation is greater than 10 MW and 
#solar generation is greater than the median value of solar generation
generation_dt[coal > 10 & solar > median(soalr)]

# Column operations
generation_dt[,wind + solar]

generation_dt[,3*wind + solar*biogas/2]

# New columns
generation_dt[,newcol := 3*wind + solar*biogas/2]

generation_dt[,.(newcol = 3*wind + solar*biogas/2)]

generation_dt[,newcol := NULL] # delete


# Add a column called ???total_hydro??? that is the sum of the 
# small_hydro and large_hydro columns
generation_dt[,.(total_hydro = small_hydro+large_hydro)] # not directly modify the table and print the results
generation_dt[,total_hydro := small_hydro + large_hydro] # directly modify the table but not print the results

# Find the mean of the nuclear and biogas columns
generation_dt[,.(V1 = mean(nuclear), V2 =mean(biogas))]
# Create a new table: for the hours when solar generation is zero, 
# get the datetime and total_thermal (sum of natural gas and coal generation)
generation_dt[solar == 0, .(datetime, total_thermal = natural_gas + coal)]


# Group by
generation_dt[,.(mean_nuc = mean(nuclear), mean_wind = mean(wind)), by = mday(datetime)]

# Find the median solar generation by hour.
generation_dt[, median(solar), by = hour(datetime)]

# For hours when the solar generation is greater than zero, 
# find the maximum natural gas generation by day
generation_dt[solar > 0, max(natural_gas), by = mday(datetime)]

# Convert this dplyr syntax into data.table syntax 
long_ca_energy <- long_ca_energy %>%
    mutate(day = as_date(datetime),
           log_output = log(output)) %>%
    group_by(day) %>%
    mutate(total_daily_output = sum(output, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(per_output = output/total_daily_output)

# Answer
all_generation_long[,day := as_date(datetime)]
all_generation_long[,log_output := log(value)]
all_generation_long[,per_output := value/sum(value), by = day]

# set column names
setnames(dt, "old", "new")

# set row order
setorder(dt, col1, -col2, ...)

# set anything
set(dt, i, j)

# set colum
dt[,col1 := 2*col2]

# .N: number of rows in the current group
# .I: a vector, 1:nrow(dt), usually used for more advanced operations
generation_dt[,.N] 
generation_dt[,.I]

# keys: one or more columns, pre-sorted index of the table.
key(generation_dt)
setkey(generation_dt, datetime)
key(generation_dt)


