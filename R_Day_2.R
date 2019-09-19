library(here)
library(readr)


generation <- read.csv("data/ca_energy_generation.csv", stringsAsFactors = FALSE)
imports <- read.csv("data/ca_energy_imports.csv", stringsAsFactors = FALSE)

str(generation)
class(generation$datetime) #Notice that the first variable "datetime" is a character.
#You want it to be a datetime format, because it's more useful and easy to work with.

#To convert to daytime, we will use lubridate!
library(lubridate)
generation$datetime <- as_datetime(generation$datetime)
imports$datetime <- as_datetime(imports$datetime)
class(generation$datetime)
class(imports$datetime)


#
library(reshape2)
long_gen <- melt(generation, id.vars="datetime",
                             variable.name="source",
                             value.name = "usage")
head(long_gen)

#It's a good idea to put the names of the variables in quotation marks.
#R is unique in the fact that sometimes it will understand you if you don't (that's called non-standard evaluation), but it's a better idea to use quotation marks just in case.

#MERGING Data
#Syntax: merge(x, y, by.x="id", by.y="cd")
dim(generation)
dim(imports)
merged_energy <- merge(generation,imports, by="datetime")
dim(merged_energy)

long_merged_energy <- melt(merged_energy, id.vars="datetime",
                           variable.name = "source",
                           value.name = "usage")
head(long_merged_energy)

##Dplyr - It uses a chain of simple functions instead of single functions with a lot of arguments
#Note that dplyr is meant for small/medium datasets.
#data.table is compact, syntax-based and much faster on large data sets.

library(tidyverse)
merged_energy %>%
    select(contains("hydro")) %>% 
    mutate(total_hydro=rowSums(. , na.rm=TRUE)) #We use rowSUms to sum all the variables in a row fashion. THe "." means we are taking teh result of the previous step %>% 
    summarize(mean_of_hydro_variables=mean(total_hydro))
              
long_merged_energy %>% 
    group_by(source) %>% 
    summarize (sum(usage, na.rm=True))

#Task: Find the mean usage for small hydro, large hydro , biogas and biomass

merged_energy <- merge(generation,imports, by="datetime")
merged_energy %>% 
    select(contains("hydro"),contains("bio")) %>% 
    melt(id.vars = "datetime",
         variable.name="source",
         value.name="usage") %>% 
    group_by(source) %>% 
    summarize(mean_usage=mean(usage,na.rm=TRUE))

### DATA.TABLE (dt[<row filtering>, <column operations>, by {think group_by}])

library(data.table)
data_file <- here::here("data","ca_energy_generation.csv")
# read in two versions of data, one as a data.frame and one as a data.table
generation_df <- read.csv(data_file, stringsAsFactors = F)
generation_dt <- fread(data_file)
class(generation_df)
class(generation_dt)
View(generation_df)
View(generation_dt)
str(generation_df)
str(generation_dt)

generation_dt[, total_hydro := small_hydro+large_hydro]
generation_dt[,.(mean(nuclear),mean(biogas))]
#When using .( ) i.e. a dot with commas after it, this refers to exporting data
#On the other hand, if we use := i.e. a column followed by a equal sign, we will be performing actions in place

generation_dt[solar==0, .(datetime, total_thermal=natural_gas+coal)]
generation_dt[solar >0 , max(natural_gas) ,  mday(datetime)]

all_generation_long[, day := as_date(datetime)]
all_generation_long[,log_output := log(value)]
all_generation_long[,per_output:= value/sum(value), by = day]

---

#We rename the "source" column in long_merged_data so we can easily merge with regroup data.
names(long_merged_data)[2]="type"
