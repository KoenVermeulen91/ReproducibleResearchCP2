### DRAFT SCRIPT ###


# Opening libraries
library(dplyr)
library(stringr)
library(lubridate)
## if statement
## unzip

# Reading storm data
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "storms.csv.bz2")
storms <- read.csv("storms.csv.bz2")



## Across the United States, which types of events (as indicated in the EVTYPE variable) are 
# most harmful with respect to population health?

## Across the United States, which types of events have the greatest economic consequences?


names(storm)
summary(storm)
str(storm)
hist(storm$WIDTH)
hist(storm$LENGTH)
hist(storm$F)
hist(storm$INJURIES)
hist(storm$FATALITIES)
plot(storm$WIDTH[storm$WIDTH != 0])
types <- as.data.frame(table(storm$EVTYPE))
types <- ordered(types$Freq)

# Converting dates

# Filtering latest years (2000)

summary(storm$BGN_DATE)
sum(is.na(storm$BGN_DATE))
sum(is.na(storm$END_DATE))
sum(storm$BGN_DATE == "")

# Datum hervormen
storm$begin_date_char <- as.character(storm$BGN_DATE)
storm$begin_date_nchar <- nchar(storm$begin_date_char)
storm$begin_date <- str_sub(storm$begin_date_char, 
                            start = 1, 
                            end = storm$begin_date_nchar - 8)

head(storm$begin_date)

# Datum class nog toevoegen

summary(storm$begin_date_nchar)
table(storm$begin_date_nchar)

dataformat16 <- filter(storm, begin_date_nchar == 16)
head(dataformat16$BGN_DATE)
tail(dataformat16$BGN_DATE)
dataformat17 <- filter(storm, begin_date_nchar == 17)
head(dataformat17$BGN_DATE)
tail(dataformat17$BGN_DATE)
dataformat18 <- filter(storm, begin_date_nchar == 18)
head(dataformat18$BGN_DATE)
tail(dataformat18$BGN_DATE)


# splitten op spatie
storm$begin_date_short <- 

storm$begin_date <- as.Date(storm$BGN_DATE, "%m/%d/%yyyy")
storm$begin_date



class(storm$BGN_DATE)
begin_dates <- str_extract(storm$begin_date, " ")


begin_dates_split <- str_split_fixed(begin_dates, pattern = " ")

# omzetten naar posix
storm$end_date <- as.character(storm$BGN_DATE)
storm$end_date_char <- nchar(storm$end_date)
summary(storm$end_date_char)



