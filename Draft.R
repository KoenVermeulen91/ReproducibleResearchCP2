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
hist(storms2000$FATALITIES)
plot(storm$WIDTH[storm$WIDTH != 0])
types <- as.data.frame(table(storms2000$EVTYPE))
types <- filter(types, Freq != 0)

# Converting dates

# Filtering latest years (2000)

summary(storm$BGN_DATE)
sum(is.na(storm$BGN_DATE))
sum(is.na(storm$END_DATE))
sum(storm$BGN_DATE == "")

# Datum hervormen
storm$begin_date_char <- as.character(storm$BGN_DATE)
storm$begin_date_nchar <- nchar(storm$begin_date_char)
storm$begin_date_char <- str_sub(storm$begin_date_char, 
                            start = 1, 
                            end = storm$begin_date_nchar - 8)
storms$begin_date <- as.Date(storms$begin_date, tryFormats = "%m/%d/%Y")
storms$begin_date_year <- year(storms$begin_date)
storms$begin_date_month <- month(storms$begin_date)
storms$begin_date_day <- day(storms$begin_date)

dates <- select(storms, BGN_DATE, begin_date, begin_date_year, begin_date_month, begin_date_day)
head(dates)

head(storm$begin_date)

storms2000$end_date_char <- as.character(storms2000$END_DATE)
storms2000$end_date_nchar <- nchar(storms2000$end_date_char)
storm$begin_date_char <- str_sub(storm$begin_date_char, 
                                 start = 1, 
                                 end = storm$begin_date_nchar - 8)


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

table(storms$PROPDMGEXP)


class(storm$BGN_DATE)
begin_dates <- str_extract(storm$begin_date, " ")


begin_dates_split <- str_split_fixed(begin_dates, pattern = " ")

# omzetten naar posix
storm$end_date <- as.character(storm$BGN_DATE)
storm$end_date_char <- nchar(storm$end_date)
summary(storm$end_date_char)

types <- storms2000dmg %>%
        group_by(EVTYPE) %>%
        summarize(DMG_SUM = sum(DMGtotal),
                  DMG_MAX = max(DMGtotal),
                  DMG_AVG = mean(DMGtotal),
                  PROPDMG_SUM = sum(PROPDMGtotal),
                  PROPDMG_MAX = max(PROPDMGtotal),
                  PROPDMG_AVG = mean(PROPDMGtotal),
                  CROPDMG_SUM = sum(CROPDMGtotal),
                  CROPDMG_MAX = max(CROPDMGtotal),
                  CROPDMG_AVG = mean(CROPDMGtotal),
                  HARMED_SUM = sum(HARMEDtotal),
                  HARMED_MAX = max(HARMEDtotal), 
                  HARMED_AVG = mean(HARMEDtotal),
                  INJURIES_SUM = sum(INJURIES),
                  INJURIES_MAX = max(INJURIES), 
                  INJURIES_AVG = mean(INJURIES),
                  FATALITIES_SUM = sum(FATALITIES),
                  FATALITIES_MAX = max(FATALITIES), 
                  FATALITIES_AVG = mean(FATALITIES))

harm <- types %>%
        top_n(n = 5, wt = HARMED_SUM) %>%
        ggplot(aes(y = HARMED_SUM, x = reorder(x = EVTYPE, X = HARMED_SUM), fill=EVTYPE)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        labs(title = "Top 5 most harmful event types", 
             x = "", y = "Number of harmed people involved") +
        coord_flip() + 
        theme_light()
harm



