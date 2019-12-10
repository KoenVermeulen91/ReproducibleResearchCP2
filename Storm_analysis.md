---
title: "Storm analysis"
subtitle: "Which storm types have the biggest impact on population health and the economy?"
author: "Koen Vermeulen"
date: "7-12-2019"
output: 
        html_document:
                keep_md: true
---



# Summary
This report contains a storm analysis. It answers two questions: 

1. Across the United States, which types of events are most harmful with respect to population health?

2. Across the United States, which types of events have the greatest economic consequences?

After some data processing, the questions are answered by two bar chart visualisations. The most harmful type of events shows to be tornado's. The event type with the most economic damages is a flood.

## Loading necessary libraries


```r
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
```

## Data Processing

### Downloading the data

If the data is not already in the working directory, it is downloaded from the correct URL.


```r
filename <- 'storms.csv.bz2'
if (!file.exists(filename)) {
  download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2', filename)
}
storms <- read.csv(filename)
```

### Correcting dates

In the documentation on the dataset it was mentioned that the data was properly collected after 1996. This report focussed on data from 1996 and later. Therefore it was necessary to properly store the BGN_DATE variable. 


```r
storms <- storms %>%
        mutate(begin_date_char = as.character(storms$BGN_DATE)) %>%
        mutate(begin_date_nchar = nchar(begin_date_char)) %>%
        mutate(begin_date = as.Date(str_sub(begin_date_char, 
                                            start = 1,
                                            end = begin_date_nchar - 8), 
                                    tryFormats = "%m/%d/%Y")) %>%
        mutate(begin_date_year = year(begin_date))
```

### Reducing data to recent times

After correcting the date variable it was possible to filter the data to 1996 and later.


```r
storms1996 <- filter(storms, begin_date_year >= 1996)
range(storms1996$begin_date)
```

```
## [1] "1996-01-01" "1999-12-31"
```

```r
sapply(storms1996, function(x) sum(is.na(x)))
```

```
##          STATE__         BGN_DATE         BGN_TIME        TIME_ZONE 
##                0                0                0                0 
##           COUNTY       COUNTYNAME            STATE           EVTYPE 
##                0                0                0                0 
##        BGN_RANGE          BGN_AZI       BGN_LOCATI         END_DATE 
##                0                0                0                0 
##         END_TIME       COUNTY_END       COUNTYENDN        END_RANGE 
##                0                0           120954                0 
##          END_AZI       END_LOCATI           LENGTH            WIDTH 
##                0                0                0                0 
##                F              MAG       FATALITIES         INJURIES 
##           115827                0                0                0 
##          PROPDMG       PROPDMGEXP          CROPDMG       CROPDMGEXP 
##                0                0                0                0 
##              WFO       STATEOFFIC        ZONENAMES         LATITUDE 
##                0                0                0                0 
##        LONGITUDE       LATITUDE_E       LONGITUDE_          REMARKS 
##                0                0                0                0 
##           REFNUM  begin_date_char begin_date_nchar       begin_date 
##                1                0                0                0 
##  begin_date_year 
##                0
```

### Revalue and combining damages & combining harm

The damages (property and crop) were reported in the dataset in seperate columns: 

1. PROPDMG entered as actual dollar amount.

2. PROPDMGEXP containing the magnitude ("K" for thousands, "M" for millions & "B" for billions)

In order to analyse the damages the damages were multiplied by the magnitude for both property and crop damages. After that the two total columns were summed to assess the total economic impact.

The harmful impact on the population was reported in the dataset by two variables, INJURIES & FATALITIES. In order to analyse the harm inflicted by the types of events, the two variables were added together. 


```r
storms1996dmg <- storms1996 %>%
        mutate(PROPDMGEXPnum = 1) %>%
        mutate(CROPDMGEXPnum = 1) %>%
        mutate(PROPDMGEXPnum = ifelse(PROPDMGEXP == "K", 1000, PROPDMGEXPnum)) %>%
        mutate(CROPDMGEXPnum = ifelse(CROPDMGEXP == "K", 1000, CROPDMGEXPnum)) %>%
        mutate(PROPDMGEXPnum = ifelse(PROPDMGEXP == "M", 1000000, PROPDMGEXPnum)) %>%
        mutate(CROPDMGEXPnum = ifelse(CROPDMGEXP == "M", 1000000, CROPDMGEXPnum)) %>%
        mutate(PROPDMGEXPnum = ifelse(PROPDMGEXP == "B", 1000000000, PROPDMGEXPnum)) %>%
        mutate(CROPDMGEXPnum = ifelse(CROPDMGEXP == "B", 1000000000, CROPDMGEXPnum)) %>%
        mutate(PROPDMGtotal = PROPDMG * PROPDMGEXPnum) %>%
        mutate(CROPDMGtotal = CROPDMG * CROPDMGEXPnum) %>%
        mutate(DMGtotal = PROPDMGtotal + CROPDMGtotal) %>%
        mutate(HARMEDtotal = INJURIES + FATALITIES)
```

### Grouping damages and harming impact by event type

To assess damages and harm in relation to the types of events, the dataset was grouped by the event type. For the damages and harm the sum was summarized in this group by.


```r
types <- storms1996dmg %>%
        group_by(EVTYPE) %>%
        summarize(DMG_SUM = sum(DMGtotal),
                  HARMED_SUM = sum(HARMEDtotal))
```

## Results

### Question 1: Across the United States, which types of events are most harmful with respect to population health?

The barchart below shows that floods are the most harmful with respect to the population health.


```r
harm <- types %>%
        top_n(n = 5, wt = HARMED_SUM) %>%
        ggplot(aes(y = HARMED_SUM, x = reorder(x = EVTYPE, X = HARMED_SUM), fill=EVTYPE)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        labs(title = "Top 5 most harmful event types", 
             x = "", y = "Number of harmed people involved") +
        coord_flip() + 
        theme_light()
harm
```

![](Storm_analysis_files/figure-html/results question 1-1.png)<!-- -->

### Question 2: Across the United States, which types of events have the greatest economic consequences?

The barchart below shows that hurricanes have the greatest economic consequences. 


```r
damage <- types %>%
        top_n(n = 5, wt = DMG_SUM) %>%
        ggplot(aes(y = DMG_SUM, x = reorder(x = EVTYPE, X = DMG_SUM), fill=EVTYPE)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        labs(title = "Top 5 event types with the greatest economic consequences",
             x = "", y = "Total economic damages") +
        coord_flip() +
        theme_light()
damage
```

![](Storm_analysis_files/figure-html/results question 2-1.png)<!-- -->

