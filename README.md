#NYPD Shooting Data Analysis
Taking data from data.gov and performing analysis on New York shootings from 2006 to 2020

## 1. Importing Data
Obtain the NYPD Shooting Incident Data (historic) from Data.gov at the following url:

https://data.cityofnewyork.us/api/views/833y-fsy8/rows.csv?accessType=DOWNLOAD

Read using '<read_csv()>'...

```{r import data}
library(tidyverse)
library(tidyr)
library(stringr)
library(dplyr)
url = "https://data.cityofnewyork.us/api/views/833y-fsy8/rows.csv?accessType=DOWNLOAD"
NYPD_shooting_data = read_csv(url)
```

```{r initial data}
NYPD_shooting_data
```
## 2. Tidy Data

Time to tidy up the data and make it more "R" friendly. I will change the OCCUR_DATE into a "date" object and arrange the data by the following priorities OCCUR_DATE, OCCUR_TIME, BORO, PRECINCT, LOCATION_DESC, VIC_AGE_GROUP, VIC_SEX, VIC_RACE

Additionally, I will remove some columns with data we are not interested in (i.e. INCIDENT_KEY, JURISDICTION_CODE, STATISTICAL_MURDER_FLAG, X_COORD_CD, Y_COORD_CD, Latitude, Longitude, Lon_Lat).  Looking at the location descriptions, there were a lot of observations that were missing this information. If I wanted to use this characteristic in my analysis, the "NA" entries would not serve me much use so I would get rid of them by using the `<na.omit()>` function. For this analysis, I decided to focus on observations as they relate to the Victim's characteristics.

I started with cleaning up the Victim's age range column by splitting the column into  numeric low and high end points. I also decided to further categorize the location description column since I noticed a common primary location description listed as "MULTI DWELL" and differing secondary location descriptions. This appeared to be a good opportunity to split up that data.

```{r tidy data}
library(lubridate)
NYPD_shooting_data = mutate(NYPD_shooting_data, OCCUR_DATE = mdy(OCCUR_DATE)) %>%
  arrange(OCCUR_DATE,OCCUR_TIME,BORO,PRECINCT, LOCATION_DESC, VIC_AGE_GROUP, VIC_SEX, VIC_RACE) %>%
  select(-c(INCIDENT_KEY,JURISDICTION_CODE,STATISTICAL_MURDER_FLAG,X_COORD_CD,Y_COORD_CD, Latitude, Longitude, Lon_Lat, PERP_AGE_GROUP, PERP_SEX, PERP_RACE)) %>%
  separate(LOCATION_DESC, into = c("Primary_loc","Secondary_loc"), sep = "-")
  
NYPD_shooting_data$VIC_AGE_GROUP = sub("65\\+", "65-100", NYPD_shooting_data$VIC_AGE_GROUP)
NYPD_shooting_data$VIC_AGE_GROUP = sub("<", "0-", NYPD_shooting_data$VIC_AGE_GROUP )
NYPD_shooting_data = separate(NYPD_shooting_data, VIC_AGE_GROUP, into = c("VicLowAgeRange","VicHighAgeRange"), sep = "-")
NYPD_shooting_data$VicLowAgeRange = as.numeric(NYPD_shooting_data$VicLowAgeRange)
NYPD_shooting_data$VicHighAgeRange = as.numeric(NYPD_shooting_data$VicHighAgeRange)
```

```{r long data}
NYPD_shooting_data
```
Printing summary of data...
```{r summary of data}
summary(NYPD_shooting_data)
```
As expected, the only numerical data produced from tidying up is the date, the precinct the shootings were reported to, and the victim's age ranges. We can see the data ranges from 2006 to 2020, indicating this data goes back ~14 years. A majority of the data falls under the character category and will require more analysis.

## 3. Visualizing Data
To start visualizing the data, I decided to group the data 3 different ways: by the boroughs, victim's race,the victim's sex. I was curious to see the count of victims of different races, sexes, and boroughs. I also took the average age low and high end points of the victims. Additionally, I grouped the data ordered by borough then by victim race.

```{r visualize data}
total = nrow(NYPD_shooting_data)
NYPD_BORO_analyze = NYPD_shooting_data %>% 
  group_by(BORO) %>%
  summarize(countB = n(), perc_shootingB = (countB/total)*100) %>%
  select(BORO, countB, perc_shootingB) %>%
  ungroup() %>%
  mutate_if(is.numeric, round, digits = 2)

NYPD_Race_analyze = NYPD_shooting_data %>% 
  group_by(VIC_RACE)%>% 
  summarize(countR = n(), perc_shootingR = (countR/total*100) , Avg_LowVicAge = mean(VicLowAgeRange), Avg_HighVicAge = mean(VicHighAgeRange)) %>%
  select(VIC_RACE, countR, perc_shootingR, Avg_LowVicAge, Avg_HighVicAge) %>%
  ungroup() %>% 
  mutate_if(is.numeric, round, digits = 2)

NYPD_Sex_analyze = NYPD_shooting_data %>% 
  group_by(VIC_SEX)%>% 
  summarize(countS = n(), perc_shootingS = (countS/total*100) , Avg_LowVicAge = mean(VicLowAgeRange), Avg_HighVicAge = mean(VicHighAgeRange)) %>%
  select(VIC_SEX, countS, perc_shootingS, Avg_LowVicAge, Avg_HighVicAge) %>%
  ungroup() %>% 
  mutate_if(is.numeric, round, digits = 2)

NYPD_BORO_VICRACE_analyze = NYPD_shooting_data %>% 
  group_by(BORO,VIC_RACE) %>% 
  summarize(countBV = n(), perc_shootingB = (countBV/total*100)) %>% 
  select(BORO, VIC_RACE, countBV, perc_shootingB) %>% 
  ungroup() %>%  
  mutate_if(is.numeric, round, digits = 2)

```

```{r visualize data table}
NYPD_BORO_analyze
NYPD_Race_analyze
NYPD_Sex_analyze
```

```{r inital plot}
bpBoro = NYPD_BORO_analyze %>%
  ggplot(aes(x = "", y = perc_shootingB, fill = BORO))+geom_bar(width = 1, stat = "identity") 
bpBoro + coord_polar("y", start = 0) + theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank()) +ggtitle("NYPD reported shootings by Borough") +
  xlab(element_blank()) + ylab(element_blank()) + geom_text(aes(label = scales::percent(perc_shootingB/100)),position = position_stack(vjust = 0.5))

bpRace = NYPD_Race_analyze %>%
  ggplot(aes(x = "", y = perc_shootingR, fill = VIC_RACE))+geom_bar(width = 1, stat = "identity") 
bpRace + coord_polar("y", start = 0) +theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank()) +ggtitle("NYPD reported shootings by Race") +
  xlab(element_blank()) + ylab(element_blank())+geom_text(aes(label = scales::percent(perc_shootingR/100)),position = position_stack(vjust = 0.5))  


bpSex = NYPD_Sex_analyze %>%
  ggplot(aes(x = "", y = perc_shootingS, fill = VIC_SEX))+geom_bar(width = 1, stat = "identity") 
bpSex + coord_polar("y", start = 0) +theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank()) +ggtitle("NYPD reported shootings by Sex") +
  xlab(element_blank()) + ylab(element_blank()) + geom_text(aes(label = scales::percent(perc_shootingS/100)),position = position_stack(vjust = 0.5))

```
![Uploading Screen Shot 2022-07-17 at 11.21.04 PM.png…]()

Now that we have had a chance to see some preliminary visualizations, some questions arise about the different demographics and their relations. I was mainly interested in the relationship of the victim's race as it pertained to the borough in which the shootings occurred. How much did the race distribution of each borough reflect the demographic of the population of each borough or of New York? If they didn't match, does that mean that a particular race is being targeted more frequently in specific boroughs? 

Filtering out the data to show one victim race's distribution throughout different boroughs in descending count order yields us...
```{r race throughout boroughs}
NYPD_BORO_VICRACE_analyze %>% filter(VIC_RACE == "BLACK")%>% arrange(desc(countBV))
NYPD_BORO_VICRACE_analyze %>% filter(VIC_RACE == "WHITE HISPANIC") %>% arrange(desc(countBV))
NYPD_BORO_VICRACE_analyze %>% filter(VIC_RACE == "BLACK HISPANIC") %>% arrange(desc(countBV))
NYPD_BORO_VICRACE_analyze %>% filter(VIC_RACE == "WHITE") %>% arrange(desc(countBV))
NYPD_BORO_VICRACE_analyze %>% filter(VIC_RACE == "ASIAN / PACIFIC ISLANDER") %>% arrange(desc(countBV))
```
Filtering out the data to show one boroughs victim racial distribution in descending count order yields us...
```{r boroughs racial distribution}
NYPD_BORO_VICRACE_analyze %>% filter(BORO == "BROOKLYN") %>% arrange(desc(countBV))
NYPD_BORO_VICRACE_analyze %>% filter(BORO == "BRONX") %>% arrange(desc(countBV))
NYPD_BORO_VICRACE_analyze %>% filter(BORO == "MANHATTAN") %>% arrange(desc(countBV))
NYPD_BORO_VICRACE_analyze %>% filter(BORO == "QUEENS") %>% arrange(desc(countBV))
NYPD_BORO_VICRACE_analyze %>% filter(BORO == "STATEN ISLAND") %>% arrange(desc(countBV))
```
From this analysis, we can see each of the boroughs distribution of victim's races more or less resembles the population distribution, in terms of descending percentages of shootings.Since the shooting population distribution of race across each borough is consistent, it would not seem that any one race is being targeted in a particular borough. That being said, I cannot conclude that one particular race is not being targeted in general because looking across the board, the highest percentage of shootings falls under victims who are black.

Interestingly, when looking at one race across the different boroughs we can see each race has a different borough in which it has the most victims. This again led me to question whether this was reflective of the demographic population of each borough i.e. do more Asian/Pacific Islander people live in Queens vs other boroughs. Those questions would likely need additional New York population data to answer.

## 4. Model Data
Taking a different perspective, I decided to see if I could model a linear relationship between time and the shootings that occur in each borough. Additionally, since we have seen from our previous analysis that the highest number of victims generally are black individuals, I wanted to futher specify this model for shootings with black victims in Staten Island.  For this example, I grouped the NYPD shooting data by "OCCUR_DATE", "BORO", and "VIC_RACE". I then filtered out the single boro of interest ("STATEN ISLAND") and the instances when there was more than one shooting to normalize the data set. From there, I calculated a linear model relating the shooting counts and the date and added that column as the "prediction" column to the data set. Finally, I plotted the Staten Island shootings over time with black victims (purple) with its predictive linear model(green). 
``` {r creating a model}
NYPD_modelB = NYPD_shooting_data %>%
  group_by(OCCUR_DATE, BORO, VIC_RACE) %>% 
  summarize(countDB = n()) %>%
  select(OCCUR_DATE, BORO,VIC_RACE, countDB) %>%
  ungroup()
NYPD_modelB_SI = NYPD_modelB %>% 
  filter(BORO == "STATEN ISLAND") %>% 
  filter(VIC_RACE == "BLACK") %>%
  filter(countDB >1)
mod = lm(countDB ~ OCCUR_DATE, data = NYPD_modelB_SI)
NYPD_modelB_SI = NYPD_modelB_SI %>% mutate(pred = predict(mod))
NYPD_modelB_SI %>% ggplot() + geom_point(aes(x = OCCUR_DATE, y = countDB), color = "purple") + geom_point(aes(x = OCCUR_DATE, y = pred), color = "green") + ggtitle("NYPD reported shootings in Staten Island with Black Victims") +
  xlab("Date of Shooting") + ylab("Count") 
  
```
From this model, there seems to be a downward trend which indicates that the shootings have been going down over time. Additional questions that come out of this analysis include:
* Have new community actions been put in place that could be contributing to less shootings?
* Have new laws restricting gun ownership been implemented in this time frame?
* Is there a significantly larger police presence since earlier in this time frame?
* How have the demographics of the borough changed over time?
* How many of these shootings were police related?

This modeling frame work can be applied to all the other boroughs to see if the trend is consistent throughout NYC. 

## 5. Conclusions
Although there are many additional directions of analysis that one could go with this data set, some conclusions can be drawn with the ones performed. Based on the initial single variable analysis, the highest amount of shootings occur in Brooklyn with black male victims. Breaking down the data set by boroughs, the borough population distribution of victims based on race is consistent with the over all population distribution of victims which would suggest that there isn't a novel targeted race in a particular borough. However, when the data set is broken down by each race and it's distribution throughout the different boroughs, this does not always match the over all population distribution. This could be indicative of the overall demographic of the boroughs (i.e higher percentage of one race reside in a particular borough and therefore have a higher shooting distribution in comparison to the over all population distribution). Additionally, the linear modelling performed on the amount of shootings with black victims over time indicated possible decrease over time in Staten Island, but remained relatively constant when applying that model to any other borough, any other race, or to the general data set. This indicates the number of shootings has remained pretty consistent over the years.

Some biases could have come straight away from the beginning in what variables I chose to analyze, most notably the victim's race. Especially in the political climate of today, race is a very heavy topic to speak on but I also think that analyzing data knowing this allows us to shed light and give concrete evidence to speak to these topics. Additional biases could have come from the outside influence of the recent media coverage of shooting victims who are black justifying the acceptance of the shooting percentage breakdown by race. This was mitigated by doing multiple modes of analysis to confirm the percentage breakdown by race was consistent and accurate throughout each mode.

```{r sessionInfo, echo = FALSE}
sessionInfo()
```
