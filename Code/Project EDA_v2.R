library(tidyr)
library(dplyr)
library(lubridate)
library(zoo)

#Working directory
wd <- dirname(rstudioapi::getSourceEditorContext()$path) #get rmd file path
wd <- ifelse(substr(wd,nchar(wd)-3,nchar(wd)) == 'Code', substr(wd,1,nchar(wd)-5), wd)
setwd(wd) #set working directory

######################################
######## Employment data #############
######################################

# Source: Bureau of Labor Statistics
# Link: https://download.bls.gov/pub/time.series/sm/sm.data.1.AllData
# Series description: The original series are the SA thousand of employees at each month by sector and state

#Read data
all_data <- read.delim('https://download.bls.gov/pub/time.series/sm/sm.data.1.AllData')

#Create index of series 
index <- read.delim('https://download.bls.gov/pub/time.series/sm/sm.series') %>% 
  filter(seasonal=='S', area_code == 00000, data_type_code == 01, 
         industry_code == 1000000*supersector_code, state_code <= 56) %>%
  select(c(series_id,state_code,supersector_code)) %>%
  filter(supersector_code %in% c(10, 20, 30, 41, 42, 43, 50, 55, 60, 65, 70, 80, 90))
states_index <- read.delim('https://download.bls.gov/pub/time.series/sm/sm.state')
supersector_index <- read.delim('https://download.bls.gov/pub/time.series/sm/sm.supersector')


index <- index %>% merge(states_index, by='state_code') %>% merge(supersector_index, by='supersector_code') %>%
  select(series_id, state_name, supersector_name)


##Filter data
series <- unique(index[,1])
filtered_data <- all_data %>% filter(series_id %in% series) %>% merge(index, by='series_id') %>% 
  mutate(month = substr(period,2,3)) %>%
  filter(year >= 2019) %>%
  select(value, month, year, state_name, supersector_name)


##Transform data

#Obtain monthly change and drop off 2019 (jan-nov)
employment_data <- filtered_data %>% mutate(emp_monthly_change = as.numeric(value)/lag(as.numeric(value))-1) %>%
  mutate(year_month = paste0(year,month)) %>%
  filter(year_month >= 201912) %>%
  select(value, emp_monthly_change, month, year, state_name, supersector_name)

#Built date variable
dates <- as.Date(as.yearmon(paste(employment_data$year, employment_data$month, sep='-')), frac = 1)
employment_data <- employment_data %>% mutate(date = dates) %>% select(date, value, emp_monthly_change, state_name, supersector_name) 

colnames(employment_data)[2] <- 'employees'

head(employment_data)
tail(employment_data)


##########################
##### Mobility data ######
##########################

# Source: Google

#Read data
mobility <- read.csv('Data/2020_US_Region_Mobility_Report.csv', header = T, na.strings ="") %>% 
  mutate(date = as.Date(date)) %>% filter(is.na(sub_region_2) & !is.na(sub_region_1)) %>% 
  select(c(date, sub_region_1, retail_and_recreation_percent_change_from_baseline,
           grocery_and_pharmacy_percent_change_from_baseline, parks_percent_change_from_baseline,
           transit_stations_percent_change_from_baseline, workplaces_percent_change_from_baseline,
           residential_percent_change_from_baseline)) %>% 
  'colnames<-'(c('date','state_name','Mob_ret_rec','Mob_gro_pha', 'Mob_parks','Mob_transit','Mob_work','Mob_res'))


#Seasonal adjusted daily data (only taking residential mobility)

mobility <- mobility %>% select(date, state_name, Mob_res) %>% mutate(Mob_res_SA = NA)

## Example for Alabama (weekly seasonal adjusted)
example <- mobility %>% filter(state_name == 'Alabama') %>% select(date, Mob_res)
ts <- ts(example$Mob_res, frequency = 7, start = c(2020,as.numeric(format(as.Date('2020-02-15'), "%j"))))
plot(ts)
decompose_ts <- decompose(ts, 'additive')
plot(decompose_ts)
adjust_ts <- ts - decompose_ts$seasonal
plot(adjust_ts)

## Do for all data
states <- unique(mobility$state_name)
for(i in 1:length(states)){
  state <- states[i]
  
  state_data <- mobility %>% filter(state_name == state) %>% select(date, Mob_res)
  ts <- ts(state_data$Mob_res, frequency = 7, start = c(2020,as.numeric(format(as.Date('2020-02-15'), "%j"))))
  decompose_ts <- decompose(ts, 'additive')
  adjust_ts <- ts - decompose_ts$seasonal
  
  mobility[mobility$state_name == state,]$Mob_res_SA <- adjust_ts
}

#Computing 30 days change (to obtain the monthly change)
mobility <- mobility %>% mutate(delta_Mob_res_SA = NA)
states <- unique(mobility$state_name)
for(i in 1:length(states)){
  state <- states[i]
  
  mob <- mobility %>% filter(state_name == state) %>% select(Mob_res_SA)
  delta_mob <- unname(unlist(mob/lag(mob, n=30)-1))
  
  mobility[mobility$state_name == state,]$delta_Mob_res_SA <- delta_mob
}

unique(employment_data$state_name)
unique(mobility$state_name)
unique(stringency_us$state_name)

head(mobility)
tail(mobility)


#########################
#### Stringency data ####
#########################

#load data
stringency_us <- read.csv("https://raw.githubusercontent.com/OxCGRT/USA-covid-policy/master/data/OxCGRT_US_latest.csv")
stringency_us <- stringency_us %>% select(Date, RegionName, StringencyIndex) %>% filter(RegionName != '')

#Modify name of one state
stringency_us[stringency_us$RegionName == 'Washington DC',]$RegionName <- rep('District of Columbia', length(stringency_us[stringency_us$RegionName == 'Washington DC',]$RegionName))

#Convert dates
stringency_us <- stringency_us %>% mutate(date = as.Date(as.character(Date), '%Y%m%d')) %>% 
  select(date, RegionName, StringencyIndex) %>% 
  'colnames<-'(c('date','state_name', 'stringency_index'))

#Compute monthly change of stringency index
stringency_us <- stringency_us %>% mutate(delta_stringency_index = NA)
states <- unique(stringency_us$state_name)
for(i in 1:length(states)){
  state <- states[i]
  
  str <- stringency_us %>% filter(state_name == state) %>% select(stringency_index)
  delta_str <- unname(unlist(str - lag(str, n=30)))
  
  stringency_us[stringency_us$state_name == state,]$delta_stringency_index <- delta_str
}


head(stringency_us)
tail(stringency_us)


#########################
##### Merge all data ####
#########################

#Merge employment and mobility

data <- merge(employment_data, mobility, by.x=c('date','state_name'))
data <- merge(data, stringency_us, by.x = c('date','state_name'))

head(data)
tail(data)

