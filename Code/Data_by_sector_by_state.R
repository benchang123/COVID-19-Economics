library(tidyr)

#Read data
all_data <- read.delim('https://download.bls.gov/pub/time.series/sm/sm.data.1.AllData')

#Create index of series 
index <- read.delim('https://download.bls.gov/pub/time.series/sm/sm.series') %>% 
  dplyr::filter(seasonal=='S', area_code == 00000, data_type_code == 01, 
         industry_code == 1000000*supersector_code, state_code <= 56) %>%
  dplyr::select(c(series_id,state_code,supersector_code))
states_index <- read.delim('https://download.bls.gov/pub/time.series/sm/sm.state')
supersector_index <- read.delim('https://download.bls.gov/pub/time.series/sm/sm.supersector')


index <- index %>% merge(states_index, by='state_code') %>% merge(supersector_index, by='supersector_code') %>%
  select(series_id, state_name, supersector_name)


#Filtered data
series <- unique(index[,1])
filtered_data <- all_data %>% filter(series_id %in% series) %>% merge(index, by='series_id') %>% 
  mutate(month = substr(period,2,3)) %>%
  filter(year >= 2019) %>%
  select(value, month, year, state_name, supersector_name)

head(filtered_data)
tail(filtered_data)
