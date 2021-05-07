knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
library(dplyr)
library(stringr)
library(gridExtra)
library(reshape2)
library(readxl)
library(lubridate)
library(zoo)

#Obtain working directory
wd <- dirname(rstudioapi::getSourceEditorContext()$path) #get rmd file path
wd <- ifelse(substr(wd,nchar(wd)-3,nchar(wd)) == 'Code', substr(wd,1,nchar(wd)-5), wd)
setwd(wd) #set working directory

### Unemployment per state data ###
state_data <- read.delim('https://download.bls.gov/pub/time.series/la/la.data.3.AllStatesS')

#Create index of series 
index <- read.delim('https://download.bls.gov/pub/time.series/la/la.series') %>% 
  filter(seasonal=='S'& grepl('LASST', series_id) & measure_code == 3) %>%
  select(c(series_id,series_title,srd_code))
states_index <- read.delim('https://download.bls.gov/pub/time.series/la/la.state_region_division')

index <- index %>% merge(states_index)

#Specify states
states <- c('California','New York', 'Nevada', 'Florida', 'Hawaii')
series <- index %>% filter(srd_text %in% states) %>% select(series_id, srd_text)

#Obtain specific data series
state_data_sp <- state_data %>% filter(series_id %in% unlist(series), year >= 2019) %>% 
  merge(series) %>%
  select(c(srd_text,year, period, value))

#Date format
dates <- as.Date(paste0(state_data_sp$year,substr(state_data_sp$period, 2,3),'01'),format='%Y%m%d')

state_data_sp <- state_data_sp %>% mutate(date = dates) %>%
  select(c(date, srd_text, value)) %>%
  'colnames<-'(c('Date','State','UR'))

### Mobility data ###
mobility <- read.csv('Data/2020_US_Region_Mobility_Report.csv', header = T, na.strings ="") %>% 
  mutate(date = as.Date(date)) %>% filter(sub_region_1 %in% states & is.na(sub_region_2)) %>% 
  select(c(date, sub_region_1, retail_and_recreation_percent_change_from_baseline,
           grocery_and_pharmacy_percent_change_from_baseline, parks_percent_change_from_baseline,
           transit_stations_percent_change_from_baseline, workplaces_percent_change_from_baseline,
           residential_percent_change_from_baseline)) %>% 
  'colnames<-'(c('Date','State','Mob_ret_rec','Mob_gro_pha', 'Mob_parks','Mob_transit','Mob_work','Mob_res'))

#Merge both data bases
merge_data <- left_join(mobility, state_data_sp, by = c('Date','State'), all.x = TRUE)

#Fill Unemployment Rates gaps
last_UR <- state_data_sp %>% filter(Date=='2020-02-01') %>% select(UR) %>% unlist() %>% as.numeric() #obtain last ur data
merge_data[merge_data$Date=='2020-02-15','UR'] <- last_UR #replace last UR data available for February 15, 2020
merge_data <- merge_data %>% mutate(UR = as.numeric(na.locf(UR))) #Replace NAs with previous values


## Plots

colors <- c("Unemployment Rate" = "blue", "Mobility. Residential" = "red", 'Mobility. Retail and Recreation' = "black",
            'Mobility. Grocery and Pharmacies'='darkgreen')

#California
merge_data %>% filter(State=='California') %>% 
  ggplot(aes(x=Date)) + 
  geom_line(aes(y=UR, color='Unemployment Rate'), size=1)+
  geom_line(aes(y=Mob_ret_rec, color='Mobility. Retail and Recreation'), size=0.5)+
  labs(x = 'Date', y = '% (Unemployment rate) / Index (Mobility)',
       title = 'California. Unemployment rate (blue) and Mobility index (black)',
       color='Legend')+
  theme_bw()+
  scale_color_manual(values=colors)


#################

#load data
stringency_us <- read.csv("https://raw.githubusercontent.com/OxCGRT/USA-covid-policy/master/data/OxCGRT_US_latest.csv")
stringency_states <- stringency_us[stringency_us$RegionName!="",]
stringency_cali <- stringency_us[stringency_us$RegionName=='California',]

#convert to date format

stringency_cali$Date=as.Date(as.character(stringency_cali$Date), format = "%Y%m%d")

#plot (stringency by state)
ggplot(stringency_cali) +
  geom_line(aes(x=Date, y=StringencyIndex))+
  ylab("Stringency Index") +
  xlab("Date") +
  ggtitle("Stringency Level over Time (California)") +
  theme_bw()


#load
mobility=read.csv('Data/2020_US_Region_Mobility_Report.csv', header = T, na.strings ="")

#party of governor
demstates <- c("US-CA","US-CO","US-CT","US-DE","US-HI","US-IL","US-KS","US-KY","US-LA","US-ME","US-MI","US-MN","US-NV","US-NJ","US-NM","US-NY","US-NC","US-OR","US-PA","US-RI","US-VA","US-WA","US-WI")

for (i in 1:length(demstates)){
  mobility$governor[mobility$iso_3166_2_code == demstates[i]] <- "Dem."
} 
mobility$governor[is.na(mobility$governor)] <- "Rep."
mobility$governor[mobility$iso_3166_2_code=="US-DC" ] <- "D.C." 

#date
mobility$date=as.Date(mobility$date)

#drop NA in iso code
mobility=mobility[!is.na(mobility$iso_3166_2_code),]

#plot (mobility by state)
ggplot(mobility) +
  geom_path(aes(x=date, y=retail_and_recreation_percent_change_from_baseline, color=governor))+
  scale_color_manual(values=c("black", "blue", "red")) +
  ylab("%Change in Mobility from Baseline") +
  xlab("Date (2020.02.15 - 2021.02.23)") +
  ggtitle("Retail and Recreational Mobility by State") +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position=c(0.9,0.05), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  facet_wrap("iso_3166_2_code")

#compute mobility change average per state
mean_mobility <- mobility %>% group_by(iso_3166_2_code) %>%
  mutate(avg_retail_mob = mean(retail_and_recreation_percent_change_from_baseline, na.rm = TRUE)) %>%
  mutate(avg_res_mob = mean(residential_percent_change_from_baseline, na.rm = TRUE)) %>%
  slice(1) %>% #only take one entry per code
  ungroup()

#plot (recreation histogram)
ggplot(mean_mobility, aes(x=avg_retail_mob)) +
  geom_histogram(aes(color=governor), alpha = 0.2) +
  scale_color_manual(values=c("black", "blue", "red")) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position=c(0.11,0.8))+
  xlab("Average Retail Mobility % Change from Baseline") +
  ylab("Frequency") +
  ggtitle("Average Retail Mobility Statistics by State")

#plot (residential histogram)
ggplot(mean_mobility, aes(x=avg_res_mob )) +
  geom_histogram(aes(color=governor), alpha = 0.2) +
  scale_color_manual(values=c("black", "blue", "red")) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position=c(0.89,0.89))+
  xlab("Average Residential Mobility %Change from Baseline") +
  ggtitle("Average Residential Mobility Statistics by State")  


# EDA 5: Unemployment vs Mobility by Time and Sector

##########################################################################################

mobility=read.csv('Data/2020_US_Region_Mobility_Report.csv', header = T, na.strings ="")
sector=read.csv('Data/unemployment_sector_NSA.csv', header = T, na.strings ="")



#change column title of sector data
nameTitle=c('Date','Construction','Manufacturing','Durable Goods', 'Financial',
            'Leisure and Hospitality','Agricultural')

names(sector) <- nameTitle

#convert to date format
mobility$date=as.Date(mobility$date)
sector$Date=as.Date(sector$Date)

#subset data
usMob=mobility[is.na(mobility$sub_region_1),]

#mobility granularity day to month

changemob=rep(0, 11)
datearray=rep(0, 11)

for (i in 2:12){
  
  if (i<10){
    dates = paste0('2020-0',i)
    fordatearr=paste0('0',i)
  }
  else
  {
    dates = paste0('2020-',i)
    fordatearr=i
  }
  
  datearray[i-1]=fordatearr
  
  monthdf=subset(usMob, substr(date, 1, 7) %in% dates)
  
  changemob[i-1]=monthdf[nrow(monthdf),10]-monthdf[1,10]
}

##Visualization



#subset data
secdiv=sector[,2]

changesec=diff(secdiv)
changesec=changesec[-c(1,length(changesec))]

#subset data
secdiv2=sector[,6]

changesec2=diff(secdiv2)
changesec2=changesec2[-c(1,length(changesec2))]

#plot
print(ggplot() +
        geom_point(aes(x = changemob, y = changesec,color='Construction')) + 
        geom_text(aes(x = changemob, y = changesec, label=datearray),nudge_x = -1.5,size = 3) + 
        geom_point(aes(x = changemob, y = changesec2,color='Leisure')) + 
        geom_text(aes(x = changemob, y = changesec2, label=datearray),nudge_x = -1.5,size = 3) + 
        ylim(-5, 35)+
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = expression(Delta*'Change in Mobility (%)'), y = expression(Delta*' Sector Unemployment Rate (%)', color='Legend'),
             title = paste0('Sector Unemployment Rate vs Change in Mobility')))


##########################################################################################


### Unemployment per state data ###
state_data <- read.delim('https://download.bls.gov/pub/time.series/la/la.data.3.AllStatesS')

#Create index of series 
index <- read.delim('https://download.bls.gov/pub/time.series/la/la.series') %>% 
  filter(seasonal=='S'& grepl('LASST', series_id) & measure_code == 3) %>%
  select(c(series_id,series_title,srd_code))
states_index <- read.delim('https://download.bls.gov/pub/time.series/la/la.state_region_division')

index <- index %>% merge(states_index)

#Specify states
states <- c('California','Utah')
series <- index %>% filter(srd_text %in% states) %>% select(series_id, srd_text)

#Obtain specific data series
state_data_sp <- state_data %>% filter(series_id %in% unlist(series), year >= 2019) %>% 
  merge(series) %>%
  select(c(srd_text,year, period, value))

#Date format
dates <- as.Date(paste0(state_data_sp$year,substr(state_data_sp$period, 2,3),'01'),format='%Y%m%d')

state_data_sp <- state_data_sp %>% mutate(date = dates) %>%
  select(c(date, srd_text, value)) %>%
  'colnames<-'(c('Date','State','UR'))


mobilityd=read.csv('Data/2020_US_Region_Mobility_Report.csv', header = T, na.strings ="")

#change column title of sector data
nameTitle=c('California','Utah')

#convert to date format
mobilityd$date=as.Date(mobilityd$date)





#subset data by state
stateMob=mobilityd[is.na(mobilityd$sub_region_2),]
stateMob2=stateMob[stateMob$sub_region_1=='California',]

#mobility granularity day to month

changemob=rep(0, 11)
datearray=rep(0, 11)

for (j in 2:12){
  
  if (j<10){
    dates = paste0('2020-0',j)
    fordatearr=paste0('0',j)
  }
  else
  {
    dates = paste0('2020-',j)
    fordatearr=j
  }
  
  datearray[j-1]=fordatearr
  
  monthdf=subset(stateMob2, substr(date, 1, 7) %in% dates)
  
  changemob[j-1]=monthdf[nrow(monthdf),10]-monthdf[1,10]
}

##Visualization

#subset data
statediv=state_data_sp[state_data_sp$State=='California',]

changestate=diff(as.double(statediv$UR))
changeUR=tail(changestate,11)






#subset data by state
stateMob=mobilityd[is.na(mobilityd$sub_region_2),]
stateMob3=stateMob[stateMob$sub_region_1=='Utah',]

#mobility granularity day to month

changemob3=rep(0, 11)
datearray3=rep(0, 11)

for (j in 2:12){
  
  if (j<10){
    dates = paste0('2020-0',j)
    fordatearr=paste0('0',j)
  }
  else
  {
    dates = paste0('2020-',j)
    fordatearr=j
  }
  
  datearray3[j-1]=fordatearr
  
  monthdf=subset(stateMob3, substr(date, 1, 7) %in% dates)
  
  changemob3[j-1]=monthdf[nrow(monthdf),10]-monthdf[1,10]
}
##Visualization

#subset data
statediv3=state_data_sp[state_data_sp$State=='Utah',]

changestate3=diff(as.double(statediv3$UR))
changeUR3=tail(changestate3,11)



#plot
print(ggplot() +
        geom_point(aes(x = changemob, y = changeUR,color='California')) + 
        geom_text(aes(x = changemob, y = changeUR, label=datearray),nudge_x = -1.5,size = 3) + 
        geom_point(aes(x = changemob3, y = changeUR3,color='Utah')) + 
        geom_text(aes(x = changemob3, y = changeUR3, label=datearray),nudge_x = -1.5,size = 3) + 
        ylim(-5, 35)+
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = expression(Delta*'Change in State Mobility (%)'), y = expression(Delta*' State Unemployment Rate (%)'),
             title = paste0('Unemployment Rate vs Change in Mobility by State'), color='Legend'))


##########################################################################################

#load data
stringency_us <- read.csv("https://raw.githubusercontent.com/OxCGRT/USA-covid-policy/master/data/OxCGRT_US_latest.csv")
stringency_states <- stringency_us[stringency_us$RegionName!="",]

#change column title of sector data
nameTitle=c('California','Utah')


stringency_states_agg=stringency_states[stringency_states$RegionName=='California',]

stringency_states_agg_refin=stringency_states_agg %>%
  select('RegionName','Date','StringencyIndex')

stringency_states_agg_refin$Date <- as.Date(paste(stringency_states_agg_refin$Date), format("%Y%m%d"))

changestr=rep(0, 11)
datearray=rep(0, 11)

for (j in 2:12){
  
  if (j<10){
    dates = paste0('2020-0',j)
    fordatearr=paste0('0',j)
  }
  else
  {
    dates = paste0('2020-',j)
    fordatearr=j
  }
  
  datearray[j-1]=fordatearr
  
  monthdf=subset(stringency_states_agg_refin, substr(Date, 1, 7) %in% dates)
  
  changestr[j-1]=monthdf[nrow(monthdf),3]-monthdf[1,3]
}





stringency_states_agg2=stringency_states[stringency_states$RegionName=='Utah',]

stringency_states_agg_refin2=stringency_states_agg2 %>%
  select('RegionName','Date','StringencyIndex')

stringency_states_agg_refin2$Date <- as.Date(paste(stringency_states_agg_refin2$Date), format("%Y%m%d"))

changestr2=rep(0, 11)
datearray2=rep(0, 11)

for (j in 2:12){
  
  if (j<10){
    dates = paste0('2020-0',j)
    fordatearr=paste0('0',j)
  }
  else
  {
    dates = paste0('2020-',j)
    fordatearr=j
  }
  
  datearray2[j-1]=fordatearr
  
  monthdf=subset(stringency_states_agg_refin2, substr(Date, 1, 7) %in% dates)
  
  changestr2[j-1]=monthdf[nrow(monthdf),3]-monthdf[1,3]
}

##Visualization

#plot
print(ggplot() +
        geom_point(aes(x = changestr, y = changeUR,color='California')) + 
        geom_text(aes(x = changestr, y = changeUR, label=datearray),nudge_x = -2,size = 3) + 
        geom_point(aes(x = changestr2, y = changeUR3,color='Utah')) + 
        geom_text(aes(x = changestr2, y = changeUR3, label=datearray),nudge_x = -2,size = 3) + 
        ylim(-5, 35)+
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = expression(Delta*'Change in State Stringency (%)'), y = expression(Delta*' State Unemployment Rate (%)'),
             title = paste0('Unemployment Rate vs Change in Stringency by State')))


