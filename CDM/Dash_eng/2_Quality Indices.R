

# call packages
library(tidyverse)
library(lubridate)
library(reshape2)

setwd('c:/users/hsj28/desktop/cdm-dash/응급실/_result')
#---- Dataset ----
# Import completed CDM mapping data
cdm <- read.csv("cdm_mapping.csv")


# ================================================
# Clinical Quality Indices - Calculation algorithm
# ================================================

#---- 1. Bed Occupancy Index ----
# select necessary variables and generate time variables
df <- cdm %>% 
  select(person_id, visit_start_datetime, visit_end_datetime,
         value_as_number, condition_source_value,
         discharge_to_source_value, visit_detail_source_value) %>%
  mutate(year=year(visit_start_datetime), mon=month(visit_start_datetime),
         hour=round(as.numeric(difftime(visit_end_datetime, visit_start_datetime, units = 'hours')),1))

#formula = {(number of patients * average length of stay) / (total number of bed * days * 24hours)} * 100
BOI <- df %>% select(person_id, year, mon, hour) %>% group_by(year, mon) %>%
  summarise(mt = round(mean(hour, na.rm=T),1), np = length(person_id)) %>%   # average length of stay in ED
  mutate(nb = 73,                                                            # total number of bed in ER
         nm = ifelse(mon%in%c(1,3,5,7,8,10,12),31,                           # generate days by month
                     ifelse(mon%in%c(4,6,9,11),30,
                            ifelse(as.numeric(year)%%4==0,29,28))),          # consider a leap month
         bed = round({(np*mt)/(nb*nm*24)}*100,1)) %>%                        # calculate index
  as.data.frame()

BOI



#---- 2. long-term patients Index ----
# remove DOA
i1 <- df %>% filter(condition_source_value == 63238001) %>% select(person_id) # DOA 3
df1 <- df %>% filter(!person_id %in% i1$person_id) # 72-3=69
df1 <- df1 %>% filter(is.na(value_as_number)==F)
rm(i1)

# derived variable: long-term ED time>12hr & >24hr & >48hr
tmp1 <- df1 %>% mutate(ot = ifelse(hour>12&hour<=24,12,
                                   ifelse(hour>24&hour<=48,24,
                                          ifelse(hour>48,48,NA))))
with(data=tmp1, table(ot,mon,year))
a <- with(data=tmp1,table(ot,mon,year)) %>% as.data.frame()
a18 <- a%>%filter(year==2018)
a19 <- a%>%filter(year==2019)
a20 <- a%>%filter(year==2020)

# calculate the number of patients in long-term ED time (>12hr, >24hr, >48hr)
a18_ <- dcast(data=a18,year+mon~ot, value.var='Freq')
a18_$t12 <- apply(a18_[,c(3:5)], 1, sum, na.rm=T)
a18_$t24 <- apply(a18_[,c(4:5)], 1, sum, na.rm=T)
a18_$t48 <- a18_$`48`
a18_ <- a18_ %>% select(1,2,6:8)

a19_ <- dcast(data=a19,year+mon~ot, value.var='Freq')
a19_$t12 <- apply(a19_[,c(3:5)], 1, sum, na.rm=T)
a19_$t24 <- apply(a19_[,c(4:5)], 1, sum, na.rm=T)
a19_$t48 <- a19_$`48`
a19_ <- a19_ %>% select(1,2,6:8)

a20_ <- dcast(data=a20,year+mon~ot, value.var='Freq')
a20_$t12 <- apply(a20_[,c(3:5)], 1, sum, na.rm=T)
a20_$t24 <- apply(a20_[,c(4:5)], 1, sum, na.rm=T)
a20_$t48 <- a20_$`48`
a20_ <- a20_ %>% select(1,2,6:8)

t18_ <- melt(data=a18_, id.vars=c('year','mon'), variable.name='ot',value.name='value')
t18 <- dcast(data=t18_,year+ot~mon, value.var='value')
t19_ <- melt(data=a19_, id.vars=c('year','mon'), variable.name='ot',value.name='value')
t19 <- dcast(data=t19_,year+ot~mon, value.var='value')
t20_ <- melt(data=a20_, id.vars=c('year','mon'), variable.name='ot',value.name='value')
t20 <- dcast(data=t20_,year+ot~mon, value.var='value')
rm(a,a18,a19,a20,a18_,a19_,a20_,t18_,t19_,t20_)

# total patients
tn <- df1 %>% group_by(year, mon) %>% summarise(np = length( person_id)) %>% as.data.frame()
tnn <- dcast(data=tn,year~mon, value.var='np')
tnn <- tnn %>% mutate(ot='all')
tnn <- tnn%>%select(1,14,2:13)

tt18 <- rbind(t18, subset(tnn,year==2018))
tt19 <- rbind(t19, subset(tnn,year==2019))
tt20 <- rbind(t20, subset(tnn,year==2020))
index <- rbind(tt18,tt19,tt20);rownames(index) <- NULL
rm(t18,t19,t20,tn,tnn,tt18,tt19,tt20,tmp1)

# 2018
p18 <- subset(index,year==2018);head(p18)
p18_s <- apply(p18[1:3,3:14],2,sum,na.rm=T)
p18_r <- round(p18_s / p18[4,3:14]*100,2)
rm(p18,p18_s)
# 2019
p19 <- subset(index,year==2019)
p19_s <- apply(p19[1:3,3:14],2,sum,na.rm=T)
p19_r <- round(p19_s / p19[4,3:14]*100,2)
rm(p19,p19_s)
# 2020
p20 <- subset(index,year==2020)
p20_s <- apply(p20[1:3,3:14],2,sum,na.rm=T)
p20_r <- round(p20_s / p20[4,3:14]*100,2)
rm(p20,p20_s)
# total
index1 <- as.data.frame(rbind(p18_r,p19_r,p20_r))
index1 <- index1 %>% mutate(year=c(2018,2019,2020), ot='rate') %>% select(year,ot,1:12)
index1
rm(p18_r,p19_r,p20_r)
# concatenate all years
rownames(index) <- NULL;rownames(index1) <- NULL
LPI <- rbind(index, index1) %>% arrange(year) %>% as.data.frame()
LPI
rm(index, index1)



#---- 3. Severe KTAS Levels ----
# use previous total number of patients
t_time <- LPI %>% filter(ot=='all')
t_time <- t_time %>% rename(value_as_number=ot)
kt_ <- df1 %>% group_by(year,mon, value_as_number) %>% summarise(N=n()) %>% as.data.frame()

# reshape data structure
kt <- dcast(data=kt_,year+value_as_number~mon, value.var='N')
rm(kt_)

k18 <- rbind(subset(kt, year==2018), subset(t_time, year==2018))
k19 <- rbind(subset(kt, year==2019), subset(t_time, year==2019))
k20 <- rbind(subset(kt, year==2020), subset(t_time, year==2020))
index <- rbind(k18,k19,k20);rownames(index) <- NULL
rm(t_time,kt,k18,k19,k20)

# rates each KTAS levels (1,2,3)
index1 <- index %>% filter(value_as_number %in% c(1,2,3,'all'))
# 2018
k18 <- subset(index1,year==2018)
k18_s <- apply(k18[1:3,3:14],2,sum,na.rm=T)
k18_r <- round(k18_s / k18[4,3:14]*100,2)
rm(k18,k18_s)
# 2019
k19 <- subset(index1,year==2019)
k19_s <- apply(k19[1:3,3:14],2,sum,na.rm=T)
k19_r <- round(k19_s / k19[4,3:14]*100,2)
rm(k19,k19_s)
# 2020
k20 <- subset(index1,year==2020)
k20_s <- apply(k20[1:3,3:14],2,sum,na.rm=T)
k20_r <- round(k20_s / k20[4,3:14]*100,2)
rm(k20,k20_s)
# total
index2 <- as.data.frame(rbind(k18_r,k19_r,k20_r))
index2 <- index2 %>% mutate(year=c(2018,2019,2020), ot='rate') %>% select(year,ot,1:12)
index2
rm(k18_r,k19_r,k20_r)
# concatenate all years
rownames(index) <- NULL;rownames(index2) <- NULL
index <- index %>% rename(ot = value_as_number)
KTAS <- rbind(index, index2) %>% arrange(year) %>% as.data.frame()
KTAS

rm(index, index1, index2)



#---- 4. Rate of ER Discharge ----
# check discharge type
table(df1$discharge_to_source_value)

# calculate rates
tmp <- df1 %>% mutate(n=1) %>% group_by(year, mon, discharge_to_source_value) %>% summarise(n1=sum(n)) %>% as.data.frame()
tn <- tmp %>% group_by(year, mon) %>% summarise(tn = sum(n1)) %>% as.data.frame();head(tn)
index <- tmp %>% left_join(tn, by=c('year','mon')) %>% rename(visit_result = discharge_to_source_value)
index <- index %>% mutate(rate = round(n1/tn*100,2))
head(index)
rm(tmp, tn)

# change name shortly
index$visit_result <- ifelse(index$visit_result=='Patient transfer to another hospital','Hospital transfer',
                             ifelse(index$visit_result=='Hospital admission','Admission',index$visit_result))
Discharge<-index

rm(index)



#---- 5. Rate of Hospital Admission ----
# check discharge type
table(df1$visit_detail_source_value)

# calculate rates
tmp <- df1 %>% filter(discharge_to_source_value=='Hospital admission') %>% 
  mutate(n=1) %>% group_by(year, mon, visit_detail_source_value) %>% summarise(n1=sum(n)) %>% as.data.frame()
tn <- tmp %>% group_by(year, mon) %>% summarise(tn = sum(n1)) %>% as.data.frame();head(tn)
index <- tmp %>% left_join(tn, by=c('year','mon')) %>% rename(location = visit_detail_source_value)
index <- index %>% mutate(rate = round(n1/tn*100,2))
head(index)
rm(tmp, tn)

# change name shortly
index$location <- ifelse(index$location=='Ward','GW',
                         ifelse(index$location=='Intensive Care','ICU','OP'))
Admission<-index

rm(index)



#---- Save results for dashboard development ----
write.csv(BOI, 'occupancy rate.csv', row.names = F)        # bed occupancy index
write.csv(LPI, 'long time.csv', row.names = F)             # long-term patients Index
write.csv(KTAS, 'ktas rate.csv', row.names = F)            # severe KTAS levels
write.csv(Discharge, 'result rate.csv', row.names = F)     # Rate of ER Discharge
write.csv(Admission, 'admission rate.csv', row.names = F)  # Rate of Hospital Admission



