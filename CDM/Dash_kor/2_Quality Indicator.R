

## 외부 검증용 구현 코드
# update: 2021.04.21

# 패키지 호출
pacman::p_load(tidyverse, lubridate, reshape2)

# 경로 설정
mypath <- "directory/"


# CDM 매핑 데이터 호출
load(paste0(mypath, '/er_cdm.rdata'))


# Data after CDM Mapping
head(df)


# 응급진료결과 재정의
df1 <- df %>%
  select(person_id, visit_occurrence_id, visit_start_datetime, visit_end_datetime,
         value_as_number, discharge_to_source_value) %>% 
  mutate(discharge=ifelse(substr(discharge_to_source_value,1,1)==1,'Home',
                          ifelse(substr(discharge_to_source_value,1,1)==2,'Patient transfer from hospital to hospital',
                                 ifelse(substr(discharge_to_source_value,1,1)==3,'Hospital patient','Patient died'))))
# 입원구분 재정의
df1 <- df1 %>% 
  mutate(admission=ifelse(discharge_to_source_value==31,'GW',
                          ifelse(discharge_to_source_value==32,'ICU',
                                 ifelse(discharge_to_source_value %in% c(33,34), 'OP', NA))))

table(df1$discharge)
# Home 107  admission 42  death 2  transfer 2
table(df1$admission)
# GW 36  ICU  4  OP 2


# select necessary variables and generate time variables
df1 <- df1 %>%
  mutate(year = year(visit_start_datetime),
         month = month(visit_start_datetime),
         hour = round(as.numeric(difftime(visit_end_datetime, visit_start_datetime, units = 'hours')),1))


# ============
# 병상포화지수
# ============
# 병상포화지수 = {(내원환자수 * 평균 재실시간) / (기준병상수 * 월별일자수 * 24시간)}*100
# 접수취소, 진료외방문 제외한 총 내원환자수 기준
BOI <- df1 %>% select(visit_occurrence_id, year, month, hour) %>% group_by(year, month) %>%
  summarise(mt = round(mean(hour, na.rm=T),1), np = length(visit_occurrence_id)) %>%   # average length of stay in ED
  mutate(nb = 73,                                                                      # total number of beds in ER
         nm = ifelse(month%in%c(1,3,5,7,8,10,12),31,                                   # generate days by month
                     ifelse(month%in%c(4,6,9,11),30,
                            ifelse(as.numeric(year)%%4==0,29,28))),                    # consider a leap month
         bed = round({(np*mt)/(nb*nm*24)}*100,1)) %>%                                  # expression of indicator 
  as.data.frame()

BOI



# =================
# 장기체류환자 지수
# =================
# 파생 변수 - time >12hr; >24hr;>48hr
df2 <- df1 %>%
  mutate(ot = ifelse(hour>12&hour<=24,12,
                     ifelse(hour>24&hour<=48,24,
                            ifelse(hour>48,48,NA))))
# remove missing KTAS-level & DOA
table(df2$discharge_to_source_value) # 41번이 DOA = 0
df2 <- df2 %>% filter(discharge_to_source_value!=41) # DOA : 153 -> 153
df2 <- df2 %>% filter(is.na(value_as_number)==F) # missing KTAS : 153 -> 153

with(data=df2, table(ot,month,year))
a <- with(data=df2,table(ot,month,year)) %>% as.data.frame()
a18 <- a%>%filter(year==2018)
a19 <- a%>%filter(year==2019)
a20 <- a%>%filter(year==2020)

# calculate the number of patients in prolonged ED time (>12hr, >24hr, >48hr)
a18_ <- dcast(data=a18,year+month~ot, value.var='Freq', fill=0)
a18_$t12 <- apply(a18_[,c(3:5)], 1, sum, na.rm=T)
a18_$t24 <- apply(a18_[,c(4:5)], 1, sum, na.rm=T)
a18_$t48 <- a18_$`48`
a18_ <- a18_ %>% select(1,2,6:8)

a19_ <- dcast(data=a19,year+month~ot, value.var='Freq', fill=0)
a19_$t12 <- apply(a19_[,c(3:5)], 1, sum, na.rm=T)
a19_$t24 <- apply(a19_[,c(4:5)], 1, sum, na.rm=T)
a19_$t48 <- a19_$`48`
a19_ <- a19_ %>% select(1,2,6:8)

a20_ <- dcast(data=a20,year+month~ot, value.var='Freq', fill=0)
a20_$t12 <- apply(a20_[,c(3:5)], 1, sum, na.rm=T)
a20_$t24 <- apply(a20_[,c(4:5)], 1, sum, na.rm=T)
a20_$t48 <- a20_$`48`
a20_ <- a20_ %>% select(1,2,6:8)

t18_ <- melt(data=a18_, id.vars=c('year','month'), variable.name='ot',value.name='value')
t18 <- dcast(data=t18_,year+ot~month, value.var='value', fill=0)
t19_ <- melt(data=a19_, id.vars=c('year','month'), variable.name='ot',value.name='value')
t19 <- dcast(data=t19_,year+ot~month, value.var='value', fill=0)
t20_ <- melt(data=a20_, id.vars=c('year','month'), variable.name='ot',value.name='value')
t20 <- dcast(data=t20_,year+ot~month, value.var='value', fill=0)
rm(a,a18,a19,a20,a18_,a19_,a20_,t18_,t19_,t20_)

# total patients
tn <- df1 %>% group_by(year, month) %>% summarise(np = length(visit_occurrence_id)) %>% as.data.frame()
tnn <- dcast(data=tn,year~month, value.var='np', fill=0)
tnn <- tnn %>% mutate(ot='all')
tnn <- tnn%>%select(1,14,2:13)

tt18 <- rbind(t18, subset(tnn,year==2018))
tt19 <- rbind(t19, subset(tnn,year==2019))
tt20 <- rbind(t20, subset(tnn,year==2020))
index <- rbind(tt18,tt19,tt20);rownames(index) <- NULL

index
rm(t18,t19,t20,tn,tnn,tt18,tt19,tt20)

# 2018
p18 <- subset(index,year==2018);head(p18)
p18_s <- apply(p18[1:3,3:14],2,sum, na.rm=T)
p18_r <- round(p18_s / p18[4,3:14]*100,2)
rm(p18,p18_s)
# 2019
p19 <- subset(index,year==2019)
p19_s <- apply(p19[1:3,3:14],2,sum, na.rm=T)
p19_r <- round(p19_s / p19[4,3:14]*100,2)
rm(p19,p19_s)
# 2020
p20 <- subset(index,year==2020)
p20_s <- apply(p20[1:3,3:14],2,sum, na.rm=T)
p20_r <- round(p20_s / p20[4,3:14]*100,2)
rm(p20,p20_s)
# total
index1 <- as.data.frame(rbind(p18_r,p19_r,p20_r))
index1 <- index1 %>% mutate(year=c(2018,2019,2020), ot='rate') %>% select(year,ot,1:12)
index1
rm(p18_r,p19_r,p20_r)

# 결과 취합
rownames(index) <- NULL;rownames(index1) <- NULL
PPI <- rbind(index, index1) %>% arrange(year) %>% as.data.frame()
PPI

rm(index, index1)



# ======================
# 중증응급환자비율(KTAS)
# ======================
# 1) the number of patients by KTAS-Level
t_time <- PPI %>% filter(ot=='all') # 접수취소, 진료외방문, DOA 제외한 총 내원환자수 기준
t_time <- t_time %>% rename(value_as_number=ot)
kt_ <- df2 %>% group_by(year,month, value_as_number) %>% summarise(N=n()) %>% as.data.frame()

kt <- dcast(data=kt_,year+value_as_number~month, value.var='N', fill=0)
rm(kt_)

k18 <- rbind(subset(kt, year==2018), subset(t_time, year==2018))
k19 <- rbind(subset(kt, year==2019), subset(t_time, year==2019))
k20 <- rbind(subset(kt, year==2020), subset(t_time, year==2020))
index <- rbind(k18,k19,k20);rownames(index) <- NULL

index
rm(t_time,kt,k18,k19,k20)

# 2) Level 3 이상 (KTAS=1,2,3) 비율
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
index2 <- index2 %>% mutate(year=c(2018,2019,2020), KTAS_Level='rate') %>% select(year,KTAS_Level,1:12)
index2
rm(k18_r,k19_r,k20_r)

# 결과 취합
rownames(index) <- NULL;rownames(index2) <- NULL
index <- index %>% rename(KTAS_Level = value_as_number)
KTAS <- rbind(index, index2) %>% arrange(year) %>% as.data.frame()
KTAS

rm(index, index1, index2)



# ====================
# 응급실 퇴실결과 비율
# ====================
# compare result of ER discharge (discharge, admission, death, transfer)

table(df1$discharge)
# Home       Hospital patient           Patient died 
# 107              42                     2 
# Patient transfer from hospital to hospital 
# 2

tmp <- df1 %>% mutate(n=1) %>% group_by(year, month, discharge) %>% summarise(n1=sum(n)) %>% as.data.frame()
tn <- tmp %>% group_by(year, month) %>% summarise(tn = sum(n1)) %>% as.data.frame();head(tn)
index <- tmp %>% left_join(tn, by=c('year','month')) %>% rename(visit_result = discharge)
index <- index %>% mutate(rate = round(n1/tn*100,2))
head(index)
rm(tmp, tn)

# change name shortly
index$visit_result <- ifelse(index$visit_result=='Patient transfer from hospital to hospital','Hospital transfer',
                             ifelse(index$visit_result=='Hospital patient','Admission',
                                    ifelse(index$visit_result=='Home','Discharge','Death')))
table(index$visit_result)
Discharge<-index
Discharge

rm(index)



# =========
# 입원 비율
# ========
# compare result of ER discharge (GW, ICU, OP)

table(df1$admission)
# Intensive Care  Operating room          Ward 
#           4            2                 36

tmp <- df1 %>% filter(discharge=='Hospital patient') %>% 
  mutate(n=1) %>% group_by(year, month, admission) %>% summarise(n1=sum(n)) %>% as.data.frame()
tn <- tmp %>% group_by(year, month) %>% summarise(tn = sum(n1)) %>% as.data.frame();head(tn)
index <- tmp %>% left_join(tn, by=c('year','month')) %>% rename(location = admission)
index <- index %>% mutate(rate = round(n1/tn*100,2))
head(index)
rm(tmp, tn)

Admission<-index
Admission

rm(index)


# 하나의 xlsx 파일에 시트로 구분하여 저장
library(writexl)
mydata <- list(discharge=Discharge, admission=Admission, ktas=KTAS, long=PPI, bed=BOI)
write_xlsx(mydata, paste0(mypath, "/index_result.xlsx"))


