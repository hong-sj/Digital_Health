

# call packages
library(dplyr)
library(lubridate)
library(reshape2)

# ================================
# 의료 질 지표 산출 알고리즘
# 병상포화지수(Bed Occupancy Rate)
# ================================
# CDM Mapping 완료된 데이터
df1

# 필요 변수 추출(시간 파생변수 생성)
df2 <- df1 %>% select(person_id, visit_start_datetime, visit_end_datetime) %>%
  mutate(year=year(visit_start_datetime), mon=month(visit_start_datetime),
         hour=round(as.numeric(difftime(visit_end_datetime, visit_start_datetime, units = 'hours')),1))


# 병상포화지수 = {(내원환자수 * 평균 재실시간) / (기준병상수 * 월별일자수 * 24시간)}*100
BOR <- df2 %>% select(person_id, year, mon, hour) %>% group_by(year, mon) %>%
  summarise(mt = round(mean(hour, na.rm=T),1), np = length(person_id)) %>%   # 응급실 평균 재실시간 계산
  mutate(nb = 73,                                                            # 응급실 내 병상 총 갯수
         nm = ifelse(mon%in%c(1,3,5,7,8,10,12),31,                           # 월별 일자 수
                     ifelse(mon%in%c(4,6,9,11),30,
                            ifelse(year==2020,29,28))),                      # 2월 윤달에 대한 자동화는 적용 예정
         bed = round({(np*mt)/(nb*nm*24)}*100,1))                            # 산출식에 따른 병상포화지수 산출 

rm(df2)


# ================================
# 장기체류환자 지수
# ================================
# 필요 변수 추출(시간 파생변수 생성)
df2 <- df1 %>% select(person_id, visit_start_datetime, visit_end_datetime, value_as_number) %>%
  mutate(year=year(visit_start_datetime), mon=month(visit_start_datetime),
         hour=round(as.numeric(difftime(visit_end_datetime, visit_start_datetime, units = 'hours')),1))

# KTAS level 결측치 제외; 파생변수 생성 - 12시간 초과(12<h) / 24시간 초과(24<h) / 48시간 초과(h>48)
tmp1 <- df2 %>% filter(is.na(value_as_number)==F) %>% 
  mutate(ot = ifelse(hour>12&hour<=24,12,
                     ifelse(hour>24&hour<=48,24,
                            ifelse(hour>48,48,NA))))

with(data=tmp1, table(ot,mon,year))
a <- with(data=tmp1,table(ot,mon,year)) %>% as.data.frame()
a18 <- a%>%filter(year==2018)
a19 <- a%>%filter(year==2019)
a20 <- a%>%filter(year==2020)

a18_ <- dcast(data=a18,year+mon~ot, value.var='Freq')
a18_$t12 <- apply(a18_[,c(3:5)], 1, sum)
a18_$t24 <- apply(a18_[,c(4:5)], 1, sum)
a18_$t48 <- a18_$`48`
a18_ <- a18_ %>% select(1,2,6:8)

a19_ <- dcast(data=a19,year+mon~ot, value.var='Freq')
a19_$t12 <- apply(a19_[,c(3:5)], 1, sum)
a19_$t24 <- apply(a19_[,c(4:5)], 1, sum)
a19_$t48 <- a19_$`48`
a19_ <- a19_ %>% select(1,2,6:8)

a20_ <- dcast(data=a20,year+mon~ot, value.var='Freq')
a20_$t12 <- apply(a20_[,c(3:5)], 1, sum)
a20_$t24 <- apply(a20_[,c(4:5)], 1, sum)
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
tn <- df2 %>% group_by(year, mon) %>% summarise(np = length( person_id)) %>% as.data.frame()
tnn <- dcast(data=tn,year~mon, value.var='np')
tnn <- tnn %>% mutate(ot='all')
tnn <- tnn%>%select(1,14,2:13)

tt18 <- rbind(t18, subset(tnn,year==2018))
tt19 <- rbind(t19, subset(tnn,year==2019))
tt20 <- rbind(t20, subset(tnn,year==2020))
index3 <- rbind(tt18,tt19,tt20)

rm(t18,t19,t20,tn,tnn,tt18,tt19,tt20,tmp1)


# ================================
# 중증응급환자비율(KTAS) 
# ================================
# 1) 연/월 별 KTAS 수
t_time <- index3 %>% filter(ot=='all') # 접수취소, 진료외, DOA 제외한 총 내원환자수 기준
t_time <- t_time %>% rename(value_as_number=ot)
kt_ <- df2 %>% group_by(year,mon, value_as_number) %>% summarise(N=n()) %>% as.data.frame()

kt <- dcast(data=kt_,year+value_as_number~mon, value.var='N')
rm(kt_)

k18 <- rbind(subset(kt, year==2018), subset(t_time, year==2018))
k19 <- rbind(subset(kt, year==2019), subset(t_time, year==2019))
k20 <- rbind(subset(kt, year==2020), subset(t_time, year==2020))
ktas <- rbind(k18,k19,k20)
rm(t_time,kt,k18,k19,k20)

### 2) Level 3 이상 (KTAS=1,2,3) 비율
ktas_r <- ktas %>% filter(value_as_number %in% c(1,2,3,'all'))
# 2018
k18 <- subset(ktas_r,year==2018)
k18_s <- apply(k18[1:3,3:14],2,sum)
k18_r <- round(k18_s / k18[4,3:14]*100,1)
rm(k18,k18_s)
# 2019
k19 <- subset(ktas_r,year==2019)
k19_s <- apply(k19[1:3,3:14],2,sum)
k19_r <- round(k19_s / k19[4,3:14]*100,1)
rm(k19,k19_s)
# 2020
k20 <- subset(ktas_r,year==2020)
k20_s <- apply(k20[1:3,3:14],2,sum)
k20_r <- round(k20_s / k20[4,3:14]*100,1)
rm(k20,k20_s)
# total
index5 <- as.data.frame(rbind(k18_r,k19_r,k20_r))
index5 <- index5 %>% mutate(year=c(2018,2019,2020)) %>% select(year,1:12)
index5
rm(k18_r,k19_r,k20_r)
