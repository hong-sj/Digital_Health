

## 외부 검증용 구현 코드
# update: 2021.04.21

# 패키지 호출
pacman::p_load(tidyverse, lubridate, readxl)

# 경로 설정
mypath <- "directory/"


# 샘플데이터 호출: 접수취소, 진료외방문 등 제외된 데이터
er <- read.csv(paste0(mypath, '/sample.csv'))
sapply(er, class) # class 확인

# 유형 변경
er$id <- as.character(er$id) # 환자번호
er$in_time <- as.POSIXct(er$in_time) # 내원일시
er$out_time <- as.POSIXct(er$out_time) # 퇴실일시
er$birth_Date <- as.POSIXct(er$birth_Date) # 생년월일
er$sex <- as.character(er$sex) # 성별
er$ktas_time <- as.POSIXct(er$ktas_time) # KTAS 기록 시간
er$ktas <- as.numeric(er$ktas) # KTAS 레벨
er$disease_type <- as.character(er$disease_type) # 질병 구분
er$visit_result <- as.numeric(er$visit_result) # 퇴실 결과(번호)
er$visit_result_text <- as.character(er$visit_result_text) # 퇴실 결과(한글)



# ===========================
# mapping : OMOP CDM v5.3.1 
# ===========================

#--- Prepare ID ---
## Generate ID
# 1. u_id: 환자 총 기록수 (Record 기준)
er <- er %>% mutate(u_id = as.character(1:n())) # 153


#--- PERSON ---
#cdw data
person <- er %>% select(c(id, sex, birth_Date)) %>% 
  rename(PT_ID = id,
         GENDER_SOURCE_VALUE = sex) %>%
  distinct() # 100

#cdm data
join_table_gender <- read_xlsx(paste0(mypath,"/join_table/join_table_gender.xlsx"))

#mapping data
PERSON <- person %>%
  mutate(person_id = 1:n(), 
         # gender_concept_id = NA,                   # Exist join_table
         year_of_birth = year(birth_Date), 
         month_of_birth = month(birth_Date), 
         day_of_birth = day(birth_Date), 
         birth_datetime = NA, 
         race_concept_id = NA, 
         ethnicity_concept_id = NA, 
         location_id = NA, 
         provider_id = NA, 
         care_site_id = 1005,                        # SMC care site value
         person_source_value = PT_ID, 
         gender_source_value = GENDER_SOURCE_VALUE,
         # gender_source_concept_id = NA,            # Exist join_table
         race_source_value = NA, 
         race_source_concept_id = NA, 
         ethnicity_source_value = NA, 
         ethnicity_source_concept_id = NA) %>%
  left_join(join_table_gender, by='gender_source_value') %>%
  select(person_id, gender_concept_id, year_of_birth, month_of_birth, day_of_birth, birth_datetime, 
         race_concept_id, ethnicity_concept_id, location_id, provider_id, 
         care_site_id, person_source_value, gender_source_value, gender_source_concept_id, 
         race_source_value, race_source_concept_id, ethnicity_source_value, ethnicity_source_concept_id)

# data type 변경: date(as.Date), datetype(as.POSIXct)
type_person <- read.csv(paste0(mypath, "/type/type_person.csv"))

for(i in 1:length(type_person)){type_person[,i] <- as.character(type_person[,i])}
table(type_person$Type)
dt <- type_person[type_person$Type == "datetime",]$Field
i <- type_person[type_person$Type == "integer",]$Field
c50 <- type_person[type_person$Type == "varchar(50)",]$Field

PERSON[,dt] <- as.POSIXct(PERSON[,dt])
PERSON[,i] <- lapply(PERSON[,i], as.integer)
PERSON[,c50] <- lapply(PERSON[,c50], as.character)
rm(dt,i,c50, join_table_gender, type_person)

# person ID와 CDM source value 맞춘 table
personID_sourceValue_map <- PERSON %>%
  select(person_id, person_source_value)
head(personID_sourceValue_map)




#--- VISIT_OCCURRENCE------
#cdw data
visit_occurrence <- er %>% 
  select(id, u_id, in_time, out_time, visit_result) %>%
  mutate(VISIT_SOURCE_VALUE = paste0(id,'_',in_time)) %>%  # 접수번호 대용 (환자번호 + 내원일시))
  rename(VISIT_OCCURRENCE_ID = u_id,
         VISIT_START_DATETIME = in_time,
         VISIT_END_DATETIME = out_time,
         DISCHARGE_TO_SOURCE_VALUE = visit_result)

#join table
join_table_visit_discharge <- read_xlsx(paste0(mypath,"/join_table/join_table_visit_discharge.xlsx"))

#mapping data
VISIT_OCCURRENCE <- visit_occurrence %>%
  left_join(personID_sourceValue_map, by = c("id" = "person_source_value")) %>%
  mutate(visit_occurrence_id = VISIT_OCCURRENCE_ID,
         visit_concept_id = 9203,                        # Emergence Room Visit
         visit_start_date = date(VISIT_START_DATETIME),
         visit_start_datetime = VISIT_START_DATETIME, 
         visit_end_date = date(VISIT_END_DATETIME),
         visit_end_datetime = VISIT_END_DATETIME, 
         visit_type_concept_id = NA,
         provider_id = NA,
         care_site_id = 1005,                        # SMC care site value
         visit_source_value = VISIT_SOURCE_VALUE,
         visit_source_concept_id = NA,
         admitting_source_concept_id = NA,
         admitting_source_value = NA,
         # discharge_to_concept_id = NA,                              # Exist join table
         discharge_to_source_value = DISCHARGE_TO_SOURCE_VALUE,
         preceding_visit_occurrence_id = NA) %>%
  left_join(join_table_visit_discharge, by = "discharge_to_source_value") %>%
  select(visit_occurrence_id, person_id, visit_concept_id, visit_start_date, 
         visit_start_datetime, visit_end_date, visit_end_datetime, visit_type_concept_id,
         provider_id, care_site_id, visit_source_value, visit_source_concept_id,
         admitting_source_concept_id, admitting_source_value, discharge_to_concept_id,
         discharge_to_source_value, preceding_visit_occurrence_id)

# data type 변경: date(as.Date), datetype(as.POSIXct)
type_visit <- read.csv(paste0(mypath, "/type/type_visit.csv"))

for(i in 1:length(type_visit)){type_visit[,i] <- as.character(type_visit[,i])}
table(type_visit$Type)
d <- type_visit[type_visit$Type == "date",]$Field
dt <- type_visit[type_visit$Type == "datetime",]$Field
i <- type_visit[type_visit$Type == "integer",]$Field
I <- type_visit[type_visit$Type == "Integer",]$Field
c50 <- type_visit[type_visit$Type == "varchar(50)",]$Field

VISIT_OCCURRENCE[,d] <- lapply(VISIT_OCCURRENCE[,d], as.Date)
VISIT_OCCURRENCE[,dt] <- lapply(VISIT_OCCURRENCE[,dt], as.POSIXct)
VISIT_OCCURRENCE[,i] <- lapply(VISIT_OCCURRENCE[,i], as.integer)
VISIT_OCCURRENCE[,I] <- as.integer(VISIT_OCCURRENCE[,I])
VISIT_OCCURRENCE[,c50] <- lapply(VISIT_OCCURRENCE[,c50], as.character)

rm(d,dt,i,I,c50,join_table_visit_discharge)



#--- measurement------
# Triage index : KTAS 사용

#CDW data
measurement <- er %>% 
  select(id, u_id, ktas_time, ktas) %>% 
  mutate(MEASUREMENT_SOURCE_VALUE = 'KTAS',
         MEASUREMENT_ID = u_id,
         VISIT_OCCURRENCE_ID = u_id) %>%
  rename(MEASUREMENT_DATE = ktas_time,
         VALUE_AS_NUMBER = ktas)

#join table
join_table_measurement <- read_xlsx(paste0(mypath,"/join_table/join_table_measurement.xlsx"))

#mapping data
MEASUREMENT <- measurement %>%
  left_join(personID_sourceValue_map, by = c("id" = "person_source_value")) %>%
  mutate(measurement_id = MEASUREMENT_ID,
         # measurement_concept_id = NA,                          # Exist join_table
         measurement_date = date(MEASUREMENT_DATE),
         measurement_datetime = MEASUREMENT_DATE,
         measurement_time = NA,
         measurement_type_concept_id = NA,
         operator_concept_id = NA,
         value_as_number = VALUE_AS_NUMBER,
         value_as_concept_id = NA,
         unit_concept_id = NA,
         range_low = NA,
         range_high = NA,
         provider_id = NA,
         visit_occurrence_id = VISIT_OCCURRENCE_ID,
         visit_detail_id = NA,
         measurement_source_value = MEASUREMENT_SOURCE_VALUE,
         measurement_source_concept_id = NA,
         unit_source_value = NA,
         value_source_value = NA) %>%
  left_join(join_table_measurement, by = "measurement_source_value") %>%
  select(measurement_id,person_id,measurement_concept_id,measurement_date,measurement_datetime,
         measurement_time,measurement_type_concept_id,operator_concept_id,value_as_number,
         value_as_concept_id,unit_concept_id,range_low,range_high,provider_id,visit_occurrence_id,
         visit_detail_id,measurement_source_value,measurement_source_concept_id,unit_source_value,value_source_value)

# data type 변경: date(as.Date), datetype(as.POSIXct)
type_measure <- read.csv(paste0(mypath, "/type/type_measure.csv"))

for(i in 1:length(type_measure)){type_measure[,i] <- as.character(type_measure[,i])}
table(type_measure$Type)
d <- type_measure[type_measure$Type == "date",]$Field
dt <- type_measure[type_measure$Type == "datetime",]$Field
f <- type_measure[type_measure$Type == "float",]$Field
i <- type_measure[type_measure$Type == "integer",]$Field
c10 <- type_measure[type_measure$Type == "varchar(10)",]$Field
c50 <- type_measure[type_measure$Type == "varchar(50)",]$Field

MEASUREMENT[,d] <- as.Date(MEASUREMENT[,d])
MEASUREMENT[,dt] <- as.POSIXct(MEASUREMENT[,dt])
MEASUREMENT[,f] <- lapply(MEASUREMENT[,f], as.double)
MEASUREMENT[,i] <- lapply(MEASUREMENT[,i], as.integer)
MEASUREMENT[,c10] <- as.character(MEASUREMENT[,c10])
MEASUREMENT[,c50] <- lapply(MEASUREMENT[,c50], as.character)

rm(d,dt,f,i,c10,c50, join_table_measurement)




# Clinical Data Tables
# data merge
df <- left_join(PERSON, VISIT_OCCURRENCE,by = intersect(names(PERSON),names(VISIT_OCCURRENCE))) # 153
df <- left_join(df, MEASUREMENT, by = intersect(names(df),names(MEASUREMENT)))# 153


# remove
rm(list = setdiff(ls(), c('mypath','er','df')))

# save file
save.image(paste0(mypath, '/er_cdm.rdata'))


