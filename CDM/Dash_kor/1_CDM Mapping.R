

# call packages
library(dplyr)
library(lubridate)
library(readxl)

# ===========================
# mapping
# ===========================
#--- Dataset -----
data_ED <- read.csv("sample.csv")

# CDW data: 결합을 위해 CDM Table의 field명에 맞추어 이름 변경
data_ED <- data_ED %>% rename(P_ID = "환자번호", sex = "성별", birth_Date = "생년월일", KTAS="최초KTAS레벨", 
                            VISIT_START_DATETIME = "내원일시", VISIT_END_DATETIME = "퇴실일시")
head(data_ED)

#--- PERSON ------
#1) CDW data: 필요 변수 추출
person_db <- data_ED %>% select(P_ID, sex, birth_Date)

#2) CDM table: 매핑을 위한 CDM Table 호출 (엑셀로 사전 저장)
join_table_gender <- read_xlsx("join_table/join_table_gender.xlsx")

#3) mapping data: 매핑 진행 후, 마지막에 OMOP CDM Table field 순서에 맞게 정렬
PERSON <- person_db %>%
  mutate(person_id = 1:n(), 
         gender_concept_id = case_when(sex == "M" ~ 8507, sex == "F" ~ 8532), 
         year_of_birth = year(birth_Date), 
         month_of_birth = month(birth_Date), 
         day_of_birth = day(birth_Date), 
         birth_datetime = NA, 
         death_datetime = NA, 
         race_concept_id = NA, 
         ethnicity_concept_id = NA, 
         location_id = NA, 
         provider_id = NA, 
         care_site_id = NA, 
         person_source_value = P_ID, 
         gender_source_value = sex,
         race_source_value = NA, 
         race_source_concept_id = NA, 
         ethnicity_source_value = NA, 
         ethnicity_source_concept_id = NA) %>%
  left_join(join_table_gender) %>%
  select("person_id", "gender_concept_id", "year_of_birth", "month_of_birth", "day_of_birth", "birth_datetime", 
         "death_datetime", "race_concept_id", "ethnicity_concept_id", "location_id", "provider_id", 
         "care_site_id", "person_source_value", "gender_source_value", "gender_source_concept_id", 
         "race_source_value", "race_source_concept_id", "ethnicity_source_value", "ethnicity_source_concept_id")

# person ID와 CDM source value 맞춘 table
personID_sourceValue_map <- PERSON %>%
  select(person_id, person_source_value)
head(personID_sourceValue_map)


#--- VISIT_OCCURRENCE ------
#1) CDW data: 필요 변수 추출
visit_occurrence <- data_ED %>% 
  select("P_ID", "VISIT_START_DATETIME", "VISIT_END_DATETIME")%>%
  mutate(visit_type='ER')

#2) CDM table: 매핑을 위한 CDM Table 호출 (엑셀로 사전 저장)
join_table_visit <- read_xlsx("join_table/join_table_visit.xlsx")

#3) mapping data: 매핑 진행 후, 마지막에 OMOP CDM Table field 순서에 맞게 정렬
VISIT_OCCURRENCE <- visit_occurrence %>%
  left_join(personID_sourceValue_map, by = c("P_ID" = "person_source_value")) %>%
  mutate(visit_occurrence_id = NA,
         visit_start_date = date(VISIT_START_DATETIME),
         visit_start_datetime = VISIT_START_DATETIME, 
         visit_end_date = date(VISIT_END_DATETIME),
         visit_end_datetime = VISIT_END_DATETIME, 
         visit_type_concept_id = NA,
         provider_id = NA,
         care_site_id = NA, 
         admitted_from_concept_id = NA,
         admitted_from_source_value = NA,
         discharge_to_concept_id = NA,
         discharge_to_source_value = NA,
         preceding_visit_occurrence_id = NA) %>%
  left_join(join_table_visit, by = c("visit_type" = "visit_source_value")) %>%
  rename(visit_source_value = visit_type) %>%
  select("visit_occurrence_id", "person_id", "visit_concept_id", "visit_start_date", 
         "visit_start_datetime", "visit_end_date", "visit_end_datetime", "visit_type_concept_id",
         "provider_id", "care_site_id", "visit_source_value", "visit_source_concept_id",
         "admitted_from_concept_id", "admitted_from_source_value", "discharge_to_concept_id",
         "discharge_to_source_value", "preceding_visit_occurrence_id")



#--- observation (KTAS)------
# observation table에서 KTAS Level에 대한 항목은 'ED triage nurse Provider(40759929)' 이용

#1) CDW data: 필요 변수 추출
observation <- data_ED %>% 
  select("P_ID",  "KTAS", "VISIT_START_DATETIME")%>%
  mutate(score_type = "56810-5")

#2) CDM table: 매핑을 위한 CDM Table 호출 (엑셀로 사전 저장)
join_table_observation <- read_xlsx("join_table/join_table_observation.xlsx")

#3) mapping data: 매핑 진행 후, 마지막에 OMOP CDM Table field 순서에 맞게 정렬
# data type 변경: date(as.Date), datetype(as.POSIXct)/ float(as.double)
OBSERVATION <- observation %>%
  left_join(personID_sourceValue_map, by = c("P_ID" = "person_source_value")) %>%
  mutate(observation_id = NA,
         observation_date = NA, 
         observation_datetime = NA,
         observation_type_concept_id = NA,
         # value_as_number = NA, 
         value_as_string = NA, 
         value_as_concept_id = NA, 
         qualifier_concept_id = NA, 
         unit_concept_id = NA, 
         provider_id = NA, 
         visit_occurrence_id = NA, 
         visit_detail_id = NA, 
         unit_source_value = NA, 
         qualifier_source_value = NA, 
         observation_event_id = NA, 
         obs_event_field_concept_id = NA, 
         value_as_datetime = NA) %>%
  left_join(join_table_observation, by = c("score_type" = "observation_source_value")) %>%
  rename(observation_source_value = score_type,
         value_as_number = KTAS) %>%
  select(observation_id, person_id, observation_concept_id, observation_date, observation_datetime,
         observation_type_concept_id, value_as_number, value_as_string, value_as_concept_id,
         qualifier_concept_id, unit_concept_id, provider_id, visit_occurrence_id,
         visit_detail_id, observation_source_value, observation_source_concept_id,
         unit_source_value, qualifier_source_value, observation_event_id,
         obs_event_field_concept_id, value_as_datetime)


#--- CDM Data 결합 ------
# PERSON과 VISIT_OCCURRENCE를 결합
df <- left_join(PERSON, VISIT_OCCURRENCE, by = intersect(names(PERSON), names(VISIT_OCCURRENCE)))
df1 <- left_join(df, OBSERVATION, by = intersect(names(df), names(OBSERVATION)))

