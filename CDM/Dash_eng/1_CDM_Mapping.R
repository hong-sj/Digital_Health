

# call packages
library(tidyverse)
library(lubridate)
library(readxl)


# ===========================
# mapping : OMOP CDM v5.3.1 
# ===========================
#---- Dataset ----
data_ED <- read.csv("sample.csv")
# change variable names
data_ED <- data_ED %>% rename(PT_ID = id, birth_Date = birth_date)


#---- PERSON ----
#1) extract required variables for CDM mapping
person_db <- data_ED %>% select(PT_ID, sex, birth_Date)


#2) import join table for CDM table
join_table_gender <- read_xlsx("join_table/join_table_gender.xlsx")


#3) generate PERSON TABLE through CDM mapping
#   arrange variables according to the OMOP table order after CDM mapping
PERSON <- person_db %>%
  mutate(person_id = 1:n(), 
         gender_concept_id = case_when(sex == "M" ~ 8507, sex == "F" ~ 8532), 
         year_of_birth = year(birth_Date),
         month_of_birth = month(birth_Date),
         day_of_birth = day(birth_Date),
         birth_datetime = NA,
         race_concept_id = NA, 
         ethnicity_concept_id = NA, 
         location_id = NA, 
         provider_id = NA, 
         care_site_id = NA, 
         person_source_value = PT_ID, 
         gender_source_value = sex,
         # gender_source_concept_id = NA,          # exist join table
         race_source_value = NA, 
         race_source_concept_id = NA, 
         ethnicity_source_value = NA, 
         ethnicity_source_concept_id = NA) %>%
  left_join(join_table_gender) %>%
  select("person_id", "gender_concept_id", "year_of_birth", "month_of_birth", "day_of_birth", "birth_datetime", 
         "race_concept_id", "ethnicity_concept_id", "location_id", "provider_id", 
         "care_site_id", "person_source_value", "gender_source_value", "gender_source_concept_id", 
         "race_source_value", "race_source_concept_id", "ethnicity_source_value", "ethnicity_source_concept_id")

# check patient id after maaping
personID_sourceValue_map <- PERSON %>%
  select(person_id, person_source_value)
head(personID_sourceValue_map)



#---- VISIT_OCCURRENCE ----
#1) extract required variables for CDM mapping & change name
visit_occurrence <- data_ED %>%
  select(PT_ID, in_time, out_time, visit_type, visit_result)%>%
  rename(VISIT_START_DATETIME = "in_time",
         VISIT_END_DATETIME = "out_time",
         VISIT_SOURCE_VALUE = "visit_type",
         DISCHARGE_TO_SOURCE_VALUE = "visit_result")


#2) import join table for CDM table
join_table_visit <- read_xlsx("join_table/join_table_visit.xlsx")
join_table_visit_discharge <- read_xlsx("join_table/join_table_visit_discharge.xlsx")


#3) generate VISIT_OCCURRENCE TABLE through CDM mapping
#   arrange variables according to the OMOP table order after CDM mapping
VISIT_OCCURRENCE <- visit_occurrence %>%
  left_join(personID_sourceValue_map, by = c("PT_ID" = "person_source_value")) %>%
  mutate(visit_occurrence_id = NA,
         visit_start_date = date(VISIT_START_DATETIME),
         visit_start_datetime = VISIT_START_DATETIME, 
         visit_end_date = date(VISIT_END_DATETIME),
         visit_end_datetime = VISIT_END_DATETIME, 
         visit_type_concept_id = NA,
         provider_id = NA,
         care_site_id = NA, 
         admitting_source_concept_id = NA,
         admitting_source_value = NA,
         # discharge_to_concept_id = NA,                              # exist join table
         # discharge_to_source_value = DISCHARGE_TO_SOURCE_VALUE,     # exist join table
         preceding_visit_occurrence_id = NA) %>%
  left_join(join_table_visit, by = c("VISIT_SOURCE_VALUE" = "visit_source_value")) %>%
  left_join(join_table_visit_discharge, by = c("DISCHARGE_TO_SOURCE_VALUE" = "discharge_to_source_value")) %>%
  rename(visit_source_value = VISIT_SOURCE_VALUE, discharge_to_source_value = DISCHARGE_TO_SOURCE_VALUE) %>%
  select("visit_occurrence_id", "person_id", "visit_concept_id", "visit_start_date", 
         "visit_start_datetime", "visit_end_date", "visit_end_datetime", "visit_type_concept_id",
         "provider_id", "care_site_id", "visit_source_value", "visit_source_concept_id",
         "admitting_source_concept_id", "admitting_source_value", "discharge_to_concept_id",
         "discharge_to_source_value", "preceding_visit_occurrence_id")



#---- VISIT_DETAIL ----
#1) extract required variables for CDM mapping & change name
visit_detail <- data_ED %>% 
  select(PT_ID, visit_result, admit_type)%>%
  rename(VISIT_DETAIL_SOURCE_VALUE = "admit_type",
         DISCHARGE_TO_SOURCE_VALUE = "visit_result")


#2) import join table for CDM table
join_table_visit_detail <- read_xlsx("join_table/join_table_visit_detail.xlsx")


#3) generate VISIT_DETAIL TABLE through CDM mapping
#   arrange variables according to the OMOP table order after CDM mapping
VISIT_DETAIL <- visit_detail %>%
  left_join(personID_sourceValue_map, by = c("PT_ID" = "person_source_value")) %>%
  mutate(visit_detail_id = NA,
         # visit_detail_concept_id = NA,          # exist join table
         visit_detail_start_date = NA,
         visit_detail_start_datetime = NA, 
         visit_detail_end_date = NA, 
         visit_detail_end_datetime = NA,
         visit_detail_type_concept_id = NA,
         provider_id = NA,
         care_site_id = NA, 
         # visit_detail_source_value = NA,        # exist join table
         # visit_detail_source_concept_id = NA,   # exist join table
         admitting_source_value = NA,
         admitting_source_concept_id = NA,
         # discharge_to_source_value = NA,        # exist join table
         # discharge_to_concept_id = NA,          # exist join table
         preceding_visit_detail_id = NA,
         visit_detail_parent_id = NA,
         visit_occurrence_id = NA) %>%
  left_join(join_table_visit_discharge, by = c("DISCHARGE_TO_SOURCE_VALUE" = "discharge_to_source_value")) %>%
  left_join(join_table_visit_detail, by = c("VISIT_DETAIL_SOURCE_VALUE" = "visit_detail_source_value")) %>%
  rename(visit_detail_source_value = VISIT_DETAIL_SOURCE_VALUE, discharge_to_source_value = DISCHARGE_TO_SOURCE_VALUE) %>%
  select("visit_detail_id", "person_id", "visit_detail_concept_id", "visit_detail_start_date", 
         "visit_detail_start_datetime", "visit_detail_end_date", "visit_detail_end_datetime", "visit_detail_type_concept_id",
         "provider_id", "care_site_id", "visit_detail_source_value", "visit_detail_source_concept_id",
         "admitting_source_value", "admitting_source_concept_id", "discharge_to_source_value",
         "discharge_to_concept_id", "preceding_visit_detail_id","visit_detail_parent_id","visit_occurrence_id")



#---- CONDITION_OCCURRENCE ----
#1) extract required variables for CDM mapping & change name
condition_occurrence <- data_ED %>%
  select(PT_ID,  CC)%>%
  mutate(condition_type=ifelse(CC == "DOA", 63238001, NA))


#2) import join table for CDM table
join_table_condition <- read_xlsx("join_table/join_table_condition.xlsx")


#3) generate CONDITION_OCCURRENCE TABLE through CDM mapping
#   arrange variables according to the OMOP table order after CDM mapping
CONDITION_OCCURRENCE <- condition_occurrence %>%
  left_join(personID_sourceValue_map, by = c("PT_ID" = "person_source_value")) %>%
  mutate(condition_occurrence_id = NA,
         condition_start_date =NA,
         condition_start_datetime = NA,
         condition_end_date = NA,
         condition_end_datetime = NA,
         condition_type_concept_id = NA,
         condition_status_concept_id = NA,
         stop_reason = NA,
         provider_id = NA,
         visit_occurrence_id = NA,
         visit_detail_id = NA,
         condition_status_source_value = NA) %>%
  left_join(join_table_condition, by = c("condition_type" = "condition_source_value")) %>%
  rename(condition_source_value = condition_type) %>%
  select(condition_occurrence_id, person_id, condition_concept_id, condition_start_date,
         condition_start_datetime, condition_end_date, condition_end_datetime,
         condition_type_concept_id, condition_status_concept_id, stop_reason, 
         provider_id, visit_occurrence_id, visit_detail_id, 
         condition_source_value, condition_source_concept_id, condition_status_source_value)



#---- OBSERVATION ----
#1) extract required variables for CDM mapping & change name
# severity score(KTAS Level) : ED triage nurse Provider(40759929)
observation <- data_ED %>% 
  select(PT_ID,  ktas, in_time)%>%
  mutate(score_type = "56810-5")


#2) import join table for CDM table
join_table_observation <- read_xlsx("join_table/join_table_observation.xlsx")


#3) generate OBSERVATION TABLE through CDM mapping
#   arrange variables according to the OMOP table order after CDM mapping
OBSERVATION <- observation %>%
  left_join(personID_sourceValue_map, by = c("PT_ID" = "person_source_value")) %>%
  mutate(observation_id = NA,
         observation_date = NA, 
         observation_datetime = NA,
         observation_type_concept_id = NA,
         # value_as_number = NA,              # exist join table
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
  rename(observation_source_value = score_type, value_as_number = ktas) %>%
  select(observation_id, person_id, observation_concept_id, observation_date, observation_datetime,
         observation_type_concept_id, value_as_number, value_as_string, value_as_concept_id,
         qualifier_concept_id, unit_concept_id, provider_id, visit_occurrence_id,
         visit_detail_id, observation_source_value, observation_source_concept_id,
         unit_source_value, qualifier_source_value, observation_event_id,
         obs_event_field_concept_id, value_as_datetime)



# Data Concatenate
df <- left_join(PERSON, VISIT_OCCURRENCE, by = intersect(names(PERSON),names(VISIT_OCCURRENCE)))
df <- left_join(df, VISIT_DETAIL, by = intersect(names(df),names(VISIT_DETAIL)))
df <- left_join(df, CONDITION_OCCURRENCE, by = intersect(names(df),names(CONDITION_OCCURRENCE)))
df <- left_join(df, OBSERVATION, by = intersect(names(df),names(OBSERVATION)))



# Save CDM data
write.csv(df, 'cdm_mapping.csv', row.names = F)



