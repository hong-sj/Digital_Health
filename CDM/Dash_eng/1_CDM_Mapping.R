

# call packages
library(tidyverse)
library(lubridate)
library(readxl)


# ===========================
# mapping : OMOP CDM v5.3.1 
# ===========================
#---- Dataset ----
data_ED <- read_xlsx("sample.xlsx")


#--- Prepare ID ---
data_ED <- data_ED %>% 
  mutate(PT_ID = as.character(id),
         U_ID = as.character(1:n()))


#---- PERSON ----
#1) extract required variables for CDM mapping
person_db <- data_ED %>% select(c(PT_ID, sex, birth_Date)) %>% 
  rename(GENDER_SOURCE_VALUE = sex) %>%
  distinct()


#2) import join table for CDM table
join_table_gender <- read_xlsx("join_table/join_table_gender.xlsx")


#3) generate PERSON TABLE through CDM mapping
#   arrange variables according to the OMOP table order after CDM mapping
PERSON <- person_db %>%
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
         care_site_id = NA, 
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

# Formatting data type: date(as.Date), datetype(as.POSIXct)
type_person <- read.csv("type/type_person.csv")

for(i in 1:length(type_person)){type_person[,i] <- as.character(type_person[,i])}
table(type_person$Type)
dt <- type_person[type_person$Type == "datetime",]$Field
i <- type_person[type_person$Type == "integer",]$Field
c50 <- type_person[type_person$Type == "varchar(50)",]$Field

PERSON[,dt] <- lapply(PERSON[,dt],as.POSIXct)
PERSON[,i] <- lapply(PERSON[,i], as.integer)
PERSON[,c50] <- lapply(PERSON[,c50], as.character)
str(PERSON)
rm(dt,i,c50,join_table_gender,type_person)

# check patient id after maaping
personID_sourceValue_map <- PERSON %>%
  select(person_id, person_source_value)
head(personID_sourceValue_map)




#---- VISIT_OCCURRENCE ----
#1) extract required variables for CDM mapping & change name
visit_occurrence <- data_ED %>% 
  select(PT_ID, U_ID, in_time, out_time, visit_type, visit_result) %>%
  rename(VISIT_OCCURRENCE_ID = U_ID,
         VISIT_START_DATETIME = in_time,
         VISIT_END_DATETIME = out_time,
         VISIT_SOURCE_VALUE = visit_type,
         DISCHARGE_TO_SOURCE_VALUE = visit_result)


#2) import join table for CDM table
join_table_visit <- read_xlsx("join_table/join_table_visit.xlsx")
join_table_visit_discharge <- read_xlsx("join_table/join_table_visit_discharge.xlsx")


#3) generate VISIT_OCCURRENCE TABLE through CDM mapping
#   arrange variables according to the OMOP table order after CDM mapping
VISIT_OCCURRENCE <- visit_occurrence %>%
  left_join(personID_sourceValue_map, by = c("PT_ID" = "person_source_value")) %>%
  mutate(visit_occurrence_id = VISIT_OCCURRENCE_ID,
         # visit_concept_id = NA,                        # Exist join_table
         visit_start_date = date(VISIT_START_DATETIME),
         visit_start_datetime = VISIT_START_DATETIME, 
         visit_end_date = date(VISIT_END_DATETIME),
         visit_end_datetime = VISIT_END_DATETIME, 
         visit_type_concept_id = NA,
         provider_id = NA,
         care_site_id = NA, 
         visit_source_value = VISIT_SOURCE_VALUE,
         # visit_source_concept_id = NA,                 # Exist join_table
         admitting_source_concept_id = NA,
         admitting_source_value = NA,
         # discharge_to_concept_id = NA,                              # Exist join table
         discharge_to_source_value = DISCHARGE_TO_SOURCE_VALUE,
         preceding_visit_occurrence_id = NA) %>%
  left_join(join_table_visit, by = "visit_source_value") %>%
  left_join(join_table_visit_discharge, by = "discharge_to_source_value") %>%
  select(visit_occurrence_id, person_id, visit_concept_id, visit_start_date, 
         visit_start_datetime, visit_end_date, visit_end_datetime, visit_type_concept_id,
         provider_id, care_site_id, visit_source_value, visit_source_concept_id,
         admitting_source_concept_id, admitting_source_value, discharge_to_concept_id,
         discharge_to_source_value, preceding_visit_occurrence_id)

# Formatting data type: date(as.Date), datetype(as.POSIXct)
type_visit <- read.csv("type/type_visit.csv")

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
VISIT_OCCURRENCE[,I] <- lapply(VISIT_OCCURRENCE[,I], as.integer)
VISIT_OCCURRENCE[,c50] <- lapply(VISIT_OCCURRENCE[,c50], as.character)
str(VISIT_OCCURRENCE)
rm(d,dt,i,I,c50,join_table_visit,join_table_visit_discharge,type_visit)




#---- CONDITION_OCCURRENCE ----
#1) extract required variables for CDM mapping & change name
condition_occurrence <- data_ED %>% 
  select(PT_ID, U_ID,  CC) %>%
  mutate(CONDITION_OCCURRENCE_ID = U_ID,
         VISIT_OCCURRENCE_ID = U_ID,
         CONDITION_SOURCE_VALUE=ifelse(CC == "DOA", "DOA", NA))


#2) import join table for CDM table
join_table_condition <- read_xlsx("join_table/join_table_condition.xlsx")


#3) generate CONDITION_OCCURRENCE TABLE through CDM mapping
#   arrange variables according to the OMOP table order after CDM mapping
CONDITION_OCCURRENCE <- condition_occurrence %>%
  left_join(personID_sourceValue_map, by = c("PT_ID" = "person_source_value")) %>%
  mutate(condition_occurrence_id = CONDITION_OCCURRENCE_ID,
         # condition_concept_id = NA,         # Exist join_table
         condition_start_date =NA,
         condition_start_datetime = NA,
         condition_end_date = NA,
         condition_end_datetime = NA,
         condition_type_concept_id = NA,
         condition_status_concept_id = NA,
         stop_reason = NA,
         provider_id = NA,
         visit_occurrence_id = VISIT_OCCURRENCE_ID,
         visit_detail_id = NA,
         condition_source_value = CONDITION_SOURCE_VALUE,
         # condition_source_concept_id = NA, # Exist join_table
         condition_status_source_value = NA) %>%
  left_join(join_table_condition, by = "condition_source_value") %>%
  select(condition_occurrence_id, person_id, condition_concept_id, condition_start_date,
         condition_start_datetime, condition_end_date, condition_end_datetime,
         condition_type_concept_id, condition_status_concept_id, stop_reason, 
         provider_id, visit_occurrence_id, visit_detail_id, 
         condition_source_value, condition_source_concept_id, condition_status_source_value)

# Formatting data type: date(as.Date), datetype(as.POSIXct)
type_cond <- read.csv("type/type_cond.csv")

for(i in 1:length(type_cond)){type_cond[,i] <- as.character(type_cond[,i])}
table(type_cond$Type)
d <- type_cond[type_cond$Type == "date",]$Field
dt <- type_cond[type_cond$Type == "datetime",]$Field
i <- type_cond[type_cond$Type == "integer",]$Field
c20 <- type_cond[type_cond$Type == "varchar(20)",]$Field
c50 <- type_cond[type_cond$Type == "varchar(50)",]$Field

CONDITION_OCCURRENCE[,d] <- lapply(CONDITION_OCCURRENCE[,d], as.Date)
CONDITION_OCCURRENCE[,dt] <- lapply(CONDITION_OCCURRENCE[,dt], as.POSIXct)
CONDITION_OCCURRENCE[,i] <- lapply(CONDITION_OCCURRENCE[,i], as.integer)
CONDITION_OCCURRENCE[,c20] <- lapply(CONDITION_OCCURRENCE[,c20], as.character)
CONDITION_OCCURRENCE[,c50] <- lapply(CONDITION_OCCURRENCE[,c50], as.character)
str(CONDITION_OCCURRENCE)
rm(d,dt,i,c20,c50,join_table_condition,type_cond)



#---- OBSERVATION ----
#1) extract required variables for CDM mapping & change name
# severity score(KTAS Level) : ED triage nurse Provider(40759929)
observation <- data_ED %>% 
  select(PT_ID, U_ID, ktas) %>%
  mutate(OBSERVATION_ID = U_ID,
         VISIT_OCCURRENCE_ID = U_ID,
         OBSERVATION_SOURCE_VALUE= "KTAS") %>%
  rename(VALUE_AS_NUMBER = ktas)


#2) import join table for CDM table
join_table_observation <- read_xlsx("join_table/join_table_observation.xlsx")


#3) generate OBSERVATION TABLE through CDM mapping
#   arrange variables according to the OMOP table order after CDM mapping
OBSERVATION <- observation %>%
  left_join(personID_sourceValue_map, by = c("PT_ID" = "person_source_value")) %>%
  mutate(observation_id = OBSERVATION_ID,
         # observation_concept_id = NA,                          # Exist join_table
         observation_date = NA, 
         observation_datetime = NA,
         observation_type_concept_id = NA,
         value_as_number = VALUE_AS_NUMBER,
         value_as_string = NA, 
         value_as_concept_id = NA, 
         qualifier_concept_id = NA, 
         unit_concept_id = NA, 
         provider_id = NA, 
         visit_occurrence_id = VISIT_OCCURRENCE_ID, 
         visit_detail_id = NA, 
         observation_source_value = OBSERVATION_SOURCE_VALUE,
         # observation_source_concept_id = NA,                   # Exist join_table
         unit_source_value = NA, 
         qualifier_source_value = NA) %>%
  left_join(join_table_observation, by = "observation_source_value") %>%
  select(observation_id, person_id, observation_concept_id, observation_date, observation_datetime,
         observation_type_concept_id, value_as_number, value_as_string, value_as_concept_id,
         qualifier_concept_id, unit_concept_id, provider_id, visit_occurrence_id,
         visit_detail_id, observation_source_value, observation_source_concept_id,
         unit_source_value, qualifier_source_value)

# Formatting data type: date(as.Date), datetype(as.POSIXct)
type_obs <- read.csv("type/type_obs.csv")

for(i in 1:length(type_obs)){type_obs[,i] <- as.character(type_obs[,i])}
table(type_obs$Type)
d <- type_obs[type_obs$Type == "date",]$Field
dt <- type_obs[type_obs$Type == "datetime",]$Field
f <- type_obs[type_obs$Type == "float",]$Field
i <- type_obs[type_obs$Type == "integer",]$Field
I <- type_obs[type_obs$Type == "Integer",]$Field
c50 <- type_obs[type_obs$Type == "varchar(50)",]$Field
c60 <- type_obs[type_obs$Type == "varchar(60)",]$Field

OBSERVATION[,d] <- lapply(OBSERVATION[,d], as.Date)
OBSERVATION[,dt] <- lapply(OBSERVATION[,dt], as.POSIXct)
OBSERVATION[,f] <- lapply(OBSERVATION[,f], as.double)
OBSERVATION[,i] <- lapply(OBSERVATION[,i], as.integer)
OBSERVATION[,I] <- lapply(OBSERVATION[,I], as.integer)
OBSERVATION[,c50] <- lapply(OBSERVATION[,c50], as.character)
OBSERVATION[,c60] <- lapply(OBSERVATION[,c60], as.character)
str(OBSERVATION)
rm(d,dt,f,i,I,c50,c60,join_table_observation,type_obs)



# Data Concatenate
df <- left_join(PERSON, VISIT_OCCURRENCE,by = intersect(names(PERSON),names(VISIT_OCCURRENCE)))
df <- left_join(df, CONDITION_OCCURRENCE, by = intersect(names(df),names(CONDITION_OCCURRENCE)))
df <- left_join(df, OBSERVATION, by = intersect(names(df),names(OBSERVATION)))
str(df)


# Save CDM data
write.csv(df, 'cdm_mapping.csv', row.names = F)



