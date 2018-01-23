# Graduation Cohort Shelby-Municipals-ASD Issues
# Last updated by: Evan Kramer
# Last updated on: 5/25/2017

library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(haven)
library(readr)
library(openxlsx)
library(xlsx)
library(readxl)
library(httr)
library(ggplot2)
library(googleVis)
library(statisticalModeling)

# Problems with Municipals and ASD Schools
a = read_csv("C:/Users/CA19130/Downloads/CohortDownload_2016-2017 (15).csv") # ASD
b = read_csv("C:/Users/CA19130/Downloads/CohortDownload_2016-2017 (16).csv") # Shelby

a %>% filter(SCHOOL_NO == 8140) # 153 in ASD
b %>% filter(SCHOOL_NO == 2335) # 37 in Shelby

a = data.frame()
for(i in 15:19) {
    b = paste0("C:/Users/CA19130/Downloads/CohortDownload_2016-2017 (", i, ").csv") %>% 
        read_csv() %>% 
        mutate(student_key = as.numeric(STUDENT_KEY), system = as.numeric(DISTRICT_NO),
               school = as.numeric(SCHOOL_NO), cohort_year = as.numeric(COHORTYEAR), 
               calc_from = as.numeric(CALC_FROM), grade = as.numeric(ASSIGNMENT), 
               year_withdrawn = as.numeric(YEAR_WITHDRAWN),
               withdrawal_reason = as.numeric(WITHDRAWAL_REASON),
               revised_included_in_cohort = REVISED_INCLUDED_IN_COHORT) %>% 
        select(student_key, system, school, calc_from, year_withdrawn, 
               withdrawal_reason, revised_included_in_cohort)
    a = bind_rows(a, b)
}

## Where did students count who withdrew from a municipal school when it was under Shelby's jurisdiction? - Municipals
s = read_csv("K:/ORP_accountability/data/2016_graduation_rate/student_level_20161201.csv")
s %>% filter(system >= 793 & system <= 798 & year_withdrawn <= 2013) %>% 
    select(student_key, year_withdrawn, system, school)

## Who uploaded documents for the students who withdrew from municipal schools when they were under Shelby's jurisdiction? - Municipals
s %>% filter(system >= 793 & system <= 798 & year_withdrawn <= 2013) %>% 
    group_by(user_id) %>% 
    summarize(n = n())

## How many students withdrew from municipal schools when they were under Shelby's jurisdiction and had a valid withdrawal code but no documentation?
s %>% filter(system >= 793 & system <= 798 & year_withdrawn <= 2013) %>% 
    group_by(system) %>% 
    summarise(n_elig = sum(withdrawal_reason %in% c(2,5,6,8,10,17), na.rm = T),
              n_upload = sum(withdrawal_reason %in% c(2,5,6,8,10,17) & !is.na(upload_date), na.rm = T),
              n_no_upload = sum(withdrawal_reason %in% c(2,5,6,8,10,17) & is.na(upload_date), na.rm = T))

## How did municipals graduation rates change from 2014-15 to 2015-16?
p = read_csv("K:/ORP_accountability/projects/2016_graduation_rate/2015 Graduation Cohort Materials for Evan and Mary/Student-Level Data/2011 Cohort_110615 (FINAL).csv") %>% 
    group_by(`District no`) %>% 
    summarize(grad_cohort = sum(`Included in cohort` == "Y", na.rm = T),
              grad_count = sum(`Included in cohort` == "Y" & `Completion type` == 1, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(grad_rate2015 = round(100 * grad_count / grad_cohort, 1)) %>% 
    filter(between(`District no`, 793, 798))

s %>% 
    group_by(system) %>% 
    summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
              grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(grad_rate2016 = round(100 * grad_count / grad_cohort, 1)) %>% 
    filter(between(system, 793, 798)) %>% 
    full_join(p, by = c("system" = "District no")) %>% 
    select(system, starts_with("grad_rate")) %>% 
    mutate(diff = grad_rate2016 - grad_rate2015)

## Where are these students being counted in the current file?
paste0("K:/ORP_accountability/projects/2017_graduation_rate/Data/studentcohortdata_", read_dates[length(read_dates)], ".csv") %>% 
    read_csv() %>% 
    left_join(read_csv(paste0("K:/ORP_accountability/projects/2017_graduation_rate/Data/studentcohortdocs_", read_dates[length(read_dates)], ".csv")),
              by = c("Student key" = "Student Key SUM")) %>% 
    filter(between(`District no`, 792, 798) & `Year withdrawn` <= 2014) %>% 
    group_by(`District no`, `School no`) %>% 
    summarize(n())

### Counts from the individual district file download on the Cohort application 20170417 3:36:00
read_csv("C:/Users/CA19130/Downloads/CohortDownload_2016-2017 (22).csv") %>% 
    filter(DISTRICT_NO == 792) %>% 
    group_by(SCHOOL_NO) %>% 
    summarize(n())

### Counts from the last file I pulled from the database
paste0("K:/ORP_accountability/projects/2017_graduation_rate/Data/studentcohortdata_", read_dates[length(read_dates)], ".csv") %>% 
    read_csv() %>% 
    left_join(read_csv(paste0("K:/ORP_accountability/projects/2017_graduation_rate/Data/studentcohortdocs_", read_dates[length(read_dates)], ".csv")),
              by = c("Student key" = "Student Key SUM")) %>% 
    filter(`District no` == 792) %>% 
    group_by(`School no`) %>% 
    summarize(n())

### Counts from the file Catherine just pulled (20170417 3:45:00) from the database
read_csv("K:/Research_Transfers/Data_Management/COHORTDATA 2013.csv") %>% 
    filter(DISTRICT_NO == 792) %>% 
    group_by(SCHOOL_NO) %>% 
    summarize(n())

### File from 20170418 9:10:00
read_csv("C:/Users/CA19130/Downloads/CohortDownload_2016-2017 (23).csv") %>% 
    filter(DISTRICT_NO == 792) %>% 
    group_by(SCHOOL_NO) %>% 
    summarize(n())
