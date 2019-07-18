# Graduation Cohort Application - Grad Rate Tab
# Evan Kramer
# 6/7/2017

library(tidyverse)
library(lubridate)
library(stringr)
library(haven)
library(readxl)

date = str_replace_all(today(), "-", "")
setwd("K:/ORP_accountability/projects/2018_graduation_rate/Data")

# Replicate Grad Rate tab
## Data
### Determine most recent file
temp = as.tbl(data.frame(file_name = list.files(), file_date = file.mtime(list.files())))
for(f in seq_along(list.files())) {
    if(str_detect(temp$file_name[f], "studentcohortdata") == T & temp$file_date[f] == max(temp$file_date, na.rm = T)) {
        g = read_csv(as.character(temp$file_name[f]))
    } 
}
rm(temp)

h = g %>% 
    mutate(included_in_cohort = (INCLUDED_IN_COHORT == "Y" & (REVISED_INCLUDED_IN_COHORT != "N" | is.na(REVISED_INCLUDED_IN_COHORT))) | 
               REVISED_INCLUDED_IN_COHORT == "Y" | 
               (INCLUDED_IN_COHORT == "P" & (is.na(REVISED_INCLUDED_IN_COHORT) | REVISED_INCLUDED_IN_COHORT == "Y")),
           included_in_cohort = ifelse(is.na(included_in_cohort), F, included_in_cohort)) 

a = h %>% 
    filter(DISTRICT_NO == 798) %>% 
    transmute(student_key = STUDENT_KEY,
              grad_cohort = included_in_cohort == T,
              grad_count = included_in_cohort == T & COMPLETION_TYPE %in% c(1, 11, 12, 13),
              grad_rate = round(100 * sum(included_in_cohort = T & COMPLETION_TYPE %in% c(1, 11, 12, 13), na.rm = T) / sum(included_in_cohort, na.rm = T), 1),
              n_denied = included_in_cohort == T & REVISED_INCLUDED_IN_COHORT == "Y",
              n_elig_no_doc = included_in_cohort == T & WITHDRAWAL_REASON %in% c(2, 5, 6, 8, 10, 17) & is.na(MODIFIED_DATE),
              n_no_wd_no_comp = included_in_cohort == T & is.na(WITHDRAWAL_REASON) & is.na(COMPLETION_TYPE),
              n_other_school_dist = included_in_cohort == T & WITHDRAWAL_REASON %in% c(3, 4)) 

h %>% 
    filter(DISTRICT_NO == 798 & included_in_cohort == T & REVISED_INCLUDED_IN_COHORT == "Y") %>% 
    count()

h %>% 
    filter(DISTRICT_NO == 940 & STUDENT_KEY == 4270076) %>% 
    select(STUDENT_KEY, COMPLETION_TYPE)

# District Level
## All Students    
b = h %>% 
    group_by(DISTRICT_NO) %>% 
    summarize(grad_cohort = sum(included_in_cohort == T, na.rm = T),
              grad_count = sum(included_in_cohort == T & COMPLETION_TYPE %in% c(1, 11, 12, 13), na.rm = T)) %>% 
    ungroup() %>% 
    mutate(subgroup = "All Students", SCHOOL_NO = 0)

## Racial/ethnic groups    
races = unique(g$RACE_ETHNICITY)
for(i in seq_along(races)) { 
    j = h %>% 
        filter(RACE_ETHNICITY == races[i]) %>% 
        group_by(DISTRICT_NO) %>% 
        summarize(grad_cohort = sum(included_in_cohort == T, na.rm = T),
                  grad_count = sum(included_in_cohort == T & COMPLETION_TYPE %in% c(1, 11, 12, 13), na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = races[i], SCHOOL_NO = 0)
    b = bind_rows(b, j)
}

## Male
j = h %>% 
    filter(GENDER == "M") %>%
    group_by(DISTRICT_NO) %>% 
    summarize(grad_cohort = sum(included_in_cohort == T, na.rm = T),
              grad_count = sum(included_in_cohort == T & COMPLETION_TYPE %in% c(1, 11, 12, 13), na.rm = T)) %>% 
    ungroup() %>% 
    mutate(subgroup = "Male", SCHOOL_NO = 0)
b = bind_rows(b, j)

## Female
j = h %>% 
    filter(GENDER == "F") %>% 
    group_by(DISTRICT_NO) %>% 
    summarize(grad_cohort = sum(included_in_cohort == T, na.rm = T),
              grad_count = sum(included_in_cohort == T & COMPLETION_TYPE %in% c(1, 11, 12, 13), na.rm = T)) %>% 
    ungroup() %>% 
    mutate(subgroup = "Female", SCHOOL_NO = 0)
b = bind_rows(b, j)

## ECON
j = h %>% 
    filter(ECON_DIS == "Y") %>% 
    group_by(DISTRICT_NO) %>% 
    summarize(grad_cohort = sum(included_in_cohort == T, na.rm = T),
              grad_count = sum(included_in_cohort == T & COMPLETION_TYPE %in% c(1, 11, 12, 13), na.rm = T)) %>% 
    ungroup() %>% 
    mutate(subgroup = "ECON", SCHOOL_NO = 0)
b = bind_rows(b, j)

## Migrant
j = h %>% 
    filter(MIGRANT == "Y") %>%
    group_by(DISTRICT_NO) %>% 
    summarize(grad_cohort = sum(included_in_cohort == T, na.rm = T),
              grad_count = sum(included_in_cohort == T & COMPLETION_TYPE %in% c(1, 11, 12, 13), na.rm = T)) %>% 
    ungroup() %>% 
    mutate(subgroup = "Migrant", SCHOOL_NO = 0)
b = bind_rows(b, j)

## SPED
j = h %>% 
    filter(SPED == "Y") %>% 
    group_by(DISTRICT_NO) %>% 
    summarize(grad_cohort = sum(included_in_cohort == T, na.rm = T),
              grad_count = sum(included_in_cohort == T & COMPLETION_TYPE %in% c(1, 11, 12, 13), na.rm = T)) %>% 
    ungroup() %>% 
    mutate(subgroup = "SPED", SCHOOL_NO = 0)
b = bind_rows(b, j)

## ELL
j = h %>% 
    filter(ELL == "Y") %>% 
    group_by(DISTRICT_NO) %>% 
    summarize(grad_cohort = sum(included_in_cohort == T, na.rm = T),
              grad_count = sum(included_in_cohort == T & COMPLETION_TYPE %in% c(1, 11, 12, 13), na.rm = T)) %>% 
    ungroup() %>% 
    mutate(subgroup = "ELL", SCHOOL_NO = 0)
b = bind_rows(b, j)

## Homeless
j = h %>% 
    filter(HOMELESS == "Y") %>% 
    group_by(DISTRICT_NO) %>% 
    summarize(grad_cohort = sum(included_in_cohort == T, na.rm = T),
              grad_count = sum(included_in_cohort == T & COMPLETION_TYPE %in% c(1, 11, 12, 13), na.rm = T)) %>% 
    ungroup() %>% 
    mutate(subgroup = "Homeless", SCHOOL_NO = 0)
b = bind_rows(b, j)

## CTE
j = h %>% 
    filter(CTE == "Y") %>% 
    group_by(DISTRICT_NO) %>% 
    summarize(grad_cohort = sum(included_in_cohort == T, na.rm = T),
              grad_count = sum(included_in_cohort == T & COMPLETION_TYPE %in% c(1, 11, 12, 13), na.rm = T)) %>% 
    ungroup() %>% 
    mutate(subgroup = "CTE", SCHOOL_NO = 0)
b = bind_rows(b, j)

# School Level
## All Students    
a = h %>% 
    group_by(DISTRICT_NO, SCHOOL_NO) %>% 
    summarize(grad_cohort = sum(included_in_cohort == T, na.rm = T),
              grad_count = sum(included_in_cohort == T & COMPLETION_TYPE %in% c(1, 11, 12, 13), na.rm = T)) %>% 
    ungroup() %>% 
    mutate(subgroup = "All Students")

## Racial/ethnic groups    
races = c("A", "B", "H", "I", "P", "W")
for(i in seq_along(races)) { 
    j = h %>% 
        filter(RACE_ETHNICITY == races[i]) %>% 
        group_by(DISTRICT_NO, SCHOOL_NO) %>% 
        summarize(grad_cohort = sum(included_in_cohort == T, na.rm = T),
                  grad_count = sum(included_in_cohort == T & COMPLETION_TYPE %in% c(1, 11, 12, 13), na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = races[i])
    a = bind_rows(a, j)
}

## Male
j = h %>% 
    filter(GENDER == "M") %>%
    group_by(DISTRICT_NO, SCHOOL_NO) %>% 
    summarize(grad_cohort = sum(included_in_cohort == T, na.rm = T),
              grad_count = sum(included_in_cohort == T & COMPLETION_TYPE %in% c(1, 11, 12, 13), na.rm = T)) %>% 
    ungroup() %>% 
    mutate(subgroup = "Male")
a = bind_rows(a, j)

## Female
j = h %>% 
    filter(GENDER == "F") %>% 
    group_by(DISTRICT_NO, SCHOOL_NO) %>% 
    summarize(grad_cohort = sum(included_in_cohort == T, na.rm = T),
              grad_count = sum(included_in_cohort == T & COMPLETION_TYPE %in% c(1, 11, 12, 13), na.rm = T)) %>% 
    ungroup() %>% 
    mutate(subgroup = "Female")
a = bind_rows(a, j)

## ECON
j = h %>% 
    filter(ECON_DIS == "Y") %>% 
    group_by(DISTRICT_NO, SCHOOL_NO) %>% 
    summarize(grad_cohort = sum(included_in_cohort == T, na.rm = T),
              grad_count = sum(included_in_cohort == T & COMPLETION_TYPE %in% c(1, 11, 12, 13), na.rm = T)) %>% 
    ungroup() %>% 
    mutate(subgroup = "ECON")
a = bind_rows(a, j)

## Migrant
j = h %>% 
    filter(MIGRANT == "Y") %>%
    group_by(DISTRICT_NO, SCHOOL_NO) %>% 
    summarize(grad_cohort = sum(included_in_cohort == T, na.rm = T),
              grad_count = sum(included_in_cohort == T & COMPLETION_TYPE %in% c(1, 11, 12, 13), na.rm = T)) %>% 
    ungroup() %>% 
    mutate(subgroup = "Migrant")
a = bind_rows(a, j)

## SPED
j = h %>% 
    filter(SPED == "Y") %>% 
    group_by(DISTRICT_NO, SCHOOL_NO) %>% 
    summarize(grad_cohort = sum(included_in_cohort == T, na.rm = T),
              grad_count = sum(included_in_cohort == T & COMPLETION_TYPE %in% c(1, 11, 12, 13), na.rm = T)) %>% 
    ungroup() %>% 
    mutate(subgroup = "SPED")
a = bind_rows(a, j)

## ELL
j = h %>% 
    filter(ELL == "Y") %>% 
    group_by(DISTRICT_NO, SCHOOL_NO) %>% 
    summarize(grad_cohort = sum(included_in_cohort == T, na.rm = T),
              grad_count = sum(included_in_cohort == T & COMPLETION_TYPE %in% c(1, 11, 12, 13), na.rm = T)) %>% 
    ungroup() %>% 
    mutate(subgroup = "ELL")
a = bind_rows(a, j)

## Homeless
j = h %>% 
    filter(HOMELESS == "Y") %>% 
    group_by(DISTRICT_NO, SCHOOL_NO) %>% 
    summarize(grad_cohort = sum(included_in_cohort == T, na.rm = T),
              grad_count = sum(included_in_cohort == T & COMPLETION_TYPE %in% c(1, 11, 12, 13), na.rm = T)) %>% 
    ungroup() %>% 
    mutate(subgroup = "Homeless")
a = bind_rows(a, j)

## CTE
j = h %>% 
    filter(CTE == "Y") %>% 
    group_by(DISTRICT_NO, SCHOOL_NO) %>% 
    summarize(grad_cohort = sum(included_in_cohort == T, na.rm = T),
              grad_count = sum(included_in_cohort == T & COMPLETION_TYPE %in% c(1, 11, 12, 13), na.rm = T)) %>% 
    ungroup() %>% 
    mutate(subgroup = "CTE")
a = bind_rows(a, j)

# Calculate rates and spread
a = bind_rows(b, a) %>% 
    arrange(DISTRICT_NO, SCHOOL_NO) %>% 
    mutate(grad_rate = round(100 * grad_count / grad_cohort, 2)) %>% 
    select(-starts_with("grad_c")) %>% 
    spread(subgroup, grad_rate) %>% 
    select(District = DISTRICT_NO,
           School = SCHOOL_NO,
           `African American` = B,
           Asian = A,
           `Pacific Islander` = P,
           Hispanic = H,
           `Native American` = I,
           White = W,
           Male, Female, ECON, Migrant, SPED, ELL, Homeless, CTE, `All Students`)
a$School[a$School == 0] = NA

# Output file
write_excel_csv(a, "C:/Users/CA19130/Downloads/Grad_Rate_Table_Comparison.csv", na = "")


