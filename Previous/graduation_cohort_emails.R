# Graduation Cohort Emails
# Evan Kramer
# 6/5/2017

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

rm(list = ls())
date = str_replace_all(as.character(Sys.Date()), "-", "")
setwd("K:/ORP_accountability/projects/2017_graduation_rate/Data")
read_dates = c("20161207", "20170202", "20170207", "20170210", "20170217", "20170224", 
               "20170302", "20170313", "20170317", "20170324", "20170331", "20170407",
               "20170419", "20170421", "20170428", "20170508", "20170515", "20170519",
               "20170526", "20170602", "20170606", "20170607", "20170608", "20170609",
               "20170612")

read_csv("K:/ORP_accountability/data/2016_graduation_rate/student_level_20161201.csv") %>%
    filter(student_key %in% c(2985110, 4347174))    
    
    
break



# Data checks
g = read_csv(str_c("studentcohortdata_", read_dates[length(read_dates)], ".csv")) %>% 
    left_join(read_csv(str_c("studentcohortdocs_", read_dates[length(read_dates)], ".csv")),
              by = c("Student key" = "Student Key SUM")) 

j = g %>% 
    mutate(included_in_cohort = (`Included in cohort` == "Y" & (`Revised Included In Cohort` != "N" | is.na(`Revised Included In Cohort`))) | 
               `Revised Included In Cohort` == "Y" | 
               (`Included in cohort` == "P" & (is.na(`Revised Included In Cohort`) | `Revised Included In Cohort` == "Y")),
           included_in_cohort = ifelse(is.na(included_in_cohort), F, included_in_cohort)) 

j %>% 
    filter(`District no` == 951) %>% 
    group_by(`District no`) %>% 
    summarize(grad_cohort = sum(included_in_cohort == T, na.rm = T),
              grad_count = sum(included_in_cohort == T & `Completion type` == 1, na.rm = T),
              grad_rate = round(100 * sum(included_in_cohort == T & `Completion type` == 1, na.rm = T) / sum(included_in_cohort == T, na.rm = T), 1),
              n_denied = sum(included_in_cohort == T & `Revised Included In Cohort` == "Y", na.rm = T),
              n_elig_no_doc = sum(included_in_cohort == T & `Withdrawal reason SUM` %in% c(2, 5, 6, 8, 10, 17) & is.na(`Modified Date`), na.rm = T),
              n_no_wd_no_comp = sum(included_in_cohort == T & is.na(`Withdrawal reason SUM`) & is.na(`Completion type`))) %>% 
    arrange(grad_rate) %>% 
    print()


# Phase I deadline emails to districts
## Read in prior year student-level file
p = read_csv("K:/ORP_accountability/data/2016_graduation_rate/student_level_20161201.csv") %>% 
    group_by(system) %>% 
    summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
              grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(grad_rate = round(100 * grad_count / grad_cohort, 1))

## Calculate current flags and rates
d = read_csv(paste0("studentcohortdata_", read_dates[length(read_dates)], ".csv")) %>% 
    full_join(read_csv(paste0("studentcohortdocs_", read_dates[length(read_dates)], ".csv")), 
              by = c("Student key" = "Student Key SUM")) %>% 
    mutate(grad_cohort = `Included in cohort` == "Y" | 
               (`Included in cohort` == "P" & (`Revised Included In Cohort` == "Y" | is.na(`Revised Included In Cohort`))),
           grad_count = grad_cohort == T & `Completion type` == 1) %>% 
    group_by(`District no`) %>% 
    summarize(doc_denied = sum(`Revised Included In Cohort` == "Y", na.rm = T),
              elig_no_doc = sum(`Withdrawal reason SUM` %in% c(2, 5, 6, 8, 10, 17) & is.na(`Save As Filename`), na.rm = T),
              wd_to_other_school_dist = sum(`Withdrawal reason SUM` %in% c(3, 4), na.rm = T),
              no_comp_no_wd = sum(is.na(`Completion type`) & is.na(`Withdrawal reason SUM`), na.rm = T),
              current_grad_cohort = sum(grad_cohort, na.rm = T),
              current_grad_count = sum(grad_count, na.rm = T)) %>% 
    mutate(current_grad_rate = round(100 * current_grad_count / current_grad_cohort, 1),
           `District no` = as.double(`District no`)) %>% 
    left_join(read_dta("C:/Users/CA19130/Documents/Data/Crosswalks/system_system_name_crosswalk.dta"),
              by = c("District no" = "system")) %>% 
    dplyr::select(system = `District no`, system_name, everything()) %>% 
    left_join(p, by = "system") %>% 
    mutate(system_name = ifelse(system == 90, "Carroll County", system_name))

## District contacts
c = openxlsx::read.xlsx("http://www.tennessee.gov/assets/entities/education/attachments/eis_contacts.xlsx") %>% 
    filter(`Prim/Sec` == "P") %>% 
    group_by(X1) %>% 
    summarize(contact_email = first(EMAIL.ADDRESS)) %>%
    rename(system = X1) %>% 
    mutate(system = as.numeric(system)) %>% 
    ungroup() %>% 
    full_join(openxlsx::read.xlsx("C:/Users/CA19130/Documents/Communications and Documentation/Contacts/District Contact List.xlsx"), 
              by = c("system" = "leaid"))

## Join all objects together
g = left_join(d, c, by = "system") %>% 
    left_join(p, by = "system") %>% 
    select(system, system_name, doc_denied, elig_no_doc, wd_to_other_school_dist,
           no_comp_no_wd, starts_with("current_"), prev_grad_rate = grad_rate.x, 
           dir_last = last, dir_email = email, contact_email) %>% 
    filter(system != 941 & system != 951 & system != 970) # check that email suffixes match? use str_split()

## Write all files to same workbook 
dir = "K:/ORP_accountability/projects/2017_graduation_rate/Coding/VBA"
f = "Pre-Deadline Flag Email.xlsx"
#xlsx::write.xlsx(as.data.frame(g), "K:/ORP_accountability/projects/2017_graduation_rate/Coding/VBA/Pre-Deadline Flag Email.xlsx", 
#                 row.names = F, showNA = F)

h = filter(g, system %in% c(661, 941, 951, 963, 310, 970, 620, 670))
xlsx::write.xlsx(as.data.frame(h), "K:/ORP_accountability/projects/2017_graduation_rate/Coding/VBA/Pre-Deadline Email Follow Up.xlsx", 
                 row.names = F, showNA = F)


break



# Phase II deadline emails to districts
## Read in prior year student-level file
### use p above

## Calculate current flags and rates
d2 = read_csv(paste0("studentcohortdata_", read_dates[length(read_dates)], ".csv")) %>% 
    left_join(read_csv(paste0("studentcohortdocs_", read_dates[length(read_dates)], ".csv")), 
              by = c("Student key" = "Student Key SUM")) %>% 
    mutate(grad_cohort = `Included in cohort` == "Y" | 
               (`Included in cohort` == "P" & (`Revised Included In Cohort` == "Y" | is.na(`Revised Included In Cohort`))),
           grad_count = grad_cohort == T & `Completion type` == 1,
           `Completion Date` = dmy(`Completion Date`)) %>%
    group_by(`District no`) %>% 
    summarize(current_grad_cohort = sum(grad_cohort, na.rm = T),
              current_grad_count = sum(grad_count, na.rm = T),
              summer_grads = sum(`Completion Date` >= ymd(20170605), na.rm = T)) %>% 
    mutate(current_grad_rate = round(100 * current_grad_count / current_grad_cohort, 1),
           `District no` = as.double(`District no`)) %>% 
    left_join(read_dta("C:/Users/CA19130/Documents/Data/Crosswalks/system_system_name_crosswalk.dta"),
              by = c("District no" = "system")) %>% 
    dplyr::select(system = `District no`, system_name, everything()) %>% 
    left_join(p, by = "system") %>% 
    mutate(system_name = ifelse(system == 90, "Carroll County", system_name))

## Number of appeals
a = read_excel("K:/ORP_accountability/projects/2017_graduation_rate/Appeals/Appeals Trackers/Appeals Tracker.xlsx") %>% 
    group_by(`District Number`) %>% 
    summarize(n_appeals = n()) %>%
    ungroup()

## District contacts
### use c above

## Join all objects together
g2 = left_join(d2, a, by = c("system" = "District Number")) %>% 
    left_join(c, by = "system") %>% 
    left_join(p, by = "system") %>% 
    select(system, system_name, summer_grads, n_appeals, starts_with("current_"),
           prev_grad_rate = grad_rate.x, dir = super, dir_last = last, dir_email = email, contact_email) %>% 
    mutate(n_appeals = ifelse(is.na(n_appeals), 0, n_appeals))

## Save file if it's not in the directory or if it has been more than five days
f2 = "Phase II Deadline Email.xlsx"
if(f2 %in% list.files(dir) == F | file.info(paste(dir, f2, sep = "/"))$mtime < now() - days(1)) {
    xlsx::write.xlsx(as.data.frame(g2), "K:/ORP_accountability/projects/2017_graduation_rate/Coding/VBA/Phase II Deadline Email.xlsx", row.names = F)
}