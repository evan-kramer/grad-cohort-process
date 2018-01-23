# Graduation Cohort Analysis - Check Manual Updates
# Evan Kramer
# 7/21/2017

library(tidyverse)
library(lubridate)
library(stringr)
library(haven)
library(readxl)

date = str_replace_all(as.character(Sys.Date()), "-", "")
read_dates = c("20161207", "20170202", "20170207", "20170210", "20170217", "20170224", 
               "20170302", "20170313", "20170317", "20170324", "20170331", "20170407",
               "20170419", "20170421", "20170428", "20170508", "20170515", "20170519",
               "20170526", "20170602", "20170606", "20170607", "20170608", "20170609",
               "20170612", "20170613", "20170621", "20170623", "20170630", "20170707", 
               "20170714", "20170721", "20170724")

setwd("K:/ORP_accountability/projects/2017_graduation_rate/Data")

# Check manual update for summer completion data
g1 = read_csv(str_c("studentcohortdata_", read_dates[length(read_dates) - 2], ".csv")) %>% 
    left_join(read_csv(str_c("studentcohortdocs_", read_dates[length(read_dates) - 2], ".csv")),
              by = c("Student key" = "Student Key SUM")) %>% 
    mutate(included_in_cohort = (`Included in cohort` == "Y" & (`Revised Included In Cohort` != "N" | is.na(`Revised Included In Cohort`))) | 
                  `Revised Included In Cohort` == "Y" | 
                  (`Included in cohort` == "P" & (is.na(`Revised Included In Cohort`) | `Revised Included In Cohort` == "Y")),
           included_in_cohort = ifelse(is.na(included_in_cohort), F, included_in_cohort))

g2 = read_csv(str_c("studentcohortdata_", read_dates[length(read_dates) - 1], ".csv")) %>% 
    left_join(read_csv(str_c("studentcohortdocs_", read_dates[length(read_dates) - 1], ".csv")),
              by = c("Student key" = "Student Key SUM")) %>%
    mutate(included_in_cohort = `Included in cohort` == "Y")

g3 = read_csv(str_c("studentcohortdata_", read_dates[length(read_dates)], ".csv")) %>% 
    left_join(read_csv(str_c("studentcohortdocs_", read_dates[length(read_dates)], ".csv")),
              by = c("Student key" = "Student Key SUM")) %>%
    mutate(included_in_cohort = `Included in cohort` == "Y")



cd = read_csv("K:/ORP_accountability/projects/2017_graduation_rate/Data/completion_document_manual_update_check.csv") %>% 
    transmute(student_key = `Student key`, comp_type = `Completion Type`, comp_date = dmy(`Completion Date`), 
              comp_period = `Completion Period`, cd_status = `Cd Status`) %>% 
    filter(comp_date >= ymd(20170609) & cd_status == "A") %>% 
    left_join(g1, by = c("student_key" = "Student key")) %>% 
    select(student_key, starts_with("comp"), starts_with("Comp"))





