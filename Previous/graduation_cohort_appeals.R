# Graduation Cohort Appeals
# Evan Kramer
# 6/22/2017

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

date = str_replace_all(as.character(Sys.Date()), "-", "")
setwd("K:/ORP_accountability/projects/2017_graduation_rate/Appeals/District Documents")

# Compile Appeals Tracker
lf = list.files()
sl = rep(NA, length(lf))
a = data.frame(x1 = NA, x2 = NA, x3 = NA, x4 = NA, x5 = NA, x6 = NA, x7 = NA, x8 = NA)
names(a) = c("Student ID", "District Number", "District Name", "School Number", 
             "Student Last Name", "Reason for Appeal", "Explanation (Required)", "Date") 
for(i in seq_along(lf)) {
    system = as.numeric(str_replace_all(substr(lf[i], 1, 3), "_", ""))
    sl[i] = system
    if(!is.na(system)) {
        c = read.xlsx(paste0("K:/ORP_accountability/projects/2017_graduation_rate/Appeals/District Documents/", lf[i]),
                      sheetIndex = 1) 
        b = read_excel(paste0("K:/ORP_accountability/projects/2017_graduation_rate/Appeals/District Documents/", lf[i]),
                       sheet = 2) %>% 
            filter(!is.na(`Student ID`)) %>% 
            mutate(Date = as.Date.numeric(as.numeric(as.character(c[1, 2])), origin = "1899-12-30"),
                   `Contact Email` = as.character(c[7, 2]))
        
        a = bind_rows(a, b) %>% 
            filter(!is.na(`Student ID`) & !is.na(`Reason for Appeal`) & !is.na(`Explanation (Required)`))
    }
}

a = a %>% 
    arrange(Date, `District Number`, `School Number`, `Student ID`)

break
write.xlsx(a, paste0("K:/ORP_accountability/projects/2017_graduation_rate/Appeals/Appeals Trackers/Appeals Tracker.xlsx"),
           row.names = F, append = F, sheetName = "Individual")

# Estimated appealable situations
appeals = left_join(read_csv("K:/ORP_accountability/projects/2017_graduation_rate/Data/studentcohortdata_20170621.csv"),
                    read_csv("K:/ORP_accountability/projects/2017_graduation_rate/Data/studentcohortdocs_20170621.csv"),
                    by = c("Student key" = "Student Key SUM")) %>% 
    mutate(included_in_cohort = (`Included in cohort` == "Y" & (`Revised Included In Cohort` != "N" | is.na(`Revised Included In Cohort`))) | 
               `Revised Included In Cohort` == "Y" | 
               (`Included in cohort` == "P" & (is.na(`Revised Included In Cohort`) | `Revised Included In Cohort` == "Y")),
           included_in_cohort = ifelse(is.na(included_in_cohort), F, included_in_cohort),
           potential_appeal = `Revised Included In Cohort` == "Y" | 
               included_in_cohort == T & is.na(`Withdrawal reason SUM`) & is.na(`Completion type`)) %>% 
    group_by(`District no`) %>% 
    summarize(grad_rate_no_appeals = 100 * sum(`Completion type` == 1 & included_in_cohort == T, na.rm = T) / 
                  sum(included_in_cohort, na.rm = T),
              grad_rate_all_appeals = 100 * sum(`Completion type` == 1 & included_in_cohort == T, na.rm = T) / 
                  (sum(included_in_cohort == T & (is.na(potential_appeal) | potential_appeal == F)))) %>% 
    mutate(diff = grad_rate_all_appeals - grad_rate_no_appeals,
           pct_diff = 100 * diff / grad_rate_no_appeals) %>% 
    arrange(desc(pct_diff))