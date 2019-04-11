# Graduation Cohort Reminder Emails
# Evan Kramer
# 4/11/2019

options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
library(RJDBC)

date = str_replace_all(today(), "-", "")
phase1 = F
phase2 = F
checks = F

# Phase I
if(phase1 == T) {
  # Data
  ## From EIS
  con = dbConnect(
    JDBC("oracle.jdbc.OracleDriver", classPath="C:/Users/CA19130/Downloads/ojdbc6.jar"), 
    readRegistry("Environment", "HCU")$EIS_MGR_CXN_STR,
    "EIS_MGR",
    readRegistry("Environment", "HCU")$EIS_MGR_PWD
  )
  
  student_level_current = as.tbl(dbGetQuery(con, "
  SELECT scd.student_key, district_no, school_no, included_in_cohort, revised_included_in_cohort, withdrawal_reason,
    completion_type, modified_date, user_id, status, comments, reviewer_user_id, reviewed_date, cte
  FROM studentcohortdocs doc
  RIGHT OUTER JOIN studentcohortdata scd on scd.student_key = doc.student_key
  WHERE scd.cohortyear = EXTRACT(year from sysdate) - 4")) 
    
  ## From prior cohort files
  student_level_prior = read_csv(str_c("N:/ORP_accountability/data/", year(today()) - 1, "_graduation_rate/student_level.csv"))
  
  ## Compile 
  grad_data = mutate(student_level_current, 
                     grad_cohort = (INCLUDED_IN_COHORT == "Y" & (REVISED_INCLUDED_IN_COHORT != "N" | is.na(REVISED_INCLUDED_IN_COHORT))) | 
                                      (INCLUDED_IN_COHORT == "P" & (REVISED_INCLUDED_IN_COHORT == "Y" | is.na(REVISED_INCLUDED_IN_COHORT)))) %>% 
    group_by(DISTRICT_NO) %>% 
    summarize(doc_denied = sum(grad_cohort == T & STATUS == 2, na.rm = T),
              elig_no_doc = sum(grad_cohort == T & WITHDRAWAL_REASON %in% c(2, 5, 6, 8, 10, 17)),
              wd_to_other_school_dist = sum(grad_cohort == T & WITHDRAWAL_REASON %in% c(3, 4)),
              no_comp_no_wd = sum(grad_cohort == T & is.na(COMPLETION_TYPE) & is.na(WITHDRAWAL_REASON)),
              current_grad_cohort = sum(grad_cohort, na.rm = T),
              current_grad_count = sum(grad_cohort == T & COMPLETION_TYPE %in% c(1, 11, 12, 13))) %>% 
    ungroup() %>% 
    mutate(current_grad_rate = round(100 * current_grad_count / current_grad_cohort, 1)) %>% 
    rename(system = DISTRICT_NO) %>% 
    filter(!is.na(system)) %>% 
    # Previous graduation rate
    left_join(
      group_by(student_level_prior, system) %>% 
        summarize(prev_grad_rate = round(100 * sum(completion_type %in% c(1, 11, 12, 13) & included_in_cohort == "Y", na.rm = T) / 
                                             sum(included_in_cohort == "Y", na.rm = T) + 1e-9, 1)), 
      by = "system"
    ) %>%
    # Crosswalk district names
    left_join(read_dta("C:/Users/CA19130/Documents/Data/Crosswalks/system_system_name_crosswalk.dta"), by = "system") %>% 
    mutate(system_name = ifelse(system == 90, "Carroll County", system_name)) %>% 
    # Email addresses
    left_join(readxl::read_excel("N:/ORP_accountability/projects/Evan/Communications and Documentation/District Contact List.xlsm") %>% 
                transmute(system, dir_last = director_last, dir_email = director_email, cohort_email = str_to_lower(cohort_email)), 
              by = "system") %>%
    left_join(openxlsx::read.xlsx("https://www.tn.gov/content/dam/tn/education/technology/EIS/eis_contacts.xlsx") %>% 
                filter(Primary.or.Secondary.Contact == "P") %>% 
                transmute(system = ifelse(row_number() == 1, 985, as.numeric(X1)), eis_email = Email.Address) %>% 
                group_by(system) %>% 
                summarise(eis_email = first(str_to_lower(eis_email))) %>% 
                ungroup(), by = "system") %>% 
    select(starts_with("system"), everything()) %>% 
    mutate(eis_email= ifelse(is.na(cohort_email) | cohort_email != eis_email, eis_email, NA))
    
  ## Save file
  # xlsx::write.xlsx(as.data.frame(grad_data), 
  #                  str_c("N:/ORP_accountability/projects/", year(today()), "_graduation_rate/Coding/VBA/Mid-April Flag Email.xlsx"),
  #                  row.names = F, showNA = F)
} else {
  rm(phase1)
}

# Phase II
if(phase2 == T) {
  # Set directory
  setwd(str_c("N:/ORP_accountability/projects/", year(today()), "_graduation_rate"))
  
  # system, system_name, summer_grads, n_appeals, current_grad_cohort, current_grad_count, current_grad_rate, prev_grad_rate,
  # dir, dir_last, dir_email, contact_email
  ## Read in prior year student-level file
  prev = read_csv(str_c("N:/ORP_accountability/data/", year(today()) - 1, "_graduation_rate/grad_rate_base_EK.csv")) %>% 
    filter(system != 0 & school == 0 & subgroup == "All Students") %>% 
    transmute(system = as.numeric(system), prev_grad_rate = grad_rate)
  
  ## Determine most recent data file
  flis = data.frame(fl = list.files("Data")) %>% 
    mutate(fdate = ymd(str_sub(str_replace_all(fl, ".csv", ""), -8, -1))) %>% 
    arrange(desc(fdate)) %>% 
    summarize(fl = first(fl)) 
  
  ## Calculate current flags and rates
  curr = read_csv(str_c("Data/", flis$fl[1])) %>% 
    mutate(system = as.numeric(DISTRICT_NO),
           current_grad_cohort = INCLUDED_IN_COHORT == "Y",
           current_grad_count = current_grad_cohort == T & COMPLETION_TYPE %in% c(1, 11, 12, 13),
           summer_grads = current_grad_count == T & dmy(COMPLETION_DATE) >= ymd(str_c(year(today()), "0608"))) %>% 
    group_by(system) %>% 
    summarize_at(vars(starts_with("current"), summer_grads), funs(sum(., na.rm = T))) %>% 
    mutate(current_grad_rate = round(100 * current_grad_count / current_grad_cohort, 1)) %>% 
    ungroup() %>% 
    left_join(read_dta("C:/Users/CA19130/Documents/Data/Crosswalks/system_system_name_crosswalk.dta"),
              by = "system") %>% 
    select(system, system_name, everything()) %>% 
    left_join(prev, by = "system") %>% 
    mutate(system_name = ifelse(system == 90, "Carroll County", system_name)) %>% 
    filter(!is.na(system))
  
  ## Number of appeals
  apps = readxl::read_excel(str_c("N:/ORP_accountability/projects/", year(today()), "_graduation_rate/Appeals/Appeals Trackers/Appeals Tracker.xlsx")) %>% 
    group_by(`District Number`) %>% 
    summarize(n_appeals = n()) %>%
    ungroup()
  
  ## District contacts
  cont = readxl::read_excel("N:/ORP_accountability/projects/Evan/Communications and Documentation/District Contact List.xlsm") %>% 
    transmute(system, director, director_last, director_email, cohort_email) %>% 
    left_join(openxlsx::read.xlsx("https://www.tn.gov/content/dam/tn/education/technology/EIS/eis_contacts.xlsx") %>% 
                mutate(X1 = ifelse(row_number() == 1, "985", X1), 
                       X1 = ifelse(X1 == "560 Macon", "560", X1), 
                       X1 = as.numeric(X1)) %>% 
                filter(Primary.or.Secondary.Contact == "P" & !is.na(X1)) %>% 
                group_by(X1) %>% 
                summarize(eis_email = first(Email.Address)) %>%
                rename(system = X1) %>% 
                ungroup(), by = "system") %>% 
    mutate(cohort_email = ifelse(is.na(cohort_email), eis_email, cohort_email))
  
  ## Join everything together
  join = left_join(cont, apps, by = c("system" = "District Number")) %>% 
    left_join(curr, by = "system") %>% 
    select(system, system_name, summer_grads, n_appeals, starts_with("current_"),
           prev_grad_rate, director, director_last, director_email, cohort_email) %>% 
    mutate_at(vars(summer_grads, n_appeals), funs(ifelse(is.na(.), 0, .))) %>% 
    filter(!is.na(system_name))
  
  ## Save file if it's not in the directory or if it has been more than 1 days
  dir = str_c("N:/ORP_accountability/projects/", year(today()), "_graduation_rate/Coding/VBA")
  f2 = "Phase II Deadline Email.xlsx"
  if(f2 %in% list.files(dir) == F | file.info(paste(dir, f2, sep = "/"))$mtime < now() - days(1)) {
    xlsx::write.xlsx(as.data.frame(join), str_c("N:/ORP_accountability/projects/", year(today()), "_graduation_rate/Coding/VBA/Phase II Deadline Email.xlsx"), 
                                          row.names = F)
  } 
} else { 
  rm(phase2)
}

# Checks
if(checks == T) {
  con = dbConnect(
    JDBC("oracle.jdbc.OracleDriver", classPath="C:/Users/CA19130/Downloads/ojdbc6.jar"), 
    readRegistry("Environment", "HCU")$EIS_MGR_CXN_STR,
    "EIS_MGR",
    readRegistry("Environment", "HCU")$EIS_MGR_PWD
  )
  
  as.data.frame(as.tbl(dbGetQuery(con, "
SELECT scd.student_key, first_name, last_name, withdrawal_reason, comments 
FROM studentcohortdata scd
LEFT OUTER JOIN studentcohortdocs doc on scd.student_key = doc.student_key
WHERE status = 2
AND district_no = 360"))) %>%
  write_csv("C:/Users/CA19130/Downloads/cohort_hardin_denied.csv", na = "")
  #%>% 
    filter(str_detect(COMMENTS, "The document indicates") == T) %>% 
    transmute(name = str_c(str_sub(FIRST_NAME, 1, 1), ". ", LAST_NAME, " (", STUDENT_KEY, ")")) %>% 
    print()
} else {
  rm(checks)
}
