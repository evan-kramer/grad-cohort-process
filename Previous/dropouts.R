# Dropout Calculations
# Evan Kramer
# 4/29/2019

options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
library(RJDBC)
setwd(str_c("N:/ORP_accountability/data/", year(today()) - 1, "_graduation_rate"))

# Enrollments
dropouts = dbGetQuery(
  # Connect to database
  dbConnect(
    JDBC("oracle.jdbc.OracleDriver", classPath="C:/Users/CA19130/Downloads/ojdbc6.jar"), 
    readRegistry("Environment", "HCU")$EIS_MGR_CXN_STR,
    "EIS_MGR",
    readRegistry("Environment", "HCU")$EIS_MGR_PWD
  ),
  # Select potential dropouts from previous cohort
  "select student_key, h.withdrawal_reason as historic_withdrawal_reason, d.withdrawal_reason as isp_withdrawal_reason, 
    begin_date, end_date
  from studentcohortdata_historic h 
  left outer join (
    select distinct student_key, withdrawal_reason, begin_date, end_date
    from instructional_service_period
    where school_year = extract(year from sysdate) - 1
  ) d 
  using (student_key)
  where cohortyear = extract(year from sysdate) - 5 and 
    included_in_cohort = 'Y' and (completion_type = 5 or completion_type is null)
  order by student_key, begin_date, end_date"
) %>% 
  as.tbl() %>% 
  janitor::clean_names() %>%
  # Collapse to most recent enrollment
  mutate(end_date = ifelse(is.na(end_date) & !is.na(begin_date), ymd_hms(str_c(year(now()), "-06-01 00:00:00.0")), end_date)) %>%
  arrange(student_key, desc(begin_date), desc(end_date)) %>% 
  group_by(student_key) %>% 
  summarize_all("first") %>% 
  # Calculate actual dropouts
  transmute(student_key, dropout_count = as.integer(is.na(begin_date) | isp_withdrawal_reason %in% c(0, 1, 3, 4))) %>% 
  # Join with original cohort
  right_join(read_csv("student_level.csv"), by = "student_key")
  