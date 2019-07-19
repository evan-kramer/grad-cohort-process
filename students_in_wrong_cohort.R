# Graduation Cohort Reminder Emails
# Evan Kramer
# 6/14/2018

options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
library(RJDBC)
setwd(str_c("N:/ORP_accountability/projects/", ifelse(month(today()) >= 10, year(today()) - 1, year(today())),
            "_graduation_rate"))

# Set up
con = dbConnect(
  JDBC("oracle.jdbc.OracleDriver", classPath="C:/Users/CA19130/Downloads/ojdbc6.jar"), 
  readRegistry("Environment", "HCU")$EIS_MGR_CXN_STR,
  "EIS_MGR",
  readRegistry("Environment", "HCU")$EIS_MGR_PWD
)

current_cohort = dbGetQuery(
  con,
  "select student_key, district_no, school_no, cohortyear, withdrawal_reason, included_in_cohort, assignment revised_included_in_cohort,
    save_as_filename, assignment
  from studentcohortdata 
  left outer join studentcohortdocs
  using (student_key)
  where cohortyear = extract(year from sysdate) - 4"
) %>% 
  janitor::clean_names() %>% 
  as.tbl()

# In previous cohorts? 
dbGetQuery(
  con,
  str_c(
    "select * 
    from studentcohortdata_historic
    where student_key in (", current_cohort$student_key, ") and 
      included_in_cohort = 'Y'"
  )
) %>% 
  janitor::clean_names() %>% 
  as.tbl()

# Wrong grade? 
filter(current_cohort, assignment != 12 & included_in_cohort %in% c("P", "Y"))

# EIS enrollments in grade 9 in another year? 
incorrect_enrollments = dbGetQuery(
  con,
  str_c(
    "select student_key, assignment, min(school_year), max(school_year)
    from instructional_service_period
    inner join instructional_grade 
    using (student_key, isp_id)
    where assignment in ('09', '10', '11', '12') and
      type_of_service = 'P' and 
      student_key in (
        select student_key
        from studentcohortdata
        where cohortyear = extract(year from sysdate) - 4 and 
          included_in_cohort in ('P', 'Y')
      )
    group by student_key, school_year, assignment
    order by student_key, school_year, assignment"
  )
) %>% 
  janitor::clean_names() %>% 
  as.tbl() %>% 
  mutate_all("as.double") 

filter(incorrect_enrollments, min_school_year - assignment != 2006) %>% 
  distinct(student_key)

# By age? 
dbGetQuery(
  con,
  "select student_key, cohortyear, (sysdate - date_of_birth) / 365.25 as age
  from studentcohortdata
  where cohortyear = extract(year from sysdate) - 4"
) %>% 
  janitor::clean_names() %>% 
  as.tbl() %>% 
  filter(!between(age, 12, 22)) %>% # What is reasonable here? 
  arrange(age)
