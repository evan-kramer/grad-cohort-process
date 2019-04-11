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

# Current year
scd = as.tbl(
  dbGetQuery(
    con, str_c(
      "select scd.student_key, student_ssn, first_name, middle_name, 
                                last_name, suffix, date_of_birth, gender, student_pin, immigrant, 
                                date_1st_enrolled_us_school, year_entered_grade9, native_language, 
                                grad_ell_flag, grad_se_flag, ethnicity, race_i, race_a, race_p, 
                                race_b, race_w, cohortyear, calc_from, district_no, school_no, 
                                assignment, eoy_action, withdrawal_reason, completion_type, ell, 
                                econ_dis, sped, year_withdrawn, included_in_cohort, race_ethnicity, 
                                manual_intervention, homeless, cte, migrant, scd.isp_id, ell_detail, 
                                pending_included_in_cohort, completion_period, completion_date, 
                                allow_upload, t1_t2, original_filename, modified_date, user_id, 
                                save_as_filename, comments, revised_included_in_cohort, 
                                reviewer_user_id, reviewed_date, appeal_comments, appeal_date, status 
                              from studentcohortdata scd 
                              left outer join studentcohortdocs doc on scd.student_key = doc.student_key 
                              where scd.cohortyear = ", ifelse(month(today()) >= 10, year(today()) - 1, year(today())) - 4
    )
  )
) %>% 
  janitor::clean_names()

# Prior years
scd_prior = tibble()
for(y in 1:2) {
  temp = read_csv(str_c("N:/ORP_accountability/data/",
                        ifelse(month(today()) >= 10, year(today()) - y - 1, year(today()) - y),
                        "_graduation_rate/student_level.csv")) 
  if(y == 2) {
    scd_prior = bind_rows(
      scd_prior, 
      transmute(temp, student_key, cohortyear = cohort_year, date_of_birth = as_datetime(dmy(dob)), 
                withdrawal_reason = withdrawal_code, year_entered_grade9 = year_entered_grade_9,
                calc_from, included_in_cohort, assignment = as.numeric(grade), completion_type)
    )
  } else {
    scd_prior = bind_rows(
      scd_prior, 
      transmute(temp, student_key, cohortyear, date_of_birth, withdrawal_reason, 
                year_entered_grade9, calc_from, included_in_cohort, assignment, completion_type)
    )
  }
}

# Changes
scd_changes = tibble()
for(y in 1:2) {
  temp = readxl::read_excel(str_c("N:/ORP_accountability/projects/",
                                  ifelse(month(today()) >= 10, year(today()) - y - 1, year(today()) - y),
                                  "_graduation_rate/Documentation for Cohort Changes/Students to be Changed in Cohort.xlsx")) %>% 
    janitor::clean_names() %>% 
    transmute(student_key, requested_change, notes) %>% 
    filter(str_detect(requested_change, "Change COHORTYEAR"))
  scd_changes = bind_rows(scd_changes, temp)
}
rm(y, temp)

# Predict? 
c = group_by(scd_changes, student_key) %>% 
  summarize(requested_change = 1) %>% 
  ungroup() %>%
  full_join(scd_prior, by = "student_key") %>% 
  mutate(age = as_datetime(ymd(str_c(cohortyear, "0701"))) - date_of_birth)
         # age = format(difftime(as_datetime(ymd(str_c(cohortyear, "0701"))), date_of_birth, units = "days")))

# What percent of students in grade 11 were requested to be changed? 
filter(c, included_in_cohort == "Y") %>% 
  group_by(cohortyear, assignment, requested_change) %>% 
  summarize(n = n_distinct(student_key)) %>% 
  ungroup()

# What percent of students with different YE9 and cohortyear values were requested to be changed? 
filter(c, included_in_cohort == "Y") %>% 
  group_by(cohortyear, year_entered_grade9, requested_change) %>% 
  summarize(n = n_distinct(student_key)) %>% 
  ungroup() %>% 
  View()

# Significant differences in ages? 
t.test(c$age[!is.na(c$age) & c$requested_change == 1],
       c$age[!is.na(c$age) & is.na(c$requested_change)]
  
)

# What percent of change requests had significantly different DOBs? 
# What percent of change requests were grades other than 12? 
# What percent of change requests had different cohortyear and ye9 values?



