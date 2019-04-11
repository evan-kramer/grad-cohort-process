# Graduation Cohort Reminder Emails
# Evan Kramer
# 4/11/2019

options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
library(RJDBC)

# Set up
date = str_replace_all(today(), "-", "")
current_year = ifelse(month(today()) >= 9, year(today()) + 1, year(today()))
deadline = ymd(str_c(current_year, "0615"))
con = dbConnect(
  JDBC("oracle.jdbc.OracleDriver", classPath="C:/Users/CA19130/Downloads/ojdbc6.jar"), 
  readRegistry("Environment", "HCU")$EIS_MGR_CXN_STR,
  "EIS_MGR",
  readRegistry("Environment", "HCU")$EIS_MGR_PWD
)

# Current cohort
scd = as.tbl(dbGetQuery(con, 
                        str_c("select scd.student_key, student_ssn, first_name, middle_name, 
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
                              where scd.cohortyear = ", current_year - 4))) %>% 
  janitor::clean_names()

# Make sure counts only reflect included students
table(inc = scd$included_in_cohort, rev = scd$revised_included_in_cohort, useNA = "ifany")
mutate(scd, grad_cohort = ifelse(is.na(revised_included_in_cohort), 
                                 ifelse(included_in_cohort == "P", "Y", included_in_cohort),
                                 revised_included_in_cohort)) %>% 
  group_by(grad_cohort) %>% 
  summarize(n = n())


mutate(scd, grad_cohort = included_in_cohort == "Y" | (included_in_cohort == "P" & is.na(revised_included_in_cohort)),
       grad_cohort = ifelse(is.na(revised_included_in_cohort), grad_cohort, 
                            ifelse(revised_included_in_cohort == "Y" & included_in_cohort %in% c("P", "Y"), T, F))) %>%
  group_by(grad_cohort) %>% 
  summarize(n = n())




# Eligible for removal but no document
print(filter(scd, withdrawal_reason %in% c(2,  5, 6, 8, 10, 17) & is.na(modified_date)))

# Documentation left to review
print(filter(scd, !is.na(save_as_filename) & is.na(reviewed_date)))

# Denied documentation by district
print(filter(scd, status == 2))

# Mis-coded early graduates
filter(scd, withdrawal_reason == 12 & (is.na(completion_type) | !completion_type %in% c(1, 11, 12, 13))) %>% 
  select(student_key, district_no, school_no, withdrawal_reason, starts_with("completion"))

# Students withdrawn but no subsequent enrollment
filter(scd, withdrawal_reason %in% c(3, 4))

# Students with neither completion nor withdrawal information
filter(scd, is.na(completion_type) & is.na(withdrawal_reason))

# Students who might be in the wrong cohort?

# List of students counting as non-graduates?