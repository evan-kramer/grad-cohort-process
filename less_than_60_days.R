# Graduation Rate - Automating Less than 60 Days Counts?
# Evan Kramer
# 4/11/2019

# Set up
options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
library(RJDBC)
setwd(str_c("N:/ORP_accountability/projects/", year(today()), "_graduation_rate/"))

# Data
con = dbConnect(
  JDBC("oracle.jdbc.OracleDriver", classPath="C:/Users/CA19130/Downloads/ojdbc6.jar"), 
  readRegistry("Environment", "HCU")$EIS_MGR_CXN_STR,
  "EIS_MGR",
  readRegistry("Environment", "HCU")$EIS_MGR_PWD
)

# Instructional days, too?  
#data = 
as.tbl(dbGetQuery(
  con,
  "select isp.isp_id, isp.student_key, type_of_service, first_name, middle_name, last_name, suffix, date_of_birth,
    isp.school_year, isp.instructional_program_num, enrollment_reason, withdrawal_reason, course_of_study,
    english_language_background, isp.school_bu_id, begin_date, end_date, primary_district_id, primary_school_id,
    normal_progression_school, recalc_flg, last_recalc_date, voc_recalc_flg, voc_last_recalc_date, 
    homeless_residence, homeless_mckinney_vento, homeless_unaccomp_youth, assignment, i.last_instructional_day
  from instructional_service_period isp
  join (
    select isp_id, assignment
    from instructional_grade
  ) ig on ig.isp_id = isp.isp_id
  join (
    select school_bu_id, school_year, instructional_program_num, max(id_date) as last_instructional_day
    from scal_id_days
    group by school_bu_id, school_year, instructional_program_num
  ) i on (
    isp.school_bu_id = i.school_bu_id and 
    isp.school_year = i.school_year and 
    isp.instructional_program_num = i.instructional_program_num
  )
  where isp.student_key = 3430677")
) %>% 
  janitor::clean_names() %>%
  filter(as.integer(assignment) >= 9) %>% 
  arrange(school_year) 
