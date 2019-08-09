# Graduation Rate - Automating Less than 60 Days Counts?
# Evan Kramer

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

# All those eligible and Where they would go back to? 
reassign = dbGetQuery(
  con, 
  "select student_key, school_year, primary_district_id, primary_school_id, count(*) as n_instructional_days
  from instructional_service_period
  inner join scal_id_days 
  using (school_bu_id, school_year, instructional_program_num)
  where student_key in (
      select student_key
      from studentcohortdata
      where cohortyear = extract(year from sysdate) - 4
  ) and 
      school_year between extract(year from sysdate) - 4 and extract(year from sysdate) - 1 and 
      id_date between begin_date and end_date
  group by student_key, school_year, primary_district_id, primary_school_id
  order by student_key, school_year, primary_district_id, primary_school_id"
) %>% 
  as.tbl() %>%
  janitor::clean_names()