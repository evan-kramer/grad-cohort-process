# Graduation Rate - Automating Less than 60 Days Counts?
# Evan Kramer
# 7/18/2019

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
reassign = as.tbl(
  dbGetQuery(
    con, 
    str_c(
      "--select *
      select student_key, primary_district_id, primary_school_id, count(distinct id_date) as n_days
      from instructional_service_period
      inner join instructional_grade 
      using (student_key, isp_id)
      inner join scal_id_days
      using (school_bu_id, instructional_program_num, school_year)
      where student_key in (
        select student_key
        from studentcohortdata
        left outer join instructional_service_period
        using (student_key, isp_id) 
        where cohortyear = extract(year from sysdate) - 4 and 
          end_date - begin_date < 60
      ) and school_year >= extract(year from sysdate) - 4 and 
        assignment in ('09','10','11','12') and
        type_of_service = 'P' and 
        id_date between begin_date and end_date
      group by student_key, primary_district_id, primary_school_id
      order by student_key, primary_district_id, primary_school_id"
    )
  )
) %>% 
  janitor::clean_names()