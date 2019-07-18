# Extended Year Graduation Rates
# Evan Kramer
# 4/11/2019

library(tidyverse)
library(lubridate)
library(RJDBC)

# Connect to database and set directory
setwd("N:/ORP_accountability")
con = dbConnect(
  JDBC("oracle.jdbc.OracleDriver", classPath="C:/Users/CA19130/Downloads/ojdbc6.jar"), 
  readRegistry("Environment", "HCU")$EIS_MGR_CXN_STR,
  "EIS_MGR",
  readRegistry("Environment", "HCU")$EIS_MGR_PWD
)

# 2013 cohort
xt = read_csv(str_c(getwd(), "/data/", year(today()) - 3, "_graduation_rate/student_level_20161201.csv")) %>% 
  filter(included_in_cohort == "Y")
