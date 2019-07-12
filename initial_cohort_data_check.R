# Initial Cohort Data Check
# Evan Kramer
# 7/12/2019

# Set up
options(java.parameters = "-Xmx16G")
library(RJDBC)
library(rgdal)
library(tidyverse)
library(lubridate)
library(haven)
library(knitr)
library(rmarkdown)
setwd("N:/ORP_accountability/")
con = dbConnect(
  JDBC("oracle.jdbc.OracleDriver", classPath="C:/Users/CA19130/Downloads/ojdbc6.jar"), 
  readRegistry("Environment", "HCU")$EIS_MGR_CXN_STR,
  "EIS_MGR",
  readRegistry("Environment", "HCU")$EIS_MGR_PWD
)

# Application contains all four active cohorts
dbGetQuery(
  con,
  "select cohortyear, count(distinct student_key) as n_students
  from studentcohortdata 
  group by cohortyear
  order by n_students desc"
) %>% 
  janitor::clean_names() %>% 
  as.tbl() %>% 
  filter(between(cohortyear, year(now()) - 3, year(now())))

# Number of students in each school/district/state cohort is within 10% of prior year
full_join(
  dbGetQuery(
    con,
    "select cohortyear, district_no, school_no, count(distinct student_key) as n_students
    from studentcohortdata
    where included_in_cohort in ('Y', 'P') and cohortyear = extract(year from sysdate) - 3
    group by cohortyear, district_no, school_no
    order by district_no, school_no"
  ),
  dbGetQuery(
    con,
    "select cohortyear, district_no, school_no, count(distinct student_key) as n_students
    from studentcohortdata_historic
    where included_in_cohort = 'Y' and cohortyear = extract(year from sysdate) - 5 -- change to 4
    group by cohortyear, district_no, school_no
    order by district_no, school_no"
  ),
  by = c("DISTRICT_NO", "SCHOOL_NO")
) %>%
  janitor::clean_names() %>%
  as.tbl() %>% 
  mutate(pct_diff = ifelse(n_students_y >= 30 & n_students_x >= 30, 
                           round(100 * (n_students_x - n_students_y) / n_students_y, 1), NA)) %>% 
  arrange(desc(pct_diff))

# Same as above by subgroup
# No students without district or school numbers
# Students in inactive schools?
# Students with missing values across various fields?
# Inappropriate included_in_cohort values?
# Changes in withdrawals? 

# Read in data
data = read_csv("studentcohortdata_20171101.csv")
docs = read_csv("studentcohortdocs_20171101.csv")
grad = left_join(data, docs, by = c("Student key" = "Student Key SUM"))
prev = read_csv("K:/ORP_accountability/data/2017_graduation_rate/student_level_20170830.csv")

# Check available cohort years
print(table(data = grad$Cohortyear.x, docs = grad$Cohortyear.y, useNA = "ifany"))

# How do numbers compare to previous years? 
bind_rows(prev %>% 
              group_by(system) %>% 
              summarize(year = 2013, n = n()) %>% 
              ungroup(),
          data %>% 
              filter(Cohortyear == 2014) %>% 
              group_by(`District no`) %>% 
              summarize(year = max(Cohortyear, na.rm = T), n = n()) %>% 
              rename(system = `District no`),
          data %>% 
              filter(Cohortyear == 2015) %>% 
              group_by(`District no`) %>% 
              summarize(year = max(Cohortyear, na.rm = T), n = n()) %>% 
              rename(system = `District no`),
          data %>% 
              filter(Cohortyear == 2016) %>% 
              group_by(`District no`) %>% 
              summarize(year = max(Cohortyear, na.rm = T), n = n()) %>% 
              rename(system = `District no`),
          data %>% 
              filter(Cohortyear == 2017) %>% 
              group_by(`District no`) %>% 
              summarize(year = max(Cohortyear, na.rm = T), n = n()) %>% 
              rename(system = `District no`)) %>% 
    group_by(system) %>% 
    mutate(min = min(n, na.rm = T), max = max(n, na.rm = T), 
           mean = round(mean(n, na.rm = T), 1), sd = round(sd(n, na.rm = T), 1)) %>% 
    ungroup() %>% 
    spread(year, n)
    