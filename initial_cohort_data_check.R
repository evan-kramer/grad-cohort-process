# Initial Cohort Data Check
# Evan Kramer
# 11/2/2017

library(tidyverse)
library(lubridate)
library(stringr)
library(haven)

date = str_replace_all(today(), "-", "")
setwd("C:/Users/CA19130/Documents/Data/Graduation Rate")

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
    