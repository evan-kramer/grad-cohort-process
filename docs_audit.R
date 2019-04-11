# Cohort Documentation Audit
# Evan Kramer
# 4/11/2019

options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
library(RJDBC)
setwd("N:/")

# Connect to database
eis_con = dbConnect(
  JDBC("oracle.jdbc.OracleDriver", classPath="C:/Users/CA19130/Downloads/ojdbc6.jar"), 
  readRegistry("Environment", "HCU")$EIS_MGR_CXN_STR,
  "EIS_MGR",
  readRegistry("Environment", "HCU")$EIS_MGR_PWD
)

# Pull document blobs
q = 
  "select doc_blob from studentcohortdocs doc 
join studentcohortdata scd on scd.student_key = doc.student_key 
where cohortyear = 2014"
s = .jcall(eis_con@jc, "Ljava/sql/Statement;", "createStatement")
r = .jcall(s, "Ljava/sql/ResultSet;", "executeQuery", q, check = F)
l = list()
c = 1L
i = 1

while(.jcall(r, 'Z', 'next')) {
  l[[i]] = .jcall(r, '[B', 'getBytes', c)
  i = i + 1
}

a = read_dta("ORP_accountability/data/2018_ACT/Post-Appeals/2019_ACT_student_level_actcohorthighest_appeals.dta")
b = read_dta("Assessment_Data Returns/ACT/2017-18/2017 Retake/20171212_ACT_cohort_retake_FinalFile_SY2016-17_Whalen_v1.dta")

left_join(select(a, student_key, ends_with("_highest")),
          select(b, state_id, contains("scale")), by = c("student_key" = "state_id")) %>% 
  filter(act_composite_highest < act_composite_scaleMR | 
           is.na(act_composite_highest) & !is.na(act_composite_scaleMR)) %>%
  View()


