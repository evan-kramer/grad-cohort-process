# Graduation Cohort Appeals
# Evan Kramer
# 7/12/2019

library(tidyverse)
library(lubridate)
library(haven)
library(RJDBC)

date = str_replace_all(as.character(Sys.Date()), "-", "")
setwd(str_c("N:/ORP_accountability/projects/", year(today()), "_graduation_rate/Appeals/District Documents/Worksheets"))

# Compile Appeals Tracker
## Determine order to load worksheets
file_list = list.files()
sys_list = rep(NA, length(file_list))
load_order = data.frame()
for(f in seq_along(file_list)) {
  temp = data.frame(filename = file_list[f], timestamp = file.mtime(file_list[f]))
  load_order = bind_rows(load_order, temp)
}
load_order = arrange(load_order, timestamp)
# file_list = unique(as.character(load_order$filename))

## Load and bind rows
a = data.frame()

for(i in seq_along(file_list)) {
  if(str_detect(file_list[i], "Appeal") == T) {
    c = readxl::read_excel(file_list[i], sheet = 1) 
    system = as.integer(c[2, 2])
    b = readxl::read_excel(file_list[i], sheet = 2, col_types = "text") %>% 
      filter(!is.na(`Student ID`)) %>% 
      mutate(Date = as.Date.numeric(as.numeric(as.character(c[1, 2])), origin = "1899-12-30"),
             `Director Email` = as.character(c[5, 2]),
             `Contact Email` = as.character(c[7, 2]))
    a = bind_rows(a, b)
  }
}

xlsx::write.xlsx(a, str_c("N:/ORP_accountability/projects/", year(today()), "_graduation_rate/Appeals/Appeals Trackers/Appeals Tracker.xlsx"),
           row.names = F, append = F, sheetName = "Individual", showNA = F)