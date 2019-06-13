# Graduation Cohort Change Requests
# Evan Kramer
# 6/11/2019

options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
library(RJDBC)
setwd("N:/ORP_accountability/projects/2019_graduation_rate/Documentation for Cohort Changes")

# Switches
tracker = T
output = T
changes = F

# Tracker
if(tracker) {
  # Find the worksheet that is closest to today's date
  d = list.files("Worksheets")[ymd(list.files("Worksheets")) <= today() + ddays(3)]
  
  # Read files and bind rows
  # Initialize data frame for storing lists of files to read and data
  a = tibble()
  
  # Loop through all submissions
  for(i in seq_along(d)) {
    # Create file list
    f = list.files(str_c("Worksheets/", d[i]))[str_detect(list.files(str_c("Worksheets/", d[i])), "Cohort_Change") == T]
    
    # Iterate over file 
    for(j in seq_along(f)) {
      # Read in contact information and define variables
      c = readxl::read_excel(str_c("Worksheets/", d[i], "/", f[j]), sheet = 1)   
      system = as.integer(c[2, 2])
      director_name = as.character(c[4, 2])
      director_email = as.character(c[5, 2])
      contact_name = as.character(c[6, 2])
      contact_email = as.character(c[7, 2])
      
      # Read in change request
      b = readxl::read_excel(str_c("Worksheets/", d[i], "/", f[j]), sheet = 2,
                             col_types = "text") %>% 
        filter(!is.na(`Student ID`)) %>% 
        mutate(Date = as_datetime(ymd(d[i])), `Director Name` = director_name, `Director Email` = director_email, 
               `Contact Name` = contact_name, `Contact Email` = contact_email)
      
      # Bind rows
      a = bind_rows(a, b)
      print(f[j])
    }
  } 
  
  # Remove objects
  # rm(system, director_name, director_email, contact_name, contact_email, b, c, d, f, i, j)
} else {
  rm(tracker)
}

# Output files
if(output) {
  file = "Cohort Changes Data.xlsx"
  if(file %in% list.files()) {
    # If there is no Archive folder, create one 
    if(!dir.exists("Archive")) {
      dir.create("Archive")
      dir.create(str_c("Archive/", str_replace(str_sub(str_replace_all(now(), "[-:]", ""), 1, -3), " ", "_"))) # create a folder for this save
    }
    # If there is no folder for this moment's archive, create one
    if(!dir.exists(str_c(getwd(), "/Archive/", str_replace(str_sub(str_replace_all(now(), "[-:]", ""), 1, -3), " ", "_")))) {
      dir.create(str_c(getwd(), "/Archive/", str_replace(str_sub(str_replace_all(now(), "[-:]", ""), 1, -3), " ", "_")))
    }
    # Save current file and archive
    xlsx::write.xlsx(as.data.frame(a), str_c(getwd(), 
                                             "/Archive/", 
                                             str_replace(str_sub(str_replace_all(now(), "[-:]", ""), 1, -3), " ", "_"), 
                                             "/", 
                                             file), 
                     row.names = F, append = F, sheetName = "Individual", showNA = F)
  } 
  xlsx::write.xlsx(as.data.frame(a), file, row.names = F, append = F, sheetName = "Individual", showNA = F)
} else {
  rm(output)
}

# Changes
if(changes) {
  con = dbConnect(
    JDBC("oracle.jdbc.OracleDriver", classPath="C:/Users/CA19130/Downloads/ojdbc6.jar"), 
    readRegistry("Environment", "HCU")$EIS_MGR_CXN_STR,
    "EIS_MGR",
    readRegistry("Environment", "HCU")$EIS_MGR_PWD
  )
  
  t = readxl::read_excel("N:/ORP_accountability/projects/2019_graduation_rate/Documentation for Cohort Changes/Cohort Changes Master Tracker.xlsx") %>% 
    filter(Status == "Approve") %>% 
    mutate(Action = str_replace_all(Action, "Change COHORTYEAR to", "Set COHORTYEAR ="))
  
  for(i in 1:nrow(t)) {
    dbSendUpdate(
      con,
      str_c(
        "update studentcohortdata ",
        t$Action[i], 
        " where student_key = ",
        str_trim(t$`Student ID`[i])
      )
    )
  }
} else {
  rm(changes)
}