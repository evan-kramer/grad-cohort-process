# Graduation Cohort Analysis
# Evan Kramer
# 1/19/2018

library(tidyverse)
library(lubridate)
library(stringr)
library(haven)
library(readxl)
library(xlsx)

date = str_replace_all(as.character(today()), "-", "")
file_list_2018 = list.files("2018_graduation_rate/Data")
d2016 = F
d2017 = T
d2018 = F

# Prior year data for reports
## 2016
if(d2016 == T) {
    setwd("K:/ORP_accountability/projects/2016_graduation_rate/Data/Raw")
    aa = read_excel("COHORTDATA_DOCS_03242006.xls") %>% 
        summarize(n_elig = sum(WITHDRAWAL_REASON %in% c(2, 5, 6, 8, 10, 17)),
                  n_docs = sum(!is.na(ORIGINAL_FILENAME), na.rm = T),
                  n_reviewed = sum(!is.na(REVIEWED_DATE), na.rm = T),
                  date = ymd(20160324)) 
    a = read_csv("cohort_data_20160413.csv") %>% 
        summarize(n_elig = sum(withdrawal_reason %in% c(2, 5, 6, 8, 10, 17)),
                  n_docs = sum(!is.na(original_filename), na.rm = T),
                  n_reviewed = sum(!is.na(reviewed_date), na.rm = T),
                  date = ymd(20160413)) 
    b = read_tsv("StudentcohortData_Docs_cohortyear2012_04252016.txt") %>% 
        summarize(n_elig = sum(WITHDRAWAL_REASON %in% c(2, 5, 6, 8, 10, 17)),
                  n_docs = sum(!is.na(ORIGINAL_FILENAME), na.rm = T),
                  n_reviewed = sum(!is.na(REVIEWED_DATE), na.rm = T),
                  date = ymd(20160425))
    c = read_tsv("StudentcohortData_Docs_cohortyear2012_04302016.txt") %>% 
        summarize(n_elig = sum(WITHDRAWAL_REASON %in% c(2, 5, 6, 8, 10, 17)),
                  n_docs = sum(!is.na(ORIGINAL_FILENAME), na.rm = T),
                  n_reviewed = sum(!is.na(REVIEWED_DATE), na.rm = T),
                  date = ymd(20160425))
    d = read_tsv("StudentcohortData_Docs_cohortyear2012_05162015.txt") %>% 
        summarize(n_elig = sum(WITHDRAWAL_REASON %in% c(2, 5, 6, 8, 10, 17)),
                  n_docs = sum(!is.na(ORIGINAL_FILENAME), na.rm = T),
                  n_reviewed = sum(!is.na(REVIEWED_DATE), na.rm = T),
                  date = ymd(20160516))
    e = read_tsv("StudentcohortData_Docs_cohortyear2012_05252016.txt") %>% 
        summarize(n_elig = sum(WITHDRAWAL_REASON %in% c(2, 5, 6, 8, 10, 17)),
                  n_docs = sum(!is.na(ORIGINAL_FILENAME), na.rm = T),
                  n_reviewed = sum(!is.na(REVIEWED_DATE), na.rm = T),
                  date = ymd(20160525))
    f = read_tsv("OuterJoin_StudentcohortData_Docs_cohortyear2012_06012016.txt") %>% 
        summarize(n_elig = sum(WITHDRAWAL_REASON %in% c(2, 5, 6, 8, 10, 17)),
                  n_docs = sum(!is.na(ORIGINAL_FILENAME), na.rm = T),
                  n_reviewed = sum(!is.na(REVIEWED_DATE), na.rm = T),
                  date = ymd(20160601))
    g = read_tsv("StudentcohortData_Docs_cohortyear2012_06132016.txt") %>% 
        summarize(n_elig = sum(WITHDRAWAL_REASON %in% c(2, 5, 6, 8, 10, 17)),
                  n_docs = sum(!is.na(ORIGINAL_FILENAME), na.rm = T),
                  n_reviewed = sum(!is.na(REVIEWED_DATE), na.rm = T),
                  date = ymd(20160613))
    h = read_tsv("OuterJoin_StudentcohortData_Docs_cohortyear2012_06152016_R.txt") %>% 
        summarize(n_elig = sum(WITHDRAWAL_REASON %in% c(2, 5, 6, 8, 10, 17)),
                  n_docs = sum(!is.na(ORIGINAL_FILENAME), na.rm = T),
                  n_reviewed = sum(!is.na(REVIEWED_DATE), na.rm = T),
                  date = ymd(20160615))
    i = read_tsv("OuterJoin_StudentcohortData_Docs_cohortyear2012_06272016.txt") %>% 
        summarize(n_elig = sum(WITHDRAWAL_REASON %in% c(2, 5, 6, 8, 10, 17)),
                  n_docs = sum(!is.na(ORIGINAL_FILENAME), na.rm = T),
                  n_reviewed = sum(!is.na(REVIEWED_DATE), na.rm = T),
                  date = ymd(20160627))
    j = read_tsv("StudentcohortData_Docs_cohortyear2012_07012016.txt") %>% 
        summarize(n_elig = sum(WITHDRAWAL_REASON %in% c(2, 5, 6, 8, 10, 17)),
                  n_docs = sum(!is.na(ORIGINAL_FILENAME), na.rm = T),
                  n_reviewed = sum(!is.na(REVIEWED_DATE), na.rm = T),
                  date = ymd(20160701))
    k = read_tsv("StudentcohortData_Docs_cohortyear2012_07082016.txt") %>% 
        summarize(n_elig = sum(WITHDRAWAL_REASON %in% c(2, 5, 6, 8, 10, 17)),
                  n_docs = sum(!is.na(ORIGINAL_FILENAME), na.rm = T),
                  n_reviewed = sum(!is.na(REVIEWED_DATE), na.rm = T),
                  date = ymd(20160708))
    l = read_tsv("studentcohortdata_cohort2012_20160715.txt") %>% 
        left_join(read_tsv("studentcohortdocs_20160715.txt"), by = c("student_key" = "Student Key SUM")) %>% 
        summarize(n_elig = sum(withdrawal_reason %in% c(2, 5, 6, 8, 10, 17)),
                  n_docs = sum(!is.na(`Original Filename`), na.rm = T),
                  n_reviewed = sum(!is.na(`Reviewed Date`), na.rm = T),
                  date = ymd(20160715))
    m = read_tsv("StudentcohortData_Docs_cohortyear2012_07212016.txt") %>% 
        summarize(n_elig = sum(WITHDRAWAL_REASON %in% c(2, 5, 6, 8, 10, 17)),
                  n_docs = sum(!is.na(ORIGINAL_FILENAME), na.rm = T),
                  n_reviewed = sum(!is.na(REVIEWED_DATE), na.rm = T),
                  date = ymd(20160721))
    n = read_tsv("revised_included_in_cohort_20160809.txt") %>% 
        summarize(n_elig = sum(`Withdrawal reason` %in% c(2, 5, 6, 8, 10, 17)),
                  n_docs = n(),
                  n_reviewed = sum(!is.na(`Revised Included In Cohort`), na.rm = T),
                  date = ymd(20160809))
    o = read_tsv("studentcohortdata_cohort2012_revisedincluded_20160817.txt") %>% 
        summarize(n_elig = sum(`Withdrawal reason SUM` %in% c(2, 5, 6, 8, 10, 17)),
                  n_docs = n(),
                  n_reviewed = sum(!is.na(`Revised Included In Cohort`), na.rm = T),
                  date = ymd(20160817))
    
    docs2016 = bind_rows(aa, a, b, c, d, e, f, g, h, i, j, l, m, n, o) %>% 
        mutate(year = 2016)
    write_csv(docs2016, "K:/ORP_accountability/projects/2018_graduation_rate/Data/n_docs_over_time_2016.csv", na = "")
    rm(aa, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
}

## 2017
if(d2017 == T) {
    setwd("K:/ORP_accountability/projects/2017_graduation_rate/Data")
    read_dates_2017 = c("20161207", "20170202", "20170207", "20170210", "20170217", 
                        "20170224", "20170302", "20170313", "20170317", "20170324", 
                        "20170331", "20170407", "20170419", "20170421", "20170428",
                        "20170508", "20170515", "20170519", "20170526", "20170602", 
                        "20170606", "20170607", "20170608", "20170609", "20170612", 
                        "20170613", "20170614", "20170615", "20170616", "20170619", 
                        "20170621", "20170623", "20170630", "20170707") # there are more; might not need to add
    n_docs =  as.tbl(data.frame())
    for(d in seq_along(read_dates_2017)) {
        temp = read_csv(str_c("studentcohortdata_", 
                              read_dates_2017[d], ".csv")) %>% 
            left_join(read_csv(str_c("studentcohortdocs_",
                                     read_dates_2017[d], ".csv")), by = c("Student key" = "Student Key SUM")) %>% 
            summarize(date = ymd(read_dates_2017[d]),
                      n_elig = sum(`Withdrawal reason SUM` %in% c(2,5,6,8,10,17), na.rm = T),
                      n_docs = sum(!is.na(`Modified Date`)),
                      n_reviewed = sum(!is.na(`Reviewed Date`))) 
            n_docs = bind_rows(temp, n_docs)
    }
    n_docs = mutate(n_docs, year = 2017)
}

break


## Docs by district over time
n_docs_district = as.tbl(data.frame())
for(d in seq_along(read_dates_2017)) {
    temp = read_csv(str_c("2017_graduation_rate/Data/studentcohortdata_", 
                          read_dates_2017[d], ".csv")) %>% 
        left_join(read_csv(str_c("2017_graduation_rate/Data/studentcohortdocs_",
                                 read_dates_2017[d], ".csv")), by = c("Student key" = "Student Key SUM")) %>% 
        group_by(`District no`) %>% 
        summarize(date = ymd(read_dates_2017[d]),
                  n_elig = sum(`Withdrawal reason SUM` %in% c(2,5,6,8,10,17), na.rm = T),
                  n_docs = sum(!is.na(`Modified Date`)),
                  n_reviewed = sum(!is.na(`Reviewed Date`))) %>% 
        ungroup()
    n_docs_district = bind_rows(temp, n_docs_district)
}
n_docs_district = bind_rows(mutate(n_docs_district, year = 2017),
                            read_csv("2017_graduation_rate/Data/n_docs_over_time_2016.csv"))

## Docs by withdrawal code over time
n_docs_wd = as.tbl(data.frame())
for(d in seq_along(read_dates_2017)) {
    temp = read_csv(str_c("2017_graduation_rate/Data/studentcohortdata_", 
                          read_dates_2017[d], ".csv")) %>% 
        left_join(read_csv(str_c("2017_graduation_rate/Data/studentcohortdocs_",
                                 read_dates_2017[d], ".csv")), by = c("Student key" = "Student Key SUM")) %>% 
        group_by(`Withdrawal reason SUM`) %>% 
        summarize(date = ymd(read_dates_2017[d]),
                  n_elig = sum(`Withdrawal reason SUM` %in% c(2,5,6,8,10,17), na.rm = T),
                  n_docs = sum(!is.na(`Modified Date`)),
                  n_reviewed = sum(!is.na(`Reviewed Date`))) %>% 
        ungroup()
    n_docs_wd = bind_rows(temp, n_docs_wd)
}
n_docs_wd = bind_rows(mutate(n_docs_wd, year = 2017),
                      read_csv("2017_graduation_rate/Data/n_docs_by_district_over_time_2016.csv"))



break





























break

## 2017 Documents
docs2017 = as.tbl(data.frame(date = as.Date(NA),
                             n_elig = as.integer(NA),
                             n_docs = as.integer(NA),
                             n_reviewed = as.integer(NA),
                             year = as.integer(NA)))
for(i in seq_along(read_dates)) {
    d = read_dates[i]
    g = paste0("K:/ORP_accountability/projects/2017_graduation_rate/Data/studentcohortdata_",
               d, ".csv") %>% 
        read_csv() %>% 
        filter(Cohortyear == 2013) %>% 
        left_join(read_csv(paste0("K:/ORP_accountability/projects/2017_graduation_rate/Data/studentcohortdocs_",
                                  d, ".csv")), 
                  by = c("Student key" = "Student Key SUM")) %>% 
        summarise(date = ymd(d),
                  n_elig = sum(`Withdrawal reason SUM` %in% c(2,5,6,8,10,17), na.rm = T),
                  n_docs = sum(!is.na(`Modified Date`)),
                  n_reviewed = sum(!is.na(`Reviewed Date`)),
                  year = year(Sys.Date()))
    docs2017[i, ] = g
}
rm(d, g, i)


# More/less not correct in RMD?
read_dates = c("20161207", "20170202", "20170207", "20170210", "20170217", "20170224", "20170302", "20170313", "20170317", "20170324", "20170331", "20170407", "20170419", "20170421", "20170428", "20170508", "20170515", "20170519")

yoy = docs2016 %>% 
    mutate(date_prev = ymd(read_dates[length(read_dates)]), 
           date_check = abs(date - date_prev)) %>% 
    filter(date_check == min(date_check)) %>% 
    filter(n_reviewed == max(n_reviewed)) %>% 
    bind_rows(filter(docs2017, date == max(date))) %>% 
    select(n_docs, n_reviewed, date, year)
    
abs(yoy[2, 1] - yoy[1, 1])
ifelse(yoy[2, 1] - yoy[1, 1] >= 0, "more", "less")

# Average number of documents reviewed per day?
ltr = bind_rows(docs2017 %>% 
                    filter(date == ymd(20170224)),
                docs2017 %>% 
                    filter(date == max(date))) # will need a way of accounting for when they plateau

as.numeric(round((ltr[2, 3] - ltr[1, 3]) / as.numeric((ltr[2, 1] - ltr[1, 1]))))

## How long will it take to review the rest? 
as.numeric(round((docs2017 %>% filter(date == max(date)) %>% select(n_docs) - 
    docs2017 %>% filter(date == max(date)) %>% select(n_reviewed)) / 
    as.numeric(round((ltr[2, 3] - ltr[1, 3]) / as.numeric((ltr[2, 1] - ltr[1, 1]))))))

paste(month(ymd(read_dates[length(read_dates)]) + days(as.numeric(round((docs2017 %>% filter(date == max(date)) %>% select(n_docs) - 
                                                                 docs2017 %>% filter(date == max(date)) %>% select(n_reviewed)) / 
                                                                as.numeric(round((ltr[2, 3] - ltr[1, 3]) / as.numeric((ltr[2, 1] - ltr[1, 1]))))))), label = T, abbr = F),
      day(ymd(read_dates[length(read_dates)]) + days(as.numeric(round((docs2017 %>% filter(date == max(date)) %>% select(n_docs) - 
                                                                           docs2017 %>% filter(date == max(date)) %>% select(n_reviewed)) / 
                                                                          as.numeric(round((ltr[2, 3] - ltr[1, 3]) / as.numeric((ltr[2, 1] - ltr[1, 1])))))))))





as.numeric(round((docs2017 %>% filter(date == max(date)) %>% select(n_elig) - 
        docs2017 %>% filter(date == max(date)) %>% select(n_reviewed)) / 
    as.numeric(round((ltr[2, 3] - ltr[1, 3]) / as.numeric((ltr[2, 1] - ltr[1, 1]))))))


break


























## Plot 2016 and 2017 Documents/Reviews
docs2016 %>% 
    mutate(date = date + dyears(1),
           year = 2016) %>% 
    bind_rows(docs2017) %>% 
    ggplot(aes(x = date, y = n_reviewed, col = factor(year))) + 
    #ggplot(aes(x = date, y = n_docs, col = factor(year))) + 
    geom_point() + 
    geom_line() + 
    geom_vline(xintercept = as.numeric(ymd(20170616))) + 
    xlab("Date") + 
    ylab("Number of Documents") + 
    scale_color_discrete(guide = guide_legend(title = "Year")) + 
    theme_bw()

## Plot Document Approval Rates by Withdrawal Code
revs2016 = read_tsv("studentcohortdata_cohort2012_revisedincluded_20160817.txt") %>% 
    group_by(`Withdrawal reason SUM`) %>% 
    summarize(n_reviewed = sum(!is.na(`Revised Included In Cohort`), na.rm = T),
              n_approved = sum(`Revised Included In Cohort` == "N", na.rm = T)) %>% 
    ungroup() %>% 
    mutate(year = 2016,
           pct_approved = ifelse(n_reviewed == 0, NA, round(100 * n_approved / n_reviewed, 1))) %>% 
    filter(`Withdrawal reason SUM` %in% c(2, 5, 6, 8, 10, 17))

revs2017 = paste0("K:/ORP_accountability/projects/2017_graduation_rate/Data/studentcohortdata_", read_dates[length(read_dates)], ".csv") %>% 
    read_csv() %>% 
    left_join(read_csv(paste0("K:/ORP_accountability/projects/2017_graduation_rate/Data/studentcohortdocs_", 
                              read_dates[length(read_dates)], 
                              ".csv")),
              by = c("Student key" = "Student Key SUM")) %>% 
    group_by(`Withdrawal reason SUM`) %>% 
    summarize(n_reviewed = sum(!is.na(`Revised Included In Cohort`), na.rm = T),
              n_approved = sum(`Revised Included In Cohort` == "N", na.rm = T)) %>% 
    ungroup() %>% 
    mutate(year = 2017,
           pct_approved = ifelse(n_reviewed == 0, NA, round(100 * n_approved / n_reviewed, 1))) %>% 
    filter(`Withdrawal reason SUM` %in% c(2, 5, 6, 8, 10, 17))

bind_rows(revs2016, revs2017) %>% 
    group_by(`Withdrawal reason SUM`) %>% 
    mutate(diff = max(pct_approved) - min(pct_approved) >= 5) %>% 
    ungroup() %>% 
    ggplot(aes(x = factor(`Withdrawal reason SUM`), y = pct_approved, fill = factor(year), alpha = diff)) + 
    geom_bar(position = "dodge", stat = "identity") + 
    geom_label(aes(label = pct_approved), position = position_dodge(width = .9)) + 
    xlab("Withdrawal Code") + 
    ylab("Percent of Documents Approved") + 
    scale_fill_discrete(guide = guide_legend("Year"))
theme_bw() 

rm(revs2016, revs2017)

## Plot Places with Low Document Uploads and Low Percentages of Completion Information
### Chloropleth?

# Figure out which students in Students to be Changed... need to be updated again because of withdrawal code in 2,5,6,8,10,17 overwriting their included_in_cohort value to P
read.xlsx("K:/ORP_accountability/projects/2017_graduation_rate/Documentation for Cohort Changes/Students to be Changed in Cohort.xlsx") %>% 
    left_join(read_csv(paste0("K:/ORP_accountability/projects/2017_graduation_rate/Data/studentcohortdata_", read_dates[length(read_dates)], ".csv")),
              by = c("STUDENT_KEY" = "Student key")) %>% 
    filter(`Withdrawal reason SUM` %in% c(2, 5, 6, 8, 10, 17)) %>% 
    select(STUDENT_KEY, `Included in cohort`)







