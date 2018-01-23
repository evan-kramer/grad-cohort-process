# Dropout Calculations
# Evan Kramer
# 7/31/2017

library(tidyverse)
library(lubridate)
library(stringr)
library(haven)

date = str_replace_all(as.character(Sys.Date()), "-", "")

g2018 = T
g2017 = F
g2016 = F
g2015 = F
check = F

if(g2018 == T) {
    # Students who did not have a completion_type of 1 or 3 and were not enrolled the following year
    coh = read_csv("K:/ORP_accountability/data/2017_graduation_rate/student_level_20170830.csv") %>% 
        mutate(dropout_count = included_in_cohort == "Y" & withdrawal_code %in% c(0, 1, 3, 4) & 
                   (is.na(completion_type) | completion_type != 1))
    enr = read_csv("K:/ORP_accountability/projects/2018_graduation_rate/Data/studentenrollment_20171002.csv") %>% 
        transmute(year = `School Year` + 1, student_key = `Student Key`,
                  system = `Primary District Id`, school = `Primary School Id`,
                  withdrawal_code = `Withdrawal Reason SUM`, begin_date = dmy(`Begin Date`),
                  end_date = `End Date`, dropout_count = withdrawal_code %in% c(0, 1, 3, 4))
    
    ## Students who were potential or actual dropouts
    drop1 = coh %>% 
        filter(is.na(withdrawal_code) & (is.na(completion_type) | completion_type == 5) & included_in_cohort == "Y") %>% 
        transmute(student_key, dropout_count = T)
    
    ## Students who enrolled the following year AND were not also withdrawn
    drop2 = enr %>% 
        mutate(end_date = ifelse(is.na(end_date), as.Date(ymd(str_c(year, "0601"))), end_date)) %>% 
        arrange(student_key, desc(begin_date), desc(end_date)) %>% 
        group_by(student_key) %>% 
        summarize_each(funs(first(.)), year, system, school, withdrawal_code, begin_date, end_date) %>% 
        filter(!(withdrawal_code %in% c(0, 1, 3, 4)))
    
    ## Join
    drop = anti_join(drop1, drop2, by = "student_key")
    grad = left_join(coh, drop, by = "student_key") %>% 
        mutate(dropout_count = dropout_count.x == T | dropout_count.y == T) %>% 
        select(-dropout_count.x, -dropout_count.y)
    
    ## Checks
    grad %>% 
        group_by(system) %>% 
        summarize(grad_rate = round(100 * sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T) / 
                                        sum(included_in_cohort == "Y", na.rm = T), 1),
                  dropout_rate = round(100 * sum(included_in_cohort == "Y" & dropout_count == T, na.rm = T) / 
                                           sum(included_in_cohort == "Y", na.rm = T), 1)) %>% 
        mutate(grad_rate + dropout_rate) %>% 
        filter(`grad_rate + dropout_rate` > 100) %>% 
        print()
    
    # Output file
    #write_csv(grad, "K:/ORP_accountability/data/2017_graduation_rate/student_level_with_dropout_count_20171113.csv", na = "")
    
    ## Output state level file
    state_level_grad = grad %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(included_in_cohort == "Y" & dropout_count == T, na.rm = T)) %>% 
        mutate(grad_rate = round(100 * grad_count / grad_cohort, 1),
               dropout_rate = round(100 * dropout_count / grad_cohort, 1))
    #write_csv(state_level_grad, "K:/ORP_accountability/data/2017_graduation_rate/state_level_EK.csv", na = "")
}

if(g2017 == T) {
    # How many students did not have a completion_type of 1 or 3 and were not enrolled the following year (what about withdrawal codes?)
    ## Read in data
    setwd("K:/ORP_accountability/data/2016_graduation_rate")
    coh = read_csv("student_level_20161201.csv") %>% 
        mutate(dropout_count = included_in_cohort == "Y" & withdrawal_reason %in% c(0, 1, 3, 4) & (is.na(completion_type) | completion_type != 1))
    enr = read_tsv("enrollment2017_for_dropouts.txt") %>% 
        transmute(year = `School Year` + 1, student_key = `Student Key`, 
                  system = `Primary District Id`, school = `Primary School Id`,
                  withdrawal_reason = `Withdrawal Reason`, begin_date = dmy(`Begin Date`),
                  end_date = dmy(`End Date`), 
                  dropout_count = withdrawal_reason %in% c(0, 1, 3, 4))
    
    ## Students who were (potential or actual) dropouts
    drop1 = coh %>% 
        filter(is.na(withdrawal_reason) & (is.na(completion_type) | completion_type == 5) & included_in_cohort == "Y") %>% 
        transmute(student_key, dropout_count = T)
    
    ## Students who enrolled the following year (who were not also withdrawn?)
    drop2 = enr %>% 
        mutate(end_date = ifelse(is.na(end_date), as.Date(ymd(str_c(year, "0601"))), end_date)) %>% 
        arrange(student_key, desc(begin_date), desc(end_date)) %>% 
        group_by(student_key) %>% 
        summarize(year = first(year), system = first(system), school = first(school), 
                  withdrawal_reason = first(withdrawal_reason), begin_date = first(begin_date),
                  end_date = first(end_date)) %>% 
        filter(!(withdrawal_reason %in% c(0, 1, 3, 4)))
    
    ## Join
    drop = anti_join(drop1, drop2, by = "student_key")
    grad = left_join(coh, drop, by = "student_key") %>% 
        mutate(dropout_count = dropout_count.x == T | dropout_count.y == T) %>% 
        select(-dropout_count.x, -dropout_count.y)
    
    ## Do some checks
    grad %>% filter(dropout_count == T) %>% print()
    grad %>% 
        group_by(system) %>% 
        summarize(grad_rate = round(100 * sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T) / 
                                        sum(included_in_cohort == "Y", na.rm = T), 1),
                  dropout_rate = round(100 * sum(included_in_cohort == "Y" & dropout_count == T, na.rm = T) / 
                                           sum(included_in_cohort == "Y", na.rm = T), 1)) %>% 
        mutate(grad_rate + dropout_rate) %>% 
        filter(`grad_rate + dropout_rate` > 100)
    
    # State Level
    ## All Students    
    b_sta = grad %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        mutate(subgroup = "All Students", system = 0, school = 0)
    
    ## Super Subgroup
    j = grad %>% 
        filter(race_ethnicity %in% c("B", "H", "I") | econ_dis == "Y" | ell == "Y" | sped == "Y") %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        mutate(subgroup = "Super Subgroup", system = 0, school = 0)
    b_sta = bind_rows(b_sta, j)
    
    ## Racial/ethnic groups    
    races = c("A", "B", "H", "I", "P", "W")
    for(i in seq_along(races)) { 
        j = grad %>% 
            filter(race_ethnicity == races[i]) %>% 
            summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                      grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                      dropout_count = sum(dropout_count, na.rm = T)) %>% 
            mutate(subgroup = races[i], system = 0, school = 0)
        b_sta = bind_rows(b_sta, j)
    }
    
    ## BHN
    j = grad %>% 
        filter(race_ethnicity %in% c("B", "H", "I")) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        mutate(subgroup = "Black/Hispanic/Native American", system = 0, school = 0)
    b_sta = bind_rows(b_sta, j)
    
    ## ED
    j = grad %>% 
        filter(econ_dis == "Y") %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        mutate(subgroup = "Economically Disadvantaged", system = 0, school = 0)
    b_sta = bind_rows(b_sta, j)
    
    ## Non-ED
    j = grad %>% 
        filter(econ_dis == "N") %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        mutate(subgroup = "Non-Economically Disadvantaged", system = 0, school = 0)
    b_sta = bind_rows(b_sta, j)
    
    ## EL
    j = grad %>% 
        filter(ell == "Y") %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        mutate(subgroup = "English Learners", system = 0, school = 0)
    b_sta = bind_rows(b_sta, j)
    
    ## Non-EL
    j = grad %>% 
        filter(ell == "N") %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        mutate(subgroup = "Non-English Learners", system = 0, school = 0)
    b_sta = bind_rows(b_sta, j)
    
    ## SWD
    j = grad %>% 
        filter(sped == "Y") %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        mutate(subgroup = "Students with Disabilities", system = 0, school = 0)
    b_sta = bind_rows(b_sta, j)
    
    ## Non-SWD
    j = grad %>% 
        filter(sped == "N") %>%
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        mutate(subgroup = "Non-Students with Disabilities", system = 0, school = 0)
    b_sta = bind_rows(b_sta, j)
    
    # District Level
    ## All Students    
    b_sys = grad %>% 
        group_by(system) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "All Students", school = 0)
    
    ## Super Subgroup
    j = grad %>% 
        filter(race_ethnicity %in% c("B", "H", "I") | econ_dis == "Y" | ell == "Y" | sped == "Y") %>% 
        group_by(system) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Super Subgroup", school = 0)
    b_sys = bind_rows(b_sys, j)
    
    ## Racial/ethnic groups    
    races = c("A", "B", "H", "I", "P", "W")
    for(i in seq_along(races)) { 
        j = grad %>% 
            filter(race_ethnicity == races[i]) %>% 
            group_by(system) %>% 
            summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                      grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                      dropout_count = sum(dropout_count, na.rm = T)) %>% 
            ungroup() %>% 
            mutate(subgroup = races[i], school = 0)
        b_sys = bind_rows(b_sys, j)
    }
    
    ## BHN
    j = grad %>% 
        filter(race_ethnicity %in% c("B", "H", "I")) %>% 
        group_by(system) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Black/Hispanic/Native American", school = 0)
    b_sys = bind_rows(b_sys, j)
    
    ## ED
    j = grad %>% 
        filter(econ_dis == "Y") %>% 
        group_by(system) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Economically Disadvantaged", school = 0)
    b_sys = bind_rows(b_sys, j)
    
    ## Non-ED
    j = grad %>% 
        filter(econ_dis == "N") %>% 
        group_by(system) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Non-Economically Disadvantaged", school = 0)
    b_sys = bind_rows(b_sys, j)
    
    ## EL
    j = grad %>% 
        filter(ell == "Y") %>% 
        group_by(system) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "English Learners", school = 0)
    b_sys = bind_rows(b_sys, j)
    
    ## Non-EL
    j = grad %>% 
        filter(ell == "N") %>% 
        group_by(system) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Non-English Learners", school = 0)
    b_sys = bind_rows(b_sys, j)
    
    ## SWD
    j = grad %>% 
        filter(sped == "Y") %>% 
        group_by(system) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Students with Disabilities", school = 0)
    b_sys = bind_rows(b_sys, j)
    
    ## Non-SWD
    j = grad %>% 
        filter(sped == "N") %>% 
        group_by(system) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Non-Students with Disabilities", school = 0)
    b_sys = bind_rows(b_sys, j)
    
    # School Level
    ## All Students    
    b_sch = grad %>% 
        group_by(system, school) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "All Students")
    
    ## Super Subgroup
    j = grad %>% 
        filter(race_ethnicity %in% c("B", "H", "I") | econ_dis == "Y" | ell == "Y" | sped == "Y") %>% 
        group_by(system, school) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Super Subgroup")
    b_sys = bind_rows(b_sys, j)
    
    ## Racial/ethnic groups    
    races = c("A", "B", "H", "I", "P", "W")
    for(i in seq_along(races)) { 
        j = grad %>% 
            filter(race_ethnicity == races[i]) %>% 
            group_by(system, school) %>% 
            summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                      grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                      dropout_count = sum(dropout_count, na.rm = T)) %>% 
            ungroup() %>% 
            mutate(subgroup = races[i])
        b_sch = bind_rows(b_sch, j)
    }
    
    ## BHN
    j = grad %>% 
        filter(race_ethnicity %in% c("B", "H", "I")) %>% 
        group_by(system, school) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Black/Hispanic/Native American")
    b_sch = bind_rows(b_sch, j)
    
    ## ED
    j = grad %>% 
        filter(econ_dis == "Y") %>% 
        group_by(system, school) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Economically Disadvantaged")
    b_sch = bind_rows(b_sch, j)
    
    ## Non-ED
    j = grad %>% 
        filter(econ_dis == "N") %>% 
        group_by(system, school) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Non-Economically Disadvantaged")
    b_sch = bind_rows(b_sch, j)
    
    ## EL
    j = grad %>% 
        filter(ell == "Y") %>% 
        group_by(system, school) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "English Learners")
    b_sch = bind_rows(b_sch, j)
    
    ## Non-EL
    j = grad %>% 
        filter(ell == "N") %>% 
        group_by(system, school) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Non-English Learners")
    b_sch = bind_rows(b_sch, j)
    
    ## SWD
    j = grad %>% 
        filter(sped == "Y") %>% 
        group_by(system, school) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Students with Disabilities")
    b_sch = bind_rows(b_sch, j)
    
    ## Non-SWD
    j = grad %>% 
        filter(sped == "N") %>% 
        group_by(system, school) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Non-Students with Disabilities")
    b_sch = bind_rows(b_sch, j)
    
    # Calculate rates and replace subgroup names
    b = bind_rows(b_sta, b_sys, b_sch) %>% 
        mutate(system = as.numeric(system)) %>% 
        left_join(read_dta("C:/Users/CA19130/Documents/Data/Crosswalks/system_school_crosswalk.dta"),
                  by = c("system", "school")) %>% 
        mutate(subject = "Graduation Rate",
               school_name = ifelse(system == 61 & school == 40, "F.I. Denning Center of Technology and Careers", school_name),
               school_name = ifelse(system %in% c(0, 90) & school == 0, "All Schools", school_name),
               school_name = ifelse(system == 90 & school == 7, "Carroll County Technical Center", school_name),
               school_name = ifelse(system == 110 & school == 40, "Riverside Academy", school_name),
               school_name = ifelse(system == 190 & school == 740, "Johnson Alternative Learning Center", school_name),
               school_name = ifelse(system == 190 & school == 745, "The Cohn Learning Center", school_name),
               school_name = ifelse(system == 470 & school == 225, "Fort Sanders Education Development Center", school_name),
               school_name = ifelse(system == 580 & school == 75, "Central Prep Academy", school_name),
               school_name = ifelse(system == 650 & school == 46, "Morgan County Career and Technical Center", school_name),
               school_name = ifelse(system == 792 & school == 2700, "Exceptional Children Placements", school_name),
               school_name = ifelse(system == 792 & school == 8105, "Thurgood Marshall High School of Career Development", school_name),
               school_name = ifelse(system == 792 & school == 8275, "The Excel Center", school_name),
               school_name = ifelse(system == 822 & school == 60, "Cora Cox Academy", school_name),
               school_name = ifelse(system == 830 & school == 155, "Sumner County Middle College High School", school_name),
               school_name = ifelse(system == 830 & school == 1010, "Sumner County Middle Technical College High School at Portland", school_name),
               school_name = ifelse(system == 900 & school == 115, "Tennessee Virtual Learning Academy", school_name),
               school_name = ifelse(system == 985 & school == 45, "Pathways in Education - TN", school_name),
               school_name = ifelse(system == 985 & school == 50, "Pathways in Education - Whitehaven", school_name),
               school_name = ifelse(system == 985 & school == 8055, "Fairley High School", school_name),
               school_name = ifelse(system == 985 & school == 8065, "Martin Luther King Preparatory High School", school_name),
               system_name = ifelse(system == 90, "Carroll County", system_name),
               system_name = ifelse(system == 0, "State of Tennessee", system_name),
               system_name = dendextend::na_locf(system_name),
               subgroup = ifelse(subgroup == "A", "Asian", subgroup),
               subgroup = ifelse(subgroup == "B", "Black or African American", subgroup),
               subgroup = ifelse(subgroup == "H", "Hispanic", subgroup),
               subgroup = ifelse(subgroup == "I", "American Indian or Alaska Native", subgroup),
               subgroup = ifelse(subgroup == "P", "Native Hawaiian or Other Pacific Islander", subgroup),
               subgroup = ifelse(subgroup == "W", "White", subgroup),
               grad_rate = round(100 * grad_count / grad_cohort, 1),
               dropout_rate = round(100 * dropout_count / grad_cohort, 1)) %>% 
        arrange(system, school, subgroup) %>% 
        select(starts_with("system"), starts_with("school"), subgroup, subject, 
               grad_cohort, grad_count, grad_rate, dropout_count, dropout_rate) %>% 
        filter(!is.na(system) & system != 791) %>% 
        mutate_each(funs(ifelse(is.nan(.), NA, .)), ends_with("_rate"))
    
    write_csv(b, "K:/ORP_accountability/data/2016_graduation_rate/grad_rate_base_EK.csv", na = "")
    
    # Suppress for DMR Data Downloads file
}

if(g2016 == T) {
    # How many students did not have a completion_type of 1 or 3 and were not enrolled the following year (what about withdrawal codes?)
    ## Read in data
    setwd("K:/ORP_accountability/data/2016_graduation_rate")
    coh = read_csv("student_level_20161201.csv") %>% 
        mutate(dropout_count = included_in_cohort == "Y" & withdrawal_reason %in% c(0, 1, 3, 4) & (is.na(completion_type) | completion_type != 1))
    enr = read_tsv("enrollment2017_for_dropouts.txt") %>% 
        transmute(year = `School Year` + 1, student_key = `Student Key`, 
                  system = `Primary District Id`, school = `Primary School Id`,
                  withdrawal_reason = `Withdrawal Reason`, begin_date = dmy(`Begin Date`),
                  end_date = dmy(`End Date`), 
                  dropout_count = withdrawal_reason %in% c(0, 1, 3, 4))
    
    ## Students who were (potential or actual) dropouts
    drop1 = coh %>% 
        filter(is.na(withdrawal_reason) & (is.na(completion_type) | completion_type == 5) & included_in_cohort == "Y") %>% 
        transmute(student_key, dropout_count = T)
    
    ## Students who enrolled the following year (who were not also withdrawn?)
    drop2 = enr %>% 
        mutate(end_date = ifelse(is.na(end_date), as.Date(ymd(str_c(year, "0601"))), end_date)) %>% 
        arrange(student_key, desc(begin_date), desc(end_date)) %>% 
        group_by(student_key) %>% 
        summarize(year = first(year), system = first(system), school = first(school), 
                  withdrawal_reason = first(withdrawal_reason), begin_date = first(begin_date),
                  end_date = first(end_date)) %>% 
        filter(!(withdrawal_reason %in% c(0, 1, 3, 4)))
    
    ## Join
    drop = anti_join(drop1, drop2, by = "student_key")
    grad = left_join(coh, drop, by = "student_key") %>% 
        mutate(dropout_count = dropout_count.x == T | dropout_count.y == T) %>% 
        select(-dropout_count.x, -dropout_count.y)
    
    ## Do some checks
    grad %>% filter(dropout_count == T) %>% print()
    grad %>% 
        group_by(system) %>% 
        summarize(grad_rate = round(100 * sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T) / 
                                        sum(included_in_cohort == "Y", na.rm = T), 1),
                  dropout_rate = round(100 * sum(included_in_cohort == "Y" & dropout_count == T, na.rm = T) / 
                                           sum(included_in_cohort == "Y", na.rm = T), 1)) %>% 
        mutate(grad_rate + dropout_rate) %>% 
        filter(`grad_rate + dropout_rate` > 100)
    
    # State Level
    ## All Students    
    b_sta = grad %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        mutate(subgroup = "All Students", system = 0, school = 0)
    
    ## Super Subgroup
    j = grad %>% 
        filter(race_ethnicity %in% c("B", "H", "I") | econ_dis == "Y" | ell == "Y" | sped == "Y") %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        mutate(subgroup = "Super Subgroup", system = 0, school = 0)
    b_sta = bind_rows(b_sta, j)
    
    ## Racial/ethnic groups    
    races = c("A", "B", "H", "I", "P", "W")
    for(i in seq_along(races)) { 
        j = grad %>% 
            filter(race_ethnicity == races[i]) %>% 
            summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                      grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                      dropout_count = sum(dropout_count, na.rm = T)) %>% 
            mutate(subgroup = races[i], system = 0, school = 0)
        b_sta = bind_rows(b_sta, j)
    }
    
    ## BHN
    j = grad %>% 
        filter(race_ethnicity %in% c("B", "H", "I")) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        mutate(subgroup = "Black/Hispanic/Native American", system = 0, school = 0)
    b_sta = bind_rows(b_sta, j)
    
    ## ED
    j = grad %>% 
        filter(econ_dis == "Y") %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        mutate(subgroup = "Economically Disadvantaged", system = 0, school = 0)
    b_sta = bind_rows(b_sta, j)
    
    ## Non-ED
    j = grad %>% 
        filter(econ_dis == "N") %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        mutate(subgroup = "Non-Economically Disadvantaged", system = 0, school = 0)
    b_sta = bind_rows(b_sta, j)
    
    ## EL
    j = grad %>% 
        filter(ell == "Y") %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        mutate(subgroup = "English Learners", system = 0, school = 0)
    b_sta = bind_rows(b_sta, j)
    
    ## Non-EL
    j = grad %>% 
        filter(ell == "N") %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        mutate(subgroup = "Non-English Learners", system = 0, school = 0)
    b_sta = bind_rows(b_sta, j)
    
    ## SWD
    j = grad %>% 
        filter(sped == "Y") %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        mutate(subgroup = "Students with Disabilities", system = 0, school = 0)
    b_sta = bind_rows(b_sta, j)
    
    ## Non-SWD
    j = grad %>% 
        filter(sped == "N") %>%
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        mutate(subgroup = "Non-Students with Disabilities", system = 0, school = 0)
    b_sta = bind_rows(b_sta, j)
    
    # District Level
    ## All Students    
    b_sys = grad %>% 
        group_by(system) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "All Students", school = 0)
    
    ## Super Subgroup
    j = grad %>% 
        filter(race_ethnicity %in% c("B", "H", "I") | econ_dis == "Y" | ell == "Y" | sped == "Y") %>% 
        group_by(system) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Super Subgroup", school = 0)
    b_sys = bind_rows(b_sys, j)
    
    ## Racial/ethnic groups    
    races = c("A", "B", "H", "I", "P", "W")
    for(i in seq_along(races)) { 
        j = grad %>% 
            filter(race_ethnicity == races[i]) %>% 
            group_by(system) %>% 
            summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                      grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                      dropout_count = sum(dropout_count, na.rm = T)) %>% 
            ungroup() %>% 
            mutate(subgroup = races[i], school = 0)
        b_sys = bind_rows(b_sys, j)
    }
    
    ## BHN
    j = grad %>% 
        filter(race_ethnicity %in% c("B", "H", "I")) %>% 
        group_by(system) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Black/Hispanic/Native American", school = 0)
    b_sys = bind_rows(b_sys, j)
    
    ## ED
    j = grad %>% 
        filter(econ_dis == "Y") %>% 
        group_by(system) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Economically Disadvantaged", school = 0)
    b_sys = bind_rows(b_sys, j)
    
    ## Non-ED
    j = grad %>% 
        filter(econ_dis == "N") %>% 
        group_by(system) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Non-Economically Disadvantaged", school = 0)
    b_sys = bind_rows(b_sys, j)
    
    ## EL
    j = grad %>% 
        filter(ell == "Y") %>% 
        group_by(system) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "English Learners", school = 0)
    b_sys = bind_rows(b_sys, j)
    
    ## Non-EL
    j = grad %>% 
        filter(ell == "N") %>% 
        group_by(system) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Non-English Learners", school = 0)
    b_sys = bind_rows(b_sys, j)
    
    ## SWD
    j = grad %>% 
        filter(sped == "Y") %>% 
        group_by(system) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Students with Disabilities", school = 0)
    b_sys = bind_rows(b_sys, j)
    
    ## Non-SWD
    j = grad %>% 
        filter(sped == "N") %>% 
        group_by(system) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Non-Students with Disabilities", school = 0)
    b_sys = bind_rows(b_sys, j)
    
    # School Level
    ## All Students    
    b_sch = grad %>% 
        group_by(system, school) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "All Students")
    
    ## Super Subgroup
    j = grad %>% 
        filter(race_ethnicity %in% c("B", "H", "I") | econ_dis == "Y" | ell == "Y" | sped == "Y") %>% 
        group_by(system, school) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Super Subgroup")
    b_sys = bind_rows(b_sys, j)
    
    ## Racial/ethnic groups    
    races = c("A", "B", "H", "I", "P", "W")
    for(i in seq_along(races)) { 
        j = grad %>% 
            filter(race_ethnicity == races[i]) %>% 
            group_by(system, school) %>% 
            summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                      grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                      dropout_count = sum(dropout_count, na.rm = T)) %>% 
            ungroup() %>% 
            mutate(subgroup = races[i])
        b_sch = bind_rows(b_sch, j)
    }
    
    ## BHN
    j = grad %>% 
        filter(race_ethnicity %in% c("B", "H", "I")) %>% 
        group_by(system, school) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Black/Hispanic/Native American")
    b_sch = bind_rows(b_sch, j)
    
    ## ED
    j = grad %>% 
        filter(econ_dis == "Y") %>% 
        group_by(system, school) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Economically Disadvantaged")
    b_sch = bind_rows(b_sch, j)
    
    ## Non-ED
    j = grad %>% 
        filter(econ_dis == "N") %>% 
        group_by(system, school) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Non-Economically Disadvantaged")
    b_sch = bind_rows(b_sch, j)
    
    ## EL
    j = grad %>% 
        filter(ell == "Y") %>% 
        group_by(system, school) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "English Learners")
    b_sch = bind_rows(b_sch, j)
    
    ## Non-EL
    j = grad %>% 
        filter(ell == "N") %>% 
        group_by(system, school) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Non-English Learners")
    b_sch = bind_rows(b_sch, j)
    
    ## SWD
    j = grad %>% 
        filter(sped == "Y") %>% 
        group_by(system, school) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Students with Disabilities")
    b_sch = bind_rows(b_sch, j)
    
    ## Non-SWD
    j = grad %>% 
        filter(sped == "N") %>% 
        group_by(system, school) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Non-Students with Disabilities")
    b_sch = bind_rows(b_sch, j)
    
    # Calculate rates and replace subgroup names
    b = bind_rows(b_sta, b_sys, b_sch) %>% 
        mutate(system = as.numeric(system)) %>% 
        left_join(read_dta("C:/Users/CA19130/Documents/Data/Crosswalks/system_school_crosswalk.dta"),
                  by = c("system", "school")) %>% 
        mutate(subject = "Graduation Rate",
               school_name = ifelse(system == 61 & school == 40, "F.I. Denning Center of Technology and Careers", school_name),
               school_name = ifelse(system %in% c(0, 90) & school == 0, "All Schools", school_name),
               school_name = ifelse(system == 90 & school == 7, "Carroll County Technical Center", school_name),
               school_name = ifelse(system == 110 & school == 40, "Riverside Academy", school_name),
               school_name = ifelse(system == 190 & school == 740, "Johnson Alternative Learning Center", school_name),
               school_name = ifelse(system == 190 & school == 745, "The Cohn Learning Center", school_name),
               school_name = ifelse(system == 470 & school == 225, "Fort Sanders Education Development Center", school_name),
               school_name = ifelse(system == 580 & school == 75, "Central Prep Academy", school_name),
               school_name = ifelse(system == 650 & school == 46, "Morgan County Career and Technical Center", school_name),
               school_name = ifelse(system == 792 & school == 2700, "Exceptional Children Placements", school_name),
               school_name = ifelse(system == 792 & school == 8105, "Thurgood Marshall High School of Career Development", school_name),
               school_name = ifelse(system == 792 & school == 8275, "The Excel Center", school_name),
               school_name = ifelse(system == 822 & school == 60, "Cora Cox Academy", school_name),
               school_name = ifelse(system == 830 & school == 155, "Sumner County Middle College High School", school_name),
               school_name = ifelse(system == 830 & school == 1010, "Sumner County Middle Technical College High School at Portland", school_name),
               school_name = ifelse(system == 900 & school == 115, "Tennessee Virtual Learning Academy", school_name),
               school_name = ifelse(system == 985 & school == 45, "Pathways in Education - TN", school_name),
               school_name = ifelse(system == 985 & school == 50, "Pathways in Education - Whitehaven", school_name),
               school_name = ifelse(system == 985 & school == 8055, "Fairley High School", school_name),
               school_name = ifelse(system == 985 & school == 8065, "Martin Luther King Preparatory High School", school_name),
               system_name = ifelse(system == 90, "Carroll County", system_name),
               system_name = ifelse(system == 0, "State of Tennessee", system_name),
               system_name = dendextend::na_locf(system_name),
               subgroup = ifelse(subgroup == "A", "Asian", subgroup),
               subgroup = ifelse(subgroup == "B", "Black or African American", subgroup),
               subgroup = ifelse(subgroup == "H", "Hispanic", subgroup),
               subgroup = ifelse(subgroup == "I", "American Indian or Alaska Native", subgroup),
               subgroup = ifelse(subgroup == "P", "Native Hawaiian or Other Pacific Islander", subgroup),
               subgroup = ifelse(subgroup == "W", "White", subgroup),
               grad_rate = round(100 * grad_count / grad_cohort, 1),
               dropout_rate = round(100 * dropout_count / grad_cohort, 1)) %>% 
        arrange(system, school, subgroup) %>% 
        select(starts_with("system"), starts_with("school"), subgroup, subject, 
               grad_cohort, grad_count, grad_rate, dropout_count, dropout_rate) %>% 
        filter(!is.na(system) & system != 791) %>% 
        mutate_each(funs(ifelse(is.nan(.), NA, .)), ends_with("_rate"))
    
    write_csv(b, "K:/ORP_accountability/data/2016_graduation_rate/grad_rate_base_EK.csv", na = "")
    
    # Suppress for DMR Data Downloads file
}

if(g2015 == T) {
    # How many students did not have a completion_type of 1 or 3 and were not enrolled the following year (what about withdrawal codes?)
    ## Read in data
    setwd("K:/ORP_accountability/data/2015_graduation_rate/grad_data_2014-15")
    coh = read_dta("2011GradCohort.dta") %>% 
        mutate(dropout_count = includedincohort == "Y" & withdrawalreason %in% c(0, 1, 3, 4) & (is.na(completiontype) | completiontype != 1))
    enr = read_csv("enrollment2016_for_dropouts.csv") %>% 
        transmute(year = `School Year` + 1, student_key = as.numeric(`Student Key`), 
                  system = `Primary District Id`, school = `Primary School Id`,
                  withdrawal_reason = `Withdrawal Reason`, begin_date = dmy(`Begin Date`),
                  end_date = dmy(`End Date`), 
                  dropout_count = withdrawal_reason %in% c(0, 1, 3, 4))

    ## Students who were (potential or actual) dropouts
    drop1 = coh %>% 
        filter(is.na(withdrawalreason) & (is.na(completiontype) | completiontype == 5) & includedincohort == "Y") %>% 
        transmute(student_key = studentkey, dropout_count = T)
    
    ## Students who enrolled the following year (who were not also withdrawn?)
    drop2 = enr %>% 
        mutate(end_date = ifelse(is.na(end_date), as.Date(ymd(str_c(year, "0601"))), end_date)) %>% 
        arrange(student_key, desc(begin_date), desc(end_date)) %>% 
        group_by(student_key) %>% 
        summarize(year = first(year), system = first(system), school = first(school), 
                  withdrawal_reason = first(withdrawal_reason), begin_date = first(begin_date),
                  end_date = first(end_date)) %>% 
        filter(!(withdrawal_reason %in% c(0, 1, 3, 4)))
    
    ## Join
    drop = anti_join(drop1, drop2, by = "student_key")
    grad = left_join(coh, drop, by = c("studentkey" = "student_key")) %>% 
        mutate(dropout_count = dropout_count.x == T | dropout_count.y == T) %>% 
        select(-dropout_count.x, -dropout_count.y) %>% 
        rename(included_in_cohort = includedincohort, completion_type = completiontype,
               race_ethnicity = raceethnicity, econ_dis = econdis, system = districtno,
               school = schoolno) %>%
    
    # State Level
    ## All Students    
    b_sta = grad %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        mutate(subgroup = "All Students", system = 0, school = 0)
    
    ## Super Subgroup
    j = grad %>% 
        filter(race_ethnicity %in% c("B", "H", "I") | econ_dis == "Y" | ell == "Y" | sped == "Y") %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        mutate(subgroup = "Super Subgroup", system = 0, school = 0)
    b_sta = bind_rows(b_sta, j)
    
    ## Racial/ethnic groups    
    races = c("A", "B", "H", "I", "P", "W")
    for(i in seq_along(races)) { 
        j = grad %>% 
            filter(race_ethnicity == races[i]) %>% 
            summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                      grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                      dropout_count = sum(dropout_count, na.rm = T)) %>% 
            mutate(subgroup = races[i], system = 0, school = 0)
        b_sta = bind_rows(b_sta, j)
    }
    
    ## BHN
    j = grad %>% 
        filter(race_ethnicity %in% c("B", "H", "I")) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        mutate(subgroup = "Black/Hispanic/Native American", system = 0, school = 0)
    b_sta = bind_rows(b_sta, j)
    
    ## ED
    j = grad %>% 
        filter(econ_dis == "Y") %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        mutate(subgroup = "Economically Disadvantaged", system = 0, school = 0)
    b_sta = bind_rows(b_sta, j)
    
    ## Non-ED
    j = grad %>% 
        filter(econ_dis == "N") %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        mutate(subgroup = "Non-Economically Disadvantaged", system = 0, school = 0)
    b_sta = bind_rows(b_sta, j)
    
    ## EL
    j = grad %>% 
        filter(ell == "Y") %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        mutate(subgroup = "English Learners", system = 0, school = 0)
    b_sta = bind_rows(b_sta, j)
    
    ## Non-EL
    j = grad %>% 
        filter(ell == "N") %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        mutate(subgroup = "Non-English Learners", system = 0, school = 0)
    b_sta = bind_rows(b_sta, j)
    
    ## SWD
    j = grad %>% 
        filter(sped == "Y") %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        mutate(subgroup = "Students with Disabilities", system = 0, school = 0)
    b_sta = bind_rows(b_sta, j)
    
    ## Non-SWD
    j = grad %>% 
        filter(sped == "N") %>%
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        mutate(subgroup = "Non-Students with Disabilities", system = 0, school = 0)
    b_sta = bind_rows(b_sta, j)
    
    # District Level
    ## All Students    
    b_sys = grad %>% 
        group_by(system) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "All Students", school = 0)
    
    ## Super Subgroup
    j = grad %>% 
        filter(race_ethnicity %in% c("B", "H", "I") | econ_dis == "Y" | ell == "Y" | sped == "Y") %>% 
        group_by(system) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Super Subgroup", school = 0)
    b_sys = bind_rows(b_sys, j)
    
    ## Racial/ethnic groups    
    races = c("A", "B", "H", "I", "P", "W")
    for(i in seq_along(races)) { 
        j = grad %>% 
            filter(race_ethnicity == races[i]) %>% 
            group_by(system) %>% 
            summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                      grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                      dropout_count = sum(dropout_count, na.rm = T)) %>% 
            ungroup() %>% 
            mutate(subgroup = races[i], school = 0)
        b_sys = bind_rows(b_sys, j)
    }
    
    ## BHN
    j = grad %>% 
        filter(race_ethnicity %in% c("B", "H", "I")) %>% 
        group_by(system) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Black/Hispanic/Native American", school = 0)
    b_sys = bind_rows(b_sys, j)
    
    ## ED
    j = grad %>% 
        filter(econ_dis == "Y") %>% 
        group_by(system) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Economically Disadvantaged", school = 0)
    b_sys = bind_rows(b_sys, j)
    
    ## Non-ED
    j = grad %>% 
        filter(econ_dis == "N") %>% 
        group_by(system) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Non-Economically Disadvantaged", school = 0)
    b_sys = bind_rows(b_sys, j)
    
    ## EL
    j = grad %>% 
        filter(ell == "Y") %>% 
        group_by(system) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "English Learners", school = 0)
    b_sys = bind_rows(b_sys, j)
    
    ## Non-EL
    j = grad %>% 
        filter(ell == "N") %>% 
        group_by(system) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Non-English Learners", school = 0)
    b_sys = bind_rows(b_sys, j)
    
    ## SWD
    j = grad %>% 
        filter(sped == "Y") %>% 
        group_by(system) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Students with Disabilities", school = 0)
    b_sys = bind_rows(b_sys, j)
    
    ## Non-SWD
    j = grad %>% 
        filter(sped == "N") %>% 
        group_by(system) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Non-Students with Disabilities", school = 0)
    b_sys = bind_rows(b_sys, j)
    
    # School Level
    ## All Students    
    b_sch = grad %>% 
        group_by(system, school) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "All Students")
    
    ## Super Subgroup
    j = grad %>% 
        filter(race_ethnicity %in% c("B", "H", "I") | econ_dis == "Y" | ell == "Y" | sped == "Y") %>% 
        group_by(system, school) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Super Subgroup")
    b_sys = bind_rows(b_sys, j)
    
    ## Racial/ethnic groups    
    races = c("A", "B", "H", "I", "P", "W")
    for(i in seq_along(races)) { 
        j = grad %>% 
            filter(race_ethnicity == races[i]) %>% 
            group_by(system, school) %>% 
            summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                      grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                      dropout_count = sum(dropout_count, na.rm = T)) %>% 
            ungroup() %>% 
            mutate(subgroup = races[i])
        b_sch = bind_rows(b_sch, j)
    }
    
    ## BHN
    j = grad %>% 
        filter(race_ethnicity %in% c("B", "H", "I")) %>% 
        group_by(system, school) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Black/Hispanic/Native American")
    b_sch = bind_rows(b_sch, j)
    
    ## ED
    j = grad %>% 
        filter(econ_dis == "Y") %>% 
        group_by(system, school) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Economically Disadvantaged")
    b_sch = bind_rows(b_sch, j)
    
    ## Non-ED
    j = grad %>% 
        filter(econ_dis == "N") %>% 
        group_by(system, school) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Non-Economically Disadvantaged")
    b_sch = bind_rows(b_sch, j)
    
    ## EL
    j = grad %>% 
        filter(ell == "Y") %>% 
        group_by(system, school) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "English Learners")
    b_sch = bind_rows(b_sch, j)
    
    ## Non-EL
    j = grad %>% 
        filter(ell == "N") %>% 
        group_by(system, school) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Non-English Learners")
    b_sch = bind_rows(b_sch, j)
    
    ## SWD
    j = grad %>% 
        filter(sped == "Y") %>% 
        group_by(system, school) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Students with Disabilities")
    b_sch = bind_rows(b_sch, j)
    
    ## Non-SWD
    j = grad %>% 
        filter(sped == "N") %>% 
        group_by(system, school) %>% 
        summarize(grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                  grad_count = sum(included_in_cohort == "Y" & completion_type == 1, na.rm = T),
                  dropout_count = sum(dropout_count, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(subgroup = "Non-Students with Disabilities")
    b_sch = bind_rows(b_sch, j)
    
    # Calculate rates and replace subgroup names
    b = bind_rows(b_sta, b_sys, b_sch) %>% 
        mutate(system = as.numeric(system)) %>% 
        left_join(read_dta("C:/Users/CA19130/Documents/Data/Crosswalks/system_school_crosswalk.dta"),
                  by = c("system", "school")) %>% 
        arrange(system, school) %>% 
        mutate(subject = "Graduation Rate",
               school_name = ifelse(system == 61 & school == 40, "F.I. Denning Center of Technology and Careers", school_name),
               school_name = ifelse(system %in% c(0, 90) & school == 0, "All Schools", school_name),
               school_name = ifelse(system == 90 & school == 7, "Carroll County Technical Center", school_name),
               school_name = ifelse(system == 110 & school == 40, "Riverside Academy", school_name),
               school_name = ifelse(system == 190 & school == 400, "Johnson School", school_name),
               school_name = ifelse(system == 190 & school == 740, "Johnson Alternative Learning Center", school_name),
               school_name = ifelse(system == 190 & school == 745, "The Cohn Learning Center", school_name),
               school_name = ifelse(system == 290 & school == 20, "Rutledge High School", school_name),
               school_name = ifelse(system == 470 & school == 225, "Fort Sanders Education Development Center", school_name),
               school_name = ifelse(system == 570 & school == 135, "Whitehall Pre-K Center", school_name),
               school_name = ifelse(system == 580 & school == 75, "Central Prep Academy", school_name),
               school_name = ifelse(system == 650 & school == 46, "Morgan County Career and Technical Center", school_name),
               school_name = ifelse(system == 792 & school == 2700, "Exceptional Children Placements", school_name),
               school_name = ifelse(system == 792 & school == 8105, "Thurgood Marshall High School of Career Development", school_name),
               school_name = ifelse(system == 792 & school == 8275, "The Excel Center", school_name),
               school_name = ifelse(system == 822 & school == 60, "Cora Cox Academy", school_name),
               school_name = ifelse(system == 830 & school == 155, "Sumner County Middle College High School", school_name),
               school_name = ifelse(system == 830 & school == 1010, "Sumner County Middle Technical College High School at Portland", school_name),
               school_name = ifelse(system == 900 & school == 115, "Tennessee Virtual Learning Academy", school_name),
               school_name = ifelse(system == 985 & school == 45, "Pathways in Education - TN", school_name),
               school_name = ifelse(system == 985 & school == 50, "Pathways in Education - Whitehaven", school_name),
               school_name = ifelse(system == 985 & school == 8055, "Fairley High School", school_name),
               school_name = ifelse(system == 985 & school == 8065, "Martin Luther King Preparatory High School", school_name),
               system_name = ifelse(system == 90, "Carroll County", system_name),
               system_name = ifelse(system == 0, "State of Tennessee", system_name),
               system_name = dendextend::na_locf(system_name),
               subgroup = ifelse(subgroup == "A", "Asian", subgroup),
               subgroup = ifelse(subgroup == "B", "Black or African American", subgroup),
               subgroup = ifelse(subgroup == "H", "Hispanic", subgroup),
               subgroup = ifelse(subgroup == "I", "American Indian or Alaska Native", subgroup),
               subgroup = ifelse(subgroup == "P", "Native Hawaiian or Other Pacific Islander", subgroup),
               subgroup = ifelse(subgroup == "W", "White", subgroup),
               grad_rate = round(100 * grad_count / grad_cohort, 1),
               dropout_rate = round(100 * dropout_count / grad_cohort, 1)) %>% 
        arrange(system, school, subgroup) %>% 
        select(starts_with("system"), starts_with("school"), subgroup, subject, 
               grad_cohort, grad_count, grad_rate, dropout_count, dropout_rate) %>% 
        filter(!is.na(system) & system != 791) %>% 
        mutate_each(funs(ifelse(is.nan(.), NA, .)), ends_with("_rate")) 
    
    write_csv(b, "K:/ORP_accountability/data/2015_graduation_rate/grad_rate_base_EK.csv", na = "")
    
    # Suppress for DMR Data Downloads file
}

if(check == T) {
    # Prepare data
    setwd("K:/ORP_accountability/data/2015_graduation_rate")
    enr = read_csv("grad_data_2014-15/enrollment2016_for_dropouts.csv") %>% 
        transmute(year = `School Year` + 1, student_key = `Student Key`, 
                  system = `Primary District Id`, school = `Primary School Id`,
                  withdrawal_reason = `Withdrawal Reason`, begin_date = dmy(`Begin Date`),
                  end_date = dmy(`End Date`))
    ek = read_csv("grad_rate_base_EK.csv")
    jw = read_dta("district_grad_rate2016.dta") %>% 
        bind_rows(read_dta("school_grad_rate2016.dta")) %>% 
        mutate(school = ifelse(is.na(school), 0, school),
               school_name = ifelse(school == 0, "All Schools", school_name)) %>% 
        arrange(system, school) %>% 
        select(year, starts_with("system"), starts_with("school"), grade, subject, 
               subgroup, starts_with("grad_"), dropout_count = drop_count, dropout_rate = drop_rate)
    sl_jw = read_dta("grad_student_level2016.dta")
    
    # Check 2016 school- and district-level files
    a = ek %>% 
        filter(system != 0) %>% 
        mutate(subgroup = ifelse(subgroup == "English Learners", "English Language Learners with T1/T2", subgroup),
               subgroup = ifelse(subgroup == "Non-English Learners", "Non-English Language Learners with T1/T2", subgroup)) %>% 
        full_join(jw, by = c("system", "school", "subgroup")) %>% 
        arrange(system, school) %>% 
        filter(dropout_count.x != dropout_count.y) %>% 
        select(year, system, school, subgroup, starts_with("subject"), starts_with("grad_c"),
               starts_with("dropout_c"))
    
    # Check student-level files
    a = full_join(grad, sl_jw, by = "studentkey") %>% 
        filter(dropout_count != dropout_student | 
                   (is.na(dropout_count) & !is.na(dropout_student == 1)) | 
                   (dropout_count == T & is.na(dropout_student))) %>% 
        select(studentkey, dropout_count, dropout_student) %>% 
        arrange(studentkey)
    
    sk_list = unique(a$studentkey)
    
    b = enr %>% filter(student_key %in% sk_list) %>% 
        arrange(student_key, desc(begin_date), desc(end_date)) 
    
    # # Check 2016 files
    # ek = read_csv("K:/ORP_accountability/data/2016_graduation_rate/grad_rate_base_EK.csv") %>% 
    #     filter(system != 0) %>% 
    #     mutate(subgroup = ifelse(subgroup == "English Learners", "English Language Learners with T1/T2", subgroup),
    #            subgroup = ifelse(subgroup == "Non-English Learners", "Non-English Language Learners with T1/T2", subgroup))
    # jw = read_dta("K:/ORP_accountability/data/2016_graduation_rate/District_grad_rate2017_JP.dta") %>% 
    #     bind_rows(read_dta("K:/ORP_accountability/data/2016_graduation_rate/School_grad_rate2017_JP.dta")) %>% 
    #     mutate(school = ifelse(is.na(school), 0, school),
    #            school_name = ifelse(school == 0, "All Schools", school_name)) %>% 
    #     arrange(system, school) %>% 
    #     select(year, starts_with("system"), starts_with("school"), grade, subject, 
    #            subgroup, starts_with("grad_"), dropout_count = drop_count, dropout_rate)
    # 
    # full_join(ek, jw, by = c("system", "school", "subgroup")) %>% 
    #     arrange(system, school) %>% 
    #     filter(dropout_count.x != dropout_count.y) %>% 
    #     select(year, system, school, subgroup, starts_with("subject"), starts_with("grad_c"),
    #            starts_with("dropout_c")) %>% 
    #     View()
}
