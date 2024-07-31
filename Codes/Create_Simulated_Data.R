# Load in the necessary libraries ---------------------------------------------------------------------------------
library(ips.tools)
library(readr)
library(fastDummies)
library(zoo)
library(janitor)
library(survival)
library(ggplot2)
library(survminer)
library(openxlsx)
library(lubridate)
library(tidyverse)
library(openxlsx)

# Data Preparation ------------------------------------------------------------------------------------------------
# Set directory to the current folder
getwd()
setwd(getwd())


####################################################################
dat.full.clean <- read_csv("dat.full.clean.csv")
dat.full.clean <- dat.full.clean %>% filter(Window != "AB2021AB_WIN")

Updated_simulated_item_stats <- openxlsx::read.xlsx("AB_Updated_simulated_item_stats.xlsx", check.names = F)
names(Updated_simulated_item_stats) <- sub(".", " ", names(Updated_simulated_item_stats), fixed = TRUE)

Updated_simulated_item_stats <- Updated_simulated_item_stats %>%
  mutate(Year = as.numeric(str_extract(Window, "[0-9]+"))) %>%
  mutate(Scored = 1)


dat.full.clean2 <- bind_rows(dat.full.clean, Updated_simulated_item_stats) %>% distinct(Item, Window, .keep_all = T) %>%
  mutate(Current_status = if_else(is.na(Current_status),0,Current_status)) 

# Make sure pretest.active is pretest only
# Separate data into different year cohort
pretest_2016_list <- dat.full.clean2 %>% filter(Window=="AB2016AB_WIN" & `How Used`=="Pretest")
pretest_2017_list <- dat.full.clean2 %>% filter(Window=="AB2017AB_WIN" & `How Used`=="Pretest")
pretest_2019_list <- dat.full.clean2 %>% filter(Window=="AB2019AB_WIN" & `How Used`=="Pretest")
pretest_2020_list <- dat.full.clean2 %>% filter(Window=="AB2020AB_WIN" & `How Used`=="Pretest")
pretest_2021_list <- dat.full.clean2 %>% filter(Window=="AB2021AB_WIN" & `How Used`=="Pretest")
pretest_2022_list <- dat.full.clean2 %>% filter(Window=="AB2022AB_WIN" & `How Used`=="Pretest")
pretest_2023_list <- dat.full.clean2 %>% filter(Window=="AB2023AB_WIN" & `How Used`=="Pretest")
pretest_2024_list <- dat.full.clean2 %>% filter(Window=="AB2024AB_WIN" & `How Used`=="Pretest")
pretest_2025_list <- dat.full.clean2 %>% filter(Window=="AB2025AB_WIN" & `How Used`=="Pretest")


dat.full.2016 <- dat.full.clean2 %>% filter(Item %in% pretest_2016_list$Item) 
dat.full.2017 <- dat.full.clean2 %>% filter(Item %in% pretest_2017_list$Item) 
dat.full.2019 <- dat.full.clean2 %>% filter(Item %in% pretest_2019_list$Item) 
dat.full.2020 <- dat.full.clean2 %>% filter(Item %in% pretest_2020_list$Item)
dat.full.2021 <- dat.full.clean2 %>% filter(Item %in% pretest_2021_list$Item) 
dat.full.2022 <- dat.full.clean2 %>% filter(Item %in% pretest_2022_list$Item) 
dat.full.2023 <- dat.full.clean2 %>% filter(Item %in% pretest_2023_list$Item) 
dat.full.2024 <- dat.full.clean2 %>% filter(Item %in% pretest_2024_list$Item) 
dat.full.2025 <- dat.full.clean2 %>% filter(Item %in% pretest_2025_list$Item) 


# Data Aggregation ------------------------------------------------------------------------------------------------

# 2016 pretest cohort ---------------------------------------------------------------------------------------------
# Create a usage_rate dataset
# We want to know how many years an item is available: if an item is retired/rejected before 2020, we take their max year; otherwise, 2020
Usage_rate_df <- dat.full.2016 %>%
  dplyr::group_by(Item) %>% 
  dplyr::summarise(n_form = n(), 
                   n_candidate = sum(total_candidate),
                   min_year = min(Year),
                   max_year = if_else(Current_status == 0, 2025, max(Year)),
                   n_year = if_else(max_year-min_year==0,1,max_year-min_year+1),
                   LastStatusChangeYear = max(LastStatusChangeYear),
                   usage_rate = n_form/n_year,
                   Scored = sum(Scored), 
                   total_flag_all = sum(Total_Flag),
                   total_flag_distractor = sum(Flag_Distractor),
                   total_falg_differentiate = sum(Flag_Differentiate),
                   total_flag_difficult = sum(Flag_Difficult),
                   total_flag_easy = sum(Flag_Easy),
                   total_flag_miskeyed = sum(Flag_Miskeyed),
                   total_flag_lowperformers = sum(Flag_LowPerformers),
                   total_flag_changestats = sum(Flag_ChangeStats)) %>%
  mutate(usage_rate = if_else(usage_rate > 1, 1, usage_rate)) %>%
  distinct(Item, .keep_all = T) %>%
  ungroup()

Usage_time_df <- dat.full.2016 %>% 
  select(Item, Year, `How Used`,`Item Status`) %>%
  mutate(Status_Year_Pretest_2016 = if_else(Year==2016 & `How Used`=="Pretest",1,0),
         Status_Year_Operational_2017 = if_else(Year==2017 & `How Used`=="Operational",1,0),
         Status_Year_Operational_2019 = if_else(Year==2019 & `How Used`=="Operational",1,0),
         Status_Year_Operational_2020 = if_else(Year==2020 & `How Used`=="Operational",1,0),
         Status_Year_Operational_2021 = if_else(Year==2021 & `How Used`=="Operational",1,0),
         Status_Year_Operational_2022 = if_else(Year==2022 & `How Used`=="Operational",1,0),
         Status_Year_Operational_2023 = if_else(Year==2023 & `How Used`=="Operational",1,0),
         Status_Year_Operational_2024 = if_else(Year==2024 & `How Used`=="Operational",1,0),
         Status_Year_Operational_2025 = if_else(Year==2025 & `How Used`=="Operational",1,0)) %>%
  group_by(Item) %>%
  summarise(Status_Year_Pretest_2016 = sum(Status_Year_Pretest_2016, na.rm = T),
            Status_Year_Operational_2017 = sum(Status_Year_Operational_2017, na.rm = T),
            Status_Year_Operational_2019 = sum(Status_Year_Operational_2019, na.rm = T),
            Status_Year_Operational_2020 = sum(Status_Year_Operational_2020, na.rm = T),
            Status_Year_Operational_2021 = sum(Status_Year_Operational_2021, na.rm = T),
            Status_Year_Operational_2022 = sum(Status_Year_Operational_2022, na.rm = T),
            Status_Year_Operational_2023 = sum(Status_Year_Operational_2023, na.rm = T),
            Status_Year_Operational_2024 = sum(Status_Year_Operational_2024, na.rm = T),
            Status_Year_Operational_2025 = sum(Status_Year_Operational_2025, na.rm = T),
            Current_status = unique(`Item Status`))  %>% distinct(Item, .keep_all = T)


# Join the usage rate and the usage time together.
# Convert the status_year number to binary
Usage_rate_full_df <- Usage_rate_df %>% 
  left_join(Usage_time_df, by="Item")


# 2 forms in one year. How many of those years the item are on multiple forms.
Usage_rate_full_df <- Usage_rate_full_df %>%
  group_by(Item) %>%
  mutate(multiple_form_1_year = if_else(any(Status_Year_Pretest_2016>=2,
                                            Status_Year_Operational_2017>=2,
                                            Status_Year_Operational_2019>=2,
                                            Status_Year_Operational_2020>=2,
                                            Status_Year_Operational_2021>=2,
                                            Status_Year_Operational_2022>=2,
                                            Status_Year_Operational_2023>=2,
                                            Status_Year_Operational_2024>=2,
                                            Status_Year_Operational_2025>=2), 1, 0))

# Deduplicate
Usage_rate_full_df <- unique(Usage_rate_full_df)

# Merge in the flags and other stats of the last rows of each item
dat.last.row <- dat.full.clean2 %>%
  group_by(Item) %>%
  arrange(Form) %>%
  slice(n()) %>%
  select(Item, Item_Type_HotSpot, Item_Type_SSMC,Form_Build, PValue, PBis, Corrected_PBis, IRT_b, Mean_Time_Seconds, 
         Has_Image, Has_Video, Has_Exhibit, Domain, Current_status)

Usage_rate_full_df <-Usage_rate_full_df %>%
  left_join(dat.last.row, by = "Item")

# Calculate pretest only
Usage_rate_full_df <-Usage_rate_full_df %>%
  rename(Current_status = Current_status.x, Current_status_recode = Current_status.y) %>%
  dummy_cols(select_columns = c('Domain', 'Current_status'))  # Create dummy variables for domain

# Calculate the time gap between the last form year and last status change year
# If the status change is resting/retired/rework within 2 years since the last operational, update the LastStatusChangeYear to the same as the max year
Usage_rate_full_df <-Usage_rate_full_df %>%
  mutate(gap_year = LastStatusChangeYear - max_year) %>%
  mutate(LastStatusChangeYear = if_else(gap_year <=2, max_year, LastStatusChangeYear))

# Convert the status by year to binary
Usage_rate_full_df2 <- Usage_rate_full_df %>%
  mutate(Status_Year_Pretest_2016 = if_else(Status_Year_Pretest_2016>=1, 1, 0),
         Status_Year_Operational_2017 = if_else(Status_Year_Operational_2017>=1, 1, 0),
         Status_Year_Operational_2019 = if_else(Status_Year_Operational_2019>=1, 1, 0),
         Status_Year_Operational_2020 = if_else(Status_Year_Operational_2020>=1, 1, 0),
         Status_Year_Operational_2021 = if_else(Status_Year_Operational_2021>=1, 1, 0),
         Status_Year_Operational_2022 = if_else(Status_Year_Operational_2022>=1, 1, 0),
         Status_Year_Operational_2023 = if_else(Status_Year_Operational_2023>=1, 1, 0),
         Status_Year_Operational_2024 = if_else(Status_Year_Operational_2024>=1, 1, 0),
         Status_Year_Operational_2025 = if_else(Status_Year_Operational_2025>=1, 1, 0))%>%
  mutate(total_operational_time = Status_Year_Operational_2017+
           Status_Year_Operational_2019+Status_Year_Operational_2020+Status_Year_Operational_2021+
           Status_Year_Operational_2022+Status_Year_Operational_2023+Status_Year_Operational_2024+Status_Year_Operational_2025) %>%
  mutate(total_flag_all =  if_else(total_flag_all>=1, 1, 0),
         total_flag_distractor =  if_else(total_flag_distractor>=1, 1, 0),
         total_falg_differentiate =  if_else(total_falg_differentiate>=1, 1, 0),
         total_flag_difficult =  if_else(total_flag_difficult>=1, 1, 0),
         total_flag_easy =  if_else(total_flag_easy>=1, 1, 0),
         total_flag_miskeyed =  if_else(total_flag_miskeyed>=1, 1, 0),
         total_flag_lowperformers =  if_else(total_flag_lowperformers>=1, 1, 0),
         total_flag_changestats =  if_else(total_flag_changestats>=1, 1, 0)) %>%
  mutate(pretest_only = if_else(total_operational_time == 0,1,0)) %>%
  mutate(pretest_only_retired = if_else(total_operational_time == 0 & Current_status_recode==1,1,0)) 

Usage_rate_full_df2 <- Usage_rate_full_df2 %>%
  mutate(Status_Year_Retired_2016 = if_else(max_year==2016 & Current_status_Operational==0,1,0),
         Status_Year_Retired_2017 = if_else(max_year==2017 & Current_status_Operational==0,1,0),
         Status_Year_Retired_2019 = if_else(max_year==2019 & Current_status_Operational==0,1,0),
         Status_Year_Retired_2020 = if_else(max_year==2020 & Current_status_Operational==0,1,0),
         Status_Year_Retired_2021 = if_else((max_year==2021 | max_year==2022) & Current_status_Operational==0,1,0))

# Summary
pretest.2016_summary <- Usage_rate_full_df2 %>%
  mutate(total_operational_time = if_else(total_operational_time > 4, 4, total_operational_time)) %>%
  group_by(total_operational_time) %>%
  summarise(total_items = n(),
            total_n_form = mean(n_form),
            total_n_candidate = mean(n_candidate),
            min_year = min(min_year),
            max_year = max(max_year),
            avg_n_year = mean(n_year),
            avg_usage_rate = mean(usage_rate),
            total_pretest_2016 = sum(Status_Year_Pretest_2016),
            total_operationalUsed_2017 = sum(Status_Year_Operational_2017),
            total_operationalUsed_2019 = sum(Status_Year_Operational_2019),
            total_operationalUsed_2020 = sum(Status_Year_Operational_2020),
            total_operationalUsed_2021 = sum(Status_Year_Operational_2021),
            total_operationalUsed_2022 = sum(Status_Year_Operational_2022),
            total_operationalUsed_2023 = sum(Status_Year_Operational_2023),
            total_operationalUsed_2024 = sum(Status_Year_Operational_2024),
            total_operationalUsed_2025 = sum(Status_Year_Operational_2025),
            total_retired_2016 = sum(Status_Year_Retired_2016),
            total_retired_2017 = sum(Status_Year_Retired_2017),
            total_retired_2019 = sum(Status_Year_Retired_2019),
            total_retired_2020 = sum(Status_Year_Retired_2020),
            total_retired_2021 = sum(Status_Year_Retired_2021),
            total_current_operational = sum(Current_status_Operational),
            total_current_lost = sum(Current_status_Retired) + sum(Current_status_Rework)+sum(Current_status_Resting),
            total_current_resting = sum(Current_status_Resting),
            total_current_retired = sum(Current_status_Retired),
            total__current_rework = sum(Current_status_Rework),
            total_multiple_form = sum(multiple_form_1_year),
            total_HotSpot = sum(Item_Type_HotSpot),
            total_SSMC = sum(Item_Type_SSMC),
            total_form_build = sum(Form_Build),
            avg_PValue = mean(PValue, na.rm = T),
            avg_PBis = mean(PBis, na.rm = T),
            avg_Corrected_PBis = mean(Corrected_PBis, na.rm = T),
            avg_IRT_b = mean(IRT_b, na.rm = T),
            avg_mean_time = mean(Mean_Time_Seconds, na.rm = T),
            total_image = sum(Has_Image),
            total_video = sum(Has_Video),
            total_exhibit = sum(Has_Exhibit),
            total_flag_all = sum(total_flag_all),
            total_flag_distractor = sum(total_flag_distractor),
            total_falg_differentiate = sum(total_falg_differentiate),
            total_flag_difficult = sum(total_flag_difficult),
            total_flag_easy = sum(total_flag_easy),
            total_flag_miskeyed = sum(total_flag_miskeyed),
            total_flag_lowperformers = sum(total_flag_lowperformers),
            total_flag_changestats = sum(total_flag_changestats),
            total_domain_0 = sum(Domain_0, na.rm = T),
            total_domain_1 = sum(Domain_1, na.rm = T),
            total_domain_2 = sum(Domain_2, na.rm = T),
            total_domain_3 = sum(Domain_3, na.rm = T),
            total_domain_4 = sum(Domain_4, na.rm = T),
            total_pretest_only_retired = sum(pretest_only_retired))

pretest.2016_summary$cohort <- "pretest_2016"
pretest.2016_cohort <- Usage_rate_full_df2

# 2017 pretest cohort ---------------------------------------------------------------------------------------------
# Create a usage_rate dataset
# We want to know how many years an item is available: if an item is retired/rejected before 2020, we take their max year; otherwise, 2020
Usage_rate_df <- dat.full.2017 %>%
  group_by(Item) %>% 
  summarise(n_form = n(), 
            n_candidate = sum(total_candidate),
            min_year = min(Year),
            max_year = if_else(Current_status == 0, 2025, max(Year)),
            n_year = if_else(max_year-min_year==0,1,max_year-min_year+1),
            LastStatusChangeYear = max(LastStatusChangeYear),
            usage_rate = n_form/n_year,
            Scored = sum(Scored), 
            total_flag_all = sum(Total_Flag),
            total_flag_distractor = sum(Flag_Distractor),
            total_falg_differentiate = sum(Flag_Differentiate),
            total_flag_difficult = sum(Flag_Difficult),
            total_flag_easy = sum(Flag_Easy),
            total_flag_miskeyed = sum(Flag_Miskeyed),
            total_flag_lowperformers = sum(Flag_LowPerformers),
            total_flag_changestats = sum(Flag_ChangeStats)) %>%
  mutate(usage_rate = if_else(usage_rate > 1, 1, usage_rate)) %>%
  distinct() %>% ungroup()

Usage_time_df <- dat.full.2017 %>% 
  select(Item, Year, `How Used`,`Item Status`) %>%
  mutate(Status_Year_Pretest_2017 = if_else(Year==2017 & `How Used`=="Pretest",1,0),
         Status_Year_Operational_2019 = if_else(Year==2019 & `How Used`=="Operational",1,0),
         Status_Year_Operational_2020 = if_else(Year==2020 & `How Used`=="Operational",1,0),
         Status_Year_Operational_2021 = if_else(Year==2021 & `How Used`=="Operational",1,0),
         Status_Year_Operational_2022 = if_else(Year==2022 & `How Used`=="Operational",1,0),
         Status_Year_Operational_2023 = if_else(Year==2023 & `How Used`=="Operational",1,0),
         Status_Year_Operational_2024 = if_else(Year==2024 & `How Used`=="Operational",1,0),
         Status_Year_Operational_2025 = if_else(Year==2025 & `How Used`=="Operational",1,0)) %>%
  group_by(Item) %>%
  summarise(Status_Year_Pretest_2017 = sum(Status_Year_Pretest_2017, na.rm = T),
            Status_Year_Operational_2019 = sum(Status_Year_Operational_2019, na.rm = T),
            Status_Year_Operational_2020 = sum(Status_Year_Operational_2020, na.rm = T),
            Status_Year_Operational_2021 = sum(Status_Year_Operational_2021, na.rm = T),
            Status_Year_Operational_2022 = sum(Status_Year_Operational_2022, na.rm = T),
            Status_Year_Operational_2023 = sum(Status_Year_Operational_2023, na.rm = T),
            Status_Year_Operational_2024 = sum(Status_Year_Operational_2024, na.rm = T),
            Status_Year_Operational_2025 = sum(Status_Year_Operational_2025, na.rm = T),
            Current_status = unique(`Item Status`)) 

# Join the usage rate and the usage time together.
# Convert the status_year number to binary
Usage_rate_full_df <- Usage_rate_df %>% 
  left_join(Usage_time_df, by="Item")


# 2 forms in one year. How many of those years the item are on multiple forms.
Usage_rate_full_df <- Usage_rate_full_df %>%
  group_by(Item) %>%
  mutate(multiple_form_1_year = if_else(any(Status_Year_Pretest_2017>=2,
                                            Status_Year_Operational_2019>=2,
                                            Status_Year_Operational_2020>=2,
                                            Status_Year_Operational_2021>=2,
                                            Status_Year_Operational_2022>=2,
                                            Status_Year_Operational_2023>=2,
                                            Status_Year_Operational_2024>=2,
                                            Status_Year_Operational_2025>=2), 1, 0))

# Deduplicate
Usage_rate_full_df <- unique(Usage_rate_full_df)

# Merge in the flags and other stats of the last rows of each item
dat.last.row <- dat.full.clean2 %>%
  group_by(Item) %>%
  arrange(Form) %>%
  slice(n()) %>%
  select(Item, Item_Type_HotSpot, Item_Type_SSMC,Form_Build, PValue, PBis, Corrected_PBis, IRT_b, Mean_Time_Seconds, 
         Has_Image, Has_Video, Has_Exhibit, Domain, Current_status)

Usage_rate_full_df <-Usage_rate_full_df %>%
  left_join(dat.last.row, by = "Item")

# Calculate pretest only
Usage_rate_full_df <-Usage_rate_full_df %>%
  rename(Current_status = Current_status.x, Current_status_recode = Current_status.y) %>%
  dummy_cols(select_columns = c('Domain', 'Current_status'))  # Create dummy variables for domain

# Calculate the time gap between the last form year and last status change year
# If the status change is resting/retired/rework within 2 years since the last operational, update the LastStatusChangeYear to the same as the max year
Usage_rate_full_df <-Usage_rate_full_df %>%
  mutate(gap_year = LastStatusChangeYear - max_year) %>%
  mutate(LastStatusChangeYear = if_else(gap_year <=2, max_year, LastStatusChangeYear))

# Convert the status by year to binary
Usage_rate_full_df2 <- Usage_rate_full_df %>%
  mutate(Status_Year_Pretest_2017 = if_else(Status_Year_Pretest_2017>=1, 1, 0),
         Status_Year_Operational_2019 = if_else(Status_Year_Operational_2019>=1, 1, 0),
         Status_Year_Operational_2020 = if_else(Status_Year_Operational_2020>=1, 1, 0),
         Status_Year_Operational_2021 = if_else(Status_Year_Operational_2021>=1, 1, 0),
         Status_Year_Operational_2022 = if_else(Status_Year_Operational_2022>=1, 1, 0),
         Status_Year_Operational_2023 = if_else(Status_Year_Operational_2023>=1, 1, 0),
         Status_Year_Operational_2024 = if_else(Status_Year_Operational_2024>=1, 1, 0),
         Status_Year_Operational_2025 = if_else(Status_Year_Operational_2025>=1, 1, 0)) %>%
  mutate(total_operational_time = 
           Status_Year_Operational_2019+Status_Year_Operational_2020+Status_Year_Operational_2021+Status_Year_Operational_2022+
           Status_Year_Operational_2023+Status_Year_Operational_2024+Status_Year_Operational_2025) %>%
  mutate(total_flag_all =  if_else(total_flag_all>=1, 1, 0),
         total_flag_distractor =  if_else(total_flag_distractor>=1, 1, 0),
         total_falg_differentiate =  if_else(total_falg_differentiate>=1, 1, 0),
         total_flag_difficult =  if_else(total_flag_difficult>=1, 1, 0),
         total_flag_easy =  if_else(total_flag_easy>=1, 1, 0),
         total_flag_miskeyed =  if_else(total_flag_miskeyed>=1, 1, 0),
         total_flag_lowperformers =  if_else(total_flag_lowperformers>=1, 1, 0),
         total_flag_changestats =  if_else(total_flag_changestats>=1, 1, 0)) %>%
  mutate(pretest_only = if_else(total_operational_time == 0,1,0)) %>%
  mutate(pretest_only_retired = if_else(total_operational_time == 0 & Current_status_recode==1,1,0)) 

Usage_rate_full_df2 <- Usage_rate_full_df2 %>%
  mutate(Status_Year_Retired_2016 = if_else(max_year==2016 & Current_status_Operational==0,1,0),
         Status_Year_Retired_2017 = if_else(max_year==2017 & Current_status_Operational==0,1,0),
         Status_Year_Retired_2019 = if_else(max_year==2019 & Current_status_Operational==0,1,0),
         Status_Year_Retired_2020 = if_else(max_year==2020 & Current_status_Operational==0,1,0),
         Status_Year_Retired_2021 = if_else((max_year==2021 | max_year==2022) & Current_status_Operational==0,1,0))


# Summary
pretest.2017_summary <- Usage_rate_full_df2 %>%
  mutate(total_operational_time = if_else(total_operational_time > 4, 4, total_operational_time)) %>%
  group_by(total_operational_time) %>%
  summarise(total_items = n(),
            total_n_form = mean(n_form),
            total_n_candidate = mean(n_candidate),
            min_year = min(min_year),
            max_year = max(max_year),
            avg_n_year = mean(n_year),
            avg_usage_rate = mean(usage_rate),
            total_pretest_2017 = sum(Status_Year_Pretest_2017),
            total_operationalUsed_2019 = sum(Status_Year_Operational_2019),
            total_operationalUsed_2020 = sum(Status_Year_Operational_2020),
            total_operationalUsed_2021 = sum(Status_Year_Operational_2021),
            total_operationalUsed_2022 = sum(Status_Year_Operational_2022),
            total_operationalUsed_2023 = sum(Status_Year_Operational_2023),
            total_operationalUsed_2024 = sum(Status_Year_Operational_2024),
            total_operationalUsed_2025 = sum(Status_Year_Operational_2025),
            total_retired_2016 = sum(Status_Year_Retired_2016),
            total_retired_2017 = sum(Status_Year_Retired_2017),
            total_retired_2019 = sum(Status_Year_Retired_2019),
            total_retired_2020 = sum(Status_Year_Retired_2020),
            total_retired_2021 = sum(Status_Year_Retired_2021),
            total_current_operational = sum(Current_status_Operational),
            total_current_lost = sum(Current_status_Retired) + sum(Current_status_Resting),
            total_current_resting = sum(Current_status_Resting),
            total_current_retired = sum(Current_status_Retired),
            total_multiple_form = sum(multiple_form_1_year),
            total_HotSpot = sum(Item_Type_HotSpot),
            total_SSMC = sum(Item_Type_SSMC),
            total_form_build = sum(Form_Build),
            avg_PValue = mean(PValue, na.rm = T),
            avg_PBis = mean(PBis, na.rm = T),
            avg_Corrected_PBis = mean(Corrected_PBis, na.rm = T),
            avg_IRT_b = mean(IRT_b, na.rm = T),
            avg_mean_time = mean(Mean_Time_Seconds, na.rm = T),
            total_image = sum(Has_Image),
            total_video = sum(Has_Video),
            total_exhibit = sum(Has_Exhibit),
            total_flag_all = sum(total_flag_all),
            total_flag_distractor = sum(total_flag_distractor),
            total_falg_differentiate = sum(total_falg_differentiate),
            total_flag_difficult = sum(total_flag_difficult),
            total_flag_easy = sum(total_flag_easy),
            total_flag_miskeyed = sum(total_flag_miskeyed),
            total_flag_lowperformers = sum(total_flag_lowperformers),
            total_flag_changestats = sum(total_flag_changestats),
            total_domain_1 = sum(Domain_1),
            total_domain_2 = sum(Domain_2),
            total_domain_3 = sum(Domain_3),
            total_domain_4 = sum(Domain_4),
            total_domain_5 = sum(Domain_5),
            total_domain_6 = sum(Domain_6),
            total_domain_7 = sum(Domain_7),
            total_domain_8 = sum(Domain_8),
            total_pretest_only_retired = sum(pretest_only_retired))

pretest.2017_summary$cohort <- "pretest_2017"
pretest.2017_cohort <- Usage_rate_full_df2
#write.csv(pretest.2017_cohort, "pretest.2017_cohort.csv")

# 2019 pretest cohort ---------------------------------------------------------------------------------------------
# Create a usage_rate dataset
# We want to know how many years an item is available: if an item is retired/rejected before 2020, we take their max year; otherwise, 2020
Usage_rate_df <- dat.full.2019 %>%
  group_by(Item) %>% 
  summarise(n_form = n(), 
            n_candidate = sum(total_candidate),
            min_year = min(Year),
            max_year = if_else(Current_status == 0, 2025, max(Year)),
            n_year = if_else(max_year-min_year==0,1,max_year-min_year+1),
            LastStatusChangeYear = max(LastStatusChangeYear),
            usage_rate = n_form/n_year,
            Scored = sum(Scored), 
            total_flag_all = sum(Total_Flag),
            total_flag_distractor = sum(Flag_Distractor),
            total_falg_differentiate = sum(Flag_Differentiate),
            total_flag_difficult = sum(Flag_Difficult),
            total_flag_easy = sum(Flag_Easy),
            total_flag_miskeyed = sum(Flag_Miskeyed),
            total_flag_lowperformers = sum(Flag_LowPerformers),
            total_flag_changestats = sum(Flag_ChangeStats)) %>%
  mutate(usage_rate = if_else(usage_rate > 1, 1, usage_rate)) %>%
  distinct() %>% ungroup()


Usage_time_df <- dat.full.2019 %>% 
  select(Item, Year, `How Used`,`Item Status`) %>%
  mutate(Status_Year_Pretest_2019 = if_else(Year==2019 & `How Used`=="Pretest",1,0),
         Status_Year_Operational_2020 = if_else(Year==2020 & `How Used`=="Operational",1,0),
         Status_Year_Operational_2021 = if_else(Year==2021 & `How Used`=="Operational",1,0),
         Status_Year_Operational_2022 = if_else(Year==2022 & `How Used`=="Operational",1,0),
         Status_Year_Operational_2023 = if_else(Year==2023 & `How Used`=="Operational",1,0),
         Status_Year_Operational_2024 = if_else(Year==2024 & `How Used`=="Operational",1,0),
         Status_Year_Operational_2025 = if_else(Year==2025 & `How Used`=="Operational",1,0)) %>%
  group_by(Item) %>%
  summarise(Status_Year_Pretest_2019 = sum(Status_Year_Pretest_2019, na.rm = T),
            Status_Year_Operational_2020 = sum(Status_Year_Operational_2020, na.rm = T),
            Status_Year_Operational_2021 = sum(Status_Year_Operational_2021, na.rm = T),
            Status_Year_Operational_2022 = sum(Status_Year_Operational_2022, na.rm = T),
            Status_Year_Operational_2023 = sum(Status_Year_Operational_2023, na.rm = T),
            Status_Year_Operational_2024 = sum(Status_Year_Operational_2024, na.rm = T),
            Status_Year_Operational_2025 = sum(Status_Year_Operational_2025, na.rm = T),
            Current_status = unique(`Item Status`)) 

# Join the usage rate and the usage time together.
# Convert the status_year number to binary
Usage_rate_full_df <- Usage_rate_df %>% 
  left_join(Usage_time_df, by="Item")


# 2 forms in one year. How many of those years the item are on multiple forms.
Usage_rate_full_df <- Usage_rate_full_df %>%
  group_by(Item) %>%
  mutate(multiple_form_1_year = if_else(any(Status_Year_Pretest_2019>=2,
                                            Status_Year_Operational_2020>=2,
                                            Status_Year_Operational_2021>=2,
                                            Status_Year_Operational_2022>=2,
                                            Status_Year_Operational_2023>=2,
                                            Status_Year_Operational_2024>=2,
                                            Status_Year_Operational_2025>=2), 1, 0))

# Deduplicate
Usage_rate_full_df <- unique(Usage_rate_full_df)

# Merge in the flags and other stats of the last rows of each item
dat.last.row <- dat.full.clean2 %>%
  group_by(Item) %>%
  arrange(Form) %>%
  slice(n()) %>%
  select(Item, Item_Type_HotSpot, Item_Type_SSMC,Form_Build, PValue, PBis, Corrected_PBis, IRT_b, Mean_Time_Seconds, 
         Has_Image, Has_Video, Has_Exhibit, Domain, Current_status)

Usage_rate_full_df <-Usage_rate_full_df %>%
  left_join(dat.last.row, by = "Item")

# Calculate pretest only
Usage_rate_full_df <-Usage_rate_full_df %>%
  rename(Current_status = Current_status.x, Current_status_recode = Current_status.y) %>%
  dummy_cols(select_columns = c('Domain', 'Current_status'))  # Create dummy variables for domain

# Calculate the time gap between the last form year and last status change year
# If the status change is resting/retired/rework within 2 years since the last operational, update the LastStatusChangeYear to the same as the max year
Usage_rate_full_df <-Usage_rate_full_df %>%
  mutate(gap_year = LastStatusChangeYear - max_year) %>%
  mutate(LastStatusChangeYear = if_else(gap_year <=2, max_year, LastStatusChangeYear))

# Convert the status by year to binary
Usage_rate_full_df2 <- Usage_rate_full_df %>%
  mutate(Status_Year_Pretest_2019 = if_else(Status_Year_Pretest_2019>=1, 1, 0),
         Status_Year_Operational_2020 = if_else(Status_Year_Operational_2020>=1, 1, 0),
         Status_Year_Operational_2021 = if_else(Status_Year_Operational_2021>=1, 1, 0),
         Status_Year_Operational_2022 = if_else(Status_Year_Operational_2022>=1, 1, 0),
         Status_Year_Operational_2023 = if_else(Status_Year_Operational_2023>=1, 1, 0),
         Status_Year_Operational_2024 = if_else(Status_Year_Operational_2024>=1, 1, 0),
         Status_Year_Operational_2025 = if_else(Status_Year_Operational_2025>=1, 1, 0),)%>%
  mutate(total_operational_time = Status_Year_Operational_2020+Status_Year_Operational_2021+Status_Year_Operational_2022
         +Status_Year_Operational_2023+Status_Year_Operational_2024+Status_Year_Operational_2025) %>%
  mutate(total_flag_all =  if_else(total_flag_all>=1, 1, 0),
         total_flag_distractor =  if_else(total_flag_distractor>=1, 1, 0),
         total_falg_differentiate =  if_else(total_falg_differentiate>=1, 1, 0),
         total_flag_difficult =  if_else(total_flag_difficult>=1, 1, 0),
         total_flag_easy =  if_else(total_flag_easy>=1, 1, 0),
         total_flag_miskeyed =  if_else(total_flag_miskeyed>=1, 1, 0),
         total_flag_lowperformers =  if_else(total_flag_lowperformers>=1, 1, 0),
         total_flag_changestats =  if_else(total_flag_changestats>=1, 1, 0)) %>%
  mutate(pretest_only = if_else(total_operational_time == 0,1,0)) %>%
  mutate(pretest_only_retired = if_else(total_operational_time == 0 & Current_status_recode==1,1,0)) 

Usage_rate_full_df2 <- Usage_rate_full_df2 %>%
  mutate(Status_Year_Retired_2016 = if_else(max_year==2016 & Current_status_Operational==0,1,0),
         Status_Year_Retired_2017 = if_else(max_year==2017 & Current_status_Operational==0,1,0),
         Status_Year_Retired_2019 = if_else(max_year==2019 & Current_status_Operational==0,1,0),
         Status_Year_Retired_2020 = if_else(max_year==2020 & Current_status_Operational==0,1,0),
         Status_Year_Retired_2021 = if_else((max_year==2021 | max_year==2022) & Current_status_Operational==0,1,0))

# Summary
pretest.2019_summary <- Usage_rate_full_df2 %>%
  mutate(total_operational_time = if_else(total_operational_time > 4, 4, total_operational_time)) %>%
  group_by(total_operational_time) %>%
  summarise(total_items = n(),
            total_n_form = mean(n_form),
            total_n_candidate = mean(n_candidate),
            min_year = min(min_year),
            max_year = max(max_year),
            avg_n_year = mean(n_year),
            avg_usage_rate = mean(usage_rate),
            total_pretest_2019 = sum(Status_Year_Pretest_2019),
            total_operationalUsed_2020 = sum(Status_Year_Operational_2020),
            total_operationalUsed_2021 = sum(Status_Year_Operational_2021),
            total_operationalUsed_2022 = sum(Status_Year_Operational_2022),
            total_operationalUsed_2023 = sum(Status_Year_Operational_2023),
            total_operationalUsed_2024 = sum(Status_Year_Operational_2024),
            total_operationalUsed_2025 = sum(Status_Year_Operational_2025),
            total_retired_2016 = sum(Status_Year_Retired_2016),
            total_retired_2017 = sum(Status_Year_Retired_2017),
            total_retired_2019 = sum(Status_Year_Retired_2019),
            total_retired_2020 = sum(Status_Year_Retired_2020),
            total_retired_2021 = sum(Status_Year_Retired_2021),
            total_current_operational = sum(Current_status_Operational),
            total_current_lost = sum(Current_status_Retired) + sum(Current_status_Rework) + sum(Current_status_Resting),
            total_current_resting = sum(Current_status_Resting),
            total_current_retired = sum(Current_status_Retired),
            total__current_rework = sum(Current_status_Rework),
            total_multiple_form = sum(multiple_form_1_year),
            total_HotSpot = sum(Item_Type_HotSpot),
            total_SSMC = sum(Item_Type_SSMC),
            total_form_build = sum(Form_Build),
            avg_PValue = mean(PValue, na.rm = T),
            avg_PBis = mean(PBis, na.rm = T),
            avg_Corrected_PBis = mean(Corrected_PBis, na.rm = T),
            avg_IRT_b = mean(IRT_b, na.rm = T),
            avg_mean_time = mean(Mean_Time_Seconds, na.rm = T),
            total_image = sum(Has_Image),
            total_video = sum(Has_Video),
            total_exhibit = sum(Has_Exhibit),
            total_flag_all = sum(total_flag_all),
            total_flag_distractor = sum(total_flag_distractor),
            total_falg_differentiate = sum(total_falg_differentiate),
            total_flag_difficult = sum(total_flag_difficult),
            total_flag_easy = sum(total_flag_easy),
            total_flag_miskeyed = sum(total_flag_miskeyed),
            total_flag_lowperformers = sum(total_flag_lowperformers),
            total_flag_changestats = sum(total_flag_changestats),
            total_domain_1 = sum(Domain_1),
            total_domain_2 = sum(Domain_2),
            total_domain_3 = sum(Domain_3),
            total_domain_4 = sum(Domain_4),
            total_pretest_only_retired = sum(pretest_only_retired))

pretest.2019_summary$cohort <- "pretest_2019"
pretest.2019_cohort <- Usage_rate_full_df2

# 2020 pretest cohort ---------------------------------------------------------------------------------------------
# Create a usage_rate dataset
# We want to know how many years an item is available: if an item is retired/rejected before 2020, we take their max year; otherwise, 2020
Usage_rate_df <- dat.full.2020 %>%
  group_by(Item) %>% 
  summarise(n_form = n(), 
            n_candidate = sum(total_candidate),
            min_year = min(Year),
            max_year = if_else(Current_status == 0, 2025, max(Year)),
            n_year = if_else(max_year-min_year==0,1,max_year-min_year+1),
            LastStatusChangeYear = max(LastStatusChangeYear),
            usage_rate = n_form/n_year,
            Scored = sum(Scored), 
            total_flag_all = sum(Total_Flag),
            total_flag_distractor = sum(Flag_Distractor),
            total_falg_differentiate = sum(Flag_Differentiate),
            total_flag_difficult = sum(Flag_Difficult),
            total_flag_easy = sum(Flag_Easy),
            total_flag_miskeyed = sum(Flag_Miskeyed),
            total_flag_lowperformers = sum(Flag_LowPerformers),
            total_flag_changestats = sum(Flag_ChangeStats)) %>%
  mutate(usage_rate = if_else(usage_rate > 1, 1, usage_rate)) %>%
  distinct() %>% ungroup()

Usage_rate_df$average_rate <- mean(Usage_rate_df$usage_rate) 

Usage_time_df <- dat.full.2020 %>% 
  select(Item, Year, `How Used`,`Item Status`) %>%
  mutate(Status_Year_Pretest_2020 = if_else(Year==2020 & `How Used`=="Pretest",1,0),
         Status_Year_Operational_2021 = if_else(Year==2021 & `How Used`=="Operational",1,0),
         Status_Year_Operational_2022 = if_else(Year==2022 & `How Used`=="Operational",1,0),
         Status_Year_Operational_2023 = if_else(Year==2023 & `How Used`=="Operational",1,0),
         Status_Year_Operational_2024 = if_else(Year==2024 & `How Used`=="Operational",1,0),
         Status_Year_Operational_2025 = if_else(Year==2025 & `How Used`=="Operational",1,0)) %>%
  group_by(Item) %>%
  summarise(Status_Year_Pretest_2020 = sum(Status_Year_Pretest_2020, na.rm = T),
            Status_Year_Operational_2021 = sum(Status_Year_Operational_2021, na.rm = T),
            Status_Year_Operational_2022 = sum(Status_Year_Operational_2022, na.rm = T),
            Status_Year_Operational_2023 = sum(Status_Year_Operational_2023, na.rm = T),
            Status_Year_Operational_2024 = sum(Status_Year_Operational_2024, na.rm = T),
            Status_Year_Operational_2025 = sum(Status_Year_Operational_2025, na.rm = T),
            Current_status = unique(`Item Status`)) 

# Join the usage rate and the usage time together.
# Convert the status_year number to binary
Usage_rate_full_df <- Usage_rate_df %>% 
  left_join(Usage_time_df, by="Item")


# 2 forms in one year. How many of those years the item are on multiple forms.
Usage_rate_full_df <- Usage_rate_full_df %>%
  group_by(Item) %>%
  mutate(multiple_form_1_year = if_else(any(Status_Year_Pretest_2020>=2,
                                            Status_Year_Operational_2021>=2,
                                            Status_Year_Operational_2022>=2,
                                            Status_Year_Operational_2023>=2,
                                            Status_Year_Operational_2024>=2,
                                            Status_Year_Operational_2025>=2), 1, 0))

# Deduplicate
Usage_rate_full_df <- unique(Usage_rate_full_df)

# Merge in the flags and other stats of the last rows of each item
dat.last.row <- dat.full.clean2 %>%
  group_by(Item) %>%
  arrange(Form) %>%
  slice(n()) %>%
  select(Item, Item_Type_HotSpot, Item_Type_SSMC,Form_Build, PValue, PBis, Corrected_PBis, IRT_b, Mean_Time_Seconds, 
         Has_Image, Has_Video, Has_Exhibit, Domain, Current_status)

Usage_rate_full_df <-Usage_rate_full_df %>%
  left_join(dat.last.row, by = "Item")

# Calculate pretest only
Usage_rate_full_df <-Usage_rate_full_df %>%
  rename(Current_status = Current_status.x, Current_status_recode = Current_status.y) %>%
  dummy_cols(select_columns = c('Domain', 'Current_status'))  # Create dummy variables for domain

# Calculate the time gap between the last form year and last status change year
# If the status change is resting/retired/rework within 2 years since the last operational, update the LastStatusChangeYear to the same as the max year
Usage_rate_full_df <-Usage_rate_full_df %>%
  mutate(gap_year = LastStatusChangeYear - max_year) %>%
  mutate(LastStatusChangeYear = if_else(gap_year <=2, max_year, LastStatusChangeYear))

# Convert the status by year to binary
Usage_rate_full_df2 <- Usage_rate_full_df %>%
  mutate(Status_Year_Pretest_2020 = if_else(Status_Year_Pretest_2020>=1, 1, 0),
         Status_Year_Operational_2021 = if_else(Status_Year_Operational_2021>=1, 1, 0),
         Status_Year_Operational_2022 = if_else(Status_Year_Operational_2022>=1, 1, 0),
         Status_Year_Operational_2023 = if_else(Status_Year_Operational_2023>=1, 1, 0),
         Status_Year_Operational_2024 = if_else(Status_Year_Operational_2024>=1, 1, 0),
         Status_Year_Operational_2025 = if_else(Status_Year_Operational_2025>=1, 1, 0))%>%
  mutate(total_operational_time = Status_Year_Operational_2021+Status_Year_Operational_2022+Status_Year_Operational_2023+
           Status_Year_Operational_2024+Status_Year_Operational_2025) %>%
  mutate(total_flag_all =  if_else(total_flag_all>=1, 1, 0),
         total_flag_distractor =  if_else(total_flag_distractor>=1, 1, 0),
         total_falg_differentiate =  if_else(total_falg_differentiate>=1, 1, 0),
         total_flag_difficult =  if_else(total_flag_difficult>=1, 1, 0),
         total_flag_easy =  if_else(total_flag_easy>=1, 1, 0),
         total_flag_miskeyed =  if_else(total_flag_miskeyed>=1, 1, 0),
         total_flag_lowperformers =  if_else(total_flag_lowperformers>=1, 1, 0),
         total_flag_changestats =  if_else(total_flag_changestats>=1, 1, 0)) %>%
  mutate(pretest_only = if_else(total_operational_time == 0,1,0)) %>%
  mutate(pretest_only_retired = if_else(total_operational_time == 0 & Current_status_recode==1,1,0)) 

Usage_rate_full_df2 <- Usage_rate_full_df2 %>%
  mutate(Status_Year_Retired_2016 = if_else(max_year==2016 & Current_status_Operational==0,1,0),
         Status_Year_Retired_2017 = if_else(max_year==2017 & Current_status_Operational==0,1,0),
         Status_Year_Retired_2019 = if_else(max_year==2019 & Current_status_Operational==0,1,0),
         Status_Year_Retired_2020 = if_else(max_year==2020 & Current_status_Operational==0,1,0),
         Status_Year_Retired_2021 = if_else((max_year==2021 | max_year==2022) & Current_status_Operational==0,1,0))

# Summary
pretest.2020_summary <- Usage_rate_full_df2 %>%
  group_by(total_operational_time) %>%
  summarise(total_items = n(),
            total_n_form = mean(n_form),
            total_n_candidate = mean(n_candidate),
            min_year = min(min_year),
            max_year = max(max_year),
            avg_n_year = mean(n_year),
            avg_usage_rate = mean(usage_rate),
            total_pretest_2020 = sum(Status_Year_Pretest_2020),
            total_operationalUsed_2021 = sum(Status_Year_Operational_2021),
            total_operationalUsed_2022 = sum(Status_Year_Operational_2022),
            total_operationalUsed_2023 = sum(Status_Year_Operational_2023),
            total_operationalUsed_2024 = sum(Status_Year_Operational_2024),
            total_operationalUsed_2025 = sum(Status_Year_Operational_2025),
            total_retired_2016 = sum(Status_Year_Retired_2016),
            total_retired_2017 = sum(Status_Year_Retired_2017),
            total_retired_2019 = sum(Status_Year_Retired_2019),
            total_retired_2020 = sum(Status_Year_Retired_2020),
            total_retired_2021 = sum(Status_Year_Retired_2021),
            total_current_operational = sum(Current_status_Operational),
            total_current_lost = sum(Current_status_Retired) + sum(Current_status_Rework),
            total_current_retired = sum(Current_status_Retired),
            total__current_rework = sum(Current_status_Rework),
            total_multiple_form = sum(multiple_form_1_year),
            total_HotSpot = sum(Item_Type_HotSpot),
            total_SSMC = sum(Item_Type_SSMC),
            total_form_build = sum(Form_Build),
            avg_PValue = mean(PValue, na.rm = T),
            avg_PBis = mean(PBis, na.rm = T),
            avg_Corrected_PBis = mean(Corrected_PBis, na.rm = T),
            avg_IRT_b = mean(IRT_b, na.rm = T),
            avg_mean_time = mean(Mean_Time_Seconds, na.rm = T),
            total_image = sum(Has_Image),
            total_video = sum(Has_Video),
            total_exhibit = sum(Has_Exhibit),
            total_flag_all = sum(total_flag_all),
            total_flag_distractor = sum(total_flag_distractor),
            total_falg_differentiate = sum(total_falg_differentiate),
            total_flag_difficult = sum(total_flag_difficult),
            total_flag_easy = sum(total_flag_easy),
            total_flag_miskeyed = sum(total_flag_miskeyed),
            total_flag_lowperformers = sum(total_flag_lowperformers),
            total_flag_changestats = sum(total_flag_changestats),
            total_domain_1 = sum(Domain_1),
            total_domain_2 = sum(Domain_2),
            total_domain_3 = sum(Domain_3),
            total_domain_4 = sum(Domain_4),
            total_pretest_only_retired = sum(pretest_only_retired))

pretest.2020_summary$cohort <- "pretest_2020"
pretest.2020_cohort <- Usage_rate_full_df2


# 2021 pretest cohort ---------------------------------------------------------------------------------------------
# Create a usage_rate dataset
# We want to know how many years an item is available: if an item is retired/rejected before 2020, we take their max year; otherwise, 2020
Usage_rate_df <- dat.full.2021 %>%
  group_by(Item) %>% 
  summarise(n_form = n(), 
            n_candidate = sum(total_candidate),
            min_year = min(Year),
            max_year = if_else(Current_status == 0, 2025, max(Year)),
            n_year = if_else(max_year-min_year==0,1,max_year-min_year+1),
            LastStatusChangeYear = max(LastStatusChangeYear),
            usage_rate = n_form/n_year,
            Scored = sum(Scored), 
            total_flag_all = sum(Total_Flag),
            total_flag_distractor = sum(Flag_Distractor),
            total_falg_differentiate = sum(Flag_Differentiate),
            total_flag_difficult = sum(Flag_Difficult),
            total_flag_easy = sum(Flag_Easy),
            total_flag_miskeyed = sum(Flag_Miskeyed),
            total_flag_lowperformers = sum(Flag_LowPerformers),
            total_flag_changestats = sum(Flag_ChangeStats)) %>%
  mutate(usage_rate = if_else(usage_rate > 1, 1, usage_rate)) %>%
  distinct() %>% ungroup()

Usage_rate_df$average_rate <- mean(Usage_rate_df$usage_rate) 

Usage_time_df <- dat.full.2021 %>% 
  select(Item, Year, `How Used`,`Item Status`) %>%
  mutate(Status_Year_Pretest_2021 = if_else(Year==2021 & `How Used`=="Pretest",1,0),
         Status_Year_Operational_2022 = if_else(Year==2022 & `How Used`=="Operational",1,0),
         Status_Year_Operational_2023 = if_else(Year==2023 & `How Used`=="Operational",1,0),
         Status_Year_Operational_2024 = if_else(Year==2024 & `How Used`=="Operational",1,0),
         Status_Year_Operational_2025 = if_else(Year==2025 & `How Used`=="Operational",1,0)) %>%
  group_by(Item) %>%
  summarise(Status_Year_Pretest_2021 = sum(Status_Year_Pretest_2021, na.rm = T),
            Status_Year_Operational_2022 = sum(Status_Year_Operational_2022, na.rm = T),
            Status_Year_Operational_2023 = sum(Status_Year_Operational_2023, na.rm = T),
            Status_Year_Operational_2024 = sum(Status_Year_Operational_2024, na.rm = T),
            Status_Year_Operational_2025 = sum(Status_Year_Operational_2025, na.rm = T),
            Current_status = unique(`Item Status`)) 

# Join the usage rate and the usage time together.
# Convert the status_year number to binary
Usage_rate_full_df <- Usage_rate_df %>% 
  left_join(Usage_time_df, by="Item")


# 2 forms in one year. How many of those years the item are on multiple forms.
Usage_rate_full_df <- Usage_rate_full_df %>%
  group_by(Item) %>%
  mutate(multiple_form_1_year = if_else(any(Status_Year_Pretest_2021>=2,
                                            Status_Year_Operational_2022>=2,
                                            Status_Year_Operational_2023>=2,
                                            Status_Year_Operational_2024>=2,
                                            Status_Year_Operational_2025>=2), 1, 0))

# Deduplicate
Usage_rate_full_df <- unique(Usage_rate_full_df)

# Merge in the flags and other stats of the last rows of each item
dat.last.row <- dat.full.clean2 %>%
  group_by(Item) %>%
  arrange(Form) %>%
  slice(n()) %>%
  select(Item, Item_Type_HotSpot, Item_Type_SSMC,Form_Build, PValue, PBis, Corrected_PBis, IRT_b, Mean_Time_Seconds, 
         Has_Image, Has_Video, Has_Exhibit, Domain, Current_status)

Usage_rate_full_df <-Usage_rate_full_df %>%
  left_join(dat.last.row, by = "Item")

# Calculate pretest only
Usage_rate_full_df <-Usage_rate_full_df %>%
  rename(Current_status = Current_status.x, Current_status_recode = Current_status.y) %>%
  dummy_cols(select_columns = c('Domain', 'Current_status'))  # Create dummy variables for domain

# Calculate the time gap between the last form year and last status change year
# If the status change is resting/retired/rework within 2 years since the last operational, update the LastStatusChangeYear to the same as the max year
Usage_rate_full_df <-Usage_rate_full_df %>%
  mutate(gap_year = LastStatusChangeYear - max_year) %>%
  mutate(LastStatusChangeYear = if_else(gap_year <=2, max_year, LastStatusChangeYear))

# Convert the status by year to binary
Usage_rate_full_df2 <- Usage_rate_full_df %>%
  mutate(Status_Year_Pretest_2021 = if_else(Status_Year_Pretest_2021>=1, 1, 0),
         Status_Year_Operational_2022 = if_else(Status_Year_Operational_2022>=1, 1, 0),
         Status_Year_Operational_2023 = if_else(Status_Year_Operational_2023>=1, 1, 0),
         Status_Year_Operational_2024 = if_else(Status_Year_Operational_2024>=1, 1, 0),
         Status_Year_Operational_2025 = if_else(Status_Year_Operational_2025>=1, 1, 0)) %>%
  mutate(total_flag_all =  if_else(total_flag_all>=1, 1, 0),
         total_flag_distractor =  if_else(total_flag_distractor>=1, 1, 0),
         total_falg_differentiate =  if_else(total_falg_differentiate>=1, 1, 0),
         total_flag_difficult =  if_else(total_flag_difficult>=1, 1, 0),
         total_flag_easy =  if_else(total_flag_easy>=1, 1, 0),
         total_flag_miskeyed =  if_else(total_flag_miskeyed>=1, 1, 0),
         total_flag_lowperformers =  if_else(total_flag_lowperformers>=1, 1, 0),
         total_flag_changestats =  if_else(total_flag_changestats>=1, 1, 0)) %>%
  mutate(total_operational_time=Status_Year_Operational_2022+Status_Year_Operational_2023+Status_Year_Operational_2024+Status_Year_Operational_2025) %>% 
  mutate(pretest_only = if_else(total_operational_time == 0,1,0)) %>%
  mutate(pretest_only_retired = if_else(total_operational_time == 0 & Current_status_recode==1,1,0)) 

# Summary
pretest.2021_summary <- Usage_rate_full_df2 %>%
  group_by(total_operational_time) %>%
  summarise(total_items = n(),
            total_n_form = mean(n_form),
            total_n_candidate = mean(n_candidate),
            min_year = min(min_year),
            max_year = max(max_year),
            avg_n_year = mean(n_year),
            avg_usage_rate = mean(usage_rate),
            total_pretest_2021 = sum(Status_Year_Pretest_2021),
            total_operationalUsed_2022 = sum(Status_Year_Operational_2022),
            total_operationalUsed_2023 = sum(Status_Year_Operational_2023),
            total_operationalUsed_2024 = sum(Status_Year_Operational_2024),
            total_operationalUsed_2025 = sum(Status_Year_Operational_2025),
            total_current_operational = sum(total_operationalUsed_2022, total_operationalUsed_2023, total_operationalUsed_2024, total_operationalUsed_2025),
            total_multiple_form = sum(multiple_form_1_year),
            total_HotSpot = sum(Item_Type_HotSpot),
            total_SSMC = sum(Item_Type_SSMC),
            total_form_build = sum(Form_Build),
            avg_PValue = mean(PValue, na.rm = T),
            avg_PBis = mean(PBis, na.rm = T),
            avg_Corrected_PBis = mean(Corrected_PBis, na.rm = T),
            avg_IRT_b = mean(IRT_b, na.rm = T),
            avg_mean_time = mean(Mean_Time_Seconds, na.rm = T),
            total_image = sum(Has_Image),
            total_video = sum(Has_Video),
            total_exhibit = sum(Has_Exhibit),
            total_flag_all = sum(total_flag_all),
            total_flag_distractor = sum(total_flag_distractor),
            total_falg_differentiate = sum(total_falg_differentiate),
            total_flag_difficult = sum(total_flag_difficult),
            total_flag_easy = sum(total_flag_easy),
            total_flag_miskeyed = sum(total_flag_miskeyed),
            total_flag_lowperformers = sum(total_flag_lowperformers),
            total_flag_changestats = sum(total_flag_changestats),
            total_domain_1 = sum(Domain_1, na.rm=T),
            total_domain_2 = sum(Domain_2, na.rm=T),
            total_domain_3 = sum(Domain_3, na.rm=T),
            total_domain_4 = sum(Domain_4, na.rm=T),
            total_pretest_only_retired = sum(pretest_only_retired))


pretest.2021_summary$cohort <- "pretest_2021"
pretest.2021_cohort <- Usage_rate_full_df2

# 2022 pretest cohort ---------------------------------------------------------------------------------------------
# Create a usage_rate dataset
# We want to know how many years an item is available: if an item is retired/rejected before 2020, we take their max year; otherwise, 2020
Usage_rate_df <- dat.full.2022 %>%
  group_by(Item) %>% 
  summarise(n_form = n(), 
            n_candidate = sum(total_candidate),
            min_year = min(Year),
            max_year = if_else(Current_status == 0, 2025, max(Year)),
            n_year = if_else(max_year-min_year==0,1,max_year-min_year+1),
            LastStatusChangeYear = max(LastStatusChangeYear),
            usage_rate = n_form/n_year,
            Scored = sum(Scored), 
            total_flag_all = sum(Total_Flag),
            total_flag_distractor = sum(Flag_Distractor),
            total_falg_differentiate = sum(Flag_Differentiate),
            total_flag_difficult = sum(Flag_Difficult),
            total_flag_easy = sum(Flag_Easy),
            total_flag_miskeyed = sum(Flag_Miskeyed),
            total_flag_lowperformers = sum(Flag_LowPerformers),
            total_flag_changestats = sum(Flag_ChangeStats)) %>%
  mutate(usage_rate = if_else(usage_rate > 1, 1, usage_rate)) %>%
  distinct() %>% ungroup()

Usage_rate_df$average_rate <- mean(Usage_rate_df$usage_rate) 

Usage_time_df <- dat.full.2022 %>% 
  select(Item, Year, `How Used`,`Item Status`) %>%
  mutate(Status_Year_Pretest_2022 = if_else(Year==2022 & `How Used`=="Pretest",1,0),
         Status_Year_Operational_2023 = if_else(Year==2023 & `How Used`=="Operational",1,0),
         Status_Year_Operational_2024 = if_else(Year==2024 & `How Used`=="Operational",1,0),
         Status_Year_Operational_2025 = if_else(Year==2025 & `How Used`=="Operational",1,0)) %>%
  group_by(Item) %>%
  summarise(Status_Year_Pretest_2022 = sum(Status_Year_Pretest_2022, na.rm = T),
            Status_Year_Operational_2023 = sum(Status_Year_Operational_2023, na.rm = T),
            Status_Year_Operational_2024 = sum(Status_Year_Operational_2024, na.rm = T),
            Status_Year_Operational_2025 = sum(Status_Year_Operational_2025, na.rm = T),
            Current_status = unique(`Item Status`)) 

# Join the usage rate and the usage time together.
# Convert the status_year number to binary
Usage_rate_full_df <- Usage_rate_df %>% 
  left_join(Usage_time_df, by="Item")


# 2 forms in one year. How many of those years the item are on multiple forms.
Usage_rate_full_df <- Usage_rate_full_df %>%
  group_by(Item) %>%
  mutate(multiple_form_1_year = if_else(any(Status_Year_Pretest_2022>=2,
                                            Status_Year_Operational_2023>=2,
                                            Status_Year_Operational_2024>=2,
                                            Status_Year_Operational_2025>=2), 1, 0))

# Deduplicate
Usage_rate_full_df <- unique(Usage_rate_full_df)

# Merge in the flags and other stats of the last rows of each item
dat.last.row <- dat.full.clean2 %>%
  group_by(Item) %>%
  arrange(Form) %>%
  slice(n()) %>%
  select(Item, Item_Type_HotSpot, Item_Type_SSMC,Form_Build, PValue, PBis, Corrected_PBis, IRT_b, Mean_Time_Seconds, 
         Has_Image, Has_Video, Has_Exhibit, Domain, Current_status)

Usage_rate_full_df <-Usage_rate_full_df %>%
  left_join(dat.last.row, by = "Item")

# Calculate pretest only
Usage_rate_full_df <-Usage_rate_full_df %>%
  rename(Current_status = Current_status.x, Current_status_recode = Current_status.y) %>%
  dummy_cols(select_columns = c('Domain', 'Current_status'))  # Create dummy variables for domain

# Calculate the time gap between the last form year and last status change year
# If the status change is resting/retired/rework within 2 years since the last operational, update the LastStatusChangeYear to the same as the max year
Usage_rate_full_df <-Usage_rate_full_df %>%
  mutate(gap_year = LastStatusChangeYear - max_year) %>%
  mutate(LastStatusChangeYear = if_else(gap_year <=2, max_year, LastStatusChangeYear))

# Convert the status by year to binary
Usage_rate_full_df2 <- Usage_rate_full_df %>%
  mutate(Status_Year_Pretest_2022 = if_else(Status_Year_Pretest_2022>=1, 1, 0),
         Status_Year_Operational_2023 = if_else(Status_Year_Operational_2023>=1, 1, 0),
         Status_Year_Operational_2024 = if_else(Status_Year_Operational_2024>=1, 1, 0),
         Status_Year_Operational_2025 = if_else(Status_Year_Operational_2025>=1, 1, 0)) %>%
  mutate(total_flag_all =  if_else(total_flag_all>=1, 1, 0),
         total_flag_distractor =  if_else(total_flag_distractor>=1, 1, 0),
         total_falg_differentiate =  if_else(total_falg_differentiate>=1, 1, 0),
         total_flag_difficult =  if_else(total_flag_difficult>=1, 1, 0),
         total_flag_easy =  if_else(total_flag_easy>=1, 1, 0),
         total_flag_miskeyed =  if_else(total_flag_miskeyed>=1, 1, 0),
         total_flag_lowperformers =  if_else(total_flag_lowperformers>=1, 1, 0),
         total_flag_changestats =  if_else(total_flag_changestats>=1, 1, 0)) %>%
  mutate(total_operational_time=Status_Year_Operational_2023+Status_Year_Operational_2024+Status_Year_Operational_2025) %>% 
  mutate(pretest_only = if_else(total_operational_time == 0,1,0)) %>%
  mutate(pretest_only_retired = if_else(total_operational_time == 0 & Current_status_recode==1,1,0)) 

# Summary
pretest.2022_summary <- Usage_rate_full_df2 %>%
  group_by(total_operational_time) %>%
  summarise(total_items = n(),
            total_n_form = mean(n_form),
            total_n_candidate = mean(n_candidate),
            min_year = min(min_year),
            max_year = max(max_year),
            avg_n_year = mean(n_year),
            avg_usage_rate = mean(usage_rate),
            total_pretest_2022 = sum(Status_Year_Pretest_2022),
            total_operationalUsed_2023 = sum(Status_Year_Operational_2023),
            total_operationalUsed_2024 = sum(Status_Year_Operational_2024),
            total_operationalUsed_2025 = sum(Status_Year_Operational_2025),
            total_current_operational = sum(total_operationalUsed_2023, total_operationalUsed_2024, total_operationalUsed_2025),
            total_multiple_form = sum(multiple_form_1_year),
            total_HotSpot = sum(Item_Type_HotSpot),
            total_SSMC = sum(Item_Type_SSMC),
            total_form_build = sum(Form_Build),
            avg_PValue = mean(PValue, na.rm = T),
            avg_PBis = mean(PBis, na.rm = T),
            avg_Corrected_PBis = mean(Corrected_PBis, na.rm = T),
            avg_IRT_b = mean(IRT_b, na.rm = T),
            avg_mean_time = mean(Mean_Time_Seconds, na.rm = T),
            total_image = sum(Has_Image),
            total_video = sum(Has_Video),
            total_exhibit = sum(Has_Exhibit),
            total_flag_all = sum(total_flag_all),
            total_flag_distractor = sum(total_flag_distractor),
            total_falg_differentiate = sum(total_falg_differentiate),
            total_flag_difficult = sum(total_flag_difficult),
            total_flag_easy = sum(total_flag_easy),
            total_flag_miskeyed = sum(total_flag_miskeyed),
            total_flag_lowperformers = sum(total_flag_lowperformers),
            total_flag_changestats = sum(total_flag_changestats),
            total_domain_1 = sum(Domain_1, na.rm=T),
            total_domain_2 = sum(Domain_2, na.rm=T),
            total_domain_3 = sum(Domain_3, na.rm=T),
            total_domain_4 = sum(Domain_4, na.rm=T),
            total_pretest_only_retired = sum(pretest_only_retired))


pretest.2022_summary$cohort <- "pretest_2022"
pretest.2022_cohort <- Usage_rate_full_df2


# 2023 pretest cohort ---------------------------------------------------------------------------------------------
# Create a usage_rate dataset
# We want to know how many years an item is available: if an item is retired/rejected before 2020, we take their max year; otherwise, 2020
Usage_rate_df <- dat.full.2023 %>%
  group_by(Item) %>% 
  summarise(n_form = n(), 
            n_candidate = sum(total_candidate),
            min_year = min(Year),
            max_year = if_else(Current_status == 0, 2025, max(Year)),
            n_year = if_else(max_year-min_year==0,1,max_year-min_year+1),
            LastStatusChangeYear = max(LastStatusChangeYear),
            usage_rate = n_form/n_year,
            Scored = sum(Scored), 
            total_flag_all = sum(Total_Flag),
            total_flag_distractor = sum(Flag_Distractor),
            total_falg_differentiate = sum(Flag_Differentiate),
            total_flag_difficult = sum(Flag_Difficult),
            total_flag_easy = sum(Flag_Easy),
            total_flag_miskeyed = sum(Flag_Miskeyed),
            total_flag_lowperformers = sum(Flag_LowPerformers),
            total_flag_changestats = sum(Flag_ChangeStats)) %>%
  mutate(usage_rate = if_else(usage_rate > 1, 1, usage_rate)) %>%
  distinct() %>% ungroup()

Usage_rate_df$average_rate <- mean(Usage_rate_df$usage_rate) 

Usage_time_df <- dat.full.2023 %>% 
  select(Item, Year, `How Used`,`Item Status`) %>%
  mutate(Status_Year_Pretest_2023 = if_else(Year==2023 & `How Used`=="Pretest",1,0),
         Status_Year_Operational_2024 = if_else(Year==2024 & `How Used`=="Operational",1,0),
         Status_Year_Operational_2025 = if_else(Year==2025 & `How Used`=="Operational",1,0)) %>%
  group_by(Item) %>%
  summarise(Status_Year_Pretest_2023 = sum(Status_Year_Pretest_2023, na.rm = T),
            Status_Year_Operational_2024 = sum(Status_Year_Operational_2024, na.rm = T),
            Status_Year_Operational_2025 = sum(Status_Year_Operational_2025, na.rm = T),
            Current_status = unique(`Item Status`)) 

# Join the usage rate and the usage time together.
# Convert the status_year number to binary
Usage_rate_full_df <- Usage_rate_df %>% 
  left_join(Usage_time_df, by="Item")


# 2 forms in one year. How many of those years the item are on multiple forms.
Usage_rate_full_df <- Usage_rate_full_df %>%
  group_by(Item) %>%
  mutate(multiple_form_1_year = if_else(any(Status_Year_Pretest_2023>=2,
                                            Status_Year_Operational_2024>=2,
                                            Status_Year_Operational_2025>=2), 1, 0))

# Deduplicate
Usage_rate_full_df <- unique(Usage_rate_full_df)

# Merge in the flags and other stats of the last rows of each item
dat.last.row <- dat.full.clean2 %>%
  group_by(Item) %>%
  arrange(Form) %>%
  slice(n()) %>%
  select(Item, Item_Type_HotSpot, Item_Type_SSMC,Form_Build, PValue, PBis, Corrected_PBis, IRT_b, Mean_Time_Seconds, 
         Has_Image, Has_Video, Has_Exhibit, Domain, Current_status)

Usage_rate_full_df <-Usage_rate_full_df %>%
  left_join(dat.last.row, by = "Item")

# Calculate pretest only
Usage_rate_full_df <-Usage_rate_full_df %>%
  rename(Current_status = Current_status.x, Current_status_recode = Current_status.y) %>%
  dummy_cols(select_columns = c('Domain', 'Current_status'))  # Create dummy variables for domain

# Calculate the time gap between the last form year and last status change year
# If the status change is resting/retired/rework within 2 years since the last operational, update the LastStatusChangeYear to the same as the max year
Usage_rate_full_df <-Usage_rate_full_df %>%
  mutate(gap_year = LastStatusChangeYear - max_year) %>%
  mutate(LastStatusChangeYear = if_else(gap_year <=2, max_year, LastStatusChangeYear))

# Convert the status by year to binary
Usage_rate_full_df2 <- Usage_rate_full_df %>%
  mutate(Status_Year_Pretest_2023 = if_else(Status_Year_Pretest_2023>=1, 1, 0),
         Status_Year_Operational_2024 = if_else(Status_Year_Operational_2024>=1, 1, 0),
         Status_Year_Operational_2025 = if_else(Status_Year_Operational_2025>=1, 1, 0)) %>%
  mutate(total_flag_all =  if_else(total_flag_all>=1, 1, 0),
         total_flag_distractor =  if_else(total_flag_distractor>=1, 1, 0),
         total_falg_differentiate =  if_else(total_falg_differentiate>=1, 1, 0),
         total_flag_difficult =  if_else(total_flag_difficult>=1, 1, 0),
         total_flag_easy =  if_else(total_flag_easy>=1, 1, 0),
         total_flag_miskeyed =  if_else(total_flag_miskeyed>=1, 1, 0),
         total_flag_lowperformers =  if_else(total_flag_lowperformers>=1, 1, 0),
         total_flag_changestats =  if_else(total_flag_changestats>=1, 1, 0)) %>%
  mutate(total_operational_time=Status_Year_Operational_2024+Status_Year_Operational_2025) %>% 
  mutate(pretest_only = if_else(total_operational_time == 0,1,0)) %>%
  mutate(pretest_only_retired = if_else(total_operational_time == 0 & Current_status_recode==1,1,0)) 

# Summary
pretest.2023_summary <- Usage_rate_full_df2 %>%
  group_by(total_operational_time) %>%
  summarise(total_items = n(),
            total_n_form = mean(n_form),
            total_n_candidate = mean(n_candidate),
            min_year = min(min_year),
            max_year = max(max_year),
            avg_n_year = mean(n_year),
            avg_usage_rate = mean(usage_rate),
            total_pretest_2023 = sum(Status_Year_Pretest_2023),
            total_operationalUsed_2024 = sum(Status_Year_Operational_2024),
            total_operationalUsed_2025 = sum(Status_Year_Operational_2025),
            total_current_operational = sum(total_operationalUsed_2024, total_operationalUsed_2025),
            total_multiple_form = sum(multiple_form_1_year),
            total_HotSpot = sum(Item_Type_HotSpot),
            total_SSMC = sum(Item_Type_SSMC),
            total_form_build = sum(Form_Build),
            avg_PValue = mean(PValue, na.rm = T),
            avg_PBis = mean(PBis, na.rm = T),
            avg_Corrected_PBis = mean(Corrected_PBis, na.rm = T),
            avg_IRT_b = mean(IRT_b, na.rm = T),
            avg_mean_time = mean(Mean_Time_Seconds, na.rm = T),
            total_image = sum(Has_Image),
            total_video = sum(Has_Video),
            total_exhibit = sum(Has_Exhibit),
            total_flag_all = sum(total_flag_all),
            total_flag_distractor = sum(total_flag_distractor),
            total_falg_differentiate = sum(total_falg_differentiate),
            total_flag_difficult = sum(total_flag_difficult),
            total_flag_easy = sum(total_flag_easy),
            total_flag_miskeyed = sum(total_flag_miskeyed),
            total_flag_lowperformers = sum(total_flag_lowperformers),
            total_flag_changestats = sum(total_flag_changestats),
            total_domain_1 = sum(Domain_1, na.rm=T),
            total_domain_2 = sum(Domain_2, na.rm=T),
            total_domain_3 = sum(Domain_3, na.rm=T),
            total_domain_4 = sum(Domain_4, na.rm=T),
            total_pretest_only_retired = sum(pretest_only_retired))

pretest.2023_summary$cohort <- "pretest_2023"
pretest.2023_cohort <- Usage_rate_full_df2

# 2024 pretest cohort ---------------------------------------------------------------------------------------------
# Create a usage_rate dataset
# We want to know how many years an item is available: if an item is retired/rejected before 2020, we take their max year; otherwise, 2020
Usage_rate_df <- dat.full.2024 %>%
  group_by(Item) %>% 
  summarise(n_form = n(), 
            n_candidate = sum(total_candidate),
            min_year = min(Year),
            max_year = if_else(Current_status == 0, 2025, max(Year)),
            n_year = if_else(max_year-min_year==0,1,max_year-min_year+1),
            LastStatusChangeYear = max(LastStatusChangeYear),
            usage_rate = n_form/n_year,
            Scored = sum(Scored), 
            total_flag_all = sum(Total_Flag),
            total_flag_distractor = sum(Flag_Distractor),
            total_falg_differentiate = sum(Flag_Differentiate),
            total_flag_difficult = sum(Flag_Difficult),
            total_flag_easy = sum(Flag_Easy),
            total_flag_miskeyed = sum(Flag_Miskeyed),
            total_flag_lowperformers = sum(Flag_LowPerformers),
            total_flag_changestats = sum(Flag_ChangeStats)) %>%
  mutate(usage_rate = if_else(usage_rate > 1, 1, usage_rate)) %>%
  distinct() %>% ungroup()

Usage_rate_df$average_rate <- mean(Usage_rate_df$usage_rate) 

Usage_time_df <- dat.full.2024 %>% 
  select(Item, Year, `How Used`,`Item Status`) %>%
  mutate(Status_Year_Pretest_2024 = if_else(Year==2024 & `How Used`=="Pretest",1,0),
         Status_Year_Operational_2025 = if_else(Year==2025 & `How Used`=="Operational",1,0)) %>%
  group_by(Item) %>%
  summarise(Status_Year_Pretest_2024 = sum(Status_Year_Pretest_2024, na.rm = T),
            Status_Year_Operational_2025 = sum(Status_Year_Operational_2025, na.rm = T),
            Current_status = unique(`Item Status`)) 

# Join the usage rate and the usage time together.
# Convert the status_year number to binary
Usage_rate_full_df <- Usage_rate_df %>% 
  left_join(Usage_time_df, by="Item")


# 2 forms in one year. How many of those years the item are on multiple forms.
Usage_rate_full_df <- Usage_rate_full_df %>%
  group_by(Item) %>%
  mutate(multiple_form_1_year = if_else(any(Status_Year_Pretest_2024>=2,
                                            Status_Year_Operational_2025>=2), 1, 0))

# Deduplicate
Usage_rate_full_df <- unique(Usage_rate_full_df)

# Merge in the flags and other stats of the last rows of each item
dat.last.row <- dat.full.clean2 %>%
  group_by(Item) %>%
  arrange(Form) %>%
  slice(n()) %>%
  select(Item, Item_Type_HotSpot, Item_Type_SSMC,Form_Build, PValue, PBis, Corrected_PBis, IRT_b, Mean_Time_Seconds, 
         Has_Image, Has_Video, Has_Exhibit, Domain, Current_status)

Usage_rate_full_df <-Usage_rate_full_df %>%
  left_join(dat.last.row, by = "Item")

# Calculate pretest only
Usage_rate_full_df <-Usage_rate_full_df %>%
  rename(Current_status = Current_status.x, Current_status_recode = Current_status.y) %>%
  dummy_cols(select_columns = c('Domain', 'Current_status'))  # Create dummy variables for domain

# Calculate the time gap between the last form year and last status change year
# If the status change is resting/retired/rework within 2 years since the last operational, update the LastStatusChangeYear to the same as the max year
Usage_rate_full_df <-Usage_rate_full_df %>%
  mutate(gap_year = LastStatusChangeYear - max_year) %>%
  mutate(LastStatusChangeYear = if_else(gap_year <=2, max_year, LastStatusChangeYear))

# Convert the status by year to binary
Usage_rate_full_df2 <- Usage_rate_full_df %>%
  mutate(Status_Year_Pretest_2024 = if_else(Status_Year_Pretest_2024>=1, 1, 0),
         Status_Year_Operational_2025 = if_else(Status_Year_Operational_2025>=1, 1, 0)) %>%
  mutate(total_flag_all =  if_else(total_flag_all>=1, 1, 0),
         total_flag_distractor =  if_else(total_flag_distractor>=1, 1, 0),
         total_falg_differentiate =  if_else(total_falg_differentiate>=1, 1, 0),
         total_flag_difficult =  if_else(total_flag_difficult>=1, 1, 0),
         total_flag_easy =  if_else(total_flag_easy>=1, 1, 0),
         total_flag_miskeyed =  if_else(total_flag_miskeyed>=1, 1, 0),
         total_flag_lowperformers =  if_else(total_flag_lowperformers>=1, 1, 0),
         total_flag_changestats =  if_else(total_flag_changestats>=1, 1, 0)) %>%
  mutate(total_operational_time=Status_Year_Operational_2025) %>% 
  mutate(pretest_only = if_else(total_operational_time == 0,1,0)) %>%
  mutate(pretest_only_retired = if_else(total_operational_time == 0 & Current_status_recode==1,1,0)) 

# Summary
pretest.2024_summary <- Usage_rate_full_df2 %>%
  group_by(total_operational_time) %>%
  summarise(total_items = n(),
            total_n_form = mean(n_form),
            total_n_candidate = mean(n_candidate),
            min_year = min(min_year),
            max_year = max(max_year),
            avg_n_year = mean(n_year),
            avg_usage_rate = mean(usage_rate),
            total_pretest_2024 = sum(Status_Year_Pretest_2024),
            total_operationalUsed_2025 = sum(Status_Year_Operational_2025),
            total_current_operational = sum(total_operationalUsed_2025),
            total_multiple_form = sum(multiple_form_1_year),
            total_HotSpot = sum(Item_Type_HotSpot),
            total_SSMC = sum(Item_Type_SSMC),
            total_form_build = sum(Form_Build),
            avg_PValue = mean(PValue, na.rm = T),
            avg_PBis = mean(PBis, na.rm = T),
            avg_Corrected_PBis = mean(Corrected_PBis, na.rm = T),
            avg_IRT_b = mean(IRT_b, na.rm = T),
            avg_mean_time = mean(Mean_Time_Seconds, na.rm = T),
            total_image = sum(Has_Image),
            total_video = sum(Has_Video),
            total_exhibit = sum(Has_Exhibit),
            total_flag_all = sum(total_flag_all),
            total_flag_distractor = sum(total_flag_distractor),
            total_falg_differentiate = sum(total_falg_differentiate),
            total_flag_difficult = sum(total_flag_difficult),
            total_flag_easy = sum(total_flag_easy),
            total_flag_miskeyed = sum(total_flag_miskeyed),
            total_flag_lowperformers = sum(total_flag_lowperformers),
            total_flag_changestats = sum(total_flag_changestats),
            total_domain_1 = sum(Domain_1, na.rm=T),
            total_domain_2 = sum(Domain_2, na.rm=T),
            total_domain_3 = sum(Domain_3, na.rm=T),
            total_domain_4 = sum(Domain_4, na.rm=T),
            total_pretest_only_retired = sum(pretest_only_retired))

pretest.2024_summary$cohort <- "pretest_2024"
pretest.2024_cohort <- Usage_rate_full_df2

# 2025 pretest cohort ---------------------------------------------------------------------------------------------
# Create a usage_rate dataset
# We want to know how many years an item is available: if an item is retired/rejected before 2020, we take their max year; otherwise, 2020
Usage_rate_df <- dat.full.2025 %>%
  group_by(Item) %>% 
  summarise(n_form = n(), 
            n_candidate = sum(total_candidate),
            min_year = min(Year),
            max_year = if_else(Current_status == 0, 2025, max(Year)),
            n_year = if_else(max_year-min_year==0,1,max_year-min_year+1),
            LastStatusChangeYear = max(LastStatusChangeYear),
            usage_rate = n_form/n_year,
            Scored = sum(Scored), 
            total_flag_all = sum(Total_Flag),
            total_flag_distractor = sum(Flag_Distractor),
            total_falg_differentiate = sum(Flag_Differentiate),
            total_flag_difficult = sum(Flag_Difficult),
            total_flag_easy = sum(Flag_Easy),
            total_flag_miskeyed = sum(Flag_Miskeyed),
            total_flag_lowperformers = sum(Flag_LowPerformers),
            total_flag_changestats = sum(Flag_ChangeStats)) %>%
  mutate(usage_rate = if_else(usage_rate > 1, 1, usage_rate)) %>%
  distinct() %>% ungroup()

Usage_rate_df$average_rate <- mean(Usage_rate_df$usage_rate) 

Usage_time_df <- dat.full.2025 %>% 
  select(Item, Year, `How Used`,`Item Status`) %>%
  mutate(Status_Year_Pretest_2025 = if_else(Year==2025 & `How Used`=="Pretest",1,0),
         Status_Year_Operational_2026 = if_else(Year==2026 & `How Used`=="Operational",1,0)) %>%
  group_by(Item) %>%
  summarise(Status_Year_Pretest_2025 = sum(Status_Year_Pretest_2025, na.rm = T),
            Status_Year_Operational_2026 = sum(Status_Year_Operational_2026, na.rm = T),
            Current_status = unique(`Item Status`)) 

# Join the usage rate and the usage time together.
# Convert the status_year number to binary
Usage_rate_full_df <- Usage_rate_df %>% 
  left_join(Usage_time_df, by="Item")


# 2 forms in one year. How many of those years the item are on multiple forms.
Usage_rate_full_df <- Usage_rate_full_df %>%
  group_by(Item) %>%
  mutate(multiple_form_1_year = if_else(any(Status_Year_Pretest_2025>=2,
                                            Status_Year_Operational_2026>=2), 1, 0))

# Deduplicate
Usage_rate_full_df <- unique(Usage_rate_full_df)

# Merge in the flags and other stats of the last rows of each item
dat.last.row <- dat.full.clean2 %>%
  group_by(Item) %>%
  arrange(Form) %>%
  slice(n()) %>%
  select(Item, Item_Type_HotSpot, Item_Type_SSMC,Form_Build, PValue, PBis, Corrected_PBis, IRT_b, Mean_Time_Seconds, 
         Has_Image, Has_Video, Has_Exhibit, Domain, Current_status)

Usage_rate_full_df <-Usage_rate_full_df %>%
  left_join(dat.last.row, by = "Item")

# Calculate pretest only
Usage_rate_full_df <-Usage_rate_full_df %>%
  rename(Current_status = Current_status.x, Current_status_recode = Current_status.y) %>%
  dummy_cols(select_columns = c('Domain', 'Current_status'))  # Create dummy variables for domain

# Calculate the time gap between the last form year and last status change year
# If the status change is resting/retired/rework within 2 years since the last operational, update the LastStatusChangeYear to the same as the max year
Usage_rate_full_df <-Usage_rate_full_df %>%
  mutate(gap_year = LastStatusChangeYear - max_year) %>%
  mutate(LastStatusChangeYear = if_else(gap_year <=2, max_year, LastStatusChangeYear))

# Convert the status by year to binary
Usage_rate_full_df2 <- Usage_rate_full_df %>%
  mutate(Status_Year_Pretest_2025 = if_else(Status_Year_Pretest_2025>=1, 1, 0),
         Status_Year_Operational_2026 = if_else(Status_Year_Operational_2026>=1, 1, 0)) %>%
  mutate(total_flag_all =  if_else(total_flag_all>=1, 1, 0),
         total_flag_distractor =  if_else(total_flag_distractor>=1, 1, 0),
         total_falg_differentiate =  if_else(total_falg_differentiate>=1, 1, 0),
         total_flag_difficult =  if_else(total_flag_difficult>=1, 1, 0),
         total_flag_easy =  if_else(total_flag_easy>=1, 1, 0),
         total_flag_miskeyed =  if_else(total_flag_miskeyed>=1, 1, 0),
         total_flag_lowperformers =  if_else(total_flag_lowperformers>=1, 1, 0),
         total_flag_changestats =  if_else(total_flag_changestats>=1, 1, 0)) %>%
  mutate(total_operational_time=Status_Year_Operational_2026) %>% 
  mutate(pretest_only = if_else(total_operational_time == 0,1,0)) %>%
  mutate(pretest_only_retired = if_else(total_operational_time == 0 & Current_status_recode==1,1,0)) 

# Summary
pretest.2025_summary <- Usage_rate_full_df2 %>%
  group_by(total_operational_time) %>%
  summarise(total_items = n(),
            total_n_form = mean(n_form),
            total_n_candidate = mean(n_candidate),
            min_year = min(min_year),
            max_year = max(max_year),
            avg_n_year = mean(n_year),
            avg_usage_rate = mean(usage_rate),
            total_pretest_2025 = sum(Status_Year_Pretest_2025),
            total_operationalUsed_2026 = sum(Status_Year_Operational_2026),
            total_multiple_form = sum(multiple_form_1_year),
            total_HotSpot = sum(Item_Type_HotSpot),
            total_SSMC = sum(Item_Type_SSMC),
            total_form_build = sum(Form_Build),
            avg_PValue = mean(PValue, na.rm = T),
            avg_PBis = mean(PBis, na.rm = T),
            avg_Corrected_PBis = mean(Corrected_PBis, na.rm = T),
            avg_IRT_b = mean(IRT_b, na.rm = T),
            avg_mean_time = mean(Mean_Time_Seconds, na.rm = T),
            total_image = sum(Has_Image),
            total_video = sum(Has_Video),
            total_exhibit = sum(Has_Exhibit),
            total_flag_all = sum(total_flag_all),
            total_flag_distractor = sum(total_flag_distractor),
            total_falg_differentiate = sum(total_falg_differentiate),
            total_flag_difficult = sum(total_flag_difficult),
            total_flag_easy = sum(total_flag_easy),
            total_flag_miskeyed = sum(total_flag_miskeyed),
            total_flag_lowperformers = sum(total_flag_lowperformers),
            total_flag_changestats = sum(total_flag_changestats),
            total_domain_1 = sum(Domain_1, na.rm=T),
            total_domain_2 = sum(Domain_2, na.rm=T),
            total_domain_3 = sum(Domain_3, na.rm=T),
            total_domain_4 = sum(Domain_4, na.rm=T),
            total_pretest_only_retired = sum(pretest_only_retired))

pretest.2025_summary$cohort <- "pretest_2025"
pretest.2025_cohort <- Usage_rate_full_df2

##################################
Pretest.all_summary <- bind_rows(pretest.2016_summary,pretest.2017_summary,
                            pretest.2019_summary, pretest.2020_summary, pretest.2021_summary,
                            pretest.2022_summary,pretest.2023_summary,pretest.2024_summary,pretest.2025_summary)
Pretest.all_summary[is.na(Pretest.all_summary)] <- 0
Pretest.all_summary <-Pretest.all_summary %>%
  select(cohort, total_operational_time, min_year, max_year, avg_n_year, total_items,
         total_pretest_only_retired, total_current_operational, total_current_lost, 
         total_current_resting, total_current_retired, total__current_rework,
         total_HotSpot, total_SSMC, avg_PValue, avg_PBis, avg_Corrected_PBis,
         avg_IRT_b, total_image, total_video, total_exhibit, total_domain_1,
         total_domain_2, total_domain_3, total_domain_4, total_domain_5, total_domain_6, total_domain_7,
         total_domain_8, total_domain_0)


write.xlsx(Pretest.all_summary,"Pretest.all_summary_Year_Updated.xlsx")

# Analysis for different groups of pretest/operation -------------------------------------------------------------------------------------
# Group items by their time of being operational
pretest.all_cohorts <- dplyr::bind_rows(pretest.2016_cohort, pretest.2017_cohort,
                                        pretest.2019_cohort, pretest.2020_cohort,
                                        pretest.2021_cohort, pretest.2022_cohort, 
                                        pretest.2023_cohort, pretest.2024_cohort,
                                        pretest.2025_cohort)
pretest.all_cohorts[is.na(pretest.all_cohorts)] <- 0
pretest.all_cohorts <- pretest.all_cohorts %>%
  mutate(group = case_when(pretest_only==1 & pretest_only_retired==0 ~ "Pretest.active",
                           pretest_only_retired==1~"Pretest.inactive",
                           total_operational_time==1~"operation.1",
                           total_operational_time>=2~"operation.2more"))

pretest.active_group <- pretest.all_cohorts %>% filter(pretest_only==1 & pretest_only_retired==0)
pretest.inactive_group <- pretest.all_cohorts %>% filter(pretest_only_retired==1)
operation.1_group <- pretest.all_cohorts %>% filter(total_operational_time==1)
operation.2more_group <- pretest.all_cohorts %>% filter(total_operational_time>=2)

# EDA
pretest.all_cohorts_summary <- pretest.all_cohorts %>%
  group_by(group) %>%
  summarise(total_items = n(),
            avg_n_form = mean(n_form),
            avg_n_candidate = mean(n_candidate),
            min_year = min(min_year),
            max_year = max(max_year),
            total_current_operational = sum(Current_status_Operational),
            total_current_lost = sum(Current_status_Retired)+sum(Current_status_Resting)+sum(Current_status_Rework),
            total_current_resting = sum(Current_status_Resting),
            total_current_retired = sum(Current_status_Retired),
            total__current_rework = sum(Current_status_Rework),
            total_multiple_form = sum(multiple_form_1_year),
            total_HotSpot = sum(Item_Type_HotSpot),
            total_SSMC = sum(Item_Type_SSMC),
            total_form_build = sum(Form_Build),
            avg_PValue = mean(PValue, na.rm = T),
            avg_PBis = mean(PBis, na.rm = T),
            avg_Corrected_PBis = mean(Corrected_PBis, na.rm = T),
            avg_IRT_b = mean(IRT_b, na.rm = T),
            avg_mean_time = mean(Mean_Time_Seconds, na.rm = T),
            total_image = sum(Has_Image),
            total_video = sum(Has_Video),
            total_exhibit = sum(Has_Exhibit),
            total_flag_all = sum(total_flag_all),
            total_flag_distractor = sum(total_flag_distractor),
            total_falg_differentiate = sum(total_falg_differentiate),
            total_flag_difficult = sum(total_flag_difficult),
            total_flag_easy = sum(total_flag_easy),
            total_flag_miskeyed = sum(total_flag_miskeyed),
            total_flag_lowperformers = sum(total_flag_lowperformers),
            total_flag_changestats = sum(total_flag_changestats),
            total_domain_1 = sum(Domain_1),
            total_domain_2 = sum(Domain_2),
            total_domain_3 = sum(Domain_3),
            total_domain_4 = sum(Domain_4),
            total_domain_5 = sum(Domain_5),
            total_domain_6 = sum(Domain_6),
            total_domain_7 = sum(Domain_7),
            total_domain_8 = sum(Domain_8))

write.xlsx(pretest.all_cohorts_summary,"Pretest.all_summary_Group_updated.xlsx")


# Visualization
ggplot(data = pretest.all_cohorts, aes(x = group)) + 
  geom_bar() + 
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  scale_x_discrete(limits = c("Pretest.inactive", "Pretest.active", "operation.1", "operation.2more"))

ggplot(data = pretest.all_cohorts, aes(x = PValue, y = Corrected_PBis)) + 
  geom_point(aes(color=group))

ggplot(data = pretest.all_cohorts, aes(x = PValue, y = PBis)) + 
  geom_point(aes(color=group))

ggplot(data = pretest.all_cohorts, aes(x = IRT_b, y = Corrected_PBis)) + 
  geom_point(aes(color=group))



# Examine PValue, pbis, corrected_pbis
# one box per variety
value_stats <- pretest.all_cohorts %>% group_by(group) %>% 
  summarise(min_PValue = min(PValue),
            max_PValue = max(PValue),
            mean_PValue = mean(PValue),
            median_PValue = median(PValue),
            min_PBis = min(PBis),
            max_PBis = max(PBis),
            mean_PBis = mean(PBis),
            median_PBis = median(PBis),
            min_Corrected_PBis = min(Corrected_PBis),
            max_Corrected_PBis = max(Corrected_PBis),
            mean_Corrected_PBis = mean(Corrected_PBis),
            median_Corrected_PBis = median(Corrected_PBis),
            min_IRT_b = min(IRT_b),
            max_IRT_b = max(IRT_b),
            mean_IRT_b = mean(IRT_b),
            median_IRT_b = median(IRT_b))

write.xlsx(value_stats,"Pretest.all_summary_GroupStats_updated.xlsx")


ggplot(pretest.all_cohorts, aes(x=reorder(group, PValue), y=PValue, fill=group)) + 
  geom_boxplot() +
  xlab("group") +
  theme(legend.position="none") +
  xlab("") +
  scale_x_discrete(limits = c("Pretest.inactive", "Pretest.active", "operation.1", "operation.2more"))

ggplot(pretest.all_cohorts, aes(x=reorder(group, PBis), y=PBis, fill=group)) + 
  geom_boxplot() +
  xlab("group") +
  theme(legend.position="none") +
  xlab("") +
  scale_x_discrete(limits = c("Pretest.inactive", "Pretest.active", "operation.1", "operation.2more"))

ggplot(pretest.all_cohorts, aes(x=reorder(group, Corrected_PBis), y=Corrected_PBis, fill=group)) + 
  geom_boxplot() +
  xlab("group") +
  theme(legend.position="none") +
  xlab("") +
  scale_x_discrete(limits = c("Pretest.inactive", "Pretest.active", "operation.1", "operation.2more"))


ggplot(pretest.all_cohorts, aes(x=reorder(group, IRT_b), y=IRT_b, fill=group)) + 
  geom_boxplot() +
  xlab("group") +
  theme(legend.position="none") +
  xlab("") +
  scale_x_discrete(limits = c("Pretest.inactive", "Pretest.active", "operation.1", "operation.2more"))

