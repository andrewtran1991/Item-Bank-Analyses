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

# Data Preparation ------------------------------------------------------------------------------------------------
# Set directory to the current folder
getwd()
setwd(getwd())

# Load in the clean dataset from the bankanalysis
dat_full_clean <- read_csv("Individual outcomes/dat.full2.clean_FE.csv")

# If an item is used consecutively (0 or 1 year gap), that item is an anchor item
dat_full_clean <- dat_full_clean %>%
  mutate(Year = if_else(str_detect(Window, "B_WIN"),Year+0.5,Year)) %>% # Add 0.5 year for B form
  group_by(Item) %>%
  arrange(Window) %>%
  mutate(Year_after = lead(Year)) %>%
  mutate(Year_gap = Year_after  - Year)

dat_full_clean <- dat_full_clean %>%
  mutate(cohort = if_else(Status=="Pretest", as.integer(Year), NA_integer_)) %>% # Determine the pretest cohort
  group_by(Item) %>%
  fill(cohort, .direction = "updown") %>%
  ungroup()

dat_anchor_list <-dat_full_clean %>% filter(Year_gap==0.5) %>% group_by(Item) %>% summarise(anchor_time=n()) 
dat_anchor <- dat_full_clean %>%
  right_join(dat_anchor_list) %>%
  dummy_cols(select_columns = c('Item Status', 'cohort'))

write.csv(dat_anchor, "dat_anchor_FE.csv")

# Analysis
# Aggregate same items
Usage_rate_df <- dat_anchor %>%
  dplyr::group_by(Item) %>% 
  dplyr::summarise(n_anchor = mean(anchor_time), 
                   total_Pretest = sum(Status_Pretest, na.rm = T),
                   total_Operational = sum(Status_Operational, na.rm = T),
                   total_flag_all = sum(Total_Flag, na.rm=T),
                   total_flag_distractor = sum(Flag_Distractor),
                   total_falg_differentiate = sum(Flag_Differentiate),
                   total_flag_difficult = sum(Flag_Difficult),
                   total_flag_easy = sum(Flag_Easy),
                   total_flag_miskeyed = sum(Flag_Miskeyed),
                   total_flag_lowperformers = sum(Flag_LowPerformers),
                   total_flag_changestats = sum(Flag_ChangeStats),
                   total_flag_HigherScore = sum(Flag_HigherScore),
                   total_flag_Largeprop = sum(Flag_Largeprop)) %>%
  distinct() %>%
  ungroup()

dat.last.row <- dat_anchor %>%
  group_by(Item) %>%
  arrange(Form) %>%
  slice(n()) %>%
  select(Item, `Item Status`, Item_Type_HotSpot, Item_Type_SSMC, PValue, PBis, Corrected_PBis, Mean_Time_Seconds, 
         Has_Image, Has_Video, Has_Exhibit, Domain, Current_status, cohort_2015, cohort_2016, cohort_2017,
         cohort_2018, cohort_2019, cohort_2020, cohort_2021, cohort_NA)

Usage_rate_full_df <-Usage_rate_df %>%
  left_join(dat.last.row, by = "Item")
Usage_rate_full_df[is.na(Usage_rate_full_df)] <- 0

# Analysis
Usage_anchor_summary <- Usage_rate_full_df %>%
  group_by(n_anchor) %>%
  summarise(total_items = n(),
            avg_PValue = mean(PValue, na.rm=T),
            avg_PBis = mean(PBis, na.rm=T),
            avg_corrected_PBis = mean(Corrected_PBis, na.rm=T),
            avg_mean_time_seconds = mean(Mean_Time_Seconds, na.rm=T),
            total_flag_items = sum(total_flag_all>0, na.rm=T),
            total_flag_distractor = sum(total_flag_distractor>0),
            total_falg_differentiate = sum(total_falg_differentiate>0),
            total_flag_difficult = sum(total_flag_difficult>0),
            total_flag_easy = sum(total_flag_easy>0),
            total_flag_miskeyed = sum(total_flag_miskeyed>0),
            total_flag_lowperformers = sum(total_flag_lowperformers>0),
            total_flag_changestats = sum(total_flag_changestats>0),
            total_flag_HigherScore = sum(total_flag_HigherScore>0),
            total_flag_Largeprop = sum(total_flag_Largeprop>0),
            total_image = sum(Has_Image),
            total_video = sum(Has_Video),
            total_exhibit = sum(Has_Exhibit),
            Cohort_before_2015 = sum(cohort_NA),
            Cohort_2015 = sum(cohort_2015),
            Cohort_2016 = sum(cohort_2016),
            Cohort_2017 = sum(cohort_2017),
            Cohort_2018 = sum(cohort_2018),
            Cohort_2019 = sum(cohort_2019),
            Cohort_2020 = sum(cohort_2020),
            Cohort_2021 = sum(cohort_2021))

Usage_anchor_summary_all <- Usage_rate_full_df %>%
  summarise(total_items = n(),
            avg_PValue = mean(PValue, na.rm=T),
            avg_PBis = mean(PBis, na.rm=T),
            avg_corrected_PBis = mean(Corrected_PBis, na.rm=T),
            avg_mean_time_seconds = mean(Mean_Time_Seconds, na.rm=T),
            total_flag_items = sum(total_flag_all>0, na.rm=T),
            total_flag_distractor = sum(total_flag_distractor>0),
            total_falg_differentiate = sum(total_falg_differentiate>0),
            total_flag_difficult = sum(total_flag_difficult>0),
            total_flag_easy = sum(total_flag_easy>0),
            total_flag_miskeyed = sum(total_flag_miskeyed>0),
            total_flag_lowperformers = sum(total_flag_lowperformers>0),
            total_flag_changestats = sum(total_flag_changestats>0),
            total_flag_HigherScore = sum(total_flag_HigherScore>0),
            total_flag_Largeprop = sum(total_flag_Largeprop>0),
            total_image = sum(Has_Image),
            total_video = sum(Has_Video),
            total_exhibit = sum(Has_Exhibit),
            Cohort_before_2015 = sum(cohort_NA),
            Cohort_2015 = sum(cohort_2015),
            Cohort_2016 = sum(cohort_2016),
            Cohort_2017 = sum(cohort_2017),
            Cohort_2018 = sum(cohort_2018),
            Cohort_2019 = sum(cohort_2019),
            Cohort_2020 = sum(cohort_2020),
            Cohort_2021 = sum(cohort_2021))


Usage_anchor_summary_all <- dplyr::bind_rows(Usage_anchor_summary,Usage_anchor_summary_all)

write.csv(Usage_rate_full_df, "Usage_rate_full_df_FE.csv")
write.csv(Usage_anchor_summary_all, "Usage_anchor_summary_FE.csv")


