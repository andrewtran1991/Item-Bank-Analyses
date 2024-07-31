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
source("util.R")

# Data Preparation ------------------------------------------------------------------------------------------------
# Set directory to the current folder
getwd()
setwd(getwd())

### User Specifications 
client <- "ARDMS"
exam <- "AB"                   
examType <- "on-demand"    
formLength <- 140
propDomain <- c(0.30,.42,.08,.20,.00,.00,.00,.00,.00) # 9 domains in total. put 0.00 as place holder if domain does not exist.
username <- "andrew.tran" #your username (firstname.lastname). Used for path in Box.


### Load in Datasets ###
# Attribute (up to 2022)
att <- get.itemattributes(exam)
att_2021AB <- att %>% filter(Window == "AB2021AB_WIN" & Scored == 1) %>% group_by(Item) %>% mutate(n_item = n())
att_2020AB <- att %>% filter(Window == "AB2020AB_WIN" & Scored == 1) %>% group_by(Item) %>% mutate(n_item = n())

# Overlapping items
att_2021AB_overlap <- att_2021AB %>% filter(n_item==2) %>% distinct(Item) 
att_2020AB_overlap <- att_2020AB %>% filter(n_item==2) %>% distinct(Item) 

#2023
# Retrieve the operational item lists from previous form builds
# Run the "Create the operational item list OB_for next form" to create the operational item list for AB2023AB
# Read the AB2023AB operational item list and apply eligibility conditions
#optitem_new <- read_csv("1. OB Operational Items w Stats_2023.csv")
optitem_new <- get.examitembank(exam, examType)
# Missing some attribute columns: Has_Exhibit
att_cols <- att %>% group_by(Item) %>% arrange(desc(Window)) %>% slice(1) %>% select(Item, Surpass_Id, Has_Image, Has_Video, Has_Exhibit) %>% ungroup() 
optitem_new <- optitem_new %>% select(-Surpass_Id, - Has_Image, - Has_Video, - Has_Exhibit) %>% mutate(Item = as.character(Item))
optitem_new <- optitem_new %>% left_join(att_cols, by="Item")
  
score_A <- openxlsx::read.xlsx("C:/Users/andrew.tran/Box/IPS/IPS Exam Folders/ARDMS and Transition Exams/AB/AB2022A-B/2. Score Table/AB2022A_01 Score Table and List of Scored Items.xlsx", 
                               sheet = "Scored Items")
score_B <- openxlsx::read.xlsx("C:/Users/andrew.tran/Box/IPS/IPS Exam Folders/ARDMS and Transition Exams/AB/AB2022A-B/2. Score Table/AB2022B_01 Score Table and List of Scored Items.xlsx", 
                               sheet = "Scored Items")
previousFormItems <- rbind(score_A, score_B)  %>% distinct(Item)

optitem_new <- cond.setup(dat = optitem_new, form_build_year = 2023,
                          cond1_old = F, year_threshold = 2017, 
                          cond1 = T, threshold1 = 5, 
                          cond2 = T, threshold2 = 7,
                          cond3 = T, threshold3 = 10,
                          cond4 = T, previousFormItems$Item)

optitem_new <- optitem_new %>% select(-Born_before, -Eligible_cond1_old) %>% mutate(Previous_Windows = as.character(Previous_Windows))

writexl::write_xlsx(optitem_new, "1. AB Operational Items w Stats_2023_with eligibility.xlsx")
optitem_new %>% group_by(Eligible_form_build) %>% count()
optitem_new %>% filter(Eligible_form_build==1) %>% group_by(Has_Exhibit) %>% count()
optitem_new %>% filter(Eligible_form_build==1) %>% group_by(Item_Type) %>% count()
optitem_new %>% filter(Eligible_form_build==1) %>% group_by(Domain) %>% count()
pool_2023AB <- optitem_new

# 2022
opitems <- openxlsx::read.xlsx("C:/Users/andrew.tran/Box/IPS/IPS Exam Folders/ARDMS and Transition Exams/AB/AB2022A-B/1. Form Build/ToVendor/1. AB2022A_01,AB2022B_01_Operational Items w Stats.xlsx", 
                               sheet = "Sheet 1")
score_A <- openxlsx::read.xlsx("C:/Users/andrew.tran/Box/IPS/IPS Exam Folders/ARDMS and Transition Exams/AB/AB2021A-B/2. Score Table (PV)/AB2021A_01 Score Table and List of Scored Items.xlsx", 
                               sheet = "Scored Items") %>% select(Item)
score_B <- openxlsx::read.xlsx("C:/Users/andrew.tran/Box/IPS/IPS Exam Folders/ARDMS and Transition Exams/AB/AB2021A-B/2. Score Table (PV)/AB2021B_01 Score Table and List of Scored Items.xlsx", 
                               sheet = "Scored Items") %>% select(Item)
previousFormItems <- rbind(score_A, score_B) %>% distinct(Item)
pool_2022AB <- cond.setup(dat = opitems, form_build_year = 2022,
                          cond1_old = F, year_threshold = 2016, 
                          cond1 = T, threshold1 = 5, 
                          cond2 = T, threshold2 = 7,
                          cond3 = T, threshold3 = 10,
                          cond4 = T, previousFormItems$Item)

pool_2022AB %>% group_by(Eligible_form_build) %>% count() # Eligible items
pool_2022AB %>% filter(Eligible_form_build == 1) %>% filter(Has_Exhibit == 1) %>% group_by(Exam) %>% count()
pool_2022AB %>% filter(Eligible_form_build == 1) %>% group_by(Item_Type) %>% count()
pool_2022AB %>% filter(Eligible_form_build == 1) %>% group_by(Domain) %>% count()


# 2021
opitems <- openxlsx::read.xlsx("C:/Users/andrew.tran/Box/IPS/IPS Exam Folders/ARDMS and Transition Exams/AB/AB2021A-B/1. Form Build (PV)/ToPearson/1. AB Operational Items w Stats.xlsx")
score_A <- openxlsx::read.xlsx("C:/Users/andrew.tran/Box/IPS/IPS Exam Folders/ARDMS and Transition Exams/AB/AB2020A-B/2. Score Table (PV)/AB2020A_01 Score Table and List of Scored Items.xlsx", 
                               sheet = "Scored Items") %>% select(Item)
score_B <- openxlsx::read.xlsx("C:/Users/andrew.tran/Box/IPS/IPS Exam Folders/ARDMS and Transition Exams/AB/AB2020A-B/2. Score Table (PV)/AB2020B_01 Score Table and List of Scored Items.xlsx", 
                               sheet = "Scored Items") %>% select(Item)
previousFormItems <- rbind(score_A, score_B) %>% distinct(Item)
pool_2021AB <- cond.setup(dat = opitems, form_build_year = 2021,
                          cond1_old = F, year_threshold = 2015, 
                          cond1 = T, threshold1 = 5, 
                          cond2 = T, threshold2 = 7,
                          cond3 = T, threshold3 = 10,
                          cond4 = T, previousFormItems$Item)

pool_2021AB %>% group_by(Eligible_form_build) %>% count() # Eligible items
pool_2021AB %>% filter(Eligible_form_build == 1) %>% filter(Has_Exhibit == 1) %>% group_by(Exam) %>% count()
pool_2021AB %>% filter(Eligible_form_build == 1) %>% group_by(Item_Type) %>% count()
pool_2021AB %>% filter(Eligible_form_build == 1) %>% group_by(Domain) %>% count()

# 2020
opitems <- openxlsx::read.xlsx("C:/Users/andrew.tran/Box/IPS/IPS Exam Folders/ARDMS and Transition Exams/AB/AB2020A-B/1. Form Build (PV)/ToPearson/1. AB Operational Items w Stats.xlsx")
# Run the "Create operational item list" to create itemstatistics that have missing columns
source("Create the operational item list_for next form_v3.R")

score_A <- openxlsx::read.xlsx("C:/Users/andrew.tran/Box/IPS/IPS Exam Folders/ARDMS and Transition Exams/AB/AB2019A-B/2. Pre-equating and Score Table/Score Table/AB2019A_01 Score Table and List of Scored Items.xlsx", 
                               sheet = "Scored Items") %>% select(Item)
score_B <- openxlsx::read.xlsx("C:/Users/andrew.tran/Box/IPS/IPS Exam Folders/ARDMS and Transition Exams/AB/AB2019A-B/2. Pre-equating and Score Table/Score Table/AB2019B_01 Score Table and List of Scored Items.xlsx", 
                               sheet = "Scored Items") %>% select(Item)
previousFormItems <- rbind(score_A, score_B) %>% distinct(Item) %>% mutate(Item = as.character(Item))
pool_2020AB <- cond.setup(dat = opitems, form_build_year = 2020,
                          cond1_old = F, year_threshold = 2014, 
                          cond1 = T, threshold1 = 5, 
                          cond2 = T, threshold2 = 7,
                          cond3 = T, threshold3 = 10,
                          cond4 = T, previousFormItems$Item)

pool_2020AB %>% group_by(Eligible_form_build) %>% count() # Eligible items
pool_2020AB %>% filter(Eligible_form_build == 1) %>% filter(Has_Exhibit == 1) %>% group_by(Exam) %>% count()
pool_2020AB %>% filter(Eligible_form_build == 1) %>% group_by(Item_Type) %>% count()
pool_2020AB %>% filter(Eligible_form_build == 1) %>% group_by(Domain) %>% count()

# Retrieve the pretested item lists from previous form builds
# 2022
att %>% filter(Window == "AB2022AB_WIN" & Status == "Being Pretested") %>% group_by(Status) %>% count() 
att %>% filter(Window == "AB2022AB_WIN" & Status == "Being Pretested" & Has_Exhibit==1) %>% group_by(Status) %>% count() 
att %>% filter(Window == "AB2022AB_WIN" & Status == "Being Pretested") %>% group_by(Item_Type) %>% count() 
att %>% filter(Window == "AB2022AB_WIN" & Status == "Being Pretested") %>% group_by(Domain) %>% count() 

# 2021
att %>% filter(Window == "AB2021AB_WIN" & Status == "Being Pretested") %>% group_by(Status) %>% count() 
att %>% filter(Window == "AB2021AB_WIN" & Status == "Being Pretested" & Has_Exhibit==1) %>% group_by(Status) %>% count() 
att %>% filter(Window == "AB2021AB_WIN" & Status == "Being Pretested") %>% group_by(Item_Type) %>% count() 
att %>% filter(Window == "AB2021AB_WIN" & Status == "Being Pretested") %>% group_by(Domain) %>% count() 

# 2020
att %>% filter(Window == "AB2020AB_WIN" & Status == "Being Pretested") %>% group_by(Status) %>% count() 
att %>% filter(Window == "AB2020AB_WIN" & Status == "Being Pretested" & Has_Exhibit==1) %>% group_by(Status) %>% count() 
att %>% filter(Window == "AB2020AB_WIN" & Status == "Being Pretested") %>% group_by(Item_Type) %>% count() 
att %>% filter(Window == "AB2020AB_WIN" & Status == "Being Pretested") %>% group_by(Domain) %>% count() 

# Retrieve the Pretest entering pool and pass content review
# 2021
item_status <- read_csv("C:/Users/andrew.tran/Box/IPS/IPS Exam Folders/ARDMS and Transition Exams/AB/AB2021A-B/3. Item Analysis/AB2021A_01,AB2021B_01_PRO Item Status Update.csv")
item_status_operational <- item_status %>% filter(Bucket=="Operational")
att_operational <- att %>% filter(Window == "AB2021AB_WIN" & Status == "Being Pretested") %>% 
  filter(Item %in% item_status_operational$Item)
att_operational %>% filter(Window == "AB2021AB_WIN" & Status == "Being Pretested") %>% group_by(Status) %>% count() 
att_operational %>% filter(Window == "AB2021AB_WIN" & Status == "Being Pretested" & Has_Exhibit==1) %>% group_by(Status) %>% count() 
att_operational %>% filter(Window == "AB2021AB_WIN" & Status == "Being Pretested") %>% group_by(Item_Type) %>% count() 
att_operational %>% filter(Window == "AB2021AB_WIN" & Status == "Being Pretested") %>% group_by(Domain) %>% count() 

# 2020
item_status <-  read_csv("C:/Users/andrew.tran/Box/IPS/IPS Exam Folders/ARDMS and Transition Exams/AB/AB2020A-B/3. Item Analysis/AB2020A-B Item Status Update.csv")
item_status_operational <- item_status %>% filter(Bucket=="Operational")
att_operational <- att %>% filter(Window == "AB2020AB_WIN" & Status == "Being Pretested") %>% 
  filter(Item %in% item_status_operational$Item)
att_operational %>% filter(Window == "AB2020AB_WIN" & Status == "Being Pretested") %>% group_by(Status) %>% count() 
att_operational %>% filter(Window == "AB2020AB_WIN" & Status == "Being Pretested" & Has_Exhibit==1) %>% group_by(Status) %>% count() 
att_operational %>% filter(Window == "AB2020AB_WIN" & Status == "Being Pretested") %>% group_by(Item_Type) %>% count() 
att_operational %>% filter(Window == "AB2020AB_WIN" & Status == "Being Pretested") %>% group_by(Domain) %>% count() 

# Operational Items (2023)
# Breakdown of items b-value range (full pool)
pool_2023AB <- pool_2023AB %>%
  mutate(IRT_b_range = case_when(IRT_b <= -6  ~ "0. <-6",
                                 IRT_b > -6 & IRT_b <= -4 ~ "1. (-6,-4]",
                                 IRT_b > -4 & IRT_b <= -2 ~ "2. (-4,-2]",
                                 IRT_b > -2 & IRT_b <= -1 ~ "3. (-2,-1]",
                                 IRT_b > -1 & IRT_b <= -.5 ~ "4. (-1,-.5]",
                                 IRT_b > -.5 & IRT_b <= 0 ~ "5. (-.5,0]",
                                 IRT_b > 0 & IRT_b <= .5 ~ "6. (0,.5]",
                                 IRT_b > .5 & IRT_b <= 1 ~ "7. (.5,1]",
                                 IRT_b > 1 & IRT_b <= 1.5 ~ "8. (1,1.5]",
                                 IRT_b > 1.5 & IRT_b <= 2 ~ "9. (1.5,2]",
                                 IRT_b > 2 & IRT_b <= 4 ~ "10. (2,4]",
                                 IRT_b > 4 & IRT_b <= 6 ~ "11. (4,6]", 
                                 IRT_b > 6 ~ "12. >6"))

poolranges_full_Domain <- pool_2023AB %>%
  group_by(Domain, IRT_b_range) %>%
  count() %>%
  pivot_wider(names_from = Domain, values_from = n)

poolranges_full_ItemType <- pool_2023AB %>%
  group_by(Item_Type, IRT_b_range) %>%
  count()%>%
  pivot_wider(names_from = Item_Type, values_from = n)

# Breakdown of items b-value range (eligible next build)
pool_2023AB_eligible <- pool_2023AB %>% filter(Eligible_form_build==1)
poolranges_full_Domain <- pool_2023AB_eligible %>%
  group_by(Domain, IRT_b_range) %>%
  count() %>%
  pivot_wider(names_from = Domain, values_from = n)


poolranges_full_ItemType <- pool_2023AB_eligible %>%
  group_by(Item_Type, IRT_b_range) %>%
  count()%>%
  pivot_wider(names_from = Item_Type, values_from = n)

# Breakdown of available items by pretest year
pool_2023AB_eligible %>% group_by(Year_born) %>% count()
poolranges_pretest_Domain <- pool_2023AB_eligible %>% group_by(Domain, Year_born) %>% count() %>% pivot_wider(names_from = Domain, values_from = n)
poolranges_pretest_ItemType <- pool_2023AB_eligible %>% group_by(Item_Type, Year_born) %>% count() %>% pivot_wider(names_from = Item_Type, values_from = n)

# Pretest info from most recent form
bank <- read.csv("AB_Item Search 20230130_154743380.csv")

bank %>% filter((Exam.Form=="AB2019A_01"|Exam.Form=="AB2019B_01") & How.Used=="Pretest") %>% group_by(Item.Status) %>% count()
bank %>% filter((Exam.Form=="AB2020A_01"|Exam.Form=="AB2020B_01") & How.Used=="Pretest") %>% group_by(Item.Status) %>% count()
bank %>% filter((Exam.Form=="AB2021A_01"|Exam.Form=="AB2021B_01") & How.Used=="Pretest") %>% group_by(Item.Status) %>% count()
