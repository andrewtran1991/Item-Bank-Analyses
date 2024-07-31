# Util functions
# Function to create a folder ----
folder.setup <- function(folder) {
  if (file.exists(folder)) {
    print("The folder already exists")
  } else {
    dir.create(folder)
  }
}


# Define Eligibility ----
cond.setup <- function(dat, form_build_year,
                       cond1_old = T, year_threshold, 
                       cond1 = T, threshold1, 
                       cond2 = T, threshold2,
                       cond3 = T, threshold3,
                       cond4 = T, previousFormItems) {
  dat <- dat %>%
    mutate(Year_born = as.integer(substr(Previous_Windows,1,2)) + 2000,
           Operation_time_count = Window_Count - 1,
           Born_before = ifelse(Year_born < year_threshold, 1, 0),
           # Condition 1_old: 
           # 1a: For items were born before previous-6-year mark (e.g. form build 2022 - we take the threshold at 2016), anything has been used 4 or more time (operational) : not eligible for form build.
           # 1b: For items were born at previous-6-year mark and after, anything has been used twice or more (operational): not eligible for form build.
           Eligible_cond1_old = ifelse(Born_before == 1 & Operation_time_count < 4, 1, if_else(Born_before == 0 & Operation_time_count < 2, 1, 0)),
           # Condition 1: Operational time
           Eligible_cond1 = ifelse(Operation_time_count >= threshold1, 0, 1),
           # Condition 2: Inactive pretest
           Pretest_active = ifelse(Window_Count==1, 2000+ as.integer(Previous_Windows), 0),
           Year_gap = form_build_year - Year_born,
           Eligible_cond2 = ifelse(Year_gap > (threshold2 + 1) & Pretest_active != 0, 0, 1),
           # Condition 3: Old item
           Eligible_cond3 = ifelse(Year_gap > threshold3,0,1),
           # Condition 4: No previous year item
           Eligible_cond4 = ifelse(Item %in% previousFormItems,0,1)
           # Condition 5: No previously used items (for SPI, if an item is used in SPI2021ABC, it cannot be used in SPI2021EDF but can be used in SPI2022ABC)
    )
  if(cond1_old == F) {dat$Eligible_cond1_old = NA_integer_}
  if(cond1 == F) {dat$Eligible_cond1 = NA_integer_}
  if(cond2 == F) {dat$Eligible_cond2 = NA_integer_}
  if(cond3 == F) {dat$Eligible_cond3 = NA_integer_}
  if(cond4 == F) {dat$Eligible_cond4 = NA_integer_}
  
  # Final eligibility
  dat <- dat %>%
    rowwise() %>%
    mutate(Eligible_total = sum(Eligible_cond1_old, Eligible_cond1, Eligible_cond2, Eligible_cond3, Eligible_cond4, na.rm = T))
  
  if(cond1==F | cond2==F | cond3==F | cond4==F) {
    dat <- dat %>%
      mutate(Eligible_form_build = ifelse(Eligible_total== 3, 1, 0))
  } else {
    dat <- dat %>%
      mutate(Eligible_form_build = ifelse(Eligible_total== 4, 1, 0))
  }
}


# Form build function ----
formbuilding.simulation <- function(object_name) {
  # Subset the pool to only eligible items, at b-value range variable
  pool <- pool %>% filter(Eligible_form_build == 1)
  poolranges <- table(cut(pool$IRT_b, breaks = c(-6,-4, -2, -1, -.5, 0, .5, 1, 1.5, 2, 4, 6)))/nrow(pool)
  
  # Load in the previous form stats (Items and IRT_b)
  # Take the 2021A form as the target - everything in this section can be done once. The results do not change based on which form is being built.
  prevformstats <- prevformstatsA
  
  # Define theta and constraints
  theta<-c(0.5,  1.5)
  TCC<-sapply(theta, expraw, items = cbind.data.frame(prevformstats$IRT_b, NA)) 
  targets <- cbind.data.frame(theta, TCC)
  # Establish the ranges for the b-value constraint. This stays the same for all forms, it changes between exams.
  bconst <- cbind.data.frame(names(poolranges), min = c(0,0, .00, .0, .05, .10, .05, 0, 0, 0,0), max = c(0, .03, .14, .40, .40, .40, .40, .40, .40, .014, 0))
  
  # Create list of enemies
  # Gets rid of those items with no enemies.
  eSub <- subset(enemy[,c("Item.ID","Enemy.Item.IDs")],!is.na(enemy$Enemy.Item.IDs))
  
  # Create a list of each item and its enemies.
  enemies<-list()
  for(i in 1:nrow(eSub))
  {
    enemies[[i]]<- c(eSub[i,1], as.numeric(unlist(strsplit(eSub[i,2], split=","))))
  }
  
  # Show object
  print(object_name)
  
  # Build forms
  form1 <- ata(pool, N=120, a = 2, enemies = enemies, propDomain = ae@propDomain, targets = targets, bconst = bconst, mediaconst = .35, PAtarget = 8)[[1]]
  form2 <- ata(pool[!pool$Item %in% form1$Item,], N=120, a = 2, enemies = enemies, propDomain = ae@propDomain, targets = targets, bconst = bconst, mediaconst = .35, PAtarget = 8)[[1]]
  
  result <- list(form1, form2)
  return(result)
}


# Update att and itemstats ----
update_att_itemstats <- function(pretest_list, att, itemstats, exam, form_build_year) {
  ## The operational item list is up to pretestWindow
  ## The current operational items in the Snowflake are up to 2020 only. 
  pretest_list <- pretest_list %>%
    filter(Window == ae2021@window) %>%
    mutate(Item = as.character(Item), Domain = as.character(Domain))
  
  # Load the list of scored items from 2 years ago. 
  FormA_Scored_Items <- result[[1]]
  FormB_Scored_Items <- result[[2]]
  FormAB_Scored_Items <- as.data.frame(rbind(FormA_Scored_Items, FormB_Scored_Items)) %>% mutate(Item = as.character(Item)) %>% unique()
  FormAB_Scored_Items <- FormAB_Scored_Items %>% mutate(Window = ae2021@window, Scored = 1, Form = ae2021@form) %>% unique()
  
  
  # Update attribute list (replace the 2021 and 2022 items by artificial pretest items)
  att <- att %>% 
    filter(Window != "AE2021AB_WIN")%>% 
    filter(Window != "AE2022AB_WIN")
  att_updated <- bind_rows(att, pretest_list, FormAB_Scored_Items) %>% 
    select(colnames(att)) %>%
    group_by(Item) %>%
    arrange(Window) %>%
    mutate_all(list(~na.locf(., na.rm = FALSE))) %>%
    distinct(Item, Window, .keep_all = T)
  att <- att_updated
  
  # Update item stats
  #To get the 2021 values, you need to use the Midpoint check or current IA
  itemstats <- itemstats %>% filter(Taker_Status == "First-Timers") %>% 
    mutate(Item = as.character(Item)) %>%
    select(Window, Item, Form_Build, Official_Stat, c(19:51))
  
  itemstats_updated <- bind_rows(itemstats, pretest_list, FormAB_Scored_Items)%>% 
    select(colnames(itemstats)) %>%
    group_by(Item) %>%
    arrange(Window) %>%
    mutate_all(list(~na.locf(., na.rm = FALSE))) %>%
    distinct(Item, Window, .keep_all = T)
  itemstats <- itemstats_updated
  
  write.csv(att, paste0(exam, "_att_updated_", form_build_year, ".csv"), row.names=FALSE)
  write.csv(itemstats, paste0(exam, "_itemstats_updated_", form_build_year, ".csv"), row.names=FALSE)
}
