library(survival)
library(ggplot2)
library(ggfortify)


# Read in the clean data
dat.full.clean <- read_csv("dat.full.clean.csv") %>% arrange(Form, Item)
dat.full.clean$index <- 1:nrow(dat.full.clean)

# Transform data for survival analysis
# Create 2 datasets: last form and the rest
# For the rest forms, their current status should be 0. The last row is the one that has the most current status.
dat.last.row <- dat.full.clean %>%
  group_by(Item) %>%
  arrange(Form) %>%
  slice(n()) %>%
  mutate(last_row = 1)

dat.the.rest <- dat.full.clean %>%
  filter(!(index %in% dat.last.row$index)) %>%
  mutate(Current_status = 0) 
  
dat.full.clean2 <- dplyr::bind_rows(dat.the.rest, dat.last.row)

# Find the items that have multiple forms in one year
dat.full.clean2 <- dat.full.clean2 %>%
  group_by(Item) %>% 
  mutate(total_form = n()) %>%
  mutate(year_after = lead(Year)) # get the year after the current row
  
dat.full.clean2 <- dat.full.clean2 %>%
  group_by(Item, Year) %>% 
  mutate(n_form_1_year = n()) # count how many forms exist in one year

dat.full.clean2 <- dat.full.clean2 %>%
  group_by(Item) %>%
  mutate(year_after = if_else(is.na(year_after), Year+1, year_after))  %>%
  group_by(Item, Year) %>%
  mutate(year_after = max(year_after)) %>% # for item exists on 2 forms in one year, their year_after should be the same
  mutate(last_row = if_else(n_form_1_year==2, max(last_row, na.rm = T), last_row))  # for item exists on 2 forms in one year, their last_row value should be the same


# Calculate the length that the form exists
# We want to know how many years an item is available up to 2022: 
# if an item is retired/rejected before 2021, we take their last year; otherwise, 2022

dat.full.clean2 <- dat.full.clean2 %>%
  group_by(Item) %>%
  arrange(Form) %>%
  mutate(max_current_status = max(Current_status)) %>%
  mutate(max_year = if_else(last_row == 1 & max_current_status == 0, 2022, year_after)) %>%
  mutate(max_year = if_else(is.na(max_year), year_after, max_year)) %>%
  mutate(n_year = max_year-Year,
         n_form_csum = order(Form)) %>%
  group_by(Item, Year) %>%
  mutate(n_form_csum = max(n_form_csum))  


# 200037



# Survival Analysis (aggregated data) ----------------------------------------------------------------------------------------------------
# In this step, we apply different Survival Analysis models to find the probability of survival for each item
# We censor the data at 2021
# Event: 0 = operation/resting, 1 = retired/rework

# Create Survival Object
# Use Suv() syntax for right-censored data
survobj <- Surv(time = Usage_rate_full_df$n_year,
                event = Usage_rate_full_df$Current_status_recode)

#print the 10 first elements of the vector to see how it presents
head(survobj, 20)

# fit the KM estimates using a formula where the Surv object "survobj" is the response variable.
# "~ 1" signifies that we run the model for the overall survival  
surv_fit <-  survival::survfit(survobj ~ 1)

#print its summary at specific times
summary(surv_fit, times = c(1,2,3,4,5,6))

# The summary of this survfit object will give what is called a life table. For each time step of the follow-up (time) where an event happened (in ascending order):
# 1. n.risk: the number of people who were at risk of developing the event (people who did not have the event yet nor were censored)
# 2. n.event: those who did develop the event 
# 3. survival: the probability of not developing the event (probability of not dying, or of surviving past that specific time)
# 4. std.err and CI: the standard error and the confidence interval for that probability are derived and displayed


# Plotting Kaplan-Meir curves
plot(surv_fit, 
     xlab = "Years of follow-up",    # x-axis label
     ylab="Survival Probability",   # y-axis label
     main= "Overall survival curve" # figure title
)
axis(1,at=c(0,1,2,3,4,5,6))



# Time-dependent covariates in the Cox proportional hazard analysis
surv_cox_model1 <-
  coxph(formula = Surv(Year, max_year, max_current_status) ~ n_form_csum + total_candidate  + n_form_1_year +
          Item_Type_HotSpot + PValue  + PBis+
          Corrected_PBis + IRT_b + Mean_Time_Seconds + Is_New + Has_Image + Total_Flag + 
          cluster(Item), 
        data    = dat.full.clean2,
        ties    = c("efron","breslow","exact")[1])

summary(surv_cox_model1)

ggforest(surv_cox_model1, data = dat.full.clean2)


