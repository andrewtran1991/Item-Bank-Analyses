

opitems <- openxlsx::read.xlsx("C:/ARDMS and Transition Exams/AB/AB2020A-B/1. Form Build (PV)/ToPearson/1. AB Operational Items w Stats.xlsx")
att <- get.itemattributes(exam)
result <- get.itemstatistics(exam)
itemStatistics <- subset(result)
itemStatistics <- itemStatistics %>% filter(Taker_Status=="First-Timers")

# Filter out windows after 2017
itemStatistics <- itemStatistics %>% 
  filter(Window!="AB2019AB_WIN") %>%
  filter(Window!="AB2020AB_WIN") %>%
  filter(Window!="AB2021AB_WIN") %>%
  filter(Window!="UNKNOWN")
  

# Calculate exposure of item
Temp <- subset(itemStatistics,Taker_Status=="First-Timers")
Temp <- Temp[order(Temp$Window),] %>% dplyr::group_by(Item) %>%
  dplyr::summarise(Total_Count = sum(Total_Count, na.rm = TRUE),
                   Window_Count = length(Window),
                   Previous_Windows = paste(substr(unlist(stringr::str_extract_all(Window, "20[0-9]{2}")),3,4),
                                            collapse = ","))
itemStatistics$Total_Count <- with(Temp, Total_Count[match(itemStatistics$Item,Item)])
itemStatistics <- merge(itemStatistics, Temp[,c("Item","Window_Count","Previous_Windows")],
                        by = "Item", all.x = TRUE)

itemStatistics <- itemStatistics %>% select(Item, Window, Window_Count, Previous_Windows, Comments) %>% mutate(Item = as.character(Item))

# Merge in the missing columns
opitems <- opitems %>% left_join(itemStatistics, by=c("Item", "Window")) 
