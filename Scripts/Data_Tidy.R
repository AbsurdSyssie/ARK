library(tidyverse)
library(dplyr)
library(lubridate)
library(stringr)

options(scipen = 999) #make it so numbers are not displayed as exponents
#load data from CSV source to a df
raw_data <- read.csv(file = "RAW_DATA\\Data Extract.csv")

Clean_Data <- raw_data 

#make all indications same case to reduce number of duplicates

Clean_Data$Reason.given <-str_to_upper(Clean_Data$Reason.given, locale ="en") 

#remove duplicates
Clean_Data <- Clean_Data[!duplicated(Clean_Data), ]

#change NHS# into an arbitrary Patient ID No
PID <- Clean_Data %>%                                        # Create ID by group
  group_by(NHS.) %>%
  mutate(ID = cur_group_id())

Clean_Data$NHS. <- PID$ID

#make columns dates not characters
Clean_Data$Prescription.start <- dmy_hm(Clean_Data$Prescription.start) 
Clean_Data$Prescription.end <- dmy_hm(Clean_Data$Prescription.end)
Clean_Data$Prescription_Length <- interval(Clean_Data$Prescription.start, Clean_Data$Prescription.end) / hours(1)

#remove prescriptions with no indication
Clean_Data <- Clean_Data %>%
  filter(Reason.given != "") %>%
  print()


#remove prescriptions before Dec 1st and ending after 2022
Clean_Data <- Clean_Data %>%
  filter(!Prescription.start <= as.POSIXct.Date("2021-12-01") )

| Prescription.end > as.POSIXct.Date("2022-12-31"))

#find all the prescriptions which were over 72 hours long

#count the number of prescriptions that were over 72 hours


#find the percentage of prescriptions which were overdue

#find all distinct indications for an abx
data.frame(indications = c(distinct(Clean_Data,Clean_Data$Reason.given))) %>%
print(indications)

indications <- distinct(Clean_Data ,Clean_Data$Reason.given)



#tidy up all indications
  
  #gsub(old,new,data)
  
  #make all UTIs display as Urinary Tract Infection
  Clean_Data$Reason.given <- gsub( "UTI", "URINARY TRACT INFECTION",Clean_Data$Reason.given) %>%
    print()
  
  #make all community acquired pneumonia display as CAP
  Clean_Data$Reason.given <- gsub("CAP", "COMMUNITY ACQUIRED PNEUMONIA", Clean_Data$Reason.given) %>%
    print()
  #make all IEoCOPD as IECOPD
  Clean_Data$Reason.given <- gsub("IECOPD", "INFECTIVE EXACERBATION OF CHRONIC OBSTRUCTIVE PULMONARY DISEASE", Clean_Data$Reason.given) %>%
    print()
  
  #make all LRTI full name
  Clean_Data$Reason.given <- gsub("LRTI", "LOWER RESPIRATORY TRACT INFECTION", Clean_Data$Reason.given) %>%
    print()
  
  #correct common spelling mistakes
  Clean_Data$Reason.given <- gsub("PYONEPHRITIS", "PYELONEPHRITIS", Clean_Data$Reason.given) %>%
    print()         
  
  Clean_Data$Reason.given <- gsub("PYENOPHRENITIS", "PYELONEPHRITIS", Clean_Data$Reason.given) %>%
    print()
  
 Clean_Data <- Clean_Data %>% 
  mutate(Reason.given = gsub("C.DIFFICLE", "Clostridium Difficile", Clean_Data$Reason.given)) %>% 
    print()
 Clean_Data <- Clean_Data %>% 
   mutate(Reason.given = sub("C.DIFF", "Clostridium Difficile", Clean_Data$Reason.given)) %>% 
   print()
 Clean_Data <- Clean_Data %>% 
   mutate(Reason.given = sub("CLOSTRIDIUM DIFFICILE*", "Clostridium Difficile", Clean_Data$Reason.given)) %>% 
   print()
  
  #remove any stat, bleed, prophylactic abx, leg cramps, hepatic encephalo
  Clean_Data <- Clean_Data %>%
    filter(!(grepl("*BLEED*", Reason.given) | grepl("*PROPHY*", Reason.given) | grepl("*CRAMP*", Reason.given) | 
               grepl("MS", Reason.given) | grepl("*ONCE*", Frequency) | grepl("H.ENCEPH*", Reason.given) | grepl("*HEPATIC ENCEPHALOPATHY*", Reason.given)))  %>%
    print()

#remove any rows where indication is NA
Clean_Data <- Clean_Data %>%
  na.omit(Reason.given) 
  


  #Sometimes antibiotic treatment is stopped and then restarted within a short time. 
  # If the prescriptions are for the same indication and within 24 hours 
  #(gap of <24 hours between the last dose of original antibiotic and the first dose of new antibiotic) 
  #then they should be considered as the same course. 

#something like if prescription_length <24 & reason.given == reasongiven+1
  
short_rx <- Clean_Data %>%
  filter(Prescription_Length<=24 & Reason.given == Reason.given[+1]) %>%
  print()

merged_Rx <- Clean_Data %>%
  if (Prescription_Length <= 24){
    merged_Rx$Prescription.end <- merged_Rx$Prescription.end[+1] 
      } %>%
      print()
Clean_Data %>%
  #arrange(Pt_ID, desc = FALSE) %>%
  #filter(Pt_ID == Pt_ID[+1]) %>%
  subset(Clean_Data$Pt_ID == Clean_Data$Pt_ID[+1]) %>%
  print()

  

#import / export clean data as csv
Clean_Data %>% write.csv("CLEAN_DATA\\Clean_data.csv", row.names = FALSE)
Clean_Data <- read.csv("CLEAN_DATA\\Clean_data.csv")
  
