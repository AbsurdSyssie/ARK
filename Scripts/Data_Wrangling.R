library(ggstatsplot)
library(tidytext)
library(stringr)
library(tidyverse)
Clean_Data %>%
  filter(grepl("*CRAMPS*", Reason.given)| grepl("COMMUNITY", Reason.given)) %>%
  print()


#find top n most frequently prescribed abx
pop_abx <- Clean_Data %>%
  count(Name) %>%
  top_n(10)

mean_rx_length <- Clean_Data %>%
   group_by(Reason.given) %>%
  summarise(mean_length = mean(Prescription_Length)) %>%
  arrange(-mean_length)


under_72 <- Clean_Data %>% 
  filter(Prescription_Length <= 72) %>% 
  count() %>% 
  print()

proportion_under_72 <- (under_72 / nrow(Clean_Data)) *100
 

Clean_Data %>%
 group_by(Reason.given) %>%
   summarise(Prescription_Length) %>% 
   arrange(-Prescription_Length) %>%
   print()
 
by_Reason <- Clean_Data%>%
  group_by(Reason.given) %>%
  select(NHS.,Reason.given,Name,Dose,Prescription_Length) %>%
  arrange(Reason.given) 

#find UTIs
UTI_Search <- c("*UTI* | *URINARY* | URINARY TRACT INFECTION | UTI/ | 'UTI\'")
UTIs <- data.frame(Clean_Data[grep(UTI_Search, Clean_Data$Reason.given),]) %>%
  group_by(Reason.given) %>%  
  mutate(Prescription.start = NULL, Prescription.end = NULL)

#find queries
#Queries <- 
Clean_Data %>%
  filter((grepl("\\?", Reason.given)))

Clean_Data %>%
  group_by(Name) %>%
  summarise( n()) %>%  
  arrange(n()) %>%
  print()

pop_indic <- Clean_Data %>%
  count(Reason.given) %>%
  top_n(10)

#standard diff  
Clean_Data%>%
  summarise(sd(Prescription_Length), mean(Prescription_Length))

  Clean_Data %>%
  group_by(Reason.given) %>%
  summarise(sd(Prescription_Length), mean(Prescription_Length)) %>%
  print()
  
str(Clean_Data$Name)
  #create table of prescription event
prescription_events <- data.frame(PID = Clean_Data$NHS., indication = char, rx_1 = char, rx_2 = char)


#get the agents lined up by their start times and find their frequency on that day
agent_by_date <- Clean_Data %>%
  group_by(Prescription.start) %>%
  count(Name) 
#then find their cumulative frequency over time
agent_by_date <- agent_by_date %>%
  group_by(Name) %>%
  mutate(Cum_n = cumsum(n)) 

#then select only the most commonly prescribed abx
pop_agent_by_date <- filter(agent_by_date, Name %in% pop_abx$Name) %>%
  print()

agent_by_reason <- Clean_Data %>%
  group_by(Reason.given) %>%
  count(Name)

agent_by_reason <- filter(agent_by_reason,Reason.given %in% pop_indic$Reason.given) %>%
  print()
#organise pop indic by date and count how many over time
indic_by_date <- Clean_Data %>% 
  group_by(Prescription.start) %>% 
  count(Reason.given)
#find most common indications
indic_by_date <- indic_by_date %>% 
  filter(Reason.given %in% pop_indic$Reason.given) 
#find totals over time  
indic_by_date <- indic_by_date  %>% 
  mutate(Reason.given = str_to_title(Reason.given)) %>% 
    group_by(Reason.given) %>% 
   mutate(cum_n = cumsum(n))

#line up all rows per patient for the same indication
abx_per_indication <- Clean_Data%>% 
 # filter(Prescription.start < 2022-02-01) %>%
  group_by(NHS., Reason.given) %>% 
  mutate(rn = row_number()) %>%
  pivot_wider(values_from = c(Name,Route,Prescription.start,Prescription.end, Dose, 
                              Frequency, ARK.category, Prescription_Length ),
              names_from =  rn)
#classify each change as per ARK protocol
abx_per_indication <- abx_per_indication %>% 
  mutate(Decision_1 = case_when(Route_1 =="Intravenous" & Route_2 =="Oral" ~ 'IV to Oral Switch',
                                Route_1 =="Oral" & Route_2 =="Intravenous"  ~ 'Oral to IV Switch',
                                Route_1 =="Intravenous" & Route_2 == "Intravenous" & is.na(Route_3)  ~ 'Finalised as IV',
                                Route_1 =="Oral" & Route_2 == "Oral" & is.na(Route_3)  ~ 'Finalised as Oral',
                                Route_1 =="Intravenous" & is.na(Route_2) ~ 'Stopped as IV',
                                Route_1 =="Oral" & is.na(Route_2)  ~ 'Stopped as Oral',
                                Route_1 == Route_2 & Name_1 == Name_2 & Dose_1 == Dose_2 & Frequency_1 == Frequency_2  ~ 'No Change',
                                Route_1 == Route_2 & Name_1 != Name_2  ~ ('Agent changed'),
                                Route_1 == Route_2 & Dose_1 != Dose_2 | Frequency_1 != Frequency_2 ~ ('Regiment changed'),
                                TRUE ~ 'something else'
  ))


abx_per_indication <- abx_per_indication %>% 
  mutate(Decision_2 = case_when(Route_2 =="Intravenous" & Route_3 =="Oral" ~ 'IV to Oral Switch',
                                Route_2 =="Oral" & Route_3 =="Intravenous"  ~ 'Oral to IV Switch',
                                Route_2 =="Intravenous" & Route_3 == "Intravenous" & is.na(Route_4)  ~ 'Finalised as IV',
                                Route_2 =="Oral" & Route_3 == "Oral" & is.na(Route_4)  ~ 'Finalised as Oral',
                                Route_2 =="Intravenous" & is.na(Route_3) ~ 'Stopped as IV',
                                Route_2 =="Oral" & is.na(Route_3)  ~ 'Stopped as Oral',
                                Route_2 == Route_3 & Name_2 == Name_3 & Dose_2 == Dose_3 & Frequency_2 == Frequency_3  ~ 'No Change',
                                Route_2 == Route_3 & Name_2 != Name_3  ~ 'Agent changed',
                                Decision_1 == "Stopped as Oral" | Decision_1 == "Stopped as IV" | Decision_1 == "" ~ "",
                                Route_2 == Route_3 & Dose_2 != Dose_3 | Frequency_2 != Frequency_3 ~ ('Regiment changed'),
                                TRUE ~ 'something else'
  ))

abx_per_indication <- abx_per_indication %>% 
  mutate(Decision_3 = case_when(Route_3 =="Intravenous" & Route_4 =="Oral" ~ 'IV to Oral Switch',
                                Route_3 =="Oral" & Route_4 =="Intravenous"  ~ 'Oral to IV Switch',
                                Route_3 =="Intravenous" & Route_4 == "Intravenous" & is.na(Route_3)  ~ 'Finalised as IV',
                                Route_3 =="Oral" & Route_4 == "Oral" & is.na(Route_3)  ~ 'Finalised as Oral',
                                Route_3 =="Intravenous" & is.na(Route_4) ~ 'Stopped as IV',
                                Route_3 =="Oral" & is.na(Route_4)  ~ 'Stopped as Oral',
                                Route_3 == Route_4 & Name_3 == Name_4 & Dose_3 == Dose_4 & Frequency_3 == Frequency_4  ~ 'No Change',
                                Route_3 == Route_4 & Name_3 != Name_4  ~ ('Agent changed'),
                                Route_3 == Route_4 & Dose_3 != Dose_4 | Frequency_3 != Frequency_4 ~ ('Regiment changed'),
                                Decision_2 == "Stopped as Oral" | Decision_2 == "Stopped as IV" | Decision_2 =="" ~ "",
                                TRUE ~ 'something else'
  ))


ark_results <- abx_per_indication %>% 
 select(c(Name_1,Route_1, Name_2,Route_2, Name_3,Route_3,Name_4,Route_4,Decision_1,Decision_2,Decision_3,ARK.category_1,ARK.category_2,ARK.category_3,ARK.category_4)) %>% 
  arrange(Name_4, Name_3, Name_2)

single_rx <- ark_results %>% 
  filter(is.na(Name_2)) %>% 
  select(c(Name_1,Route_1,Decision_1))

double_rx <- ark_results %>% 
  filter(is.na(Name_3)) %>% 
  select(c(Name_1,Route_1,Decision_1, Name_2,Route_2,Decision_2))

triple_rx <- ark_results %>% 
  filter(is.na(Name_4)) %>% 
  select(-c(Name_4,Route_4))

quad_rx <- ark_results %>% 
  filter(!is.na(Name_4))
