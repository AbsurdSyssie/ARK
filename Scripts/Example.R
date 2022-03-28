
example_data <- data.frame(ID = c("1","1","2","2"), Start_Time = c("2022-03-05 07:00","2022-03-06 07:00", "2022-03-07 09:00", "2022-03-07 15:00"),
                           End_Time = c("2022-03-06 07:00","2022-03-07 16:00", "2022-03-07 14:30", "2022-03-08 15:00"), 
                           Name = c("Amoxicillin","Flucloxicillin","Clarithromycin","Benzylpenicillin"),
                           Dose = c("500mg","1g","1g","4.5g"),
                           Freq = c("Four times a day","Four times a day" ,"Three times a day", "Three times a day"), 
                           Route = c("IV","Oral", "Oral", "IV"),
                           Reason = c("Sepsis","Cellulitis", "CAP", "CAP"))

example_data2 <-data.frame(ID = c("1","1", "2"), 
                           Start_Time1 = c("2022-03-05 07:00", "2022-03-06 07:00","2022-03-07 09:00"), 
                           End_Time1 = c("2022-03-06 07:00", "2202-03-07 16:00", "2022-03-07 14:30"), 
                           Name1 = c("Amoxicillin","Flucloxicillin","Clarithromycin"),
                           Dose1= c("500mg","1g","1g"),
                           Freq1 = c("Four times a day","Four times a day" ,"Three times a day"),
                           Route1 = c("IV","Oral", "Oral"),
                           Reason = c("Sepsis","Cellulitis", "CAP"),
                           
                           Start_Time2 = c("NA", "NA","2022-03-07 09:00"), 
                           End_Time2 = c("NA", "NA", "2022-03-07 14:30"), 
                           Name2 = c("NA","NA","Benzylpenicillin"),
                           Dose2= c("NA","NA","1g"),
                           Freq2 = c("NA","NA" ,"Three times a day"),
                           Route2 = c("NA","NA", "IV")
                           
                           )
example_data %>% 
  group_by(ID, Reason) %>% 
  mutate(rn = row_number()) %>%
  pivot_wider(values_from = c(Name, Dose,Freq,Start_Time,End_Time,Route ), names_from =  rn
            )


write.csv(example_data2, "Clean_Data\\example_2.csv")

write.csv(example_data, "Clean_Data\\example_1.csv")

#if_else(abx_per_indication$Route_1 == "Oral" & abx_per_indication$Route_2 == "Intravenous", print("Oral to IV Switch"),
        if_else(abx_per_indication$Route_1 == "Oral" & abx_per_indication$Route_2 == "" | abx_per_indication$Route_2 == "Oral", print("Finalised as Oral"),
        if_else(abx_per_indication$Route_1 == "IV" & abx_per_indication$Route_2 == "IV" | abx_per_indication$Route_2 == "", print("Finalised To IV"),
                if_else(abx_per_indication$Route_1 == "IV" & abx_per_indication$Route_2 == "Oral", print("IV to Oral Switch"), print("Something else"))
        
        )))

ifelse(abx_per_indication$Route_1 == "Oral" & abx_per_indication$Route_2 == "",mutate(abx_per_indication, Decision_1 = "Finalised to Oral"),print("Hi") )


abx_per_indication <- abx_per_indication %>% 
  mutate(Decision_1 = case_when(Route_1 =="Intravenous" & Route_2 =="Oral" ~ 'IV to Oral Switch',
                                Route_1 =="Oral" & Route_2 =="Intravenous"  ~ 'Oral to IV Switch',
                                Route_1 =="Intravenous" & Route_2 == "Intravenous" & is.na(Route_3)  ~ 'Finalised as IV',
                                Route_1 =="Oral" & Route_2 == "Oral" & is.na(Route_3)  ~ 'Finalised as Oral',
                                Route_1 =="Intravenous" & is.na(Route_2) ~ 'Stopped as IV',
                                Route_1 =="Oral" & is.na(Route_2)  ~ 'Stopped as Oral',
                                TRUE ~ 'something else'
  ))

print(abx_per_indication$Route_2, na.rm =TRUE)
  
