
example_data <- data.frame(ID = c("1","1","2","2"), Start_Time = c("2022-03-05 07:00","2022-03-06 07:00", "2022-03-07 09:00", "2022-03-07 15:00"),
                           End_Time = c("2022-03-06 07:00","2022-03-07 16:00", "2022-03-07 14:30", "2022-03-08 15:00"), 
                           Name = c("Amoxicillin","Flucloxicillin","Clarithromycin","Benzylpenicillin"), Dose = c("500mg","1g","1g","4.5g"),
                           Freq = c("Four times a day","Four times a day" ,"Three times a day", "Three times a day"), 
                           Route = c("IV","Oral", "Oral", "IV"),
                           Reason = c("Sepsis","Cellulitis", "CAP", "CAP"))

write.csv(example_data, "Clean_Data\\example_data.csv")

example_data2 <-data.frame(ID = c("1","1", "2"), 
                           Start_Time1 = c("2022-03-05 07:00", "2022-03-06 07:00","2022-03-07 09:00"), 
                           End_Time1 = c("2022-03-06 07:00", "2202-03-07 16:00", "2022-03-07 14:30"), 
                           Name1 = c("Amoxicillin","Flucloxicillin","Clarithromycin"),
                           Dose1= c("500mg","1g","1g"),
                           Freq1 = c("Four times a day","Four times a day" ,"Three times a day"),
                           Route1 = c("IV","Oral", "Oral"),
                           Reason1 = c("Sepsis","Cellulitis", "CAP"))
