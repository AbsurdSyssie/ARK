library(ggplot2)
library(ggthemes)
library(tidyverse)
freq_abx <- Clean_Data %>%
  group_by(Name) %>%
  count(Name)%>%
  print()

freq_abx <- as.data.frame(freq_abx)  
ggplot(Clean_Data, aes(fill=Reason.given, y=freq_abx, x=Reason.given)) +
  geom_bar(position='stack', stat='identity') 

barplot(Clean_Data$Name)
rank_abx <- as.data.frame(sort(table(freq_abx),decreasing = TRUE)) %>%
  print()
barplot(table(freq_abx))
  print()


arrange(freq_abx$n, decreasing = TRUE) %>%
  print()

barplot(table(Clean_Data$Name, Clean_Data$Reason.given)) +
  scale_x_discrete(guide = guide_axis(n.dodge=3))  

Clean_Data <- read.csv("CLEAN_DATA\\Clean_data.csv")
Clean_Data <- select(Clean_Data,-X)

pie_data <- sort(freq_abx$n, decreasing = TRUE) %>%
                     print()
ggplot(pie_data)


#top ten prescriptions
ggplot(data=pop_abx, aes(y=reorder(Name, -n), x = n)) +
# if wanting abx name on x axis- scale_x_discrete(guide = guide_axis(n.dodge=3)) +
  geom_bar(stat = "identity", fill="Steel Blue", colour="black") +
  xlab("Prescriptions on Lilac | 06-Dec-21 : 07-Mar-21") + ylab("Antibiotic") 
#top ten indications
pop_indic <- Clean_Data %>%
  count(Reason.given) %>%
  top_n(5)
#make graph of top ten indications
ggplot(pop_indic, aes(x =reorder(Reason.given, -n), y = n)) +
  scale_x_discrete(guide = guide_axis(n.dodge=6)) +
  geom_bar(stat = "identity")

#line graph?
ggplot(pop_agent_by_date, aes(x=Prescription.start, y=Cum_n, group = Name, color = Name)) +
  geom_smooth()
ggsave("test.pdf", height = 9, width = 18, units ="in", dpi = 400)

#indications over time
indic_over_time_line <- ggplot(indic_by_date, aes(x = Prescription.start, y = cum_n, group = Reason.given, colour = Reason.given)) +
  geom_smooth()+
  ylim(0,700)+
  xlim(as.Date(c("2021-12-06","2022-04-01"))) +
  scale_color_discrete("Indication", labels = c("Community Acquired Pneumonia" = "CAP",
                                                                  "Infective Exacerbation Of Chronic Obstructive Pulmonary Disease" = "IECOPD",
                                                                  "Lower Respiratory Tract Infection" = "LRTI",
                                                                  "Urinary Tract Infection" = "UTI",
                                                                  "Healthcare Associated Pneumonia" = "HAP")) +
  labs(x = "Date" , y = "Cumulative Prescriptions")

Clean_Data %>%
  ggplot() + 
  geom_bar(aes(y = n, x = pop_indic, fill = Name),stat="identity")

#stacked bar graph, abx by indic
Abx_indic_stacked <- agent_by_reason %>%
  filter(Name %in% pop_abx$Name) %>%
  mutate(Reason.given = str_to_title(Reason.given)) %>%
  mutate(Name = str_to_title(Name)) 

Abx_indic_stacked %>% 
ggplot(aes(x = reorder(Reason.given, -n), y = n, fill =Name)) +
  theme(axis.text.x = element_text(angle = 15, vjust = 0.6)) +
  geom_bar(position='stack', stat='identity') +
  labs(x= "Indication", y= "No of Prescriptions", title="Proportion of Antibiotics by Indication") +
  scale_x_discrete("Indication", labels = c("Community Acquired Pneumonia" = "CAP",
      "Infective Exacerbation Of Chronic Obstructive Pulmonary Disease" = "IECOPD",
       "Lower Respiratory Tract Infection" = "LRTI",
        "Urinary Tract Infection" = "UTI",
      "Healthcare Associated Pneumonia" = "HAP"))


Clean_Data %>%
  select(NHS., Prescription.start,Name, Route, Reason.given,  ) %>%
  arrange(Prescription.start, descending = FALSE) %>%
  head()
my_plot <- (Abx_indic_stacked + indic_over_time_line)
