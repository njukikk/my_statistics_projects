#================================================================================================#
#-------Kelvin Njuki                                                                             #
#-------STA 504 A                                                                                #
#-------Advanced Data Visualization                                                              #
#-------Graduate Project                                                                         #
#================================================================================================#

#Loading all necessary packages
library(tidyverse)
library(plotly)
library(stringr)

#Setting working directory
setwd("E:/Miami/ClassWork/Spring2020/STA 504/Graduate Project")
#================================================================================================#
#------------------------------------------DATA HANDLING-----------------------------------------#
#================================================================================================#

#Reading in data
covid_trts_vaccines_raw <- read_csv("COVID-19 Treatment and Vaccine Tracker - Treatments and Vaccines..csv",
                                   col_names = T,skip = 2,skip_empty_rows = TRUE)
#Renaming column names
colnames(covid_trts_vaccines_raw) <- c("none","none1","Type","Category","description",
                                   "Stage","next_steps","clinical_trials",
                                   "Developer_Researcher","funder","published_results",
                                   "clinical_trials_for_other_diseases",
                                   "FDA_approved_indications","sources","date_last_updated",
                                   "none2")
#Data cleaning and aggregation
covid_trts_vaccines <- covid_trts_vaccines_raw %>%
  select(Type,Category,Stage,Developer_Researcher) %>% #Selecting variables of interest
  #Replacing long character string with equivalent shorter meaningful character strings
  mutate(Stage=str_replace_all(Stage,c(
  "FDA issued an Emergency Use Authorization on May 1, 2020"= "Emergency Use",
  "FDA issued an Emergency Use Authorization on April 10, 2020"= "Emergency Use",
  "FDA issued an Emergency Use Authorization on April 9, 2020"= "Emergency Use", 
  "FDA issued an Emergency Use Authorization on April 17, 2020"= "Emergency Use",
  "FDA issued an Emergency Use Authorization on April 23, 2020"= "Emergency Use",                               
  "FDA issued an Emergency Use Authorization on April 24, 2020"= "Emergency Use",
  "FDA issued an Emergency Use Authorization on April 30, 2020"= "Emergency Use",
  "Expanded access"="Expanded Access",
  "Clinical/ Compassionate Use \n" = "Clinical / Compassionate Use",  
  "Clinical/ Compassionate Use\n" = "Clinical / Compassionate Use",
  "Clinical / Expanded Access" ="Clinical / Compassionate Use",
  "Clinical / Expanded Access / Emergency Use"="Clinical / Emergency Use",
  "Clinical/ Compassionate Use/ Expanded Access"="Clinical / Compassionate Use",
  "Emergency Use / Clinical / Compassionate Use"="Clinical / Emergency Use",
  "Clinical/ Expanded Access"="Clinical / Compassionate Use",
  "Compassionate Use/ Clinical"="Clinical / Compassionate Use",
  "Clinical/ Emergency Use"="Clinical / Emergency Use",
  "Clinical / Compassionate Use / Emergency Use"="Clinical / Emergency Use",
  "Pre-clincial"="Pre-clinical",
  "Pre-clinical\n" = "Pre-clinical",
  "Emergency Use / Clinical"="Clinical / Emergency Use",
  "Expanded Access"="Compassionate Use"
  ))) %>%
  mutate(Developer=str_extract(Developer_Researcher, 
                               "University|Hospital|Institute|Therapeutics|
                               Pharmaceuticals")) %>% #Extracting common names 
  mutate(Developer=replace_na(Developer,"Others"))%>%
  select(-Developer_Researcher) %>%
  mutate(Developer=str_replace_all(Developer, c("University"="Universities",
                                                "Hospital"="Hospitals",
                                                "Institute"="Institutes"))) %>%
  na.omit()

#Subsetting data to treaments data and vaccines data
covid_treatments <- covid_trts_vaccines %>%
filter(Type=="Treatment")
covid_vaccines <- covid_trts_vaccines %>%
  filter(Type=="Vaccine")

#================================================================================================#
#--------------------------SAVING PROCESSED DATA IN RData Format---------------------------------#
#================================================================================================#

save(covid_treatments,file="covid_treatments.RData")
save(covid_vaccines,file="covid_vaccines.RData")

#================================================================================================#
#---------------------------------------GENERATING PLOTS-----------------------------------------#
#================================================================================================#

#Visualizing Treatments Categories versus Stage
trts_plots1 <- ggplot()+
  geom_bar(aes(x=Category, fill=Stage), 
           stat="count",position="stack",
           data=covid_treatments)+
  labs(x="Treatment Category",y="Counts") +
  ggtitle(paste("Distribution of Treatments Categories versus Stage"))+
  theme_bw()+
  coord_flip()
ggplotly(trts_plots1)

#Visualizing Treatments Categories versus Developers
trts_plots2 <- ggplot()+
  geom_bar(aes(x=Category, fill=Developer), 
           stat="count",position="stack",
           data=covid_treatments)+
  labs(x="Treatment Category",y="Counts") +
  ggtitle(paste("Distribution of Treatments Categories versus Developers")) +
  theme_bw()+
  coord_flip()
ggplotly(trts_plots2)

#================================================================================================#

#Visualizing Vaccines Categories versus Stage
vaccines_plots1 <- ggplot()+
  geom_bar(aes(x=Category, fill=Stage), 
           stat="count",position="stack",
           data=covid_vaccines)+
  labs(x="Vaccine Category",y="Counts") +
  ggtitle(paste("Distribution of Vaccines Categories versus Stage"))+
  theme_bw()+
  coord_flip()
ggplotly(vaccines_plots1)

#Visualizing Vaccines Categories versus Developers
vaccines_plots2 <- ggplot()+
  geom_bar(aes(x=Category, fill=Developer), 
           stat="count",position="stack",
           data=covid_vaccines)+
  labs(x="Vaccine Category",y="Counts") +
  ggtitle(paste("Distribution of Vaccines Categories versus Stage"))+
  theme_bw()+
  coord_flip()
ggplotly(vaccines_plots2)

#================================================================================================#

