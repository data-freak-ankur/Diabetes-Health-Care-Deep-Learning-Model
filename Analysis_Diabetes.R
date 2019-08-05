# Fetching Data from Database
#Connecting to Database
install.packages("RODBC")
library(RODBC)

#Setting the Database connection
#1st need to create DSN Chhabra from the Administrative Tools
con <- odbcConnect(dsn = 'Chhabra')
Dataset_Raw <- sqlQuery(con, 'select * from diabetic_data_backup')

#Now this is our Dataset
#Playing with Data
nrow(Dataset_Raw)
ncol(Dataset_Raw)
str(Dataset_Raw)
summary(Dataset_Raw)

# DATA PREPROCESSING & CLEANING

library(tidyverse)
library(corrr)

# Removing some of the columns that are not statistically significant for our analysis and contain lot of missing values
#Removing Weight Column , Payer code Column : Reason - Missing Values
# Weight Column - 97% Data Missing, Payer Code - 52%
Dataset <- Dataset_Raw %>%
  select(everything()) %>% 
  .[,c(-6,-11)] # or you can use, select with - columns


glimpse(Dataset)
# Checking the columns in our New Data set 
ncol(Dataset)

#As per Data - column Medical speciality comprises of 53% Missing Data. Since this column is statistically significant
# we need to deal with Missing Data
#Checking the characteristic of this column
str(Dataset$medical_specialty)
summary(Dataset$medical_specialty) # It comprises of 49,949 MIssing Values with Value ?

#Adding the Value Missing for these Question Marks. Using recode finction in dplyr Package
#1st Method
#This is the Best Method as by this method it remains as factor only
Dataset = Dataset %>%
  mutate(medical_specialty = recode(medical_specialty,'?'='Missing'))

#Rechecking this column
summary(Dataset$medical_specialty) #This field is as Expected now

#Looking at the Anomlies column wise
#Column 1 - Patient Number
summary(Dataset$patient_nbr)
str(Dataset$patient_nbr)
#Checking if this column has duplicate value, 
#there might be a number of Encounter Ids or records of a single Patient
#This can be done by DPLYR and here is the method
Duplication_check_new = Dataset %>%
  group_by(patient_nbr)%>%
  summarise(count=n())%>%
  filter(count>1)

#Ordering the Dataset with Patient Numbers to find the duplicacy. This will help us to find - which records to filter out

Dataset <- Dataset %>%
  arrange(encounter_id)%>%
  arrange(patient_nbr)

# We know that medical history should have unique key as patient_id and it should only be one.
# So it is required to filter data and select unique encounter id based on following conditions:
# 1)Considering more than 2 readmissions across multiple encounters as readmission for collapsed record.
# 2)Considering average stay at hospital across multiple encounters.
# 3)Considering the percentage of the medication changes across multiple encounters
# 4)Considering the total number of the encounters to replace the encounter unique ID
# 5)Considering the combination of diagnoses across multiple encounters as a list

# We will initially continue our analysis and will rectify this column in the end to avoid issues,
# so that we will not miss out any important data info

#Data is looking beutiful now. Let us continue our further Analysis
str(Dataset) # Seeing it there are still ? present in 4 columns - diag_1,diag_2,diag_3 & Race
# Before teh Analyze part, this data anomlies should be sort out


Dataset = Dataset %>%
  mutate(diag_1 = recode(diag_1,'?'='Missing'))%>%
  mutate(diag_2 = recode(diag_2,'?'='Missing'))%>%
  mutate(diag_3 = recode(diag_3,'?'='Missing'))%>%
  mutate(race = recode(race,'?'='Missing'))


#Looking at the data again
str(Dataset)

# one more thing that drew our attention.
# The gender data has 3 categories - Male, female and unknown.
# There are only 3 rows for this unknown and creating 1 extra factor which might 
#create issue of Dummy Variable Trap during deep learning, so removing these 3 rows

Dataset = Dataset %>%
  select(everything())%>%
  filter(gender !='Unknown/Invalid')%>%
  droplevels()

str(Dataset$gender)
summary(Dataset$gender)
# DATA PRE PROCESSING IS COMPLETED 
# -------------------------------------------------------------------------------------

# Exploratory Data Analysis

#1st Column : Patient Number : Nothing to do
#2nd Column : Gender :  Nothing to do
#3rd Column : Race

#PAtients Race Analysis with Gender
#Categorical Variable with 6 levels
#PAtient Admission on the base of their race subdevided to Males and Females
library(ggplot2)
plot1 <- ggplot(data = Dataset,aes(x=race,y=..count..,fill=gender))

plot1 + geom_bar(position=position_dodge()) + 
  geom_text(stat = 'count',aes(label=..count..),position=position_dodge(.9))+
  xlab("Patient's Race") + ylab("Number of Patients")+
  ggtitle("Patients Race Analysis with Gender")+
  theme(axis.title.x = element_text(colour 
                                    = "DarkGreen",size = 30),
        axis.title.y = element_text(colour = "DarkGreen",size = 30),
        axis.text = element_text(size = 20),
        plot.title = element_text(colour = "Red",size = 40,
                                  face = "bold.italic",hjust = .5))

# It seems like most of the patients n whom this analysis are carried on from the Caucasian Race
#with almost equalvent ratio of Males and Females


#4thColumn : Age

#Patient Analysis on base of Ages
#Find out what ages this analysis have been carried out

plot2 <- ggplot(data = Dataset,aes(x=age,fill=gender))
plot2 + geom_histogram(binwidth = 10,stat = "count")+
  xlab("Patient's Age") + ylab("Number of Patients")+
  ggtitle("Patients Age Analysis with Gender") + 
  theme(axis.title.x = element_text(colour 
                                    = "DarkGreen",size = 30),
        axis.title.y = element_text(colour = "DarkGreen",size = 30),
        axis.text = element_text(size = 20),
        plot.title = element_text(colour = "Red",size = 40,
                                  face = "bold.italic",hjust = .5))

#Most of the patients present for the study are in the age group 60-70 and 70-80. 
#The data is negatively skewed and it seems like this is the perfect data as diabetes 
#and hyperglycaemia are mostly present in higher age group say - above 40.

#Let us see what our data says about this age consideration
plot3 <- ggplot(data = Dataset,aes(x=age,fill=A1Cresult))
plot3 + geom_histogram(binwidth = 10,stat = "count")+
  xlab("Patient's Age") + ylab("Number of Patients")+
  ggtitle("Patients Age Analysis with Hyperglycemia Result") + 
  theme(axis.title.x = element_text(colour 
                                    = "DarkGreen",size = 30),
        axis.title.y = element_text(colour = "DarkGreen",size = 30),
        axis.text = element_text(size = 20),
        plot.title = element_text(colour = "Red",size = 40,
                                  face = "bold.italic",hjust = .5))


#The Analysis perfectly shows Ages above 40 has more chances of having diabetes but 
#however it shows an important insight that for a large number of patients - this 
#test was not conducted, which clearly shows that protocol driven inpatient strategies 
#are not there in the hospital and these should be there to prevent further readmission 
#costs.

#But, before that - we need to analyse that - the cases where this test is not 
#conducted are only the non ICU cases or the protocols are not even followed for the 
#ICU patients:

#Let us see what data tells us in that case:
#This can be visible by three columns namely admission_type_id , admission_source_id and 
#discharge_disposition_id.

#Here the following assumption as made as per feature engineering and experience...

# Also, one more cleaning step that depends on understanding the data and some common sense: 
# since we are trying to predict readmissions, those patients who died during this hospital 
# admission, have zero probability of readmission. So we should remove those records

Dataset = Dataset %>%
  filter(discharge_disposition_id != 11)
#The patients, who are admitted with the descriptions as Emergency, Urgent or Trauma Centre
#are considered to be the patients in ICU.

#Similarly , admission source ID for patients with description - Transfer from a 
#hospital, Emergency, critical access, premature delivery , surgery centre , hospice are 
#considered as ICU patients. Based on this data, a column ICU is created, 
#which make these 3 columns ambiguous as a more statistically important column will 
#replace these.

Dataset = Dataset %>%
  mutate(admission_type_id = ifelse(admission_type_id %in% c(1,2,7),
                                    "ICU Patient","Non ICU Patient"))%>%
  mutate(admission_source_id = ifelse(admission_source_id %in% c(4,7,10,12,26),
                                    "ICU Patient","Non ICU Patient"))%>%
  mutate(discharge_disposition_id = ifelse(discharge_disposition_id %in% c(11,13,14,19,20,21),
                                    "ICU Patient","Non ICU Patient"))

# Here we know that admission type ID is least significant data out of 3 as here we have 
#taken emergency.Its not necessary that all 
#emergency patients are kept in ICU because all the patients who are admitted in the 
#non working hours of the hospital are admitted under this category

# let us check our assumptions by equating these columns and making them as a 
#single column by replacing the non matching values to ICU patient as 
#Type II errors are more dangerous

Dataset = Dataset %>%
  mutate(Inpatient_ICU_Statistics = 
           ifelse(admission_type_id == discharge_disposition_id &
                    admission_source_id == discharge_disposition_id,
                    admission_source_id,ifelse(admission_source_id == 
                                               discharge_disposition_id,
                  admission_source_id,admission_source_id)))

# Let us Analyze our new column and since we have this new column so deleting
# the 3 redundant columns

str(Dataset$Inpatient_ICU_Statistics)

# This is a Character column. So we have to change it to factor

Dataset = Dataset %>%
  select(-admission_type_id,-admission_source_id,-discharge_disposition_id) %>%
  mutate(Inpatient_ICU_Statistics = as.factor(Inpatient_ICU_Statistics))

# Rechecking the column for Factors
str(Dataset$Inpatient_ICU_Statistics)
# This is now as expected

#Analysis of Test over ICU and Non ICU Patients

plot4 <- ggplot(data = Dataset,aes(x=A1Cresult,fill=Inpatient_ICU_Statistics))
plot4 + geom_histogram(binwidth = 10,stat = "count")+
  xlab("Diabetes AIC Results") + ylab("Number of Patients")+
  ggtitle("Protocol Analysis with Hyperglycemia Result") + 
  theme(axis.title.x = element_text(colour 
                                    = "DarkGreen",size = 30),
        axis.title.y = element_text(colour = "DarkGreen",size = 30),
        axis.text = element_text(size = 20),
        plot.title = element_text(colour = "Red",size = 40,
                                  face = "bold.italic",hjust = .5))

#What result shows that, the assumption is absolutely correct, for most of the NON ICU 
#Patients, this Hyperglycaemia test was not conducted, however the analysis raise further 
#questions that protocol was not followed for some of the ICU Patients as well.

#Now let us further Analyze this Data.

# Service utilization: The data contains variables for number of inpatient (admissions), 
# emergency room visits and outpatient visits for a given patient in the previous one year. 
# These are (crude) measures of how much hospital/clinic services a person has used in the 
# past year. We added these three to create a new variable called service utilization 
# The idea was to see which version gives us better results. 
# Granted, we did not apply any special weighting to the three ingredients of service 
# utilization but we wanted to try something simple at this stage.

Dataset = Dataset %>%
  mutate(Service_Utilization = number_inpatient + number_outpatient + number_emergency)
                   
# Deleting the 3 obselete columns now as this data is present in Service Utilization Column

Dataset = Dataset %>%
  select(-number_inpatient,-number_outpatient,-number_emergency)

# Number of medication changes: The dataset contains 23 features for 23 drugs (or combos) 
# which indicate for each of these, whether a change in that medication was made or not 
# during the current hospital stay of patient. Medication change for diabetics upon admission 
# has been shown by previous research to be associated with lower readmission rates. 
# We decided to count how many changes were made in total for each patient, and declared 
# that a new feature. The reasoning here was to both simplify the model and possibly discover 
# a relationship with number of changes regardless of which drug was changed.

keys = c('metformin', 'repaglinide', 'nateglinide', 'chlorpropamide', 'glimepiride', 
         'glipizide', 'glyburide', 'pioglitazone', 'rosiglitazone', 'acarbose', 'miglitol', 
         'insulin', 'glyburide-metformin', 'tolazamide', 'metformin-pioglitazone',
         'metformin-rosiglitazone', 'glimepiride-pioglitazone', 'glipizide-metformin', 
         'troglitazone', 'tolbutamide', 'acetohexamide','examide','citoglipton')

Dataset = Dataset %>% 
  mutate_at(keys, ~case_when(. %in% c("No", "Steady") ~ 0, TRUE ~ 1))
# Create a new column by name num_med_change with the sum of all these columns

Dataset = Dataset %>%
  mutate(num_med_change = metformin + repaglinide + nateglinide + chlorpropamide + glimepiride + 
         glipizide + glyburide + pioglitazone + rosiglitazone + acarbose + miglitol + 
         insulin + `glyburide-metformin` + tolazamide + `metformin-pioglitazone` +
         `metformin-rosiglitazone` + glimepiride-pioglitazone + glipizide-metformin + 
         troglitazone + tolbutamide + acetohexamide + examide + citoglipton)


#The 24 features of medications are not significant for our Analysis as these details can 
#be extracted out form the variable Diabetes Medications and so these 24 features 
#don't have any significant impact on our Analysis as we are analyzing the Impact of early 
#Diagnosis of Diabetes on the Patient Re admission Rate

Dataset <- Dataset %>%
  select(-c(17:39))

# Reordering Data Columns to move the dependent Variable at the end 

Dataset <- Dataset %>%
  select(c(1:18,20:22,19))

# Look at Columns Diag 1 , Diag 2 and Diag 3. These are the Diagnosis for patient Illness
#Let us classify these columns on the base of ICD Codes classification

Dataset <- Dataset %>%
  mutate(diag_1 = case_when(diag_1 %in% c(240 : 279) ~ "Diabetes",
                            diag_1 %in% c(280 : 289) ~ "Blood",
                            diag_1 %in% c(290 : 319) ~ "Mental",
                            diag_1 %in% c(320 : 359) ~ "Nervous",
                            diag_1 %in% c(360 : 389) ~ "Sensory",
                            diag_1 %in% c(390 : 459) ~ "Circulatory",
                            diag_1 %in% c(460 : 519) ~ "Respiratory",
                            diag_1 %in% c(520 : 579) ~ "Digestive",
                            diag_1 %in% c(580 : 629) ~ "Genito Urinary",
                            diag_1 %in% c(630 : 679) ~ "Pregnancy",
                            diag_1 %in% c(680 : 709) ~ "Skin",
                            diag_1 %in% c(710 : 739) ~ "Tissue",
                            diag_1 %in% c(740 : 759) ~ "Genetic",
                            TRUE ~ "Others"))
  
Dataset <- Dataset %>%
  mutate(diag_2 = case_when(diag_2 %in% c(240 : 279) ~ "Diabetes",
                            diag_2 %in% c(280 : 289) ~ "Blood",
                            diag_2 %in% c(290 : 319) ~ "Mental",
                            diag_2 %in% c(320 : 359) ~ "Nervous",
                            diag_2 %in% c(360 : 389) ~ "Sensory",
                            diag_2 %in% c(390 : 459) ~ "Circulatory",
                            diag_2 %in% c(460 : 519) ~ "Respiratory",
                            diag_2 %in% c(520 : 579) ~ "Digestive",
                            diag_2 %in% c(580 : 629) ~ "Genito Urinary",
                            diag_2 %in% c(630 : 679) ~ "Pregnancy",
                            diag_2 %in% c(680 : 709) ~ "Skin",
                            diag_2 %in% c(710 : 739) ~ "Tissue",
                            diag_2 %in% c(740 : 759) ~ "Genetic",
                            TRUE ~ "Others"))

Dataset <- Dataset %>%
  mutate(diag_3 = case_when(diag_3 %in% c(240 : 279) ~ "Diabetes",
                            diag_3 %in% c(280 : 289) ~ "Blood",
                            diag_3 %in% c(290 : 319) ~ "Mental",
                            diag_3 %in% c(320 : 359) ~ "Nervous",
                            diag_3 %in% c(360 : 389) ~ "Sensory",
                            diag_3 %in% c(390 : 459) ~ "Circulatory",
                            diag_3 %in% c(460 : 519) ~ "Respiratory",
                            diag_3 %in% c(520 : 579) ~ "Digestive",
                            diag_3 %in% c(580 : 629) ~ "Genito Urinary",
                            diag_3 %in% c(630 : 679) ~ "Pregnancy",
                            diag_3 %in% c(680 : 709) ~ "Skin",
                            diag_3 %in% c(710 : 739) ~ "Tissue",
                            diag_3 %in% c(740 : 759) ~ "Genetic",
                            TRUE ~ "Others"))

#checking these 3 columns 
str(Dataset[,c(13,14,15)]) # They have character Data.

#Converting into factors
Dataset = Dataset %>%
  mutate(diag_1 = as.factor(diag_1),diag_2 = as.factor(diag_2),diag_3 = as.factor(diag_3))

#Rechecking these 3 columns 
str(Dataset[,c(13,14,15)]) # They have factor Data, which is as desired.

#Visualizing this Analysis
#Diagnosis 1
plot5 <- ggplot(data = Dataset,aes(x=diag_1,fill=gender))
plot5 + geom_histogram(binwidth = 10,stat = "count") +
  xlab("Diagnosis 1") + ylab("Number of Patients")+
  ggtitle("Patient 1st Diagnosis with Gender") + 
  theme(axis.title.x = element_text(colour 
                                    = "DarkGreen",size = 30),
        axis.title.y = element_text(colour = "DarkGreen",size = 30),
        axis.text = element_text(size = 15),
        plot.title = element_text(colour = "Red",size = 40,
                                  face = "bold.italic",hjust = .5))

#Diagnosis 2


plot6 <- ggplot(data = Dataset,aes(x=diag_2,fill=gender))
plot6 + geom_histogram(binwidth = 10,stat = "count") +
  xlab("Diagnosis 2") + ylab("Number of Patients")+
  ggtitle("Patient 2nd Diagnosis with Gender") + 
  theme(axis.title.x = element_text(colour 
                                    = "DarkGreen",size = 30),
        axis.title.y = element_text(colour = "DarkGreen",size = 30),
        axis.text = element_text(size = 15),
        plot.title = element_text(colour = "Red",size = 40,
                                  face = "bold.italic",hjust = .5))

#Diagnosis 3
plot7 <- ggplot(data = Dataset,aes(x=diag_3,fill=gender))
plot7 + geom_histogram(binwidth = 10,stat = "count") +
  xlab("Diagnosis 3") + ylab("Number of Patients")+
  ggtitle("Patient 3rd Diagnosis with Gender") + 
  theme(axis.title.x = element_text(colour 
                                    = "DarkGreen",size = 30),
        axis.title.y = element_text(colour = "DarkGreen",size = 30),
        axis.text = element_text(size = 15),
        plot.title = element_text(colour = "Red",size = 40,
                                  face = "bold.italic",hjust = .5))




# Now let us analyze our main dependent variable which is Readmitted - based on 
#Gender, Age,Class,Diagnosis,Gulucose serum level,A1C result, ICU Patients etc....

#RACE, READMITTED , Gender

plot8 <- ggplot(data = Dataset,aes(x=race,fill=readmitted))
plot8 + geom_histogram(binwidth = 10,stat = "count") +
  xlab("Race") + ylab("Number of Patients")+
  ggtitle("Patient Readmission with race & Gender") + 
  facet_grid(gender~.)+
  theme(axis.title.x = element_text(colour 
                                    = "DarkGreen",size = 30),
        axis.title.y = element_text(colour = "DarkGreen",size = 30),
        axis.text = element_text(size = 15),
        plot.title = element_text(colour = "Red",size = 40,
                                  face = "bold.italic",hjust = .5))

#AGE, READMITTED , Gender

plot9 <- ggplot(data = Dataset,aes(x=age,fill=readmitted))
plot9 + geom_histogram(binwidth = 10,stat = "count") +
  xlab("Age") + ylab("Number of Patients")+
  ggtitle("Patient Readmission with Age & Gender") + 
  facet_grid(gender~.)+
  theme(axis.title.x = element_text(colour 
                                    = "DarkGreen",size = 30),
        axis.title.y = element_text(colour = "DarkGreen",size = 30),
        axis.text = element_text(size = 15),
        plot.title = element_text(colour = "Red",size = 40,
                                  face = "bold.italic",hjust = .5))


#Diagnosis 1, READMITTED , ICU Statistics

plot10 <- ggplot(data = Dataset,aes(x=diag_1,fill=readmitted))
plot10 + geom_histogram(binwidth = 10,stat = "count",position=position_dodge()) +
  xlab("First Diagnosis") + ylab("Number of Patients")+
  ggtitle("Patient Readmission with First Diagnosis & Level of Sickness") + 
  
  facet_grid(Inpatient_ICU_Statistics~.)+
  theme(axis.title.x = element_text(colour 
                                    = "DarkGreen",size = 30),
        axis.title.y = element_text(colour = "DarkGreen",size = 30),
        axis.text = element_text(size = 15),
        plot.title = element_text(colour = "Red",size = 40,
                                  face = "bold.italic",hjust = .5)) +
  
  
  geom_text(aes(label = ..count.., y = ..count..),stat="count",
            position = position_dodge(width = 1),
            vjust = -0.5, size = 3)

#Diagnosis 2, READMITTED , ICU Statistics

plot11 <- ggplot(data = Dataset,aes(x=diag_2,fill=readmitted))
plot11 + geom_histogram(binwidth = 10,stat = "count",position=position_dodge()) +
  xlab("Second Diagnosis") + ylab("Number of Patients")+
  ggtitle("Patient Readmission with Second Diagnosis & Level of Sickness") + 
  geom_text(aes(label = ..count.., y = ..count..),stat="count",
            position = position_dodge(width = 1),
            vjust = -0.5, size = 3)+
  facet_grid(Inpatient_ICU_Statistics~.)+
  theme(axis.title.x = element_text(colour 
                                    = "DarkGreen",size = 30),
        axis.title.y = element_text(colour = "DarkGreen",size = 30),
        axis.text = element_text(size = 15),
        plot.title = element_text(colour = "Red",size = 40,
                                  face = "bold.italic",hjust = .5))

#Diagnosis 3, READMITTED , ICU Statistics

plot12 <- ggplot(data = Dataset,aes(x=diag_3,fill=readmitted))
plot12 + geom_histogram(binwidth = 10,stat = "count",position = position_dodge()) +
  xlab("Third Diagnosis") + ylab("Number of Patients")+
  ggtitle("Patient Readmission with Third Diagnosis & Level of Sickness") + 
  geom_text(aes(label = ..count.., y = ..count..),stat="count",
            position = position_dodge(width = 1),
            vjust = -0.5, size = 3)+
  facet_grid(Inpatient_ICU_Statistics~.)+
  theme(axis.title.x = element_text(colour 
                                    = "DarkGreen",size = 30),
        axis.title.y = element_text(colour = "DarkGreen",size = 30),
        axis.text = element_text(size = 15),
        plot.title = element_text(colour = "Red",size = 40,
                                  face = "bold.italic",hjust = .5))


#However the picture is not totally clear with this data, let us code the data into 
#2 categories - 
#Diagnosis 1 : Diabetes and Others
#Diagnosis 2 : Diabetes and Others
#Diagnosis 3 : Diabetes and Others
#However, this is only for the purpose of creating Graphs and EDA. We will be not doing this during
# modeling. For modeling, we will use all factors

Dataset_Sample = Dataset %>%
  mutate(diag_1 = ifelse(diag_1 =="Diabetes","Diabetes","Others"),
         diag_2 = ifelse(diag_2 =="Diabetes","Diabetes","Others"),
         diag_3 = ifelse(diag_3 =="Diabetes","Diabetes","Others"))
  
#checking these 3 columns 
str(Dataset_Sample[,c(13,14,15)]) # They have character Data.

#Converting into factors
Dataset_sample = Dataset_Sample %>%
  mutate(diag_1 = as.factor(diag_1),diag_2 = as.factor(diag_2),diag_3 = as.factor(diag_3))

#Rechecking these 3 columns 
str(Dataset_Sample[,c(13,14,15)]) # They have factor Data, which is as desired.

# Recreating the same Graphs

#Diagnosis 1, READMITTED , ICU Statistics

plot13 <- ggplot(data = Dataset_Sample,aes(x=diag_1,fill=readmitted))
plot13 + geom_histogram(binwidth = 10,stat = "count",position=position_dodge()) +
  xlab("First Diagnosis") + ylab("Number of Patients")+
  ggtitle("Patient Readmission with First Diagnosis & Level of Sickness") + 
  geom_text(aes(label = ..count.., y = ..count..),stat="count",
            position = position_dodge(width = 1),
            vjust = -0.5, size = 3)+
  facet_grid(Inpatient_ICU_Statistics~.)+
  theme(axis.title.x = element_text(colour 
                                    = "DarkGreen",size = 30),
        axis.title.y = element_text(colour = "DarkGreen",size = 30),
        axis.text = element_text(size = 15),
        plot.title = element_text(colour = "Red",size = 40,
                                  face = "bold.italic",hjust = .5))

#Diagnosis 2, READMITTED , ICU Statistics

plot14 <- ggplot(data = Dataset_Sample,aes(x=diag_2,fill=readmitted))
plot14 + geom_histogram(binwidth = 10,stat = "count",position=position_dodge()) +
  xlab("Second Diagnosis") + ylab("Number of Patients")+
  ggtitle("Patient Readmission with Second Diagnosis & Level of Sickness") + 
  geom_text(aes(label = ..count.., y = ..count..),stat="count",
            position = position_dodge(width = 1),
            vjust = -0.5, size = 3)+
  facet_grid(Inpatient_ICU_Statistics~.)+
  theme(axis.title.x = element_text(colour 
                                    = "DarkGreen",size = 30),
        axis.title.y = element_text(colour = "DarkGreen",size = 30),
        axis.text = element_text(size = 15),
        plot.title = element_text(colour = "Red",size = 40,
                                  face = "bold.italic",hjust = .5))

#Diagnosis 3, READMITTED , ICU Statistics

plot15 <- ggplot(data = Dataset_Sample,aes(x=diag_3,fill=readmitted))
plot15 + geom_histogram(binwidth = 10,stat = "count",position = position_dodge()) +
  xlab("Third Diagnosis") + ylab("Number of Patients")+
  ggtitle("Patient Readmission with Third Diagnosis & Level of Sickness") + 
  geom_text(aes(label = ..count.., y = ..count..),stat="count",
            position = position_dodge(width = 1),
            vjust = -0.5, size = 3)+
  facet_grid(Inpatient_ICU_Statistics~.)+
  theme(axis.title.x = element_text(colour 
                                    = "DarkGreen",size = 30),
        axis.title.y = element_text(colour = "DarkGreen",size = 30),
        axis.text = element_text(size = 15),
        plot.title = element_text(colour = "Red",size = 40,
                                  face = "bold.italic",hjust = .5))



# What the result depicts is that - there is no much difference between the test results 
# for ICU and Non ICU patients , which clearly states that there may be 2 reasons  - 
# a)	Either hospital is following the protocol for both ICU as well as Non ICU Patients. 
# b)	Or the protocol is not at all followed.
# This will become clear with further analysis - Single Test Analysis

#Gulucose Serum, READMITTED , ICU Statistics 


plot16 <- ggplot(data = Dataset,aes(x=max_glu_serum,fill=readmitted))
plot16 + geom_histogram(binwidth = 10,stat = "count",position = position_dodge()) +
  xlab("Gulucose Serum Diagnostic") + ylab("Number of Patients")+
  ggtitle("Patient Sickness & Readmission with Gulucose Diagnostic") + 
  geom_text(aes(label = ..count.., y = ..count..),stat="count",
            position = position_dodge(width = 1),
            vjust = -0.5, size = 3)+
  facet_grid(Inpatient_ICU_Statistics~.,labeller=label_both) +
  theme(axis.title.x = element_text(colour 
                                    = "DarkGreen",size = 30),
        axis.title.y = element_text(colour = "DarkGreen",size = 30),
        axis.text = element_text(size = 15),
        plot.title = element_text(colour = "Red",size = 40,
                                  face = "bold.italic",hjust = .5))

#A1C Result, READMITTED , ICU Statistics

plot17 <- ggplot(data = Dataset,aes(x=A1Cresult,fill=readmitted))
plot17 + geom_histogram(binwidth = 10,stat = "count",position = position_dodge()) +
  xlab("A1C Diagnostic") + ylab("Number of Patients")+
  ggtitle("Patient Sickness & Readmission with A1C Diagnostic") + 
  geom_text(aes(label = ..count.., y = ..count..),stat="count",
            position = position_dodge(width = 1),
            vjust = -0.5, size = 3)+
  facet_grid(Inpatient_ICU_Statistics~.,labeller=label_both) +
  theme(axis.title.x = element_text(colour 
                                    = "DarkGreen",size = 30),
        axis.title.y = element_text(colour = "DarkGreen",size = 30),
        axis.text = element_text(size = 15),
        plot.title = element_text(colour = "Red",size = 40,
                                  face = "bold.italic",hjust = .5))


# Diabetes Medication should be present on the top of the 
#graph, so changing the name of the column from diabetesMed to Diabetes Medication

# Changing Rowname using Tidyverse

Dataset = Dataset %>%
  rename("Diabetes Medication" = "diabetesMed")

#Gulucose Serum, READMITTED , Diabetes Medication 


plot18 <- ggplot(data = Dataset,aes(x=max_glu_serum,fill=readmitted))
plot18 + geom_histogram(binwidth = 10,stat = "count",position = position_dodge()) +
  xlab("Gulucose Serum Diagnostic") + ylab("Number of Patients")+
  ggtitle("Diabetes Medication & Readmission with Gulucose Diagnostic") + 
  geom_text(aes(label = ..count.., y = ..count..),stat="count",
            position = position_dodge(width = 1),
            vjust = -0.5, size = 3)+
  facet_grid(DiabetesMedication~.,labeller=label_both) +
  theme(axis.title.x = element_text(colour 
                                    = "DarkGreen",size = 30),
        axis.title.y = element_text(colour = "DarkGreen",size = 30),
        axis.text = element_text(size = 15),
        plot.title = element_text(colour = "Red",size = 40,
                                  face = "bold.italic",hjust = .5))

#READMITTED , Diabetes Medication 
plot19 <- ggplot(data = Dataset,aes(x=readmitted,fill=DiabetesMedication))
plot19 + geom_histogram(binwidth = 10,stat = "count",position = position_dodge()) +
  xlab("Readmission") + ylab("Number of Patients")+
  ggtitle("Diabetes Medication & Readmission") + 
  geom_text(aes(label = ..count.., y = ..count..),stat="count",
            position = position_dodge(width = 1),
            vjust = -0.5, size = 3)+
  theme(axis.title.x = element_text(colour 
                                    = "DarkGreen",size = 30),
        axis.title.y = element_text(colour = "DarkGreen",size = 30),
        axis.text = element_text(size = 15),
        plot.title = element_text(colour = "Red",size = 40,
                                  face = "bold.italic",hjust = .5))
#A1C Result, READMITTED , Diabetes Medication

plot20 <- ggplot(data = Dataset,aes(x=A1Cresult,fill=readmitted))
plot20 + geom_histogram(binwidth = 10,stat = "count",position = position_dodge()) +
  xlab("A1C Diagnostic") + ylab("Number of Patients")+
  ggtitle("Diabetes Medication & Readmission with A1C Diagnostic") + 
  geom_text(aes(label = ..count.., y = ..count..),stat="count",
            position = position_dodge(width = 1),
            vjust = -0.5, size = 3)+
  facet_grid(DiabetesMedication~.,labeller=label_both) +
  theme(axis.title.x = element_text(colour 
                                    = "DarkGreen",size = 30),
        axis.title.y = element_text(colour = "DarkGreen",size = 30),
        axis.text = element_text(size = 15),
        plot.title = element_text(colour = "Red",size = 40,
                                  face = "bold.italic",hjust = .5))

# Hypothesis :
##Are the patients who have diabetes (Bad A1C result and Glucose Serum Result) 
#have more chances of Re Admission into the Hospital?

Diabetic_Data = Dataset %>%
  filter(max_glu_serum %in% c(">200",">300") &
           Dataset$A1Cresult %in% c(">7",">8"))

Non_Diabetic_Data = Dataset %>%
  filter(!(max_glu_serum %in% c(">200",">300") &
           Dataset$A1Cresult %in% c(">7",">8")))

#GRAPH

# Use of Library GridExtra
library(gridExtra) # Present in DPLYR


plot21 <- ggplot(data = Diabetic_Data,aes(x=readmitted,fill=readmitted))
p1 <- plot21 + geom_histogram(binwidth = 10,stat = "count",position = position_dodge()) +
  xlab("Readmission") + ylab("Number of Patients")+
  ggtitle("Readmission for Diabetic Patients") + 
  geom_text(aes(label = ..count.., y = ..count..),stat="count",
            position = position_dodge(width = 1),
            vjust = -0.5, size = 3)+
  theme(axis.title.x = element_text(colour 
                                    = "DarkGreen",size = 30),
        axis.title.y = element_text(colour = "DarkGreen",size = 30),
        axis.text = element_text(size = 15),
        plot.title = element_text(colour = "Red",size = 40,
                                  face = "bold.italic",hjust = .5))

plot22 <- ggplot(data = Non_Diabetic_Data,aes(x=readmitted,fill=readmitted))
p2 <- plot22 + geom_histogram(binwidth = 10,stat = "count",position = position_dodge()) +
  xlab("Readmission") + ylab("Number of Patients")+
  ggtitle("Readmission for Non Diabetic Patients") + 
  geom_text(aes(label = ..count.., y = ..count..),stat="count",
            position = position_dodge(width = 1),
            vjust = -0.5, size = 3)+
  theme(axis.title.x = element_text(colour 
                                    = "DarkGreen",size = 30),
        axis.title.y = element_text(colour = "DarkGreen",size = 30),
        axis.text = element_text(size = 15),
        plot.title = element_text(colour = "Red",size = 40,
                                  face = "bold.italic",hjust = .5))

grid.arrange(p1, p2, nrow=2)

# Hypothesis :
# Are the patients with diabetes (Bad A1C result as well as Glucose Serum) and who are not diagnosed during their 
# hospital stay, have more chances of Re Admission into the Hospital as compared to the diabetic ones who are diagnosed?

#Filtering the Dataset with our Results

HypothesisCondition1 <- Dataset %>%
  filter(diag_1 == "Others" & diag_2 == "Others" & diag_3 == "Others" &
           max_glu_serum %in% c(">200",">300") & A1Cresult %in% c(">7",">8"))
  
HypothesisCondition2 <- Dataset %>%
  filter(!(diag_1 == "Others" | diag_2 == "Others" | diag_3 == "Others" |
           max_glu_serum %in% c(">200",">300") | A1Cresult %in% c(">7",">8")))
  


#GRAPH

plot23 <- ggplot(data = Filtered_Dataset3,aes(x=readmitted,fill=readmitted))
p3 <- plot23 + geom_histogram(binwidth = 10,stat = "count",position = position_dodge()) +
  xlab("Readmission") + ylab("Number of Patients")+
  ggtitle("Readmission for Diabetic Patients with no Diagnosis") + 
  geom_text(aes(label = ..count.., y = ..count..),stat="count",
            position = position_dodge(width = 1),
            vjust = -0.5, size = 3)+
  theme(axis.title.x = element_text(colour 
                                    = "DarkGreen",size = 30),
        axis.title.y = element_text(colour = "DarkGreen",size = 30),
        axis.text = element_text(size = 15),
        plot.title = element_text(colour = "Red",size = 40,
                                  face = "bold.italic",hjust = .5))

plot24 <- ggplot(data = Filtered_Dataset4,aes(x=readmitted,fill=readmitted))
p4 <- plot24 + geom_histogram(binwidth = 10,stat = "count",position = position_dodge()) +
  xlab("Readmission") + ylab("Number of Patients")+
  ggtitle("Readmission for Diagnosed Diabetic Patients") + 
  geom_text(aes(label = ..count.., y = ..count..),stat="count",
            position = position_dodge(width = 1),
            vjust = -0.5, size = 3)+
  theme(axis.title.x = element_text(colour 
                                    = "DarkGreen",size = 30),
        axis.title.y = element_text(colour = "DarkGreen",size = 30),
        axis.text = element_text(size = 15),
        plot.title = element_text(colour = "Red",size = 40,
                                  face = "bold.italic",hjust = .5))

grid.arrange(p3, p4, nrow=2)

# Recoding the output variable and some other variables to avoid confusion

Dataset <- Dataset %>%
  mutate(max_glu_serum = case_when(max_glu_serum %in% c('>200','>300') ~ "Abnormal",
                                   max_glu_serum == 'None' ~ "Not Tested",
                                   max_glu_serum == 'Norm' ~ "Normal")) %>%
  mutate(max_glu_serum = as.factor(max_glu_serum)) %>%
  mutate(A1Cresult = case_when(A1Cresult %in% c('>7','>8') ~ "Abnormal",
                               A1Cresult == 'None' ~ "Not Tested",
                               A1Cresult == 'Norm' ~ "Normal")) %>%
  mutate(A1Cresult = as.factor(A1Cresult)) %>%
  mutate(readmitted = ifelse(readmitted == '<30','Yes','No')) %>%
  mutate(readmitted = as.factor(readmitted))

# Collapsing of Multiple Encounters for same patient
# Some patients in the dataset had more than one encounter.We could not count them as independent 
# encounters because that bias the results towards those patients who had multiple encounters. 
# Thus we tried multiple techniques to collapse and consolidate multiple encounters for same patient such as:
  
# 1)Considering more than 2 readmissions across multiple encounters as readmission for collapsed record.
# 2)Considering average stay at hospital across multiple encounters.
# 3)Considering the percentage of the medication changes across multiple encounters
# 4)Considering the total number of the encounters to replace the encounter unique ID
# 5)Considering the combination of diagnoses across multiple encounters as a list

#However the most viable approach is to keep the Data for the 1st encounter only as the last encounter 
#gives very unbalanced data.


Dataset <- Dataset %>%
  arrange(encounter_id)%>%
  arrange(patient_nbr)
# Approach
#................................................................................................
# 1) We will keep the 1st readmitted encounter ID after adding the values of following rows 
# NOT USING THIS FOR THIS MODEL
#................................................................................................
#2) We will keep the 1st readmitted encounter ID

#Analyze the dataset by repeated patient Numbers 135,1152 and 1314
#It has been observed that the Health History of the Patients doesn't change and the repeated encounters for same patients is regarded as redundant data
#The Patient Number should be our unique key and should have 1st Encounter ID
#Removing this redundant data

Dataset = Dataset%>%
  distinct(patient_nbr,.keep_all = T)


#Now we can remove the column Encounter ID as its also not required
Dataset <- Dataset%>%
  select(-encounter_id)

# Exploratory Data Analysis is completed at this point.
# Now we are required to create a model based on Deep Learning.
# We have to eliminate some of the redundant columns like Patient Number which are not required to put into the ANN

# Let us see the Data
str(Dataset)

# Found following Anomolies in the Data
#1) patient_nbr column needs to be dropped
#2) medical_specialty column has 73 factors which will create an issue in ANN as we have to create 72 dummy variables

# So deleting above 2 columns

Dataset = Dataset %>%
  select(-patient_nbr,-medical_specialty)


# Finding correlations between Data:

# To find the correlations, Data should be completely integer. 
#So there are 2 methods
#1) Corrlation Matrix only for the numeric Data
#2) Use one Hot encoding and convert the factor Data to dummy Variables and
# perform the coefficient 

#1) Corrlation Matrix only for the numeric Data wrt to our dependent variable

# correlation_matrix <- Dataset %>%
#   select_if(is.numeric) %>%
#   cor()

correlation_matrix <- Dataset %>%
  mutate(readmitted = as.numeric(as.factor(readmitted)))%>%
  select_if(is.numeric) %>%
  cor()

library("RColorBrewer")
col <- colorRampPalette(brewer.pal(10, "RdYlBu"))(256)
heatmap(correlation_matrix, scale = "none", col =  col)

library(corrplot)
corrplot(correlation_matrix, method = "square", 
         type = "upper", tl.col = "black", order = "hclust", 
         col = brewer.pal(n = 5, name = "RdYlBu"))

library(corrr)
# We can also find paired correlations with the dependent variable

Dataset %>%
  select(readmitted,time_in_hospital) %>%
  mutate(
    readmitted = as.numeric(as.factor(readmitted))
    #LogTotalCharges = log(TotalCharges)
  ) %>%
  correlate() %>%
  focus(readmitted) %>%
  fashion()

# Also, we can look if the correlations are better with log of numeric data, for our model.
# But for now, we are good to create the model
library(keras)
library(tensorflow)
library(rsample)
library(recipes)

#Preparing Dataset for ANN
#DISCRETIZE THE "TENURE" FEATURE
#ONE-HOT ENCODING for Factor Data
#FEATURE SCALING

#With the help of Recipe Package

# Create recipe
Preprocess_Recipe <- recipe(readmitted ~ . , data = Dataset) %>%
  step_dummy(all_nominal(),-all_outcomes()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep(data = Dataset)

#Dividing the Dataset into Training and Test Set
set.seed(100)
train_test_split <- initial_split(Dataset, prop = 0.8)
train_test_split

# Retrieve train and test sets
training_set <- training(train_test_split)
test_set  <- testing(train_test_split) 

str(training_set)

#Baking the Recipe for both Train as well as Test Data
#And dividing the Data into X_train, x_Test and Y_train and Y_test

x_train <- bake(Preprocess_Recipe, new_data = training_set) %>%
  select(-readmitted)%>%
  as.matrix() #Requiremet for ANN


x_test <- bake(Preprocess_Recipe, new_data = test_set) %>%
  select(-readmitted) %>%
  as.matrix() #Requiremet for ANN


y_train <- ifelse(select(training_set, readmitted) == "Yes", 1, 0)

y_test <- ifelse(select(test_set, readmitted) == "Yes", 1, 0)

# Initialising the ANN
classifier <- keras_model_sequential()

# Adding the input layer and 1st Hidden layer
# This function adds the 1st Hidden layer with 6 nodes and here only we will specify
# the structure of the Input layer with variable input_shape
#Why 6  

dim(x_train) # input shape should match this 2nd parameter
#Input Layer
classifier %>%
  layer_dense(
    units = 34, 
    activation = 'relu',
    input_shape = 68) %>% #Fits becoz we have 35 variables in our data set. Here we subtracted the Dependent Variable
  # Dropout to prevent overfitting
  #layer_dropout(classifier,rate = 0.1) %>%
  # Adding the second hidden layer
  layer_dense(
    units = 34,
    activation = 'relu',
    kernel_initializer = "glorot_uniform")%>%
  # Dropout to prevent overfitting
  #layer_dropout(classifier,rate = 0.1) %>%
  # Adding the output layer
  layer_dense(units = 1,
              activation = 'sigmoid',
              kernel_initializer = "glorot_uniform")%>%
  # Compiling the ANN or applying Stochastic Gradient Descent to ANN
  compile(
    optimizer = 'adam',
    loss = 'binary_crossentropy',
    metrics ='accuracy')


#Summary of the Classifier

summary(classifier)

# Fitting the ANN to the Training set

history <- fit(
  classifier,
  x_train,
  y_train, 
  batch_size = 50,
  epochs = 75, 
  validation_split = .30,
  callbacks = callback_tensorboard("E:/Research/Projects/Self Projects/Diabetes Care Dataset/Tensorboard Logs/log1"),
  write_graph=True, write_images=True)

print(history)
tensorboard("E:/Research/Projects/Self Projects/Diabetes Care Dataset/Tensorboard Logs") #This will Open this in Chrome
#Howevr open this in IE to see the proper Graphs
#FIREFOX is the best

# Predicting the Test set results
y_pred = predict_classes(classifier,x_test)
y_pred

classifier
# Making the Confusion Matrix
cm_test = table(y_test, y_pred)
cm_test

library(caret)
confusionMatrix(cm_test)

# Confusion Matrix and Statistics
# 
# y_pred
# y_test     0     1
# 0 12520   323
# 1  1198    47
# 
# Accuracy : 0.892           
# 95% CI : (0.8868, 0.8971)
# No Information Rate : 0.9737          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.0185          
# 
# Mcnemar's Test P-Value : <2e-16          
#                                           
#             Sensitivity : 0.91267         
#             Specificity : 0.12703         
#          Pos Pred Value : 0.97485         
#          Neg Pred Value : 0.03775         
#              Prevalence : 0.97374         
#          Detection Rate : 0.88870         
#    Detection Prevalence : 0.91163         
#       Balanced Accuracy : 0.51985         
#                                           
#        'Positive' Class : 0  

library(ROCR)
#Creating the ROCR Curve
pred <- prediction(y_pred,y_test)
perf <- performance(pred,measure = "tpr", x.measure = "fpr")

# calculating AUC
auc <- performance(pred,"auc")
auc <- unlist(slot(auc, "y.values"))
auc #.5063

# plotting the ROC curve
plot(perf,colorize=TRUE)
plot(perf,col="black",lty=3, lwd=3)
abline(a=0, b= 1)
legend(0.6,0.4,round(auc, digits = 4),cex=1.1, box.col = "white")

