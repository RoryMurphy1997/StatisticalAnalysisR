#Part 1

#Read EU Dataset
EU_Data <- read.table(file = "EU_20190102.txt", header = TRUE, sep = ",")
#Read US Dataset
US_Data <- read.table(file = "US_2019_02_01.txt", header = TRUE, sep = ";")

#Create EU customers between 30 and 40 years old (inclusive)
EU_cust_30_40 <- subset(EU_Data,(age >= 30)&(age <= 40))
#Creating CSV of EU customers between 30 and 40 (inclusive)
write.table(EU_cust_30_40, file = "EU_cust_30_40.csv", row.names = FALSE, sep = ",")

#Create EU customers with tertiary education and blue-collar or management job
EU_cust_ter_bc_mgmt <- subset(EU_Data,((education == "tertiary")&((job == "management")|(job == "blue-collar"))))
#Creating CSV of EU customers with tertiary education and blue-collar or management job
write.table(EU_cust_ter_bc_mgmt, file = "EU_cust_ter_bc_mgmt.csv", row.names = FALSE, sep = ",")

#Part 2

#Adjust admin. to admin for EU and US data so that filename is not later confused and the two match when combined in part 3
levels(EU_Data$job)[1] <- "admin"
levels(US_Data$job)[1] <- "admin"
#Get all types of jobs in EU_Data
job_types <- levels(EU_Data$job)
#Loop through each job, creating the csv of customer data for customers specific to each type of occupation
for(i in 1:length(job_types))
{
  #Store job 'i' in variable
  current_job <- job_types[i]
  #Find EU customers with job 'i'
  temp_jobs <- subset(EU_Data, job == current_job)
  #Create CSV name
  csv_name <- paste("EU_cust_occupation_",current_job,".csv")
  #Create CSV
  write.table(temp_jobs, file = csv_name, row.names = FALSE, sep = ",")
}

#Part 3

#Make all mobile and landline contact variables "teleph" in EU_Data
levels(EU_Data$contact)[1:2] <- "teleph"
#Convert US data balance from dollars to euros (1 dollar = 0.87 Euro)
US_Data$balance <- US_Data$balance * 0.87
#Remove poutcome from EU data
EU_Data <- EU_Data[,-16]
#Add source (where data came from) to EU and US Data
EU_Data$source <- "EU"
US_Data$source <- "US"
#Combine data
All_Data <- rbind(EU_Data, US_Data)
#Create csv of all data
write.table(All_Data, file = "EU_US_Merge.csv", row.names = FALSE, sep = ",")

