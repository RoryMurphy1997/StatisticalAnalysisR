#Set working directory
setwd("C:/Users/Cormac/Desktop/Statistical Analysis/Assignment")
#Read in data
bank <- read.table(file = "CreditData.txt", header = FALSE, sep = " ")

#Randomise the rows to ensure no underlying order in data may affect training-test split
bank <- bank[sample(nrow(bank)),]

#Add labels to each column
names(bank) <- c('Status', 'DurationinMonths', 'CreditHistory', 'Purpose', 'CreditAmount', 'Savingsaccount/bonds', 'PresentEmploymentLength', 'InstallmentRate', 'PersonalStatus', 'OtherDebtorsorGuarantors', 'PresentResidenceSince', 'Property', 'AgeinYears', 'OtherInstallmentPlans', 'Housing', 'NumberofExistingCreditsatthisBank', 'Job', 'NumberofPeopleLiabletoProvideMaintenancefor', 'Telephone', 'ForeignWorker', 'Outcome')

#Change levels to meaningful values
#Status
levels(bank$Status) <- c('<0euro','0euro-500euro','>500euro/SecureSalary','NoCheckingAccount')
#Credit History
levels(bank$CreditHistory) <- c('NoCreditTaken/AllPaidBack','AllToThisBankPaidBack','PaidBackDulyTillNow','DelaysinPast','CriticalAccount/OtherCreditsinOtherBanks')
#Purpose
#No entries for vacation, therefore isn't relabelled or considered in analysis
levels(bank$Purpose) <- c('NewCar','UsedCar','Others','Furniture/Equipment','Radio/Television','DomesticAppliances','Repairs','Education','Retraining','Business')
#Savings account/bonds
levels(bank$`Savingsaccount/bonds`) <- c('<100euro','100euro-500euro','500euro-1000euro','>1000euro','Unknown/NoAccount')
#Present Employment Length
levels(bank$PresentEmploymentLength) <- c('<1yr','1yr-4yrs','4yrs-7yrs','>7yrs','')
#Personal Status
#No single female accounts
#Classifications here are very inconsistent, hard to read a lot out of
levels(bank$PersonalStatus) <- c('Male(Divorced/Separated)','Female(Divorced/Separated/Married)','Male(Single)','Male(Married/Widowed)')
#Other Debtors or Guarantors
levels(bank$OtherDebtorsorGuarantors) <- c('None','CoApplicant','Guarantor')
#Property
levels(bank$Property) <- c('RealEstate','BuildingSocietySavingsAgreement/LifeInsurance','Car/Other','Unknown/NoProperty')
#Other Installment Plans
levels(bank$OtherInstallmentPlans) <- c('Bank','Stores','None')
#Housing
levels(bank$Housing) <- c('Rent', 'Own', 'Free')
#Job
levels(bank$Job) <- c('Unemployed/unskilled(nonResident)','Unskilled(Resident)','Skilled/Official','Management/Self-Employed/HighlyQualified/Officer')
#Telephone
levels(bank$Telephone) <- c('MobileOnly','MobleandLandline')
#Foreign Worker
levels(bank$ForeignWorker) <- c(1,2)
bank[1:5,]

#Split into test and training data
#Randomise to ensure no underlying order exists

#Data without results
bankinput <- bank[-21]