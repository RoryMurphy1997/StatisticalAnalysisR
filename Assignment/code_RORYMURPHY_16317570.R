#Set working directory
setwd("C:/Users/Cormac/Desktop/Statistical Analysis/Assignment")
#Set seed for purpose of selecting training-test data and replicating results
set.seed(5)
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
levels(bank$PresentEmploymentLength) <- c('Unemployed','<1yr','1yr-4yrs','4yrs-7yrs','>7yrs')
#Personal Status
#No single female accounts
#Classifications here are very inconsistent.
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
levels(bank$ForeignWorker) <- c('Yes','No')

#Subtract 1 from "Outcome" to allow glm to read as binary
bank$Outcome <- bank$Outcome - 1

#Check data type of each list are factors or ints
str(bank)

#Combine groupings where values are underrepresented (<15)
#Status : None underrepresented
table(bank$Status)
#Credit History : None underrepresented
table(bank$CreditHistory)
#Purpose : Only 9 retraining and 12 in domestic appliances, so combined them with "Others"
table(bank$Purpose)
levels(bank$Purpose)[c(3,6,9)] <- "Others"
#Savings account/bonds : None underrepresented
table(bank$`Savingsaccount/bonds`)
#Present Employment Length : None underrepresented
table(bank$PresentEmploymentLength)
#Personal Status: Due to inconsistent classification, combined into Male(single), Male(Married/Other), Female(Married/Other)
table(bank$PersonalStatus)
levels(bank$PersonalStatus)[c(1,4)] <- "Male(Married/Other)"
levels(bank$PersonalStatus)[2] <- "Female(Married/Other)"
#No single female accounts seen, but this would not need to be changed anyway
#Other Debtors or Guarantors : None underrepresented
table(bank$OtherDebtorsorGuarantors)
#Property : None underrepresented
table(bank$Property)
#Other Installment Plans : None underrepresented
table(bank$OtherInstallmentPlans)
#Housing : None underrepresented
table(bank$Housing)
#Job : None underrepresented
table(bank$Job)
#Telephone : None underrepresented
table(bank$Telephone)
#Foreign Worker : None underrepresented
table(bank$ForeignWorker)

#Remove highly correlated factors
#Numeric factor correlation
cor(bank[,c(2,5,8,11,13,16,18)])
#Categorical factors
#Cramer V calculation formula
cramer <- function(table)
{
  #Total sample size
  n <- sum(table)
  #Minimum of number of rows and number of columns
  q <- min(nrow(table),ncol(table))
  #chiSquared value
  #p values determined via Monte Carlo simulation due to small values in some categories potentially leading to inaccurate values of "p"
  chi <- unname(chisq.test(table, correct = FALSE, simulate.p.value = TRUE)$statistic)
  #Cramer's V
  v <- sqrt(chi/(n*(q-1)))
  return(v)
}
#Status
mapply(function(x, y) cramer(table(x, y)), bank[, -c(2,5,8,11,13,16,18,21)], MoreArgs=list(bank$Status))
#Credit History
mapply(function(x, y) cramer(table(x, y)), bank[, -c(2,5,8,11,13,16,18,21)], MoreArgs=list(bank$CreditHistory))
#Purpose
mapply(function(x, y) cramer(table(x, y)), bank[, -c(2,5,8,11,13,16,18,21)], MoreArgs=list(bank$Purpose))
#Savings account/bonds
mapply(function(x, y) cramer(table(x, y)), bank[, -c(2,5,8,11,13,16,18,21)], MoreArgs=list(bank$`Savingsaccount/bonds`))
#Present Employment Length
mapply(function(x, y) cramer(table(x, y)), bank[, -c(2,5,8,11,13,16,18,21)], MoreArgs=list(bank$PresentEmploymentLength))
#Personal Status
mapply(function(x, y) cramer(table(x, y)), bank[, -c(2,5,8,11,13,16,18,21)], MoreArgs=list(bank$PersonalStatus))
#Other Debtors or Guarantors
mapply(function(x, y) cramer(table(x, y)), bank[, -c(2,5,8,11,13,16,18,21)], MoreArgs=list(bank$OtherDebtorsorGuarantors))
#Property
mapply(function(x, y) cramer(table(x, y)), bank[, -c(2,5,8,11,13,16,18,21)], MoreArgs=list(bank$Property))
#Other Installment Plans
mapply(function(x, y) cramer(table(x, y)), bank[, -c(2,5,8,11,13,16,18,21)], MoreArgs=list(bank$OtherInstallmentPlans))
#Housing
mapply(function(x, y) cramer(table(x, y)), bank[, -c(2,5,8,11,13,16,18,21)], MoreArgs=list(bank$Housing))
#Job
mapply(function(x, y) cramer(table(x, y)), bank[, -c(2,5,8,11,13,16,18,21)], MoreArgs=list(bank$Job))
#Telephone
mapply(function(x, y) cramer(table(x, y)), bank[, -c(2,5,8,11,13,16,18,21)], MoreArgs=list(bank$Telephone))
#Foreign Worker
mapply(function(x, y) cramer(table(x, y)), bank[, -c(2,5,8,11,13,16,18,21)], MoreArgs=list(bank$ForeignWorker))

#Split into test and training data
#Stratified split: Ensure that training and test data both contain proportional number of good and bad credit scores
#Split bank data into good and bad credit score matrices
goodBank <- bank[which(bank$Outcome == 0),]
badBank <- bank[which(bank$Outcome == 1),]
#Create training data using 80% of values from both good and bad bank
training <- rbind(goodBank[0:(nrow(goodBank)* 0.8),],badBank[0:(nrow(badBank)* 0.8),])
#Create test data using remaining 20% of values from both good and bad bank
test <- rbind(goodBank[((nrow(goodBank)* 0.8) + 1) :nrow(goodBank),],badBank[((nrow(badBank)* 0.8) + 1) :nrow(badBank),])

#Visualisations
#Status of existing current account
StatusByOutcome <- prop.table(table(training$Outcome, training$Status),1)
#Adjust plot window to allow labels to be seen
op <- par(mar=c(10,4,4,2))
barplot(StatusByOutcome, main="Status of existing current account by credit outcome", col=c("dodgerblue","red"), legend = c("Good","Bad"), beside=TRUE, las = 2)
#Reset plot window to default
par(op)

#Duration in Months
boxplot(DurationinMonths~Outcome, training, xlab = "Outcome", ylab = "Duration in Months", main = "Duration in Months by Credit Outcome", names = c("Good","Bad"), col = "dodgerblue")

#Credit History
CreditHistoryByOutcome <- prop.table(table(training$Outcome, training$CreditHistory),1)
#Adjust plot window to allow labels to be seen
op <- par(mar=c(18,4,4,2))
barplot(CreditHistoryByOutcome, main="Credit history by credit outcome", col=c("dodgerblue","red"), legend = c("Good","Bad"), beside=TRUE, las = 2)
#Reset plot window to default
par(op)

#Purpose
PurposeByOutcome <- prop.table(table(training$Outcome, training$Purpose),1)
#Adjust plot window to allow labels to be seen
op <- par(mar=c(9,4,4,2))
barplot(PurposeByOutcome, main="Purpose by credit outcome", col=c("dodgerblue","red"), legend = c("Good","Bad"), beside=TRUE, las = 2)
#Reset plot window to default
par(op)

#Credit Amount
boxplot(CreditAmount~Outcome, training, xlab = "Outcome", ylab = "Credit Amount", main = "Credit Amount by Credit Outcome", names = c("Good","Bad"), col = "dodgerblue")

#Savings account/bonds
SavingsAccountByOutcome <- prop.table(table(training$Outcome, training$`Savingsaccount/bonds`),1)
#Adjust plot window to allow labels to be seen
op <- par(mar=c(9,4,4,2))
barplot(SavingsAccountByOutcome, main="Savings account by credit outcome", col=c("dodgerblue","red"), legend = c("Good","Bad"), beside=TRUE, las = 2)
#Reset plot window to default
par(op)

#Time at current employment
EmploymentTimeByOutcome <- prop.table(table(training$Outcome, training$PresentEmploymentLength),1)
#Adjust plot window to allow labels to be seen
op <- par(mar=c(7,4,4,2))
barplot(EmploymentTimeByOutcome, main="Time at current employment by credit outcome", col=c("dodgerblue","red"), legend = c("Good","Bad"), beside=TRUE, las = 2)
#Reset plot window to default
par(op)

#Installment Rate
InstallmentRateByOutcome <- prop.table(table(training$Outcome, training$InstallmentRate),1)
#Adjust plot window to allow labels to be seen
op <- par(mar=c(7,4,4,2))
barplot(InstallmentRateByOutcome, main="Installment by credit outcome", col=c("dodgerblue","red"), legend = c("Good","Bad"), beside=TRUE, las = 2)
#Reset plot window to default
par(op)

#Personal status
PersonalStatusByOutcome <- prop.table(table(training$Outcome, training$PersonalStatus),1)
#Adjust plot window to allow labels to be seen
op <- par(mar=c(10,4,4,2))
barplot(PersonalStatusByOutcome, main="Personal status by credit outcome", col=c("dodgerblue","red"), legend = c("Good","Bad"), beside=TRUE, las = 2)
#Reset plot window to default
par(op)

#Other debtors/guarantors
OtherDebtorsorGuarantorsByOutcome <- prop.table(table(training$Outcome, training$OtherDebtorsorGuarantors),1)
#Adjust plot window to allow labels to be seen
op <- par(mar=c(6,4,4,2))
barplot(OtherDebtorsorGuarantorsByOutcome, main="Other debtors or guarantors by credit outcome", col=c("dodgerblue","red"), legend = c("Good","Bad"), beside=TRUE, las = 2)
#Reset plot window to default
par(op)

#Present Residence Since
PresentResidenceByOutcome <- prop.table(table(training$Outcome, training$PresentResidenceSince),1)
#Adjust plot window to allow labels to be seen
op <- par(mar=c(6,4,4,2))
barplot(PresentResidenceByOutcome, main="Time in present residence by credit outcome", col=c("dodgerblue","red"), legend = c("Good","Bad"), beside=TRUE, las = 2)
#Reset plot window to default
par(op)

#Property Type
PropertyByOutcome <- prop.table(table(training$Outcome, training$Property),1)
#Adjust plot window to allow labels to be seen
op <- par(mar=c(19.2,4,4,2))
barplot(PropertyByOutcome, main="Property type by credit outcome", col=c("dodgerblue","red"), legend = c("Good","Bad"), beside=TRUE, las = 2)
#Reset plot window to default
par(op)

#Age in Years
boxplot(AgeinYears~Outcome, training, xlab = "Outcome", ylab = "Age in Years", main = "Age in Years by Credit Outcome", names = c("Good","Bad"), col = "dodgerblue")

#Other installment plans
OtherInstallmentPlansByOutcome <- prop.table(table(training$Outcome, training$OtherInstallmentPlans),1)
#Adjust plot window to allow labels to be seen
op <- par(mar=c(5,4,4,2))
barplot(OtherInstallmentPlansByOutcome, main="Other installement plans by credit outcome", col=c("dodgerblue","red"), legend = c("Good","Bad"), beside=TRUE, las = 2)
#Reset plot window to default
par(op)

#Housing
HousingByOutcome <- prop.table(table(training$Outcome, training$Housing),1)
#Adjust plot window to allow labels to be seen
op <- par(mar=c(5,4,4,2))
barplot(HousingByOutcome, main="Housing by credit outcome", col=c("dodgerblue","red"), legend = c("Good","Bad"), beside=TRUE, las = 2)
#Reset plot window to default
par(op)

#Number of Existing Credits at this bank
NumbCreditsByOutcome <- prop.table(table(training$Outcome, training$NumberofExistingCreditsatthisBank),1)
#Adjust plot window to allow labels to be seen
op <- par(mar=c(5,4,4,2))
barplot(NumbCreditsByOutcome, main="Number of Existing Credits at this bank by credit outcome", col=c("dodgerblue","red"), legend = c("Good","Bad"), beside=TRUE, las = 2)
#Reset plot window to default
par(op)

#Job
JobByOutcome <- prop.table(table(training$Outcome, training$Job),1)
#Adjust plot window to allow labels to be seen
op <- par(mar=c(20.2,4,4,2))
barplot(JobByOutcome, main="Job status by credit outcome", col=c("dodgerblue","red"), legend = c("Good","Bad"), beside=TRUE, las = 2)
#Reset plot window to default
par(op)

#Number of People Being Liable to Provide Maintenance for
NumbLiableByOutcome <- prop.table(table(training$Outcome, training$NumberofPeopleLiabletoProvideMaintenancefor),1)
#Adjust plot window to allow labels to be seen
op <- par(mar=c(5,4,4,2))
barplot(NumbLiableByOutcome, main="Number of People Being Liable to Provide Maintenance for by credit outcome", col=c("dodgerblue","red"), legend = c("Good","Bad"), beside=TRUE, las = 2)
#Reset plot window to default
par(op)

#Telephone
TelephoneByOutcome <- prop.table(table(training$Outcome, training$Telephone),1)
#Adjust plot window to allow labels to be seen
op <- par(mar=c(9,4,4,2))
barplot(TelephoneByOutcome, main="Telephone by credit outcome", col=c("dodgerblue","red"), legend = c("Good","Bad"), beside=TRUE, las = 2)
#Reset plot window to default
par(op)

#Foreign Worker
ForeignWorkerByOutcome <- prop.table(table(training$Outcome, training$ForeignWorker),1)
#Adjust plot window to allow labels to be seen
op <- par(mar=c(5,4,4,2))
barplot(ForeignWorkerByOutcome, main="Foreign worker by credit outcome", col=c("dodgerblue","red"), legend = c("Good","Bad"), beside=TRUE, las = 2)
#Reset plot window to default
par(op)

#Build model
fit <- glm(Outcome~., data = training, family = binomial(logit))
summary(fit)
#Refined model
eq <- Outcome~Status+DurationinMonths+CreditHistory+Purpose+CreditAmount+`Savingsaccount/bonds`+PresentEmploymentLength+InstallmentRate+PersonalStatus+Property+OtherInstallmentPlans+Housing
fitTwo <- glm(eq, data = training, family = binomial(logit))
summary(fitTwo)
#Refined Further
eqTwo<- Outcome~Status+DurationinMonths+CreditHistory+Purpose+CreditAmount+`Savingsaccount/bonds`+InstallmentRate+OtherInstallmentPlans+Housing
fitThree <- glm(eqTwo, data = training, family = binomial(logit))
summary(fitThree)
#Keep "fitTwo" since it has lowest AIC
#Create classification matrix
modelClass <- character(200)
modelClass[1:200] <- "P"
modelClass[which(as.vector(predict(fitTwo,test,type="response"))>0.5)] <- "N"
#First 140 items had positive credit score for "Test", the rest had negative
#Number of True Negatives
trueNeg <- length(modelClass[which(modelClass[141:200] == "N")])
#Number of True Positives
truePos <- length(modelClass[which(modelClass[1:140] == "P")])
#Number of False Negatives
falseNeg <- length(modelClass[which(modelClass[1:140] == "N")])
#Number of False Positives
falsePos <- length(modelClass[which(modelClass[141:200] == "P")])
#Misclassification Rate
missClass <- ((falseNeg + falsePos)/200) * 100
cat("Missclassification Rate: ",missClass,"%\n")
#Specificity - Proportion of false positives, v important for bank credit scoring
specificity <- falsePos/(falsePos+trueNeg) * 100
cat("Specificity: ",specificity,"%\n")
#Sensitivity - Proportion of false negatives
sensitivity <- falseNeg/(falseNeg+truePos) * 100
cat("Sensitivity: ",sensitivity,"%\n")

#Far more effective at sensitivity than specificity due to far larger proportion of good credit score customers in model. Model made up of
#only 230 poor credit score customers, not enough to accurately predict negative score (only 58.33% accurate)

