churn.data <- read.csv("C:/Users/jsunez/Desktop/DSS 665/Final Project/WA_Fn-UseC_-Telco-Customer-Churn.csv", header = T)
attach(churn.data)
dim(churn.data)
head(churn.data)
summary(churn.data)

detach(churn.data)
churn.data$SeniorCitizen <- factor(churn.data$SeniorCitizen, labels = c("No", "Yes"))
attach(churn.data)
summary(churn.data)

####################
aggregate(churn.data[c("tenure", "MonthlyCharges", "TotalCharges")], list(Churn=churn.data$Churn), mean, na.rm=T)
by(churn.data, churn.data["Churn"], summary)
####################
churn.logistic.model <- glm(Churn ~ gender + SeniorCitizen + Partner + Dependents + tenure + MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges + TotalCharges, family = "binomial")
summary(churn.logistic.model)
churn.logistic.model <- glm(Churn ~ SeniorCitizen + Dependents + tenure + PhoneService + MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges + TotalCharges, family = "binomial")
####################

tapply(tenure, Churn, mean, na.rm=T)

tapply(MonthlyCharges, Churn, mean, na.rm=T)

tapply(TotalCharges, Churn, mean, na.rm=T)

par(mfrow=c(3,2))
hist(tenure[Churn=="Yes"], main="Tenure of those who Churned", xlab = "Tenure in Months-Churned")
hist(tenure[Churn=="No"], main="Tenure of those who did not Churn", xlab = "Tenure in Months-Did Not Churned")

hist(MonthlyCharges[Churn=="Yes"], main="Monthly Charges of those who Churned", xlab = "Monthly Charges in Dollars-Churned")
hist(MonthlyCharges[Churn=="No"], main="Monthly Charges of those who Churned", xlab = "Monthly Charges in Dollars-Did Not Churned")

hist(TotalCharges[Churn=="Yes"], main="Total Charges of those who Churned", xlab = "Total Charges in Dollars-Churned")
hist(TotalCharges[Churn=="No"], main="Total Charges of those who Churned", xlab = "Total Charges in Dollars-Did Not Churned")

par(mfrow=c(1,3))
boxplot(tenure~Churn, xlab = "Churn", ylab = "Months", main="Boxplot of Tenure by Churn", col=c(2,4))
boxplot(MonthlyCharges~Churn, xlab = "Churn", ylab = "Dollars", main="Boxplot of Monthly Charges by Churn", col=c(2,4))
boxplot(TotalCharges~Churn, xlab = "Churn", ylab = "Dollars", main="Boxplot of Total Charges by Churn", col=c(2,4))

summary(tenure)
summary(tenure[Churn=="Yes"])
summary(tenure[Churn=="No"])

summary(MonthlyCharges)
summary(MonthlyCharges[Churn=="Yes"])
summary(MonthlyCharges[Churn=="No"])

summary(TotalCharges)
summary(TotalCharges[Churn=="Yes"])
summary(TotalCharges[Churn=="No"])

tapply(tenure, Churn, mean, na.rm=T)

tapply(MonthlyCharges, Churn, mean, na.rm=T)

tapply(TotalCharges, Churn, mean, na.rm=T)

#Tables
table(gender, Churn)
t(table(gender, Churn))
#Bar Plot
barplot(table(gender, Churn))

bp <- table(gender, Churn)
barplot(bp, beside=T, legend.text = rownames(bp), main="Churn by Senior Citizen Status", ylab= "Amount of", xlab="Churn", col=c(2,4))
#Proportion Plot
#barplot(prop.table(t(bp), 2), beside=T, legend.text = colnames(bp), main="Churn by Gender", ylab= "Percentage", xlab="Churn", col=c(2,4))
#Transversed-t()
bp <- table(gender, Churn)
par(mfrow=c(1,2))
barplot(bp, beside=T, legend.text = rownames(bp), main="Churn by Senior Citizen Status", ylab= "Amount of", xlab="Churn", col=c(2,4))
barplot(prop.table((bp), 2), beside=T, legend.text = rownames(bp), main="Churn by Gender", ylab= "Percentage", xlab="Churn", col=c(2,4))
prop.table((bp), 2)
bp

bp <- table(SeniorCitizen, Churn)
par(mfrow=c(1,2))
barplot(bp, beside=T, legend.text = rownames(bp), main="Churn by Senior Citizen Status", ylab= "Amount of", xlab="Churn", col=c(2,4))
barplot(prop.table((bp), 2), beside=T, legend.text = rownames(bp), main="Churn by Senior Citizen Status", ylab= "Percentage", xlab="Churn", col=c(2,4))
prop.table((bp), 2)
bp

bp <- table(Partner, Churn)
par(mfrow=c(1,2))
barplot(bp, beside=T, legend.text = rownames(bp), main="Churn by Partner or 'In Relationship'' Status", ylab= "Amount of", xlab="Churn", col=c(2,4))
barplot(prop.table((bp), 2), beside=T, legend.text = rownames(bp), main="Churn by Partner or 'In Relationship'", ylab= "Percentage", xlab="Churn", col=c(2,4))
prop.table((bp), 2)
bp

bp <- table(Dependents, Churn)
par(mfrow=c(1,2))
barplot(bp, beside=T, legend.text = rownames(bp), main="Churn by Having Dependents", ylab= "Amount of", xlab="Churn", col=c(2,4))
barplot(prop.table((bp), 2), beside=T, legend.text = rownames(bp), main="Churn by Having Dependents", ylab= "Percentage", xlab="Churn", col=c(2,4))
prop.table((bp), 2)
bp

bp <- table(PhoneService, Churn)
par(mfrow=c(1,2))
barplot(bp, beside=T, legend.text = rownames(bp), main="Churn by Phone Service Customers", ylab= "Amount of", xlab="Churn", col=c(2,4))
barplot(prop.table((bp), 2), beside=T, legend.text = rownames(bp), main="Churn by Phone Service Customers", ylab= "Percentage", xlab="Churn", col=c(2,4))
prop.table((bp), 2)
bp

bp <- table(MultipleLines, Churn)
par(mfrow=c(1,2))
barplot(bp, beside=T, legend.text = rownames(bp), main="Churn by Having Multiple Lines", ylab= "Amount of", xlab="Churn", col=c(2,4,6))
barplot(prop.table((bp), 2), beside=T, legend.text = rownames(bp), main="Churn by Having Multiple Lines", ylab= "Percentage", xlab="Churn", col=c(2,4,6))
prop.table((bp), 2)
bp

bp <- table(InternetService, Churn)
par(mfrow=c(1,2))
barplot(bp, beside=T, legend.text = rownames(bp), main="Churn by Having Internet Service", ylab= "Amount of", xlab="Churn", col=c(2,4,6))
barplot(prop.table((bp), 2), beside=T, legend.text = rownames(bp), main="Churn by Internet Service", ylab= "Percentage", xlab="Churn", col=c(2,4,6))
prop.table((bp), 2)
bp

bp <- table(OnlineSecurity, Churn)
par(mfrow=c(1,2))
barplot(bp, beside=T, legend.text = rownames(bp), main="Churn by Having Online Security", ylab= "Amount of", xlab="Churn", col=c(2,4,6))
barplot(prop.table((bp), 2), beside=T, legend.text = rownames(bp), main="Churn by Online Security", ylab= "Percentage", xlab="Churn", col=c(2,4,6))
prop.table((bp), 2)
bp

bp <- table(OnlineBackup, Churn)
par(mfrow=c(1,2))
barplot(bp, beside=T, legend.text = rownames(bp), main="Churn by Having Online Backup", ylab= "Amount of", xlab="Churn", col=c(2,4,6))
barplot(prop.table((bp), 2), beside=T, legend.text = rownames(bp), main="Churn by Having Online Backup", ylab= "Percentage", xlab="Churn", col=c(2,4,6))
prop.table((bp), 2)
bp

bp <- table(DeviceProtection, Churn)
par(mfrow=c(1,2))
barplot(bp, beside=T, legend.text = rownames(bp), main="Churn by Having Device Protection", ylab= "Amount of", xlab="Churn", col=c(2,4,6))
barplot(prop.table((bp), 2), beside=T, legend.text = rownames(bp), main="Churn by Having Device Protection", ylab= "Percentage", xlab="Churn", col=c(2,4,6))
prop.table((bp), 2)
bp

bp <- table(TechSupport, Churn)
par(mfrow=c(1,2))
barplot(bp, beside=T, legend.text = rownames(bp), main="Churn by Having Tech Support", ylab= "Amount of", xlab="Churn", col=c(2,4,6))
barplot(prop.table((bp), 2), beside=T, legend.text = rownames(bp), main="Churn by Having Tech Support", ylab= "Percentage", xlab="Churn", col=c(2,4,6))
prop.table((bp), 2)
bp

bp <- table(StreamingTV, Churn)
par(mfrow=c(1,2))
barplot(bp, beside=T, legend.text = rownames(bp), main="Churn by Having Stream TV", ylab= "Amount of", xlab="Churn", col=c(2,4,6))
barplot(prop.table((bp), 2), beside=T, legend.text = rownames(bp), main="Churn by Having Stream TV", ylab= "Percentage", xlab="Churn", col=c(2,4,6))
prop.table((bp), 2)
bp

bp <- table(StreamingMovies, Churn)
par(mfrow=c(1,2))
barplot(bp, beside=T, legend.text = rownames(bp), main="Churn by Having Stream Movies", ylab= "Amount of", xlab="Churn", col=c(2,4,6))
barplot(prop.table((bp), 2), beside=T, legend.text = rownames(bp), main="Churn by Stream Movies", ylab= "Percentage", xlab="Churn", col=c(2,4,6))
prop.table((bp), 2)
bp

bp <- table(Contract, Churn)
par(mfrow=c(1,2))
barplot(bp, beside=T, legend.text = rownames(bp), main="Churn by Having a Contract", ylab= "Amount of", xlab="Churn", col=c(2,4,6))
barplot(prop.table((bp), 2), beside=T, legend.text = rownames(bp), main="Churn by Having a Contract", ylab= "Percentage", xlab="Churn", col=c(2,4,6))
prop.table((bp), 2)
bp

bp <- table(PaperlessBilling, Churn)
par(mfrow=c(1,2))
barplot(bp, beside=T, legend.text = rownames(bp), main="Churn by Having Paperless Billing", ylab= "Amount of", xlab="Churn", col=c(2,4))
barplot(prop.table((bp), 2), beside=T, legend.text = rownames(bp), main="Churn by Having Paperless Billing", ylab= "Percentage", xlab="Churn", col=c(2,4))
prop.table((bp), 2)
bp

bp <- table(PaymentMethod, Churn)
par(mfrow=c(1,2))
barplot(bp, beside=T, legend.text = rownames(bp), main="Churn by Payment Method", ylab= "Amount of", xlab="Churn", col=c(2,4,6,8))
barplot(prop.table((bp), 2), beside=T, legend.text = rownames(bp), main="Churn by Payment Method", ylab= "Percentage", xlab="Churn", col=c(2,4,6,8))
prop.table((bp), 2)
bp
########ANOVA################
t.test(tenure~Churn)
t.test(MonthlyCharges~Churn)
t.test(TotalCharges~Churn)

anova(lm(tenure~Churn))

shapiro.test(tenure[Churn=="Yes"])
shapiro.test(tenure[Churn=="No"])

shapiro.test(MonthlyCharges[Churn=="Yes"])
shapiro.test(MonthlyCharges[Churn=="No"])

shapiro.test(TotalCharges[Churn=="Yes"])
shapiro.test(TotalCharges[Churn=="No"])

bartlett.test(tenure~Churn)
bartlett.test(MonthlyCharges~Churn)
bartlett.test(TotalCharges~Churn)

churn.logistic.model <- glm(Churn ~ gender + SeniorCitizen + Partner + Dependents + tenure + MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges + TotalCharges, family = "binomial")
summary(churn.logistic.model)
##Variable Reduction
churn.logistic.model <- glm(Churn ~ SeniorCitizen + Dependents + tenure + MultipleLines + InternetService + OnlineSecurity + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod + TotalCharges, family = "binomial")
summary(churn.logistic.model)
#Grab random Sample
churn.data[sample(nrow(churn.data), 1),]