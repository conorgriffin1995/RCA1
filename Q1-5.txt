cardiology <- read.table(file="C:/RCA1/CardiologyRel.csv", stringsAsFactors=FALSE, sep=",", header=TRUE)

# show the first 10 records
head(cardiology)
cardiology[1:10,]

# show the first 12 records and first 3 columns
cardiology[1:12,1:3]

#Create a table of counts for each discrete value of outlook
A <- cardiology$sex
ZZ <- table(A)
ZZ

# Let's show the data as a piechart - not the best visualisation to use!
# Good for getting a rough idea of proportions
pie(ZZ, labels=names(ZZ), edges=200, col=c("yellow","red","navy","green"), radius=
      0.9)

# Part 1 Question 1 B
# % of missing values
sum(is.na(cardiology$age))
sum(is.na(cardiology$sex))
sum(is.na(cardiology$cp))
sum(is.na(cardiology$trestbps))
sum(is.na(cardiology$cholesterol))
sum(is.na(cardiology$Fasting.blood.sugar...120))
sum(is.na(cardiology$restecg))
sum(is.na(cardiology$diastbpexerc))
sum(is.na(cardiology$thalach))
sum(is.na(cardiology$exang))
sum(is.na(cardiology$oldpeak))
sum(is.na(cardiology$slope))
sum(is.na(cardiology$ca))
sum(is.na(cardiology$thal))
sum(is.na(cardiology$class))

# Part 1 Question 1 C
# Finding MAX 
max(cardiology$age, na.rm=TRUE)
max(cardiology$sex, na.rm=TRUE)
max(cardiology$cp, na.rm=TRUE)
max(cardiology$trestbps, na.rm=TRUE)
max(cardiology$cholesterol, na.rm = TRUE)
max(cardiology$Fasting.blood.sugar...120, na.rm=TRUE)
max(cardiology$restecg, na.rm = TRUE)
max(cardiology$diastbpexerc, na.rm = TRUE)
max(cardiology$thalach, na.rm = TRUE)
max(cardiology$exang, na.rm = TRUE)
max(cardiology$oldpeak, na.rm=TRUE)
max(cardiology$slope, na.rm = TRUE)
max(cardiology$ca, na.rm = TRUE)
max(cardiology$thal, na.rm = TRUE)
max(cardiology$class, na.rm = TRUE)

# Part 1 Question 1 C
# Finding MIN 
min(cardiology$age, na.rm=TRUE)
min(cardiology$sex, na.rm=TRUE)
min(cardiology$cp, na.rm=TRUE)
min(cardiology$trestbps, na.rm=TRUE)
min(cardiology$cholesterol, na.rm = TRUE)
min(cardiology$Fasting.blood.sugar...120, na.rm=TRUE)
min(cardiology$restecg, na.rm=TRUE)
min(cardiology$diastbpexerc, na.rm = TRUE)
min(cardiology$thalach, na.rm = TRUE)
min(cardiology$exang, na.rm=TRUE)
min(cardiology$oldpeak, na.rm=TRUE)
min(cardiology$slope, na.rm=TRUE)
min(cardiology$ca, na.rm=TRUE)
min(cardiology$thal, na.rm=TRUE)
min(cardiology$class, na.rm=TRUE)

# Part 1 Question 1 C
# Finding MEAN 
mean(cardiology$age, na.rm = TRUE)
mean(cardiology$sex, na.rm = TRUE)
mean(cardiology$cp, na.rm = TRUE)
mean(cardiology$trestbps, na.rm = TRUE)
mean(cardiology$cholesterol, na.rm = TRUE)
mean(cardiology$Fasting.blood.sugar...120, na.rm = TRUE)
mean(cardiology$restecg, na.rm = TRUE)
mean(cardiology$diastbpexerc, na.rm = TRUE)
mean(cardiology$thalach, na.rm = TRUE)
mean(cardiology$exang, na.rm = TRUE)
mean(cardiology$oldpeak, na.rm = TRUE)
mean(cardiology$slope, na.rm = TRUE)
mean(cardiology$ca, na.rm = TRUE)
mean(cardiology$thal, na.rm = TRUE)
mean(cardiology$class, na.rm = TRUE)

# Part 1 Question 1 C
# Finding Mode Function
Mode <- function(cardiology){
  ux <- unique(cardiology)
  return (ux[which.max(tabulate(match(cardiology, ux)))])
}

Mode(cardiology$age)
Mode(cardiology$sex)
Mode(cardiology$cp)
Mode(cardiology$trestbps)
Mode(cardiology$cholesterol)
Mode(cardiology$Fasting.blood.sugar...120)
Mode(cardiology$restecg)
Mode(cardiology$diastbpexerc)
Mode(cardiology$thalach)
Mode(cardiology$exang)
Mode(cardiology$oldpeak)
Mode(cardiology$slope)
Mode(cardiology$ca)
Mode(cardiology$thal)
Mode(cardiology$class)

# Part 1 Question 1 C
# Finding Median
median(cardiology$age, na.rm=TRUE)
median(cardiology$sex, na.rm=TRUE)
median(cardiology$cp, na.rm=TRUE)
median(cardiology$trestbps, na.rm=TRUE)
median(cardiology$cholesterol, na.rm = TRUE)
median(cardiology$restecg, na.rm=TRUE)
median(cardiology$diastbpexerc, na.rm = TRUE)
median(cardiology$thalach, na.rm = TRUE)
median(cardiology$exang, na.rm=TRUE)
median(cardiology$oldpeak, na.rm=TRUE)
median(cardiology$slope, na.rm=TRUE)
median(cardiology$ca, na.rm=TRUE)
median(cardiology$thal, na.rm=TRUE)
median(cardiology$class, na.rm=TRUE)

# Part 1 Question 1 C
#Finding Standard Deviation
sd(cardiology$age)
sd(cardiology$sex)
sd(cardiology$cp)
sd(cardiology$trestbps)
sd(cardiology$cholesterol)
sd(cardiology$Fasting.blood.sugar...120)
sd(cardiology$restecg)
sd(cardiology$diastbpexerc)
sd(cardiology$thalach)
sd(cardiology$exang)
sd(cardiology$oldpeak)
sd(cardiology$slope)
sd(cardiology$ca)
sd(cardiology$thal)
sd(cardiology$class)

# Part 1 Question 1 D
# Shapiro-Wilks test for Normality
shapiro.test(cardiology$age)            # p-value = 0.0007914 -> Deviates From Normality
shapiro.test(cardiology$trestbps)       # p-value = 8.431e-07 -> Normal Distribution
shapiro.test(cardiology$cholesterol)    # p-value = 4.548e-09 -> Normal Distribution
shapiro.test(cardiology$diastbpexerc)   # p-value = 3.104e-07 -> Normal Distribution
shapiro.test(cardiology$thalach)        # p-value = 5.608e-05 -> Normal Distribution
# https://stats.stackexchange.com/questions/173893/interpreting-p-value-2-2e-16-in-r
# oldpeak is < 2.2e-16 means 0.00000000000000022. It is (very much) less than 0.05
shapiro.test(cardiology$oldpeak)        # p-value < 2.2e-16 -> Deviates From Normal Distribution
shapiro.test(cardiology$ca)             # p-value < 2.2e-16 -> Deviates From Normality

# Part 1 Question 1 E
# Skewness and type
skewness(cardiology$age)
skewness(cardiology$trestbps)
skewness(cardiology$cholesterol, na.rm = TRUE)
skewness(cardiology$diastbpexerc)
skewness(cardiology$thalach)
skewness(cardiology$oldpeak)
skewness(cardiology$ca)

# Part 1 Question 1 F
# 1 indicates a strong positive relationship.
# -1 indicates a strong negative relationship.
# A result of zero indicates no relationship at all.
# level of correlation for age with other predictor variables
cor(cardiology$age, cardiology$trestbps)
cor(cardiology$age, cardiology$cholesterol, use = "complete.obs")
cor(cardiology$age, cardiology$thalach)
cor(cardiology$age, cardiology$oldpeak)
cor(cardiology$age, cardiology$diastbpexerc)

# level of correlation for trestbps with other predictor variables
cor(cardiology$trestbps, cardiology$age)
cor(cardiology$trestbps, cardiology$cholesterol, use = "complete.obs")
cor(cardiology$trestbps, cardiology$thalach)
cor(cardiology$trestbps, cardiology$oldpeak)
cor(cardiology$trestbps, cardiology$diastbpexerc)

# level of correlation for diastbpexerc with other predictor variables
cor(cardiology$diastbpexerc, cardiology$age)
cor(cardiology$diastbpexerc, cardiology$trestbps)
cor(cardiology$diastbpexerc, cardiology$cholesterol, use = "complete.obs")
cor(cardiology$diastbpexerc, cardiology$thalach)
cor(cardiology$diastbpexerc, cardiology$oldpeak)

# level of correlation for thalach with other predictor variables
cor(cardiology$thalach, cardiology$age)
cor(cardiology$thalach, cardiology$trestbps)
cor(cardiology$thalach, cardiology$cholesterol, use = "complete.obs")
cor(cardiology$thalach, cardiology$diastbpexerc)
cor(cardiology$thalach, cardiology$oldpeak)

# level of correlation for oldpeak with other predictor variables
cor(cardiology$oldpeak, cardiology$age)
cor(cardiology$oldpeak, cardiology$trestbps)
cor(cardiology$oldpeak, cardiology$cholesterol, use = "complete.obs")
cor(cardiology$oldpeak, cardiology$diastbpexerc)
cor(cardiology$oldpeak, cardiology$thalach)

# level of correlation  for cholesterol with other predictor variables
cor(cardiology$cholesterol, cardiology$age, use = "complete.obs")
cor(cardiology$cholesterol, cardiology$trestbps, use = "complete.obs")
cor(cardiology$cholesterol, cardiology$thalach, use = "complete.obs")
cor(cardiology$cholesterol, cardiology$oldpeak, use = "complete.obs")
cor(cardiology$cholesterol, cardiology$diastbpexerc, use = "complete.obs")

# level of correlation for ca with other predictor variables
cor(cardiology$ca, cardiology$age)
cor(cardiology$ca, cardiology$trestbps)
cor(cardiology$ca, cardiology$cholesterol)
cor(cardiology$ca, cardiology$cholesterol, use = "complete.obs")
cor(cardiology$ca, cardiology$diastbpexerc)
cor(cardiology$ca, cardiology$thalach)

# Part 1 Question 2
# Histogram for each numerical variable, with an overlay of the target variable
# Histogram for Age
ggplot(cardiology, aes(x = age, fill = class)) + geom_histogram() + ggtitle("Age of patients") + labs(x = "Age of patients", y = "Number of patients") + theme_bw()

# Histogram for Trestbps (Resting blood pressure of patients)
ggplot(cardiology, aes(x = trestbps, fill = class)) + geom_histogram() + ggtitle("Resting blood pressure of patients") + labs(x = "Resting blood pressure of patients", y = "Number of patients") + theme_bw()

# Histogram for Cholesterol
ggplot(cardiology, aes(x = cholesterol, fill = class)) + geom_histogram() + ggtitle("Cholesterol of patients") + labs(x = "Cholesterol of patients", y = "Number of patients") + theme_bw()

# Histogram for Diastbpexerc (Diastolic blood pressure of patients)
ggplot(cardiology, aes(x = diastbpexerc, fill = class)) + geom_histogram() + ggtitle("Diastolic blood pressure of patients") + labs(x = "Diastolic blood pressure of patients", y = "Number of patients") + theme_bw()

# Histogram for Thalach (Maximum heart rate achieved for patients)
ggplot(cardiology, aes(x = thalach, fill = class)) + geom_histogram() + ggtitle("Maximum heart rate achieved for patients") + labs(x = "Maximum heart rate achieved for patients", y = "Number of patients") + theme_bw()

# Histogram for Oldpeak (ST depression induced by exercise relative to rest of patients)
ggplot(cardiology, aes(x = oldpeak, fill = class)) + geom_histogram() + ggtitle("ST depression induced by exercise relative to rest of patients") + labs(x = "ST depression induced by exercise relative to rest of patients", y = "Number of patients") + theme_bw()

# Histogram for ca (Number of major vessels)
ggplot(cardiology, aes(x = ca, fill = class)) + geom_histogram() + ggtitle("Number of major vessels") + labs(x = "ST depression induced by exercise relative to rest of patients", y = "Number of patients") + theme_bw()
 
# Part 1 Question 3
# BarChart for each categorical variable,with an overlay of the target variable
# Bar chart for each sex
ggplot(cardiology, aes(x = sex, fill = class)) + geom_bar() + ggtitle("Sex of patients") + labs(x = "Sex of patient", y = "Number of patients") + theme_bw()

# Bar chart for each cp
ggplot(cardiology, aes(x = cp, fill = class)) + geom_bar() + ggtitle("Chest pain type") + labs(x = "Chest pain type", y = "Number of patients") + theme_bw()

# Bar chart for each Fasting blood sugar
ggplot(cardiology, aes(x = Fasting.blood.sugar...120, fill = class)) + geom_bar() + ggtitle("Fasting blood sugar") + labs(x = "Fasting blood sugar", y = "Number of patients") + theme_bw()

# Bar chart for each restecg
ggplot(cardiology, aes(x = restecg, fill = class)) + geom_bar() + ggtitle("Resting electrocardiographic results") + labs(x = "Resting electrocardiographic results", y = "Number of patients") + theme_bw()

# Bar chart for each exang
ggplot(cardiology, aes(x = exang, fill = class)) + geom_bar() + ggtitle("Exercise induced angina") + labs(x = "Exercise induced angina ", y = "Number of patients") + theme_bw()

# Bar chart for each slope
ggplot(cardiology, aes(x = slope, fill = class)) + geom_bar() + ggtitle("The slope of the peak exercise ST segment") + labs(x = "The slope of the peak exercise ST segment", y = "Number of patients") + theme_bw()

# Bar chart for each thal
ggplot(cardiology, aes(x = thal, fill = class)) + geom_bar() + ggtitle("Thal") + labs(x = "Thal", y = "Number of patients") + theme_bw()

# Part 1, Question 4:
# Detect outliers in numerical variables
cardiologyNumeric <- read.table(file="C:/RCA1/CardiologyNumeric.csv", stringsAsFactors=FALSE, sep=",", header=TRUE)
outlier(cardiologyNumeric)

# Graphical means of finding outliers.
# Box Plot of Age vs. Age.
ggplot(cardiology, aes(x = age, y = classNumeric)) + geom_boxplot()

# Box Plot of Trestbps vs. Trestbps.
ggplot(cardiologyNumeric, aes(x = trestbps, y = trestbps)) + geom_boxplot()

# Box Plot of Cholesterol vs. Cholesterol.
ggplot(cardiologyNumeric, aes(x = cholesterol, y = cholesterol)) + geom_boxplot() 

# Box Plot of Diastbpexercvs. Diastbpexerc.
ggplot(cardiologyNumeric, aes(x = diastbpexerc, y = diastbpexerc)) + geom_boxplot()

# Box Plot of Thalach vs. Thalach.
ggplot(cardiologyNumeric, aes(x = thalach, y = thalach)) + geom_boxplot()

# Box Plot of Oldpeak vs. Oldpeak.
ggplot(cardiologyNumeric, aes(x = oldpeak, y = oldpeak)) + geom_boxplot()

# Box Plot of Ca vs. Ca.
ggplot(cardiologyNumeric, aes(x = ca, y = ca)) + geom_boxplot()

# Statistical means of finding outliers.

# Made this function to find the outliers of a numeric column.
# This takes in the attribute and the column number in order to find and print out the outliers.
# It finds the IQR using the same methods we did in class by getting Q3 - Q1.
# It then gets the upper and lower bounds (-1.5 IQR and 1.5 IQR) and displays these bounds to the screen.
# Lastly, it displays all the numeric data outliers from that attribute that are 
# greater than the upper bound and less than the lower bound.
geneterateOutliers <- function(arg1, column){
  # Get your IQR (Interquartile range) and lower/upper quartile using:
  lowerq = quantile(arg1)[2]
  upperq = quantile(arg1)[4]
  iqr = upperq - lowerq #Or use IQR(data)
  
  # Compute the bounds for a mild outlier:
  threshold.upper = (iqr * 1.5) + upperq
  threshold.lower = lowerq - (iqr * 1.5)
  
  # Any age less than the threshold.lower value is an outlier
  threshold.lower
  # Any age greater than the threshold.upper value is an outlier
  threshold.upper
  
  print(paste0("Lower outlier bound: ", threshold.lower))
  print(paste0("Upper outlier bound: ", threshold.upper))
  
  GetUpperOutliers <- cardiologyNumeric[which(cardiologyNumeric[,column]>threshold.upper),column]
  print(paste0("Upper outliers:"))
  print(GetUpperOutliers)
  # IF statement to deal with when there are no Upper outliers integer(0)
  
  GetLowerOutliers <- cardiologyNumeric[which(cardiologyNumeric[,column]<threshold.lower),column]
  print(paste0("Lower Outliers:"))
  print(GetLowerOutliers)
  # IF statement to deal with when there are no Lower outliers integer(0)
}

# Generate outliers for all numeric attributes.
geneterateOutliers(cardiologyNumeric$age, 1)
geneterateOutliers(cardiologyNumeric$trestbps,2)
geneterateOutliers(cardiologyNumeric$cholesterol,3)
geneterateOutliers(cardiologyNumeric$diastbpexerc, 4)
geneterateOutliers(cardiologyNumeric$thalach, 5)
geneterateOutliers(cardiologyNumeric$oldpeak, 6)
geneterateOutliers(cardiologyNumeric$ca, 7)

# Z-Score to detect outliers
zscoreAge <- (cardiologyNumeric$age - mean(cardiologyNumeric$age, na.rm = TRUE)) / sd(cardiology$age)
zscoreAge

# Part 1 Question 5 A
# Investigate whether there are any correlated variables. Using Scatter Plots
# Age plots
ggplot(cardiology, aes(x=age, y=trestbps)) + geom_point()
ggplot(cardiology, aes(x=age, y=cholesterol)) + geom_point()
ggplot(cardiology, aes(x=age, y=thalach)) + geom_point()
ggplot(cardiology, aes(x=age, y=oldpeak)) + geom_point()
ggplot(cardiology, aes(x=age, y=diastbpexerc)) + geom_point()

# trestbps plots
ggplot(cardiology, aes(x=trestbps, y=age)) + geom_point()
ggplot(cardiology, aes(x=trestbps, y=cholesterol)) + geom_point()
ggplot(cardiology, aes(x=trestbps, y=thalach)) + geom_point()
ggplot(cardiology, aes(x=trestbps, y=oldpeak)) + geom_point()
ggplot(cardiology, aes(x=trestbps, y=diastbpexerc)) + geom_point()

# diastbpexerc plots
ggplot(cardiology, aes(x=diastbpexerc, y=age)) + geom_point()
ggplot(cardiology, aes(x=diastbpexerc, y=trestbps)) + geom_point()
ggplot(cardiology, aes(x=diastbpexerc, y=cholesterol)) + geom_point()
ggplot(cardiology, aes(x=diastbpexerc, y=thalach)) + geom_point()
ggplot(cardiology, aes(x=diastbpexerc, y=oldpeak)) + geom_point()

# thalach plots
ggplot(cardiology, aes(x=thalach, y=age)) + geom_point()
ggplot(cardiology, aes(x=thalach, y=trestbps)) + geom_point()
ggplot(cardiology, aes(x=thalach, y=cholesterol)) + geom_point()
ggplot(cardiology, aes(x=thalach, y=diastbpexerc)) + geom_point()
ggplot(cardiology, aes(x=thalach, y=oldpeak)) + geom_point()

# oldpeak plots
ggplot(cardiology, aes(x=oldpeak, y=age)) + geom_point()
ggplot(cardiology, aes(x=oldpeak, y=trestbps)) + geom_point()
ggplot(cardiology, aes(x=oldpeak, y=cholesterol)) + geom_point()
ggplot(cardiology, aes(x=oldpeak, y=diastbpexerc)) + geom_point()
ggplot(cardiology, aes(x=oldpeak, y=thalach)) + geom_point()

# cholesterol plots
ggplot(cardiology, aes(x=cholesterol, y=age)) + geom_point()
ggplot(cardiology, aes(x=cholesterol, y=trestbps)) + geom_point()
ggplot(cardiology, aes(x=cholesterol, y=oldpeak)) + geom_point()
ggplot(cardiology, aes(x=cholesterol, y=diastbpexerc)) + geom_point()
ggplot(cardiology, aes(x=cholesterol, y=thalach)) + geom_point()

# Part 1 Question 5 B Verifying assertions 
# level of correlation for age with other predictor variables
cor(cardiology$age, cardiology$trestbps)
cor(cardiology$age, cardiology$cholesterol, use = "complete.obs")
cor(cardiology$age, cardiology$thalach)
cor(cardiology$age, cardiology$oldpeak)
cor(cardiology$age, cardiology$diastbpexerc)

# level of correlation for trestbps with other predictor variables
cor(cardiology$trestbps, cardiology$age)
cor(cardiology$trestbps, cardiology$cholesterol, use = "complete.obs")
cor(cardiology$trestbps, cardiology$thalach)
cor(cardiology$trestbps, cardiology$oldpeak)
cor(cardiology$trestbps, cardiology$diastbpexerc)

# level of correlation for diastbpexerc with other predictor variables
cor(cardiology$diastbpexerc, cardiology$age)
cor(cardiology$diastbpexerc, cardiology$trestbps)
cor(cardiology$diastbpexerc, cardiology$cholesterol, use = "complete.obs")
cor(cardiology$diastbpexerc, cardiology$thalach)
cor(cardiology$diastbpexerc, cardiology$oldpeak)

# level of correlation for thalach with other predictor variables
cor(cardiology$thalach, cardiology$age)
cor(cardiology$thalach, cardiology$trestbps)
cor(cardiology$thalach, cardiology$cholesterol, use = "complete.obs")
cor(cardiology$thalach, cardiology$diastbpexerc)
cor(cardiology$thalach, cardiology$oldpeak)

# level of correlation for oldpeak with other predictor variables
cor(cardiology$oldpeak, cardiology$age)
cor(cardiology$oldpeak, cardiology$trestbps)
cor(cardiology$oldpeak, cardiology$cholesterol, use = "complete.obs")
cor(cardiology$oldpeak, cardiology$diastbpexerc)
cor(cardiology$oldpeak, cardiology$thalach)

# level of correlation for cholesterol with other predictor variables
cor(cardiology$cholesterol, cardiology$age, use = "complete.obs")
cor(cardiology$cholesterol, cardiology$trestbps, use = "complete.obs")
cor(cardiology$cholesterol, cardiology$thalach, use = "complete.obs")
cor(cardiology$cholesterol, cardiology$oldpeak, use = "complete.obs")
cor(cardiology$cholesterol, cardiology$diastbpexerc, use = "complete.obs")

# Converting class to classNumeric 1 || 0
cardiology$classNumeric[cardiology$class=="Sick"]<-"0" 
cardiology$classNumeric[cardiology$class=="Healthy"]<-"1" 

