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

# Skewness and type
skewness(cardiology$age)
skewness(cardiology$trestbps)
skewness(cardiology$cholesterol, na.rm = TRUE)
skewness(cardiology$diastbpexerc)
skewness(cardiology$thalach)
skewness(cardiology$oldpeak)
skewness(cardiology$ca)


# level of correlation for age with other predictor variables
cor(cardiology$age, cardiology$trestbps)
cor(cardiology$age, cardiology$cholesterol, use = "complete.obs")
cor(cardiology$age, cardiology$thalach)
cor(cardiology$age, cardiology$oldpeak)
cor(cardiology$age, cardiology$diastbpexerc)

ggplot(cardiology, aes(x=age, y=trestbps)) + geom_point()
ggplot(cardiology, aes(x=age, y=cholesterol)) + geom_point()
ggplot(cardiology, aes(x=age, y=thalach)) + geom_point()
ggplot(cardiology, aes(x=age, y=oldpeak)) + geom_point()
ggplot(cardiology, aes(x=age, y=diastbpexerc)) + geom_point()

# level of correlation for trestbps with other predictor variables
cor(cardiology$trestbps, cardiology$age)
cor(cardiology$trestbps, cardiology$cholesterol, use = "complete.obs")
cor(cardiology$trestbps, cardiology$thalach)
cor(cardiology$trestbps, cardiology$oldpeak)
cor(cardiology$trestbps, cardiology$diastbpexerc)

ggplot(cardiology, aes(x=trestbps, y=age)) + geom_point()
ggplot(cardiology, aes(x=trestbps, y=cholesterol)) + geom_point()
ggplot(cardiology, aes(x=trestbps, y=thalach)) + geom_point()
ggplot(cardiology, aes(x=trestbps, y=oldpeak)) + geom_point()
ggplot(cardiology, aes(x=trestbps, y=diastbpexerc)) + geom_point()

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

# Detect outliers in numerical variables
cardiologyNumeric <- read.table(file="C:/RCA1/CardiologyNumeric.csv", stringsAsFactors=FALSE, sep=",", header=TRUE)
outlier(cardiologyNumeric)



