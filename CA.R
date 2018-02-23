cardiology <- read.table(file="C:/student/ENTDATABASETECHCA/CardiologyRel.csv", stringsAsFactors=FALSE, sep=",", header=TRUE)

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
