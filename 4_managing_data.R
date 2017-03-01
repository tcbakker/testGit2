library(dplyr)

custdata <- read.table('custdata.tsv',header=T,sep='\t')

# test if NA's are similar in alle cases
summary(custdata[is.na(custdata$housing.type),
                 c("recent.move","num.vehicles")])

# add missing to NA's in is.employed
custdata$is.employed.fix <- ifelse(is.na(custdata$is.employed),
                                   "missing",
                                   ifelse(custdata$is.employed==T,
                                          "employed",
                                          "not employed"))
summary(as.factor(custdata$is.employed.fix))

custdata$is.employed.fix <- ifelse(is.na(custdata$is.employed),
                                   "not in active workforce",
                                   ifelse(custdata$is.employed==T,
                                          "employed",
                                          "not employed"))

summary(as.factor(custdata$is.employed.fix))


# replace NA's in a numeric value
summary(custdata$income)

meanIncome <- mean(custdata$income, na.rm=T)
Income.fix <- ifelse(is.na(custdata$income),
                       meanIncome,
                       custdata$income)
summary(Income.fix)

# transform data cuts
brks <- c(0, 25, 65, Inf)
custdata$age.range <- cut(custdata$age,
                          breaks=brks, include.lowest=T)
summary(custdata$age.range)

# Listing 4.8 Summarizing age
summary(custdata$age)
meanage <- mean(custdata$age)
stdage <- sd(custdata$age)
meanage
stdage
custdata$age.normalized <- (custdata$age-meanage)/stdage
summary(custdata$age.normalized)

# Listing 4.9 assign random number to split set in training and test data
custdata$gp <- runif(dim(custdata)[1])

testSet <- subset(custdata, custdata$gp <= 0.1)
trainingSet <- subset(custdata, custdata$gp > 0.1)
dim(testSet)[1]
dim(trainingSet)[1]

# assign random with grouping
hh <- unique(hhdata$household_id)
households <- data.frame(household_id = hh, gp = runif(length(hh)))
hhdata <- merge(hhdata, households, by="household_id")
