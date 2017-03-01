## Use the following to load the entire project at once
load('KDD2009.Rdata')

######################
## Or do the real thing
######################

## Read data
d <- read.table(
    'orange_small_train.data.gz',
    header = T,
    sep = '\t',
    na.strings = c('NA', '') # treat NA and empty string both as NA
)

## Read churn data and add to d in a separate column
churn <- read.table('orange_small_train_churn.labels.txt',
                    header = F,
                    sep = '\t')
d$churn <- churn$V1

## Read appetency data and add to d in a separate column
appetency <- read.table('orange_small_train_appetency.labels.txt',
                        
                        header = F,
                        sep = '\t')
d$appetency <- appetency$V1

## Read upselling data and add to d in a separate column
upselling <- read.table('orange_small_train_upselling.labels.txt',
                        header = F,
                        sep = '\t')
d$upselling <- upselling$V1

## Set seed to make the work reproducible
set.seed(729375)

## Add column with a random number to divide the set into Train and Test data
## dim()[[1]] returns the number of rows
## runif() give a random uniform distribution
d$rgroup <- runif(dim(d)[[1]]) 
dTrainAll <- subset(d, rgroup <= 0.9)
dTest <- subset(d, rgroup > 0.9)

## Define the outcomes as a list
outcomes = c('churn', 'appetency', 'upselling')

## Set vars to determine their class; we only need the variabels that are not created by ourselves
## Determine categorical variable (their class is a factor or character)
## Or a numeric variable (which is a numeric or integer)
vars <- setdiff(colnames(dTrainAll),
                c(outcomes, 'rgroup'))
catVars <- vars[sapply(dTrainAll[, vars], class) %in%
                    c('factor', 'character')]
numericVars <- vars[sapply(dTrainAll[, vars], class) %in%
                        c('numeric', 'integer')]

## Remove unnecessary object
rm(list = c('d', 'churn', 'appetency', 'upselling'))

## Determine the outcome we want to make a model on and what the positive values is
outcome <- 'churn'
pos <- '1'

## Split the Trainings set into 2 sets: Training and Calibration
## Create a random binominal distribution (of size 1 with prob 0.1)
## This will result in a random distribution of 90% 0 and 10% 1.
## Split based on those values that are larger than 1.
## Assign the 10% to the calibration set; the other 90% to the training set
## For an explanation on rbinom see http://www.stats.uwo.ca/faculty/braun/RTricks/basics/BasicRIV.pdf
useForCal <- rbinom(n = dim(dTrainAll)[[1]],
                    size = 1,
                    prob = 0.1) > 0
dCal <- subset(dTrainAll, useForCal)
dTrain <- subset(dTrainAll, !useForCal)

## Listing 6.2 Plotting churn grouped by variable 218 levels
## useNA = ifany 
table218 <- table(
    Var218=dTrain[,'Var218'],
    churn=dTrain[,outcome],
    useNA='ifany')
print(table218)

## Use the following to see proportions
prop.table(table218, 2)

## Improve using the gmodels package
# library(gmodels)
# CrossTable(table218)

## Listing 6.3 Churn rates grouped by variable 218 codes
## Take every element in the second column and divide it 
## with the total for each element of both columns
## This way you determine the proportion of churning
print(table218[,2]/(table218[,1]+table218[,2]))

## Listing 6.4 Function to build single-variable models for categorical variables
## As the NA value is very predictive for churning,
## we want to turn into a level and use it for modelling
## 

mkPredC <- function(outCol,varCol,appCol) { #1
    pPos <- sum(outCol==pos)/length(outCol) #2
    naTab <- table(as.factor(outCol[is.na(varCol)]))
    pPosWna <- (naTab/sum(naTab))[pos] #3
    vTab <- table(as.factor(outCol),varCol)
    pPosWv <- (vTab[pos,]+1.0e-3*pPos)/(colSums(vTab)+1.0e-3) #4 
    pred <- pPosWv[appCol] #5
    pred[is.na(appCol)] <- pPosWna #6
    pred[is.na(pred)] <- pPos #7
    pred #8
}

# 1. Given a vector of training outcomes (outCol), a categorical training variable
# (varCol), and a prediction variable (appCol), use outCol and varCol to build a
# single-variable model and then apply the model to appCol to get new
# predictions.

# 2. Get stats appCol on how often outcome is positive during training.

# 3. Get stats on how often outcome is positive for NA values of variable during
# training.

# 4. Get stats on how often outcome is positive, conditioned on levels of training
# variable. (1.0e-3 = 1 x 10 tot de macht -3 = 0,001)

# 5. Make predictions by looking up levels of appCol.

# 6. Add in predictions for NA levels of appCol.

# 7. Add in predictions for levels of appCol that weren’t known during training.

# 8. Return vector of predictions.

## Listing 6.5 Applying single-categorical variable models to all of our datasets
for(v in catVars) {
    pi <- paste('pred',v,sep='')
    dTrain[,pi] <- mkPredC(dTrain[,outcome],dTrain[,v],dTrain[,v])
    dCal[,pi]   <- mkPredC(dTrain[,outcome],dTrain[,v],dCal[,v])
    dTest[,pi]  <- mkPredC(dTrain[,outcome],dTrain[,v],dTest[,v])
}

# Listing 6.6 Scoring categorical variables by AUC
library('ROCR')
calcAUC <- function(predcol,outcol) {
    perf <- performance(prediction(predcol,outcol==pos),'auc')
    as.numeric(perf@y.values)
}
for(v in catVars) {
    pi <- paste('pred',v,sep='')
    aucTrain <- calcAUC(dTrain[,pi],dTrain[,outcome])
    if(aucTrain>=0.8) {
        aucCal <- calcAUC(dCal[,pi],dCal[,outcome])
        print(sprintf("%s, trainAUC: %4.3f calibrationAUC: %4.3f",
                      pi,aucTrain,aucCal))
    }
}

# Listing 6.7 Scoring numeric variables by AUC
# Create bins for a numeric variable, using the quantile function
mkPredN <- function(outCol,varCol,appCol) {
    cuts <- unique(as.numeric(quantile(varCol,
                                       probs=seq(0, 1, 0.1),na.rm=T)))
    varC <- cut(varCol,cuts)
    appC <- cut(appCol,cuts)
    mkPredC(outCol,varC,appC)
}

for(v in numericVars) {
    pi <- paste('pred',v,sep='')
    dTrain[,pi] <- mkPredN(dTrain[,outcome],dTrain[,v],dTrain[,v])
    dTest[,pi] <- mkPredN(dTrain[,outcome],dTrain[,v],dTest[,v])
    dCal[,pi] <- mkPredN(dTrain[,outcome],dTrain[,v],dCal[,v])
    aucTrain <- calcAUC(dTrain[,pi],dTrain[,outcome])
    if(aucTrain>=0.55) {
        aucCal <- calcAUC(dCal[,pi],dCal[,outcome])
        print(sprintf("%s, trainAUC: %4.3f calibrationAUC: %4.3f",
                      pi,aucTrain,aucCal))
    }
}

# Plot the results Listing 6.8 Plotting variable performance
ggplot(data=dCal) +
    geom_density(aes(x=predVar126,color=as.factor(churn)))

# Cross Validation
# Listing 6.9 Running a repeated cross-validation experiment
# 100-fold

# Set varialbe
var <- 'Var217'

# Set amount of repetitions ( = x-fold)
aucs <- rep(0,100)

# Loop over the repetitions
# Change the calibration set / hold-oud set every time
for(rep in 1:length(aucs)) {
    useForCalRep <- rbinom(n=dim(dTrainAll)[[1]],size=1,prob=0.1)>0
    predRep <- mkPredC(dTrainAll[!useForCalRep,outcome],
                       dTrainAll[!useForCalRep,var],
                       dTrainAll[useForCalRep,var])
    aucs[rep] <- calcAUC(predRep,dTrainAll[useForCalRep,outcome])
}

# Get the mean and standard deviation
mean(aucs)
sd(aucs)

# Listing 6.10 Empirically cross-validating performance
fCross <- function() {
    useForCalRep <- rbinom(n=dim(dTrainAll)[[1]],size=1,prob=0.1)>0
    predRep <- mkPredC(dTrainAll[!useForCalRep,outcome],
                       dTrainAll[!useForCalRep,var],
                       dTrainAll[useForCalRep,var])
    calcAUC(predRep,dTrainAll[useForCalRep,outcome])
}
aucs <- replicate(100,fCross())

# Get the mean and standard deviation
mean(aucs)
sd(aucs)

# Listing 6.11 Basic variable selection

# Maak een functie om de log likelyhood te berekenen
logLikelyhood <-
    function(outCol,predCol) {
        sum(ifelse(outCol==pos,log(predCol),log(1-predCol)))
    }
selVars <- c()
minStep <- 5

# Bepaal de baserate op basis van de uitkomsten
baseRateCheck <- logLikelyhood(dCal[,outcome],
                               sum(dCal[,outcome]==pos)/length(dCal[,outcome]))

# Run through categorical variables and pick based on a deviance improvement 
# (related to difference in log likelihoods; see chapter 3).
for(v in catVars) {
    pi <- paste('pred',v,sep='')
    liCheck <- 2*((logLikelyhood(dCal[,outcome],dCal[,pi]) -
                       baseRateCheck))
    if(liCheck>minStep) {
        print(sprintf("%s, calibrationScore: %g",
                      pi,liCheck))
        selVars <- c(selVars,pi)
    }
}

# Run through categorical variables and pick based on a deviance improvement.
for(v in numericVars) {
    pi <- paste('pred',v,sep='')
    liCheck <- 2*((logLikelyhood(dCal[,outcome],dCal[,pi]) -
                       baseRateCheck) - 1)
    if(liCheck>=minStep) {
        print(sprintf("%s, calibrationScore: %g",
                      pi,liCheck))
        selVars <- c(selVars,pi)
    }
}

## DECISION TREES
# Listing 6.13 Building a bad decision tree

library('rpart')
fV <- paste(outcome,'>0 ~ ',
              paste(c(catVars,numericVars),collapse=' + '),sep='')
tmodel <- rpart(fV,data=dTrain)
print(calcAUC(predict(tmodel,newdata=dTrain),dTrain[,outcome]))
print(calcAUC(predict(tmodel,newdata=dTest),dTest[,outcome]))
print(calcAUC(predict(tmodel,newdata=dCal),dCal[,outcome]))

#Listing 6.14 Building another bad decision tree
tVars <- paste('pred',c(catVars,numericVars),sep='')
fV2 <- paste(outcome,'>0 ~ ',paste(tVars,collapse=' + '),sep='')
tmodel <- rpart(fV2,data=dTrain)
print(calcAUC(predict(tmodel,newdata=dTrain),dTrain[,outcome]))
print(calcAUC(predict(tmodel,newdata=dTest),dTest[,outcome]))
print(calcAUC(predict(tmodel,newdata=dCal),dCal[,outcome]))

#Listing 6.15 Building yet another bad decision tree
tmodel <- rpart(fV2,data=dTrain,
                  control=rpart.control(cp=0.001,minsplit=1000,
                                        minbucket=1000,maxdepth=5)
)
print(calcAUC(predict(tmodel,newdata=dTrain),dTrain[,outcome]))
print(calcAUC(predict(tmodel,newdata=dTest),dTest[,outcome]))
print(calcAUC(predict(tmodel,newdata=dCal),dCal[,outcome]))

# Listing 6.16 Building a better decision tree
f <- paste(outcome,'>0 ~ ',paste(selVars,collapse=' + '),sep='')
tmodel <- rpart(f,data=dTrain,
                  control=rpart.control(cp=0.001,minsplit=1000,
                                        minbucket=1000,maxdepth=5)
)
print(calcAUC(predict(tmodel,newdata=dTrain),dTrain[,outcome]))
print(calcAUC(predict(tmodel,newdata=dTest),dTest[,outcome]))
print(calcAUC(predict(tmodel,newdata=dCal),dCal[,outcome]))

# Listing 6.17 Printing the decision tree
print(tmodel)

# Listing 6.18 Plotting the decision tree
par(cex=0.7)
plot(tmodel)
text(tmodel)

## KNN - K NEAREST NEIGHBOR
# Listing 6.19 Running k-nearest neighbors

library('class')
nK <- 200

# Build a data frame with only the variables we wish to use for classification.
knnTrain <- dTrain[,selVars]

# Build a vector with the known training outcomes
knnCl <- dTrain[,outcome]==pos

# Bind the knn() training function with our data in a new function.
# Convert knn’s unfortunate convention of calculating probability as “proportion
# of the votes for the winning class” into the more useful “calculated 
# probability of being a positive example.”
knnPred <- function(df) {
    knnDecision <- knn(knnTrain,df,knnCl,k=nK,prob=T)
    ifelse(knnDecision==TRUE,
           attributes(knnDecision)$prob,
           1-(attributes(knnDecision)$prob))
}
print(calcAUC(knnPred(dTrain[,selVars]),dTrain[,outcome]))
print(calcAUC(knnPred(dCal[,selVars]),dCal[,outcome]))
print(calcAUC(knnPred(dTest[,selVars]),dTest[,outcome]))

#Listing 6.20 Platting 200-nearest neighbor performance
dCal$kpred <- knnPred(dCal[,selVars])
ggplot(data=dCal) +
    geom_density(aes(x=kpred,
                     color=as.factor(churn),linetype=as.factor(churn)))

# Listing 6.21 Plotting the receiver operating characteristic curve
plotROC <- function(predcol,outcol) {
    perf <- performance(prediction(predcol,outcol==pos),'tpr','fpr')
    pf <- data.frame(
        FalsePositiveRate=perf@x.values[[1]],
        TruePositiveRate=perf@y.values[[1]])
    ggplot() +
        geom_line(data=pf,aes(x=FalsePositiveRate,y=TruePositiveRate)) +
        geom_line(aes(x=c(0,1),y=c(0,1)))
}
print(plotROC(knnPred(dTest[,selVars]),dTest[,outcome]))

# Listing 6.22 Plotting the performance of a logistic regression model
gmodel <- glm(as.formula(f),data=dTrain,family=binomial(link='logit'))
print(calcAUC(predict(gmodel,newdata=dTrain),dTrain[,outcome]))
print(calcAUC(predict(gmodel,newdata=dTest),dTest[,outcome]))
print(calcAUC(predict(gmodel,newdata=dCal),dCal[,outcome]))

# Listing 6.23 Building, applying, and evaluating a Naive Bayes model
pPos <- sum(dTrain[,outcome]==pos)/length(dTrain[,outcome])

# Define a function that performs the Naive Bayes prediction
nBayes <- function(pPos,pf) {
    pNeg <- 1 - pPos
    smoothingEpsilon <- 1.0e-5

    # For each row, compute (with a smoothing term) the sum of log(P[positive &
    # evidence_i]/ P[positive]) across all columns. This is equivalent to the log of
    # the product of P[evidence_i | positive] up to terms that don’t depend on the
    # positive/negative outcome.
    scorePos <- log(pPos + smoothingEpsilon) +
        rowSums(log(pf/pPos + smoothingEpsilon))
    
    # For each row, compute (with a smoothing term) the
    # sum of log(P[negative & evidence_i]/P[negative])
    # across all columns. This is equivalent to the log of
    # the product of P[evidence_i | negative] up to terms
    # that don’t depend on the positive/negative outcome.
    scoreNeg <- log(pNeg + smoothingEpsilon) +
        rowSums(log((1-pf)/(1-pPos) + smoothingEpsilon))
    m <- pmax(scorePos,scoreNeg)
    
    # Exponentiate to turn sums back into products, but make sure we don’t cause
    # a floating point overflow in doing so.
    expScorePos <- exp(scorePos-m)
    expScoreNeg <- exp(scoreNeg-m)
    expScorePos/(expScorePos+expScoreNeg)
}
pVars <- paste('pred',c(numericVars,catVars),sep='')
dTrain$nbpredl <- nBayes(pPos,dTrain[,pVars])
dCal$nbpredl <- nBayes(pPos,dCal[,pVars])
dTest$nbpredl <- nBayes(pPos,dTest[,pVars])
print(calcAUC(dTrain$nbpredl,dTrain[,outcome]))
print(calcAUC(dCal$nbpredl,dCal[,outcome]))
print(calcAUC(dTest$nbpredl,dTest[,outcome]))

# en nu hebben we een wijziging