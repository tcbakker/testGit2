## See https://www.filosophy.org/post/7/visual_algorithms_precision_and_recall/

# Listing 5.1 Building and applying a logistic regression spam model
spamD <- read.table('spamD.tsv',header=T,sep='\t')
spamTrain <- subset(spamD,spamD$rgroup>=10)
spamTest <- subset(spamD,spamD$rgroup<10)
spamVars <- setdiff(colnames(spamD),list('rgroup','spam'))
spamFormula <- as.formula(paste('spam=="spam"', paste(spamVars,collapse=' + '),sep=' ~ '))
# spamFormula

spamModel <- glm(spamFormula,family=binomial(link='logit'), data=spamTrain)
spamTrain$pred <- predict(spamModel,newdata=spamTrain, type='response')
spamTest$pred <- predict(spamModel,newdata=spamTest, type='response')
print(with(spamTest,table(y=spam,glmPred=pred>0.5)))

# Listing 5.2 Spam classifications
sample <- spamTest[c(7,35,224,327),c('spam','pred')]
print(sample)

# Listing 5.3 Spam confusion matrix
cM <- table(truth=spamTest$spam,prediction=spamTest$pred>0.5)
print(cM)

TP <- cM[1,1]
TN <- cM[2,2]
(TP + TN) / sum(cM)

# Listing 5.4 Entering data by hand
t <- as.table(matrix(data=c(288-1,17,1,13882-17),nrow=2,ncol=2))
rownames(t) <- rownames(cM)
colnames(t) <- colnames(cM)
print(t)

(t[1,1] + t[2,2]) / sum(t)

# Precision TP / (TP + FP)
# "of all things that were labeled as spam, how many were actually spam?"
# van alle studenten van wie we aangaven dat ze zouden uitvallen, hoeveel vielen daadwerkelijk uit?
cM[2,2] / (cM[2,2]+cM[1,2])
t[2,2] / (t[2,2]+t[1,2])

# Recall TP / (TP + FN) - ook wel genoemd Prevalence
# "of all the things that are truly spam, how many did we label?"
# van alle studenten die uitvielen, hoeveel vonden we daarvan
cM[2,2] / (cM[2,2]+cM[2,1])
t[2,2] / (t[2,2]+t[2,1])

# 5.5 Plotting Residuals
d <- data.frame(y=(1:10)^2,x=1:10)
model <- lm(y~x,data=d)
d$prediction <- predict(model,newdata=d)
library('ggplot2')
ggplot(data=d) + geom_point(aes(x=x,y=y)) +
    geom_line(aes(x=x,y=prediction),color='blue') +
    geom_segment(aes(x=x,y=prediction,yend=y,xend=x)) +
    scale_y_continuous('')

# 5.6 Double density plot
ggplot(data=spamTest) +
    geom_density(aes(x=pred,color=spam,linetype=spam))

# Listing 5.7 Plotting the receiver operating characteristic curve
library('ROCR')
eval <- prediction(spamTest$pred,spamTest$spam)
plot(performance(eval,"tpr","fpr"))
print(attributes(performance(eval,'auc'))$y.values[[1]])

# 5.11 Clustering random data in the plane
set.seed(32297)
d <- data.frame(x=runif(100),y=runif(100))
clus <- kmeans(d,centers=5)
d$cluster <- clus$cluster

# 5.12 Plotting our clusters
library('ggplot2'); library('grDevices')
h <- do.call(rbind,
             lapply(unique(clus$cluster),
                    function(c) { f <- subset(d,cluster==c); f[chull(f),]}))
ggplot() +
    geom_text(data=d,aes(label=cluster,x=x,y=y,
                         color=cluster),size=3) +
    geom_polygon(data=h,aes(x=x,y=y,group=cluster,fill=as.factor(cluster)),
                 alpha=0.4,linetype=0) +
    theme(legend.position = "none")

# 5.13 Calculating the size of each cluster
table(d$cluster)

# 5.14 Calculating the typical distance between items in every pair of clusters
library('reshape2')
n <- dim(d)[[1]]
pairs <- data.frame(
    ca = as.vector(outer(1:n,1:n,function(a,b) d[a,'cluster'])),
    cb = as.vector(outer(1:n,1:n,function(a,b) d[b,'cluster'])),
    dist = as.vector(outer(1:n,1:n,function(a,b)
        sqrt((d[a,'x']-d[b,'x'])^2 + (d[a,'y']-d[b,'y'])^2)))
)
dcast(pairs,ca~cb,value.var='dist',mean)

