## Sginificance - http://www.win-vector.com/blog/2013/04/worry-about-correctness-and-repeatability-not-p-values/
# from tables 1 and 2 of http://www.bmj.com/content/337/bmj.a439
d = data.frame(MuscularStrength=c('lower','middle','upper'),
                 count=c(2920,2919,2923),deaths=c(214,143,146))
# make upper strength the reference level
d$MuscularStrength = relevel(as.factor(d$MuscularStrength),
                                   ref='upper')
print(d)

# quickly look at rates and typical deviations (sqrt of variance)
#  http://en.wikipedia.org/wiki/Binomial_distribution
d$MortalityRate = d$deaths/d$count
d$MortalityRateDevEst = sqrt(d$MortalityRate*(1-d$MortalityRate)/d$count)
d$MortalityRateDevBound = sqrt(0.25/d$count)
print(d)

# plot likely observed distributions of deaths, assuming 
# rates are exactly as measured
library('ggplot2') # plotting
plotFrame = c()
deaths=0:sum(d$deaths)
for(i in 1:(dim(d)[[1]])) {
    row = as.list(d[i,])
    plotData = data.frame(deaths=deaths,
                          DeathRate=deaths/row$count,
                          MuscularStrength=row$MuscularStrength,
                          density=dbinom(deaths,size=row$count,prob=row$deaths/row$count))
    plotFrame = rbind(plotFrame,plotData)
}
ggplot() + 
    geom_line(data=plotFrame,
              aes(x=DeathRate,y=density,color=MuscularStrength,
                  linetype=MuscularStrength)) +
    geom_vline(data=d,aes(xintercept=deaths/count,color=MuscularStrength,
                          linetype=MuscularStrength))

# convert data into a longer format and get at same facts as in a model
library('plyr')    # joining data
outcomes = data.frame(outcome=c('survived','died'))
outcomes$dummy = 'a'
d$dummy='a'
joined = join(d,outcomes,by=c('dummy'),type='full',match='all')
joined$count = ifelse(joined$outcome=='survived',
                        joined$count-joined$deaths,
                        joined$deaths)
data = subset(joined,select=c(MuscularStrength,count,outcome))
print(data)

model = glm(outcome=='died'~MuscularStrength,
              weights=data$count,family=binomial(link='logit'),data=data)
summary(model)