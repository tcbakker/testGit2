library(dplyr)

custdata <- read.table('custdata.tsv',header=T,sep='\t')
glimpse(custdata)
summary(custdata)

# 3.6. Plotting a histogram
library(ggplot2)
ggplot(custdata) +
    geom_histogram(aes(x=age),
                   binwidth=5, fill="gray")

# 3.7 Producing a density plot
library(scales)
ggplot(custdata) + geom_density(aes(x=income)) +
    scale_x_continuous(labels=dollar)

# 3.8 Log scaled density plot
ggplot(custdata) + geom_density(aes(x=income)) +
    scale_x_log10(breaks=c(100,1000,10000,100000), labels=dollar) +
    annotation_logticks(sides="bt")

# Marital status
ggplot(custdata) + geom_bar(aes(x=marital.stat), fill="gray")

# 3.9 Horizontal bar chart
ggplot(custdata) +
    geom_bar(aes(x=state.of.res), fill="gray") +
    coord_flip() +
    theme(axis.text.y=element_text(size=rel(0.8)))

# 3.10 sorted
statesums <- table(custdata$state.of.res)
statef <- as.data.frame(statesums)
colnames(statef)<-c("state.of.res", "count")
summary(statef)

statef <- transform(statef,state.of.res=reorder(state.of.res, count))
summary(statef)

ggplot(statef)+ geom_bar(aes(x=state.of.res,y=count),
                         stat="identity",
                         fill="gray") +
    coord_flip() +
    theme(axis.text.y=element_text(size=rel(0.8)))

# 3.12 correlation age and income
custdata2 <- subset(custdata,
                    (custdata$age > 0 & custdata$age < 100
                     & custdata$income > 0))
cor(custdata2$age, custdata2$income)

# scatterplot
ggplot(custdata2, aes(x=age, y=income)) +
    geom_point() + ylim(0, 200000)

# scatterplot with trendline
ggplot(custdata2, aes(x=age, y=income)) + geom_point() +
    stat_smooth(method="lm") +
    ylim(0, 200000)

# smoothing
ggplot(custdata2, aes(x=age, y=income)) +
    geom_point() + geom_smooth() +
    ylim(0, 200000)

# 3.13
ggplot(custdata2, aes(x=age, y=as.numeric(health.ins))) +
    geom_point(position=position_jitter(w=0.05, h=0.05)) +
    geom_smooth()

# 3.14 hexbin
library(hexbin)
ggplot(custdata2, aes(x=age, y=income)) +
    geom_hex(binwidth=c(5, 10000)) +
    geom_smooth(color="white", se=F) +
    ylim(0,200000)

# 3.15 stacked / side-by-side bars
ggplot(custdata) + geom_bar(aes(x=marital.stat,
                                fill=health.ins))
ggplot(custdata) + geom_bar(aes(x=marital.stat,
                                fill=health.ins),
                            position="dodge")
ggplot(custdata) + geom_bar(aes(x=marital.stat,
                                fill=health.ins),
                            position="fill")

# 3.16 data with a rug
ggplot(custdata, aes(x=marital.stat)) +
    geom_bar(aes(fill=health.ins), position="fill") +
    geom_point(aes(y=-0.05), size=0.75, alpha=0.3,
               position=position_jitter(h=0.01))