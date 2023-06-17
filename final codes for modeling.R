#codes for modeling part:
#we uploaded our series from world bank with WDI() function and by using merge() 
#function we constructed our data set.
#following codes are for imputation:
library(tidyverse)
library(dplyr)
library(mice)
library(VIM)
library(ggplot2)
library(tidyr)

df<- read.csv("df.csv")
library(naniar)
missplot<-ggplot(df,
                 aes(x = migration ,
                     y = year)) +
  geom_miss_point() + 
  facet_wrap(~country)
missplot #for migration

#we detected missing values and imputed it using mice()
library(mice)
num_df<-df[,-c(1,2,9)]
set.seed(5)
imputed<-mice(num_df,method = "cart")
wdicomp<-complete(imputed)
view(wdicomp)

#we also looked for the density plots before after imputation we didn't add it 
#here not to make a crowded codes.
#we worked on transformation for response:
hist((data$migration)^0.22)
powmig<-(data$migration)^0.22
shapiro.test(powmig) #p-value<0.05
skewness(data2$migration) #-2.58
hist(data2$migration,breaks=30)

#for different years examining migration:
View(data)
dat2009<- data2[data2$year==2009,]
shapiro.test(dat2009$migration) #<0.05 not normal
skewness(dat2009$migration) #=0.7864981
mean(dat2009$migration)  #-4701.05
median(dat2009$migration) #-6096.5
a<-log(dat2009$migration)
shapiro.test(a) # 0.2992 normal

dat2010<- data2[data2$year==2010,]
range(dat2010$migration)#-462265 1030731
shapiro.test(dat2010$migration) #<0.05 not normal
skewness(dat2010$migration) #2.462947
mean(dat2010$migration)  #-3194.629
median(dat2010$migration) #-5859
b<-log(dat2010$migration)
shapiro.test(b) #0.4703 normal

dat2011<- data2[data2$year==2011,]
View(dat2011)
shapiro.test(dat2011$migration) #<0.05 not normal
skewness(dat2011$migration) #2.382281
mean(dat2011$migration) #5576.271 
median(dat2011$migration) #-1772.5
log2010<-log10(dat2011$migration)
shapiro.test(log2010) #NA
ks.test(log2010, "pnorm") #not normal pvalue<0.05

dat2012<- data2[data2$year==2012,]
shapiro.test(dat2012$migration) #<0.05 not normal
skewness(dat2012$migration) #-1.241005
mean(dat2012$migration)  #6813.95 
median(dat2012$migration) #-3416.5

dat2013<- data2[data2$year==2013,]
shapiro.test(dat2013$migration) #<0.05 not normal
skewness(dat2013$migration) # -3.07141
mean(dat2013$migration)  #5072.279
median(dat2013$migration) # -1540

dat2014<- data2[data2$year==2014,]
shapiro.test(dat2014$migration) #<0.05 not normal
skewness(dat2014$migration) #-2.67
mean(dat2014$migration) #12253.95  
median(dat2014$migration) #-1654.5

dat2015<- data2[data2$year==2015,]
shapiro.test(dat2015$migration) #<0.05 not normal
skewness(dat2015$migration) #-3.59
mean(dat2015$migration)#8346.771  
median(dat2015$migration) #-1964

dat2016<- data2[data2$year==2016,]
shapiro.test(dat2016$migration) #<0.05 not normal
skewness(dat2016$migration)#-3.90
mean(dat2016$migration)  # 4435.493
median(dat2016$migration)#-641

migdata<-data$migration
hist(migdata)
hist(migdata,breaks=150) #I think it is almost normal in the end
range(migdata)
library(e1071) 
skewness(migdata) #-2.71 left skewed data power transformation needed
logmig<-log(data$migration) #nans produced (-) exists
sqmig<-(migdata)^2
shapiro.test(sqmig) #2.2e-16
hist(sqmig)
skewness(sqmig) #10.54222

cubemig<-migdata^3
hist(cubemig)
shapiro.test(cubemig) #2.2e-16
skewness(cubemig) #-12.5347

formig<-migdata^4
hist(formig)
shapiro.test(formig)
skewness(formig)

library(MASS)
bxcx<-boxcox(migdata)
library(bestNormalize)
bst<- bestNormalize(migdata)
nwbst<-bst$x.t
hist(nwbst)
shapiro.test(nwbst) #p-value=1
skewness(nwbst) #0.0000525 almost normal?
bst$method
# "Out-of-sample via CV with 10 folds and 5 repeats"
bst #ordernorm is selected interpretation is so hard...
data$nwbst<-nwbst #I don't know how to make inferences out of this

scmig2<-scale(data$migration,scale=TRUE)
shapiro.test(scmig2) #2.2e-16

ex0.2<-migdata^0.22
hist(ex0.2)
shapiro.test(ex0.2)#not normal

ex0.3<-migdata^0.3
hist(ex0.3)
shapiro.test(ex0.3)

ex0.1<-migdata^0.1
hist(ex0.1)
shapiro.test(ex0.1)#the biggest untill now 0.00003376

ex0.11<-migdata^0.11
shapiro.test(ex0.11) #devam 0.0002

ex0.112<-migdata^0.112
shapiro.test(ex0.112) #0.0003442

ex0.12<-migdata^0.12
shapiro.test(ex0.12) #0.000748

ex0.129<-migdata^0.129
shapiro.test(ex0.129) #0.0009731
hist(ex0.129)

ex0.135<-migdata^0.135 
shapiro.test(ex0.135) #0.0008619


data<-read.csv("dflast.csv")
scmig<-scale(data$migration)
range(scmig)
hist(scmig)
shapiro.test(scmig) #too small p-value
skewness(scmig) #still -2.71

#concluded to keep response not transformed and only divide it to thousand
data<-read.csv("dflast.csv")#imputed data for coding ease
data$migth<-(data$migration)/1000


#models with random effects:
library(lme4)
model1 <- lmer(migration ~ pol.stability+unemp+women.bl.scr+price.idx+happiness.scr + (1|country), data = data2)
summary(model1)
ranef(model1)

model1.1 <- lmer(migration ~ pol.stability+unemp+women.bl.scr+price.idx+happiness.scr + (year|country), data = data2)
summary(model1.1)
ranef(model1.1)

model2 <- lmer(mig_new ~ pol.stability+unemp+women.bl.scr+price.idx+happiness.scr + (1 | country), data = data2)
summary(model2)

plot(data2$migration) #some outliers are valid
data$migth<-(data$migration)/1000

model3 <- lmer(migth ~ scale(pol.stability)+ scale(unemp) +scale(women.bl.scr)+scale(price.idx)+ scale(happiness.scr) 
               + region+ (1 | country), data = data2)
summary(model3)
qnorm(0.05,lower.tail = FALSE)

#drop region for the following part 

model4 <- lmer(migth ~ scale(pol.stability)+ scale(unemp) +scale(women.bl.scr)+scale(price.idx)+ scale(happiness.scr) 
               + (year | country), data = data2)
summary(model4)

VarCorr(model3)
VarCorr(model4)

model5 <- lmer(migth ~ scale(pol.stability)+ scale(unemp) +scale(women.bl.scr)+scale(price.idx)+ scale(happiness.scr) 
               +scale(bilgdp)+(1 | country), data = data)
summary(model5)

data09<-filter(data,year==2009)
num09<-data09[,-c(1:3)]
cor(num09,method="spearman")
cor(num09,method="kendall")

#creating a matrix for covariance structure:for 8 years, 141 countries:
matmig=matrix(c(rep(0,1128*4)),ncol=8)

for(i in 1:8)
  matmig[,i]=data[data$year==(i+2008),]$migth #start with 1 year before the records

dimnames(matmig)<-list(NULL,c("2009","2010","2011","2012",
                              "2013","2014","2015","2016"))

head(matmig)

cov(matmig,m="pearson")  
cor(matmig,method="pearson") #decreasing correlation



model6 <- lmer(migth ~ scale(pol.stability)+scale(women.bl.scr)+scale(price.idx)*scale(unemp)+ scale(happiness.scr)+
                 (1 | country), data = data2)
summary(model6)
qnorm(0.1,lower.tail=FALSE)
qnorm(0.05,lower.tail = FALSE)
model6$residuals

model6.1 <- lmer(migth ~ (pol.stability)+(women.bl.scr)+(price.idx)*(unemp)+ (happiness.scr)+
                   (1 | country), data = data3)
summary(model6.1)
ranef(model6.1)
plot(model6.1)

model7 <- lmer(migth ~ scale(pol.stability)+scale(women.bl.scr)*scale(price.idx)+scale(unemp)+ scale(happiness.scr)+
                 (1 | country), data = data2)
summary(model7)

model8 <- lmer(migth ~ scale(pol.stability)+scale(women.bl.scr)*scale(price.idx)+scale(unemp)+ scale(happiness.scr)+
                 (year| country), data = data2)
summary(model8)

model9 <- lmer(migth ~ scale(pol.stability)+scale(women.bl.scr)*scale(unemp)+scale(price.idx)+ scale(happiness.scr)+
                 (1 | country), data = data2)
summary(model9)

model10 <- lmer(migth ~ scale(pol.stability)+scale(women.bl.scr)*scale(price.idx)+ scale(happiness.scr)*scale(unemp)+
                  (1 | country), data = data2)
summary(model10)

#we forgot to add year let add for our good models:

model11 <- lmer(migth ~ scale(pol.stability)+scale(women.bl.scr)*scale(price.idx)+ scale(happiness.scr)*scale(unemp)+year+
(1 | country), data = data2)
summary(model11)
#we don't need not significant and variation is too small.

#selecting best model
anova(model4,model6,test="Chisq")
anova(model7, model10, test = "Chisq")

model12<- lmer(migth ~ scale(pol.stability)+ scale(happiness.scr)+
                 (1 | country), data = data2)
summary(model12)

model13<- lmer(migth ~ pol.stability+ (price.idx)*(unemp)+(women.bl.scr)*year+
                 (1 | country), data = data3)
summary(model13)

model14.1<- lmer(migth ~ pol.stability+ (price.idx)*(unemp)+(women.bl.scr)+
                   (1 | country), data = data3)
summary(model14.1)
anova(model6.1,model13.1)
anova(model13.1,model13,test="Chisq")

#using different functions for random effects:
glmer1<-glmer(migth ~ scale(pol.stability)+scale(women.bl.scr)*scale(price.idx)+scale(unemp)+ scale(happiness.scr)+
                (1 | country), data = data,family="gaussian")
summary(glmer1)
#same with lmer()

library(nlme)
nlm1<-#couldn't perform the model became so complicated. 

#model adequacy check with residual plots  
plot(model7)
model7$residual
shapiro.test(model7$residuals)

#marginal models:
library(gee)
library(MuMIn)
margm <- gee(data2$migth~ data2$pol.stability+ data2$unemp +data2$women.bl.scr+data2$price.idx+data2$happiness.scr,id =data2$country,family = gaussian, corstr = "unstructured")
summary(margm)
QIC(md1)
#model didn't worked it said NAN values are obtained?

#linear regression:
data3<-data2
data3$year<-as.factor(data3$year)
lm1<- lm(thmig~year+pol.stability+unemp+women.bl.scr+price.idx+happiness.scr,data3)
summary(lm1)
library(car)
vif(lm1) #no vif

lm3<-lm(thmig~scale(pol.stability)+scale(unemp)+scale(women.bl.scr)+scale(price.idx)+scale(happiness.scr)+year,data3)
summary(lm3)

lm4<-lm(thmig~scale(pol.stability)+scale(women.bl.scr)+scale(happiness.scr),data3)
summary(lm4)

lm5<-lm(thmig ~ scale(pol.stability)+scale(women.bl.scr)+scale(price.idx)*scale(unemp)+ scale(happiness.scr),data3)
summary(lm5)
#main codes:
lm3 <- lm(migth ~ scale(pol.stability)+ scale(unemp) 
          +scale(women.bl.scr)+scale(price.idx)+ scale(happiness.scr) + region, data = data2)
summary(lm3)
vif(lm3)
plot(lm3)


lm6 <- lm(migth ~ scale(pol.stability)+scale(women.bl.scr)+scale(price.idx)*scale(unemp)+ scale(happiness.scr)
          , data = data2)
summary(lm6)
vif(lm6)
lm12<- lm(migth ~ scale(pol.stability)+ scale(happiness.scr)
          , data = data2)
summary(lm12)
vif(lm12)
lm13<- lm(migth ~ pol.stability+ (price.idx)*(unemp)+(women.bl.scr)*year
          , data = data2)
summary(lm13)
vif(lm13)


lm2<-lm(thmig~1,data3)
all <- lm(thmig ~ ., data=data3)
modelfin<-step(lm2, scope=list(upper=all, lower=~1), direction="forward")
forward$coefficients #pol stability ama coeff e-15

#machine learning part:

data2<-read.csv("dflast.csv")
install.packages("LongituRF")
library(LongituRF)
install.packages("groupdata2")
library(groupdata2)
library(xgboost)
library(dplyr)
data2$thmig<-(data2$migration)/10000
data2$country = factor(data2$country) 
data2$region=factor(data2$region)
#split train-test data
test.train.d=partition(data2,p = 0.2, id_col = "country")
test.train.d
test.d = test.train.d %>% .[1]
test.d
train.d=test.train.d %>% .[2]  
train.d
#setting my fixed and random effects
X.fixed.effects <- as.data.frame(train.d[[1]][,c(3,5,6,7,8,9)])
z=cbind(rep(1,dim(train.d[[1]])[1]),train.d[[1]]$year)  # tried a random intercept and slope model
y=as.matrix((train.d[[1]]$thmig))
#ml1 model with random slope
ml1<-REEMtree(Y=y,X=X.fixed.effects,Z=z,id=train.d[[1]]$country,time=train.d[[1]]$year,sto="BM")
ml1$forest #n= 904 
#root 904 272749.400   0.4284272 others also 


ml1$forest$variable.importance


p.train = predict(ml1$forest, X=X.fixed.effects,Z=z,id=train.d[[1]]$country, time=train.d[[1]]$year)
p.test=predict(ml1$forest, test.d[[1]],X=as.data.frame(test.d[[1]][,c(2,3,4,6)]),Z=cbind(rep(1,dim(test.d[[1]])[1])),id=test.d[[1]]$country, time=test.d[[1]]$year)

sum(((train.d[[1]]$thmig)-p.train)**2)/(dim(train.d[[1]])[1]-dim(X.fixed.effects)[2]) #452.3745
sum(((test.d[[1]]$thmig)-p.test)**2)/(dim(test.d[[1]])[1]-dim(X.fixed.effects)[2]) #961.8957

ml1$random_effects  #too small

######## setting z2:
z2=cbind(rep(1,dim(train.d[[1]])[1]))
ml2 <-REEMtree(Y=y,X=X.fixed.effects,Z=z2,id=train.d[[1]]$country,time=train.d[[1]]$year,sto="BM")
ml2$forest #n= 904
#root 904 524592.700   0.7733656
plot(ml2$forest,ylim=c(-0.5,1)) #works in rscript
text(ml2$forest, use.n=TRUE, all=TRUE, cex=1)

ml2$forest$variable.importance
p.train2 = predict(ml2$forest, X=X.fixed.effects,Z=z,id=train.d[[1]]$country, time=train.d[[1]]$year)
p.test2=predict(ml2$forest, test.d[[1]],X=as.data.frame(test.d[[1]][,c(2,3,4,6)]),Z=cbind(rep(1,dim(test.d[[1]])[1])),id=test.d[[1]]$country, time=test.d[[1]]$year)


sum(((train.d[[1]]$thmig)-p.train2)**2)/(dim(train.d[[1]])[1]-dim(X.fixed.effects)[2]) #442.0176
sum(((test.d[[1]]$thmig)-p.test2)**2)/(dim(test.d[[1]])[1]-dim(X.fixed.effects)[2]) #982.0563


#let's use REEMforest function:
#forest didn't worked. 
X.fixed.effects2 <- as.data.frame(train.d[[1]][,c(3,5,6,7,8,9)])
ml3 <- REEMforest(Y=y,X=X.fixed.effects2,Z=z2,id=train.d[[1]]$country,time=train.d[[1]]$year,sto="BM",mtry=2)