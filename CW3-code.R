install.packages("mosaicData")
library(mosaicData)
data(SaratogaHouses)
names(SaratogaHouses)

SaratogaHouses$tprice=SaratogaHouses$price/1000
SaratogaHouses$tlotSize=sqrt(SaratogaHouses$lotSize)
SaratogaHouses$tage=sqrt(SaratogaHouses$age)
SaratogaHouses$tlandValue=SaratogaHouses$landValue/1000


#############################################opening######################################################






############################################ part (a)###########################################################

lm1<-lm(
SaratogaHouses$tprice~SaratogaHouses$tlotSize+SaratogaHouses$tage+
  SaratogaHouses$tlandValue+SaratogaHouses$livingArea+SaratogaHouses$pctCollege+
  SaratogaHouses$bedrooms+SaratogaHouses$fireplaces+SaratogaHouses$bathrooms+
  SaratogaHouses$rooms+SaratogaHouses$heating+SaratogaHouses$fuel+
  SaratogaHouses$sewer+SaratogaHouses$waterfront+SaratogaHouses$newConstruction+
  SaratogaHouses$centralAir)
summary(lm1)


##############################################part (b)#########################################################

lm2<-lm(
  SaratogaHouses$tprice~SaratogaHouses$tlotSize+SaratogaHouses$tage+
  SaratogaHouses$tlandValue+SaratogaHouses$livingArea+SaratogaHouses$bedrooms+
  SaratogaHouses$bathrooms+SaratogaHouses$rooms+SaratogaHouses$theating+
  SaratogaHouses$twaterfront+SaratogaHouses$tnewConstruction+
  SaratogaHouses$tcentralAir)
summary(lm2)


SaratogaHouses$heating
levels(factor(SaratogaHouses$heating))
SaratogaHouses$theating=factor(SaratogaHouses$heating,labels = c("1","2","3"))
SaratogaHouses$theating
                                      
SaratogaHouses$waterfront
levels(factor(SaratogaHouses$waterfront))
SaratogaHouses$twaterfront=factor(SaratogaHouses$waterfront,labels = c("0","1"))
SaratogaHouses$twaterfront

levels(factor(SaratogaHouses$newConstruction))
SaratogaHouses$tnewConstruction=factor(SaratogaHouses$newConstruction,
                                       labels = c("0","1"))
SaratogaHouses$tnewConstruction

levels(factor(SaratogaHouses$centralAir))
SaratogaHouses$tcentralAir=factor(SaratogaHouses$centralAir,labels = c("0","1"))
SaratogaHouses$tcentralAir












###############################################################part (c)#################################################

n=1728
p1=15
p2=11

anova(lm1)
sse1=5777946

anova(lm2)
sse2=5783515

F=((sse2-sse1)/(p1-p2))/(sse1/(n-p1))
F

qf(0.95,df1=p1-p2,df2=n-p1)

anova(lm1,lm2)


##################################part (e)###################

mean(residuals(lm1))
mean(residuals(lm2))


################lm1###############

plot(fitted(lm1),residuals(lm1),xlab="fitted",ylab="residuals",cex.lab=1.2,cex.axis=1.2) #indicates some pattern?
plot(SaratogaHouses$tlotSize,residuals(lm1),ylab="residuals",cex.lab=1.2,cex.axis=1.2) #Indicates linear effect of ht is ok
plot(fitted(lm2),residuals(lm2),xlab="fitted",ylab="residuals",cex.lab=1.2,cex.axis=1.2)
plot(lm1,1)

par(mfrow=c(2,2))
plot(fitted(lm2),residuals(lm2),xlab="fitted",ylab="residuals(lm2)",cex.lab=1.2,cex.axis=1.2)
plot(SaratogaHouses$tlotSize,residuals(lm2),xlab="tlotSize",ylab="residuals(lm2)",cex.lab=1.2,cex.axis=1.2) #Indicates linear effect of ht is ok
plot(SaratogaHouses$tage,residuals(lm2),xlab="tage",ylab="residuals(lm2)",cex.lab=1.2,cex.axis=1.2)
plot(SaratogaHouses$tlandValue,residuals(lm2),xlab="tlandvalue",ylab="residuals(lm2)",cex.lab=1.2,cex.axis=1.2)
plot(SaratogaHouses$livingArea,residuals(lm2),xlab="livingArea",ylab="residuals(lm2)",cex.lab=1.2,cex.axis=1.2)
plot(SaratogaHouses$bedrooms,residuals(lm2),xlab="bedrooms",ylab="residuals(lm2)",cex.lab=1.2,cex.axis=1.2)
plot(SaratogaHouses$bathrooms,residuals(lm2),xlab="bathrooms",ylab="residuals(lm2)",cex.lab=1.2,cex.axis=1.2)
plot(SaratogaHouses$rooms,residuals(lm2),xlab="rooms",ylab="residuals(lm2)",cex.lab=1.2,cex.axis=1.2)
plot(as.numeric(SaratogaHouses$heating),xlab="heating",residuals(lm2),ylab="residuals(lm2)",col=factor(SaratogaHouses$heating),cex.lab=1.2,cex.axis=1.2)
plot(as.numeric(SaratogaHouses$waterfront),residuals(lm2),xlab="waterfront",ylab="residuals(lm2)",col=factor(SaratogaHouses$waterfront),cex.lab=1.2,cex.axis=1.2)
plot(as.numeric(SaratogaHouses$newConstruction),residuals(lm2),xlab="newConstruction",ylab="residuals(lm2)",col=factor(SaratogaHouses$newConstruction),cex.lab=1.2,cex.axis=1.2)
plot(as.numeric(SaratogaHouses$centralAir),residuals(lm2),xlab="centralAir",ylab="residuals(lm2)",col=factor(SaratogaHouses$centralAir),cex.lab=1.2,cex.axis=1.2)

plot(fitted(lm2),residuals(lm2),xlab="fitted",ylab="residuals",col = factor(SaratogaHouses$theating),cex.lab=1.2,cex.axis=1.2)
plot(ht,residuals(PEFR.lm),ylab="residuals",cex.lab=1.2,cex.axis=1.2) #Indicates linear effect of ht is ok
plot(Sex,residuals(PEFR.lm),ylab="residuals",cex.lab=1.2,cex.axis=1.2) #Different variances for different gender groups?
hist(residuals(PEFR.lm),breaks = seq(min(residuals(PEFR.lm)),max(residuals(PEFR.lm)),length.out=15))
plot(density(residuals(PEFR.lm)))





#############################################Second#########################################


std.res=rstandard(lm2)


###########################################part (f)#########################################


min(std.res=rstandard(lm2))
max(std.res=rstandard(lm2))

x.ord<-sort(std.res)

par(mfrow=c(1,1))
set.seed(914)

N<-1728
std.res1<-sort(std.res)
pn=0
for(k in 1: N){
  pn[k]=((k-3/8)/(N+1/4))}  
theoretical.quantiles<-qnorm(pn)

plot(theoretical.quantiles, std.res1,  
     main="Normal Q-Q plot of random Normal data (n=1728)",ylab = "standardised residuals")
abline(a=0, b=1, col="green")



#############################part (g)##################

x.ecdf<-(1:N)/N
x.ecdf

y=pnorm(std.res1)
y

diff1=x.ecdf-y
md1=max(diff1)
dn.plus=max(md1,0)
dn.plus
x.KS1=x.ord[dn.plus==diff1]
x.KS1


diff2=y-x.ecdf
md2=max(diff2)
md2=md2+(1/N)
dn.minus=max(md2,0)
dn.minus
x.KS2=x.ord[dn.minus==diff2+(1/N)]
x.KS2

KSstat=max(dn.minus, dn.plus)
KSstat

if(dn.minus < dn.plus) x.KSstat=x.KS1
if(dn.minus > dn.plus) x.KSstat=x.KS2
x.KSstat


#########################part (h)############################
max(x.ord)
min(x.ord)

plot.ecdf(x.ord) 

x.grid=seq(-4,8,.001)
x.grid.pnorm<-pnorm(x.grid)
lines(x.grid,x.grid.pnorm,col='green',type="l")
abline(v=x.KSstat, col='gray')
title(paste("KS-stat = ",as.character(round(KSstat,digits=3)),sep=""),adj=1)


##############################part (i)###########################

###1
set.seed(1997)

B=5000
KSstat.sim=0

for (i in 1:B) {
  x.ecdf<-(1:N)/N
  x.sim=rnorm(N)
  y.sim=pnorm(sort(x.sim))
  
  diff.p=y.sim-x.ecdf
  md.p=max(diff.p)
  dn.p=max(md.p,0)

  diff.m=x.ecdf-y.sim
  md.m=max(diff.m)+(1/N)
  dn.m=max(md.m,0)
  
  KSstat.sim[i]=max(dn.p,dn.m)
  
}

###2
hist(KSstat.sim)

###3

mean.sim=mean(KSstat.sim)
var.sim=var(KSstat.sim)

ci1=mean.sim+1.96*sqrt(var.sim)
ci2=mean.sim-1.96*sqrt(var.sim)

ci1
ci2







































