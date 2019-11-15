severity<-matrix(c(65,773,4343,14,475,4134,22,488,3992,25,310,9850,6,146,2556),nrow = 5,byrow = TRUE)
severity

dimnames(severity)<-list(c("Pedestrian","Pedal Cycle","Powered 2 Wheeler","Car","Other Vehicles"),c("Fatal","Serious","Slight"))
severity

names(dimnames(severity))<-c("Mode of Transport","Casualty Severity")
severity

addmargins(severity)

pt<-prop.table(severity,1)
pt

pt[1,1]


severity<-matrix(
c(65,773,4343,14,475,4134,22,488,3992,25,310,9850,6,146,2556),
nrow = 5,byrow = TRUE)
##read the data as a matrix


dimnames(severity)<-list(
c("Pedestrian","Pedal Cycle","Powered 2 Wheeler","Car","Other Vehicles"),
c("Fatal","Serious","Slight"))

names(dimnames(severity))<-c("Mode of Transport","Casualty Severity")
##label the two dimensions


severity

addmargins(severity)


pt<-prop.table(severity,1)
pt
##calculate the proportion table

abs(pt[2,]-pt[3,])/pt[3,]

abs(pt[4,]-pt[5,])/pt[5,]

abs(pt[1,]-pt[2:5])/pt[2:5]

abs(pt[4,]-pt[5,])

barplot(pt,beside=TRUE,
legend.text=TRUE,
args.legend = list(x = "topleft"),
ylim=c(0,1),ylab="Proportions",
xlab="Mode of Transport",
main="The Casualty Severity Data",
col=c("lightgreen", "lightblue","lightpink","orange","purple"))


################part (iv)#################################

test1<-chisq.test(severity)
test1

df<-(3-1)*(5-1)
df
qchisq(0.95,df)

qchisq(0.95,8)


##################part (v)###################################
test1$observed
test1$expected
test1$residuals
test1$residuals^2
test1$stdres


##########################part (vi)##########################

addmargins(test1$expected)

p1.hat<- 132/27199
p2.hat<-2192/27199
p3.hat<-24875/27199

pr<- c(p1.hat,p2.hat,p3.hat)
pr

B=5000

test.sim=0

for (i in 1:B) {
y1<-rmultinom(n=1,size=5181,pr) 
y2<-rmultinom(n=1,size=4623,pr)
y3<-rmultinom(n=1,size=4502,pr)
y4<-rmultinom(n=1,size=10185,pr)
y5<-rmultinom(n=1,size=2708,pr)

ysim<-t(cbind(y1,y2,y3,y4,y5))

test.sim[i]<-chisq.test(ysim,p=pr)$statistic
}


hist(test.sim,freq=F,ylim=c(0,0.15),
main="Graph of simulated X^2 values (B=5000) 
with chi-squared (df=8)",
col="Lightblue",cex.main=1)

xx=seq(from=0, to=30, length.out=500)
dxx=dchisq(xx,df=8)

lines(xx,dxx,col="Blue",lwd=2)






y1<-rmultinom(n=1,size=5181,pr)
y1

y2<-rmultinom(n=1,size=4623,pr)
y2

y3<-rmultinom(n=1,size=4502,pr)
y3

y4<-rmultinom(n=1,size=10185,pr)
y4

y5<-rmultinom(n=1,size=2708,pr)
y5

ysim<-t(cbind(y1,y2,y3,y4,y5))
ysim


######################part (vii)#########################

y12.hat=773
y42.hat=310
n1=5181
n4=10185
p12.hat=y12.hat/n1
p42.hat=y42.hat/n4
diff=p12.hat-p42.hat
v12<-p12.hat*(1-p12.hat)/n1
v42<-p42.hat*(1-p42.hat)/n4

se1=v12+v42
crit.val1<-qnorm(1-0.05/2)
ci1.1=diff-crit.val1*sqrt(se1)
ci1.2=diff+crit.val1*sqrt(se1)
ci1.1
ci1.2





































prop.diff.ci<-function(y22, y23, n2=4623, level=0.95){
p22.hat<-y22/n2
p23.hat<-y23/n2
diff<-p22.hat-p23.hat

v22<-p22.hat*(1-p22.hat)/n2
v23<-p23.hat*(1-p23.hat)/n2
cab2<- -p22.hat*p23.hat/n2

se2<-sqrt(v22+v23-2*cab2)

crit.val2<-qnorm((1+level)/2)

ci2<-0

ci2[1]<- diff - crit.val2*se2
ci2[2]<- diff + crit.val2*se2

return(ci2)
}

prop.diff.ci(475, 4134, n=4623, level=0.95)











