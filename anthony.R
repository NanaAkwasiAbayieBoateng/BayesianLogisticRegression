rm(list=ls())
#install.packages(c("xlsx","XLConnect","sas7bdat","lubridate","psych"))
library(xlsx)
require(XLConnect)
library(sas7bdat)
library(survival)
library("lubridate")
library(psych)


mydata=read.csv("~/Desktop/Anthony/Anthony.csv")


describe(mydata)


#png("~/Desktop/hist.png")
pdf("~/Desktop/hist.pdf")
par(mfrow=c(2,1))
hist(mydata$Female.Bp,breaks="fd",col="gray",prob=TRUE,main="Hostogram of Females",xlab="")
hist(mydata$Male.Bp,breaks="fd",col="gray",prob=TRUE,main="Histogram of Males",xlab="")
dev.off();

t.test(mydata$Female.Bp,mydata$Male.Bp)

png("H:/Nana/Spleen Project/ost.png")
plot(survfit(Surv(time=data4$O.S.TIME.months, status) ~ group, data=data4)
     , xlab="Time in months", ylab="Survival Probability",col=c("blue","red"), lty=1:2, mark.time=FALSE)
title(main='Kaplan Meier-Curves for Overall Survival Times ')
legend(5, 0.1,c('enlarged', 'unenlarged'), pch=c(1,2) ,col=c("blue","red"))
dev.off();



data=read.csv("~/Desktop/Anthony/Kindergarten.csv")

traditional=subset(data,  Curriculum==1)

experimental=subset(data,  Curriculum==2)

############################################
### Decriptive Statistics#################

describe(data)

describe(traditional)

describe(experimental)


############################################
### Two Sample t test with equal variance#################

### test of equal means#################

describe(traditional$Kindergarten.IQ)

describe(experimental$Kindergarten.IQ)

t.test(traditional$Kindergarten.IQ, experimental$Kindergarten.IQ,alternative = "two.sided",mu = 0,
       paired = FALSE, var.equal = T,conf.level = 0.95)


############################################
### Two Sample t test with unequal variance#################

### test of equal means#################


t.test(traditional$Kindergarten.IQ, experimental$Kindergarten.IQ,alternative = "two.sided",mu = 0,
       paired = FALSE, var.equal = F,conf.level = 0.95)



###############################################

###### Question Two ###########################
##########paired t test##########################

describe(data$Thinking.Skills.Gr.3)
describe(data$Thinking.Skills.Grade.5)

t.test(data$Thinking.Skills.Gr.3 , data$Thinking.Skills.Grade.5,alternative = "two.sided",mu = 0,
       paired = T, var.equal = F,conf.level = 0.95)


t.test(data$Thinking.Skills.Gr.3 , data$Thinking.Skills.Grade.5,alternative = "two.sided",mu = 0,
       paired = T, var.equal = T,conf.level = 0.95)

t.test( data$Thinking.Skills.Grade.5,data$Thinking.Skills.Gr.3 ,alternative = "less",
       paired = T, var.equal = T,conf.level = 0.95)


######################################

###### Question Three ################

describe(data$Thinking.Skills.Gr.3)

t.test(data$Thinking.Skills.Gr.3 ,mu=544.58)
t.test(data$Thinking.Skills.Gr.3 ,mu=572,alternative = "less")
t=(572-544.58)/(195.49/sqrt(108))

2*pnorm((572-544.58)/(195.49/sqrt(108)),0,1)

1-pnorm((572-544.58)/(195.49/sqrt(108)))

68*.25

37/50


pnorm(log(8))




#######################################################################
####### Dose group control and High ####################################
rm(list=ls())
library(CorrBin)
data(shelltox)
shelltox=shelltox
head(shelltox)
#install.packages("CorrBin")


ftable(xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox))


#####################################
##### 0 response for c,l,m,h#########
rbind(xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,1][1,],
xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,2][1,],
xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,3][1,],
xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,4][1,])

#####################################
##### 1 response for c,l,m,h#########
rbind(xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,1][2,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,2][2,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,3][2,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,4][2,])

#####################################
##### 2 response for c,l,m,h#########
rbind(xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,1][3,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,2][3,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,3][3,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,4][3,])


#####################################
##### 3 response for c,l,m,h#########
rbind(xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,1][4,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,2][4,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,3][4,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,4][4,])
#####################################
##### 4 response for c,l,m,h#########
rbind(xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,1][5,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,2][5,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,3][5,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,4][5,])

#####################################
##### 5 response for c,l,m,h#########
rbind(xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,1][6,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,2][6,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,3][6,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,4][6,])


#####################################
##### 6 response for c,l,m,h#########
rbind(xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,1][7,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,2][7,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,3][7,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,4][7,])

xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,c(1,4)]
#####################################
##### structable performs the task above for c,l,m,h#########
structable(xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox))[1,]

structable(Freq~NResp+ClusterSize,shelltox)
structable(Day ~ Time+Network,TV2)


structable(shelltox)


(spine(Trt~NResp, data = shelltox, breaks = "Scott"))
cdplot(Trt ~ NResp, data = shelltox)
with(shelltox, rug(jitter(NResp), col="white", quiet=TRUE))


mantelhaen.test(xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,1])

aggregate(Freq~NResp+ClusterSize+Trt,data=shelltox,sum)

tv.data<-read.table(system.file("doc","extdata","tv.dat",package="vcdExtra"))
head(tv.data,5)




ftable(xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox),sum)

ftable(shelltox)
install.packages("zoo")
library(vcd)
structable(xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox))


aggregate(xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox))

xtabs(Freq~NResp+ClusterSize,data=shelltox)

d1=xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,1]
d4=xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,4]
both=d1+d4

d=rbind(d1,d4)

probdist=function(d){
  d1=d[1:7,]
  d4=d[8:14,]
  m1=apply(d4,2,sum)
  m2=apply(d1,2,sum)
  M=m1+m2
  b=d1+d4
  p=sum(lfactorial(m1))+sum(lfactorial(m2))+
    sum(apply(lfactorial(both),2,sum))-sum(lfactorial(M))-
    sum(apply(lfactorial(d4),2,sum))-sum(apply(lfactorial(d1),2,sum))
  return((p))
}
pobs=probdist(d)
pobs


rowTotals <- rowSums(d)
colTotals <- colSums(d)
Nsim=100000
psim=sapply(r2dtable(Nsim, rowTotals, colTotals), probdist)

p.value=length(((psim[pobs<=psim])))/Nsim
p.value

se=sqrt((p.value*(1-p.value))/(Nsim-1))
up=p.value+2.575*se
lo=p.value-2.575*se
c(lo,up)

png("~/Documents/memphisclassesbooks/RESEARCH/Trend/histch.png")
hist(psim, breaks="fd", col="gray",main="Histogram of P(Y) of Simulated
     Tables ",xlab="log of P(Y)",prob=TRUE)
abline(v =pobs,col="blue")
box(lty = 1, col = 'black')
dev.off();


rm(list=ls())
library(CorrBin)
data(shelltox)
shelltox=shelltox




#####################################################################################
####### Dose group control,low,medium and High #####################################

control=xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,1]
low=xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,2]
medium=xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,3]
high=xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,4]
both=control+high+medium+low
d=rbind(control,high,low,medium)

probdist=function(d){
  control=d[1:7,]
  low=d[8:14,]
  medium=d[15:21,]
  high=d[22:28,]
  m1=apply(control,2,sum)
  m2=apply(low,2,sum)
  m3=apply(medium,2,sum)
  m4=apply(high,2,sum)
  M=m1+m2+m3+m4
  b=control+high+low+medium
  p=sum(lfactorial(m1))+sum(lfactorial(m2))+sum(lfactorial(m3))+sum(lfactorial(m4))+
    sum(apply(lfactorial(both),2,sum))-sum(lfactorial(M))-
    sum(apply(lfactorial(high),2,sum))-sum(apply(lfactorial(control),2,sum))-
    sum(apply(lfactorial(low),2,sum))-sum(apply(lfactorial(medium),2,sum))
  return((p))
}
pobs=probdist(d)
pobs


rowTotals <- rowSums(d)
colTotals <- colSums(d)
Nsim=100000
psim=sapply(r2dtable(Nsim, rowTotals, colTotals), probdist)
p.value=length(((psim[pobs<=psim])))/Nsim
p.value

se=sqrt((p.value*(1-p.value))/(Nsim-1))
up=p.value+2.575*se
lo=p.value-2.575*se
c(lo,up)
png("~/Documents/memphisclassesbooks/RESEARCH/Trend/histclmh.png")
hist(psim, breaks="fd", col="gray",main="Histogram of P(Y) of Simulated 
     Tables ",xlab="log of P(Y)",prob=TRUE)
abline(v =pobs,col="blue")
box(lty = 'solid', col = 'black')
dev.off();

mat <- matrix(sample(3, 500, TRUE), 10)
cl <- rep(0:1, e=25)
rowTrendStats(mat, cl)
cases <- rowTables(mat[, cl==1])
controls <- rowTables(mat[,cl==0])



c=matrix(rnorm(12,5,5),4)
c1=round(c,0)
rank(c1)
c2=as.vector(c1)
rank(c2)
matrix(rank(c2),4)

mean(matrix(c(1,2,3,4),2))



availablePackages <- available.packages()[,1]
install.packages(availablePackages)

installedPackages <- .packages(all.available = TRUE)


library(coin)
#??cmh_test
## Example data
## Davis (1986, p. 140)
davis <- matrix(
  c(3,  6,
    2, 19),
  nrow = 2, byrow = TRUE
)

## Asymptotic Pearson chi-squared test
chisq_test(as.table(davis))


## Approximative (Monte Carlo) Pearson chi-squared test
ct <- chisq_test(as.table(davis),
                 distribution = approximate(B = 10000))
pvalue(ct)          # standard p-value
midpvalue(ct)       # mid-p-value
pvalue_interval(ct) # p-value interval


## Laryngeal cancer data
## Agresti (2002, p. 107, Tab. 3.13)
cancer <- matrix(
  c(21, 2,
    15, 3),
  nrow = 2, byrow = TRUE,
  dimnames = list(
    "Treatment" = c("Surgery", "Radiation"),
    "Cancer" = c("Controlled", "Not Controlled")
  )
)



## Exact Pearson chi-squared test (Agresti, 2002, p. 108, Tab. 3.14)
## Note: agrees with Fishers's exact test
(ct <- chisq_test(as.table(cancer),
                  distribution = "exact"))
midpvalue(ct)       # mid-p-value
pvalue_interval(ct) # p-value interval
fisher.test(cancer)

## Homework conditions and teacher's rating
## Yates (1948, Tab. 1)
yates <- matrix(
  c(141, 67, 114, 79, 39,
    131, 66, 143, 72, 35,
    36, 14,  38, 28, 16),
  byrow = TRUE, ncol = 5,
  dimnames = list(
    "Rating" = c("A", "B", "C"),
    "Condition" = c("A", "B", "C", "D", "E")
  )
)


# Asymptotic Pearson chi-squared test (Yates, 1948, p. 176)
chisq_test(as.table(yates))

## Asymptotic Pearson-Yates chi-squared test (Yates, 1948, pp. 180-181)
## Note: 'Rating' and 'Condition' as ordinal
(ct <- chisq_test(as.table(yates),
                  alternative = "less",
                  scores = list("Rating" = c(-1, 0, 1),
                                "Condition" = c(2, 1, 0, -1, -2))))
statistic(ct)^2 # chi^2 = 2.332


## Asymptotic Pearson-Yates chi-squared test (Yates, 1948, p. 181)
## Note: 'Rating' as ordinal
chisq_test(as.table(yates),
           scores = list("Rating" = c(-1, 0, 1))) # Q = 3.825




## Change in clinical condition and degree of infiltration
## Cochran (1954, Tab. 6)
cochran <- matrix(
  c(11,  7,
    27, 15,
    42, 16,
    53, 13,
    11,  1),
  byrow = TRUE, ncol = 2,
  dimnames = list(
    "Change" = c("Marked", "Moderate", "Slight",
                 "Stationary", "Worse"),
    "Infiltration" = c("0-7", "8-15")
  )
)

## Asymptotic Pearson chi-squared test (Cochran, 1954, p. 435)
chisq_test(as.table(cochran)) # X^2 = 6.88



## Asymptotic Cochran-Armitage test (Cochran, 1954, p. 436)
## Note: 'Change' as ordinal
(ct <- chisq_test(as.table(cochran),
                  scores = list("Change" = c(3, 2, 1, 0, -1))))
statistic(ct)^2 # X^2 = 6.66

## Note: 'Change' as ordinal
(ct <- chisq_test(as.table(cochran)))
statistic(ct)^2 # X^2 = 6.66


## Change in size of ulcer crater for two treatment groups
## Armitage (1955, Tab. 2)
armitage <- matrix(
  c( 6, 4, 10, 12,
     11, 8,  8,  5),
  byrow = TRUE, ncol = 4,
  dimnames = list(
    "Treatment" = c("A", "B"),
    "Crater" = c("Larger", "< 2/3 healed",
                 "=> 2/3 healed", "Healed")
  )
)

## Approximative (Monte Carlo) Pearson chi-squared test (Armitage, 1955, p. 379)
chisq_test(as.table(armitage),
           distribution = approximate(B = 10000)) # chi^2 = 5.91

## Approximative (Monte Carlo) Cochran-Armitage test (Armitage, 1955, p. 379)
(ct <- chisq_test(as.table(armitage),
                  distribution = approximate(B = 10000),
                  scores = list("Crater" = c(-1.5, -0.5, 0.5, 1.5))))
statistic(ct)^2 # chi_0^2 = 5.26



## Relationship between job satisfaction and income stratified by gender
## Agresti (2002, p. 288, Tab. 7.8)

## Asymptotic generalized Cochran-Mantel-Haenszel test (Agresti, p. 297)
cmh_test(jobsatisfaction) # CMH = 10.2001

## Asymptotic generalized Cochran-Mantel-Haenszel test (Agresti, p. 297)
## Note: 'Job.Satisfaction' as ordinal
cmh_test(jobsatisfaction,
         scores = list("Job.Satisfaction" = c(1, 3, 4, 5))) # L^2 = 9.0342

pchisq(q=10.2, df=9, ncp = 0, lower.tail = F, log.p = FALSE)



## Asymptotic linear-by-linear association test (Agresti, p. 297)
## Note: 'Job.Satisfaction' and 'Income' as ordinal
(lt <- lbl_test(jobsatisfaction,
                scores = list("Job.Satisfaction" = c(1, 3, 4, 5),
                              "Income" = c(3, 10, 20, 35))))
statistic(lt)^2 # M^2 = 6.1563
shell

(lt <- lbl_test(shell,
                scores = list("NResp" = c(0,1,2, 3, 4, 5,6),
                              "Trt" = c(0, 1, 2, 3))))


### Cochran-Armitage trend test for proportions
### Lung tumors in female mice exposed to 1,2-dichloroethane
### Encyclopedia of Biostatistics (Armitage & Colton, 1998),
### Chapter Trend Test for Counts and Proportions, page 4578, Table 2
lungtumor <- data.frame(dose = rep(c(0, 1, 2), c(40, 50, 48)),
                        tumor = c(rep(c(0, 1), c(38, 2)),
                                  rep(c(0, 1), c(43, 7)),
                                  rep(c(0, 1), c(33, 15))))
table(lungtumor$dose, lungtumor$tumor)

table(lungtumor)


### Cochran-Armitage test (permutation equivalent to correlation
### between dose and tumor), cf. Table 2 for results
independence_test(tumor ~ dose, data = lungtumor, teststat = "quad")


### linear-by-linear association test with scores 0, 1, 2
### is identical with Cochran-Armitage test
lungtumor$dose <- ordered(lungtumor$dose)
independence_test(tumor ~ dose, data = lungtumor, teststat = "quad",
                  scores = list(dose = c(0, 1, 2)))


## Asymptotic generalized Cochran-Mantel-Haenszel test (Agresti, p. 297)
## Note: 'Job.Satisfaction' as ordinal
cmh_test(Job.Satisfaction~Income,data=jobsatisfaction,
         scores = list("Job.Satisfaction" = c(1, 3, 4, 5))) # L^2 = 9.0342


## Asymptotic generalized Cochran-Mantel-Haenszel test (Agresti, p. 297)
## Note: 'Job.Satisfaction' as ordinal
cmh_test(jobsatisfaction,
         scores = list("Job.Satisfaction" = c(1, 3, 4, 5))) # L^2 = 9.0342


shell=xtabs(Freq~Trt+ClusterSize+NResp,data=shelltox)[c(1,4),,]


X <- c(rep(1,17),rep(2,15))
Y <- c(rep(1,6),rep(2,11),rep(1,2),rep(2,8),rep(3,5))
d=data.frame(Y,X)
patefield=xtabs(~X+Y,data=d)



 mr <- rank(Y)
w2.mr <- (sort(unique(mr))[2]-min(mr))/(max(mr)-min(mr))
w2.mr
rank(apply(patefield,2,sum))


shell=xtabs(Freq~Trt+ClusterSize+NResp,data=shelltox)[c(1,4),,c(1,2,3,5)]
shell=as.array(xtabs(Freq~Trt+NResp+ClusterSize,data=shelltox)[,,c(3,4,5,6,7,8,9,10,11)])
shell=as.array(xtabs(Freq~Trt+NResp+ClusterSize,data=shelltox))
#shell=array(data=shell,7)
?array
?cmh_test
chisq_test(shell)

cmh_test(xtabs(Freq~Trt+ClusterSize+NResp,data=shelltox),scores = list("Trt" = c(0,1,2,3),"ClusterSize"=rep(1:13))) 

lt <- cmh_test(shell,
                scores = list("NResp" = c(0,1,2, 3, 4, 5,6),
                              "Trt" = c(0, 1, 2, 3)))
cmh_test(shell,scores = list("Trt" = c(1,2)))

cmh_test(shell, scores = list("Trt" = c(0,1,2, 3)))
cmh_test(shell, scores = list("Trt" = c(0.25,.5,0.75, 1)))
cmh_test(shell)

d=shell[,,c(4,6,7,8,9)]
cmh_test(shell, scores = list("Trt" = c(-1,0,1, 2),"ClusterSize"=rep(1:13)))


cmh_test(d, scores = list("Trt" = c(0,1,2, 3)),subset=list("Trt"=c(1,2)))

mantelhaen.test(shell,exact=T)
#####################################
##### structable performs the task above for c,l,m,h#########
c=structable(xtabs(Freq~Trt+ClusterSize+NResp,data=shelltox))[c(1,4),]

## Boxplots
boxplot(Trt ~NResp, data = shelltox)
## Asymptotic Kruskal-Wallis test
kruskal_test(elevel ~ alength, data = alpha)

?independence_test
?cmh_test


Rabbits=array(c(0, 0, 6, 5,
          3, 0, 3, 6,
          6, 2, 0, 4,
          5, 6, 1, 0,
          2, 5, 0, 0),
        dim = c(2, 2, 5),
        dimnames = list(
          Delay = c("None", "1.5h"),
          Response = c("Cured", "Died"),
          Penicillin.Level = c("1/8", "1/4", "1/2", "1", "4")))




install.packages("Rcmdr")
library(Rcmdr)


e <- expression(sin(x/2)*cos(x/4))
dydx <- D(e, "x")
 dydx
 
 
 z <- seq(-1,1,.1)
 eval(dydx, list(x=z))
 
 
 newton <- function(f, tol=1E-12,x0=1,N=20) {
   h <- 0.001
   i <- 1; x1 <- x0
   p <- numeric(N)
   while (i<=N) {
     df.dx <- (f(x0+h)-f(x0))/h
     x1 <- (x0 - (f(x0)/df.dx))
     p[i] <- x1
     i <- i + 1
     if (abs(x1-x0) < tol) break
     x0 <- x1
   }
   return(p[1:(i-1)])
 }
 
 f <- function(x) { x^3 + 4*x^2 -10 }
 
 plot(x,f(x), type='l', lwd=1.5, main=expression(sin(x/2) * cos(x/4)))
 abline(h=0)
 
 
 
 
 # Dataset 1 with known data
 known <- data.frame(
   x = c(0:6),
   y = c(0, 10, 20, 23, 41, 39, 61)
 )
 
 plot (known$x, known$y, type="o")
 
 # X values of points to interpolate from known data
 aim <- c(0.3, 0.7, 2.3, 3.3, 4.3, 5.6, 5.9)
 
 
 plot(approx(known$x, known$y, xout=aim))
 
 l=lm(y~x,data=known)
 summary(l)
 -0.3571+ 9.3571*aim
 
 
 model.lm <- lm(y ~ x, data = known)
 
 # Use predict to estimate the values for aim.
 # Note that predict expects a data.frame and the col 
 # names need to match
 newY <- predict(model.lm, newdata = data.frame(x = aim))
 
 #Add the predicted points to the original plot
 points(aim, newY, col = "red")
 
 cbind(aim, newY)
 
 
 MIOC = array(c(4,62,2,224,
                9,33,12,390,
                4,26,33,330,
                6,9,65,362,
                6,5,93,301),
              dim=c(2,2,5),
              dimnames=list(
                Status=c("Case","Control"),
                OCuse=c("Yes","No"),
                Agegrp=c("1","2","3","4","5")))
 
 MIOC
 
 mantelhaen.test(MIOC)
 
 apply(MIOC, 3, function(x) (x[1,1]*x[2,2])/(x[1,2]*x[2,1]))
 
 
 sapply(MIOC, function(x) (x[1,1]*x[2,2])/(x[1,2]*x[2,1]))
 
 
 
 
 
 #####################################################################################
 ####### Dose group  and High #####################################
 d1=xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,1]
 d4=xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,4]
 datan=xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,c(1,4)]
 #both=d1+d4
 both=d1+d4
 
 #d=rbind(d1,d4)
 d=datan
 
 probdist=function(d){
   d1=d[,,1]
   d4=d[,,2]
   m1=apply(d4,2,sum)
   m2=apply(d1,2,sum)
   M=m1+m2
   b=d1+d4
   p=sum(lfactorial(m1))+sum(lfactorial(m2))+
     sum(apply(lfactorial(both),2,sum))-sum(lfactorial(M))-
     sum(apply(lfactorial(d4),2,sum))-sum(apply(lfactorial(d1),2,sum))
   return((p))
 }
 pobs=probdist(d)
 pobs
 

 


 
 c=datan
 t=array(0,c(dim(c)[1],dim(c)[2],dim(c)[3]))
 ps=c()
 nsim=10
 for (j in 1:nsim){
   for (i in 1:dim(c)[3]){
     
     t[,,i]=r2dtable(1, rowSums(c[,,i]), colSums(c[,,i]))[[1]]
     
   }
   
   
   ps[j]=probdist(t)
   
 } 
 
 
 p.value=(length(((ps[pobs<=ps]))))/(nsim)
 p.value
 
 t
 ps
 
 
 #####################################################################################
 ####### Dose group control,low,medium and High #####################################
 c=xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)
 control=xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,1]
 low=xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,2]
 medium=xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,3]
 high=xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,4]
 both=control+high+medium+low

 #c[,,1]+c[,,2]+c[,,3]+c[,,4]
 #d=rbind(control,high,low,medium)
 d=c
 probdist=function(d){
   control=d[,,1]
   low=d[,,2]
   medium=d[,,3]
   high=d[,,4]
   m1=apply(control,2,sum)
   m2=apply(low,2,sum)
   m3=apply(medium,2,sum)
   m4=apply(high,2,sum)
   M=m1+m2+m3+m4
   b=control+high+low+medium
   p=sum(lfactorial(m1))+sum(lfactorial(m2))+sum(lfactorial(m3))+sum(lfactorial(m4))+
     sum(apply(lfactorial(both),2,sum))-sum(lfactorial(M))-
     sum(apply(lfactorial(high),2,sum))-sum(apply(lfactorial(control),2,sum))-
     sum(apply(lfactorial(low),2,sum))-sum(apply(lfactorial(medium),2,sum))
   return((p))
 }
 pobs=probdist(d)
 pobs
 
 

 
 t=array(0,c(dim(c)[1],dim(c)[2],dim(c)[3]))
 ps=c()
 nsim=1000
 for (j in 1:nsim){
   for (i in 1:dim(c)[3]){
     
     t[,,i]=r2dtable(1, rowSums(c[,,i]), colSums(c[,,i]))[[1]]
     
   }
   
   
   ps[j]=probdist(t)
   
 } 
 
 
 p.value=(length(((ps[pobs<=ps]))))/(nsim)
 p.value
 
 #t
# ps
 
 
 
 
 
 
 
 
 
 
 
 
  alpha
 ## Boxplots
 boxplot(elevel ~ alength, data = alpha)
 ## Asymptotic Kruskal-Wallis test
 kruskal_test(elevel ~ alength, data = alpha)
 
 alzheimer
 
 
 ## Spineplots
 op <- par(no.readonly = TRUE) # save current settings
 layout(matrix(1:2, ncol = 2))
 spineplot(disease ~ smoking, data = alzheimer,
           subset = gender == "Male", main = "Male")
 spineplot(disease ~ smoking, data = alzheimer,
           subset = gender == "Female", main = "Female")
 par(op) # reset
 ## Asymptotic Cochran-Mantel-Haenszel test
 cmh_test(disease ~ smoking | gender, data = alzheimer)
 
 asat
 ## Proof-of-safety based on ratio of medians (Pflueger and Hothorn, 2002)
 ## One-sided exact Wilcoxon-Mann-Whitney test
 wt <- wilcox_test(I(log(asat)) ~ group, data = asat,
                   distribution = "exact", alternative = "less",
                   conf.int = TRUE)
 ## One-sided confidence set
 ## Note: Safety cannot be concluded since the effect of the compound
 ## exceeds 20 % of the control median
 exp(confint(wt)$conf.int)
 
 lbl_test(shelltox)
 
 
 l1=list(a = c(1:10), b = c(11:20))
 l2=list(c = c(21:30), d = c(31:40))
 # sum the corresponding elements of l1 and l2
 mapply(sum, l1$a, l1$b, l2$c, l2$d)
 
 l=list(a = 1:10, b = 11:20)
 # log2 of each value in the list
 rapply(l, log2)
 sapply(l, log2)
 
 
 
 X <- c(rep(1,17),rep(2,15))
 YI <- c(rep(1,5),rep(2,11),3,rep(1,3),rep(2,8),rep(3,4))
 YII <- c(rep(2,16),rep(3,1),rep(1,8),rep(2,3),rep(3,4))
 
 
 s=matrix(c(1,2,2,1,3,3),2)
 a=array(s,c(2,3,2))
 sum(a)
 u=seq(1,dim(s)[1],1)
 u1=matrix(u,2,3)
 u2=array(u,c(2,1,2))
 u2[,,1]%*%a[,,1]
 u2%*%a
 a%*%u2
 a*u1
 u1*a
 u*u2

 
a=array(s,c(2,3,2))
z=apply(colSums(a),2,rank)
z3=t(z)
z2=apply(apply(a,3,rowSums),2,rank)
dim(a[,,1]) 
dim(z[1,])

sum(t(z2[,1]*a[,,1])*(z[,1]))

r=c()
for (i in 1:dim(a)[3]){
  
  
  r[i]=  sum(t(z2[,i]*a[,,i])*z[,i])
 
}

r

 (z[,1])%*%z2[1,] 
 
  
  
  
 #########
 ## mult array a by row ranks [1.2]
 u3=u*a
 u4=matrix(c(1,2,3),2)
 u5=c(1,2,3)
 t(u*a)
 u3*u5
 u4*u3
 apply(u3,3,function(x) t(x))*u5
 
 sum(apply(u3,3,function(x) t(x))*u5)
 
 v=seq(1,dim(s)[2],1)
 t(u*s)*v
 sum(t(u*s)*v)
 
v= matrix(c(1,3,10,6,2,3,10,7,1,6,14,12,0,1,9,11),byrow=T,4)
rank(colSums(v))
rank(rowSums(v))

## Approximative (Monte Carlo) Cochran-Mantel-Haenszel test
## Serial operation
set.seed(123)
cmh_test(disease ~ smoking | gender, data = alzheimer,
         distribution = approximate(B = 100000))

cmh_test(shelltox$Freq ~ shelltox$Trt+shelltox$NResp |factor(shelltox$ClusterSize),
         distribution = approximate(B = 100000))
cmh_test(xtabs(Freq~Trt+NResp+ClusterSize,data=shelltox), data = shelltox,
         distribution = approximate(B = 100000))

xtabs(Freq~Trt+NResp+ClusterSize,data=shelltox)
head(shelltox)
factor(shelltox$ClusterSize)
str(shelltox)


c=matrix(c(2,2,3,2,2,3,2,2,3,1,2,3,1,1,1),5,3)
#c=matrix(c(2,2,3,2,2,3,2,2,3),3,3)
#c=w1

midrankscores=function(c){

d=matrix(0,dim(c)[1],dim(c)[2])

d[1,] =0.5*(c[1,]+1 )
  
d[2,]=c[1,]+0.5*(c[2,]+1)

  for (i in 3:dim(c)[1]){
  

d[i,]=colSums(c[1:(i-1),])+0.5*(c[i,]+1)


}
 return(d)
}
 
 
midrankscores(c)

midrankscores(w1)

v1=as.vector(colSums(c[1:(3),]))
v=c(colSums(c[1:(3),]))
dim(v)
dim(v1)

matrix(c(1,1,1,1),2,2)/matrix(c(2,2,2,2),2,2)


## Fisher's Tea Drinker data.
TeaTasting <-
  matrix(c(3, 1, 1, 3),
         nrow = 2,
         dimnames = list(Guess = c("Milk", "Tea"),
                         Truth = c("Milk", "Tea")))
## Simulate permutation test for independence based on the maximum
## Pearson residuals (rather than their sum).
rowTotals<-rowSums(TeaTasting)
colTotals<-colSums(TeaTasting)
dim(a)
dim(b)
a=matrix(colTotals,1,2)
b=matrix(rowTotals,2,1)
b%*%a
nOfCases<-sum(rowTotals)
sum(TeaTasting)
expected<-outer(rowTotals, colTotals, "*") / nOfCases
maxSqResid<-function(x) max((x - expected) ^ 2 / expected)
simMaxSqResid <-
  sapply(r2dtable(1000, rowTotals, colTotals), maxSqResid)
sum(simMaxSqResid >= maxSqResid(TeaTasting)) / 1000



library("metafor")
data("dat.bcg", package = "metafor")
print(dat.bcg, row.names = FALSE)


library(car) # for data sets
plot(prestige~ income, xlab="Average Income", ylab="Prestige", data=Prestige)
with(Prestige, lines(lowess(income, prestige, f=0.2, iter=0), lwd=2))

 mod.lo <-loess(prestige ~ income + education, span=.5, degree=1, data=Prestige)
summary(mod.lo)
str(Prestige)


inc <- with(Prestige, seq(min(income), max(income), len=25))
ed <- with(Prestige, seq(min(education), max(education), len=25))
newdata <- expand.grid(income=inc, education=ed)
fit.prestige <- matrix(predict(mod.lo, newdata), 25, 25)
 persp(inc, ed, fit.prestige, theta=45, phi=30, ticktype="detailed",
             xlab="Income", ylab="Education", zlab="Prestige", expand=2/3,
         shade=0.5)
 
mod.lo.inc <- loess(prestige ~ income, span=.7, degree=1,
                        data=Prestige) # omitting education
mod.lo.ed <- loess(prestige ~ education, span=.7, degree=1,
                       data=Prestige) # omitting income

anova(mod.lo.inc, mod.lo) # test for education
anova(mod.lo,mod.lo.inc ) 

x=seq(-10,10,0.01)
y=x^3/(x^2-1)
plot(x,y)


library(mgcv)
mod.gam <- gam(prestige ~ s(income) + s(education), data=Prestige)
summary(mod.gam)

#newdata=data.frame(c(15,23),1,2)
predict(mod.gam, newdata)


remove(list=objects()) # clean up everything
Mroz$k5f <- factor(Mroz$k5)
Mroz$k618f <- factor(Mroz$k618)
Mroz$k5f <- recode(Mroz$k5f, "3 = 2")
Mroz$k618f <- recode(Mroz$k618f, "6:8 = 5")
mod.1 <- gam(lfp ~ s(age) + s(inc) + k5f + k618f + wc + hc,
                family=binomial, data=Mroz)
summary(mod.1)

anova(mod.1)
cut(rep(1,5), 6)
Z <- stats::rnorm(100000)
sum(rnorm(100000))
sum(stats::rnorm(100000))
table(cut(Z, breaks = -6:6))
sum(table(cut(Z, breaks = -6:6, labels = FALSE)))
sum(graphics::hist(Z, breaks = -6:6, plot = FALSE)$counts)

hist(seq(1,7,1),breaks=1:7,plot=F)
cut(rep(1,5), 4) #-- dummy
tx0 <- c(9, 4, 6, 5, 3, 10, 5, 3, 5)
x <- rep(0:8, tx0)
stopifnot(table(x) == tx0)

table = matrix(c(18,20,15,15,10,55,65,70,30), 3, 3)
colSums(table)
rowSums(table)
N = sum(table)                  # this is the total number of observations
N

table/N
# [1] 298
probs = prop.table(table)       # these are the probabilities of an observation
probs
sum(probs)#  being in any given cell
probs.v = as.vector(probs)      # notice that the probabilities read column-wise
probs.v
cuts = c(0, cumsum(probs.v))    # notice that I add a 0 on the front
cuts

set.seed(4941)                  # this makes it exactly reproducible
B      = 10                  # number of itterations in simulation
vals   = runif(N*B)             # generate random values / probabilities
cats   = cut(vals,breaks=cuts, labels=c("11","21","31","12","22","32","13","23","33"))
cats   = matrix(cats, nrow=N, ncol=B, byrow=F)
counts = apply(cats, 2, function(x){ as.vector(table(x)) })
apply(counts,2,sum)

a=matrix(counts[,3], nrow=3, ncol=3, byrow=T)
colSums(a)
rowSums(a)
sum(a)
rm(table, N, vals, probs, probs.v, cuts, cats) 
p.vals = vector(length=B)       # this will store the outputs
ptm = proc.time()               # this lets me time the simulation
for(i in 1:B){
  mat       = matrix(counts[,i], nrow=3, ncol=3, byrow=T)
  p.vals[i] = fisher.test(mat, simulate.p.value=T)$p.value
}
proc.time() - ptm               # not too bad, really
#  user  system elapsed 
# 28.66    0.32   29.08 
#
mean(p.vals>=.05)               # the estimated probability of type II errors is 0
# [1] 0
c(0, 3/B)                       # using the rule of 3 to estimate the 95% CI
# [1] 0e+00 3e-04




library(Exact)

#Create a data frame called d, populate it with the numbers above.
set.seed(12345)
nTreated <- 100
nUntreated <- 100
probTreated <- 0.1
probUntreated <- 0.08

d <- data.frame(id = 1:10000)
d$nTreated <- nTreated
d$nUntreated <- nUntreated
d$probTreated <- probTreated 
d$probUntreated <- probUntreated


#Generate some random results using rbinom()
d$treatedCrashes <- apply(cbind(d$nTreated, d$probTreated), 1, 
                          function(x)  sum(rbinom(x[1], 1, x[2])))

d$untreatedCrashes <- apply(cbind(d$nUntreated, d$probUntreated), 1, 
                            function(x)  sum(rbinom(x[1], 1, 
                                                    x[2])))


#Do fisher's exact test on each replication:
d$fisher <- apply(cbind(d$nTreated - d$treatedCrashes, 
                        d$treatedCrashes,
                        d$nUntreated - d$untreatedCrashes, 
                        d$untreatedCrashes), 1, 
                  function(x)  fisher.test(matrix(x, 
                                                  nrow=2))$p.value)





a=cbind(d$nTreated - d$treatedCrashes, 
      d$treatedCrashes,
      d$nUntreated - d$untreatedCrashes, 
      d$untreatedCrashes)


head(a)

#test power
mean(d$fisher < 0.05)
  
?rbinom
sum(rbinom(100,1,0.1))


qchisq(p=0.975, df=10, ncp = 0, lower.tail = TRUE, log.p = FALSE)


a=c(1,2,3,4,5,6,8)
b=c(3,4,5,7,8,1,9)
b[b>a]
length(b>a)
b[b>4]
outer(a,b,"*")
ppoints(n=8, a = ifelse(n <= 10, 3/8, 1/2))
ppoints(4) # the same as  ppoints(1:4)
ppoints(10)
ppoints(10, a = 1/2)
## visual testing
## do P-P plots for 1000 points at various degrees of freedom
L <- 1.2; n <- 1000; pp <- ppoints(n)
op <- par(mfrow = c(3,3), mar = c(3,3,1,1)+.1, mgp = c(1.5,.6,0),
          oma = c(0,0,3,0))
for(df in 2^(4*rnorm(9))) {
  plot(pp, sort(pchisq(rr <- rchisq(n, df = df, ncp = L), df = df, ncp = L)),
       ylab = "pchisq(rchisq(.),.)", pch = ".")
  mtext(paste("df = ", formatC(df, digits = 4)), line =  -2, adj = 0.05)
  abline(0, 1, col = 2)
}
mtext(expression("P-P plots : Noncentral  "*
                   chi^2 *"(n=1000, df=X, ncp= 1.2)"),
      cex = 1.5, font = 2, outer = TRUE)
par(op)

pchisq(60,40)
1-pchisq(q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)
pchisq(q, df, ncp = 0, lower.tail = F, log.p = FALSE)


rmultinom(3, size = 6, prob = c(0.1,0.2,0.8))


c=matrix(c(1,2,3,4,5,6,7,8,9),3,3)
cumsum(c)
b=c(1,2,3)
b^2
sum(shell)
sum()
c^2


c=array(c(1,1,1,1,2,2,2,2,3,3,3,3),c(2,2,3))
#v=matrix(seq(1,dim(c)[2],1),dim(c)[2],dim(c)[3])
#u=matrix(seq(1,dim(c)[1],1),dim(c)[1],dim(c)[3])

v=seq(1,dim(c)[2],1)-1
u=seq(1,dim(c)[1],1)-1
v[1];v[2]
u[1];u[2]
  tobs=sum(apply(u*c,3,function(x) t(x))*v)
  tobs=sum(apply(u*c,3,function(x) (x))*v)
  
 
  ts=array(0,c(dim(c)[1],dim(c)[2],dim(c)[3]))
  for(i in 1:dim(c)[1]){
    for (j in 1:dim(c)[2]){
      
      for (k in 1:dim(c)[3]){
       
        ts[i,j,k]=u[i]*c[i,j,k]*v[j]
        
        
      }
      
    }
    
  }
  
  ts= apply(ts ,3,sum)
   
 ts=apply( (apply(u*c,3,function(x) t(x))*v),2,sum)
 
 
 
 c=array(c(1,1,1,1,1,1,1,2,2,2,2,3,3,3,3,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2),c(3,4,3))
 as.vector(c)
 #### Wiloxon ranks of nij,midranks for ties####
 
 midrankscores=function(c){
   
   d=matrix(0,dim(c)[1],dim(c)[2])
   
   d[1,] =0.5*(c[1,]+1 )
   
   d[2,]=c[1,]+0.5*(c[2,]+1)
   
   for (i in 3:dim(c)[1]){
     
     
     d[i,]=colSums(c[1:(i-1),])+0.5*(c[i,]+1)
     
     
   }
   return(d)
 }
 
# midrankscores(c[,,3])
 
 
 v=midrankscores(apply(colSums(c),2,rank))
 
 u=midrankscores(apply(apply(c,3,rowSums),2,rank))
 
 
 multiply2=function(c,u,v) {
   ts=array(0,c(dim(c)[1],dim(c)[2],dim(c)[3]))
   
  for (k in 1:dim(c)[3]){
         
         ts[,,k]=t(u[,k]*c[,,k])*v[,k]
         
       }
       
   tss=sum(ts)
   return(list(tss))
 }
 
 multiply2(c,u,v)[[1]]
 
 sum(multiply2(c,u,v)[[1]])
 
 
 sum(multiply2(c,u,v)[[2]])
 sum(multiply2(c,u,v)[[2]][,,1])
 
c1=c[,,1]
 v1=matrix(c(1.25,2.75,5,8.5,1,1,1,1,1,1,1,1,1,1,1,1),4,4)
 u1=matrix(c(1,2.5,5,1,1,1,1,1,1),3)
u11=c(1,2.5,5);v11=c(1.25,5,5,8.5);
 t(u11*c1)*v11
dim(u1)
dim(v1)
sum( t(u1[,1]*c1)*v1[,1])
u1*v1

multiply=function(c,u,v) {
  ts=array(0,c(dim(c)[1],dim(c)[2],dim(c)[3]))
  
  for(i in 1:dim(c)[1]){
    for (j in 1:dim(c)[2]){
      
      for (k in 1:dim(c)[3]){
        
        ts[i,j,k]=u[i,j]*c[i,j,k]*v[j]
        
        
      }
      
    }
    
  }
  tss=sum(ts)
  return(tss)
}




library(geepack)
# ohio dataset from geepack - Health effect of air pollution
# Children followed for four years, wheeze status recorded annually
data(ohio) # Load the dataset
head(ohio)
str(ohio)
# Response is binary - fit a logistic GEE model
# Treat time (age) as continuous
fit.exch <- geeglm(resp~age+smoke, family=binomial(link="logit"),
                   data=ohio, id=id, corstr = "exchangeable", std.err="san.se")
fit.unstr <- geeglm(resp~age+smoke, family=binomial(link="logit"),
                    data=ohio, id=id, corstr = "unstructured", std.err="san.se")
summary(fit.exch)
summary(fit.unstr)

library(betareg)
data("GasolineYield", package = "betareg")
gy_logit <- betareg(yield ~ batch + temp, data = GasolineYield)
summary(gy_logit)
fit=glm(yield ~ batch + temp, data = GasolineYield)
summary(fit)

events.0=0   # for X = 0
events.1=5   # for X = 1
x = c(rep(0,100), rep(1,100))
y = c(rep(0,100-events.0), rep(1,events.0),
      rep(0, 100-events.1), rep(1, events.1))

library(MCMCpack)
logmcmc = MCMClogit(y~as.factor(x), burnin=1000, mcmc=21000, b0=0, B0=.04)

summary(logmcmc)

plot(logmcmc)


#############################
#### monte carlo ###########

X = rnorm(5e4,mean=1,sd=2) # we need a total of 5 x 10,000 = 5e4 samples
X = matrix(data=X,nrow=1e4,ncol=5)
Mean.dist = apply(X, MARGIN = 1, FUN = "mean")
mean(Mean.dist)
sd(Mean.dist)

Mean.sd = apply(X, MARGIN = 1, FUN = "sd")
Mean.se = Mean.sd/sqrt(5)

X2=rnorm(50e4,mean=1,sd=2) # up to n = 50
X2=rgamma(50e4,shape=1,scale=1)
X2 = matrix(data=X2,nrow=1e4,ncol=50)

# pre-allocate some variables for the for loop
Means = rep(NA,ncol(X2))
SDs = Means
SEs = Means
n = 0

# now loop over each possible sample size
for(n in 2:ncol(X2)){
  Mean.dist = apply(X2[,1:n],MARGIN=1,FUN="mean")
  Means[n]=mean(Mean.dist)
  SDs[n]=sd(Mean.dist) # this is the actual SD of the distribution of means
  SE.dist = apply(X2[,1:n],MARGIN=1,FUN='sd')
  SEs[n]=mean(SE.dist)/sqrt(n) # average estimate of the SE for each n
}

n = seq(1,ncol(X2))
Mean.pred = rep(1,ncol(X2))
SD.pred = rep(2,ncol(X2))/sqrt(n)

plot(n,Mean.pred )
lines(n,SD.pred)
lines(n,Means)
lines(n,SDs)
lines(n,SEs)

MU = seq(0,5,0.01)
Test = rep(NA,length(MU))
for(n in 1:length(MU)){
  # Create 'samples' drawn from distribution with that value of MU
  Data = matrix(rnorm(5e4,MU[n],1),nrow=1e4,ncol=5)
  # Calculate mean, SE
  Means = apply(Data,MARGIN=1,FUN="mean")
  SEs = apply(Data,MARGIN=1,FUN="sd")/sqrt(5)
  # Now do the test. We will assume that we are testing at alpha=0.05
  # So if the lower CI does not overlap zero, we reject the null hypothesis
  Lower.CIs = Means-1.96*SEs
  #How many times is null hypothesis rejected?
  Test[n] = mean(Lower.CIs > 0)
  # The proportion of cases in which we reject the (false) null hypo. = Power.
}

plot(Test,MU)
plot(MU,Test)
array(c(1,1,2,2,3,3,4,4,1,1,2,2,3,3,4,4),c(c(2,2,2),c(2,2,2),c(2,2,2)))

array(c(1,1,2,2,3,3,4,4,1,1,2,2,3,3,4,4),c(c(2,2),2,2))


Ns = seq(1,100,1)
Ts = seq(0,1,0.01)
Frac = Ts*NA
MinFrac = Ns*NA
Data=rbinom(n=1e4,size=Ns,prob=Ts) 
rbinom(n=1e4,size=10,prob=0.5) 
for(N in 1:length(Ns)){ # loop over Ns
  for(T in 1:length(Ts)){ # loop over Ts for each value of N
    Data = rbinom(n=1e4,size=Ns[N],prob=Ts[T]) # simulate the data
    Frac[T] = mean(Data==0) # fraction of observations with no malformations
  } # end the loop over Ts
  is5pct = Frac<0.05 # the values of T for which <5% samples are all zero
  whichT = which(is5pct) # indices of T corresponding to TRUE values in is5pct
  MinFrac[N]=Ts[whichT[1]] # the lowest value of T that can be rejected at 95%
} # end the loop over Ns   

length(Data[Data==0])/length(Data)
mean(Data==0)
MinFrac
whichT
is5pct
plot(Ns,MinFrac)


M = rep(NA,1e4)
N = 100
Data = rnorm(N,mean=0,sd=1)
for(b in 1:1e4){
  Bsamp = sample(x = Data,size = N,replace=TRUE)
  M[b] = mean(Bsamp) # find the mean of the resampled dataset
}


Data = rnbinom(10,size=1,mu=0.5)
var(Data)
mean(Data)


Btest = rep(NA,1e4)
library(gdata)
for(b in 1:1e4){
  Bsamp = resample(Data,length(Data),replace=TRUE) # resample Data
  Btest[b] = var(Bsamp)-mean(Bsamp) # calculate the 'test statistic'
}

quantile(Btest,0.05)
quantile(Btest,0.95)

install.packages("rmr2" )
library(rmr2 )
install.packages("SparkR")
library(SparkR)


kroger=c(34,30,40,38,36,30,30,42,36,38)
hefty=c(32,42,34,36,32,40,36,43,30,38)
tuff=c(33,34,32,40,40,34,36,34,32,34)
glad=c(26,18,20,15,20,20,17,18,19,20)
length(kroger)
length(hefty)
length(tuff)
length(glad)
x=c(kroger,hefty,glad,tuff)
y=c(rep("K",10),rep("h",10),rep("g",10),rep("t",10))
d = data.frame(x,y)

yy=y=c(rep(1,10),rep(3,10),rep(2,10),rep(4,10))
dd = data.frame(x,yy)

fit <- aov(x ~ y,data=dd)
summary(fit)



fit <- lm(x ~ yy)
summary(fit)
aov(x~y)

library(ORE)
library(ORCH)

is.integer (1)
allreduce (x)


c=c(1,2,3)
n=length(c)
d=c(4,5,6)
mean(c)*mean(d)
(sum(c)*sum(d))/n
((sum(c)/n)*(sum(d)/n))

X <- c(rep(1,17),rep(2,15))
Y <- c(rep(1,6),rep(2,11),rep(1,2),rep(2,8),rep(3,5))
X <- c(rep(1,17),rep(2,15))
YI <- c(rep(1,5),rep(2,11),3,rep(1,3),rep(2,8),rep(3,4))
YII <- c(rep(2,16),rep(3,1),rep(1,8),rep(2,3),rep(3,4))
cor(X,Y)
data.frame(X,Y)
mr <- rank(Y)
w2.mr <- (sort(unique(mr))[2]-min(mr))/(max(mr)-min(mr))
w2.mr

F <- ecdf(Y)
 varF <-sort(unique(F(Y)))*(1-sort(unique(F(Y))))
w2.ad<-(1/sqrt(varF[1]))/(1/sqrt(varF[1])+1/sqrt(varF[2]))
w2.ad
library(CorrBin)
library(vcd)
library(coin)
data(shelltox)

shell=(xtabs(Freq~Trt+NResp+ClusterSize,data=shelltox))

c=shell
x1=c(rep(1,7),rep(2,7),rep(3,7),rep(4,7))
y1=c(rep(0,7),1,rep(0,6),rep(0,14))
cor(rank(x1),rank(y1))
as.vector(c)