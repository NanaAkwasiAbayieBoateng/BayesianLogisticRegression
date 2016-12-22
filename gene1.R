rm(list=ls())
B=5
k=15
m=5000
x=NULL
y=NULL

while(length(x)<m){

x.val=c(runif(1,0,B),rep((B+1),length=k))  
y.val= c(runif(1,0,B),rep((B+1),length=k))
 for (j in 2:(k+1)){
  while(x.val[j]>B) x.val[j]=rexp(1,y.val[j-1])
  while(y.val[j]>B) y.val[j]=rexp(1,x.val[j]) 
 }
 x=c(x,x.val[(k+1)]) 
 y=c(y,y.val[(k+1)]) 
}

par(mfrow=c(2,1))
hist(x,breaks="fd",col="gray",prob=TRUE,main="Marginal Distribution of x")
hist(y,breaks="fd",col="gray",prob=TRUE,main="Marginal Distribution of x")