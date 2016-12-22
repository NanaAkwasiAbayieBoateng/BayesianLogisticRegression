moverings=function(numrings,frompole,topole){
  
if (numrings==1) {
  
cat("move ring 1 from pole",frompole,"to pole",topole,"\n")  
  
}else{
  
sparepole=6-frompole-topole
moverings(numrings-1,frompole,sparepole)
cat("move ring",numrings,"from pole",frompole,"to pole",topole,"\n")
moverings(numrings-1,sparepole,topole)
} 
 return(invisible(NULL)) 
  
}

moverings(3,1,3)

moverings(2,1,3)





logistic=function(n,r,x1){
fx=c()
x=c()

x[1]=x1
for (i in 1:(n-1)){
  
  if(((r>=0&r<=4)&(x[i]>=0&x[i]<=1))){
    
    fx[i]=r*x[i]*(1-x[i])
    
      x[i+1]=fx[i]
    
  }else {
   fx= cat("check x or r value") 
    
  }

}
#return(x)
graph=plot(seq(1,n),x,ylab="logistic",xlab="time in years",pch=19)
return(graph)

}

logistic(n=100,3.1,x1=0.1)


