 #datan=read.table("~/Desktop/DR. Eugene/earnings.txt")
 
# write.csv(datan,file="~/Desktop/DR. Eugene/earnings.csv")

new=read.csv("~/Desktop/DR. Eugene/earnings.csv")
head(new)
r=glm(Earnings~Ethnicity+ Sex+Height+Education+Age,data=new)
r$coefficients
summary(r)
levels(new$Ethnicity)
levels(new$Sex)
library(xtable)

#############################
#### html output ######

 library(R2HTML)
htlmtable <-(r$coefficients)
print(htlmtable,type="html")

 HTMLStart(outdir="~/Desktop/DR. Eugene", file=" htlmtable",
           extension="html", echo=FALSE, HTMLframe=TRUE)
 
 HTML.title("My Report", HR=1)
 
 HTML.title("Description of my data", HR=3)
 summary(mydata) 
 
 HTMLhr()
 
 HTML.title("X Y Scatter Plot", HR=2)
 plot(new$Earnings~new$Education)
 HTMLplot() 
 
 HTMLStop()
 
 
install.packages("xtable")
library(xtable)
VARIABLE=c("",gsub("[-^0-9]", "", names(unlist(r$xlevels))))
MODALITY=c("",as.character(unlist(r$xlevels)))
htlmtable <- xtable(merge(names,r))
print(htlmtable,type="html")
summary(r)

(xtabs(~Earnings+Education+Age+Sex+Ethnicity+Height,data=new))
(xtabs(~Earnings+Education,data=new))
(xtabs(~Earnings+Age,data=new))
(xtabs(~Earnings+Sex,data=new))
(xtabs(~Earnings+Ethnicity,data=new))
(xtabs(~Earnings+Height,data=new))
(xtabs(~Earnings+Education+Ethnicity+Sex,data=new))


structable(Earnings+Education ~ Sex+Ethnicity, data=new)

xtabs(Earnings ~ Sex + Age +Ethnicity, data=new)

library(gmodels)
library(gmodels)
CrossTable(GSStab,prop.t=FALSE,prop.r=FALSE,prop.c=FALSE)
CrossTable((xtabs(~Earnings+Sex,data=new)),prop.t=FALSE,prop.r=FALSE,prop.c=FALSE)

chisq.test(xtabs(~Earnings+Sex,data=new))

 
 str(new)

table(new)

ordered(sort(new))
ftable(new)

structable(new)

structable(~Earnings+Education,data=new)

xtable(Earnings~Sex,data=new)

table(new)


myreport=CrossTable((xtabs(~Earnings+Sex,data=new)),prop.t=FALSE,prop.r=FALSE,prop.c=FALSE)
library(R2HTML)
HTMLStart(outdir="~/Desktop/DR. Eugene", file="myreport",
          extension="html", echo=FALSE, HTMLframe=TRUE)

table{
  max-width: 95%;
  border: 1px solid #ccc;
  }

th {
  background-color: #000000;
    color: #ffffff;
}

td {
  background-color: #dcdcdc;
}

options(rstudio.markdownToHTML = 
          function(inputFile, outputFile) {      
            require(markdown)
            markdownToHTML(inputFile, outputFile, stylesheet='custom.css')   
          }
)
(CrossTable((xtabs(~Earnings+Sex,data=new)),prop.t=FALSE,prop.r=FALSE,prop.c=FALSE))




library(xtable)
data(iris)
print(xtable(head(iris, 10)), include.rownames = F)
library(xtable)
htlmtable <- xtable(merge(names,regression))
data(iris)
htlmtable <-xtable(head(iris, 10))
print(htlmtable,type="html")


library(xtable)
htlmtable <- xtable(merge(names,regression))
print(htlmtable,type="html")