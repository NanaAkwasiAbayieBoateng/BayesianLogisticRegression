
#install.packages("R2jags", dependencies = TRUE, repos = "http://cran.us.r-project.org")
#install.packages("runjags", dependencies = TRUE, repos = "http://cran.us.r-project.org")
#install.packages("MCMCpack", dependencies = TRUE, repos = "http://cran.us.r-project.org")
#setwd("/var/folders/mj/w1gxzjcd0qx2cw_0690z7y640000gn/T//RtmpzQiX7B/downloaded_packages")
install.packages(c("R2jags","rjags","runjags","MCMCpack","lattice","superdiag",
                   "mcmcplots","ggmcmc"), dependencies = TRUE, repos = "http://cran.us.r-project.org")
library(R2jags)
library(rjags)
library(runjags)
library(MCMCpack)
library(lattice)
library(superdiag)
library(mcmcplots)
library(ggmcmc)
set.seed(1337)
y <- rnorm(n = 20, mean = 10, sd = 5)
mean(y)
# The model specification
model_string <- "model{
for(i in 1:length(y)) {
y[i] ~ dnorm(mu, tau)
}
mu ~ dnorm(0, 0.0001)
sigma ~ dlnorm(0, 0.0625)
tau <- 1 / pow(sigma, 2)
}"

# Running the model
model <- jags.model(textConnection(model_string), data = list(y = y), n.chains = 3, n.adapt= 10000)
update(model, 10000); # Burnin for 10000 samples
mcmc_samples <- coda.samples(model, variable.names=c("mu", "sigma"), n.iter=20000)



plot(mcmc_samples)

summary(model)
summary(mcmc_samples )



# An example model file is given in:
 model.file <- system.file(package = "R2jags", "model", "schools.txt")
# data
 J <- 8.0
y <- c(28.4,7.9,-2.8,6.8,-0.6,0.6,18.0,12.2)
sd <- c(14.9,10.2,16.3,11.0,9.4,11.4,10.4,17.6)

jags.data <- list("y","sd","J")
jags.params <- c("mu","sigma","theta")
jags.inits <- function(){
 list("mu"=rnorm(1),"sigma"=runif(1),"theta"=rnorm(J))
  
}


 # Fit the model
jagsfit <- jags(data=list("y","sd","J"), inits = jags.inits,
                     jags.params, n.iter = 10, model.file = model.file)


summary(jagsfit)
update(jagsfit, 10000);
#mcmc_samples <- coda.samples(jagsfit, variable.names=c("mu", "sigma","theta"), n.iter=20000)


n.sim <- 100; set.seed(123)
x1 <- rnorm(n.sim, mean = 5, sd = 2)
x2 <- rbinom(n.sim, size = 1, prob = 0.3)
e <- rnorm(n.sim, mean = 0, sd = 1)


b1 <- 1.2
b2 <- -3.1
a <- 1.5
y <- a + b1 * x1 + b2 * x2

 sim.dat <- data.frame(y,x1,x2)   
 
freq.mod <- lm(y ~ x1 + x2, data = sim.dat)
 summary(freq.mod)
 
 
 bayes.mod <- function(){
  for(i in 1:N){
  y[i] ~ dnorm(mu[i], tau)
 mu[i] <- alpha + beta1 * x1[i] + beta2 * x2[i]
  }
 alpha ~ dnorm(0, .01)
  beta1 ~ dunif(-100, 100)
  beta2 ~ dunif(-100, 100)
  tau ~ dgamma(.01, .01)
 }
 
 y <- sim.dat$y
  x1 <- sim.dat$x1
 x2 <- sim.dat$x2
 N <- nrow(sim.dat)
 
 sim.dat.jags <- list("y", "x1", "x2", "N")
 sim.dat.jags <- as.list(sim.dat)
 
 sim.dat.jags$N <- nrow(sim.dat)
 bayes.mod.params <- c("alpha", "beta1", "beta2")
 
  bayes.mod.inits <- function(){
  list("alpha" = rnorm(1), "beta1" = rnorm(1), "beta2" = rnorm(1))
 }
 
 
 
 inits1 <- list("alpha" = 0, "beta1" = 0, "beta2" = 0)
 inits2 <- list("alpha" = 1, "beta1" = 1, "beta2" = 1)
 inits3 <- list("alpha" = -1, "beta1" = -1, "beta2" = -1)
 bayes.mod.inits <- list(inits1, inits2, inits3)
 library(R2jags)
 

 set.seed(123)
 
 
 
bayes.mod.fit=jags(data = sim.dat.jags, inits = bayes.mod.inits,
                          parameters.to.save = bayes.mod.params, n.chains = 3, n.iter = 9000,
                          n.burnin = 1000, model.file = bayes.mod)


mod.fit.upd <-update(bayes.mod.fit, n.iter=1000)
bayes.mod.fit.upd <-autojags(bayes.mod.fit)
print(bayes.mod.fit)

par(mfrow=c(2,2))
plot(bayes.mod.fit)
traceplot(bayes.mod.fit)


bayes.mod.fit.mcmc <- as.mcmc(bayes.mod.fit)
summary(bayes.mod.fit.mcmc)

xyplot(bayes.mod.fit.mcmc)


densityplot(bayes.mod.fit.mcmc)


densityplot(bayes.mod.fit.mcmc, layout=c(2,2), aspect="fill")

plot(bayes.mod.fit.mcmc)

autocorr.plot(bayes.mod.fit.mcmc)

geweke.diag(bayes.mod.fit.mcmc)

heidel.diag(bayes.mod.fit.mcmc)

############################################
############### ggplots################

install.packages("superdiag", dependencies = TRUE, repos = "http://cran.us.r-project.org")

#library(superdiag)

superdiag(bayes.mod.fit.mcmc, burnin = 100)

install.packages("mcmcplots", dependencies = TRUE, repos = "http://cran.us.r-project.org")

#library(mcmcplots)

denplot(bayes.mod.fit.mcmc)

mcmcplot(bayes.mod.fit.mcmc)

caterplot(bayes.mod.fit.mcmc)


caterplot(bayes.mod.fit.mcmc, parms = c("alpha", "beta1", "beta2"),
          labels = c("alpha", "beta1", "beta2"))



install.packages("ggmcmc", dependencies = TRUE, repos = "http://cran.us.r-project.org")


bayes.mod.fit.gg <- ggs(bayes.mod.fit.mcmc)
ggs_density(bayes.mod.fit.gg)

denplot(bayes.mod.fit.mcmc, parms = c("alpha", "beta1", "beta2"))
traplot(bayes.mod.fit.mcmc, parms = c("alpha", "beta1", "beta2"))




###############################################################################
## R2jags example using Simon Jackman's Turnout example - logit/probit model ##
###############################################################################

## Johannes Karreth
## ICPSR Summer Program 2015

## Compare to Simon Jackman's BUGS code: 
## http://jackman.stanford.edu/mcmc/mainFrameWinBugs.php#Turnout


## Required libraries
library(foreign)
library(R2jags)

setwd("~/R/Bayes/turnout")

## Read the data
## This time, read in the data from an external file. Copy the data from Simon Jackman's WinBUGS code into a text file, turnout.bugs.txt, in your WD. Then,

bugs2jags("turnout.bugs.txt", "turnout.jags.txt")

## Alternatively, read in the data from the web: 
source("http://www.jkarreth.net/files/turnout.dat.jags")

## Then create a list of these elements:

turnout.data=list("y", "educ", "age", "south", "govelec", "closing", "N")


## Model:
## Might want to run the model only on the first 300 or so observations to save time while playing around:

turnout.model.jags <- function()  {
  for (i in 1:N){   ## or: for (i in 1:300)              
    y[i] ~ dbern(p[i])  
    logit(p[i]) <- mu[i]                   
    mu[i] <- beta1              
    + beta2 *educ[i]
    + beta3 *age[i]
    + beta4 *south[i]
    + beta5 *govelec[i]
    + beta6 *closing[i]
    
    
    llh[i] <- y[i]*log(p[i]) + (1-y[i])*log(1-p[i])   
  }
  
  sumllh <- sum(llh[])     
  
  
  
  beta1 ~ dnorm(0.0,1.0E-6)
  beta2 ~ dnorm(0.0,1.0E-6)
  beta3 ~ dnorm(0.0,1.0E-6)
  beta4 ~ dnorm(0.0,1.0E-6)
  beta5 ~ dnorm(0.0,1.0E-6) 
  beta6 ~ dnorm(0.0,1.0E-6)
  
}


## Name the JAGS parameters

turnout.params <- c("beta1", "beta2", "beta3", "beta4", "beta5", "beta6")

## Define the starting values for JAGS

#  turnout.inits <- function(){
#  list("beta1"=c(0), "beta2"=c(0), "beta3"=c(0), "beta4"=c(0), "beta5"=c(0), "beta6"=c(0), "beta7"=c(0), "beta8"=c(0), "beta9"=c(0), "beta10"=c(0))
#  }

#  turnout.inits <- function(){
#  list(beta=c(-0.3392423378,  0.0741911953,  0.0012163747,  0.0230970246, -0.0001679677, -0.0333484965,  0.0204799754, -0.0068319918, 0.0017752978, -0.0001432201))
#  }

inits1 <- list(beta1 = 0, beta2 = 0, beta3 = 0, beta4 = 0, beta5 = 0, beta6 = 0) 
inits2 <- list(beta1 = -3.49, beta2 = 0.55, beta3 = 0.03, beta4 = -0.13, beta5 = 0.11, beta6 = -0.009)
turnout.inits <- list(inits1, inits2)

## Fit the model in JAGS

turnout.fit <- jags(data=turnout.data, inits=NULL, turnout.params, n.chains=2, n.iter=500, n.burnin=100, model.file=turnout.model.jags)


turnout.fit<-update(turnout.fit, n.iter=1000)
turnout.fit <-autojags(turnout.fit)
print(turnout.fit)

par(mfrow=c(2,2))
plot(turnout.fit)
traceplot(turnout.fit)


##################################################################################
## Plot predicted probabilities over simulated range of data after Logit models ##
############# Using the turnout model from Simon Jackman's examples ##############
##################### Workflow for WinBUGS/R2Jags/R2WinBUGS ######################
##################################################################################

## Johannes Karreth
## jkarreth@albany.edu

## Fit your Bayesian model, monitor the coefficients (in this example, named b[]) 
## and the cut points (in this example, named theta[])
## and if you like, the predicted probability of y=1 for each case (in this example, named p[])

library(lattice)
library(ggplot2)

#####################################################
## FIRST, PREDICTED PROBABILITIES ON OBSERVED DATA ##
#####################################################

## Data
turnout.dat <- read.csv("http://www.jkarreth.net/files/turnout.csv")

## First, fit a logit model using
## https://github.com/jkarreth/Bayes/blob/master/turnout.instructions.R
## But be sure to monitor individual predicted probabilities (if you want to plot them)
## The BUGS/JAGS object is named "turnout.fit" from here on

## R2JAGS USERS, extract the posterior distributions from your jags/bugs object:
turnout.mcmc <- as.mcmc(turnout.fit)
turnout.mat <- as.matrix(turnout.mcmc)
turnout.out <- as.data.frame(turnout.mat)

## R2WINBUGS/R2OPENBUGS USERS, extract the posterior distributions from your jags/bugs object:
## (in this case, we have 2 chains only)
turnout.mat <- rbind(turnout.fit$sims.array[, 1, ], turnout.fit$sims.array[, 2, ])
turnout.out <- data.frame(turnout.mat)

## Define vectors with the values of the predicted probabilities (pulled from the coda files or your mcmc list)
## This assumes that you used p in logit(p[i]) in your model
## grep("p[",) pulls all columns that start with p[
p <- turnout.out[, grep("p[", colnames(turnout.out), fixed = T)]

## If you want to plot mean predicted observed probabilities against a covariate,
## collapse p to the mean of all iterations
p.mean <- apply(p, 2, mean)

## Plot predicted probability (of y=1) against *observed* values of age \
## (most likely an ugly plot, b/c we have several y_i(x_i))
plot(p.mean ~ turnout.dat.dat$age, xlim = c(min(turnout.dat.dat$age), max(turnout.dat.dat$age)))

#################################################################
######### SECOND, OUT-OF-SAMPLE PREDICTED PROBABILITIES, ########
## across the range of X1 and given values of other covariates ##
#################################################################

## Import the chains containing the coefficients from your BUGS model, 
## after monitoring *only* the coefficients (in this example, named b)

## R2JAGS/R2WINBUGS USERS:
turnout.mcmc <- as.mcmc(turnout.fit)
turnout.mat <- as.matrix(turnout.mcmc)
b <- turnout.mat[ , 1:6] ## one column for each coefficient, in this case I had 6 coefficients

# Generate vector with the simulated range of X1 (here, age)
new.age <- seq(min(turnout.dat$age), max(turnout.dat$age))

# Generate vectors set at desired values of the other covariates
new.education <- rep(median(turnout.dat$educ), length(new.age))
new.closing <- rep(median(turnout.dat$closing), length(new.age))
new.govelec <- rep(median(turnout.dat$govelec), length(new.age))
new.south <- rep(median(turnout.dat$south), length(new.age))
# Need value of 1 for the constant
constant <- rep(1, length(new.age))

# Generate dataframe with simulated values
turnout.sim <- cbind(constant,new.education,new.age, new.south, new.govelec, new.closing)   ## cbind: combine (bind) columns

# Or: generate two dataframes to plot PPs for each value of the South dummy (continued further below)
turnout.sim.s <- cbind(constant,new.education,new.age,rep(1,max(turnout.dat$age)-min(turnout.dat$age)+1), new.govelec, new.closing)
turnout.sim.n <- cbind(constant,new.education,new.age, rep(0,max(turnout.dat$age)-min(turnout.dat$age)+1), new.govelec, new.closing)

# Multiply X by the betas from your BUGS output
Xb <- t(turnout.sim%*% t(b))

# Transform linear prediction to probability
turnout.pp.age <- exp(Xb)/(1+exp(Xb))

# Get CIs (for plotting)
turnout.ci.age <- apply(turnout.pp.age, 2, quantile, probs=c(.025,.975)) ## apply(a, b, c): apply function (c) to object(a), by(b: 1 for row, 2 for column)

# Get mean predictions over the n (from BUGS/JAGS iterations) sampled values of b
mean.turnout.pp.age <- apply(turnout.pp.age, 2, mean)
mean.turnout.ci.age <- apply(turnout.ci.age, 2, quantile, probs=c(.025,.975))

# Plot mean probability against the full (simulated) range of X (=age)
plot(new.age, mean.turnout.pp.age, pch=19, main="Predicted probability of voting", xlab="Age", ylab="Pr(Voting)", xlim=c(min(turnout.dat$age), max(turnout.dat$age)), ylim=c(0,1))

# Add standard errors as vertical lines (could also do this using 2.5% and 97.5% values from p.chains)
segments(new.age, mean.turnout.ci.age[1, ], new.age, mean.turnout.ci.age[2, ], lty=1)

## Continue two predictions for south=[0,1]

# Multiply X by the betas from your BUGS output
Xb.s <- t(turnout.sim.s %*% t(b))
Xb.n <- t(turnout.sim.n %*% t(b))

# Transform linear prediction to probability
turnout.pp.age.s <- exp(Xb.s)/(1+exp(Xb.s))
turnout.pp.age.n <- exp(Xb.n)/(1+exp(Xb.n))

# Get mean linear predictions & SDS over the n (from BUGS iterations) sampled values of b
mean.turnout.pp.age.s <- apply(turnout.pp.age.s, 2, mean)
mean.turnout.pp.age.n <- apply(turnout.pp.age.n, 2, mean)
turnout.s.ci <- apply(turnout.pp.age.s, 2, quantile, probs=c(.025,.975))
turnout.n.ci <- apply(turnout.pp.age.n, 2, quantile, probs=c(.025,.975))

#####################
## Plot 1 (simple) ##
#####################

# Plot mean probability against the full (simulated) range of X (=age)
adjust <- rep(.3, max(turnout.dat$age)-min(turnout.dat$age)+1)  ## slightly adjust the position of the "North" points to avoid overlay
plot(new.age, mean.turnout.pp.age.s, pch=19, main="Predicted probability of voting", xlab="Age", ylab="Pr(Voting)", col="red", xlim=c(min(turnout.dat$age), max(turnout.dat$age)), ylim=c(0,1))
points(new.age + adjust, mean.turnout.pp.age.n, pch=19, col="black")
segments(new.age, turnout.s.ci[1, ], new.age, turnout.s.ci[2, ], lty=1, col="red")
segments(new.age + adjust, turnout.n.ci[1, ], new.age + adjust, turnout.n.ci[2, ], lty=1, col="black")
legend("bottomright", c("South", "Rest of the U.S."), col=c("red", "black"), pch=19, inset=.01, bty="n")

#####################
## Plot 2 (panels) ##
#####################

## Generate data set used for the two plots below
plot.dat <- data.frame(
  means = c(mean.turnout.pp.age.s , mean.turnout.pp.age.n), 	## means of the pred. probabilities
  lower = c(turnout.s.ci[1, ] , turnout.n.ci[1, ]), 	## upper CI
  upper = c(turnout.s.ci[2, ], turnout.n.ci[2, ]),		## lower CI
  south = factor(rep(c(1,0), each=max(turnout.dat$age)-min(turnout.dat$age)+1), levels=c(1,0), labels=c("South", "Rest of the U.S.")),  ## Outcome variable
  age = rep(new.age, 2))	## Explanatory variable of interest (here: age)

xyplot(means ~ age | south, data=plot.dat, as.table=T, 
       ylim=c(min(plot.dat$lower), max(plot.dat$upper)), xlab="Age", ylab="Pr(Voting)", main="Probability of Voting",
       panel = function(x,y,subscripts){
         panel.lines(x,y,lty=1, col="black")
         panel.lines(x, plot.dat$lower[subscripts], lty=2, col="red")
         panel.lines(x, plot.dat$upper[subscripts], lty=2, col="red")})

#################################
## Plot 3 (transparent colors) ##
#################################

xyplot(mean.turnout.pp.age.s ~ new.age, ylim=c(0,1), xlab="Age", ylab="Pr(Voting)", main="Probability of voting",
       key=list(space=list("right"), rectangles=list(col=c(rgb(1,0,0, alpha=.35), rgb(0,0,1, alpha=.35))), text=list(c("South", "Rest of the U.S."))),
       panel=function(x,y){
         panel.polygon(x=c(x,rev(x),x[1]), y=c(turnout.s.ci[1,], rev(turnout.s.ci[2,]), turnout.s.ci[1,1]), 
                       col=rgb(1,0,0,alpha=.35), border=NA)
         panel.polygon(x=c(x,rev(x),x[1]), y=c(turnout.n.ci[1,], rev(turnout.n.ci[2,]), turnout.n.ci[1,1]), 
                       col=rgb(0,0,1,alpha=.35), border=NA)
         panel.lines(x, mean.turnout.pp.age.s, col="red")
         panel.lines(x, mean.turnout.pp.age.n, col="blue")
       })

######################
## Plot 4 (ggplot2) ##
######################

## Dataframe for plotting
plot.dat <- data.frame(south = as.factor(c(rep(1, length(new.age)), rep(0, length(new.age)))), new.age = c(new.age, new.age), mean.turnout.pp = c(mean.turnout.pp.age.s, mean.turnout.pp.age.n), turnout.lower = c(turnout.s.ci[1, ], turnout.n.ci[1, ]), turnout.upper = c(turnout.s.ci[2, ], turnout.n.ci[2, ]))

## Make lines and ribbons separately
p <- ggplot(dat = plot.dat, aes(x = new.age, y = mean.turnout.pp, group = south)) + geom_line(aes(colour = south))
p <- p + geom_ribbon(aes(ymin = turnout.lower, ymax = turnout.upper, fill = south), alpha = 0.2)
p <- p + xlab("Age") + ylab("Pr(Voting)") + theme_bw() + scale_colour_manual(values=c("blue", "red")) + scale_fill_manual(values=c("blue", "red"))
p <- p + theme(legend.position = "none") + annotate("text", x = 30, y = 0.8, label = "Rest of US", colour = "blue") + annotate("text", x = 60, y = 0.5, label = "South", colour = "red")

## Simpler: use geom_smooth()
p2 <- ggplot(dat = plot.dat, aes(x = new.age, y = mean.turnout.pp, group = south)) + geom_smooth(aes(x = new.age, ymin = turnout.lower, ymax = turnout.upper, fill = south, colour = south), stat = "identity")
p2 <- p2 + xlab("Age") + ylab("Pr(Voting)") + theme_bw() + scale_colour_manual(values=c("blue", "red")) + scale_fill_manual(values=c("blue", "red"))
p2 <- p2 + theme(legend.position = "none") + annotate("text", x = 30, y = 0.8, label = "Rest of US", colour = "blue") + annotate("text", x = 60, y = 0.5, label = "South", colour = "red")
