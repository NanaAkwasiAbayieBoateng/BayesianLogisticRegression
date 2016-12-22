library(R2jags)
set.seed(359)
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

bayes.mod="model{
  for (i in 1:N) {
    r[i] ~ dbin(p[i],n[i])
    b[i] ~ dnorm(0,tau)
    logit(p[i])=alpha0 + alpha1*x1[i] + alpha2*x2[i]+a;pha12*x1[i]*x2[i] + b[i] 
  }
  alpha0=dnorm(0,1.0E-6)
  alpha1=dnorm(0,1.0E-6)
  alpha2=dnorm(0,1.0E-6)
  alpha12=dnorm(0, 1.0E-6)
  tau=dgamma(0.001,0.001)
  sigma=1/sqrt(tau)
}"

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

bayes.mod.dat=list(r=c(10,23,23,26,17,5,53,55,32,46,10,8,10,8,23,0,3,22,15,32,3),
           n=c(39,62,81,51,39,6,74,72,51,79,13,16,30,28,45,4,12,41,30,51,7),
           x1=c(0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1),
           x2=c(0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,1,1,1,1,1),
           N=21)
bayes.mod.inits=function(){
  list(tau=1,alpha0=0,alpha1=0,alpha2=0,alpha12=0)
}
bayes.mod.params=c("tau","alpha0","alpha1","alpha2","alpha12")

bayes.mod.fit=jags.model(model.file=bayes.mod,data=bayes.mod.dat,inits=bayes.mod.inits,parameters.to.save=bayes.mod.params,n.chains=3,n.iter=9000,n.burnin=1000)

print(bayes.mod.fit)


# Running the model
model <- jags.model(textConnection(bayes.mod), data = bayes.mod.dat, n.chains = 3, n.adapt= 10000)
update(model, 10000); # Burnin for 10000 samples
mcmc_samples <- coda.samples(model, variable.names=c("tau","alpha0","alpha1","alpha2","alpha12")
                             , n.iter=20000)


