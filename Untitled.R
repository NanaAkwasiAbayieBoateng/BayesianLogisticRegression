
library(R2jags)
library(rjags)
library(runjags)
library(MCMCpack)
library(lattice)
library(superdiag)
library(mcmcplots)
library(ggmcmc)

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
    logit(mu[i]) <- alpha + beta1 * x1[i] + beta2 * x2[i]
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
