###############################################################################################
###############################################################################################

# Leire Ibaibarriaga: application of SPICT to Indian mackerel data
#AI_10/04/2019: modified to Loligo Vulgaris data. 

###############################################################################################
################################################################################################

#AI: updated 11 oct 2019 
#To install spict from GitHub use

library(remotes)
install_github("DTUAqua/spict/spict")            # master branch


################################################################################################


# install library

# library(devtools)
#install_github("mawp/spict/spict") 



# load library

library(spict)

###############################################################################################
###############################################################################################

# set working directory

wd <- "C:/use/GRUPOS DE TRABAJO/WGCEPH_2020/SPICT_LOLVUL_WGCEPH_2020"

setwd(wd)  
  
###############################################################################################
###############################################################################################

# load data

dat <- read.table("squid_SPICT_WGCEPH20.txt", header=T)
summary(dat)
dat

# prepare data object for spict

dd <- list(obsC=dat$Catch, timeC=dat$Year, obsI=dat$Index, timeI=dat$Year)

###############################################################################################
###############################################################################################

# the Meyer and Millar model could be fitted using the SPICT library using the function fit.meyermillar
# ?fit.meyermillar for additional help

###############################################################################################
###############################################################################################

#-------------------------
# CASE 1a: 
# alpha and beta estimated
# n estimated
#-------------------------

# check input data

inp1a <- check.inp(dd)
inp1a$dtc #dtc is equal to 1 with annual catches.

# plot data

plotspict.data(inp1a)


# fit model

out1a <-fit.spict(inp1a)
names(out1a)

# summary

summary(out1a)
capture.output(summary(out1a))

## see all quantities that can be extracted
list.quantities(out1a)

# extract quantities
get.par("logCpred",out1a)

# check sensitivity to initial values

set.seed(123)
check.ini(inp1a, ntrials=5)

# plot 

plot(out1a)

# compute residuals

res1a <- calc.osa.resid(out1a)
plotspict.diagnostic(res1a)

# correlation matrix

cov2cor(res1a$cov.fixed)
cov2cor(get.cov(res1a, "logBmsy", "logFmsy"))

# retro plots

# rep1a <- fit.spict(inp1a)
# rep1a <- retro(rep1a)
# plotspict.retro(rep1a)

#-------------------------
# CASE 2a: 
# alpha=beta=1
# n estimated
#-------------------------

# check input data

inp2a <- check.inp(dd)

# fix alpha and beta (as it cannot be done directly, very informative prior)

inp2a$priors$logalpha <- c(log(1), 1e-3)
inp2a$priors$logbeta <- c(log(1), 1e-3)

# fit model

out2a <-fit.spict(inp2a)

# summary

summary(out2a)
capture.output(summary(out2a))

# check sensitivity to initial values

set.seed(123)
check.ini(inp2a, ntrials=5)

# plot 

plot(out2a)

# compute residuals

res2a <- calc.osa.resid(out2a)
plotspict.diagnostic(res2a)

# correlation matrix

cov2cor(res2a$cov.fixed)
cov2cor(get.cov(res2a, "logBmsy", "logFmsy"))

# retro plots

# rep2a <- fit.spict(inp2a)
# rep2a <- retro(rep2a)
# plotspict.retro(rep2a)

#-------------------------
# CASE 3a: 
# alpha=4, beta=1
# n estimated
#-------------------------

# check input data

inp3a <- check.inp(dd)

# fix alpha and beta (as it cannot be done directly, very informative prior)

inp3a$priors$logalpha <- c(log(4), 1e-3)
inp3a$priors$logbeta <- c(log(1), 1e-3)

# fit model

out3a <-fit.spict(inp3a)

# summary

summary(out3a)
capture.output(summary(out3a))

# check sensitivity to initial values

set.seed(123)
check.ini(inp3a, ntrials=5)

# plot 

plot(out3a)

# compute residuals

res3a <- calc.osa.resid(out3a)
plotspict.diagnostic(res3a)

# correlation matrix

cov2cor(res3a$cov.fixed)
cov2cor(get.cov(res3a, "logBmsy", "logFmsy"))

# retro plots

# rep3a <- fit.spict(inp3a)
# rep3a <- retro(rep3a)
# plotspict.retro(rep3a)

#-------------------------
# CASE 4a: 
# alpha and beta estimated
# n=2
#-------------------------

# check input data

inp4a <- check.inp(dd)

# fix logn (very informative prior)

inp4a$priors$logn <- c(log(2), 1e-3)

# plot data

plotspict.data(inp4a)

# fit model

out4a <-fit.spict(inp4a)

# summary

summary(out4a)
capture.output(summary(out4a))

# check sensitivity to initial values

set.seed(123)
check.ini(inp4a, ntrials=5)

# plot 

plot(out4a)

# compute residuals

res4a <- calc.osa.resid(out4a)
plotspict.diagnostic(res4a)

# correlation matrix

cov2cor(res4a$cov.fixed)
cov2cor(get.cov(res4a, "logBmsy", "logFmsy"))

# retro plots

# rep4a <- fit.spict(inp4a)
# rep4 <- retro(rep4a)
# plotspict.retro(rep4a)

#-------------------------
# CASE 5a: 
# alpha=beta=1
# n=2
#-------------------------

# check input data

inp5a <- check.inp(dd)

# fix alpha and beta (as it cannot be done directly, very informative prior)
# fix logn (very informative prior)

inp5a$priors$logn <- c(log(2), 1e-3)
inp5a$priors$logalpha <- c(log(1), 1e-3)
inp5a$priors$logbeta <- c(log(1), 1e-3)

# fit model

out5a <-fit.spict(inp5a)

# summary

summary(out5a)
capture.output(summary(out5a))

# check sensitivity to initial values

set.seed(123)
check.ini(inp5a, ntrials=5)

# plot 

plot(out5a)

# compute residuals

res5a <- calc.osa.resid(out5a)
plotspict.diagnostic(res5a)

# correlation matrix

cov2cor(res5a$cov.fixed)
cov2cor(get.cov(res5a, "logBmsy", "logFmsy"))

# retro plots

# rep5a <- fit.spict(inp5a)
# rep5a <- retro(rep5a)
# plotspict.retro(rep5a)

#-------------------------
# CASE 6a: 
# alpha=4, beta=1
# n=2
#-------------------------

# check input data

inp6a <- check.inp(dd)

# fix alpha and beta (as it cannot be done directly, very informative prior)
# fix logn (very informative prior)

inp6a$priors$logn <- c(log(2), 1e-3)
inp6a$priors$logalpha <- c(log(4), 1e-3)
inp6a$priors$logbeta <- c(log(1), 1e-3)

# fit model

out6a <-fit.spict(inp6a)

# summary

summary(out6a)
capture.output(summary(out6a))

# check sensitivity to initial values

set.seed(123)
check.ini(inp6a, ntrials=5)

# plot 

plot(out6a)

# compute residuals

res6a <- calc.osa.resid(out6a)
plotspict.diagnostic(res6a)

# correlation matrix

cov2cor(res6a$cov.fixed)
cov2cor(get.cov(res6a, "logBmsy", "logFmsy"))

# retro plots

# rep6a <- fit.spict(inp6a)
# rep6a <- retro(rep6a)
# plotspict.retro(rep6a)

#-------------------------
# CASE 1b: 
# alpha and beta estimated
# n estimated
# prior on logr
#-------------------------

# check input data

inp1b <- check.inp(dd)

# plot data

plotspict.data(inp1b)

# prior on logr

inp1b$priors$logr <- c(log(1), 1)

# fit model

out1b <-fit.spict(inp1b)

# summary

summary(out1b)
capture.output(summary(out1b))

# check sensitivity to initial values

set.seed(123)
check.ini(inp1b, ntrials=5)

# plot 

plot(out1b)

# compute residuals

res1b <- calc.osa.resid(out1b)
plotspict.diagnostic(res1b)

# correlation matrix

cov2cor(res1b$cov.fixed)
cov2cor(get.cov(res1b, "logBmsy", "logFmsy"))

# retro plots

# rep1b <- fit.spict(inp1b)
# rep1b <- retro(rep1b)
# plotspict.retro(rep1b)

#-------------------------
# CASE 2b: 
# alpha=beta=1
# n estimated
# prior on logr
#-------------------------

# check input data

inp2b <- check.inp(dd)

# fix alpha and beta (as it cannot be done directly, very informative prior)
# prior on logr

inp2b$priors$logr <- c(log(1), 1)
inp2b$priors$logalpha <- c(log(1), 1e-3)
inp2b$priors$logbeta <- c(log(1), 1e-3)

# fit model

out2b <-fit.spict(inp2b)

# summary

summary(out2b)
capture.output(summary(out2b))

# check sensitivity to initial values

set.seed(123)
check.ini(inp2b, ntrials=5)

# plot 

plot(out2b)

# compute residuals

res2b <- calc.osa.resid(out2b)
plotspict.diagnostic(res2b)

# correlation matrix

cov2cor(res2b$cov.fixed)
cov2cor(get.cov(res2b, "logBmsy", "logFmsy"))

# retro plots

# rep2b <- fit.spict(inp2b)
# rep2b <- retro(rep2b)
# plotspict.retro(rep2b)

#-------------------------
# CASE 3b: 
# alpha=4, beta=1
# n estimated
# prior on logr
#-------------------------

# check input data

inp3b <- check.inp(dd)

# fix alpha and beta (as it cannot be done directly, very informative prior)
# prior on logr

inp3b$priors$logr <- c(log(1), 1)
inp3b$priors$logalpha <- c(log(4), 1e-3)
inp3b$priors$logbeta <- c(log(1), 1e-3)

# fit model

out3b <-fit.spict(inp3b)

# summary

summary(out3b)
capture.output(summary(out3b))

# check sensitivity to initial values

set.seed(123)
check.ini(inp3b, ntrials=5)

# plot 

plot(out3b)

# compute residuals

res3b <- calc.osa.resid(out3b)
plotspict.diagnostic(res3b)

# correlation matrix

cov2cor(res3b$cov.fixed)
cov2cor(get.cov(res3b, "logBmsy", "logFmsy"))

# retro plots

# rep3b <- fit.spict(inp3b)
# rep3b <- retro(rep3b)
# plotspict.retro(rep3b)

#-------------------------
# CASE 4b: 
# alpha and beta estimated
# n=2
# prior on logr
#-------------------------

# check input data

inp4b <- check.inp(dd)

# fix logn (very informative prior)
# prior on logr

inp4b$priors$logr <- c(log(1), 1)
inp4b$priors$logn <- c(log(2), 1e-3)

# plot data

plotspict.data(inp4b)

# fit model

out4b <-fit.spict(inp4b)

# summary

summary(out4b)
capture.output(summary(out4b))

# check sensitivity to initial values

set.seed(123)
check.ini(inp4b, ntrials=5)

# plot 

plot(out4b)

# compute residuals

res4b <- calc.osa.resid(out4b)
plotspict.diagnostic(res4b)

# correlation matrix

cov2cor(res4b$cov.fixed)
cov2cor(get.cov(res4b, "logBmsy", "logFmsy"))

# retro plots

# rep4b <- fit.spict(inp4b)
# rep4 <- retro(rep4b)
# plotspict.retro(rep4b)

#-------------------------
# CASE 5b: 
# alpha=beta=1
# n=2
# prior on logr
#-------------------------

# check input data

inp5b <- check.inp(dd)

# fix alpha and beta (as it cannot be done directly, very informative prior)
# prior on logr
# fix logn (very informative prior)

inp5b$priors$logr <- c(log(1), 1)
inp5b$priors$logn <- c(log(2), 1e-3)
inp5b$priors$logalpha <- c(log(1), 1e-3)
inp5b$priors$logbeta <- c(log(1), 1e-3)

# fit model

out5b <-fit.spict(inp5b)

# summary

summary(out5b)
capture.output(summary(out5b))

# check sensitivity to initial values

set.seed(123)
check.ini(inp5b, ntrials=5)

# plot 

plot(out5b)

# compute residuals

res5b <- calc.osa.resid(out5b)
plotspict.diagnostic(res5b)

# correlation matrix

cov2cor(res5b$cov.fixed)
cov2cor(get.cov(res5b, "logBmsy", "logFmsy"))

# retro plots

# rep5b <- fit.spict(inp5b)
# rep5b <- retro(rep5b)
# plotspict.retro(rep5b)

#-------------------------
# CASE 6b: 
# alpha=4, beta=1
# n=2
# prior on logr
#-------------------------

# check input data

inp6b <- check.inp(dd)

# fix alpha and beta (as it cannot be done directly, very informative prior)
# prior on logr
# fix logn (very informative prior)

inp6b$priors$logr <- c(log(1), 1)
inp6b$priors$logn <- c(log(2), 1e-3)
inp6b$priors$logalpha <- c(log(4), 1e-3)
inp6b$priors$logbeta <- c(log(1), 1e-3)

# fit model

out6b <-fit.spict(inp6b)

# summary

summary(out6b)
capture.output(summary(out6b))

# check sensitivity to initial values

set.seed(123)
check.ini(inp6b, ntrials=5)

# plot 

plot(out6b)

# compute residuals

res6b <- calc.osa.resid(out6b)
plotspict.diagnostic(res6b)

# correlation matrix

cov2cor(res6b$cov.fixed)
cov2cor(get.cov(res6b, "logBmsy", "logFmsy"))

# retro plots

# rep6b <- fit.spict(inp6b)
# rep6b <- retro(rep6b)
# plotspict.retro(rep6b)

###############################################################################################
###############################################################################################

#-------------------------
# CASE 1c: 
# alpha and beta estimated
# n estimated
# prior on logr same as in Bayesian Schaeffer
#-------------------------

# check input data

inp1c <- check.inp(dd)

# plot data

plotspict.data(inp1c)

# prior on logr

inp1c$priors$logr <- c(log(1.305), 1)

# fit model

out1c <-fit.spict(inp1c)

# summary

summary(out1c)
capture.output(summary(out1c))

# check sensitivity to initial values

set.seed(123)
check.ini(inp1c, ntrials=5)

# plot 

plot(out1c)

# compute residuals

res1c <- calc.osa.resid(out1c)
plotspict.diagnostic(res1c)

# correlation matrix

cov2cor(res1c$cov.fixed)
cov2cor(get.cov(res1c, "logBmsy", "logFmsy"))

# retro plots

# rep1c <- fit.spict(inp1c)
# rep1c <- retro(rep1c)
# plotspict.retro(rep1c)

#-------------------------
# CASE 2c: 
# alpha=beta=1
# n estimated
# prior on logr same as in Bayesian Schaeffer production model
#-------------------------

# check input data

inp2c <- check.inp(dd)

# fix alpha and beta (as it cannot be done directly, very informative prior)
# prior on logr

inp2c$priors$logr <- c(log(1.305), 1)
inp2c$priors$logalpha <- c(log(1), 1e-3)
inp2c$priors$logbeta <- c(log(1), 1e-3)

# fit model

out2c <-fit.spict(inp2c)

# summary

summary(out2c)
capture.output(summary(out2c))

# check sensitivity to initial values

set.seed(123)
check.ini(inp2c, ntrials=5)

# plot 

plot(out2c)

# compute residuals

res2c <- calc.osa.resid(out2c)
plotspict.diagnostic(res2c)

# correlation matrix

cov2cor(res2c$cov.fixed)
cov2cor(get.cov(res2c, "logBmsy", "logFmsy"))

# retro plots

# rep2c <- fit.spict(inp2c)
# rep2c <- retro(rep2c)
# plotspict.retro(rep2c)

#-------------------------
# CASE 3c: 
# alpha=4, beta=1
# n estimated
# prior on logr same prior as in Bayesian surplus production model
#-------------------------

# check input data

inp3c <- check.inp(dd)

# fix alpha and beta (as it cannot be done directly, very informative prior)
# prior on logr

inp3c$priors$logr <- c(log(1.305), 1)
inp3c$priors$logalpha <- c(log(4), 1e-3)
inp3c$priors$logbeta <- c(log(1), 1e-3)

# fit model

out3c <-fit.spict(inp3c)

# summary

summary(out3c)
capture.output(summary(out3c))

# check sensitivity to initial values

set.seed(123)
check.ini(inp3c, ntrials=5)

# plot 

plot(out3c)

# compute residuals

res3c <- calc.osa.resid(out3c)
plotspict.diagnostic(res3c)

# correlation matrix

cov2cor(res3c$cov.fixed)
cov2cor(get.cov(res3c, "logBmsy", "logFmsy"))

# retro plots

# rep3c <- fit.spict(inp3c)
# rep3c <- retro(rep3c)
# plotspict.retro(rep3c)

#-------------------------
# CASE 4c: 
# alpha and beta estimated
# n=2
# prior on logr same prior as in Bayesian surplus production model
#-------------------------

# check input data

inp4c <- check.inp(dd)

# fix logn (very informative prior)
# prior on logr

inp4c$priors$logr <- c(log(1.305), 1)
inp4c$priors$logn <- c(log(2), 1e-3)

# plot data

plotspict.data(inp4c)

# fit model

out4c <-fit.spict(inp4c)

# summary

summary(out4c)
capture.output(summary(out4c))

# check sensitivity to initial values

set.seed(123)
check.ini(inp4c, ntrials=5)

# plot 

plot(out4c)

# compute residuals

res4c <- calc.osa.resid(out4c)
plotspict.diagnostic(res4c)

# correlation matrix

cov2cor(res4c$cov.fixed)
cov2cor(get.cov(res4c, "logBmsy", "logFmsy"))

# retro plots

# rep4c <- fit.spict(inp4c)
# rep4 <- retro(rep4c)
# plotspict.retro(rep4c)

#-------------------------
# CASE 5c: 
# alpha=beta=1
# n=2
# prior on logr same prior as in Bayesian surplus production model
#-------------------------

# check input data

inp5c <- check.inp(dd)

# fix alpha and beta (as it cannot be done directly, very informative prior)
# prior on logr
# fix logn (very informative prior)

inp5c$priors$logr <- c(log(1.305), 1)
inp5c$priors$logn <- c(log(2), 1e-3)
inp5c$priors$logalpha <- c(log(1), 1e-3)
inp5c$priors$logbeta <- c(log(1), 1e-3)

# fit model

out5c <-fit.spict(inp5c)

# summary

summary(out5c)
capture.output(summary(out5c))

# check sensitivity to initial values

set.seed(123)
check.ini(inp5c, ntrials=5)

# plot 

plot(out5c)

# compute residuals

res5c <- calc.osa.resid(out5c)
plotspict.diagnostic(res5c)

# correlation matrix

cov2cor(res5c$cov.fixed)
cov2cor(get.cov(res5c, "logBmsy", "logFmsy"))

# retro plots

# rep5c <- fit.spict(inp5c)
# rep5c <- retro(rep5c)
# plotspict.retro(rep5c)

#-------------------------
# CASE 6c: 
# alpha=4, beta=1
# n=2
# prior on logr same prior as in Bayesian surplus production model
#-------------------------

# check input data

inp6c <- check.inp(dd)

# fix alpha and beta (as it cannot be done directly, very informative prior)
# prior on logr
# fix logn (very informative prior)

inp6c$priors$logr <- c(log(1.305), 1)
inp6c$priors$logn <- c(log(2), 1e-3)
inp6c$priors$logalpha <- c(log(4), 1e-3)
inp6c$priors$logbeta <- c(log(1), 1e-3)

# fit model

out6c <-fit.spict(inp6c)

# summary

summary(out6c)
capture.output(summary(out6c))

# check sensitivity to initial values

set.seed(123)
check.ini(inp6c, ntrials=5)

# plot 

plot(out6c)

# compute residuals

res6c <- calc.osa.resid(out6c)
plotspict.diagnostic(res6c)

# correlation matrix

cov2cor(res6c$cov.fixed)
cov2cor(get.cov(res6c, "logBmsy", "logFmsy"))

# retro plots

# rep6c <- fit.spict(inp6c)
# rep6c <- retro(rep6c)
# plotspict.retro(rep6c)

###############################################################################################
###############################################################################################

# compute AICs

get.AIC(res1a)
get.AIC(res2a)
get.AIC(res3a)
get.AIC(res4a)
get.AIC(res5a)
get.AIC(res6a)

get.AIC(res1b)
get.AIC(res2b)
get.AIC(res3b)
get.AIC(res4b)
get.AIC(res5b)
get.AIC(res6b)

et.AIC(res1c)
get.AIC(res2c)
get.AIC(res3c)
get.AIC(res4c)
get.AIC(res5c)
get.AIC(res6c)

# individual plots

par(mfrow=c(2,2))
plotspict.biomass(res2a, ylim=c(0,100000))
plotspict.biomass(res3a, ylim=c(0,100000))
plotspict.biomass(res5a, ylim=c(0,100000))
plotspict.biomass(res6a, ylim=c(0,100000))
plotspict.f(res2a, ylim=c(0,5))
plotspict.f(res3a, ylim=c(0,5))
plotspict.f(res5a, ylim=c(0,5))
plotspict.f(res6a, ylim=c(0,5))

# plotspict.bbmsy(res5c)
# plotspict.ffmsy(res5c)

par(mfrow=c(1,3))
plotspict.fb(res2a)
plotspict.fb(res3a)
plotspict.fb(res5a)
plotspict.fb(res6a)

rep <- res5a
idx <- seq(1, 145, by=16)
Best <- get.par("logB", rep, exp = TRUE)[idx, c("ll","est","ul","sd")]
BB <- get.par("logBBmsy", rep, exp = TRUE)[idx, c("ll","est","ul","sd")]
Fest <- get.par("logF", rep, exp = TRUE)[idx, c("ll","est","ul","sd")]
FF <- get.par("logFFmsy", rep, exp = TRUE)[idx, c("ll","est","ul","sd")]
cbind(Best, BB, Fest, FF)


###############################################################################################
###############################################################################################

# save results (spict object) to be used in the conditioning

save(out5c, file=paste("res_spict_out5c.RData",sep=""))

###############################################################################################
###############################################################################################
