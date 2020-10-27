
```{r, eval=FALSE}
library(devtools)
install_github("DTUAqua/spict/spict") 


```

Load all necessary packages.
```{r, pkgs, results = "hide",echo=TRUE,message=FALSE}
library(spict)
```


# Input data

```{r echo=TRUE, eval=TRUE}

data.ang <- read.csv("data_Spict.csv")
summary(data.ang)

data.ang$Year <- as.numeric(data.ang$Year)
data.ang$timeEv <- ifelse(data.ang$Year>=2003 & data.ang$Year!= 2017,data.ang$Year+(0.75+1)/2,NA)
data.ang$timePP <- ifelse(data.ang$Year>=2001,data.ang$Year+(0.75+1)/2,NA)
data.ang$timeMon <- ifelse(data.ang$Year %in% c(2005,2006,2015:2019),data.ang$Year+1/12*11.5,NA)
data.ang$timeMon2 <- ifelse(data.ang$Year >= 2015, data.ang$Year+1/12*11.5,NA)

attach(data.ang)


```

```{r echo=TRUE, eval=TRUE, results='hide', message=FALSE, warning=FALSE}  

dtc=1
dteuler=1/16
inp.ang <- list(obsC=Total_Catch,timeC=Year,
                obsI=list(LPUE_Vigo, SPPGFS,MON2),
                timeI=list(Year,timePP,timeMon2),
                dtc=dtc,dteuler=dteuler)
inp.ang$phases$logbeta <- 1
inp.ang$phases$logalpha <- 1
inp <- check.inp(inp.ang)
#' Plot the input data
plotspict.data(inp, qlegend = TRUE)

res <- fit.spict(inp)
summary(res)
res<- calc.osa.resid(res)

plot(res)
#plotspict.osar(rep)


# Plot the residual diagnostics
plotspict.diagnostic(res)


# Check the sensitivity to initial parameter values
sens.ini <- check.ini(res, ntrials = 5)

# Make a retrospective analysis and plot the results
res$inp$getReportCovariance = FALSE
res <- retro(res, nretroyear = 4)
plotspict.retro(res)


# Plot the relative biomass and fishing mortality
par(mfrow = c(2, 1), mar = c(4, 4, 1, 1))
plotspict.bbmsy(res, qlegend = FALSE, stamp = "")
plotspict.ffmsy(res, qlegend = FALSE, stamp = "")

# Check if there are changes in the results if the grid is finer, i.e.
# dteuler is smaller than 1/16 year.
inp.dteuler1_32 <- inp.ang
inp.dteuler1_32$dteuler <- 1/32
inp.dteuler1_32 <- check.inp(inp.dteuler1_32)

fit.dteuler1_32 <- fit.spict(inp.dteuler1_32)

## Fit the discrete time model
inp.dteuler1 <- inp.ang
inp.dteuler1$dteuler <- 1
inp.dteuler1 <- check.inp(inp.dteuler1)

fit.dteuler1 <- fit.spict(inp.dteuler1)

# Checked the fixed effects estimates
(par <- exp(res$par.fixed))
(par1 <- exp(fit.dteuler1$par.fixed))
(par1_32 <- exp(fit.dteuler1_32$par.fixed))

# Plot the percent change compared to the default run
dif1_32 <- (par - par1_32) / par * 100
dif1 <- (par - par1) / par * 100
plot(dif1, ylab = "Percent difference", xlab = "", axes = FALSE, ylim = c(-40,40))
points(dif1_32, pch = 3)
abline(h=0, col = "#00000044")
axis(1, at=seq(par), labels = gsub("log", "", names(par)))
axis(2, at = seq(-30, 30, 20))
legend("topleft", legend = c("Discrete", "Finer grid"), pch = c(1,3))

# Plot the results using the two dteuler together
par(mfrow = c(2,3))
plotspict.bbmsy(res, qlegend = FALSE, main = "")
title("Default: dteuler = 1/16 year")
plotspict.bbmsy(fit.dteuler1_32, qlegend = FALSE, main = "")
title("Finer grid: dteuler = 1/32 year")
plotspict.bbmsy(fit.dteuler1, qlegend = FALSE, main = "")
title("Discrete time: dteuler = 1 year")
plotspict.ffmsy(res)
plotspict.ffmsy(fit.dteuler1_32)
plotspict.ffmsy(fit.dteuler1)


```
