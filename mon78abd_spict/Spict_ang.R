#' title: "SPiCT workshop - Exercise 1"
#' author: "Alexandros Kokkalis and Casper W. Berg"
#' date: "February 2017"
#' output: pdf_document
#' toc: true
#' 
#+ echo = FALSE
knitr::opts_chunk$set(comment=NA, fig.width=6, fig.height=8)
#'
#' Learning objectives:
#' ------------------- 
#' * Installing SPiCT
#' * Examining an example data set
#' * Doing an assessment using SPiCT
#' * Looking at results (summary, plot, diagnostics)
#'
#' Notes
#' -----
#' * Use help to see the documentation of a function (e.g. ?get.par or 
#'   help("get.par", "spict"))
#' * The spict vignette is a long-form documentation that explains the 
#'   functionality of spict using code examples. You can open it using
#'   `vignette("vignette", "spict")`
#'
#' 1.1 Install TMB and SPiCT
#' --------------------------
#' 
#' Uncomment and run the next two lines if the spict package is not installed
# install.packages("TMB")
# devtools::install_github("mawp/spict/spict", build_vignettes = TRUE)
#'
#' 1.2 Load and look at the South Atlantic albacore data
#' -----------------------------------------------------
#' 
#' Load the spict package (it automatically loads TMB)
#' # install.packages("TMB", type = "source")

library(spict)
#' There are two messages shown after the successful loading of spict, showing 
#' that TMB was also loaded and the spict version and SHA1 code from the github 
#' repository. This code is useful to identify the exact version of the package 
#' used to produce results and is automatically saved with the results and added
#' to the default plots. The version used to create this exercise is shown below:
# Loading required package: TMB
# Welcome to spict_v1.1@9eccc2daae57dd739bc24792cfdc0d862501e7bf
#' One can install that exact same version of spict using
#'
## devtools::install_github("mawp/spict/spict", 
##                          ref = "9eccc2daae57dd739bc24792cfdc0d862501e7bf")

#' In the spict package there are included three example data sets from Polacheck
#' et al (1993). To load the data use the `data` function. Use `?pol` to see more
#' information about the data sets.
data.ang <- read.csv("data_Spict.csv")
summary(data.ang)
data.ang$Year <- as.numeric(data.ang$Year)
# Area covered and season
# For the 1987 to 1996 period, the Survey EVHOE has been conducted in the Bay of Biscay on an annual basis with the exception of the years 1993 and 1996. It has been conducted in the third or fourth quarter except in 1991 where it took place in May. In 1988 two survey were conducted, one in May the other in October.
# The Celtic Sea was surveyed from 1990 to 1994 but the sampling was restricted to a small geographical area. The duration is between 40 to 45 days depending on year and availability of ship. Since 1997, the survey covered all the Celtic Sea and Bay of Biscay during the 4th quarter.
data.ang$timeEv <- ifelse(data.ang$Year>=1997,data.ang$Year+(0.75+1)/2,NA)
attach(data.ang)

inp.ang <- list(obsC=Total_Catch,timeC=Year,obsI=EVHOE_Kg_30mn,timeI=timeEv)
inp <- check.inp(inp.ang)

str(inp)

#' Plot the input data
plotspict.data(inp, qlegend = FALSE)

#' 1.3 Make a SPiCT assessment and look at the results and diagnostics
#' -------------------------------------------------------------------
#' 
#' Make the SPiCT assessment of South Atlantic albacore with the `fit.spict` 
#' function using all default parameters and priors. Then see a summary of the 
#' result and make the standard plots. Also make a plot of relative biomass and
#' relative fishing mortality. Make these plots without the color legend and the 
#' version stamp.
#' 
#' Hint: spict provides several functions for plotting input data, results and diagnostics.
#' The generic functions `plot` and `summary` can be used on a fitted object to
#' create stadard plots and standard summary. Additional plotting functions start with
#' `plotspict.` and summary functions with `sumspict.`. 

res <- fit.spict(inp)

# See the summary of the results
summary(res)

# Make the standard results plots
plot(res)

sd(EVHOE_Kg_30mn,na.rm=TRUE)
sd(Total_Catch,na.rm=TRUE)

# Plot the relative biomass and fishing mortality
par(mfrow = c(2, 1), mar = c(4, 4, 1, 1))
plotspict.bbmsy(res, qlegend = FALSE, stamp = "")
plotspict.ffmsy(res, qlegend = FALSE, stamp = "")

#' 1.4 Model validation
#' ---------------------
#' Do the necessary model validation to ensure that no assumptions are
#' violated and that the fit is reasonable. Calculate one step ahead (OSA) residuals 
#' and check them for bias, autocorrelation and normality.
#' Then check the sensitivity to initial values and do a retrospective analysis.
#' In the end, check if choosing a finer dteuler changes the results.
#' 
#' Hint: Relevant functions: for the residual diagnostics `calc.osa.resid`, 
#' `plotspict.diagnostic` and `sumspict.diagnostic`, for the sensitivity to initial
#' values `check.ini` and for the retrospective analysis `retro`. 
#' Use inp$getReportCovariance = FALSE to reduce the size of the management 
#' object.

# Calculate the one step ahead (OSA) residuals
res <- calc.osa.resid(res)

# Plot the residual diagnostics
plotspict.diagnostic(res)

# Make a summary of the residuals tests
sumspict.diagnostics(res)

# Check the sensitivity to initial parameter values
sens.ini <- check.ini(res, ntrials = 5)

# Make a retrospective analysis and plot the results
res$inp$getReportCovariance = FALSE
res <- retro(res, nretroyear = 4)
plotspict.retro(res)

# Check if there are changes in the resukts if the grid is finer, i.e.
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

#' 1.5 Extract derived quantities from the results
#' -----------------------------------------------
#' Extract the Bmsy and Fmsy reference points and the F/Fmsy and B/Bmsy series 
#' from the South Atlantic albacore assessment and plot them. You can also add
#' the version of spict to the plots with the `txt.stamp` function.
#' 
#' Hint: Use the function `list.quantities` to see all estimated quantities and 
#' `get.par` to extract. OBS: It is preferred to extract the log transformed quantities,
#' as their confidence intervals are the correct ones.
#'
# List available quantities
list.quantities(res)

# The function `get.par` is used to extract estimates and derived quantities from 
# a fitted `spictcls` object.

# Extract the stochastic (s) and deterministic reference points. logBmsy and logFmsy
# are the stochastic or deterministic reference point depending on the `inp$msytype`
# argument, that is either "s" (default) or "d". 
(Bmsy <- get.par("logBmsy", res, exp = TRUE))
(Bmsyd <- get.par("logBmsyd", res, exp = TRUE))
(Bmsys <- get.par("logBmsys", res, exp = TRUE))
(Fmsy <- get.par("logFmsy", res, exp = TRUE))
(Fmsyd <- get.par("logFmsyd", res, exp = TRUE))
(Fmsys <- get.par("logFmsys", res, exp = TRUE))

# Extract B/Bmsy and F/Fmsy time series
bbmsy <- get.par("logBBmsy", res, exp = TRUE)
time.bbmsy <- as.numeric(rownames(bbmsy))

ffmsy <- get.par("logFFmsy", res, exp = TRUE)
time.ffmsy <- as.numeric(rownames(ffmsy))

## Plot the relative estimates
par(mfrow = c(2,1), mar = c(4.1,4.1,1,1))
plot(time.bbmsy, bbmsy[, 2], type = "l", lwd = 3, ylim = range(bbmsy[, 1:3]),
     xlab = "Year", ylab = expression(B/B[msy]))
polygon(c(time.bbmsy, rev(time.bbmsy)),
        c(bbmsy[, 1], rev(bbmsy[, 3])),
        col = "#00000044", border = FALSE)

plot(time.ffmsy, ffmsy[, 2], type = "l", lwd = 3, ylim = range(ffmsy[, 1:3]),
     xlab = "Year", ylab = expression(F/F[msy]))
polygon(c(time.ffmsy, rev(time.ffmsy)),
        c(ffmsy[, 1], rev(ffmsy[, 3])),
        col = "#00000044", border = FALSE)

txt.stamp(do.flag = TRUE)

#'
#' 1.6 Save the annual B/Bmsy
#' --------------------------
#'
#' Extract the yearly values of B/Bmsy by taking the annual mean or the estimated 
#' value in the start of the year and save them in a comma separated 
#' value (csv) file.
#' 
#' Hint: check the `?annual` function.

# Extract the B/Bmsy time series
bbmsy <- get.par("logBBmsy", res, exp = TRUE)
time <- as.numeric(rownames(bbmsy))

bbmsy.mean <- do.call("cbind", annual(time, bbmsy, "mean"))
bbmsy.start <- annual(time, bbmsy, function(x) x[1])
out <- data.frame(Year = bbmsy.mean,
                  BBmsy.start = bbmsy.start,
                  BBmsy.mean = bbmsy.mean)
write.csv(out, file = "anglerfish_BBmsy.csv", row.names = FALSE)


