
library(spict)
data.ang <- read.csv("data_Spict.csv")
summary(data.ang)
data.ang$Year <- as.numeric(data.ang$Year)
#http://datras.ices.dk/home/descriptions.aspx#IRE
# Area covered and season
# For the 1987 to 1996 period, the Survey EVHOE has been conducted in the Bay of Biscay on an annual basis with the exception of the years 1993 and 1996. It has been conducted in the third or fourth quarter except in 1991 where it took place in May. In 1988 two survey were conducted, one in May the other in October.
# The Celtic Sea was surveyed from 1990 to 1994 but the sampling was restricted to a small geographical area. The duration is between 40 to 45 days depending on year and availability of ship. Since 1997, the survey covered all the Celtic Sea and Bay of Biscay during the 4th quarter.
data.ang$timeEv <- ifelse(data.ang$Year>=1997,data.ang$Year+(0.75+1)/2,NA)
# The Spanish survey in the Porcupine Bank began in 2001 and covers ICES Divisions VIIb-k corresponding to the Porcupine Bank and adjacent area in western Irish waters from longitude 12° W to 15° W and from latitude 51° N to 54° N. The survey takes place in the third quarter (September) and covers depths between 170 and 800 m. 
data.ang$timePP <- ifelse(data.ang$Year>=2001,data.ang$Year+1/12*9.5,NA)
#from 2003 onwards all Irish Surveys will be conducted on this new vessel, starting around mid October through to late November.
data.ang$timeIG <- ifelse(data.ang$Year>=2003,data.ang$Year+1/12*10.5,NA)

attach(data.ang)


dtc=1
dteuler=1/16
inp.ang <- list(obsC=Total_Catch,timeC=Year,
                obsI=list(EVHOE_Kg_30mn,EW_Beam),
                timeI=list(timeEv,Year),
                dtc=dtc,dteuler=dteuler)
inp.ang$phases$logbeta <- 1
inp.ang$phases$logalpha <- 1
inp <- check.inp(inp.ang)
#' Plot the input data
plotspict.data(inp, qlegend = FALSE)

res <- fit.spict(inp)
summary(res)
res<- calc.osa.resid(res)
plot(res)
#' Everything seems fine
# Plot the residual diagnostics
plotspict.diagnostic(res)
#' Everything seems fine

# Check the sensitivity to initial parameter values
sens.ini <- check.ini(res, ntrials = 5)

# Make a retrospective analysis and plot the results
res$inp$getReportCovariance = FALSE
res <- retro(res, nretroyear = 4)
plotspict.retro(res)
