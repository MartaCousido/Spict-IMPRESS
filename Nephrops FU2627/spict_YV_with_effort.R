##The package is installed from gihtub using the devtools package:
#devtools::install_github("DTUAqua/spict/spict", ref="1.2.8")


np$msytype <- "d"....... "Shanna comando"


##Charge library
library(spict)
library(icesAdvice)

##Charge directory
setwd("D:/PROYECTO IMPRESS/FU2627/SPICT")

##Load data
data <- read.csv("nepFU2627_SPICT.csv" ,sep=",")

##Check data 
head(data)
summary(data)
str(data)

########################################################################################
# Create the inp object for the model. Note data are structured as:
# obsC (catch observations), 
# timeC (time of catch observations), 
# obsI (index of abundance), 
# timeI (time of obs abundance). 
#The timing of survey indices has to be given as decimal years reflecting the timing of the survey 
########################################################################################

#inp <- list(timeC=data$Year, obsC=data$Cathes_tons, obsI=data$Index_CPUE, timeI=data$Year, obsII=data$Index_Survey, timeII=data$Year)
#inp <- list(timeC=data$Year, obsC=data$Cathes_tons, obsI=data$Index_CPUE, timeI=data$Year)
#inp=check.inp(inp)
#inp$dtc # If times are not specified it's understand 1 year subsequentially. You can change as 0.25 for quarterly catches, etc

require(tidyverse)
inp <- list(timeC=data$Year, obsC=data$Catches_tons)
inp$obsI[[1]]<-(data%>%filter(Year>1987)%>%select(Index_Survey))$Index_Survey
inp$timeI[[1]]<-(data%>%filter(Year>1987)%>%select(Year))$Year+0.83
#inp$obsI[[2]]<-(data%>%filter(Year>1993)%>%select(Index_CPUE))$Index_CPUE
#inp$timeI[[2]]<-(data%>%filter(Year>1993)%>%select(Year))$Year
inp$obsE<-(data%>%filter(Year>1993)%>%select(Index_Effort))$Index_Effort
inp$timeE<-(data%>%filter(Year>1993)%>%select(Year))$Year


check.inp(inp)


#Create folders for outputs
dir.create("Model_results")
dir.create("Model_results/Plots")


########################################################################################
# Plot rwa data
########################################################################################

plotspict.data(inp)#color of individual points shows when the observation was made

#############################################################################################
# More advanced plot. Fitting (linear regression) and shows the results
#############################################################################################
plotspict.ci(inp)

#PLot explication: Dashed horizontal line representing a guess of MSY 
#from linear regression between the index and the catch divided by the index (middle left)
#Regression is expected to have a negative slop
#catch versus catch/index (middle row, right) to approximately find 
#the optimal effort (or effort proxy)
#proportional increase in the index as a function of catch (bottom row, right)
#should show primarily positive increases in index at low catches and vice versa. 
#Positive increases in index at large catches could indicate model violations
########################################################################################
# The model is fitted to data by running fix parameter 
# to Scahaeffer production curve (initial parameter)
########################################################################################

# Numerical solver time step (probably don't need to change)
inp$dteuler=1/16 #default

res <- fit.spict(inp)
names(res)
capture.output(summary(res))

#Line 1:Convergence of the model fit, which has code 0 if the fit was succesful.
#Line 2: Objective function value at the optimum. The objective function is the likelihood function if priors are not used and the posterior density function if priors are used.

#Summary of the parameter estimates and their 95% CIs. 
round(sumspict.parest(res),2)

#Reference points
sumspict.drefpoints(res)#deterministic reference points
sumspict.srefpoints(res)#stochastic reference points

#The basic plotting of the results is done using the generic function
plot(res)

#Estimates (biomass, fishing mortality, catch, production) are shown using blue lines.
#95% CIs of absolute quantities are shown using dashed blue lines.
#95% CIs of relative biomass and fishing mortality are shown using shaded blue regions.
#Estimates of reference points (BMSY , FMSY , MSY ) are shown using black lines.
#95% CIs of reference points are shown using grey shaded regions.
#The end of the data range is shown using a vertical grey line.
#Predictions beyond the data range are shown using dotted blue lines.
#Data are shown using points colored by season  (not shown here).
#Different index series use different point characters (not shown here).

###############################################################################################################################################
###############################################################################################################################################
######      DIAGNOSTIC   Checklist for the acceptance of a SPiCT assessment          ########################################
###############################################################################################################################################
###############################################################################################################################################
###############################################################################################################################################


###############################################################################################################################################
#1. The assessment converged  equals 0
##############################################################################################################################################
res$opt$convergence 

###############################################################################################################################################
#2. All variance parameters of the model parameters are finite should be TRUE
###############################################################################################################################################
all(is.finite(res$sd))                                                           

##########################################################################################
#3. No violation of model assumptions based on one-step-ahead residuals (bias, auto-correlation, normality).
##########################################################################################
res <- calc.osa.resid(res)

#tiff(filename = "Model_results/Plots/Diagnostics.tiff")
plotspict.diagnostic(res)#check correlation and normality
#dev.off()
#Slight violations of these assumptions do not necessarily invalidate model results.

############################################################################################################
#4. Consistent patterns in the retrospective analysis 
#This means that there is no tendency of consistent under- or overestimation 
#of the relative fishing mortality ( F ) and relative biomass ( B ) in successive assessment. 
#The retrospective trajectories of those two quantities should be inside 
#the confidence intervals of the base run. (fit <- fit.retro(fit))
############################################################################################################

rep=retro(res, nretroyear=3)# by the 1 to 5 last observations, change with nretroyear
plotspict.retro(rep)

##########################################################################################
#5. Realistic production curve. 
#The shape of the production curve should not be too skewed. 
# BMSY/K should be between 0.1 and 0.9 
#Low values of BMSY/K allow for an infinite population growth rate K
##########################################################################################
calc.bmsyk(res)

############################################################################################################
#6. High assessment uncertainty can indicate a lack of contrast in the input data or violation of
#the ecological model assumptions. Confidence intervals for B/BMSY and F/BSMY should not span more
#than 1 order of magnitude
##########################################################################################
calc.om(res)## correr despues del punto 7

############################################################################################################
#7. Initial values do not influence the parameter estimates 
############################################################################################################
fit <- check.ini(res)

#The estimates should be the same for all initial values 
res$ckeck.ini$resmat








############################################################################################################
############################################################################################################
# Once you have the final model, save results
############################################################################################################
############################################################################################################

#The plot of the relative biomass 
tiff(filename = "Model_results/Plots/Biomass.tiff")
plotspict.biomass(res)
dev.off()

tiff(filename = "Model_results/Plots/BBmsy.tiff")
plotspict.bbmsy(res)#This plot contains much of the same information as given by plotspict.biomass, but without the information
#about absolute biomass and without the 95% CI around the BMSY reference point.
dev.off()

#The plots of fishing mortality follow the same principles
tiff(filename = "Model_results/Plots/FishingM.tiff")
plotspict.f(res)
dev.off()

tiff(filename = "Model_results/Plots/FFmsy.tiff")
plotspict.ffmsy(res)
dev.off()

#The plot of the catch is produced using
tiff(filename = "Model_results/Plots/Catch.tiff")
plotspict.catch(res)
dev.off()

# kobe plot of fishing mortality versus biomass is plotted using
tiff(filename = "Model_results/Plots/Kobe.tiff")
plotspict.fb(res)
dev.off()

############################################################################################################
#To extract an estimated quantity, here logBmsy u
################################################################################################################################################
get.par('logBmsy', res)

#The estimated quantity can also be returned on 
#the natural scale (as opposed to log scale) by running
get.par('logBmsy', res, exp=TRUE)

list.quantities(res)

#The covariance between the model parameters (fixed effects) can be extracted from the results list
res$cov.fixed

#It is however easier to interpret the correlation 
#rather than covariance. The correlation matrix can be calculated using
cov2cor(res$cov.fixed)

cov2cor(get.cov(res, 'logBmsy', 'logFmsy'))#highly correlated=model is reparameterised.

###########################################################################################
#Set function for tables
############################################################################################################
xtab<-function(x,caption='Table X.', file=stdout(), width='"100%"', cornername='', dec=rep(1,ncol(x))){
  nc<-ncol(x)
  lin<-paste('<table width=',width,'>', sep='')
  lin<-c(lin,sub('$','</td></tr>',sub('\\. |\\.$','.</b> ',
                                      sub('^', paste('<tr><td colspan=',nc+1,'><b>',sep=''), caption))))
  hr<-paste('<tr><td colspan=',nc+1,'><hr noshade></td></tr>', sep='')
  lin<-c(lin,hr)
  cnames<-colnames(x)
  cnames<-paste(sub('$','</b></td>',sub('^','<td align=right><b>',cnames)), collapse='\t')
  lin<-c(lin,paste('<tr>',paste('<td align=left><b>',cornername,'</b></td>',sep=''),cnames,'</tr>'))
  lin<-c(lin,hr)
  rnames<-sub('$','</b></td>',sub('^','<tr> <td align=left><b>',rownames(x)))
  #x<-sapply(1:ncol(x),function(i)sub('NA','  ',format(round(x[,i],dec[i]))))
  x<-sapply(1:ncol(x),function(i)sub('NA','  ',formatC(round(x[,i],dec[i]),digits=dec[i], format='f')))
  for(i in 1:nrow(x)){
    thisline<-paste(rnames[i],paste(sub('$','</td>',sub('^','<td align=right>',x[i,])), collapse='\t'),'</tr>', sep='')
    lin<-c(lin,thisline)
  }
  lin<-c(lin,hr)
  lin<-c(lin,'</table><br>\n')
  writeLines(lin,con=file)
}

## Tables and Extracting parameter estimates
tab1 <- sumspict.parest(res)
xtab(tab1,caption="Parameter estimates",cornername="Parameter",file="Model_results/Parameter_estimates.html",dec=rep(4,ncol(tab1)))

tab2 <- sumspict.srefpoints(res);
xtab(tab2,caption="Stochastic reference points",cornername="Reference points",file="Model_results/Reference_points.html",dec=rep(4,ncol(tab2)))

tab3 <- sumspict.states(res);
xtab(tab3,caption="Estimated states",cornername="",file="Model_results/Estimated_states.html",dec=rep(4,ncol(tab3)))

tab4 <- sumspict.predictions(res);
xtab(tab4,caption="Forecast",cornername="",file="Model_results/Forecast.html",dec=rep(4,ncol(tab4)))

tab5 <- get.par("logBBmsy",res,exp=TRUE)
tab5_<- tab5[grep(".",rownames(tab5), fixed=TRUE, invert=TRUE),] 
xtab(tab5_,caption="B/Bmsy",cornername="",file="Model_results/BBmsy.html",dec=rep(4,ncol(tab5_)))
xtab(tab5,caption="B/Bmsy",cornername="",file="Model_results/BBmsy_all.html",dec=rep(4,ncol(tab5)))

tab6 <- get.par("logFFmsy",res,exp=TRUE)
tab6_<- tab6[grep(".",rownames(tab6), fixed=TRUE, invert=TRUE),] 
xtab(tab6_,caption="F/Fmsy",cornername="",file="Model_results/FFmsy.html",dec=rep(4,ncol(tab6_)))

tab6b <- get.par("logF",res,exp=TRUE)
xtab(tab6b,caption="F",cornername="",file="Model_results/F_results.html",dec=rep(4,ncol(tab6b)))

tab6c <- get.par("logCpred",res,exp=TRUE)
xtab(tab6c,caption="Catch",cornername="",file="Model_results/Catch.html",dec=rep(4,ncol(tab6c)))



