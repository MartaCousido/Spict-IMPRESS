##----------------------------------------------------------------------
#    Data Analysis
#
# Dorleta Garcia
# 2020/05/05
#-----------------------------------------------------------------------

library(gtools)
library(r4ss)
library(FLCore)
library(ggplot2)
library(tidyverse)
library(ggridges)
library(GGally)

rm(list= ls())

dtyr <- 2019

#-------------------------------------------------------------------------------------------------------
# LOAD DATA
#-------------------------------------------------------------------------------------------------------
ss3dat <- SS_readdat_3.24("c:/use/OneDrive - AZTI/wgbie2020/2020_hke.27.3a46-8abd_assessment/bootstrap/data/nhake-wg20.dat")
load("C:/use/OneDrive - AZTI/wgbie2020/2020_hke.27.3a46-8abd_assessment/model/final/post/All_Results.RDAta")

# Length weight parameters
a <- 5.13e-006
b <- 3.074
# weight at length
wal <- a*lengths^b

#-------------------------------------------------------------------------
# Estimated catches annual and seasonal
#-------------------------------------------------------------------------

# annualfitlandings
fitdisc_seas <- rowSums(fitdiscards)
fitland_seas <- rowSums(fitlandings)
fitdisc_annual <- colSums(matrix(rowSums(fitdiscards),4))
fitland_annual <- colSums(matrix(rowSums(fitlandings),4))

fitcatch_annual <- fitland_annual + fitdisc_annual
fitcatch_seas   <- fitland_seas   + fitdisc_seas

# order the seasonal catches by season first and then by year.
fitcatch_seas <- c(matrix(fitcatch_seas,4)[1,], matrix(fitcatch_seas,4)[2,], matrix(fitcatch_seas,4)[3,], matrix(fitcatch_seas,4)[4,])

write.csv(fitcatch_annual, 'C:/use/Proyectos/MEVA/HKE/Spict-IMPRESS/nhke/data/fitcatch_annual.csv')
write.csv(fitcatch_seas, 'C:/use/Proyectos/MEVA/HKE/Spict-IMPRESS/nhke/data/fitcatch_seasonal.csv')


#-------------------------------------------------------------------------
# CHANGE IN CATCH, LANDINGS AND DISCARDS BY GEAR
#-------------------------------------------------------------------------
# By SS3 fleet and only landings.
land.seas <- bind_cols(as.tbl(cbind(ss3dat$catch[,8:9], apply(ss3dat$catch[,1:7],1, sum))), cat = 'landings')
disc.seas <- bind_cols(as.tbl(aggregate(Discard ~ Yr + Seas, ss3dat$discard_data, sum)), cat = 'discards')

names(land.seas) <- names(disc.seas) <- c('year', 'season', 'tons', 'cat')

catch.seas <- bind_rows(land.seas, disc.seas) %>%  group_by(year, season) %>% summarize_at('tons', sum)

catch.seas <- catch.seas[-1,]

write.csv(catch.seas, file = 'C:/use/Proyectos/MEVA/HKE/Spict-IMPRESS/nhke/data/catch.csv')
write.csv(land.seas, file = 'C:/use/Proyectos/MEVA/HKE/Spict-IMPRESS/nhke/data/land.csv')


land.yr <- aggregate(tons~year, land.seas, 'sum')
catch.yr <- aggregate(tons~year, catch.seas, 'sum')

#-------------------------------------------------------------------------
# Abundance indices in biomass
#-------------------------------------------------------------------------
lfd.surv <- subset(ss3dat$lencomp, FltSvy %in% 8:14)[,-c(2,4:6)]
lfd.surv[,-(1:2)] <- sweep(lfd.surv[,-(1:2)], 1, rowSums(lfd.surv[,-(1:2)]), "/")

for(id in 8:12) lfd.surv[lfd.surv$FltSvy == id,-(1:2)] <- sweep(subset(lfd.surv, lfd.surv$FltSvy == id)[,-(1:2)], 1, subset(ss3dat$CPUE, index == id)$obs, "*")

wfd.surv <- lfd.surv
wfd.surv[,-(1:2)] <- sweep(lfd.surv[,-(1:2)],2,wal, "*") 

bioInd <- cbind(wfd.surv[,(1:2)], value = rowSums(wfd.surv[,-(1:2)]))

ggplot(bioInd, aes(Yr,value, col = factor(FltSvy))) + facet_grid(FltSvy~., scales = 'free') + geom_line()

save(bioInd, catch.seas, land.seas, fitcatch_annual, fitcatch_seas, catch.yr, land.yr, file = 'C:/use/Proyectos/MEVA/HKE/Spict-IMPRESS/nhke/data/data_nhke.RData')

#----------------------------------------------------------------------------------------------------
# LFD in SURVEYS
#----------------------------------------------------------------------------------------------------
lfd.surv <- subset(ss3dat$lencomp,Yr %in% ac(2010:2019) & FltSvy %in% c(8,13,14))[,-c(2,4:6)]
lfd.surv[,-(1:2)] <- as.tbl(data.frame(sweep(as.matrix(lfd.surv[,-(1:2)] ),1,apply(as.matrix(lfd.surv[,-(1:2)] ),1,sum), "/")))
lfd.surv <- lfd.surv %>% pivot_longer(-(1:2), "length") %>% 
  mutate(length = as.numeric(substr(length, 2, nchar(length))), Yr = factor(Yr))
names(lfd.surv)[1:2] <- c('year', 'survey')

p_lfd_ev <- ggplot(subset(lfd.surv, survey == 8), aes(x=length, height=value, y=year, group= year))+
  geom_density_ridges2(stat="identity", scale=1.2, fill="red", colour="red", alpha=0.3) + ggtitle('EVHOE')
p_lfd_po <- ggplot(subset(lfd.surv, survey == 13), aes(x=length, height=value, y=year, group= year))+
  geom_density_ridges2(stat="identity", scale=1.2, fill="red", colour="red",alpha=0.3) + ggtitle('SP-PORC')
p_lfd_ig <- ggplot(subset(lfd.surv, survey == 14), aes(x=length, height=value, y=year, group= year))+
  geom_density_ridges2(stat="identity", scale=1.2, fill="red",colour="red", alpha=0.3) + ggtitle('IR-IGFS')

# RESGASQ survert
lfd.resg <- subset(ss3dat$lencomp,Yr %in% 1985:1997 & FltSvy %in% 9:12)[,-c(2,4:6)]
lfd.resg[,-(1:2)] <- as.tbl(data.frame(sweep(as.matrix(lfd.resg[,-(1:2)] ),1,apply(as.matrix(lfd.resg[,-(1:2)] ),1,sum), "/")))
lfd.resg <- lfd.resg %>% pivot_longer(-(1:2), "length") %>% 
  mutate(length = as.numeric(substr(length, 2, nchar(length))), Yr = factor(Yr))
names(lfd.resg)[1:2] <- c('year', 'survey')

p_lfd_ev <- ggplot(subset(lfd.surv, survey == 8), aes(x=length, height=value, y=year, group= year))+
  geom_density_ridges2(stat="identity", scale=1.2, fill="red", colour="red", alpha=0.3) + ggtitle('EVHOE')
p_lfd_po <- ggplot(subset(lfd.surv, survey == 13), aes(x=length, height=value, y=year, group= year))+
  geom_density_ridges2(stat="identity", scale=1.2, fill="red", colour="red",alpha=0.3) + ggtitle('SP-PORC')
p_lfd_ig <- ggplot(subset(lfd.surv, survey == 14), aes(x=length, height=value, y=year, group= year))+
  geom_density_ridges2(stat="identity", scale=1.2, fill="red",colour="red", alpha=0.3) + ggtitle('IR-IGFS')

p_lfd_re <- ggplot(lfd.resg, aes(x=length, height=value, y=year, group= paste(survey, year)))+
  geom_density_ridges2(stat="identity", scale=1.2, fill="red",colour="red", alpha=0.3) + ggtitle('RESG-1')




#----------------------------------------------------------------------------------------------------
# LFD in FLEETS
#----------------------------------------------------------------------------------------------------
lfd.flt <- subset(ss3dat$lencomp,Yr %in% ac(2010:2019) & FltSvy %in% 1:7)[,-c(4,6)]
lfd.flt[,-(1:4)] <- as.tbl(data.frame(sweep(as.matrix(lfd.flt[,-(1:4)] ),1,apply(as.matrix(lfd.flt[,-(1:4)] ),1,sum), "/")))
lfd.flt <- lfd.flt %>% pivot_longer(-(1:4), "length") %>% 
  mutate(length = as.numeric(substr(length, 2, nchar(length))), Yr = factor(Yr), Seas = factor(Seas),
         Part = ifelse(Part == 1, 'Discards', 'Landings'))
names(lfd.flt)[1:4] <- c('year', 'season', 'fleet', 'category')

lfd.flt <- lfd.flt %>% mutate(seascat = paste("S",season, "_", substr(category, 1,4),sep= "" ),
                              yearcat = paste(year, "_", substr(category, 1,4),sep= "" ))

# LFD WITHIN YEAR
p_lfd_sp7 <-ggplot(subset(lfd.flt, fleet == 1 & year == 2019), 
                   aes(x=length, height=value, y=factor(seascat), group=factor(seascat)), fill = factor(category))+
  geom_density_ridges2(stat="identity", scale=1.2, aes(fill = category, colour = category), alpha=0.3, size = 0.2) +
  ylab("seasoncat") + ggtitle('SPTRAWL7 - 2019')
p_lfd_tro <-ggplot(subset(lfd.flt, fleet == 2 & year == 2019), 
                   aes(x=length, height=value, y=factor(seascat), group=factor(seascat)), fill = factor(category))+
  geom_density_ridges2(stat="identity", scale=1.2, aes(fill = category, colour = category), alpha=0.3, size = 0.2) +
  ylab("seasoncat") + ggtitle('TRAWLOTH - 2019')
p_lfd_fr8 <-ggplot(subset(lfd.flt, fleet == 3 & year == 2019), 
                   aes(x=length, height=value, y=factor(seascat), group=factor(seascat)), fill = factor(category))+
  geom_density_ridges2(stat="identity", scale=1.2, aes(fill = category, colour = category), alpha=0.3, size = 0.2) +
  ylab("seasoncat") + ggtitle('FRNEP8 - 2019')
p_lfd_sp8 <- ggplot(subset(lfd.flt, fleet == 4 & year == 2019), 
                    aes(x=length, height=value, y=factor(seascat), group=factor(seascat)), fill = factor(category))+
  geom_density_ridges2(stat="identity", scale=1.2, aes(fill = category, colour = category), alpha=0.3, size = 0.2) +
  ylab("seasoncat") + ggtitle('SPTRAWL8 - 2019')
p_lfd_gln <- ggplot(subset(lfd.flt, fleet == 5 & year == 2019), 
                    aes(x=length, height=value, y=factor(seascat), group=factor(seascat)), fill = factor(category))+
  geom_density_ridges2(stat="identity", scale=1.2, aes(fill = category, colour = category), alpha=0.3, size = 0.2) +
  ylab("seasoncat") + ggtitle('GILLNET - 2019')
p_lfd_lln <- ggplot(subset(lfd.flt, fleet == 6 & year == 2019), 
                    aes(x=length, height=value, y=factor(seascat), group=factor(seascat)), fill = factor(category))+
  geom_density_ridges2(stat="identity", scale=1.2, aes(fill = category, colour = category), alpha=0.3, size = 0.2) +
  ylab("seasoncat") + ggtitle('LONGLINE - 2019')
p_lfd_oth <- ggplot(subset(lfd.flt, fleet == 7 & year == 2019), 
                    aes(x=length, height=value, y=factor(seascat), group=factor(seascat)), fill = factor(category))+
  geom_density_ridges2(stat="identity", scale=1.2, aes(fill = category, colour = category), alpha=0.3, size = 0.2) +
  ylab("seasoncat") + ggtitle('OTHERS - 2019')


# LFD ALONG YEARS
p_lfd_yr_sp7 <- ggplot(subset(lfd.flt, fleet == 1), 
                       aes(x=length, height=value, y=year,  fill = category))+
  geom_density_ridges2(stat="identity", scale=1.2, aes(fill = category, colour = category), alpha=0.3, size = 0.2) + 
  facet_grid(~season) +   ggtitle('SPTRAWL7')
p_lfd_yr_tro <- ggplot(subset(lfd.flt, fleet == 2), 
                       aes(x=length, height=value, y=year,  fill = category))+
  geom_density_ridges2(stat="identity", scale=1.2, aes(fill = category, colour = category), alpha=0.3, size = 0.2) + 
  facet_grid(~season) +   ggtitle('TRAWLOTH')
p_lfd_yr_fr8 <- ggplot(subset(lfd.flt, fleet == 3), 
                       aes(x=length, height=value, y=year,  fill = category))+
  geom_density_ridges2(stat="identity", scale=1.2, aes(fill = category, colour = category), alpha=0.3, size = 0.2) + 
  facet_grid(~season) +   ggtitle('FRNEP8')
p_lfd_yr_sp8 <- ggplot(subset(lfd.flt, fleet == 4), 
                       aes(x=length, height=value, y=year,  fill = factor(category)))+
  geom_density_ridges2(stat="identity", scale=1.2, aes(fill = category, colour = category), alpha=0.3)+ facet_grid(~season) + 
  ggtitle('SPTRAWL8')
p_lfd_yr_gln <- ggplot(subset(lfd.flt, fleet == 5), 
                       aes(x=length, height=value, y=year,  fill = category))+
  geom_density_ridges2(stat="identity", scale=1.2, aes(fill = category, colour = category), alpha=0.3, size = 0.2) + 
  facet_grid(~season) +   ggtitle('GILLNET')
p_lfd_yr_lln <- ggplot(subset(lfd.flt, fleet == 6), 
                       aes(x=length, height=value, y=year,  fill = category))+
  geom_density_ridges2(stat="identity", scale=1.2, aes(fill = category, colour = category), alpha=0.3, size = 0.2) + 
  facet_grid(~season) +   ggtitle('LONGLINE')
p_lfd_yr_oth <- ggplot(subset(lfd.flt, fleet == 7), 
                       aes(x=length, height=value, y=year,  fill = category))+
  geom_density_ridges2(stat="identity", scale=1.2, aes(fill = category, colour = category), alpha=0.3, size = 0.2) + 
  facet_grid(~season) + 
  ggtitle('OTHERS')


# ALL TOGETHER FOR REPORT
fltnms <- c('SPTR7', 'TROTH', 'FRNEP8', 'SPTR8', 'GILLNET', 'LONGLINE', 'OTHER')
names(fltnms) <- 1:7

lfd.flt <- lfd.flt %>% mutate(fltnm = factor(fltnms[lfd.flt$fleet])) 

p_lfd <- ggplot(subset(lfd.flt,  year %in% 2015:2019), 
                aes(x=length, height=value, y=factor(fltnm), fill = year))+ facet_grid(.~category*season)+
  geom_density_ridges2(stat="identity", scale=1.2, aes(fill = year, colour = year), alpha=0.3, size = 0.2) +
  ylab("fleet") + ggtitle('Length frequency distribution - 2019')


survnms <- c(paste('RES', 1:4, sep = "_"), 'EVHOE', 'SP-PORC', 'IR-IGFS')
names(survnms) <- c(9:12, 8, 13, 14)

lfd.surv<- lfd.surv %>% mutate(survnms = (survnms[as.character(lfd.surv$survey)])) 

p_lfd_surv<- ggplot(subset(lfd.surv, survnms %in% c('EVHOE', 'SP-PORC', 'IR-IGFS') & year  %in% c(2015:2019)), 
                    aes(x=length, height=value, y=factor(survnms),  fill = year))+
  geom_density_ridges2(stat="identity", aes(fill = year, colour = year), scale=1.2,  alpha=0.3) + 
  ylab("survey")


#--------------------------------------------------------
## Indices Time Series
#--------------------------------------------------------
surveys.id <- ss3dat$CPUE

surveys.id$index <- ifelse(surveys.id$index %in% 9:12, "RESSGASC", ifelse(surveys.id$index == 8, "EVHOE-WIBTS-Q4",
                                                                          ifelse(surveys.id$index == 13, "SpPGFS-WIBTS-Q4", "IGFS-WIBTS-Q4")))
surveys.id$ll <- surveys.id$obs - 1.96*surveys.id$se_log*surveys.id$obs
surveys.id$ul <- surveys.id$obs + 1.96*surveys.id$se_log*surveys.id$obs
surveys.id$year <- surveys.id$year + (surveys.id$seas-1)*0.25

p_surveys <- ggplot(surveys.id, aes(x = year, y = obs, fill = index))  + facet_grid(index~., scales = 'free') +
  geom_ribbon(aes(ymin = ll, ymax = ul, colour = index), alpha = 0.3) +
  geom_line(aes(colour = index)) + geom_point(aes(colour = index)) +
  theme(legend.position="none")

#---------------------------------------------------------------------------------------------------
#   Make a bundle with all the plots generated.
#----------------------------------------------------------------------------------------------------

# List of produced plots
ls(pattern = 'p_')
pdf('zz_plots/NHKE_EDA_Commercial_Catch.pdf', width = 10)
p_tac + ylab('tonnes')
p_ct_area + ggtitle('Landings & Discards by area')
p_ct_area_zoom
#p_ct_gear_bars  + ggtitle('Catch by gear')
#p_ct_gear_variation  + ggtitle('Variation in catch by gear')
p_ct_ss3fl_lines + ggtitle('Catch by SS3 fleet') + ylab('tonnes')
dev.off()

pdf('zz_plots/NHKE_EDA_Surveys.pdf')
p_surveys
p_lfd_ev
p_lfd_po
p_lfd_ig
dev.off()

pdf('zz_plots/NHKE_EDA_Commercial_LFD.pdf')
p_lfd_sp7
p_lfd_yr_sp7
p_lfd_tro
p_lfd_yr_tro
p_lfd_fr8
p_lfd_yr_fr8
p_lfd_sp8
p_lfd_yr_sp8
p_lfd_gln
p_lfd_yr_gln
p_lfd_lln
p_lfd_yr_lln
p_lfd_oth
p_lfd_yr_oth
dev.off()


png('zz_plots/Figure_9.5_FleetsLFD.png', width = 700, height = 600)
p_lfd
dev.off()

png('zz_plots/Figure_9.6_SurveysLFD.png', width = 700, height = 600)
p_lfd_surv
dev.off()

# [1] "p_ct_area"           "p_ct_area_zoom"      "p_ct_cnty"           "p_ct_gear_bars"      "p_ct_gear_lines"    
# [6] "p_ct_gear_variation" "p_ct_ss3fl_lines"    "p_ld_ds_gear_bars"   "p_ld_ds_gear_lines"  "p_lfd_ev"           
# [11] "p_lfd_fr8"           "p_lfd_gln"           "p_lfd_ig"            "p_lfd_lln"           "p_lfd_oth"          
# [16] "p_lfd_po"            "p_lfd_sp7"           "p_lfd_sp8"           "p_lfd_tro"           "p_lfd_yr_gln"       
# [21] "p_lfd_yr_lln"        "p_lfd_yr_oth"        "p_lfd_yr_sp7"        "p_lfd_yr_sp8"        "p_lfd_yr_tro"       
# [26] "p_surveys"     


