
# Prepare work space ####

rm(list = ls(all = TRUE))

# Load packages
library(tidyverse) # for nice coding and plotting
library(rerddapXtracto) # for extracting ERDDAP SST data at particular dates and locations

# To make sure that simulation is the same each time:
set.seed(123)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create starting data ####

# Load BMR time multipliers for a wing-propelled, diving, water-resting bird:
FTM <- 6.6 # foraging
LTM <- 1.7 # on land
OWRTM <- 3.9 # on water (rest)
STM <- 9.9 # swimming

# Include 10 burn-in days
simlationdays <- 375

# One individual
nbirds <- 1

# Create data frame of the 1 individual's body mass
bodymass <- as.data.frame(c(1:nbirds))
colnames(bodymass) <- c("individual")
bodymass$startmass <- rnorm(n = nbirds, mean = 5000, sd = 200)

# Create new 'empty' data frames to store outputs
# DEE:
dailyEEA <- bodymass
dailyEEB <- bodymass
dailyEEC <- bodymass
# Foraging time:
foragingtime<-bodymass
# Time energy budgets:
teb.foragingA <- bodymass
teb.onwaterrestingA <- bodymass
teb.swimA <- bodymass
teb.landA <- bodymass
teb.foragingB <- bodymass
teb.onwaterrestingB <- bodymass
teb.swimB <- bodymass
teb.landB <- bodymass
teb.foragingC <- bodymass
teb.onwaterrestingC <- bodymass
teb.swimC <- bodymass
teb.landC <- bodymass
# Meta data:
metadata <- bodymass
energyincome <- bodymass
masschange <- bodymass

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load temp data ####
# Extract estimates of sst throughout the annual cycle

sstInfo <- rerddap::info('erdMH1sstdmdayR20190SQ') # MODIS AQUAL daytime SST data (monthly mean)

SST.Eldey = rxtracto(sstInfo, parameter = 'sstMasked',
               xcoord = rep_len(-22.96, length.out = 375), # lon coord in the sea near Eldey, Iceland
               ycoord = rep_len(63.74, length.out = 375), # lat coord in the sea near Eldey, Iceland
               tcoord = as.Date(seq(as.Date("2019-12-23"), as.Date("2020-12-31"), by="days")),
               xlen = .2, ylen = .2, progress_bar = TRUE)[[1]]

SST.Temara = rxtracto(sstInfo, parameter = 'sstMasked',
                     xcoord = rep_len(-6.99, length.out = 375), # lon coord in the sea near Temara, Morocco
                     ycoord = rep_len(33.96, length.out = 375), # lat coord in the sea near Temara, Morocco
                     tcoord = as.Date(seq(as.Date("2019-12-23"), as.Date("2020-12-31"), by="days")),
                     xlen = .2, ylen = .2, progress_bar = TRUE)[[1]]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Simulation A ####

# the bird stayed close to their breeding colony throughout the year and returned to land during the night
# (similar to the gentoo penguin Pygoscelis papua), 

# Run simulation through the total number of simulation days
for (days in 1:simlationdays) {
  
  # days = 1 # to test stuff on just one day
  
  Foreffic <- rnorm(1,800,100) # foraging efficiency of a great auk (scaled from guillemot foraging efficiency reported in Dunn et al 2022)
  
  sst <- SST.Eldey[days] # extract SST on each day
  
  bm <- bodymass[1,(days+1)] # extract the current body mass
  
  # Estimate foraging time and energy intake
  foragingtime <- (rnorm(n = 1, mean = ((metadata$startmass[metadata$individual == 1] - bodymass[1,(days + 1)]) * 38), sd = 30) +
                     rnorm(n = 1, mean = dailyEEA[1,(days)], sd = abs(dailyEEA[1,(days)] * 0.05))) /
    Foreffic
  foragingtime <- ifelse(foragingtime < 0, 0, foragingtime)
  foragingtime <- ifelse(foragingtime > 20, 20, foragingtime)
  intake<-foragingtime*Foreffic
  
  # Calculate the rest of the daily time activity budget
  leftovertime <- 24 - foragingtime
  landtime <- ifelse(days > 130 & days < 190, leftovertime * 0.5, leftovertime * 0.4)
  onwaterrestingtime <- (leftovertime) * ifelse(days > 130 & days < 190, 0.3, 0.35)
  swimtime <- (leftovertime) * ifelse(days > 130 & days < 190, 0.2, 0.25)
  
  BMR <- (((((4.05 * (bm)^0.79)/1000) * 18.8)) +
            ((3.201 * (bm ^0.719)))/24)/2 # calculate BMR as the mean of the two BMR equations
  
  # Calculate the activity-associated thermoregulation costs
  lowerCT<- 47.2 * ((bm)^-0.18)
  thermalcond_inwater <- (3.47 * ((bm/1000)^-0.573))#kj c^-1 h^-1 kg^-1
  thermalcond_onwater <- (1.532 * ((bm/1000)^-0.546))#kj c^-1 h^-1 kg^-1
  thermalcond_air <- (0.705 * ((bm/1000)^-0.461))#kj c^-1 h^-1 kg^-1
  
  # Calculate the time-activity-specific heat costs
  thermocosts<-((ifelse(sst < lowerCT, lowerCT - sst, 0) * (bm/1000) * thermalcond_inwater *
                   (swimtime + foragingtime)) +
                  (ifelse(sst < lowerCT, lowerCT - sst, 0) * (bm/1000) * thermalcond_onwater *
                     (onwaterrestingtime + foragingtime)) +
                  (ifelse(sst < lowerCT, lowerCT - sst, 0) * (bm/1000) * thermalcond_air *
                     landtime)) # (for simplicity we assumed that air temp = sst)
  
  # Calculate daily energy expenditure, incorporating our simulated time activity budgets,
  # their associated BMR multipliers and the associated thermoregulatory costs
  dailyEEA[1,(days+1)]<-sum((foragingtime*BMR*FTM),(onwaterrestingtime*BMR*OWRTM),
                            (swimtime*BMR*STM),(landtime*BMR*LTM)) + thermocosts
  
  # Save the other aspects of the energy budget
  energyincome[1,(days+1)]<-intake
  masschange[1,(days+1)]<-(energyincome[1,days+1]-dailyEEA[1,(days+1)])/38
  bodymass[1,(days+2)]<-bodymass[1,(days+1)]+masschange[1,(days+1)]
  
  # Save the TEBs
  teb.foragingA[1,(days+1)] <- foragingtime
  teb.onwaterrestingA[1,(days+1)] <- onwaterrestingtime
  teb.swimA[1,(days+1)] <- swimtime
  teb.landA[1,(days+1)] <- landtime
  
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Simulation B ####

# the bird stayed close to their breeding colony throughout the year and stayed at-sea during the night when not under
# the constraints of the breeding season (similar to the common guillemot Uria aalge)

# Run simulation through the total number of simulation days
for (days in 1:simlationdays) {
  
  # days = 1 # to test stuff on just one day
  
  Foreffic <- rnorm(1,800,100) # foraging efficiency of a great auk (scaled from guillemot foraging efficiency reported in Dunn et al 2022)
  
  sst <- SST.Eldey[days] # extract SST on each day
  
  bm <- bodymass[1,(days+1)] # extract the current body mass
  
  # Estimate foraging time and energy intake
  foragingtime <- (rnorm(n = 1, mean = ((metadata$startmass[metadata$individual == 1] - bodymass[1,(days + 1)]) * 38), sd = 30) +
                     rnorm(n = 1, mean = dailyEEB[1,(days)], sd = abs(dailyEEB[1,(days)] * 0.05))) /
    Foreffic
  foragingtime <- ifelse(foragingtime < 0, 0, foragingtime)
  foragingtime <- ifelse(foragingtime > 20, 20, foragingtime)
  intake<-foragingtime*Foreffic
  
  # Calculate the rest of the daily time activity budget
  leftovertime <- 24 - foragingtime
  landtime <- ifelse(days > 130 & days < 190, leftovertime * 0.5, 0)
  onwaterrestingtime <- (leftovertime) * ifelse(days > 130 & days < 190, 0.3, 0.7)
  swimtime <- (leftovertime) * ifelse(days > 130 & days < 190, 0.2, 0.3)
  
  BMR <- (((((4.05 * (bm)^0.79)/1000) * 18.8)) +
            ((3.201 * (bm ^0.719)))/24)/2 # calculate BMR as the mean of the two BMR equations
  
  # Calculate the activity-associated thermoregulation costs
  lowerCT<- 47.2 * ((bm)^-0.18)
  thermalcond_inwater <- (3.47 * ((bm/1000)^-0.573))#kj c^-1 h^-1 kg^-1
  thermalcond_onwater <- (1.532 * ((bm/1000)^-0.546))#kj c^-1 h^-1 kg^-1
  thermalcond_air <- (0.705 * ((bm/1000)^-0.461))#kj c^-1 h^-1 kg^-1
  
  # Calculate the time-activity-specific heat costs
  thermocosts<-((ifelse(sst < lowerCT, lowerCT - sst, 0) * (bm/1000) * thermalcond_inwater *
                (swimtime + foragingtime)) +
               (ifelse(sst < lowerCT, lowerCT - sst, 0) * (bm/1000) * thermalcond_onwater *
                  (onwaterrestingtime + foragingtime)) +
               (ifelse(sst < lowerCT, lowerCT - sst, 0) * (bm/1000) * thermalcond_air *
                  landtime)) # (for simplicity we assumed that air temp = sst)
  
  # Calculate daily energy expenditure, incorporating our simulated time activity budgets,
  # their associated BMR multipliers and the associated thermoregulatory costs
  dailyEEB[1,(days+1)]<-sum((foragingtime*BMR*FTM),(onwaterrestingtime*BMR*OWRTM),
                            (swimtime*BMR*STM),(landtime*BMR*LTM)) + thermocosts
  
  # Save the other aspects of the energy budget
  energyincome[1,(days+1)]<-intake
  masschange[1,(days+1)]<-(energyincome[1,days+1]-dailyEEB[1,(days+1)])/38
  bodymass[1,(days+2)]<-bodymass[1,(days+1)]+masschange[1,(days+1)]
  
  # Save the TEBs
  teb.foragingB[1,(days+1)] <- foragingtime
  teb.onwaterrestingB[1,(days+1)] <- onwaterrestingtime
  teb.swimB[1,(days+1)] <- swimtime
  teb.landB[1,(days+1)] <- landtime
    
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Simulation C ####

# outside the breeding season, the bird undertook large migratory journeys to distinct wintering grounds (similar to the razorbill Alca torda).

# Run simulation through the total number of simulation days
for (days in 1:simlationdays) {
  
  # days = 1 # to test stuff on just one day
  
  Foreffic <- rnorm(1,800,100) # foraging efficiency of a great auk (scaled from guillemot foraging efficiency reported in Dunn et al 2022)
  
  sst <- ifelse(days > 130 & days < 190, SST.Eldey[days], SST.Temara[days]) # extract SST on each day
  
  bm <- bodymass[1,(days+1)] # extract the current body mass
  
  # Estimate foraging time and energy intake
  foragingtime <- (rnorm(n = 1, mean = ((metadata$startmass[metadata$individual == 1] - bodymass[1,(days + 1)]) * 38), sd = 30) +
                     rnorm(n = 1, mean = dailyEEC[1,(days)], sd = abs(dailyEEC[1,(days)] * 0.05))) /
    Foreffic
  foragingtime <- ifelse(foragingtime < 0, 0, foragingtime)
  foragingtime <- ifelse(foragingtime > 20, 20, foragingtime)
  intake<-foragingtime*Foreffic
  
  # Calculate the rest of the daily time activity budget
  leftovertime <- 24 - foragingtime
  landtime <- ifelse(days > 130 & days < 190, leftovertime * 0.5, 0)
  onwaterrestingtime <- (leftovertime) * ifelse(days > 130 & days < 190, 0.3, 0.7)
  swimtime <- (leftovertime) * ifelse(days > 130 & days < 190, 0.2, 0.3)
  
  BMR <- (((((4.05 * (bm)^0.79)/1000) * 18.8)) +
            ((3.201 * (bm ^0.719)))/24)/2 # calculate BMR as the mean of the two BMR equations
  
  # Calculate the activity-associated thermoregulation costs
  lowerCT<- 47.2 * ((bm)^-0.18)
  thermalcond_inwater <- (3.47 * ((bm/1000)^-0.573))#kj c^-1 h^-1 kg^-1
  thermalcond_onwater <- (1.532 * ((bm/1000)^-0.546))#kj c^-1 h^-1 kg^-1
  thermalcond_air <- (0.705 * ((bm/1000)^-0.461))#kj c^-1 h^-1 kg^-1
  
  # Calculate the time-activity-specific heat costs
  thermocosts<-((ifelse(sst < lowerCT, lowerCT - sst, 0) * (bm/1000) * thermalcond_inwater *
                   (swimtime + foragingtime)) +
                  (ifelse(sst < lowerCT, lowerCT - sst, 0) * (bm/1000) * thermalcond_onwater *
                     (onwaterrestingtime + foragingtime)) +
                  (ifelse(sst < lowerCT, lowerCT - sst, 0) * (bm/1000) * thermalcond_air *
                     landtime)) # (for simplicity we assumed that air temp = sst)
  
  # Calculate daily energy expenditure, incorporating our simulated time activity budgets,
  # their associated BMR multipliers and the associated thermoregulatory costs
  dailyEEC[1,(days+1)]<-sum((foragingtime*BMR*FTM),(onwaterrestingtime*BMR*OWRTM),
                            (swimtime*BMR*STM),(landtime*BMR*LTM)) + thermocosts
  
  # Save the other aspects of the energy budget
  energyincome[1,(days+1)]<-intake
  masschange[1,(days+1)]<-(energyincome[1,days+1]-dailyEEC[1,(days+1)])/38
  bodymass[1,(days+2)]<-bodymass[1,(days+1)]+masschange[1,(days+1)]
  
  # Save the TEBs
  teb.foragingC[1,(days+1)] <- foragingtime
  teb.onwaterrestingC[1,(days+1)] <- onwaterrestingtime
  teb.swimC[1,(days+1)] <- swimtime
  teb.landC[1,(days+1)] <- landtime
  
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create plot for figure in manuscript ####

# Daily energy expenditure data:
plot.df1 <- tibble(dee = c(t(dailyEEA)[-c(1:11),], t(dailyEEB)[-c(1:11),], t(dailyEEC)[-c(1:11),]),
                  date = rep(seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by="days"), times = 3),
                  scenario = rep(c("A", "B", "C"), each = days-10))

# Activity budget data:
plot.df2 <- tibble(date = rep(seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by="days"), times = 3),
                   foraging = c(t(teb.foragingA)[-c(1:11),], t(teb.foragingB)[-c(1:11),], t(teb.foragingC)[-c(1:11),]),
                   resting = c(t(teb.onwaterrestingA)[-c(1:11),], t(teb.onwaterrestingB)[-c(1:11),], t(teb.onwaterrestingC)[-c(1:11),]),
                   active = c(t(teb.swimA)[-c(1:11),], t(teb.swimB)[-c(1:11),], t(teb.swimC)[-c(1:11),]),
                   land = c(t(teb.landA)[-c(1:11),], t(teb.landB)[-c(1:11),], t(teb.landC)[-c(1:11),]),
                   scenario = rep(c("A", "B", "C"), each = days-10)) %>%
  gather(activity, time, foraging:land, factor_key=TRUE)

# summary(plot.df1$dee)

facet_names <- c(A = "Near colony, on land at night",
                 B = "Near colony, remained at-sea",
                 C = "Migrated south, remained at-sea")

ggplot() +
  geom_bar(data = plot.df2, aes(x = date, y = time, fill = activity),
           position = "fill", stat = "identity") +
  scale_fill_manual(values=c("#88CCEE", "#CC6677", "#DDCC77", "#117733"),
                    name = "Activity",
                    labels=c("Foraging", "On water (rest)", "Swimming", "On land")) +
  geom_point(data = plot.df1, aes(x = date,
                                  y = (dee-6600)/(10600-6600)),
             alpha = 0.6) +
  theme_bw() + theme(legend.position = "bottom",
                     strip.background = element_rect(fill = NA),
                     legend.title = element_blank()) +
  scale_x_date(date_breaks = "2 month", date_labels =  "%B", expand = c(0.01, 0.01)) +
  scale_y_continuous(sec.axis = sec_axis(~.*(10600-6600)+6600, name="Daily energy expenditure (kJ)",
                                         labels=scales::comma)) +
  ylab("Proportion of day in activity") +
  xlab("Date") +
  facet_grid(scenario~., labeller = as_labeller(facet_names))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Extract values for the text ####

# Scenario A
# Total annual DEE
sum(t(dailyEEA)[-c(1:11),], na.rm = T)
# Total non-breeding DEE
sum((t(dailyEEA)[-c(1:11),])[-c(130:190)])

# Scenario B
# Total annual DEE
sum(t(dailyEEB)[-c(1:11),], na.rm = T)
# Total non-breeding DEE
sum((t(dailyEEB)[-c(1:11),])[-c(130:190)])

# Scenario C
# Total annual DEE
sum(t(dailyEEC)[-c(1:11),], na.rm = T)
# Total non-breeding DEE
sum((t(dailyEEC)[-c(1:11),])[-c(130:190)])


# Extract just May and June dates to pull out some values for the manuscript
may.june <- plot.df1 %>%
  mutate(Month = as.numeric(format(date,'%m'))) %>%
  filter(Month == c(5,6))

# assimilation efficiency of 74.4%
0.744 * mean(may.june$dee)
0.744 * sd(may.june$dee)

# 20 cm sandlance = 158 kJ
(0.744 * mean(may.june$dee))/158
(0.744 * sd(may.june$dee))/158
