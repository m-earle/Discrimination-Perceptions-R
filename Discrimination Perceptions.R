
setwd("C:/Users/megan/Documents/change in living white vs minority/ANES/Data") ##set directory

ANESdata <- read.csv("anes_timeseries merged file 2012 2016.csv")

ANESdata[ANESdata==999] <- NA
ANESdata[ANESdata==""] <- NA

View(ANESdata)

##effect size for paired values
library(effsize)

# 0 = white, 1 = black participants

#CI for paired test, white discrim vs. black discrim perceptions, WHITE respondents

library(dplyr)

ANESwhite <- filter(ANESdata, ANESdata$bwrace == 0)
racet = table(ANESwhite$bwrace)
racefreq = as.data.frame(racet)
racefreq

summary (ANESwhite$blackdiscrim)
summary (ANESwhite$whitediscrim)

cohen.d(ANESwhite$blackdiscrim, ANESwhite$whitediscrim, na.rm = TRUE, pooled = TRUE, paired = TRUE)


#CI for paired test, white discrim vs. black discrim, BLACK respondents

ANESblack <- filter(ANESdata, ANESdata$bwrace == 1)

cohen.d(ANESblack$blackdiscrim, ANESblack$whitediscrim, na.rm = TRUE, pooled = TRUE, paired = TRUE)

#getting frequencies for repub dem

drt = table(ANESdata$drparty)
drfreq = as.data.frame(drt)
drfreq

# 1 = democrat
# 2 = republican

#CI for paired test, white discrim vs. black discrim, DEMOCRAT respondents

ANESdem <- filter(ANESdata, ANESdata$drparty == 1)

cohen.d(ANESdem$blackdiscrim, ANESdem$whitediscrim, na.rm = TRUE, pooled = TRUE, paired = TRUE)

#CI for paired test, white discrim vs. black discrim, REPUBLICAN respondents

ANESrep <- filter(ANESdata, ANESdata$drparty == 2)

cohen.d(ANESrep$blackdiscrim, ANESrep$whitediscrim, na.rm = TRUE, pooled = TRUE, paired = TRUE)

#CI for paired test, white discrim vs. black discrim, WHITE DEM respondents

ANESwhitedem <- filter(ANESdata, drparty == 1 & bwrace == 0)

cohen.d(ANESwhitedem$blackdiscrim, ANESwhitedem$whitediscrim, na.rm = TRUE, pooled = TRUE, paired = TRUE)

#CI for paired test, white discrim vs. black discrim, WHITE REP respondents

ANESwhiterep <- filter(ANESdata, drparty == 2 & bwrace == 0)

cohen.d(ANESwhiterep$blackdiscrim, ANESwhiterep$whitediscrim, na.rm = TRUE, pooled = TRUE, paired = TRUE)

#CI for paired test, white discrim vs. black discrim, BLACK DEM respondents

ANESblackdem <- filter(ANESdata, drparty == 1 & bwrace == 1)

cohen.d(ANESblackdem$blackdiscrim, ANESblackdem$whitediscrim, na.rm = TRUE, pooled = TRUE, paired = TRUE)

#CI for paired test, white discrim vs. black discrim, BLACK REP respondents

ANESblackrep <- filter(ANESdata, drparty == 2 & bwrace == 1)

cohen.d(ANESblackrep$blackdiscrim, ANESblackrep$whitediscrim, na.rm = TRUE, pooled = TRUE, paired = TRUE)

#sample size

nrow(ANESblackdem)
nrow(ANESblackrep)

#creating a box and whisker plot for perceptions

library(ggplot2)

library(tidyr)
install.packages("ggplot2", dependencies = TRUE)

ANESdatarace <- ANESdata[c("bwrace", "whitediscrim", "blackdiscrim")]
ANESdatamod <- gather(ANESdatarace, discrim, value, -bwrace)
head(ANESdatamod)

ANESdatamod$bwrace[ANESdatamod$bwrace == 0] <- "White Respondents"
ANESdatamod$bwrace[ANESdatamod$bwrace == 1] <- "Black Respondents"
ANESdatamod$discrim[ANESdatamod$discrim == "whitediscrim"] <- "Percieved Discrimination Against Whites"
ANESdatamod$discrim[ANESdatamod$discrim == "blackdiscrim"] <- "Percieved Discrimination Against Blacks"

head(ANESdatamod)

p <-  ggplot(subset(ANESdatamod, !is.na(bwrace)), aes(y = value, x = bwrace, fill = discrim)) + geom_boxplot() +
  labs(x = "Participant Race", y = "Discrimination Perceptions", fill = element_blank()) + scale_y_continuous(limits=c(0,6),                                                                                                       breaks=seq(1,5,1), expand = c(0, 0.02))
p+scale_fill_manual(values=c("grey40", "#FFFFFF")) + theme(legend.position = "bottom", 
                                                           panel.background = element_blank(), 
                                                           axis.line = element_line(colour = "grey"))
p


library(dplyr)
ANESwh.mod <- filter(ANESdatamod, ANESdatamod$bwrace == "White Respondents")
ANESbl.mod <- filter(ANESdatamod, ANESdatamod$bwrace == "Black Respondents")
head(ANESbl.mod)


#dot/bar plots

#bar and dot white

ANES.wh_sumary <- ANESwh.mod %>% # the names of the new data frame and the data frame to be summarised
  group_by(discrim) %>%   # the grouping variable
  summarise(mean_discrim = mean(value, na.rm = TRUE),  # calculates the mean of each group
            sd_discrim = sd(value, na.rm = TRUE), # calculates the standard deviation of each group
            n_discim = n(),  # calculates the sample size per group
            SE_discrim = sd(value, na.rm = TRUE)/sqrt(n())) # calculates the standard error of each group

whitebar <- ggplot(ANES.wh_sumary, aes(discrim, mean_discrim, fill = discrim)) + 
  geom_col(colour = "black") +  scale_fill_manual(values = c("Percieved Discrimination Against Blacks" = "gray45",
                                                             "Percieved Discrimination Against Whites" = "white")) + 
  geom_errorbar(aes(ymin = mean_discrim - sd_discrim, ymax = mean_discrim + sd_discrim), width=0.2) +
  geom_count(data = subset(ANESwh.dot, !is.na(bwrace)), (aes(y = value)), shape = 21, colour = "black", fill = NA) +
  labs(x = "White \nRespondents", y = "Discrimination Perceptions", fill = element_blank()) +
  theme(legend.position = "right", panel.background = element_blank(), 
        axis.line = element_line(colour = "grey"), axis.text.x = element_blank(), 
        axis.title.x = element_text(size=10),
        axis.title.y = element_text((size=12), margin = margin(t = 0, r = 10, b = 0, l = 0))) + 
  coord_fixed(ratio = 0.8) + scale_y_continuous(limits=c(0,5.5), breaks=seq(0,5,0.5), expand = c(0, 0))
whitebar

#bar and dot for black

ANES.bl_sumary <- ANESbl.mod %>% # the names of the new data frame and the data frame to be summarised
  group_by(discrim) %>%   # the grouping variable
  summarise(mean_discrim = mean(value, na.rm = TRUE),  # calculates the mean of each group
            sd_discrim = sd(value, na.rm = TRUE), # calculates the standard deviation of each group
            n_discim = n(),  # calculates the sample size per group
            SE_discrim = sd(value, na.rm = TRUE)/sqrt(n())) # calculates the standard error of each group

blackbar <- ggplot(ANES.bl_sumary, aes(discrim, mean_discrim, fill = discrim)) + 
  geom_col(colour = "black") +  scale_fill_manual(values = c("Percieved Discrimination Against Blacks" = "gray45",
                                                             "Percieved Discrimination Against Whites" = "white")) + 
  geom_errorbar(aes(ymin = mean_discrim - sd_discrim, ymax = mean_discrim + sd_discrim), width=0.2) +
  geom_count(data = subset(ANESbl.mod, !is.na(bwrace)), (aes(y = value)), shape = 21, colour = "black", fill = NA) +
  labs(x = "Black \nRespondents", y = "Discrimination Perceptions", fill = element_blank()) +
  theme(legend.position = "right", panel.background = element_blank(), 
        axis.line = element_line(colour = "grey"), axis.text.x = element_blank(), 
        axis.title.x = element_text(size=10),
        axis.title.y = element_text((size=12), margin = margin(t = 0, r = 10, b = 0, l = 0))) + 
  coord_fixed(ratio = 0.8) + scale_y_continuous(limits=c(0,5.5), breaks=seq(0,5,0.5), expand = c(0, 0))
blackbar

