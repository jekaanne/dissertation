library(mediation) #Mediation package
library(rockchalk) #Graphing simple slopes; moderation
library(multilevel) #Sobel Test
library(bda) #Another Sobel Test option
library(gvlma) #Testing Model Assumptions 
library(stargazer) #Handy regression tables
library(sjPlot) # pearson correlation table for multiple variables incl. binary
library(apaTables)

# RQ1: Do disability (defined as having any type of disability), age, gender, race/ethnicity, or household income contribute to variation in self-reported transportation barriers or fulfillment of needs in transportation between disabled and nondisabled people? Do these individual characteristics contribute to variation in overall well-being? ####
ardraw2.correlates <- dplyr::select(ardraw2, disability_status, log.hhincome, age, gender, white, new.ttbpnmean, altbpnfmean, flourmean)
ardraw2.correlates <- data.table(ardraw2.correlates)
ardraw2.correlates$disability_status <- as.numeric(ardraw2.correlates$disability_status)
ardraw2.correlates$gender <- as.numeric(ardraw2.correlates$gender)
ardraw2.correlates$white <- as.numeric(ardraw2.correlates$white)
ardraw2.correlates$disability_status[ardraw2.correlates$disability_status == 2]=0
ardraw2.correlates$disability_status = relevel(ardraw2.correlates$disability_status, ref="Nondisabled")
ardraw2.correlates$log.hhincome[ardraw2.correlates$log.hhincome == "-Inf"]<- NA
#plot 
sjp.corr(ardraw2.correlates)
sjt.corr(ardraw2.correlates)

# regressions
#disability predicting TTBPN - SIGNIFICANT
reg.dis.income <- lm(new.ttbpnmean ~ disability_status + log.hhincome, ardraw2)
summary(reg.dis.income)
resid.disincome <- residuals(reg.dis.income)
shapiro.test(resid.disincome)
hist(resid.disincome)
qqPlot(resid.disincome)
skewness(resid.disincome)
gvlma(reg.dis.income)

#disability predicting Home Sat - SIGNIFICANT
summary(reg.dis.homesat <- lm(home_sat_mean ~ condis + log.hhincome, data = ardraw2, subset= -c(204, 98, 46, 81, 13)))
resid.dis.homesat<- residuals(reg.dis.homesat)
shapiro.test(resid.dis.homesat)
hist(resid.dis.homesat)
qqPlot(resid.dis.homesat)
skewness(resid.dis.homesat)
gvlma(reg.dis.homesat)

# RQ2: Which disability types (or functional limitation types) are more likely to be associated with transportation challenges? ####


summary(s1.ttbpn.funclimtypes <- lm(new.ttbpnmean ~ move_physically + understand_info + see_hear_info + be_around_people + deal_w_frustration + communicate, data = ardraw))

# S2 
library(nparLD)
ardraw2$id <- seq_len(nrow(ardraw2))
library(afex)
library(emmeans)
library(ggplot2)
library(psych)

summary(aov(new.ttbpnmean ~ bdis_seeing + bdis_hearing + bdis_walking +bdis_cognitive + bdis_selfcare, data=ardraw2))

#dis_comm, depression_severity, anxiety_severity, pain_severity, fatigue_severity, new.ttbpnmean,

# reshaping data for anova
s2disability.dat <-dplyr::select(ardraw2, bdis_seeing, bdis_hearing, bdis_walking, bdis_cognitive, bdis_selfcare, id, new.ttbpnmean, condis)

s2disability.dat <- subset(s2disability.dat, condis == 1)
id <- c("id", "new.ttbpnmean")
library(reshape2)
s2disability.dat.long <- melt(s2disability.dat, id.vars=id, variable.name = "disability", .na.rm = FALSE, factorsAsStrings = FALSE)

s2disability.dat.long <- arrange(s2disability.dat.long, id)

boxplot(new.ttbpnmean ~ disability*value, data = s2disability.dat.long)

library(nlme)
summary(Anova(c.new.ttbpnmean ~ disability, type = cc("II","III", 2, 3), data = s2disability.dat.long))

library(lmerTest)
fit <- lmer(new.ttbpnmean ~ bdis_seeing + bdis_hearing + bdis_walking + bdis_cognitive + bdis_selfcare + bdis_comm + bdepression_severity + banxiety_severity + bpain_severity + bfatigue_severity + (1|id), data=ardraw2)
anova(fit)

dis.np <- nparLD(new.ttbpnmean ~ bdis_seeing*bdis_hearing*bdis_walking*bdis_cognitive*bdis_selfcare*bdis_comm*bdepression_severity*banxiety_severity*bpain_severity*bfatigue_severity, data=ardraw2, subject = "id",  description=TRUE, time1.order=NULL, time2.order=NULL, group1.order=NULL, group2.order=NULL, plot.CI=FALSE, alpha=0.05, show.covariance=FALSE, order.warning=TRUE)

summary(s2.ttbpn.distypes <- lm(new.ttbpnmean ~ bdis_seeing + bdis_hearing + bdis_walking + bdis_cognitive + bdis_selfcare + bdis_comm + bdepression_severity + banxiety_severity + bpain_severity + bfatigue_severity, data = ardraw2))
install

summary(Anova(s2.ttbpn.distypes, Type="II",
              white.adjust=TRUE))
s2.ttbpn.distypes.resid <- residuals(s2.ttbpn.distypes) 
shapiro.test(s2.ttbpn.distypes.resid) # numeric test of normality
hist(s2.ttbpn.distypes.resid) # visual test of normality
outlierTest(s2.ttbpn.distypes) # Bonferonni p-value for most extreme obs
qqPlot(s2.ttbpn.distypes.resid) # test for influential residual outliers
skewness(s2.ttbpn.distypes.resid) 
ncvTest(s2.ttbpn.distypes) # non-constant error variance test (homoscedasticity)
gvlma(s2.ttbpn.distypes) # test of regression assumptions
lmtest::bptest(s2.ttbpn.distypes)
stargazer(s2.ttbpn.distypes,type="text")



# ? - non-independent data
  
# RQ3a: Is there a relationship between transportation barriers, disability, and well-being? ####
library(mediation)
summary(fitTTBPNmed <- lm(new.ttbpnmean ~ condis + log.hhincome, data=ardraw2))
summary(fitFlour <- lm(flourmean ~ condis + new.ttbpnmean + log.hhincome, data=ardraw2))
gvlma(fitTTBPNmed)
summary(fitTTBPNflourMed <- mediate(fitTTBPNmed, fitFlour, treat="condis", mediator="new.ttbpnmean"))
plot(fitTTBPNflourMed)
summary(fitMedBoot <- mediate(fitTTBPNmed, fitFlour, boot=TRUE, sims=999, treat="condis", mediator="new.ttbpnmean"))
plot(fitMedBoot)

# disability status moderating a relationship between TTBPN and FLOUR
summary(moddisTTBPN <- lm(flourmean ~ c.new.ttbpnmean + condis + c.new.ttbpnmean*condis + c.loghhincome:flourmean, data = ardraw2, subset = -c(39, 41))) #Model interacts IV & moderator
moddisTTBPN.resid <- residuals(moddisTTBPN) 
shapiro.test(moddisTTBPN.resid) # numeric test of normality
hist(moddisTTBPN.resid) # visual test of normality
outlierTest(moddisTTBPN) # Bonferonni p-value for most extreme obs
qqPlot(moddisTTBPN.resid) # test for influential residual outliers
skewness(moddisTTBPN.resid) 
gvlma(moddisTTBPN) #test of regression assumptions
ncvTest(moddisTTBPN) # non-constant error variance test (homoscedasticity)
lmtest::bptest(moddisTTBPN)

plotmoddisTTBPN  <- plotSlopes(moddisTTBPN, plotx="c.new.ttbpnmean", modx="condis", xlab = "FLourishing", ylab = "TTBPN", modxVals = "std.dev")


# step 1, flour predicted by disability status and hhincome 
summary(flour.dis <- lm(flourmean ~ condis + log.hhincome, data = ardraw2, subset = -c(144, 39)))
flour.dis.resid <- residuals(flour.dis) 
shapiro.test(flour.dis.resid) # numeric test of normality
hist(flour.dis.resid) # visual test of normality
outlierTest(flour.dis) # Bonferonni p-value for most extreme obs
qqPlot(flour.dis.resid) # test for influential residual outliers
skewness(flour.dis.resid) 
ncvTest(flour.dis) # non-constant error variance test (homoscedasticity)
gvlma(flour.dis) # test of regression assumptions
lmtest::bptest(flour.dis)
stargazer(flour.dis,type="text")


# step 2, flour predicted by disability status + hhincome and ttbpn
summary(flour.dis.income <- lm(flourmean ~ condis + log.hhincome + new.ttbpnmean, data = ardraw2, subset = -c(39)))
stargazer(flour.dis.income, type="text")
flour.dis.income.resid <- residuals(flour.dis.income) 
shapiro.test(flour.dis.income.resid) # numeric test of normality
hist(flour.dis.income.resid) # visual test of normality
outlierTest(flour.dis.income) # Bonferonni p-value for most extreme obs
qqPlot(flour.dis.income.resid) # test for influential residual outliers
skewness(flour.dis.income.resid) 
ncvTest(flour.dis.income) # non-constant error variance test (homoscedasticity)
gvlma(flour.dis.income) # test of regression assumptions
lmtest::bptest(flour.dis.income)

# step 3, flour predicted by hhincome, ttbpn 
summary(flour.ttbpn <- lm(flourmean ~ new.ttbpnmean + log.hhincome, data = ardraw2.correlates, subset = -c(39)))
stargazer(flour.dis,type="text")
flour.ttbpn.resid <- residuals(flour.ttbpn) 
shapiro.test(flour.ttbpn.resid) # numeric test of normality
hist(flour.ttbpn.resid) # visual test of normality
outlierTest(flour.ttbpn) # Bonferonni p-value for most extreme obs
qqPlot(flour.ttbpn.resid) # test for influential residual outliers
skewness(flour.ttbpn.resid) 
ncvTest(flour.ttbpn) # non-constant error variance test (homoscedasticity)
gvlma(flour.ttbpn) # test of regression assumptions
lmtest::bptest(flour.ttbpn)
apa.reg.table(flour.ttbpn, filename = "flourttbpn.doc", table.number = 1)

# step 3b, flour predicted by disability status, hhincome, altbpnf 
summary(flour.altbpnf <- lm(flourmean ~ altbpnfmean + disability_status + log.hhincome, data = ardraw2.correlates, subset = -c(39)))
stargazer(flour.dis,type="text")
flour.altbpnf.resid <- residuals(flour.altbpnf) 
shapiro.test(flour.altbpnf.resid) # numeric test of normality
hist(flour.altbpnf.resid) # visual test of normality
outlierTest(flour.altbpnf) # Bonferonni p-value for most extreme obs
qqPlot(flour.altbpnf.resid) # test for influential residual outliers
skewness(flour.altbpnf.resid) 
ncvTest(flour.altbpnf) # non-constant error variance test (homoscedasticity)
gvlma(flour.altbpnf) # test of regression assumptions
lmtest::bptest(flour.altbpnf)
apa.reg.table(flour.altbpnf, filename = "flouraltbpnf.doc", table.number = 1)




# RQ3b: Is there a relationship between fulfillment of psychological needs and well-being? ####
 
# RQ3c: Are there any group differences in the relationship between transportation barriers and well-being or fulfillment of basic psychological needs in transportation and well-being based on disability status? ####

# RQ4a: Does residential satisfaction moderate an inverse relationship between transportation barriers and well-being?  ####
# MODERATION
#Centering Data
ardraw2$c.new.ttbpnmean <- scale(ardraw2$new.ttbpnmean, center=TRUE, scale=FALSE) #Centering IV; ttbpn
ardraw2$c.home_sat_mean <- scale(ardraw2$home_sat_mean,  center=TRUE, scale=FALSE)
#Centering moderator; home_sat_mean
ardraw2$c.flourmean <- scale(ardraw2$flourmean,  center=TRUE, scale=FALSE)
#centering log hh.income(control)
ardraw2$c.loghhincome <- scale(ardraw2$log.hhincome,  center=TRUE, scale=FALSE)
#Centering outcome: flour
# baseline model: ttbpn and home_sat_mean predicting flourishing
library(gvlma)
summary(fitMod <- lm(c.flourmean ~ c.new.ttbpnmean + c.new.ttbpnmean:condis + home_sat_mean + log.hhincome, data = ardraw2,  subset = -c(39, 41)))
fitMod.resid <- residuals(fitMod) 
shapiro.test(fitMod.resid) # numeric test of normality
hist(fitMod.resid) # visual test of normality
outlierTest(fitMod) # Bonferonni p-value for most extreme obs
qqPlot(fitMod.resid) # test for influential residual outliers
skewness(fitMod.resid) 
ncvTest(fitMod) # non-constant error variance test (homoscedasticity)
gvlma(fitMod) # test of regression assumptions
lmtest::bptest(fitMod)
coef(summary(fitMod))
stargazer(fitMod, type="text")
apa.reg.table(fitMod, filename = "flour.ttbpn.homesat.doc", table.number = 1)


#moderation model
summary(fitMod.moder <- lm(c.flourmean ~ c.new.ttbpnmean + c.home_sat_mean + c.new.ttbpnmean*c.home_sat_mean + c.loghhincome, data = ardraw2, subset = -c(39, 41))) #Model interacts IV & moderator
fitMod.moder.resid <- residuals(fitMod.moder) 
shapiro.test(fitMod.moder.resid) # numeric test of normality
hist(fitMod.moder.resid) # visual test of normality
outlierTest(fitMod.moder) # Bonferonni p-value for most extreme obs
qqPlot(fitMod.moder.resid) # test for influential residual outliers
skewness(fitMod.moder.resid) 
ncvTest(fitMod.moder) # non-constant error variance test (homoscedasticity)
gvlma(fitMod.moder) # test of regression assumptions
lmtest::bptest(fitMod.moder)
coef(summary(fitMod.moder))
apa.reg.table(fitMod.moder, filename = "flour.ttbpn.homesat.moderation.doc", table.number = 1)

# moderation model w/ disability status and interaction
#moderation model
summary(disMod.moder <- lm(c.flourmean ~ c.new.ttbpnmean + c.home_sat_mean + c.home_sat_mean:condis + c.loghhincome, data = ardraw2, subset = -c(39, 41))) #Model interacts IV & moderator
disMod.moder.resid <- residuals(disMod.moder) 
shapiro.test(disMod.moder.resid) # numeric test of normality
hist(disMod.moder.resid) # visual test of normality
outlierTest(disMod.moder) # Bonferonni p-value for most extreme obs
qqPlot(disMod.moder.resid) # test for influential residual outliers
skewness(disMod.moder.resid) 
ncvTest(disMod.moder) # non-constant error variance test (homoscedasticity)
gvlma(disMod.moder) # test of regression assumptions



# RQ4b: Are there any group differences in the relationship between transportation barriers and well-being between disabled and nondisabled participants? ####

#moderation model + disstatus? 
library(MBESS)
summary(dis.flour.hs.ttbpn <- lm(c.flourmean ~ c.home_sat_mean + c.new.ttbpnmean + c.loghhincome + condis, data = ardraw2, subset = -c(39, 41))) #Model interacts IV & moderator
dis.flour.hs.ttbpn.resid <- residuals(dis.flour.hs.ttbpn) 
shapiro.test(dis.flour.hs.ttbpn.resid) # numeric test of normality
hist(dis.flour.hs.ttbpn.resid) # visual test of normality
outlierTest(dis.flour.hs.ttbpn) # Bonferonni p-value for most extreme obs
qqPlot(dis.flour.hs.ttbpn.resid) # test for influential residual outliers
skewness(dis.flour.hs.ttbpn.resid) 
ncvTest(dis.flour.hs.ttbpn) # non-constant error variance test (homoscedasticity)
gvlma(dis.flour.hs.ttbpn) # test of regression assumptions
lmtest::bptest(dis.flour.hs.ttbpn)
coef(summary(dis.flour.hs.ttbpn))
apa.reg.table(dis.flour.hs.ttbpn, filename = "flour.ttbpn.homesat.moderation.doc", table.number = 1)

ardraw2$c.home_sat_mean[ardraw2$c.home_sat_mean == "NaN"]=0

#mediated-moderator model
mediate <- mediation::mediate #A mediate function is in both the "psych" and "mediation" packages. This allows us to use the correct mediate function from the "mediation" package
#select variables into new data set to remove NAs
modmeddata <- dplyr::select(ardraw2, new.ttbpnmean, home_sat_mean, flourmean, condis, log.hhincome, c.new.ttbpnmean, c.home_sat_mean)
modmeddata_complete <- na.omit(modmeddata)
summary(Mod.Med.Model.1<-lm(new.ttbpnmean ~ condis*c.home_sat_mean, data = modmeddata_complete)) #This model predicts ttbpn from disability status, home satisfaction, and the interaction between the two

summary(Mod.Med.Model.2<-lm(c.flourmean ~ condis*c.home_sat_mean + c.new.ttbpnmean, data = modmeddata_complete)) #This model predicts flourishing from disability status, home satisfaction + income, ttbpn and the interaction between disability statu and home satisfaction

#Moderator must be in both models for mediate to work.
modmeddata_complete$low.homesat = modmeddata_complete$c.home_sat_mean+sd(modmeddata_complete$c.home_sat_mean) #Sets our level for 1 SD below mean of homesat
modmeddata_complete$low.ttbpn = modmeddata_complete$c.new.ttbpnmean+sd(modmeddata_complete$c.new.ttbpnmean) #Sets our level for 1 SD below mean of homesat
modmeddata_complete$high.homesat = modmeddata_complete$c.home_sat_mean-sd(modmeddata_complete$c.home_sat_mean) #Sets our level for 1 SD below mean of homesat
modmeddata_complete$high.ttbpn = modmeddata_complete$c.new.ttbpnmean-sd(modmeddata_complete$c.new.ttbpnmean) #Sets our level for 1 SD below mean of homesat

# hand model
summary(hand.mod <- lm(flourmean ~ c.new.ttbpnmean*c.home_sat_mean + condis, data = modmeddata_complete))
summary(bpathlow <- lm(flourmean ~ c.new.ttbpnmean*low.homesat+ condis, data = modmeddata_complete))
summary(bpathlow <- lm(flourmean ~ c.new.ttbpnmean*high.homesat+ condis, data = modmeddata_complete))

low.ttbpn #Check value of variable

summary(Mod.Med.low.ttbpn <- mediate(Mod.Med.Model.1, Mod.Med.Model.2, covariates = list(modmeddata_complete$c.new.ttbpnmean - low.ttbpn), boot = TRUE, boot.ci.type = "bca", treat="condis", mediator="c.new.ttbpnmean"))

plot(Mod.Med.low.ttbpn, xlim = 0:1)

high.ttbpn<-mean(modmeddata_complete$c.new.ttbpnmean)-sd(modmeddata_complete$c.new.ttbpnmean)

summary(Mod.Med.high.ttbpn <- mediate(Mod.Med.Model.1, Mod.Med.Model.2, covariates = list(modmeddata_complete$c.new.ttbpnmean - high.ttbpn), boot = TRUE, boot.ci.type = "bca", treat="condis", mediator="c.new.ttbpnmean"))

plot(Mod.Med.high.ttbpn, xlim = 0:1)


# MedMod w/ Lavaan
library(lavaan)
library(mediation) #Mediation package
library(rockchalk) #Graphing simple slopes; moderation
library(multilevel) #Sobel Test
library(bda) #Another Sobel Test option
library(gvlma) #Testing Model Assumptions 
library(stargazer) #Handy regression tables
Mod.Med.Lavaan <- 
  '
#Regressions
#These are the same regression equations from our previous example
#Except in this code we are naming the coefficients that are produced from the regression equations
#E.g., the regression coefficient for the effect of time on c.new.ttbpnmean is named "a1"
c.new.ttbpnmean ~ a1*condis + a2*c.home_sat_mean + a3*condis:c.home_sat_mean + a4*c.loghhincome
c.flourmean ~ cdash1*condis + cdash2*c.home_sat_mean + cdash3*condis:c.home_sat_mean + cdash4*c.loghhincome + b1*c.new.ttbpnmean

#Mean of centered alex (for use in simple slopes)
#This is making a coefficient labeled "c.home_sat_mean.mean" which equals the intercept because of the "1"
#(Y~1) gives you the intercept, which is the mean for our c.home_sat_mean variable
c.home_sat_mean ~ c.home_sat_mean.mean*1

#Variance of centered alex (for use in simple slopes)
#This is making a coefficient labeled "c.home_sat_mean.var" which equals the variance because of the "~~"
#Two tildes separating the same variable gives you the variance
c.home_sat_mean ~~ c.home_sat_mean.var*c.home_sat_mean

#Indirect effects conditional on moderator (a1 + a3*ModValue)*b1
indirect.SDbelow := (a1 + a3*(c.home_sat_mean.mean-sqrt(c.home_sat_mean.var)))*b1
indirect.SDabove := (a1 + a3*(c.home_sat_mean.mean+sqrt(c.home_sat_mean.var)))*b1

#Direct effects conditional on moderator (cdash1 + cdash3*ModValue)
#We have to do it this way because you cannot call the mean and sd functions in lavaan package
direct.SDbelow := cdash1 + cdash3*(c.home_sat_mean.mean-sqrt(c.home_sat_mean.var)) 
direct.SDabove := cdash1 + cdash3*(c.home_sat_mean.mean+sqrt(c.home_sat_mean.var))

#Total effects conditional on moderator
total.SDbelow := direct.SDbelow + indirect.SDbelow
total.SDabove := direct.SDabove + indirect.SDabove

#Proportion mediated conditional on moderator
#To match the output of "mediate" package
prop.mediated.SDbelow := indirect.SDbelow / total.SDbelow
prop.mediated.SDabove := indirect.SDabove / total.SDabove

#Index of moderated mediation
#An alternative way of testing if conditional indirect effects are significantly different from each other
index.mod.med := a3*b1
'

Mod.Med.SEM <- sem(Mod.Med.Lavaan, data = modmeddata_complete, se = "bootstrap",bootstrap = 10, fixed.x = TRUE)

summary(Mod.Med.SEM)
#Bootstraps
parameterEstimates(Mod.Med.SEM,
                   boot.ci.type = "bca.simple",
                   level = .95, ci = TRUE,
                   standardized = FALSE)[c(25:33),c(4:10)] #We index the matrix to only display columns we are interested in



# trying moderation w/ disability status as the predictor
summary(fitMod <- lm(c.flourmean ~ c.new.ttbpnmean + home_sat_mean + loghhincome, data = ardraw2, subset = -c(39, 41, 75, 163)))
fitMod.resid <- residuals(fitMod) 
shapiro.test(fitMod.resid) # numeric test of normality
hist(fitMod.resid) # visual test of normality
outlierTest(fitMod) # Bonferonni p-value for most extreme obs
qqPlot(fitMod.resid) # test for influential residual outliers
skewness(fitMod.resid) 
ncvTest(fitMod) # non-constant error variance test (homoscedasticity)
gvlma(fitMod) # test of regression assumptions
lmtest::bptest(fitMod)
coef(summary(fitMod))
stargazer(fitMod, type="text")
apa.reg.table(fitMod, filename = "flour.ttbpn.homesat.doc", table.number = 1)


#moderation model
summary(dispredict <- lm(c.new.ttbpnmean ~ disability_status + log.hhincome, data = ardraw2, subset = -c(39, 41))) #Model interacts IV & moderator
fitMod.moder.resid <- residuals(fitMod.moder) 
shapiro.test(fitMod.moder.resid) # numeric test of normality
hist(fitMod.moder.resid) # visual test of normality
outlierTest(fitMod.moder) # Bonferonni p-value for most extreme obs
qqPlot(fitMod.moder.resid) # test for influential residual outliers
skewness(fitMod.moder.resid) 
ncvTest(fitMod.moder) # non-constant error variance test (homoscedasticity)
gvlma(fitMod.moder) # test of regression assumptions
lmtest::bptest(fitMod.moder)
coef(summary(fitMod.moder))

# extra####
# mediation analysis
# calculate home_sat_mean variable
home_sat <- dplyr::select(ardraw2, home_sat_affordability, home_sat_conditions, home_sat_neighborhood)
ardraw2$home_sat_mean <- rowMeans(home_sat, na.rm = TRUE)
# mediation
library(mediation)
library(gvlma)
fitM <- lm(home_sat_mean ~ new.ttbpnmean, data=ardraw2, subset = -c(144, 39, 41, 75, 46)) #IV on M
fitM.resid <- residuals(fitM) 
shapiro.test(fitM.resid) # numeric test of normality
hist(fitM.resid) # visual test of normality
outlierTest(fitM) # Bonferonni p-value for most extreme obs
qqPlot(fitM.resid) # test for influential residual outliers
skewness(fitM.resid) 
ncvTest(fitM) # non-constant error variance test (homoscedasticity)
gvlma(fitM) # test of regression assumptions
lmtest::bptest(fitM)

summary(fitY <- lm(flourmean ~ new.ttbpnmean + home_sat_mean + log.hhincome, data=ardraw2, subset = -c(144, 39, 41, 75, 46))) #IV and M on DV
fitY.resid <- residuals(fitY) 
shapiro.test(fitY.resid) # numeric test of normality
hist(fitY.resid) # visual test of normality
outlierTest(fitY) # Bonferonni p-value for most extreme obs
qqPlot(fitY.resid) # test for influential residual outliers
skewness(fitY.resid) 
ncvTest(fitY) # non-constant error variance test (homoscedasticity)
gvlma(fitY) # test of regression assumptions
lmtest::bptest(fitY)


summary(fitMed <- mediate(fitM, fitY, treat="new.ttbpnmean", mediator="home_sat_mean"))
plot(fitMed)
#Bootstrap
fitMedBoot <- mediate(fitM, fitY, boot=TRUE, sims=999, treat="new.ttbpnmean", mediator="home_sat_mean")
summary(fitMedBoot)


# graphs
library(DiagrammeR)
grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle]        
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']

      # edge definitions with the node IDs
      tab1 -> tab2;

      tab1 -> tab3
      }

      [1]: 'Home satisfaction'
      [2]: 'Transportation barriers'
      [3]: 'Flourishing'
      ")


# Plot 
ardraw2 %>% group_by(flourmean, new.ttbpnmean) %>% 
  count() %>% 
  ggplot(aes(x = flourmean, y = new.ttbpnmean, size = n)) +
  geom_point(show.legend = F) +
  labs(x = expression(paste("Flourishing (", italic("X"), ")")),
       y = expression(paste("TTBPN (", italic("Y"), ")"))) + theme_bw()

ardraw2 %>% group_by(flourmean, home_sat_mean) %>% 
  count() %>% 
  ggplot(aes(x = flourmean, y = home_sat_mean, size = n)) +
  geom_point(show.legend = F) +
  labs(x = expression(paste("Flourishing (", italic("X"), ")")),
       y = expression(paste("Home Satisfaction (", italic("Y"), ")"))) + theme_bw()

cor.test(ardraw2$flourmean, ardraw2$new.ttbpnmean)
library(brms)
model2.1 <- brm(data = ardraw2, family = gaussian, mvbind(flourmean, new.ttbpnmean) ~ 1, cores = 4, set_rescor=TRUE)

fit1 <- brm(
  mvbind(new.ttbpnmean, flourmean) ~ condis + log.hhincome + (1|p|fosternest) + (1|q|dam),
  data = ardraw2, chains = 2, cores = 2
)


summary(lm(c.home_sat_mean ~ condis + new.ttbpnmean + log.hhincome, data = ardraw2))
summary(lm(formula = c.altbpnfmean ~ condis + new.ttbpnmean + log.hhincome, 
   data = ardraw2))
summary(lm(c.flourmean ~ condis + new.ttbpnmean + log.hhincome, data = ardraw2))


summary(lm(flourmean ~ new.ttbpnmean*c.home_sat_mean + log.hhincome, data = ardraw2))
summary(lm(flourmean~new.ttbpnmean*home_sat_mean +log.hhincome, data = ardraw2))