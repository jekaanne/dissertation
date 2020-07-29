library(tidyverse)
library(broom)
library(data.table)
library(ggpubr)
library(rstatix)
library(datarium)
library(car)
library(stats)
library(coin)
library(lsr)
library(MASS)
library(rcompanion)
library(moments)
library(sjPlot)
library(mediation) #Mediation package
library(rockchalk) #Graphing simple slopes; moderation
library(multilevel) #Sobel Test
library(bda) #Another Sobel Test option
library(gvlma) #Testing Model Assumptions 
library(stargazer) #Handy regression tables
#library(QuantPsyc)
library(pequod)
library(knitr)

# disability tables ####
# S1
discolstable1 <- data.table(variables = c("Physical",  "", "Blind/Low-Vision", "", "Deaf/Hard of Hearing", "", "Developmental", "", "Chronic Condition", "", "Mental Health", ""))
 + understand_info + see_hear_info + be_around_people + deal_w_frustration + communicate
distable1<-table(ardraw$dis_physical)
distable2<-table(ardraw$dis_blv)
distable3<-table(ardraw$dis_dhoh)
distable4<-table(ardraw$dis_developmental)
distable5<-table(ardraw$dis_chronic_condition)
distable6<-table(ardraw$dis_mental_health)
# join tables together
distable<- data.table(rbind(distable1, distable2, distable3, distable4, distable5, distable6))
#create proportions of rows - change margins = 2 if column proportions are preferred
#dis_proportions <- prop.table(as.matrix(distable), margin=2)*100 , round(dis_proportions, 2)
#combine variables with individual contingency tables
discomplete <- cbind(discolstable1, distable)
#print(xtable(democomplete), type="latex", comment=FALSE)
print(discomplete) %>% 
  kable(digits = 3, format="pandoc", caption="demographics 1")


# by FL Type

distypecolstable1 <- data.table(variables = c("Move Physically",  "", "Understand Information", "", "See or Hear Information", "", "Be Around People", "", "Deal with Frustration", "", "Communicate", ""))
distypetable1<-table(ardraw$move_physically)
distypetable2<-table(ardraw$dis_understand_info)
distypetable3<-table(ardraw$see_hear_info)
distypetable4<-table(ardraw$be_around_people)
distypetable5<-table(ardraw$deal_w_frustration)
distypetable6<-table(ardraw$communicate)

# join all together
distypetable <- rbind(distypetable1, distypetable2, distypetable3, distypetable4, distypetable5, distypetable6, 2)
#proportions
#distype_proportions <- prop.table(as.matrix(distypetable), margin=1)*100
#cbind tables and proportions, round(distype_proportions, 2)
distypecomplete <- cbind(distypecolstable1, distypetable)
#combine variables with individual contingency tables
#print(xtable(democomplete), type="latex", comment=FALSE)
print(distypecomplete) %>% 
  kable(digits = 3, format="pandoc", caption="demographics 1")



# SURVEY2 dis types mean ttbpn score 
discolstable2 <- data.table(variables = c("Seeing", "Hearing", "Walking", "Remembering or Concentrating", "Self-Care", "Communicating", "Severe Depression", "Severe Anxiety", "Severe Pain",  "Severe Fatigue"))
distype2table1<-table(ardraw2$bdis_seeing)
distype2table2<-table(ardraw2$bdis_hearing)
distype2table3<-table(ardraw2$bdis_walking)
distype2table4<-table(ardraw2$bdis_cognitive)
distype2table5<-table(ardraw2$bdis_selfcare)
distype2table6<-table(ardraw2$bdis_comm)
distype2table7<-table(ardraw2$bdepression_severity)
distype2table8<-table(ardraw2$banxiety_severity)
distype2table9<-table(ardraw2$bpain_severity)
distype2table10<-table(ardraw2$bfatigue_severity)
#join together
distype2table <- rbind(distype2table1, distype2table2, distype2table3, distype2table4, distype2table5, distype2table6, distype2table7, distype2table8, distype2table9, distype2table10)
distype2complete <- cbind(discolstable2, distype2table)
#combine variables with individual contingency tables
#print(xtable(democomplete), type="latex", comment=FALSE)
print(distype2complete) %>% 
  kable(digits = 3, format="pandoc", caption="demographics 1")


# preliminary analysis ####
# S1: ttbpn scale with modified variables
ttbpnmod <- dplyr::select(ardraw, aut2, aut4, aut5, aut6, rel1, rel2, rel3, rel4 , com1, com2, com3, com6)
ardraw$new.ttbpnmean <- rowMeans(ttbpnmod, na.rm = TRUE)
ardraw$altbpnfmean <- rowMeans(altbpnf, na.rm = TRUE)

# S2: ttbpn scale with modified variables
ttbpnmod2 <- select(ardraw2factors, aut2, aut3, aut5, aut6, rel2, rel3, rel4, rel6, com1 , com2, com3, com4)
ardraw2$new.ttbpnmean <- rowMeans(ttbpnmod2, na.rm = TRUE)
ardraw2$altbpnfmean <- rowMeans(altbpnf2, na.rm = TRUE)


#RQ1a: Diff in disability status on TTBPN? ALTBPNF? ####
# S1: assumptions TTBPN
# doesn't meet the assumption of normality for a t. test
shapiro.test(ardraw$new.ttbpnmean)
#try norming everything

ardraw$log.ttbpnmean <- log(ardraw$new.ttbpnmean)
shapiro.test(ardraw$log.ttbpnmean)
ardraw$sq.ttbpnmean <- (ardraw$new.ttbpnmean)^2
shapiro.test(ardraw$sq.ttbpnmean)
ardraw$sqrt.ttbpnmean <- sqrt(ardraw$new.ttbpnmean)
shapiro.test(ardraw$sqrt.ttbpnmean)
# calculate sum instead?
ardraw$sum.ttbpn <- rowSums(ttbpnmod)
shapiro.test(ardraw$sum.ttbpn)

# S1: assumptions altbpnf
shapiro.test(ardraw$altbpnfmean)
#try norming everything
ardraw$log.altbpnfmean <- log(ardraw$altbpnfmean)
shapiro.test(ardraw$log.altbpnfmean)
ardraw$sq.altbpnfmean <- (ardraw$altbpnfmean)^2
shapiro.test(ardraw$sq.altbpnfmean)
ardraw$sqrt.altbpnfmean <- sqrt(ardraw$altbpnfmean)
shapiro.test(ardraw$sqrt.altbpnfmean)
# calculate sum instead?
ardraw$sum.altbpnf <- rowSums(altbpnf)
shapiro.test(ardraw$sum.altbpnf)
# doesn't meet the assumption of normality for a t. test

# S1 wilcoxon rank sum test TTBPN
wilc.ttbpn.dis <- wilcox.test( formula = ardraw$new.ttbpnmean ~ disability_status, data = ardraw)
wilcox_test(ardraw$new.ttbpnmean ~ ardraw$disability_status) %>%
  add_significance()
wilc.ttbpn.dis
ardraw %>% wilcox_effsize(new.ttbpnmean ~ disability_status)

#significant group difference with medium effect size p < 0.001, r = 0.41)
# group differences for altbpn-f
wilc.altbpnf.dis <- wilcox.test( formula = altbpnfmean ~ disability_status, data = ardraw)
wilcox_test(ardraw$altbpnfmean ~ ardraw$disability_status) %>%
  add_significance()
wilc.altbpnf.dis
ardraw %>% wilcox_effsize(altbpnfmean ~ disability_status)

# S2: assumptions TTBPN #
# doesn't meet the assumption of normality for a t. test
shapiro.test(ardraw2$new.ttbpnmean)
#try norming everything
ardraw2$log.hhincome <- log(ardraw2$hhincome)
ardraw2$log.ttbpnmean <- log(ardraw2$new.ttbpnmean)
shapiro.test(ardraw2$log.ttbpnmean)
ardraw2$sq.ttbpnmean <- (ardraw2$new.ttbpnmean)^2
shapiro.test(ardraw2$sq.ttbpnmean)
ardraw2$sqrt.ttbpnmean <- sqrt(ardraw2$new.ttbpnmean)
shapiro.test(ardraw2$sqrt.ttbpnmean)
# calculate sum instead?
ardraw2$sum.ttbpn <- rowSums(ttbpnmod2)
shapiro.test(ardraw2$sum.ttbpn)

# S2: assumptions altbpnf
shapiro.test(ardraw2$altbpnfmean)
#try norming everything
ardraw2$log.altbpnfmean <- log(ardraw2$altbpnfmean)
shapiro.test(ardraw2$log.altbpnfmean)
ardraw2$sq.altbpnfmean <- (ardraw2$altbpnfmean)^2
shapiro.test(ardraw2$sq.altbpnfmean)
ardraw2$sqrt.altbpnfmean <- sqrt(ardraw2$altbpnfmean)
shapiro.test(ardraw2$sqrt.altbpnfmean)
# calculate sum instead?
ardraw2$sum.altbpnf <- rowSums(altbpnf)
shapiro.test(ardraw2$sum.altbpnf)
# doesn't meet the assumption of normality for a t. test

# S2 wilcoxon rank sum test TTBPN #
wilc.ttbpn.dis2 <- wilcox.test(formula = new.ttbpnmean ~ disability_status, data = ardraw2)
wilcox_test(ardraw2$new.ttbpnmean ~ ardraw2$disability_status) %>%
  add_significance()
wilc.ttbpn.dis2
ardraw2 %>% wilcox_effsize(new.ttbpnmean ~ disability_status)

#significant group difference with medium effect size p < 0.001, r = 0.41)
# group differences for altbpn-f
wilc.altbpnf.dis2 <- wilcox.test( formula = altbpnfmean ~ disability_status, data = ardraw2)
wilcox_test(ardraw2$altbpnfmean ~ ardraw2$disability_status) %>%
  add_significance()
wilc.altbpnf.dis
ardraw2 %>% wilcox_effsize(altbpnfmean ~ disability_status)

# group differences for flour
wilc.flour.dis2 <- wilcox.test( formula = flourmean ~ disability_status, data = ardraw2)
wilcox_test(ardraw2$altbpnfmean ~ ardraw2$disability_status) %>%
  add_significance()
wilc.flour.dis2
ardraw2 %>% wilcox_effsize(flourmean ~ disability_status)


#RQ1b: Age, Gender HHincome, Race on TTBPN? ALT-BPNF? ####
#new table of correlates
#new table of correlates
library(data.table)
ardraw2 <- data.table(ardraw2)
ardraw2.correlates <- dplyr::select(ardraw2, disability_status, hhincome, age, gender, white, employed, new.ttbpnmean, altbpnfmean, flourmean)
ardraw2.correlates <- data.table(ardraw2.correlates)
ardraw2.correlates$disability_status <- as.numeric(ardraw2.correlates$disability_status)
ardraw2.correlates$gender <- as.numeric(ardraw2.correlates$gender)
ardraw2.correlates$white <- as.numeric(ardraw2.correlates$white)

#plot 
library(sjPlot)
sjp.corr(ardraw2.correlates)
sjt.corr(ardraw2.correlates)


# S1 covariates TTBPN
# colinearity (may be able to remove income as a predictor)
library(VIF)
library(car)
#TTBPN - income
reg.dis.income <- lm(new.ttbpnmean ~ disability_status + hhincome + disability_status:hhincome, ardraw2)
summary(reg.dis.income)
resid.disincome <- residuals(reg.dis.income)
shapiro.test(resid.disincome)
hist(resid.disincome)
qqPlot(resid.disincome)
skewness(resid.disincome)

#altbpnf - income
altbpn.dis.income <- lm(altbpnfmean ~ disability_status + hhincome + disability_status:hhincome, ardraw2, subset = -41)
summary(altbpn.dis.income)
resid.disincome <- residuals(altbpn.dis.income)
shapiro.test(resid.disincome)
hist(resid.disincome)
qqPlot(resid.disincome)
skewness(resid.disincome)

#flour - income
flour.dis.income <- lm(flourmean ~ disability_status + hhincome + disability_status:hhincome, ardraw2, subset = -c(39))
summary(flour.dis.income)
resid.disincome <- residuals(flour.dis.income)
shapiro.test(resid.disincome)
hist(resid.disincome)
qqPlot(resid.disincome)
skewness(resid.disincome)


#RQ2: Which disability types impact TTBPN? ALT-BPNF? ####
# SURVEY1 dis types mean ttbpn score 
# White-adjusted anova for heteroscedasticity
summary(s1.ttbpn.distypes <- lm(new.ttbpnmean ~ dis_physical + dis_blv + dis_dhoh + dis_developmental + dis_chronic_condition + dis_mental_health, data = ardraw))
#Anova(s1.ttbpn.distypes, Type="II",
      #white.adjust=TRUE)
vif(s1.ttbpn.distypes)


summary(s1.ttbpn.funclimtypes <- lm(new.ttbpnmean ~ move_physically + understand_info + see_hear_info + be_around_people + deal_w_frustration + communicate, data = ardraw))
Anova(s1.ttbpn.funclimtypes, Type="II",
          white.adjust=TRUE)
vif(s1.ttbpn.funclimtypes)

# SURVEY2 dis types mean ttbpn score 
summary(s2.ttbpn.distypes <- glm(new.ttbpnmean ~ bdis_seeing + bdis_hearing + bdis_walking + bdis_cognitive + bdis_selfcare, data = ardraw2))
Anova(s2.ttbpn.distypes, Type="II",
      white.adjust=TRUE)
summary(s2.ttbpn.distypes)
vif(s2.ttbpn.distypes)
#s2 other disabilities
summary(s2.ttbpn.otherdistypes <- lm(new.ttbpnmean ~ bdis_comm + bdepression_severity + banxiety_severity + bpain_severity + bfatigue_severity, data = ardraw2))
Anova(s2.ttbpn.otherdistypes, Type="II",
      white.adjust=TRUE)
summary(s2.ttbpn.otherdistypes)
vif(s2.ttbpn.distypes)





#SURVEY 2 - impact_trans

# anova ttbpn scale
new.ttbpn_transdistypes2 <- aov(new.ttbpnmean ~ bdis_seeing + bdis_hearing + bdis_walking + bdis_cognitive + bdis_comm + bdis_selfcare, data = ardraw2)
summary(new.ttbpn_transdistypes2)

# anova altbpnf scale
new.ttbpn_transdistypes2 <- aov(altbpnfmean ~ bdis_seeing + bdis_hearing + bdis_walking + bdis_cognitive + bdis_comm + bdis_selfcare, data = ardraw2)
summary(new.ttbpn_transdistypes2)

#RQ3a: Relationship between TTBPN & FLOUR? ALT-BPNF & FLOUR?  Only for PWD? ####
#correlation matrix of continous variables
round(cor(cbind(ardraw2$flourmean,ardraw2$new.ttbpnmean, ardraw2$hhincome), use = "pairwise.complete.obs"),2)

# s2 model omitting outliers
ttbpn.flour.outlier <- lm(formula = flourmean ~ center.ttbpnmean + condis*log.hhincome, data = ardraw2, subset = -c(39, 163, 125))
resid.tf.outliers <- residuals(ttbpn.flour.outlier)
shapiro.test(resid.tf.outliers) #residuals are normal after removing outlier
hist(resid.tf.outliers)
qqPlot(resid.tf.outliers)
skewness(resid.tf.outliers)
# test of heteroscedasticity (it's not!!)
lmtest::bptest(ttbpn.flour.outlier)
summary(ttbpn.flour.outlier)
vif(ttbpn.flour.outlier)
#plot fit lines
ggplot(ardraw2, aes(y = new.ttbpnmean, x = flourmean, color = condis)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)
#simpson's paradox
ggplot(ardraw2, aes(y = log.hhincome, x = new.ttbpnmean, color = condis)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

# plot with two slopes
slr + aes(color = condis)
gvlma(ttbpn.flour.outlier)

# because I don't know wtf I'm talking about
ttbpn.dis <- lm(ttbpnmean ~ disability_status, data = ardraw2, subset = -c(44, 144, 41, 142))
ttbpn.dis.resid <- residuals(ttbpn.dis)
shapiro.test(ttbpn.dis.resid)
hist(ttbpn.dis.resid)
qqPlot(ttbpn.dis.resid)
skewness(ttbpn.dis.resid)
# test of heteroscedasticity (it's not!!)
gvlma(ttbpn.dis)
lmtest::bptest(ttbpn.dis)
summary(ttbpn.dis.resid)

#autbpnf 
altbpn.dis <- lm(altbpnfmean ~ disability_status, data = ardraw2, subset = -c(44, 144, 41, 142))
altbpn.dis.resid <- residuals(altbpn.dis)
shapiro.test(altbpn.dis.resid)
hist(altbpn.dis.resid)
qqPlot(altbpn.dis.resid)
skewness(altbpn.dis.resid)
# test of heteroscedasticity (it's not!!)
gvlma(altbpn.dis)
lmtest::bptest(altbpn.dis)
summary(altbpn.dis.resid)


ardraw2$c.altbpnfmean <- scale(ardraw2$altbpnfmean)
# create separate datasets for disabled, nondisabled
dis.ardraw <- dplyr::select(ardraw2, flourmean, c.new.ttbpnmean, condis, log.hhincome, c.altbpnfmean, c.home_sat_mean)
dis.ardraw.ttbpnflour <- subset(dis.ardraw, condis ==1)
nondis.ardraw.ttbpnflour  <- subset(dis.ardraw, condis ==-1)

dis.ttbpnflour <- lm(formula = flourmean ~ center.ttbpnmean +  log.hhincome, data = dis.ardraw.ttbpnflour)
dis.resid.tf.outliers <- residuals(dis.ttbpnflour)
shapiro.test(dis.resid.tf.outliers) 
hist(dis.resid.tf.outliers)
qqPlot(dis.resid.tf.outliers)
skewness(dis.resid.tf.outliers)
# test of heteroscedasticity (it's not!!)
lmtest::bptest(dis.ttbpnflour)
summary(dis.ttbpnflour)
gvlma(dis.ttbpnflour)
vif(dis.ttbpnflour)


nondis.ttbpnflour <- lm(formula = flourmean ~ center.ttbpnmean + log.hhincome, data = nondis.ardraw.ttbpnflour, subset = -c(26))
nondis.resid.tf.outliers <- residuals(nondis.ttbpnflour)
shapiro.test(nondis.resid.tf.outliers) 
hist(nondis.resid.tf.outliers)
qqPlot(nondis.resid.tf.outliers)
skewness(nondis.resid.tf.outliers)
# test of heteroscedasticity (it's not!!)
lmtest::bptest(nondis.ttbpnflour)
summary(nondis.ttbpnflour)


# s2 ALTBPN models
altbpnf.flour.outlier <- lm(formula = flourmean ~ altbpnfmean + condis*log.hhincome, data = ardraw2, subset = -c(39,144))
resid.tf.outliers <- residuals(altbpnf.flour.outlier)
shapiro.test(resid.tf.outliers) #residuals are normal after removing outlier
hist(resid.tf.outliers)
qqPlot(resid.tf.outliers)
skewness(resid.tf.outliers)
# test of heteroscedasticity (it's not!!)
lmtest::bptest(altbpnf.flour.outlier)
summary(altbpnf.flour.outlier)
gvlma(altbpnf.flour.outlier)
vif(altbpnf.flour.outlier)
# interaction plot
ggplot(ardraw2, aes(y = flourmean, x = altbpnfmean, color = condis)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)


dis.altbpnfflour <- lm(formula = flourmean ~ altbpnfmean*log.hhincome, data = dis.ardraw)
dis.resid.tf.outliers <- residuals(dis.altbpnfflour)
shapiro.test(dis.resid.tf.outliers) 
hist(dis.resid.tf.outliers)
qqPlot(dis.resid.tf.outliers)
skewness(dis.resid.tf.outliers)
# test of heteroscedasticity (it's not!!)
lmtest::bptest(dis.altbpnfflour)
summary(dis.altbpnfflour)
vif(dis.altbpnfflour)


nondis.altbpnfflour <- lm(formula = flourmean ~ altbpnfmean*hhincome, data = nondis.ardraw.ttbpnflour, subset = -26)
nondis.resid.tf.outliers <- residuals(nondis.altbpnfflour)
shapiro.test(nondis.resid.tf.outliers) 
hist(nondis.resid.tf.outliers)
qqPlot(nondis.resid.tf.outliers)
skewness(nondis.resid.tf.outliers)
# test of heteroscedasticity (it's not!!)
lmtest::bptest(nondis.altbpnfflour)
summary(nondis.altbpnfflour)

#RQ3b does home sat moderate? what about disability? ####
#Centering Data
ardraw2$center.ttbpnmean    <- c(scale(ardraw2$new.ttbpnmean, center=TRUE, scale=FALSE)) #Centering ttbpn
ardraw2$center.homesatmean    <- c(scale(ardraw2$home_sat_mean,  center=TRUE, scale=FALSE)) #Centering moderator; homesat

mod.allpart <- lm(flourmean ~ center.ttbpnmean + center.homesatmean + center.ttbpnmean:center.homesatmean + condis*log.hhincome, data = dis.ardraw, subset =- 39)
summary(mod.allpart)


#HOME SAT as moderator? 
#interaction for ttbpn + dis
ardraw2$numericdis <- as.numeric(ardraw2$condis)
ardraw2$numericdis[ardraw2$numericdis ==1] =0
ardraw2$numericdis[ardraw2$numericdis ==2] =1

ardraw2$ttbpn.disstatus <- ardraw2$center.ttbpnmean*ardraw2$numericdis
#mod model
moderationmodel.homesat <- lm(flourmean ~ center.ttbpnmean*center.homesatmean + condis*log.hhincome, data = ardraw2, subset = -39) #Model interacts IV & moderator
summary(moderationmodel.homesat)
#summary 
resid.mod.homesat <- residuals(moderationmodel.homesat)
shapiro.test(resid.mod.homesat) 
skewness(resid.mod.homesat)
qqPlot(resid.mod.homesat)
summary(moderationmodel.homesat)
coef(summary(moderationmodel.homesat))
gvlma(moderationmodel.homesat)
stargazer(moderationmodel.homesat,type="text", title = "homesat and tttbpn")
plothomesatflour  <- plotSlopes(moderationmodel.homesat, plotx="center.ttbpnmean", modx="center.homesatmean", xlab = "flour", ylab = "ttbpn")
library(jtools)
summ(mod.allpart)


# altbpnf model w/ home sat
ardraw2$center.altbpnfmean    <- c(scale(ardraw2$altbpnfmean, center=TRUE, scale=FALSE)) #Centering altbpn
library(gvlma)
moderation.altbpn.homesat <- lm(flourmean ~ center.altbpnfmean + center.homesatmean + center.altbpnfmean:center.homesatmean + condis*log.hhincome, data = ardraw2, subset = -39) #Model interacts IV & moderator
summary(moderation.altbpn.homesat)
#summary 
resid.altbpn.homesat <- residuals(moderation.altbpn.homesat)
shapiro.test(resid.altbpn.homesat) 
skewness(resid.altbpn.homesat)
qqPlot(resid.altbpn.homesat)
coef(summary(moderation.altbpn.homesat))
gvlma(moderation.altbpn.homesat)
library(stargazer)
stargazer(moderation.altbpn.homesat,type="text", title = "homesat and altbpn")
library(rockchalk)
plothomesat.altbpnflour  <- plotSlopes(moderation.altbpn.homesat, plotx="center.altbpnfmean",  modxVals="std.dev.",modx="center.homesatmean", xlab = "flour", ylab = "altbpn")
summ(moderation.altbpn.homesat)

# disabled only - home sat as moderator
dis.mod <- lm(flourmean ~ center.ttbpnmean*center.homesatmean + log.hhincome, data = dis.ardraw.ttbpnflour) #Model interacts IV & moderator
summary(dis.mod)
#summary 
resid.mod.homesat.dis <- residuals(moderationmodel.homesat.dis)
shapiro.test(resid.mod.homesat.dis) 
skewness(resid.mod.homesat.dis)
qqPlot(resid.mod.homesat.dis)
summary(moderationmodel.homesat.dis)
coef(summary(moderationmodel.homesat.dis))
gvlma(dis.mod)
stargazer(dis.mod.dis,type="text", title = "homesat and tttbpn")
plothomesatflour  <- plotSlopes(dis.mod, plotx="center.ttbpnmean", modx="center.homesatmean", xlab = "flour", ylab = "ttbpn")
library(jtools)
summ(dis.mod)

#nondisabled only homesat as mod
nondis.mod <- lm(flourmean ~ c.new.ttbpnmean*c.home_sat_mean + log.hhincome, data = nondis.ardraw.ttbpnflour, subset =-26) #Model interacts IV & moderator
summary(nondis.mod )
#summary 
resid.nondis.mod  <- residuals(nondis.mod )
shapiro.test(resid.nondis.mod) 
skewness(resid.nondis.mod)
qqPlot(resid.nondis.mod)
summary(resid.nondis.mod)
coef(summary(nondis.mod))
gvlma(nondis.mod)
stargazer(nondis.mod,type="text", title = "homesat and tttbpn")
plothomesatflour  <- plotSlopes(nondis.mod, plotx="center.ttbpnmean", modx="center.homesatmean", xlab = "flour", ylab = "ttbpn")
library(jtools)
summ(nondis.mod)

#extra code ####
#EMP status as moderator? 
library(gvlma)
moderationmodel.emp <- lm(flourmean ~ new.ttbpnmean + employed + new.ttbpnmean:employed + condis*log.hhincome, data = ardraw2, subset = -39) #Model interacts IV & moderator
summary(moderationmodel.emp)
#summary 
resid.mod.emp <- residuals(moderationmodel.emp)
shapiro.test(resid.mod.emp) 
skewness(resid.mod.emp)
qqPlot(resid.mod.emp)
summary(moderationmodel.emp)
coef(summary(moderationmodel.emp))
gvlma(moderationmodel.emp)
library(stargazer)
stargazer(moderationmodel.emp,type="text", title = "emp and tttbpn")
library(rockchalk)
plotempflour  <- plotSlopes(moderationmodel.emp, plotx="center.ttbpnmean", modx="center.empmean", xlab = "flour", ylab = "ttbpn")
summ(moderationmodel.emp)

#altbpn model w/ emp as moderator
library(gvlma)
moderationmodel.altbpnf.emp <- lm(flourmean ~ altbpnfmean+ employed + altbpnfmean:employed +condis*log.hhincome, data = ardraw2, subset = -39) #Model interacts IV & moderator
summary(moderationmodel.altbpnf.emp )
#summary 
resid.mod.emp <- residuals(moderationmodel.altbpnf.emp )
shapiro.test(resid.mod.emp) 
skewness(resid.mod.emp)
qqPlot(resid.mod.emp)
summary(moderationmodel.altbpnf.emp )
coef(summary(moderationmodel.altbpnf.emp ))
gvlma(moderationmodel.altbpnf.emp )
library(stargazer)
stargazer(moderationmodel.altbpnf.emp,type="text", title = "emp and taltbpnf")
library(rockchalk)
plotempflour  <- plotSlopes(moderationmodel.altbpnf.emp, plotx="center.altbpnfmean", modx="employed", xlab = "flour", ylab = "altbpnf")
summ(moderationmodel.altbpnf.emp )

library(bestNormalize)
# Box Cox's Transformation
(BN_obj <- bestNormalize(ardraw.correlates$flourmean))
# it chose "ordernorm"
# orderNorm Transformation
(orderNorm_obj <- orderNorm(ardraw.correlates$flourmean))
xx <- seq(min(ardraw.correlates$flourmean), max(ardraw.correlates$flourmean), length = 100)
plot(xx, predict(orderNorm_obj, newdata = xx), type = "l", col = 1, ylim = c(-4, 4),
     xlab = 'x', ylab = "g(x)")
lines(xx, predict(boxcox_obj, newdata = xx), col = 2)
lines(xx, predict(orderNorm_obj, newdata = xx), col = 4)
legend("bottomright", legend = c("OrderNorm", "Box Cox"), col = 1:4, lty = 1, bty = 'n')
par(mfrow = c(2,2))
MASS::truehist(boxcox_obj$x.t, main = "Box Cox transformation", nbins = 12)
MASS::truehist(orderNorm_obj$x.t, main = "orderNorm transformation", nbins = 12)

#boxcox looks more normal though?
(boxcox_obj <- boxcox(ardraw.correlates$flourmean))
plot(boxcox_obj)

# S2: box-cox transformation #flour
# https://rcompanion.org/handbook/I_12.html
# Transform flourmean as a single vector
flour.box2 = boxcox(ardraw2$flourmean ~ 1, lambda = seq(-2,2,0.1))
# Create a data frame with the results
flour.cox2 = data.frame(flour.box$x, flour.box$y)  
# Order the new data frame by decreasing y
flour.cox2.ord = flour.cox[with(flour.cox, order(-flour.cox$flour.box.y)),] 
# Display the lambda with the greatest log likelihood
flour.cox2.ord[1,]                                  
# Extract that lambda
lambda = flour.cox2.ord[1, "flour.box.x"]
# Transform the original data
ardraw2$box.flourmean = (ardraw2$flourmean ^ lambda - 1)/lambda   
plotNormalHistogram(ardraw2$box.flourmean)
#test for normality (still skewed!)
shapiro.test(ardraw2$box.flourmean)

#RQ3b: Moderation of home satisfaction or employment status? Only for PWD?
#calculate home_sat_mean variable
home_sat <- dplyr::select(ardraw2, home_sat_affordability, home_sat_conditions, home_sat_neighborhood)
ardraw2$home_sat_mean <- rowMeans(home_sat, na.rm = TRUE)

# SEM mediation model
library(lavaan)
med.model.res <- ' #direct effect
flourmean ~ c*ttbpnmean
# homogeneity of variance is assumption of ANOVA
# groups sometimes have diff variance (may need to switch back to regression if )
# moderation
2 main effects and interaction (see if interaction is significant)

center.homesatmean ~ a*center.ttbpnmean
flourmean ~ b*center.homesatmean
# indirect effect (a*b)
ab := a*b
# total effect
total := c + (a*b)
'
med.model.fit <- sem(med.model.res, data = ardraw2, group = "condis")
summary(med.model.fit)

model <- ' # direct effect
Y ~ c*X
# mediator
M ~ a*X
Y ~ b*M
# indirect effect (a*b)
ab := a*b
# total effect
total := c + (a*b)
'
fit <- sem(model, data = Data)


#Mediation package model w/ treatment variable for home satisfaction as mediator
library(mediation)
model_mediator <- lm(home_sat_mean ~ disability_status + new.ttbpnmean, data = ardraw2)
model_outcome  <- lm(flourmean ~ disability_status + new.ttbpnmean + home_sat_mean, data = ardraw2)

# Estimation via quasi-Bayesian approximation
mediation_result <- mediate(
  model_mediator, 
  model_outcome, 
  sims = 500,
  treat = "disability_status",
  mediator = "home_sat_mean"
)
summary(mediation_result)
plot(mediation_result)


#Mediation package model w/ treatment variable for employment as mediator
library(mediation)
model_mediator.emp <- lm(employed ~ condis + new.ttbpnmean, data = ardraw2)
model_outcome.emp  <- lm(flourmean ~ condis + new.ttbpnmean + employed, data = ardraw2)

# Estimation via quasi-Bayesian approximation
mediation_result.emp <- mediate(
  model_mediator.emp, 
  model_outcome.emp, 
  sims = 500,
  treat = "disability_status",
  mediator = "employed"
)
summary(mediation_result.emp)
plot(mediation_result.emp)