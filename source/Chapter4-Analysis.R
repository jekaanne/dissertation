library(here)
library(data.table)
library(sjPlot)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(gvlma)
library(nnet)
comparison_complete <- fread("data/raw/Travel-Diary-Survey.csv", na.strings = c("",NA))

# preliminary data fixes/calculations ####
#convert discr to 5 pt. scale
comparison_complete$disc1 <- (comparison_complete$discr1*5)/6
comparison_complete$disc2 <- (comparison_complete$discr2*5)/6
comparison_complete$disc3 <- (comparison_complete$discr3*5)/6
comparison_complete$disc4 <- (comparison_complete$discr4*5)/6
comparison_complete[is.na(dis_seeing), dis_seeing:= 0]
comparison_complete[is.na(dis_hearing), dis_hearing:= 0]
comparison_complete[is.na(dis_walking), dis_walking:= 0]
comparison_complete[is.na(dis_cognitive), dis_cognitive:= 0]
comparison_complete[is.na(dis_selfcare), dis_selfcare:= 0]
comparison_complete[is.na(dis_comm), dis_comm:= 0]
comparison_complete[is.na(depression_severity), depression_severity:= 0]
comparison_complete[is.na(anxiety_severity), anxiety_severity:= 0]
comparison_complete[is.na(pain_severity), pain_severity:= 0]
comparison_complete[is.na(fatigue_severity), fatigue_severity:= 0]

# create binary disability variables
comparison_complete$bdis_seeing <- (0)
comparison_complete$bdis_seeing[comparison_complete$dis_seeing == 0] = (0)
comparison_complete$bdis_seeing[comparison_complete$dis_seeing == 1] = 1
comparison_complete$bdis_seeing[comparison_complete$dis_seeing == 2] = 1
comparison_complete$bdis_seeing[comparison_complete$dis_seeing == 3] = 1
comparison_complete$bdis_hearing <- (0)
comparison_complete$bdis_hearing[comparison_complete$dis_hearing == 0] = (0)
comparison_complete$bdis_hearing[comparison_complete$dis_hearing == 1] = 1
comparison_complete$bdis_hearing[comparison_complete$dis_hearing == 2] = 1
comparison_complete$bdis_hearing[comparison_complete$dis_hearing == 3] = 1
comparison_complete$bdis_walking <- (0)
comparison_complete$bdis_walking[comparison_complete$dis_walking == 0] = (0)
comparison_complete$bdis_walking[comparison_complete$dis_walking == 1] = 1
comparison_complete$bdis_walking[comparison_complete$dis_walking == 2] = 1
comparison_complete$bdis_walking[comparison_complete$dis_walking == 3] = 1
comparison_complete$bdis_cognitive <- (0)
comparison_complete$bdis_cognitive[comparison_complete$dis_cognitive == 0] = (0)
comparison_complete$bdis_cognitive[comparison_complete$dis_cognitive == 1] = 1
comparison_complete$bdis_cognitive[comparison_complete$dis_cognitive == 2] = 1
comparison_complete$bdis_cognitive[comparison_complete$dis_cognitive == 3] = 1
comparison_complete$bdis_comm <- (0)
comparison_complete$bdis_comm[comparison_complete$dis_comm == 0] = (0)
comparison_complete$bdis_comm[comparison_complete$dis_comm == 1] = 1
comparison_complete$bdis_comm[comparison_complete$dis_comm == 2] = 1
comparison_complete$bdis_comm[comparison_complete$dis_comm == 3] = 1
comparison_complete$bdis_selfcare <- (0)
comparison_complete$bdis_selfcare[comparison_complete$dis_selfcare == 0] = (0)
comparison_complete$bdis_selfcare[comparison_complete$dis_selfcare == 1] = 1
comparison_complete$bdis_selfcare[comparison_complete$dis_selfcare == 2] = 1
comparison_complete$bdis_selfcare[comparison_complete$dis_selfcare == 3] = 1
comparison_complete$banxiety_severity <- (0)
comparison_complete$banxiety_severity[comparison_complete$anxiety_severity == 0] = (0)
comparison_complete$banxiety_severity[comparison_complete$anxiety_severity == 1] = 1
comparison_complete$banxiety_severity[comparison_complete$anxiety_severity == 2] = 1
comparison_complete$banxiety_severity[comparison_complete$anxiety_severity == 3] = 1
comparison_complete$bdepression_severity <- (0)
comparison_complete$bdepression_severity[comparison_complete$depression_severity == 0] = (0)
comparison_complete$bdepression_severity[comparison_complete$depression_severity == 1] = 1
comparison_complete$bdepression_severity[comparison_complete$depression_severity == 2] = 1
comparison_complete$bdepression_severity[comparison_complete$depression_severity == 3] = 1
comparison_complete$bpain_severity <- (0)
comparison_complete$bpain_severity[comparison_complete$pain_severity == 0] = (0)
comparison_complete$bpain_severity[comparison_complete$pain_severity == 1] = 1
comparison_complete$bpain_severity[comparison_complete$pain_severity == 2] = 1
comparison_complete$bpain_severity[comparison_complete$pain_severity == 3] = 1
comparison_complete$bfatigue_severity <- (0)
comparison_complete$bfatigue_severity[comparison_complete$fatigue_severity == 0] = (0)
comparison_complete$bfatigue_severity[comparison_complete$fatigue_severity == 1] = 1
comparison_complete$bfatigue_severity[comparison_complete$fatigue_severity == 2] = 1
comparison_complete$bfatigue_severity[comparison_complete$fatigue_severity == 3] = 1

#conventional control variable
comparison_complete$condis <- (0)
comparison_complete$condis[comparison_complete$bdis_walking == 1] <- 1
comparison_complete$condis[comparison_complete$bdis_seeing == 1] <- 1
comparison_complete$condis[comparison_complete$bdis_hearing == 1] <- 1
comparison_complete$condis[comparison_complete$bdis_cognitive == 1] <- 1
comparison_complete$condis[comparison_complete$bdis_selfcare == 1] <- 1

# ttbpn and altbpnf mean scores
ttbpn2 <- dplyr::select(comparison_complete, aut2, aut3, aut5, aut6,  rel2, rel3, rel4, rel6, com1, com2, com3, com4)
comparison_complete$ttbpnmean <- rowMeans(ttbpn2, na.rm = TRUE)
altbpnf2 <-  dplyr::select(comparison_complete, trans_pac1, trans_pac2, trans_pac3, trans_pac4, gse1, gse2, gse3, gse4,  disc1, disc2, disc3, discr4)
comparison_complete$altbpnfmean <- rowMeans(altbpnf2, na.rm = TRUE)

# calculate loghhincome 
comparison_complete$hhincome <- as.numeric(comparison_complete$hhincome)
comparison_complete$loghhincome <- log(comparison_complete$hhincome)
comparison_complete$loghhincome[which(comparison_complete$loghhincome=="-Inf")] = NA # get rid of 1 weird case


# normalized count variables
comparison_complete$center.od_totalworst.t <- scale(comparison_complete$od_totalworst.t)

# demographic tables ####
colstable <- data.table(variables = c("Female", "Male", "Not specified", "Non-white", "White", "Advanced degree","College", "High school or less", "<5,000", "5,000 - 12,000", "12,000 - 25,000", "25,000 - 50,000", "50,000-100,000", "100,000+", "Full Time", "Part time", "Unemployed", "Missing"))
demotable1<-table(comparison_complete$gender, comparison_complete$disability_status)
demotable2<-table(comparison_complete$white, comparison_complete$disability_status)
demotable3<-table(comparison_complete$education, comparison_complete$disability_status)
demotable4<-table(comparison_complete$employed, comparison_complete$disability_status)
demotable5<-table(comparison_complete$income_range, comparison_complete$disability_status)
demotable<- data.table(rbind(demotable1, demotable2, demotable3, demotable4, demotable5))
#create proportions of rows - change margins = 2 if column proportions are preferred
demo_proportions <- prop.table(as.matrix(demotable), margin=1)*100
#combine variables with individual contingency tables
democomplete <- cbind(colstable, demotable, round(demo_proportions, 1))

print(democomplete) %>% 
  kable(digits = 3, format="pandoc", caption="demographics 1")

discolstable2 <- data.table(variables = c("Seeing", "Hearing", "Walking", "Remembering or Concentrating", "Self-Care", "Communicating", "Severe Depression", "Severe Anxiety", "Severe Pain", "Severe Fatigue"))
distype2table1<-table(comparison_complete$bdis_seeing)
distype2table2<-table(comparison_complete$bdis_hearing)
distype2table3<-table(comparison_complete$bdis_walking)
distype2table4<-table(comparison_complete$bdis_cognitive)
distype2table5<-table(comparison_complete$bdis_selfcare)
distype2table6<-table(comparison_complete$bdis_comm)
distype2table7<-table(comparison_complete$bdepression_severity)
distype2table8<-table(comparison_complete$banxiety_severity)
distype2table9<-table(comparison_complete$bpain_severity)
distype2table10<-table(comparison_complete$bfatigue_severity)
#join together
distype2table <- rbind(distype2table1, distype2table2, distype2table3, distype2table4, distype2table5, distype2table6, distype2table7, distype2table8, distype2table9, distype2table10)
distype2complete <- cbind(discolstable2, distype2table)
#combine variables with individual contingency tables
#print(xtable(democomplete), type="latex", comment=FALSE)
print(distype2complete) %>% 
  kable(digits = 3, format="pandoc", caption="demographics 1")

# correlation matrix ####
cc.correlates <- dplyr::select(comparison_complete, disability_status, log.hhincome, besttripmood.m, worsttripmood.m, ttbpnmean, altbpnfmean, avg.obsperday, flourmean)
cc.correlates$disability_status <- as.numeric(cc.correlates$disability_status)

#plot correlation table
cc.correlates <- data.table(cc.correlates)
sjp.corr(cc.correlates)
sjt.corr(cc.correlates)

# RQ1: How does mood while traveling correlate with more stable measures of well-being?####
summary(moodflour <- lm(flourmean ~ besttripmood.m + worsttripmood.m, data = comparison_complete, subset= -24))
moodflour.resid <- residuals(moodflour)
shapiro.test(moodflour.resid) #residuals are normal after removing outlier
hist(moodflour.resid)
qqPlot(moodflour.resid)
skewness(moodflour.resid)
car::outlierTest(moodflour)
lmtest::bptest(moodflour)
summary(moodflour)
gvlma(moodflour)
vif(moodflour)

# testing reciprocal relationship 
summary(flourmood <- lm(flourmean ~ besttripmood.m, data = comparison_complete))
flourmood.resid <- residuals(flourmood)
shapiro.test(flourmood.resid) #residuals are normal after removing outlier
hist(flourmood.resid)
qqPlot(flourmood.resid)
skewness(flourmood.resid)
car::outlierTest(flourmood)
lmtest::bptest(flourmood)
summary(flourmood)
library(gvlma)
gvlma(flourmood)

summary(lm(worsttripmood.m ~disability_status, data = comparison_complete))
summary(lm(besttripmood.m ~disability_status, data = comparison_complete))

# RQ2: Do obstacles and delays during daily travel impact mood while traveling?####
summary(moodobstacles <- lm(averagetripmood ~ od_total, data = comparison_complete))
moodobstacles.resid <- residuals(moodobstacles)
shapiro.test(moodobstacles.resid) #residuals are normal after removing outlier
hist(moodobstacles.resid)
qqPlot(moodobstacles.resid)
skewness(moodobstacles.resid)
# test of heteroscedasticity (it's not!!)
lmtest::bptest(moodobstacles)
library(gvlma)
gvlma(moodobstacles)
vif(moodobstacles)

# by best
summary(bestmoodobstacles <- lm(besttripmood.m ~ od_totalbest.t + log.hhincome, data = comparison_complete, subset = - 52))
bestmoodobstacles.resid <- residuals(bestmoodobstacles)
plot(comparison_complete$besttripmood.m ~ comparison_complete$od_totalbest.t)
abline(lm(comparison_complete$besttripmood.m ~ comparison_complete$od_totalbest.t))
shapiro.test(bestmoodobstacles.resid) #residuals are normal after removing outlier
hist(bestmoodobstacles.resid)
qqPlot(bestmoodobstacles.resid)
skewness(bestmoodobstacles.resid)
# test of heteroscedasticity (it's not!!)
lmtest::bptest(bestmoodobstacles)
library(gvlma)
gvlma(bestmoodobstacles)
vif(bestmoodobstacles)

# by worst
worstmoodobstacles <- lm(worsttripmood.m ~ od_totalworst.t + log.hhincome, data = comparison_complete, subset = -c(60, 72))
summary(worstmoodobstacles)
confint(worstmoodobstacles)
worstmoodobstacles.resid <- residuals(worstmoodobstacles)
shapiro.test(worstmoodobstacles.resid)
hist(worstmoodobstacles.resid)
qqPlot(worstmoodobstacles.resid)
plot(comparison_complete$worsttripmood.m ~ comparison_complete$od_totalworst.t)
abline(lm(comparison_complete$worsttripmood.m ~ comparison_complete$od_totalworst.t))
skewness(worstmoodobstacles.resid)
# test of heteroscedasticity (it's not!!)
lmtest::bptest(worstmoodobstacles)
library(gvlma)
gvlma(worstmoodobstacles)
vif(worstmoodobstacles)

# RQ3a: Are there group differences in number of obstacles/mood while traveling between people with disabilities and people without disabilities?####
library(lattice)
histogram( ~ averagetripmood | disability_status, data = comparison_complete, layout = c(1,2), col = "orange", xlab = "mood")
# theres a difference in the distributions which may be saying something. 

#average obstacles per day
comparison_complete$center.avg.obsperday <- scale(comparison_complete$avg.obsperday)
hist(comparison_complete$avg.obsperday)
shapiro.test(comparison_complete$center.avg.obsperday)
comparison_complete$log.avg.obsperday[comparison_complete$log.avg.obsperday =="-Inf"] <-NA
comparison_complete$avg.obsperday[comparison_complete$avg.obsperday =="NA"] = 0
comparison_complete$avg.obsperday[is.na(comparison_complete$avg.obsperday)] <- (0)

# data are not normal
hist(comparison_complete$avg.obsperday)
shapiro.test(comparison_complete$avg.obsperday)

#variance vs. mean of total obstacles (doesn't meet poisson assumptions)
var(comparison_complete$od_total) 
mean(comparison_complete$od_total)
#variance vs. mean of total reported trips
var(comparison_complete$bestworst_trip.count.t) 
mean(comparison_complete$bestworst_trip.count.t)
#plots
ggplot(comparison_complete, aes(o_total.t, fill = disability_status)) +
  geom_histogram() +
  scale_x_log10() +
  facet_grid(disability_status ~ ., margins=TRUE, scales="free_y")



#numeric dis var
comparison_complete$n.disstatus <- as.numeric(comparison_complete$disability_status)
# trying out zero-inflated negative binomial regression cuz my variance is greater than mah mean
summary(zinb.model <- zeroinfl(o_total.t ~ log.hhincome | disability_status + bestworst_trip.count.t, data = comparison_complete, dist = "negbin", EM = TRUE))
library(rstatix)

# diff in avg. obstacles per day (almost significant, p = .05005
wlk.obs <- wilcox.test(avg.obsperday ~ disability_status, data = comparison_complete, alternative = "two.sided", na.action = na.omit, exact = FALSE)
add_significance(wlk.obs)
wlk.obs
comparison_complete %>% wilcox_effsize(avg.obsperday ~ disability_status)

#regression tests
obs.dis <- lm(avg.obsperday ~ disability_status, data = comparison_complete)
obs.dis.resid <- residuals(obs.dis)
shapiro.test(obs.dis.resid)
hist(obs.dis.resid)
qqPlot(obs.dis.resid)
skewness(obs.dis.resid)
# test of heteroscedasticity (it's not!!)
gvlma(obs.dis)
lmtest::bptest(obs.dis)
summary(obs.dis.resid)

# obstacles only
delay.dis <- lm(avg.delaysperday ~ disability_status, data = comparison_complete)
delay.dis.resid <- residuals(delay.dis)
shapiro.test(delay.dis.resid)
hist(delay.dis.resid)
qqPlot(delay.dis.resid)
skewness(delay.dis.resid)
# test of heteroscedasticity 
gvlma(delay.dis)
lmtest::bptest(delay.dis)
summary(delay.dis.resid)

# best moods
bestmood.dis <- lm(besttripmood.m ~ disability_status, data = comparison_complete)
bestmood.dis.resid <- residuals(bestmood.dis)
shapiro.test(bestmood.dis.resid)
hist(bestmood.dis.resid)
qqPlot(bestmood.dis.resid)
skewness(bestmood.dis.resid)
# test of heteroscedasticity 
gvlma(bestmood.dis)
lmtest::bptest(bestmood.dis)
summary(bestmood.dis.resid)

# worst moods
worstmood.dis <- lm(worsttripmood.m ~ disability_status, data = comparison_complete)
worstmood.dis.resid <- residuals(worstmood.dis)
shapiro.test(worstmood.dis.resid)
hist(worstmood.dis.resid)
qqPlot(worstmood.dis.resid)
skewness(worstmood.dis.resid)
# test of heteroscedasticity 
gvlma(worstmood.dis)
lmtest::bptest(worstmood.dis)
summary(worstmood.dis.resid)



# diff in avg. delays per day (significant, p = .03)
wlk.delays <- wilcox.test(avg.delaysperday ~ disability_status, data = comparison_complete, alternative = "two.sided", na.action = na.omit, exact = FALSE)
add_significance(wlk.obs)
wlk.delays
comparison_complete %>% wilcox_effsize(avg.delaysperday ~ disability_status)

# diff in avg. obstacles AND delays per day (not significant, p = .43)
hist(comparison_complete$avg.obsdelaysperday)
shapiro.test(comparison_complete$avg.obsdelaysperday) #not normal at all
wlk.obsdelays <- wilcox.test(avg.obsdelaysperday ~ disability_status, data = comparison_complete, alternative = "two.sided", na.action = na.omit, exact = FALSE)
add_significance(wlk.obs)
wlk.obsdelays
comparison_complete %>% wilcox_effsize(avg.obsdelaysperday ~ disability_status)

#kruskall wallis to 
ggboxplot(simpledatfortt, x = "disability_status", y = "avg.obsdelaysperday")
wilcox_test(avg.obsdelaysperday ~ disability_status, data = simpledatfortt, alternative = "two.sided", exact = FALSE)

summary(dispersion.compact <- glm.nb(od_total ~ log.hhincome + disability_status + bestworst_trip.count.t, data=comparison_complete))
dispersion.compact$coefficients
# vuong/nonest test to compare models
icci(dispersion.compact, zinb.model)
var(comparison_complete$o_total.t)
mean(comparison_complete$o_total.t)
# group comparison test
obstacledislm <- function(comparison_complete) {
  summary(dispersion.compact <- glm.nb(od_total ~ log.hhincome + disability_status + bestworst_trip.count.t, data=comparison_complete))
}
by(comparison_complete, comparison_complete$disability_status, dispersion.compact)


#model to test zero-inflation/dispersion w/ full dataset (diary complete cases)
summary(zinb.model.full <- zeroinfl(od_total ~ log.hhincome | disability_status +   bestworst_trip.count, data = diary_complete_cases, dist = "negbin", EM = TRUE, na.action = na.omit))
summary(dispersion.compact.full <- glm.nb(od_total ~ disability_status + bestworst_trip.count + log.hhincome, data=diary_complete_cases))
dispersion.compact.full$coefficients

ggplot(data = diary_complete_cases, aes(x = od_total, y = bestworst_trip.count.t)) + 
  geom_jitter(width = 0.05, height = 0.05) + 
  geom_smooth(method = 'glm', method.args = list(family = 'poisson'))
# vuong test to compare models (full non-zero binomial model has best fit)
icci(dispersion.compact.full, zinb.model.full)
var(diary_complete_cases$o_total.t)
mean(diary_complete_cases$o_total.t)





#
disavgmood <- filter(comparison_complete, disability_status == "Disabled")
summary(disavgmood.model <- lm(averagetripmood ~ od_total + log.hhincome, data = disavgmood))

nondisavgmood <- filter(comparison_complete, disability_status == "Nondisabled")
summary(nondisavgmood.model <- lm(averagetripmood ~ od_total + log.hhincome, data = nondisavgmood))

comparison_complete$sum

lmList(averagetripmood ~ od_total | disability_status, data=comparison_complete, na.action=na.omit)
summary(obstacledis <- glm(avg.obs.perday ~ disability_status + log.hhincome, data=diary_complete_cases, na.action=na.omit))

wilk.obs <- wilcox_test(avg.obsperday ~ disability_status, data = comparison_complete)
wilk.obs
wilcox_effsize(avg.obsperday ~ disability_status, data = comparison_complete)

wilk.mood <- wilcox_test(besttripmood.m ~ disability_status, data = comparison_complete)
wilk.mood
wilcox_effsize(besttripmood.m ~ disability_status, data = comparison_complete)

worst.dis.tt <- t.test(worsttripmood.m ~ disability_status, data = comparison_complete, var.equal = FALSE)
worst.dis.tt

best.dis.tt <- t.test(besttripmood.m ~ disability_status, data = comparison_complete, var.equal = FALSE)
best.dis.tt

avgmood.dis.tt <- t.test(averagetripmood ~ disability_status, data = comparison_complete, var.equal = FALSE)
avgmood.dis.tt


library(ggpubr)
library(dplyr)

#mean and SD for groups
avgtripmood.dis <- select(comparison_complete, averagetripmood, disability_status)
group_by(avgtripmood.dis, disability_status) %>%
  summarise(
    count = n(),
    mean = mean(averagetripmood, na.rm = TRUE),
    sd = sd(averagetripmood, na.rm = TRUE)
  )
#plot groups
library("ggpubr")
ggboxplot(avgtripmood.dis, x = "disability_status", y = "averagetripmood", 
          color = "disability_status", palette = c("#00AFBB", "#E7B800"),
          ylab = "Avg. Trip Mood", xlab = "disability status")
# Shapiro-Wilk normality test for disabled group (normal)
with(avgtripmood.dis, shapiro.test(averagetripmood[disability_status == "Disabled"]))
# Shapiro-Wilk normality test for nondisabled group (normal)
with(avgtripmood.dis, shapiro.test(averagetripmood[disability_status == "Nondisabled"])) 
#check for homogeneity of variance (no significant difference)
avgtripmood.dis.ftest <- var.test(trips, disability_status, data = avgtripmood.dis)
avgtripmood.dis.ftest 
# calculate unpaired/welch's t-test
avgtripmood.dis.tt <- t.test(averagetripmood ~ disability_status, data = comparison_complete, var.equal = FALSE)
avgtripmood.dis.tt

    
#mood overall and best/worst
mooddistest <- t.test(comparison_complete$averagetripmood ~ comparison_complete$disability_status, var.equal = FALSE)
mooddistest$statistic
worstmooddistest <- t.test(comparison_complete$worsttripmood.m ~ comparison_complete$disability_status, var.equal = FALSE)
worstmooddistest
bestmooddistest <- t.test(comparison_complete$besttripmood.m ~ comparison_complete$disability_status, var.equal = FALSE)
bestmooddistest



# RQ3b: Are differences attributable to the number of obstacles and delays encountered during daily travel? ####



# do pwd experience more obstacles and delays?
summary(disability_od <- lm(o_total ~ disability_status, data = comparison_complete))
disability_od.resid <- residuals(disability_od)
shapiro.test(disability_od.resid) #residuals are normal after removing outlier
hist(disability_od.resid)
qqPlot(disability_od.resid)
skewness(disability_od.resid)
car::outlierTest(disability_od)
lmtest::bptest(moodflour)
summary(moodflour)
library(gvlma)
gvlma(disability_od)
vif(disability_od)

# when only testing for diff in number of obstacles####
summary(obstacledislm <- glm(b.od_total ~ factor(disability_status), data = comparison_complete, family = "binomial", na.action = na.omit))


# diff in average mood as a result of obstacles
obstaclemooddislm <- function(comparison_complete) {
summary(lm(averagetripmood ~ avg.obsperday, data = comparison_complete))
}
by(comparison_complete, comparison_complete$disability_status, obstaclemooddislm)
#subset data
dis.obsmood <- subset(comparison_complete, disability_status == "Disabled")
nondis.obsmood <- subset(comparison_complete, disability_status == "Nondisabled")
# disability group
corr.test(dis.obsmood$averagetripmood, dis.obsmood$avg.obsperday, use = "pairwise",method="pearson", alpha=.05,ci=TRUE,minlength=5)
summary(disobsm.model <- lm(averagetripmood ~ avg.obsperday, data = dis.obsmood))
disobsm.resid <- resid(disobsm.model)
shapiro.test(disobsm.resid ) #residuals are normal after removing outlier
hist(obsmood.resid)
library(EnvStats)
qqPlot(obsmood.resid)
car::outlierTest(disobsm.model)
rstandard(disobsm.model)[abs(rstandard(disobsm.model)) > 2]
sum(hatvalues(disobsm.model) > 2 * mean(hatvalues(disobsm.model)))
sum(abs(rstandard(disobsm.model)) > 2)
coef(summary(disobsm.model))
skewness(obsmood.resid)
# test of heteroscedasticity (it's not!!)
lmtest::bptest(disobsm.model)
library(gvlma)
gvlma(disobsm.model)


# nondisabled group #
summary(nondisobsm.model <- lm(averagetripmood ~ avg.obsperday, data = nondis.obsmood))
corr.test(nondis.obsmood$averagetripmood, nondis.obsmood$avg.obsperday, use = "pairwise",method="pearson", alpha=.05,ci=TRUE,minlength=5)
nondisobsm.resid <- resid(nondisobsm.model)
shapiro.test(nondisobsm.resid ) #residuals are normal after removing outlier
hist(obsmood.resid)
library(EnvStats)
qqPlot(obsmood.resid)
car::outlierTest(nondisobsm.model)
rstandard(disobsm.model)[abs(rstandard(nondisobsm.model)) > 2]
sum(hatvalues(nondisobsm.model) > 2 * mean(hatvalues(nondisobsm.model)))
sum(abs(rstandard(nondisobsm.model)) > 2)
skewness(nondisobsm.resid)
# test of heteroscedasticity (it's not!!)
lmtest::bptest(nondisobsm.model)
library(gvlma)
gvlma(nondisobsm.model)
library(VIF)
vif(nondisobsm.model)

# standard lm - no relationship between avg.obsperday and avgmood and disabled
summary(glm(averagetripmood ~ avg.obsperday + avg.obsperday:disability_status, data = comparison_complete))





#t.test
simpledatfortt <- dplyr::select(comparison_complete, averagetripmood, email, disability_status, od_total, o_total.t, d_total.t, besttripmood.m, worsttripmood.m, trips, avg.obsperday, avg.delaysperday, avg.obsdelaysperday)
#remove one case w/ no mood data at all
simpledatfortt<- simpledatfortt[-c(3),]
simpledatfortt <- simpledatfortt  %>%
  mutate(condis = recode_factor(condis,
                               `1` = "Disabled",
                               `0` = "Nondisabled"))

#data vizzes
bxp <- ggboxplot(
  simpledatfortt$worsttripmood.m, width = 0.5, add = c("mean", "jitter"), 
  ylab = "Weight (g)", xlab = FALSE
)
bxp2

shapiro_test(simpledatfortt$averagetripmood)
#summary stats for each group
simpledatfortt %>%
  group_by(disability_status) %>%
  get_summary_stats(averagetripmood, type = "mean_sd")
bxp2 <- ggboxplot(simpledatfortt, x = "disability_status", y = "avg.obsperday", ylab = "Average Obstacles per Day", xlab = "Disability Status", add = "jitter", combine = TRUE)
bxp2

moodoutliers <- boxplot.stats(simpledatfortt$averagetripmood)
moodoutliers$stats
moodoutliers$stats[1]
moodoutliers$stats[5]
#outlier boxplot
moodoutlier_vis <- simpledatfortt %>% 
  filter(!is.na(disability_status) & !is.na(averagetripmood)) %>% 
  ggboxplot(x = "disability_status", y = "averagetripmood",
            palette = "aaas", 
            title = "average trip mood by group",
            xlab = "disability",
            ylab = "overall average mood",
            ggtheme = theme_gray())
moodoutlier_vis

# replace outliers w/ nearest lower value
comparison_complete$averagetripmood[comparison_complete$averagetripmood < 2.428571] <- (2.428571)


#look for group outliers
library(rstatix)
simpledatfortt <-tibble(simpledatfortt)
simpledatfortt %>%
  group_by(disability_status) %>%
  identify_outliers(od_total, )

#normality by groups
simpledatfortt %>%
  group_by(disability_status) %>%
  shapiro_test(averagetripmood)
# Draw a qq plot by group
ggqqplot(simpledatfortt, x = "averagetripmood", facet.by = "disability_status")
# check for homogeneity of variances 
simpledatfortt %>% levene_test(averagetripmood ~ disability_status)
avgmood.dis.test <- simpledatfortt %>% 
  t_test(averagetripmood ~ disability_status, var.equal = FALSE) %>%
  add_significance()
avgmood.dis.test

# best mood # worst mood are.not.normal
shapiro_test(simpledatfortt$worsttripmood.m)
hist(simpledatfortt$worsttripmood.m)
simpledatfortt %>%
  group_by(disability_status) %>%
  get_summary_stats(worsttripmood.m, type = "mean_sd")
bxp2 <- ggboxplot(
  simpledatfortt, x = "disability_status", y = "worsttripmood.m", 
  ylab = "Mood", xlab = "Disability", add = "jitter"
)
bxp2

# Draw a qq plot by group
ggqqplot(simpledatfortt, x = "worsttripmood.m", facet.by = "disability_status")
# check for homogeneity of variances 
simpledatfortt %>% levene_test(worsttripmood.m ~ disability_status)

worstmood.dis.test <- simpledatfortt %>% 
  t_test(worsttripmood.m ~ disability_status) %>%
  add_significance()
worstmood.dis.test
summary(wilcox.test(besttripmood.m ~ disability_status, data=simpledatfortt))

# 
ggplot(comparison_complete, aes(y = log.hhincome, x = new.ttbpnmean, color = condis)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)


#########################


#RQ4 - differences in coping strategies? ####
copediff <- function(comparison_complete) {
  summary(lm(r_passive.avg ~ avg.obsperday, data = comparison_complete))
}
by(comparison_complete, comparison_complete$disability_status, copediff)





comparison_complete$probsolv <- comparison_complete$r_probsolvbest.t + comparison_complete$r_probsolvworst.t
comparison_complete$passive <- comparison_complete$r_passivebest.t + comparison_complete$r_passiveworst.t
comparison_complete$helpseeking <- comparison_complete$r_helpseekingbest.t + comparison_complete$r_helpseekingworst.t
comparison_complete$emoreg <- comparison_complete$r_emoregbest.t + comparison_complete$r_emoregworst.t

passive_dis <- glm(r_passive.t ~ disability_status + od_total, data = comparison_complete, family = poisson(link = "log"), na.action = na.exclude)
summary(passive_dis)

active_dis <- zeroinfl(r_active.t ~  avg.obsperday | disability_status + bestworst_trip.count.t, data = comparison_complete, dist = "negbin", EM = TRUE, na.action = na.omit)

var(comparison_complete$r_active.t)
mean(comparison_complete$r_active.t)

summary(active_dis)
active_dis.fit <- fit


summary(active_dis.fit)
# average per person per group 
n_passive_dis <- diary_trips_only %>%                              
  group_by(disability_status) %>% summarise(n_passive_dis = sum(r_passive, na.rm= TRUE))
n_passive_dis

n_obs_dis <- diary_trips_only %>%                              
  group_by(disability_status) %>% summarise(n_obs_dis = sum(od_total, na.rm= TRUE))
n_obs_dis    

n_active_dis <- diary_trips_only %>%                              
  group_by(disability_status) %>% summarise(n_active_dis = sum(r_active, na.rm= TRUE))
n_active_dis      

response.list <- list(n_obs_dis, n_active_dis, n_passive_dis)
responsetypes <- Reduce(function(d1, d2) merge(d1, d2, by = "disability_status", all.x = TRUE, all.y = FALSE), response.list)
responsetypes 

responses <- matrix(c(96,18,26,73,40,41),ncol=3,byrow=TRUE)
colnames(responses) <- c("Obs_Delays","Active","Passive")
rownames(responses) <- c("Nondisabled","Disabled")
responses <- as.table(responses)
responses

coping <- matrix(c(18,26,40,41),ncol=2,byrow=TRUE)
colnames(coping) <- c("Active","Passive")
rownames(responses) <- c("Nondisabled","Disabled")
responses <- as.table(responses)
responses

barplot(responses,legend=T,beside=T)
plot(responses)

margin.table(responses)
margin.table(responses,1)
margin.table(responses,2)
prop.table(responses)
prop.table(responses,1)
prop.table(responses,2)
summary(responses)
library(mosaic)
mosaicplot(~ disability_status + n_obs_dis + n_active_dis + n_passive_dis, data = responsetypes, main = "Survival on the Titanic", shade = TRUE, legend = TRUE)
library(vcd)
assoc(responses, shade=TRUE)
mosaic(responses)

CrossTable(responsetypes$n_obs_dis,responsetypes$n_active_dis, responsetypes$n_passive_dis, responsetypes$disability_status, prop.c = FALSE, prop.t = FALSE,  chisq  = TRUE, prop.chisq = FALSE)


prop.test(table(responsetypes$n_active_dis, responsetypes$disability_status), correct=FALSE, alternative = "greater")
prop.test(table(responsetypes$n_passive_dis, responsetypes$disability_status), correct=FALSE, alternative = "greater")
prop.test(table(responsetypes$n_obs_dis, responsetypes$disability_status), correct=FALSE, alternative = "greater")

comparison_complete$r_active.avg <- comparison_complete$r_active.t/comparison_complete$od_total
comparison_complete$r_active.avg[comparison_complete$r_active.avg=="NaN"]<-NA
comparison_complete$r_active.avg[comparison_complete$r_active.avg=="Inf"]<-NA
comparison_complete$r_passive.avg <- comparison_complete$r_passive.t/comparison_complete$od_total
comparison_complete$r_passive.avg[comparison_complete$r_passive.avg=="NaN"]<-NA
comparison_complete$r_passive.avg[comparison_complete$r_passive.avg=="Inf"]<-NA


summary(lm(r_passive.avg ~ disability_status + trips, data = comparison_complete))


library(gmodels)
CrossTable(diary_trips_only$n_active_dis, responsetypes$disability_status, prop.t=FALSE, prop.r=FALSE, prop.c=FALSE, prop.chisq = FALSE, chisq=TRUE, digits = 2, fisher = TRUE )

### other 
library(mnlogit)
diary_complete_cases$b.od_total[diary_complete_cases$od_total>0]=1
diary_complete_cases$b.od_total[diary_complete_cases$od_total==0]=0
diary_complete_cases$b.od_best[diary_complete_cases$od_total_best>0]=1
diary_complete_cases$b.od_best[diary_complete_cases$od_total_best==0]=0
diary_complete_cases$b.od_worst[diary_complete_cases$od_total_worst>0]=1
diary_complete_cases$b.od_worst[diary_complete_cases$od_total_worst==0]=0


comparison_complete$b.od_total[comparison_complete$od_total>0]=1
comparison_complete$b.od_total[comparison_complete$od_total==0]=0
comparison_complete$b.od_best[comparison_complete$od_total_best>0]=1
comparison_complete$b.od_best[comparison_complete$od_total_best==0]=0
comparison_complete$b.od_worst[comparison_complete$od_total_worst>0]=1
comparison_complete$b.od_worst[comparison_complete$od_total_worst==0]=0

#reduce data to trips only
diary_trips_only <- na.omit(diary_complete_cases, cols=c("disability_status", "averagetripmood", "od_total"))
diary_trips_only$avgmoodbinary[diary_trips_only$averagetripmood >3]=2
diary_trips_only$avgmoodbinary[diary_trips_only$averagetripmood ==3]=1
diary_trips_only$avgmoodbinary[diary_trips_only$averagetripmood <3]=0
diary_trips_only$bestmoodbinary[diary_trips_only$best_mood >3]=2
diary_trips_only$bestmoodbinary[diary_trips_only$best_mood ==3]=1
diary_trips_only$bestmoodbinary[diary_trips_only$best_mood <3]=0
diary_trips_only$worstmoodbinary[diary_trips_only$worst_mood >3]=2
diary_trips_only$worstmoodbinary[diary_trips_only$worst_mood ==3]=1
diary_trips_only$worstmoodbinary[diary_trips_only$worst_mood <3]=0


#make disabled and nondisabled dummies
diary_trips_only$disabled[diary_trips_only$disability_status==1]=1
diary_trips_only$disabled[diary_trips_only$disability_status==0]=0
diary_trips_only$nondisabled[diary_trips_only$disability_status==0]=1
diary_trips_only$nondisabled[diary_trips_only$disability_status==1]=0

library(mlogit)
with(diary_trips_only, table(b.od_total, avgmoodbinary))
with(diary_trips_only, table(b.od_best, bestmoodbinary))
with(diary_trips_only, table(b.od_worst, worstmoodbinary))
#convert outcomes to factors
diary_trips_only <- diary_trips_only %>%
  mutate(b.od_total = recode_factor(b.od_total,
                               `1` = "Obstacles or delays",
                               `0` = "No obstacles or delays"))
diary_trips_only <- diary_trips_only %>%
  mutate(avgmoodbinary = recode_factor(avgmoodbinary,
                                    `2` = "Happy",
                                    `1` = "Neither_Happy_or_Unhappy",
                                    `0` = "Unhappy"))

#make these nests into character vectors
diary_trips_only$disabled <- as.character(diary_trips_only$disabled)
diary_trips_only$nondisabled <- as.character(diary_trips_only$nondisabled)
library(mlogit)
library(mnlogit)
mnl.model <- formula(avgmoodbinary ~ 1|b.od_total + disability_status)
attach(shaped.data)
summary(mnl.fit <- mnlogit(mnl.model, shaped.data, choiceVar = NULL, maxiter = 50, ftol = 1e-6, gtol= 1e-6, weights = NULL, ncores = 1, na.rm = TRUE, print.level = 0))
print(mnl.fit$model.size)
lrtest(mnl.fit)

comparison_complete$besttripmood.m[comparison_complete$besttripmood.m =="NaN"]<- NA

comparison_complete$avgmoodbinary[comparison_complete$averagetripmood <3]=0
comparison_complete$avgmoodbinary[comparison_complete$averagetripmood ==3]=1
comparison_complete$avgmoodbinary[comparison_complete$averagetripmood >3]=2
comparison_complete$bestmoodbinary[comparison_complete$besttripmood.m <3]=0
comparison_complete$bestmoodbinary[comparison_complete$besttripmood.m =="NA"]=1
comparison_complete$bestmoodbinary[comparison_complete$besttripmood.m >3]=2
comparison_complete$worstmoodbinary[comparison_complete$worsttripmood.m <3]=0
comparison_complete$worstmoodbinary[comparison_complete$worsttripmood.m ==3]=1
comparison_complete$worstmoodbinary[comparison_complete$worsttripmood.m >3]=2
comparison_complete$bestmoodbinary <- as.factor(comparison_complete$bestmoodbinary)

comparison_complete$odbinary[comparison_complete$od_total <1]=0
comparison_complete$odbinary[comparison_complete$od_total>0]=1

diary_trips_only$odbinary[diary_trips_only$od_total <1]=0
diary_trips_only$odbinary[diary_trips_only$od_total>0]=1

logitmodel.obs <- glm(od_total ~ disabled + log.hhincome, data=diary_trips_only, na.action = na.omit, family = "poisson")
summary(logitmodel.obs)
logit.nnet.coef<- coef(logitmodel.obs)
# convert to odds ratio / percentages for written interp
(exp(logit.nnet.coef)-1)*100
logit.zvalues <- summary(logitmodel.obs)$coefficients / summary(logitmodel.obs)$standard.errors
pnorm(abs(logit.zvalues), lower.tail=FALSE)*2
nnet.model <- multinom(avgmoodbinary ~ od_total + log.hhincome + disability_status , data=diary_trips_only, na.action = na.omit)
summary(nnet.model)
nnet.coef<- coef(nnet.model)
# convert to odds ratio / percentages for written interp
(exp(nnet.coef)-1)*100
zvalues <- summary(nnet.model)$coefficients / summary(nnet.model)$standard.errors
pnorm(abs(zvalues), lower.tail=FALSE)*2

# best trips only
best.nnet.model <- multinom(bestmoodbinary ~ b.od_best + disability_status + log.hhincome, data=diary_trips_only)
summary(best.nnet.model)
best.nnet.coef<- coef(best.nnet.model)
# convert to odds ratio / percentages for written interp
(exp(best.nnet.coef)-1)*100
best.zvalues <- summary(best.nnet.model)$coefficients / summary(best.nnet.model)$standard.errors
pnorm(abs(best.zvalues), lower.tail=FALSE)*2

#worst trips only
worst.nnet.model <- multinom(worstmoodbinary ~ b.od_worst + disability_status + log.hhincome, data=diary_trips_only, na.action = na.omit)
summary(worst.nnet.model)
worst.nnet.coef<- coef(worst.nnet.model)
# convert to odds ratio / percentages for written interp
(exp(worst.nnet.coef)-1)*100
worst.zvalues <- summary(worst.nnet.model)$coefficients / summary(worst.nnet.model)$standard.errors
pnorm(abs(worst.zvalues), lower.tail=FALSE)*2

# making this numeric so I can interpret
diary_trips_only$b.od_total <- as.numeric(diary_trips_only$b.od_total)
diary_trips_only$b.od_total[diary_trips_only$b.od_total ==2]=0

diary.shaped.data <- mlogit.data(diary_trips_only, choice = "odbinary", shape = "wide", id.var = "email", group.var = "disability_status")


shaped.data <- mlogit.data(diary_trips_only, choice = "avgmoodbinary", shape = "wide", id.var = "email", group.var = "disability_status")

MLmodel <- mlogit(avgmoodbinary ~ 1|  b.od_total + log.hhincome, data = shaped.data, reflevel ="Happy", alt.levels = c("Happy", "Neither Happy or Unhappy", "Unhappy"))
summary(MLmodel)

MLmodel2 <- mlogit(avgmoodbinary~1 | b.od_total+log.hhincome, data = shaped.data,
       alt.var='disability_status', reflevel ="Happy", alt.levels = c("Happy", "Neither Happy or Unhappy", "Unhappy"), nests=list(disabled=c('Disabled'), nondisabled=c('Nondisabled')))

logsum(
  MLmodel,
  X = NULL,
  formula = NULL,
  data = NULL,
  type = NULL,
  output = c("chid", "obs")
)

mlogit(avgmoodbinary ~ 1| b.od_total,
  shaped.data,
  start = NULL,
  alt.subset = NULL,
  nests=list(disabled=c('disabled'), nondisabled=c('nondisabled')),
  un.nest.el = FALSE,
  unscaled = FALSE,
  heterosc = FALSE,
  rpar = NULL,
  probit = FALSE,
  R = 40,
  correlation = FALSE,
  halton = NULL,
  random.nb = NULL,
  panel = FALSE,
  estimate = TRUE,
)

# Incorporating TTBPN scale
summary(ttbpnmood <- lm(averagetripmood ~ ttbpnmean + altbpnfmean + avg.obsperday + condis + loghhincome, data = comparison_complete))
