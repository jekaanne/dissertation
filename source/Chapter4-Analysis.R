library(here) #Sets working directory
library(renv) #Package management by RStudio
library(data.table) #Smart data frames
library(tidyverse) #Packages for tidy data
library(ggpubr) #Data visualization
library(sjPlot) #Spearman correlation matrix function 
library(rstatix) #For basic statistical tests
library(psych) #For basic statistical functions
library(gvlma) #For Testing Linear Model Assumptions 
library(EnvStats) #Useful for qq plots
library(coin) #Conditional inference for categorical data analysis

# demographic tables ####
# Table 32
getTable1Stats <- function(x, digits = 0, prop_fn = describeProp, total_col_show_perc = TRUE){
  getDescriptionStatsBy(x = x, by = travel_diary$disabled)
}

t1 <- list()
t1[["Gender"]] <- getTable1Stats(travel_diary$gender)
t1[["Race/Ethnicity"]] <- getTable1Stats(travel_diary$white)
t1[["Education"]] <- getTable1Stats(travel_diary$education)
t1[["Income Range"]] <- getTable1Stats(travel_diary$income_range)
t1[["Full Time"]] <- table(travel_diary$emp_ft, travel_diary$disabled)
t1[["Part Time"]] <- table(travel_diary$emp_pt, travel_diary$disabled)
t1[["Self-Employed"]] <- table(travel_diary$emp_self, travel_diary$disabled)
t1[["Student"]] <- table(travel_diary$emp_student, travel_diary$disabled)
t1[["Unemployed"]] <- table(travel_diary$emp_unemp, travel_diary$disabled)
t1[["Retired"]] <- table(travel_diary$emp_retired, travel_diary$disabled)
t1[["Age"]] <- getTable1Stats(travel_diary$age_ranges)
mergeDesc(t1, htmlTable_args = list(caption  = "Participant demographic profiles"))

# Table 33
discolstable2 <- data.table(variables = c("Seeing", "Hearing", "Walking", "Remembering or Concentrating", "Self-Care", "Communicating", "Severe Depression", "Severe Anxiety", "Severe Pain", "Severe Fatigue"))
distype2table1<-table(travel_diary$bdis_seeing)
distype2table2<-table(travel_diary$bdis_hearing)
distype2table3<-table(travel_diary$bdis_walking)
distype2table4<-table(travel_diary$bdis_cognitive)
distype2table5<-table(travel_diary$bdis_selfcare)
distype2table6<-table(travel_diary$bdis_comm)
distype2table7<-table(travel_diary$bdepression_severity)
distype2table8<-table(travel_diary$banxiety_severity)
distype2table9<-table(travel_diary$bpain_severity)
distype2table10<-table(travel_diary$bfatigue_severity)
#join together
distype2table <- rbind(distype2table1, distype2table2, distype2table3, distype2table4, distype2table5, distype2table6, distype2table7, distype2table8, distype2table9, distype2table10)
distype2complete <- cbind(discolstable2, distype2table)
#combine variables with individual contingency tables
#print(xtable(democomplete), type="latex", comment=FALSE)
print(distype2complete) %>% 
  kable(digits = 3, format="pandoc", caption="demographics 1")

# select study variables and data exploration ####
tdvars <- dplyr::select(travel_diary, id, averagetripmood, besttripmood.m, worsttripmood.m, avg.obsperday, avg.delaysperday, avg.obsdelaysperday, disabled, disabled, trips, od_total, flourmean)

# data visualizations / tests of normality
hist(tdvars$averagetripmood) #normal
shapiro_test(tdvars$averagetripmood)
hist(tdvars$besttripmood.m) #normal
shapiro_test(tdvars$besttripmood.m)
hist(tdvars$worsttripmood.m) #normal
shapiro_test(tdvars$worsttripmood.m)
hist(travel_diary$avg.obsperday)
shapiro.test(travel_diary$avg.obsperday)
hist(tdvars$avg.obsperday)
shapiro_test(tdvars$avg.obsperday)
hist(tdvars$avg.delaysperday)
shapiro_test(tdvars$avg.delaysperday)
hist(tdvars$avg.obsdelaysperday)
shapiro_test(tdvars$avg.obsdelaysperday)

# correlation matrix #
td.correlates <- dplyr::select(travel_diary, disabled, avg.obsperday, avg.delaysperday, besttripmood.m, worsttripmood.m, ttbpnmean, altbpnfmean, flourmean, loghhincome,)
# Table 34
sjPlot::tab_corr(td.correlates, na.deletion = c("listwise"),corr.method = c("spearman"),)

# RQ1a: Do people with disabilities encounter more obstacles and delays than people without disabilities? ####
# Diff in avg. obstacles per day 
wilcox.test(avg.obsperday ~ disabled, data = tdvars,  exact = FALSE, alternative = "greater")
tdvars %>% wilcox_effsize(avg.obsperday ~ disabled)
# visualizations / descriptives of avg. obstacles per day 
dis.obs <- dplyr::select(tdvars, avg.obsperday, disabled)
group_by(dis.obs, disabled) %>% summarise(count = n(), median = median(avg.obsperday), average = mean(avg.obsperday))
dis.obs %>%ggplot( aes(x=avg.obsperday, fill=disabled)) + geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', binwidth = .25)
ggboxplot(dis.obs, x = "disabled", y = "avg.obsperday", color = "disabled", palette = c("#00AFBB", "#E7B800"), ylab = "avg obs per day", xlab = "disability status")
ggplot(dis.obs, aes(x = avg.obsperday)) + geom_histogram(aes(color = "disabled"), fill = "white", position = "identity", bins = 30) + scale_color_manual(values = c("#00AFBB", "#E7B800")) 

# Diff in avg. delays per day 
wilcox.test(avg.delaysperday ~ disabled, data = tdvars, alternative = "greater", exact = FALSE) # H1a not full supported
# alternative hypothesis (nondisabled experience greater delays)
wilcox.test(avg.delaysperday ~ disabled, data = tdvars, alternative = "less", exact = FALSE) 
tdvars %>% wilcox_effsize(avg.delaysperday ~ disabled)
group_by(dis.delays, disabled) %>% summarise(count = n(), median = median(avg.delaysperday), average = mean(avg.delaysperday))

# visualizations of avg. delays per day 
dis.delays <- dplyr::select(tdvars, avg.delaysperday, disabled)
dis.delays %>%ggplot( aes(x=avg.delaysperday, fill=disabled)) + geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', binwidth = .25)
ggboxplot(dis.delays, x = "disabled", y = "avg.delaysperday", color = "disabled", palette = c("#00AFBB", "#E7B800"), ylab = "avg delays per day", xlab = "disabled")

# RQ1b: Are there group differences in mood while traveling between people with disabilities and people without disabilities? ####
# Worst trip moods normality test
shapiro_test(tdvars$worsttripmood.m)
hist(tdvars$worsttripmood.m)
#visualizations / descriptives 
tdvars %>% group_by(disabled) %>% get_summary_stats(worsttripmood.m, type = "mean_sd")
ggboxplot( tdvars, x = "disabled", y = "worsttripmood.m",ylab = "Mood", xlab = "Disability", add = "jitter")
# t-test 
worstmood.tt <- stats::t.test(worsttripmood.m ~ disabled, data = tdvars, var.equal = FALSE, alternative = c("less"))
worstmood.tt

# Best trip moods normality test
shapiro_test(tdvars$besttripmood.m) #not normal
hist(tdvars$besttripmood.m)
#visualizations / descriptives 
tdvars %>% group_by(disabled) %>% get_summary_stats(besttripmood.m, type = c("mean_sd"))
tdvars %>% group_by(disabled) %>% get_summary_stats(besttripmood.m, type = c("full"))
ggboxplot( tdvars, x = "disabled", y = "besttripmood.m", ylab = "Mood", xlab = "Disability", add = "jitter")
# t-test 
bestmood.tt <- stats::t.test(besttripmood.m ~ disabled, data = tdvars, var.equal = FALSE, alternative = c("less"))
bestmood.tt
mood.dis <- dplyr::select(tdvars, besttripmood.m, disabled)
cohen.d(mood.dis, group = "disabled",alpha=.05,std=TRUE)
summary(cd <- cohen.d.by(mood.dis,"disabled","besttripmood.m"))

# average trip moods normality test
shapiro_test(tdvars$averagetripmood)
hist(tdvars$averagetripmood)
#visualizations / descriptives 
tdvars %>% group_by(disabled) %>% get_summary_stats(averagetripmood, type = "mean_sd")
ggboxplot( tdvars, x = "disabled", y = "averagetripmood",ylab = "Mood", xlab = "Disability", add = "jitter")
# t-test 
averagemood.tt <- stats::t.test(averagetripmood ~ disabled, data = tdvars, var.equal = FALSE, alternative = c("less"))
averagemood.tt

# RQ1c: If there are differences in moods, are they attributable to the number of obstacles or delays encountered during daily travel? ####
# diff in average mood as a result of obstacles by disability status
# Table 35
obs.mood.bydis<- function(tdvars) {
  summary(lm(averagetripmood ~ avg.obsperday, data = tdvars))
}
by(tdvars, tdvars$disabled, obs.mood.bydis)

# diff in average mood as a result of delayss by disability status
delays.mood.bydis<- function(tdvars) {
  summary(lm(averagetripmood ~ avg.delaysperday, data = tdvars))
}
by(tdvars, tdvars$disabled, delays.mood.bydis)

# RQ2: What are the relationships between mood while traveling, transportation barriers, fulfillment of psychological needs and well-being?  ####

# impact of TTBPN and ALT-BPN on average mood after controlling for avg.obs perday, disability status and interaction 
summary(ttbpnmood.obs <- lm(averagetripmood ~ ttbpnmean + altbpnfmean + avg.obsperday*disabled, data = travel_diary))
ttbpnmood.obs.resid <- residuals(ttbpnmood.obs)
shapiro.test(ttbpnmood.obs.resid) #residuals are normal after removing outlier
hist(ttbpnmood.obs.resid)
qqPlot(ttbpnmood.obs.resid)
skewness(ttbpnmood.obs.resid)
car::outlierTest(ttbpnmood.obs)
lmtest::bptest(ttbpnmood.obs)
summary(ttbpnmood.obs)
gvlma(ttbpnmood.obs)

# impact of TTBPN and ALT-BPN on average mood after controlling for avg.delays perday, disability status and interaction 
summary(ttbpnmood.delays <- lm(averagetripmood ~ ttbpnmean + altbpnfmean + avg.delaysperday*disabled, data = travel_diary))
confint(ttbpnmood.delays)
ttbpnmood.delays.resid <- residuals(ttbpnmood.delays)
shapiro.test(ttbpnmood.delays.resid) #residuals are normal after removing outlier
hist(ttbpnmood.delays.resid)
qqPlot(ttbpnmood.delays.resid)
skewness(ttbpnmood.delays.resid)
car::outlierTest(ttbpnmood.delays)
lmtest::bptest(ttbpnmood.delays)
gvlma(ttbpnmood.delays)

# Table 36 
tab_model(ttbpnmood.obs, ttbpnmood.delays, show.intercept = TRUE, show.se = TRUE, show.ci = .95, show.r2 = TRUE, show.stat = TRUE, auto.label = FALSE, show.p = .01, collapse.ci = FALSE, show.std = TRUE, show.fstat = TRUE, digits = 2, digits.p = 3,col.order = c("est", "ci", "se", "std.est",  "p"), dv.labels = c("First Model", "Second Model"), string.pred = "Predictor", string.ci = "95% CI", string.p = "p", string.est = "b", string.se = "SE", string.std = "Beta")

# differences in coping strategies  #####
# calculate number of obstacles, delays, and responses per group
n_obs_dis <- travel_diary %>%                              
  group_by(disabled) %>% summarise(n_obs_dis = sum(o_total.t, na.rm= TRUE))
n_delays_dis <- travel_diary %>%                              
  group_by(disabled) %>% summarise(n_delays_dis = sum(d_total.t, na.rm= TRUE))
n_or_active_dis <- travel_diary %>%                              
  group_by(disabled) %>% summarise(n_or_active_dis = sum(or_active.t, na.rm= TRUE))
n_or_passive_dis <- travel_diary %>%                              
  group_by(disabled) %>% summarise(n_or_passive_dis = sum(or_passive.t, na.rm= TRUE))
n_dr_active_dis <- travel_diary %>%                              
  group_by(disabled) %>% summarise(n_dr_active_dis = sum(dr_active.t, na.rm= TRUE))
n_dr_passive_dis <- travel_diary %>%                              
  group_by(disabled) %>% summarise(n_dr_passive_dis = sum(dr_passive.t, na.rm= TRUE))

# create contingency tables by combining above lists
obsdelays.number.list <- list(n_obs_dis, n_delays_dis)
obs.response.list <- list(n_or_active_dis, n_or_passive_dis)
delays.response.list <- list(n_dr_active_dis, n_dr_passive_dis)

# test for significance in differences between obs and delay counts 
obsdelays.number <- Reduce(function(d1, d2) merge(d1, d2, by = "disabled", all.x = TRUE, all.y = FALSE), obsdelays.number.list)
obsdelays.number
obdel.num <- matrix(c(29,78,52,46),ncol=2,byrow=TRUE)
colnames(obdel.num) <- c("Number of Obstacles","Number of Delays")
rownames(obdel.num) <- c("Nondisabled","Disabled")
obdel.num<- as.table(obdel.num)
obdel.num.marg <- addmargins(obdel.num, margin = 1)
obdel.num.marg
obdel.num.prop <- prop.table(obdel.num, margin=2)
obdel.num.prop
# test of proportions for obstacles and delay totals by disability status
chisq.test(obdel.num)

#plot chart
# Figure 9
par(family = "Times New Roman")
barplot(obdel.num,legend=TRUE, beside=TRUE,main='Number of obstacles and delays by disability status ', xlab = "Disability Status", ylab = "Total", args.legend = list(x ='topright', bty='n', inset=c(-.18,-.1)))


# test for significance in differences between obs response counts
# Table 37a / Figure 10
obs.responsetypes <- Reduce(function(d1, d2) merge(d1, d2, by = "disabled", all.x = TRUE, all.y = FALSE), obs.response.list)
obs.responsetypes
ob.responses <- matrix(c(11,5,28,13),ncol=2,byrow=TRUE)
colnames(ob.responses) <- c("Active Responses","Passive Responses")
rownames(ob.responses) <- c("Nondisabled","Disabled")
ob.responses<- as.table(ob.responses)
ob.responses
ob.responses.marg <- addmargins(ob.responses, margin = 2)
ob.responses.marg
ob.responses.prop <- prop.table(ob.responses, margin=1)
ob.responses.prop
# test of proportions for ob responses - total obstacles encountered by disability status
ob.responses.sig <- matrix(c(16,29,41,52),ncol=2,byrow=TRUE)
colnames(ob.responses.sig) <- c("Total Responses","Total Obstacles")
rownames(ob.responses.sig) <- c("Nondisabled","Disabled")
chisq.test(ob.responses.sig)


# Table 37b
# test of proportions for ob responses - total obstacles encountered by PWOD - 29
prop.test(x = c(11, 5), n = c(29, 29))
# plot 
par(family = "Times New Roman")
barplot(ob.responses,legend=TRUE, beside=TRUE,main='Number of response types by disability status ', xlab = "Responses to Obstacles", ylab = "Total", args.legend = list(x ='topright', bty='n', inset=c(-0.15,-0.1)))

# test for significance in differences between delays response counts
delays.responsetypes <- Reduce(function(d1, d2) merge(d1, d2, by = "disabled", all.x = TRUE, all.y = FALSE), delays.response.list)
delays.responsetypes 
delays.responses <- matrix(c(5,21,7,28),ncol=2,byrow=TRUE)
colnames(delays.responses) <- c("Active Responses","Passive Responses")
rownames(delays.responses) <- c("Nondisabled","Disabled")
delays.responses<- as.table(delays.responses)
delays.responses.marg <- addmargins(delays.responses, margin = 2)
delays.responses.marg
delays.responses.prop <- prop.table(delays.responses, margin=1)
delays.responses.prop
# test of proportions for ob responses - total obstacles encountered by disability status
delay.responses.sig <- matrix(c(26,78,35,46),ncol=2,byrow=TRUE)
colnames(delay.responses.sig) <- c("Total Responses","Total Delays")
rownames(delay.responses.sig) <- c("Nondisabled","Disabled")
chisq.test(delay.responses.sig)


# test of proportions for delay responses - total delays encountered by PWD = 46
prop.test(x = c(7, 28), n = c(46, 46))
# test of proportions for delay responses - total delays encountered by PWOD = 78
prop.test(x = c(5, 21), n = c(78, 78))

par(family = "Times New Roman")
barplot(delays.responses,legend=TRUE, beside=TRUE, xlab = "Responses to Delays", ylab = "Total", args.legend = list(x ='topright', bty='n', inset=c(-0.18,-0.5)))
title(main = list("Number of response types to obstacles and delays by disability status"), font = 1, adj = TRUE)

