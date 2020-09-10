library(here) #Sets working directory
library(renv) #Package management by RStudio
library(data.table) #Smart data frames
library(tidyverse) #Packages for tidy data
library(ggpubr)
library(rstatix)
library(gvlma)
library(nnet)
library(EnvStats)
library(VIF)
library(coin)
library(psych)
library(sjPlot)
# demographic tables ####
getTable1Stats <- function(x, digits = 0, prop_fn = describeProp, total_col_show_perc = TRUE){
  getDescriptionStatsBy(x = x, by = travel_diary$disability_status)
}

t1 <- list()
t1[["Gender"]] <- getTable1Stats(travel_diary$gender)
t1[["Race/Ethnicity"]] <- getTable1Stats(travel_diary$white)
t1[["Education"]] <- getTable1Stats(travel_diary$education)
t1[["Income Range"]] <- getTable1Stats(travel_diary$income_range)
t1[["Full Time"]] <- table(travel_diary$emp_ft, travel_diary$disability_status)
t1[["Part Time"]] <- table(travel_diary$emp_pt, travel_diary$disability_status)
t1[["Self-Employed"]] <- table(travel_diary$emp_self, travel_diary$disability_status)
t1[["Student"]] <- table(travel_diary$emp_student, travel_diary$disability_status)
t1[["Unemployed"]] <- table(travel_diary$emp_unemp, travel_diary$disability_status)
t1[["Retired"]] <- table(travel_diary$emp_retired, travel_diary$disability_status)
mergeDesc(t1, htmlTable_args = list(caption  = "Participant demographic profiles"))


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
tdvars <- dplyr::select(travel_diary, id, averagetripmood, besttripmood.m, worsttripmood.m, avg.obsperday, avg.delaysperday, avg.obsdelaysperday, disability_status, disabled, trips, od_total, flourmean)

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
sjPlot::tab_corr(td.correlates, na.deletion = c("listwise"),
                 corr.method = c("spearman"),)

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
ggboxplot(dis.delays, x = "disabled", y = "avg.delaysperday", color = "disabled", palette = c("#00AFBB", "#E7B800"), ylab = "avg delays per day", xlab = "disability_status")

# RQ1b: Are there group differences in mood while traveling between people with disabilities and people without disabilities? ####
# Worst trip moods normality test
shapiro_test(tdvars$worsttripmood.m)
hist(tdvars$worsttripmood.m)
#visualizations / descriptives 
tdvars %>% group_by(disability_status) %>% get_summary_stats(worsttripmood.m, type = "mean_sd")
ggboxplot( tdvars, x = "disability_status", y = "worsttripmood.m",ylab = "Mood", xlab = "Disability", add = "jitter")
# t-test 
worstmood.tt <- stats::t.test(worsttripmood.m ~ disabled, data = tdvars, var.equal = FALSE, alternative = c("less"))
worstmood.tt

# Best trip moods normality test
shapiro_test(tdvars$besttripmood.m) #not normal
hist(tdvars$besttripmood.m)
#visualizations / descriptives 
tdvars %>% group_by(disability_status) %>% get_summary_stats(besttripmood.m, type = c("mean_sd"))
tdvars %>% group_by(disability_status) %>% get_summary_stats(besttripmood.m, type = c("full"))
ggboxplot( tdvars, x = "disability_status", y = "besttripmood.m", ylab = "Mood", xlab = "Disability", add = "jitter")
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
tdvars %>% group_by(disability_status) %>% get_summary_stats(averagetripmood, type = "mean_sd")
ggboxplot( tdvars, x = "disability_status", y = "averagetripmood",ylab = "Mood", xlab = "Disability", add = "jitter")
# t-test 
averagemood.tt <- stats::t.test(averagetripmood ~ disabled, data = tdvars, var.equal = FALSE, alternative = c("less"))
averagemood.tt

# RQ1c: If there are differences in moods, are they attributable to the number of obstacles or delays encountered during daily travel? ####
# diff in average mood as a result of obstacles by disability status
obs.mood.bydis<- function(tdvars) {
  summary(lm(averagetripmood ~ avg.obsperday, data = tdvars))
}
by(tdvars, tdvars$disability_status, obs.mood.bydis)

# diff in average mood as a result of delayss by disability status
delays.mood.bydis<- function(tdvars) {
  summary(lm(averagetripmood ~ avg.delaysperday, data = tdvars))
}
by(tdvars, tdvars$disability_status, delays.mood.bydis)

# RQ2: What are the relationships between mood while traveling, transportation barriers, fulfillment of psychological needs and well-being?  ####

# impact of TTBPN and ALT-BPN on average mood after controlling for avg.obs perday, disability status and interaction 
summary(ttbpnmood.obs <- lm(averagetripmood ~ ttbpnmean + altbpnfmean + avg.obsperday*disability_status, data = travel_diary))
ttbpnmood.resid <- residuals(ttbpnmood)
shapiro.test(ttbpnmood.obs.resid) #residuals are normal after removing outlier
hist(ttbpnmood.obs.resid)
qqPlot(ttbpnmood.obs.resid)
skewness(ttbpnmood.obs.resid)
car::outlierTest(ttbpnmood.obs)
lmtest::bptest(ttbpnmood.obs)
summary(ttbpnmood.obs)
gvlma(ttbpnmood.obs)

# impact of TTBPN and ALT-BPN on average mood after controlling for avg.delays perday, disability status and interaction 
summary(ttbpnmood.delays <- lm(averagetripmood ~ ttbpnmean + altbpnfmean + avg.delaysperday*disability_status, data = travel_diary))
confint(ttbpnmood.delays)
ttbpnmood.resid <- residuals(ttbpnmood)
shapiro.test(ttbpnmood.resid) #residuals are normal after removing outlier
hist(ttbpnmood.resid)
qqPlot(ttbpnmood.resid)
skewness(ttbpnmood.resid)
car::outlierTest(ttbpnmood)
lmtest::bptest(ttbpnmood)
summary(ttbpnmood)
gvlma(ttbpnmood)
