library(here) #Sets working directory
library(renv) #Package management by RStudio
library(data.table) #Smart data frames
library(tidyverse) #Packages for tidy data
library(broom) #Clean up output of regression models
library(kableExtra) #Nice tables
library(mediation) #Mediation package
library(gvlma) #For Testing Linear Model Assumptions 
library(sjPlot) #Spearman correlation matrix function 
library(lme4) #Linear mixed-effects models
library(car) #Companion to Applied Regression book (ncvTest)
library(lmtest) #Testing linear regression models
library(lavaan) #LAtent VAriable ANalysis -required for mediation package
library(gvlma) #Test assumptions of regression

# Participant disability tables ####
# Table 16
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

# Table 17, SURVEY2 dis types mean ttbpn score 
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


# selecting variables and dropping incomplete cases ####
model.vars <- dplyr::select(ardraw2, flourmean, disability_status, autmean, relmean, commean, pacmean, gsemean, discmean, ttbpnmean, altbpnfmean, loghhincome)
modelvars <- model.vars[!is.na(model.vars$disability_status)]
modelvars.c <- modelvars[!is.na(modelvars$loghhincome)]

# Descriptives for final subscales by group  ####
# Table 19-20
#recalculate means w/ final items
aut2 <- dplyr::select(ardraw2, aut2, aut3, aut5, aut6)
rel2 <- dplyr::select(ardraw2, rel2, rel3, rel4, rel6)
com2 <- dplyr::select(ardraw2, com1, com2, com3, com4)
pac2 <- dplyr::select(ardraw2, trans_pac1, trans_pac2, trans_pac3, trans_pac4)
gse2 <- dplyr::select(ardraw2, gse1, gse2, gse3, gse4)
disc2 <- dplyr::select(ardraw2, disc1, disc2, disc3, disc4)
ardraw2$autmean <- rowMeans(aut2, na.rm = TRUE)
ardraw2$relmean <- rowMeans(rel2, na.rm = TRUE)
ardraw2$commean <- rowMeans(com2, na.rm = TRUE)
ardraw2$pacmean <- rowMeans(pac2, na.rm = TRUE)
ardraw2$gsemean <- rowMeans(gse2, na.rm = TRUE)
ardraw2$discmean <- rowMeans(disc2, na.rm = TRUE)
ardraw.dis <- ardraw2[ardraw2$disability_status =="Disabled"]
ardraw.nondis <- ardraw2[ardraw2$disability_status =="Nondisabled"]

scalemeans.dis<- dplyr::select(ardraw.dis, disability_status, autmean, relmean, commean, pacmean, gsemean, discmean)
scalemeans.nondis <- dplyr::select(ardraw.nondis, disability_status, autmean, relmean, commean, pacmean, discmean, gsemean)


autmean.dis <- mean(scalemeans.dis$autmean, na.rm = TRUE)
relmean.dis <- mean(scalemeans.dis$relmean, na.rm = TRUE)
commean.dis <- mean(scalemeans.dis$commean, na.rm = TRUE)
pacmean.dis <- mean(scalemeans.dis$pacmean, na.rm = TRUE)
gsemean.dis <- mean(scalemeans.dis$gsemean, na.rm = TRUE)
discmean.dis <- mean(scalemeans.dis$discmean, na.rm = TRUE)

descriptives.dis <- rbind(autmean.dis, relmean.dis, commean.dis, pacmean.dis, discmean.dis, gsemean.dis)

autsd.dis <- sd(scalemeans.dis$autmean, na.rm = TRUE)
relsd.dis <- sd(scalemeans.dis$relmean, na.rm = TRUE)
comsd.dis <- sd(scalemeans.dis$commean, na.rm = TRUE)
pacsd.dis <- sd(scalemeans.dis$pacmean, na.rm = TRUE)
gsesd.dis <- sd(scalemeans.dis$gsemean, na.rm = TRUE)
discsd.dis <- sd(scalemeans.dis$discmean, na.rm = TRUE)

descr.dis.sd <- rbind(autsd.dis, relsd.dis, comsd.dis, pacsd.dis, discsd.dis, gsesd.dis)

autp.dis <- t.test(ardraw2$autmean ~ ardraw2$disability_status, alternative = c("greater"), var.equal=FALSE)
relp.dis <- t.test(ardraw2$relmean ~ ardraw2$disability_status, alternative = c("greater"), var.equal=FALSE)
comp.dis <- t.test(ardraw2$commean ~ ardraw2$disability_status, alternative = c("greater"), var.equal=FALSE)
pacp.dis <- t.test(ardraw2$pacmean ~ ardraw2$disability_status, alternative = c("less"), var.equal=FALSE)
discp.dis <- t.test(ardraw2$discmean ~ ardraw2$disability_status, alternative = c("less"), var.equal=FALSE)
gsep.dis <- t.test(ardraw2$gsemean ~ ardraw2$disability_status, alternative = c("less"), var.equal=FALSE)

meandiff.dis.p <- rbind(autp.dis$p.value, relp.dis$p.value, comp.dis$p.value, pacp.dis$p.value, discp.dis$p.value, gsep.dis$p.value)

autmean.nondis <- mean(scalemeans.nondis$autmean, na.rm = TRUE)
relmean.nondis <- mean(scalemeans.nondis$relmean, na.rm = TRUE)
commean.nondis <- mean(scalemeans.nondis$commean, na.rm = TRUE)
pacmean.nondis <- mean(scalemeans.nondis$pacmean, na.rm = TRUE)
discmean.nondis <- mean(scalemeans.nondis$discmean, na.rm = TRUE)
gsemean.nondis <- mean(scalemeans.nondis$gsemean, na.rm = TRUE)
descriptives.nondis <- rbind(autmean.nondis, relmean.nondis, commean.nondis, pacmean.nondis, discmean.nondis, gsemean.nondis)


autsd.nondis <- sd(scalemeans.nondis$autmean, na.rm = TRUE)
relsd.nondis <- sd(scalemeans.nondis$relmean, na.rm = TRUE)
comsd.nondis <- sd(scalemeans.nondis$commean, na.rm = TRUE)
pacsd.nondis <- sd(scalemeans.nondis$pacmean, na.rm = TRUE)
gsesd.nondis <- sd(scalemeans.nondis$gsemean, na.rm = TRUE)
discsd.nondis <- sd(scalemeans.nondis$discmean, na.rm = TRUE)

descr.nondis.sd <- rbind(autsd.nondis, relsd.nondis, comsd.nondis, pacsd.nondis, discsd.nondis, gsesd.nondis)


# bind all descriptives together 
descriptives <- cbind(descriptives.dis, descr.dis.sd, descriptives.nondis, descr.nondis.sd, meandiff.dis.p)
descriptives

# RQ1a: What are the relationships between transportation barriers, fulfillment of basic psychological needs, and well-being? ####

# Correlation Matrix ##
# Table 18
ardraw2.correlates <- dplyr::select(ardraw2, flourmean, disability_status, ttbpnmean, altbpnfmean, loghhincome, age, white, gender)
ardraw2.correlates$gender <- as.numeric(ardraw2.correlates$gender)
ardraw2.correlates$white <- as.numeric(ardraw2.correlates$white)
ardraw2.corr <- modelvars[!is.na(ardraw2.correlates$loghhincome)]

#make a chart
tab_corr(ardraw2.correlates)

#simple linear regression TTBPN --> FLOUR
summary(lmtest0 <- lm(flourmean ~ ttbpnmean + loghhincome, data = modelvars.c, subset = -c(38, 153))) # -2 inf outliers 
lmtest0.resid <- residuals(lmtest0) 
shapiro.test(lmtest0.resid) # numeric test of normality
hist(lmtest0.resid) # visual test of normality
outlierTest(lmtest0) # Bonferonni p-value for most extreme obs
qqPlot(lmtest0.resid) # test for influential residual outliers (extracting 2 cases above)
gvlma(lmtest0) #test of regression assumptions
ncvTest(lmtest0) # non-constant error variance test (homoscedasticity)
lmtest::bptest(lmtest0)


# a-path
summary(lmtest1 <- lm(altbpnfmean ~ ttbpnmean + loghhincome, data = modelvars.c, subset = -c(38, 153)))
lmtest1.resid <- residuals(lmtest1) 
shapiro.test(lmtest1.resid) # numeric test of normality
hist(lmtest1.resid) # visual test of normality
outlierTest(lmtest1) # Bonferonni p-value for most extreme obs
qqPlot(lmtest1.resid) # test for influential residual outliers (extracting 2 cases above)
gvlma(lmtest1) #test of regression assumptions
ncvTest(lmtest1) # non-constant error variance test (homoscedasticity)
lmtest::bptest(lmtest1)

#bpath
summary(lmtest2 <- lm(flourmean ~ altbpnfmean + ttbpnmean + loghhincome, data = modelvars.c, subset = -c(38, 153)))
lmtest1.resid <- residuals(lmtest1) 
shapiro.test(lmtest1.resid) # numeric test of normality
hist(lmtest1.resid) # visual test of normality
outlierTest(lmtest1) # Bonferonni p-value for most extreme obs
qqPlot(lmtest1.resid) # test for influential residual outliers (extracting 2 cases above)
gvlma(lmtest1) #test of regression assumptions
ncvTest(lmtest1) # non-constant error variance test (homoscedasticity)
lmtest::bptest(lmtest1)

# Table 21
# export output to table
tab_model(lmtest0, lmtest1, lmtest2, show.intercept = TRUE, show.se = TRUE, show.ci = .95, show.r2 = TRUE, show.stat = TRUE, auto.label = FALSE, show.p = .01, collapse.ci = FALSE, show.std = TRUE, show.fstat = TRUE, digits = 2, digits.p = 3, col.order = c("est", "ci", "se", "std.est",  "p"), pred.labels = c("Intercept", "TTBPN", "Income", "ALT-BPNF"), dv.labels = c("First Model", "Second Model"), string.pred = "Predictor", string.ci = "95% CI", string.p = "p", string.est = "b", string.se = "SE", string.std = "Beta", string.stat = "Statistic")


#RQ1b: Is the  relationship between transportation barriers and wellbeing mediated by fulfillment of basic psychological needs?  ####
#psych package mediation/moderation
#center predictor and mediator variables
modelvars.c$ttbpnmean.c <- c(scale(modelvars.c$ttbpnmean, center=TRUE, scale=FALSE))
modelvars.c$altbpnfmean.c <- c(scale(modelvars.c$altbpnfmean, center=TRUE, scale=FALSE))

# mediator model
summary(model.m <- lm(altbpnfmean.c ~ ttbpnmean.c + loghhincome, data = modelvars.c, subset = -c(38, 153)))
#apa.reg.table(model.m, filename="output/Ch3-H1b.1.doc")

# outcome model
summary(model.y <- lm(flourmean ~ ttbpnmean.c + altbpnfmean.c + loghhincome, data = modelvars.c,subset = -c(38, 153)))
#apa.reg.table(model.y, filename="output/Ch3-H1b.2.doc")

# export output to table
tab_model(model.m, model.y, show.intercept = TRUE, show.se = TRUE, show.ci = .95, show.r2 = TRUE, show.stat = TRUE, auto.label = FALSE, show.p = TRUE, collapse.ci = FALSE, show.std = TRUE, show.fstat = TRUE, show.obs = FALSE, digits = 2, digits.p = 3,col.order = c("est", "ci", "se", "std.est",  "p"), dv.labels = c("First Model", "Second Model"), string.pred = "Predictor", string.ci = "95% CI", string.p = "p", string.est = "b", string.se = "SE", string.std = "Beta", string.stat = "t")
# Table 22
# mediation & outcome for ACME/ADE/total effect/proportion mediated
summary(out.model <- mediate(model.m, model.y, treat="ttbpnmean.c", mediator="altbpnfmean.c", boot = TRUE, boot.ci.type = "bca"))
plot(out.model)
#sensitivity analysis
summary(sens.cont <- medsens(out.model, rho.by = 0.05))
plot(sens.cont, sens.par = "rho")
plot(sens.cont, sens.par = "R2", r.type = "total", sign.prod = "negative")

#RQ1c To what degree does disability impact the relationship? ####
#Regressing disability status on variables of of interest

summary(ttbpndis <- lm(ttbpnmean ~ disability_status + loghhincome, data = modelvars.c))
summary(lm(altbpnfdis <- lm(altbpnfmean ~ disability_status + loghhincome, data = modelvars.c)))
summary(lm(flourdis <- lm(flourmean ~ disability_status + loghhincome, data = modelvars.c)))
# Table 23
tab_model(ttbpndis, altbpnfdis, flourdis, show.intercept = TRUE, show.se = TRUE, show.ci = .95, show.r2 = TRUE, show.stat = TRUE, auto.label = FALSE, show.p = TRUE, collapse.ci = FALSE, show.std = TRUE, show.fstat = TRUE, show.obs = FALSE, digits = 2, digits.p = 3,col.order = c("est", "ci", "se", "std.est",  "p"), dv.labels = c("First Model", "Second Model"), string.pred = "Predictor", string.ci = "95% CI", string.p = "p", string.est = "b", string.se = "SE", string.std = "Beta", string.stat = "t")

summary(flourdisttbpn <- lm(flourmean ~ ttbpnmean + disability_status + loghhincome, data = modelvars.c, subset = -c(38, 153)))
outlierTest(flourdisttbpn)
gvlma(flourdisttbpn)


# mediator model
summary(model.m.dis <- lm(altbpnfmean.c ~ ttbpnmean.c*disability_status + loghhincome, data = modelvars.c, subset = -c(38, 153)))

# outcome model
summary(model.y.dis <- lm(flourmean ~ ttbpnmean.c*disability_status + altbpnfmean.c*disability_status + loghhincome, data = modelvars.c, subset = -c(38, 153)))

# export output to table
# Table 24
tab_model(model.m.dis, model.y.dis, show.intercept = TRUE, show.se = TRUE, show.ci = .95, show.r2 = TRUE, show.stat = TRUE, auto.label = FALSE, show.p = .01, collapse.ci = FALSE, show.std = TRUE, show.fstat = TRUE, digits = 2, digits.p = 3,col.order = c("est", "ci", "se", "std.est",  "p"), dv.labels = c("First Model", "Second Model"), string.pred = "Predictor", string.ci = "95% CI", string.p = "p", string.est = "b", string.se = "SE", string.std = "Beta")

# Table 25
# mediation & outcome for ACME/ADE/total effect/proportion mediated
summary(out.model.2 <- mediate(model.m.dis, model.y.dis, treat="ttbpnmean.c", mediator="altbpnfmean.c",sims = 1000, boot = TRUE, boot.ci.type = "bca"))


# for disabled participants
summary(out.model.dis <- mediate(model.m.dis, model.y.dis, treat="ttbpnmean.c", mediator="altbpnfmean.c", covariates = list("disability_status" = 1), conf.level = .984, sims = 1000, boot = TRUE, boot.ci.type = "bca"))
plot(out.model.dis)

# for nondisabled participants
summary(out.model.nondis <- mediate(model.m.dis, model.y.dis, treat="ttbpnmean.c", mediator="altbpnfmean.c", covariates = list("disability_status"= 0),sims = 1000, boot = TRUE, boot.ci.type = "perc"))
plot(out.model.nondis)

#sensitivity analysis
summary(sens.cont <- medsens(out.model.nondis, rho.by = 0.05))
plot(sens.cont, sens.par = "rho")
plot(sens.cont, sens.par = "R2", r.type = "total", sign.prod = "negative")

#RQ2 - What do the subscales tell us? ####
summary(thwartsubscales <- lm(flourmean ~ autmean + relmean +commean + loghhincome + disability_status, data = modelvars.c, subset = -c(38, 153)))
gvlma(thwartsubscales)
outlierTest(thwartsubscales)
car::vif(thwartsubscales)

summary(supportsubscales <- lm(flourmean ~ pacmean +gsemean + discmean + loghhincome + disability_status, data = modelvars.c, subset = -c(38, 153)))
gvlma(supportsubscales)
outlierTest(supportsubscales)
car::vif(supportsubscales)

# export output to table
# Table 26
tab_model(thwartsubscales, supporttsubscales, show.intercept = TRUE, show.se = TRUE, show.ci = .95, show.r2 = TRUE, show.stat = TRUE, auto.label = FALSE, show.p = .01, collapse.ci = FALSE, show.std = TRUE, show.fstat = TRUE, digits = 2, digits.p = 3,col.order = c("est", "ci", "se", "std.est",  "p"), dv.labels = c("First Model", "Second Model"), string.pred = "Predictor", string.ci = "95% CI", string.p = "p", string.est = "b", string.se = "SE", string.std = "Beta")


# AUTONOMY center predictor and mediator variables
modelvars.c$autmean.c <- c(scale(modelvars.c$autmean, center=TRUE, scale=FALSE))
modelvars.c$pacmean.c <- c(scale(modelvars.c$pacmean, center=TRUE, scale=FALSE))

# mediator model
model.m <- lm(pacmean.c ~ autmean.c*disability_status + loghhincome, data = modelvars.c, subset = -c(38, 153))

# outcome model
model.y <- lm(flourmean ~ autmean.c*disability_status + pacmean.c*disability_status + loghhincome, data = modelvars.c,subset = -c(38, 153))

# Table 27a
# mediation & outcome for ACME/ADE/total effect/proportion mediated
summary(out.model <- mediate(model.m, model.y, treat="autmean.c", mediator="pacmean.c", boot = TRUE, boot.ci.type = "bca", sims = 5000))
plot(out.model)

# RELATEDNESS center predictor and mediator variables
modelvars.c$relmean.c <- c(scale(modelvars.c$relmean, center=TRUE, scale=FALSE))
modelvars.c$discmean.c <- c(scale(modelvars.c$discmean, center=TRUE, scale=FALSE))

# mediator model
model.m <- lm(discmean.c ~ relmean.c*disability_status + loghhincome, data = modelvars.c, subset = -c(38, 153))

# outcome model
model.y <- lm(flourmean ~ relmean.c*disability_status + discmean.c*disability_status + loghhincome, data = modelvars.c,subset = -c(38, 153))

# Table 27b
# mediation & outcome for ACME/ADE/total effect/proportion mediated
summary(out.model <- mediate(model.m, model.y, treat="relmean.c", mediator="discmean.c", boot = TRUE, boot.ci.type = "bca", sims = 5000))
plot(out.model)

# COMPETENCE center predictor and mediator variables
modelvars.c$commean.c <- c(scale(modelvars.c$commean, center=TRUE, scale=FALSE))
modelvars.c$gsemean.c <- c(scale(modelvars.c$gsemean, center=TRUE, scale=FALSE))

# mediator model
model.m <- lm(gsemean.c ~ commean.c*disability_status + loghhincome, data = modelvars.c, subset = -c(38, 153))

# outcome model
model.y <- lm(flourmean ~ commean.c*disability_status + gsemean.c*disability_status + loghhincome, data = modelvars.c,subset = -c(38, 153))

# Table 27c
# mediation & outcome for ACME/ADE/total effect/proportion mediated
summary(out.model <- mediate(model.m, model.y, treat="commean.c", mediator="gsemean.c", boot = TRUE, boot.ci.type = "bca"), sims = 5000)
plot(out.model)


#RQ2 - Which disability types are associated w/ transportation barriers? ####

# S1 Disability Types
# Table 28
s1.dis.types.reg <- lm(ttbpnmean ~ dis_physical + dis_blv + dis_dhoh + dis_developmental + dis_chronic_condition + dis_mental_health, data = ardraw)
tab_model(s1.dis.types.reg, show.intercept = TRUE, show.se = TRUE, show.ci = .95, show.r2 = TRUE, show.stat = TRUE, auto.label = FALSE, show.p = .01, collapse.ci = FALSE, show.std = TRUE, show.fstat = TRUE, digits = 2, digits.p = 3,col.order = c("est", "ci", "se", "std.est",  "p"), dv.labels = c("First Model", "Second Model"), string.pred = "Predictor", string.ci = "95% CI", string.p = "p", string.est = "b", string.se = "SE", string.std = "Beta")
# select disability types and calculate correlations
s1.dis.types <- dplyr::select(ardraw, ttbpnmean, dis_physical, dis_blv, dis_dhoh, dis_developmental, dis_chronic_condition, dis_mental_health)
tab_corr(s1.dis.types, na.deletion = c("listwise"),  corr.method = c("spearman"),)
# VIF
car::vif(s1.dis.types.reg)

#S1 Functional Limitation Types
# Table 29
s1.funct.types.reg <- lm(ttbpnmean ~ move_physically + understand_info + see_hear_info + be_around_people + deal_w_frustration + communicate,  data = ardraw)
tab_model(s1.funct.types.reg, show.intercept = TRUE, show.se = TRUE, show.ci = .95, show.r2 = TRUE, show.stat = TRUE, auto.label = FALSE, show.p = .01, collapse.ci = FALSE, show.std = TRUE, show.fstat = TRUE, digits = 2, digits.p = 3,col.order = c("est", "ci", "se", "std.est",  "p"), dv.labels = c("First Model", "Second Model"), string.pred = "Predictor", string.ci = "95% CI", string.p = "p", string.est = "b", string.se = "SE", string.std = "Beta")
# select funct limitation types to calculate correlations 
s1.funct.types <- dplyr::select(ardraw, ttbpnmean, move_physically,  understand_info, see_hear_info, be_around_people, deal_w_frustration,  communicate)
tab_corr(s1.funct.types, na.deletion = c("listwise"),  corr.method = c("spearman"),)
# VIF
car::vif(s1.funct.types.reg)


#disability correlates S2
# Table 30
disttbpn <- lm(ttbpnmean ~ bdis_seeing + bdis_hearing+ bdis_walking +  bdis_cognitive + bdis_selfcare, data = ardraw2)
tab_model(disttbpn, show.intercept = TRUE, show.se = TRUE, show.ci = .95, show.r2 = TRUE, show.stat = TRUE, auto.label = FALSE, show.p = .01, collapse.ci = FALSE, show.std = TRUE, show.fstat = TRUE, digits = 2, digits.p = 3,col.order = c("est", "ci", "se", "std.est",  "p"), dv.labels = c("First Model", "Second Model"), string.pred = "Predictor", string.ci = "95% CI", string.p = "p", string.est = "b", string.se = "SE", string.std = "Beta")
# select vars for correlation
s2.distypes.con <- dplyr::select(ardraw2, ttbpnmean, bdis_seeing, bdis_hearing,  bdis_walking, bdis_cognitive, bdis_selfcare)
tab_corr(s2.distypes.con, na.deletion = c("listwise"),  corr.method = c("spearman"),)
#VIF
car::vif(disttbpn)

# Table 31
disttbpn.noncon <- lm(ttbpnmean ~ condis + bdis_comm + bdepression_severity+ banxiety_severity+ bpain_severity + bfatigue_severity, data = ardraw2, )
tab_model(disttbpn.noncon, show.intercept = TRUE, show.se = TRUE, show.ci = .95, show.r2 = TRUE, show.stat = TRUE, auto.label = FALSE, show.p = .01, collapse.ci = FALSE, show.std = TRUE, show.fstat = TRUE, digits = 2, digits.p = 3,col.order = c("est", "ci", "se", "std.est",  "p"), dv.labels = c("First Model", "Second Model"), string.pred = "Predictor", string.ci = "95% CI", string.p = "p", string.est = "b", string.se = "SE", string.std = "Beta")
# select vars for correlation
s2.distypes.noncon <- dplyr::select(ardraw2, ttbpnmean, condis, bdis_comm, bdepression_severity, banxiety_severity, bpain_severity,  bfatigue_severity)
tab_corr(s2.distypes.noncon, na.deletion = c("listwise"),  corr.method = c("spearman"),)
# VIF
car::vif(disttbpn.noncon)