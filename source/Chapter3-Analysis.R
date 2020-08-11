library(here) #sets working directory
library(data.table) #Smart data frames
library(tidyverse) #dplyr tidyr for neat data (check for conflicts)
library(mediation) #Mediation package
library(gvlma) #For Testing Linear Model Assumptions 
library(sjPlot) # Pearson correlations for multiple/ binary variables 
library(lme4) #Linear mixed-effects models
library(car) #Companion to Applied Regression book (ncvTest)
library(apaTables) #Format correlation tables
library(VIF) #Variance inflation factor
library(ggplot2) #Data visualizations
library(lmtest) #Testing linear regression models
library(lavaan) #LAtent VAriable ANalysis
library(gvlma) #Test assumptions of regression

# Participant disability tables ####
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
# preliminary calculations  ####
# calculate mean score of final model from Ch.2, Survey 1
ttbpn <- dplyr::select(ardraw, aut3,  aut4, aut5, aut6, rel1, rel2, rel3, rel4, com1, com2, com3, com6)
ardraw$ttbpnmean <- rowMeans(ttbpn, na.rm = TRUE)

# calculate mean score of final model from Ch.2, Survey 2
ttbpn2 <- dplyr::select(ardraw2, aut2, aut3, aut5, aut6,  rel2, rel3, rel4, rel6, com1, com2, com3, com4)
ardraw2$ttbpnmean <- rowMeans(ttbpn2, na.rm = TRUE)
altbpnf2 <-  dplyr::select(ardraw2, trans_pac1, trans_pac2, trans_pac3, trans_pac4, gse1, gse2, gse3, gse4,  disc1, disc2, disc3, disc4)
ardraw2$altbpnfmean <- rowMeans(altbpnf2, na.rm = TRUE)
ardraw2$loghhincome <- log(ardraw2$hhincome)
ardraw2$loghhincome[which(ardraw2$loghhincome=="-Inf")] = NA # replacing inf w/ NA for one weird case

# RQ1a: What are the relationships between transportation barriers, fulfillment of basic psychological needs, and well-being? ####

# Correlation Matrix ##
ardraw2.correlates <- dplyr::select(ardraw2, flourmean, disability_status, ttbpnmean, altbpnfmean, loghhincome, age, white, gender)
ardraw2.correlates$gender <- as.numeric(ardraw2.correlates$gender)
ardraw2.correlates$white <- as.numeric(ardraw2.correlates$white)
ardraw2.corr <- modelvars[!is.na(ardraw2.correlates$loghhincome)]

#make a chart
tab_corr(ardraw2.correlates)


# selecting variables and dropping incomplete cases
modelvars <- model.vars[!is.na(ardraw2.correlates$disability_status)]
modelvars.c <- modelvars[!is.na(modelvars$loghhincome)]

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

# adding alt-bpnf
summary(lmtest1 <- lm(flourmean ~ ttbpnmean + altbpnfmean + loghhincome, data = modelvars.c, subset = -c(38, 153)))
lmtest1.resid <- residuals(lmtest1) 
shapiro.test(lmtest1.resid) # numeric test of normality
hist(lmtest1.resid) # visual test of normality
outlierTest(lmtest1) # Bonferonni p-value for most extreme obs
qqPlot(lmtest1.resid) # test for influential residual outliers (extracting 2 cases above)
gvlma(lmtest1) #test of regression assumptions
ncvTest(lmtest1) # non-constant error variance test (homoscedasticity)
lmtest::bptest(lmtest1)

apa.reg.table(lmtest0, lmtest1,filename="output/Ch3-H1a.doc")

#RQ1b: Is the  relationship between transportation barriers and wellbeing mediated by fulfillment of basic psychological needs?  ####
#psych package mediation/moderation
#center predictor and mediator variables
modelvars.c$ttbpnmean.c <- c(scale(modelvars.c$ttbpnmean, center=TRUE, scale=FALSE))
modelvars.c$altbpnfmean.c <- c(scale(modelvars.c$altbpnfmean, center=TRUE, scale=FALSE))

# mediator model
summary(model.m <- lm(altbpnfmean.c ~ ttbpnmean.c + loghhincome, data = modelvars.c, subset = -c(38, 153)))
summary(model.m.beta <- lm.beta(model.m))
apa.reg.table(model.m, filename="output/Ch3-H1b.1.doc")

# outcome model
summary(model.y <- lm(flourmean ~ ttbpnmean.c + altbpnfmean.c + loghhincome, data = modelvars.c,subset = -c(38, 153)))
apa.reg.table(model.y, filename="output/Ch3-H1b.2.doc")

# mediation & outcome for ACME/ADE/total effect/proportion mediated
summary(out.model <- mediate(model.m, model.y, treat="ttbpnmean.c", mediator="altbpnfmean.c", boot = FALSE))
summary(out.model.boot <- mediate(model.m, model.y, treat="ttbpnmean.c", mediator="altbpnfmean.c"))
plot(out.model)
#sensitivity analysis
summary(sens.cont <- medsens(out.model, rho.by = 0.05))
plot(sens.cont, sens.par = "rho")
plot(sens.cont, sens.par = "R2", r.type = "total", sign.prod = "negative")

#RQ1c To what degree does disability impact the relationship? ####
# mediator model
summary(model.m.dis <- lm(altbpnfmean.c ~ ttbpnmean.c*disability_status + loghhincome, data = modelvars.c, subset = -c(38, 153)))
apa.reg.table(model.m.dis, filename="output/Ch3-H1c.1.doc")


# outcome model
summary(model.y.dis <- lm(flourmean ~ ttbpnmean.c*disability_status + altbpnfmean.c*disability_status + loghhincome, data = modelvars.c, subset = -c(38, 153)))
apa.reg.table(model.y.dis, filename="output/Ch3-H1c.2.doc")

# mediation & outcome for ACME/ADE/total effect/proportion mediated
summary(out.model.dis <- mediate(model.m.dis, model.y.dis, treat="ttbpnmean.c", mediator="altbpnfmean.c", sims = 1000, boot = TRUE, boot.ci.type = "bca"))
plot(out.model.dis)

#sensitivity analysis
summary(sens.cont.dis <- medsens(out.model.dis, rho.by = 0.05))
plot(sens.cont.dis, sens.par = "rho")
plot(sens.cont.dis, sens.par = "R2", r.type = "total", sign.prod = "negative")

# calculate difference in ACME based on disability status
#disabled - nondisabled
test.modmed(out.model.dis, covariates.1 = list(disability_status = 1), covariates.2 = list(disability_status = 0))


#RQ2 - Which disability types are associated w/ transportation barriers? ####

# S1 Disability Types
s1.dis.types <- dplyr::select(ardraw, ttbpnmean, dis_physical, dis_blv, dis_dhoh, dis_developmental, dis_chronic_condition, dis_mental_health)
tab_corr(s1.dis.types)

summary(s1.dis.types.reg <- lm(ttbpnmean ~ dis_physical + dis_blv + dis_dhoh + dis_developmental + dis_chronic_condition + dis_mental_health, data = ardraw))
apa.reg.table(s1.dis.types.reg, filename="output/Ch3-RQ2.1.doc")

#S1 Functional Limitation Types
s1.functlim.types <- dplyr::select(ardraw, ttbpnmean, move_physically, understand_info, see_hear_info, be_around_people, deal_w_frustration, communicate)
tab_corr(s1.functlim.types)

summary(s1.functlim.types.reg <- lm(ttbpnmean ~ move_physically + understand_info + see_hear_info + be_around_people + deal_w_frustration + communicate,  data = ardraw))
apa.reg.table(s1.functlim.types.reg, filename="output/Ch3-RQ2.2.doc")

#disability correlates S2
disttbpn.correlates <- dplyr::select(ardraw2,ttbpnmean, bdis_seeing, bdis_hearing, bdis_walking, bdis_cognitive, bdis_selfcare, bdis_comm, bdepression_severity, banxiety_severity, bpain_severity, bfatigue_severity)
tab_corr(disttbpn.correlates)

summary(disttbpn <- lm(ttbpnmean ~ bdis_seeing + bdis_hearing+ bdis_walking +  bdis_cognitive + bdis_selfcare, data = ardraw2))
apa.reg.table(disttbpn, filename="output/Ch3-RQ2.3.doc")

summary(disttbpn.noncon <- lm(ttbpnmean ~ condis + bdis_comm + bdepression_severity+ banxiety_severity+ bpain_severity + bfatigue_severity, data = ardraw2, ))
apa.reg.table(disttbpn.noncon, filename="output/Ch3-RQ2.4.doc")
