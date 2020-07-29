library(data.table)
library(tidyverse)
library(xtable)
library(pander)
library(rmarkdown)
library(kableExtra)
library(Matrix)
library(lavaan)
library(pander)
library(psych)
# Demographic Data Tables ####
#create contingency tables for each variable and combine 
colstable <- data.table(variables = c("Female", "Male", "Not specified", "White", "Non-white", "Advanced degree","College", "High school or less", "<10,000","10,000-30,000", "30,000 - 70,000", "100,000+", "Full-time", "Part-time", "Self-employed", "Student", "Unemployed", "Unable to work", "Retired"))
demotable1<-table(ardraw2$gender, ardraw2$disability_status)
demotable2<-table(ardraw2$white, ardraw2$disability_status)
demotable3<-table(ardraw2$education, ardraw2$disability_status)
demotable4<-table(ardraw2$income_range, ardraw2$disability_status)
demotable5<-table(ardraw2$emp_ft, ardraw2$disability_status)
demotable6<-table(ardraw2$emp_pt, ardraw2$disability_status)
demotable7<-table(ardraw2$emp_self, ardraw2$disability_status)
demotable8<-table(ardraw2$emp_student, ardraw2$disability_status)
demotable9<-table(ardraw2$emp_unemp, ardraw2$disability_status)
demotable10<-table(ardraw2$emp_unable, ardraw2$disability_status)
demotable11<-table(ardraw2$emp_retired, ardraw2$disability_status)
demotable<- data.table(rbind(demotable1, demotable2, demotable3, demotable4, demotable5, demotable6, demotable7, demotable8, demotable9, demotable10, demotable11))
#create proportions of rows - change margins = 2 if column proportions are preferred
demo_proportions <- prop.table(as.matrix(demotable), margin=1)*100
#combine variables with individual contingency tables
democomplete <- cbind(colstable, demotable, round(demo_proportions, 2))
#print(xtable(democomplete), type="latex", comment=FALSE)
print(democomplete) %>% 
  kable(digits = 3, format="pandoc", caption="demographics 1")

#disability profiles table



# create scale tables and means ####
ardraw2factors <- select(ardraw2, aut1, aut2, aut3, aut4, aut5, aut6, 
                         rel1, rel2, rel3, rel4, rel5, rel6,
                         com1, com2, com3, com4,
                         trans_pac1, trans_pac2, trans_pac3, trans_pac4, 
                         gse1, gse2, gse3, gse4,
                         discr1, discr2, discr3, discr4,
                         flour1, flour2, flour3, flour4,
                         flour5, flour6, flour7, flour8)
#ardrawfactors <- ardraw2factors[complete.cases(ardraw2factors),]
ttbpn2 <- select(ardraw2factors, aut1, aut2, aut3, aut4, aut5, aut6, 
                 rel1, rel2, rel3, rel4, rel5, rel6,
                 com1, com2, com3, com4)
aut2 <- select(ardraw2factors, aut2, aut3, aut4, aut5, aut6)
rel2 <- select(ardraw2factors, rel1, rel2, rel3, rel4, rel5, rel6)
com2 <- select(ardraw2factors, com1, com2, com3, com4)
pac2 <- select(ardraw2factors, trans_pac1, trans_pac2, trans_pac3, trans_pac4)
gse2 <- select(ardraw2factors, gse1, gse2, gse3, gse4)
discr2 <- select(ardraw2factors, discr1, discr2, discr3, discr4)
flour2 <- select(ardraw2factors, flour1, flour2, flour3, flour4, flour5, flour6, flour7, flour8)
# disability scale (10 dis types w/ severities)
dis_score <- select(ardraw2, dis_seeing, dis_hearing, dis_walking, dis_cognitive, dis_comm, dis_selfcare, anxiety_severity, depression_severity, pain_severity , fatigue_severity)
ardraw2$dis_score <- rowSums(dis_score, na.rm = TRUE)


# within-participant mean-score-calculations
ardraw2$ttbpnmean <- rowMeans(ttbpn2, na.rm = TRUE)
ardraw2$autmean <- rowMeans(aut2, na.rm = TRUE)
ardraw2$relmean <- rowMeans(rel2, na.rm = TRUE)
ardraw2$commean <- rowMeans(com2, na.rm = TRUE)
ardraw2$pacmean <- rowMeans(pac2, na.rm = TRUE)
ardraw2$gsemean <- rowMeans(gse2, na.rm = TRUE)
ardraw2$discrmean <- rowMeans(discr2, na.rm = TRUE)
ardraw2$flourmean <- rowMeans(flour2, na.rm = TRUE)

# missing data/MVN ####
library(dlookr)
library(missForest)
library(mice)
library(MVN)
mvn(ttbpn2, mvnTest = c("mardia", "hz", "royston", "dh", "energy"), covariance = TRUE, tol = 1e-25, alpha = 0.5, scale = FALSE, desc = TRUE, transform = "none", univariateTest = c("SW", "CVM", "Lillie", "SF", "AD"), univariatePlot = "none", multivariatePlot = "none",  multivariateOutlierMethod = "none", bc = FALSE, bcType = "rounded", showOutliers = TRUE, showNewData = TRUE)
library(BaylorEdPsych)
library (mvnmle)
library(car)
LittleMCAR(ardraw2factors)

scatterplotMatrix(subscale_means[7:9])
#univariate plots
result <- mvn(data = ttbpn2, mvnTest = "royston", univariatePlot = "histogram")


# assumptions/analytic strategy ####
# group differences ####
boxplot(ardraw2$ttbpnmean ~ ardraw2$disability_status, ylab="Score", col=c("red", "blue"), main="Boxplot of scores in 2 groups")
# independent 2-group t-test
t.test(ardraw2$ttbpnmean~ardraw2$disability_status) 
#ardraw$ttbpnmean by ardraw$disability_status
#t = -7.6032, df = 274.9, p-value = 4.589e-13
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.6782118 -0.3992372
#sample estimates:
#  mean in group Nondisabled    mean in group Disabled 
#1.772606                  2.311330 

# ttbpn by group to check for normality
dis.ttbpn2 <- ardraw2[disability_status ==1, ttbpnmean,]
nondis.ttbpn2 <- ardraw2[disability_status ==0, ttbpnmean,]

ks.test(nondis.ttbpn2, dis.ttbpn2,
        alternative = c("two.sided"),
        exact = NULL)
# normality testing ####
library(dlookr)
find_skewness(ardraw2, index = FALSE)
# univariate normality

# skewness
library(moments)
skewness(ttbpn2, na.rm = TRUE)
skewness(nondis.ttbpn2, na.rm = TRUE)

# checking for multivariate normal (it's not multivariate normal) ####
mvn(ttbpn2, mvnTest = c("mardia", "hz", "royston", "dh",
                               "energy"), covariance = TRUE, tol = 1e-25, alpha = 0.5,
    scale = FALSE, desc = TRUE, transform = "none",
    univariateTest = c("SW", "CVM", "Lillie", "SF", "AD"),
    univariatePlot = "none", multivariatePlot = "none",
    multivariateOutlierMethod = "none", bc = FALSE, bcType = "rounded",
    showOutliers = TRUE, showNewData = FALSE)
# checking for MCAR data #### (not sure it is considered MCAR)
library(mice)
md.pattern(ttbpn2)
library(MissMech)
library(BaylorEdPsych)
mcar.little <- LittleMCAR(ttbpn2)
mcar.little[c( "chi.square ", "d ", "p.value")]
#p-value for littleMCAr = 0.0609548
TestMCARNormality(ttbpn2)
# multivariate normality rejected: Provided that normality can be assumed, the hypothesis of MCAR is rejected at 0.05 significance level.
#Non-Parametric Test:
#P-value for the non-parametric test of homoscedasticity:  0.1906296 
#Reject Normality at 0.05 significance level.
#There is not sufficient evidence to reject MCAR at 0.05 significance level.

# test w/ normalized data 
#calculate z scores
z.data.n <-scale(x.n)
#calculate z score for count
z.data.p <- scale (X.p)
#one-sample z.test to see if mean is diff than specified value of pop variability
library(TeachingDemos)
z.test(na.omit(data$col), mu = 0.5, stdev = squrt(0.08))
#one-sample t-test to estimate population variance
t.test(data$col, mu = 0.5, alternative = "two.sided", conf.level = 0.95)

# missing data
# cutoffs for skewness and kurtosis
mahalanobis(datold)
# test disability group differences
#independent samples t test 




# reliability testing ####
# test scale alphas  
library(coefficientalpha)
alpha(ttbpn2)
alpha(pac2)
alpha(gse2)
alpha(discr2)
alpha(flour2)

# composite, convergent, and discriminant validity ####
#https://rpubs.com/wiryantodatascience/Comp_Reliability
threef.model <- '
aut=~ a*aut1 + b*aut2 + c*aut3 + d*aut4 + e*aut5 + f*aut6
rel=~ g*rel1 + h*rel2 + i*rel3 + j*rel4 + k*rel5 + l*rel6
com=~ m*com1 + n*com2 + o*com3 + p*com4
aut~~s*aut
rel~~t*rel
com~~u*com
com~~v*rel
aut~~w*rel
aut~~x*com
'
#aut CR calculation
aut.model <- ' aut  =~ aut1 + aut2 + aut3 + aut4 + aut5 + aut6'
aut.fit <- cfa(aut.model, data = ardraw2, meanstructure = TRUE)
sl <- standardizedSolution(aut.fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(aut2)
sl
# Compute residual variance of each item
re <- 1 - sl^2
# calculate AVE for aut
ave.aut <- (sum(sl^2))/6
ave.aut
# Compute composite reliability
sum(sl)^2 / (sum(sl)^2 + sum(re))
# Extract the standardized loading matrix
loadMatrix <- inspect(aut.fit, "std")$lambda
# Clear the zero loadings
loadMatrix[loadMatrix==0] <- NA
# Calculate mean squared loadings (i.e. AVEs)
apply(loadMatrix^2,2,mean, na.rm = TRUE)


#rel CR calculation 
rel.model <- ' rel  =~ rel1 + rel2 + rel3 + rel4 + rel5 + rel6'
rel.fit <- cfa(rel.model, data = ardraw2)
sl <- standardizedSolution(rel.fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(rel2)
sl
# Compute residual variance of each item
re <- 1 - sl^2
# calculate AVE for rel
ave.rel <- (sum(sl^2))/6
ave.rel
# Compute composite rel reliability
sum(sl)^2 / (sum(sl)^2 + sum(re))
# Extract the standardized loading matrix
loadMatrix <- inspect(rel.fit, "std")$lambda
# Clear the zero loadings
loadMatrix[loadMatrix==0] <- NA
# Calculate mean squared loadings (i.e. AVEs)
apply(loadMatrix^2,2,mean, na.rm = TRUE)
#com CR calculation 
com.model <- ' com  =~ com1 + com2 + com3 + com4'
com.fit <- cfa(com.model, data = ardraw2)
sl <- standardizedSolution(com.fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(com2)
sl
# Compute residual variance of each item
re <- 1 - sl^2
# calculate AVE for com
ave.com <- (sum(sl^2))/6
ave.com
# Compute composite com reliability
sum(sl)^2 / (sum(sl)^2 + sum(re))
# Extract the standardized loading matrix
loadMatrix <- inspect(com.fit, "std")$lambda
# Clear the zero loadings
loadMatrix[loadMatrix==0] <- NA
# Calculate mean squared loadings (i.e. AVEs)
apply(loadMatrix^2,2,mean, na.rm = TRUE)

# Compute TTBPNcomposite reliability
threef.full.model <- '
aut=~ a*aut1 + b*aut2 + c*aut3 + d*aut4 + e*aut5 + f*aut6
rel=~ g*rel1 + h*rel2 + i*rel3 + j*rel4 + k*rel5 + l*rel6
com=~ m*com1 + n*com2 + o*com3 + p*com4
aut~~s*aut
rel~~t*rel
com~~u*com
com~~v*rel
aut~~w*rel
aut~~x*com
'
cr.threef.fit <- cfa(threef.model, data = ardraw2, meanstructure = TRUE)
sl <- standardizedSolution(cr.threef.fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(ttbpn2)
# Calculate mean squared loadings (i.e. AVEs)
apply(loadMatrix^2,2,mean, na.rm = TRUE)
cr.threef.fit <- cfa(threef.model, data = ardraw2, meanstructure = TRUE)
sl <- standardizedSolution(cr.threef.fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(ttbpn2)
# Compute residual variance of each item
re <- 1 - sl^2
# Compute composite reliability
cr.threef <- sum(sl)^2 / (sum(sl)^2 + sum(re))
# Extract the standardized loading matrix
loadMatrix <- inspect(cr.threef.fit, "std")$lambda
# Clear the zero loadings
loadMatrix[loadMatrix==0] <- NA
# Calculate mean squared loadings (i.e. AVEs)
apply(loadMatrix^2,2,mean, na.rm = TRUE)
# create mean correlation table and test for discriminant validity by squaring correlation
ttbpn.factors <- ardraw2[, c("autmean", "relmean", "commean")]
corr.ttbpn <- cor(ttbpn.factors, use = "complete")
sq.corr.ttbpn <- corr.ttbpn^2
sq.corr.ttbpn
# Compute residual variance of each item
re <- 1 - sl^2
# Extract the standardized loading matrix
loadMatrix <- inspect(cr.threef.fit, "std")$lambda
# Clear the zero loadings
loadMatrix[loadMatrix==0] <- NA
# Calculate mean squared loadings (i.e. AVEs)
apply(loadMatrix^2,2,mean, na.rm = TRUE)

# 3-factor model w/ pairwise covariance ####
#calculate covariance matrix
tf.ttbpn2.cov <- cov(ttbpn2, use= "pairwise.complete.obs")
# model with all variables
ttbpn2.3f.model <- '
aut=~ aut1 + aut2 + aut3 + aut4 + aut5 + aut6
rel=~ rel1 + rel2 + rel3 + rel4 + rel5 + rel6
com=~ com1 + com2 + com3 + com4
aut~~rel
aut~~com
rel~~com
'
ttbpn2.3fmod.fit <- cfa(ttbpn2.3f.model, sample.cov = tf.ttbpn2.cov, sample.nobs = 211)
summary(ttbpn2.3fmod.fit, fit.measures=TRUE, rsquare=TRUE, standardized=TRUE, ci=TRUE, modindices = TRUE)
#print table of parameter estimates
ttbpn2.3fmod.param <- parameterEstimates(ttbpn2.3fmod.fit, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select("Latent Factor"=lhs, Indicator=rhs, B=est, SE=se, Z=z, "p-value"=pvalue, Beta=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

inspect(ttbpn2.3fmod.fit, what = "std")$lambda

#print diagram
library(semPlot)
semPaths(ttbpn2.3fmod.fit, what="paths", whatLabels="par")


# 3-factor TTBN using FIMLfor missing ####
library(lavaan)
ttbpn2.3fFIML.model <- '
aut=~ aut1 + aut2 + aut3 + aut4 + aut5 + aut6
rel=~ rel1 + rel2 + rel3 + rel4 + rel5 + rel6
com=~ com1 + com2 + com3 + com4
aut~~rel
aut~~com
rel~~com
'
ttbpn2.3fFIML.fit <- cfa(ttbpn2.3fFIML.model, sample.nobs = 211, estimator = "MLR", data = ttbpn2)
summary(ttbpn2.3fFIML.fit, fit.measures=TRUE, rsquare=TRUE, standardized=TRUE, ci=TRUE, modindices = TRUE)
#print table of parameter estimates
ttbpn2.3fFIML.param <- parameterEstimates(ttbpn2.3fFIML.fit, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select("Latent Factor"=lhs, Indicator=rhs, B=est, SE=se, Z=z, "p-value"=pvalue, Beta=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

inspect(ttbpn2.3fFIML.fit, what = "std")$lambda

#print diagram
library(semPlot)
semPaths(ttbpn2.3fmod.fit, what="paths", whatLabels="par")


# 3-factor adapted model w/ pairwise complete correlations ####
ttbpn2mod <- select(ardraw2factors, aut1, aut2, aut3, aut4, aut5, aut6, 
                 rel3, rel4, rel5, rel6,
                 com1, com2, com3, com4)
#calculate covariance matrix

tf.ttbpn2mod.cov <- cov(ttbpn2mod, use= "pairwise.complete.obs")
# model with all variables
ttbpn2.3fmod.model <- '
aut=~ aut1 + aut2 + aut3 + aut4 + aut5 + aut6
rel=~ rel3 + rel4 + rel5 + rel6
com=~ com1 + com2 + com3 + com4
aut~~rel
aut~~com
rel~~com
'
ttbpn2.3fmod.fit <- cfa(ttbpn2.3fmod.model, sample.cov = tf.ttbpn2.cov, sample.nobs = 211)
summary(ttbpn2.3fmod.fit, fit.measures=TRUE, rsquare=TRUE, standardized=TRUE, ci=TRUE, modindices = TRUE)
#print table of parameter estimates
ttbpn2.3fmod.param <- parameterEstimates(ttbpn2.3fmod.fit, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select("Latent Factor"=lhs, Indicator=rhs, B=est, SE=se, Z=z, "p-value"=pvalue, Beta=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

inspect(ttbpn2.3fmod.fit, what = "std")$lambda

#print diagram
library(semPlot)
semPaths(ttbpn2.3fmod.fit, what="paths", whatLabels="par")

ttbpn2.3fmod.model.mlr <- cfa(model = ttbpn2.3fmod.model, data = ttbpn2mod, estimator = "MLR")
summary(ttbpn2.3fmod.model.mlr, standardized = TRUE, fit.measures = TRUE)



# 3-factor adapted TTBN using FIML ####
library(lavaan)
ttbpn2.3fmodFIML.model <- '
aut=~ aut2 + aut3 + aut4 + aut6
rel=~ rel3 + rel4 + rel5 + rel6
com=~ com1 + com2 + com3 + com4
aut~~rel
aut~~com
rel~~com
aut =~ com1
aut =~ rel5
aut4 ~~ aut6
'
ttbpn2.3fmodFIML.fit <- cfa(ttbpn2.3fmodFIML.model, sample.nobs = 211, missing = "ML", estimator = "MLR", data = ttbpn2mod)
summary(ttbpn2.3fmodFIML.fit, fit.measures=TRUE, rsquare=TRUE, standardized=TRUE, ci=TRUE, modindices = TRUE)
#print table of parameter estimates
ttbpn2.3fmodFIML.param <- parameterEstimates(ttbpn2.3fmodFIML.fit, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select("Latent Factor"=lhs, Indicator=rhs, B=est, SE=se, Z=z, "p-value"=pvalue, Beta=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")

inspect(ttbpn2.3fmodFIML.fit, what = "std")$lambda

#print diagram
library(semPlot)
semPaths(ttbpn2.3fmodFIML.fit, what="paths", whatLabels="par")


