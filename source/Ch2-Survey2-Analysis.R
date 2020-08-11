library(renv)
library(here)
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
library(semPlot)
library(apaTables)
library(coefficientalpha)
# Demographic Data Tables ####
#create contingency tables for each variable and combine 
colstable <- data.table(variables = c("Female", "Male", "Not specified", "Non-white", "White", "Advanced degree","College", "High school or less", "<5,000", "5,000 - 12,000", "12,000 - 25,000", "25,000 - 50,000", "50,000-100,000", "100,000+", "Full Time", "Part time", "Unemployed", "Missing"))
demotable1<-table(ardraw2$gender, ardraw2$disability_status)
demotable2<-table(ardraw2$white, ardraw2$disability_status)
demotable3<-table(ardraw2$education, ardraw2$disability_status)
demotable4<-table(ardraw2$employed, ardraw2$disability_status)
demotable5<-table(ardraw2$income_range, ardraw2$disability_status)
demotable<- data.table(rbind(demotable1, demotable2, demotable3, demotable4, demotable5))
#create proportions of rows - change margins = 2 if column proportions are preferred
demo_proportions <- prop.table(as.matrix(demotable), margin=1)*100
#combine variables with individual contingency tables
democomplete <- cbind(colstable, demotable, round(demo_proportions, 2))
#print(xtable(democomplete), type="latex", comment=FALSE)

print(democomplete) %>% 
  kable(digits = 3, format="pandoc", caption="demographics 1")

# create scale tables and calculate means ####
# complete subscales survey 1
ardraw2factors <- select(ardraw2, aut1, aut2, aut3, aut4, aut5, aut6, 
                 rel1, rel2, rel3, rel4, rel5, rel6,
                 com1, com2, com3, com4,
                 trans_pac1, trans_pac2, trans_pac3, trans_pac4, 
                 gse1, gse2, gse3, gse4,  
                 discr1, discr2, discr3, discr4)
#complete cases? 
ttbpn2 <- dplyr::select(ardraw2, aut1, aut2, aut3, aut4, aut5, aut6, 
                 rel1, rel2, rel3, rel4, rel5, rel6,
                 com1, com2, com3, com4)
#discriminant validity for correlations
apa.cor.table(ttbpn2, filename = "discard", table.number = 1, show.conf.interval = TRUE, landscape = FALSE)

aut2 <- select(ardraw2factors, aut1, aut2, aut3, aut4, aut5, aut6)
rel2 <- select(ardraw2factors, rel1, rel2, rel3, rel4, rel5, rel6)
com2 <- select(ardraw2factors, com1, com2, com3, com4)
pac2 <- select(ardraw2factors, trans_pac1, trans_pac2, trans_pac3, trans_pac4)
gse2 <- select(ardraw2factors, gse1, gse2, gse3, gse4)
discr <- select(ardraw2factors, discr1, discr2, discr3, discr4)
altbpnf2 <-  select(ardraw2factors, trans_pac1, trans_pac2, trans_pac3, trans_pac4, gse1, gse2, gse3, gse4,  discr1, discr2, discr3, discr4)

# mean-score-calculations removing missing values for TTBPN and BPNF
ardraw2factors$ttbpnmean <- rowMeans(ttbpn2, na.rm = TRUE)
ardraw2factors$autmean <- rowMeans(aut2, na.rm = TRUE)
ardraw2factors$relmean <- rowMeans(rel2, na.rm = TRUE)
ardraw2factors$commean <- rowMeans(com2, na.rm = TRUE)
ardraw2factors$pacmean <- rowMeans(pac2, na.rm = TRUE)
ardraw2factors$gsemean <- rowMeans(gse2, na.rm = TRUE)
ardraw2factors$discrmean <- rowMeans(discr2, na.rm = TRUE)

# scale means for correlation table
subscale_means2 <- select(ardraw2factors, autmean, relmean, commean, pacmean, discrmean, gsemean)
#overall score correlation matrix
subscales2.cor <- apa.cor.table(subscale_means2, filename = "subscale-correlations", table.number = 1, show.conf.interval = TRUE, landscape = FALSE)
subscales2.cor
# create descriptive table of items
psych::describe(ttbpn2)
psych::describe(pac2)
psych::describe(gse2)
psych::describe(discr2)
# test scale alphas  #####
coefficientalpha::alpha(ttbpn2)
alpha(pac2, use = "pairwise.complete.obs")
alpha(gse2, use = "pairwise.complete.obs")
alpha(discr2)

# assumptions/normality ####
# univariate normality
# boxplots
#boxplot(ardraw2factors$ttbpnmean ~ ardraw2$disability_status, ylab#="Score", col=c("red", "blue"), main="Boxplot of scores in 3 groups")

# checking for multivariate normal #
mvn(ttbpn2, mvnTest = c("mardia", "hz", "royston", "dh", "energy"), covariance = TRUE, tol = 1e-25, alpha = 0.5, scale = FALSE, desc = TRUE, transform = "none", univariateTest = c("SW", "CVM", "Lillie", "SF", "AD"), univariatePlot = "none", multivariatePlot = "none",  multivariateOutlierMethod = "none", bc = FALSE, bcType = "rounded", showOutliers = TRUE, showNewData = TRUE)
library(MVN)
library(BaylorEdPsych)
library (mvnmle)
LittleMCAR(ardraw2factors)
library(car)
scatterplotMatrix(subscale_means[7:9])
           #univariate plots
result <- mvn(data = ttbpn, mvnTest = "royston", univariatePlot = "histogram")


# nonnormal data transformations?
ardraw2$log.ttbpnmean<- log(ardraw2$new.ttbpnmean)
ardraw2$log.hhincome<- log(ardraw2$hhincome)
ardraw2$log.flourmean<- log(ardraw2$flourmean)
ardraw2$sq.flourmean<- ardraw2$flourmean^2
hist(ardraw2$sq.flourmean)
hist(ardraw2$log.ttbpnmean)
hist(ardraw2$log.hhincome)

# test w/ normalized data 
#calculate z scores
z.ttbpn <-scale(ttbpn)
mvn(ttbpn2)
#calculate z score for count
#z.data.p <- scale (X.p)
#one-sample z.test to see if mean is diff than specified value of pop variability
library(TeachingDemos)
z.test(na.omit(data$col), mu = 0.5, stdev = squrt(0.08))
#one-sample t-test to estimate population variance
t.test(data$col, mu = 0.5, alternative = "two.sided", conf.level = 0.95)

# cutoffs for skewness and kurtosis
library("MASS")
library("ICS")
maha1.ttbpn <- sqrt(mahalanobis(ttbpn, colMeans(ttbpn), cov(ttbpn)))
set.seed(1)
covmve.ttbpn <- cov.rob(ttbpn)
maha2.ttbpn <- sqrt(mahalanobis(ttbpn, covmve.ttbpn$center, covmve.ttbpn$cov))
max.maha.ttbpn <- max(c(maha1.ttbpn, maha2.ttbpn))
out.id <- ifelse(maha2.ttbpn <= sqrt(qchisq(0.975, 6)), 0, 1)
par(mfrow = c(1, 2), las = 1)
plot(maha1.ttbpn, xlab = "index" ,ylab = "Mahalanobis distance", ylim = c(0, max.maha.ttbpn), col = out.id + 1, pch = 15 * out.id + 1)
abline(h = sqrt(qchisq(0.975, 6)))
plot(maha2.ttbpn, xlab = "index", ylab = "robust Mahalanobis distance",
     ylim = c(0, max.maha.ttbpn), col = out.id + 1, pch = 15 * out.id + 1)
abline(h = sqrt(qchisq(0.975, 6)))
par(mfrow = c(1, 1))

# test disability group differences
#independent samples t test 
#assumes variances are not equal between groups by default
t.test(var ~ group, data = data, na.rm = TRUE, var.equal = TRUE)
# for a dependent samples t-test (within-subjects) - add "paired = TRUE"
# corrections: rm outliers of error variance


# 3-factor TTBPN model baseline with Pairwise corrs ####
cov.ttbpn2 <- cov(ttbpn2, use = "pairwise.complete.obs")
threefactor2.cfa <- '
aut=~aut1 + aut2+ aut3+ aut4+ aut5+ aut6
rel=~rel1+  rel2+ rel3+ rel4+ rel5+ rel6 
com=~com1+ com2+ com3+ com4
aut~~aut
rel~~rel
com~~com
aut~~rel
aut~~com
rel~~com
'
threefactor2.fit <- cfa(threefactor2.cfa, sample.cov = cov.ttbpn2, sample.nobs = 211, meanstructure = TRUE, std.lv=FALSE)
summary(threefactor2.fit, fit.measures=TRUE, rsquare=TRUE, standardized=TRUE, ci=TRUE)
# check mindices for highly correlated items
modificationindices(threefactor2.fit, sort=TRUE, minimum.value=3)
# check loading factors on their own
inspect(threefactor2.fit, what = "std")$lambda
# check residuals (want value < .1)
threefactor2.residuals <- resid(threefactor2.fit, type = "standardized")
# covariance of residuals
vcov(threefactor2.fit)
# print Table of Factor loadings
threefactor2param <- parameterEstimates(threefactor2.fit, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, Beta=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")
threefactor2param


# 3-factor TTBPN model baseline with MLR ####
threefactor2.cfa <- '
aut=~aut1 + aut2+ aut3+ aut4+ aut5+ aut6
rel=~rel1+  rel2+ rel3+ rel4+ rel5+ rel6 
com=~com1+ com2+ com3+ com4
aut~~aut
rel~~rel
com~~com
aut~~rel
aut~~com
rel~~com
'
threefactor2.fit <- cfa(threefactor2.cfa, data = ttbpn2, sample.nobs = 211, meanstructure = TRUE, estimator = "MLR")
summary(threefactor2.fit, fit.measures=TRUE, rsquare=TRUE, standardized=TRUE, ci=TRUE)
# check mindices for highly correlated items
modificationindices(threefactor2.fit, sort=TRUE, minimum.value=3)
# check loading factors on their own
inspect(threefactor2.fit, what = "std")$lambda
# check residuals (want value < .1)
threefactor2.residuals <- resid(threefactor2.fit, type = "standardized")
# covariance of residuals
vcov(threefactor2.fit)
# print Table of Factor loadings
threefactor2param <- parameterEstimates(threefactor2.fit, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, Beta=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")
threefactor2param


# *no discernible differences between pairwise and MLR methods*

# 3- factor modified TTBPN model ####
threefactor2mod.cfa <- '
aut=~a*aut2 + b*aut3+ c*aut5+ d*aut6
rel=~e*rel2 + f*rel3 + g*rel4+ h*rel6 
com=~i*com1 + j*com2+ k*com3+ l*com4
aut~~aut
rel~~rel
com~~com
aut~~rel
aut~~com
rel~~com
rel2~~rel3
com3~~com4
'
threefactor2mod.fit <- cfa(threefactor2mod.cfa, data = ardraw2, sample.nobs = 211, meanstructure = TRUE, estimator = "MLR", std.lv=TRUE)
# print summary w/ fit statistics
summary(threefactor2mod.fit, fit.measures=TRUE, rsquare=TRUE, standardized=TRUE, ci=TRUE)
# check mindices for highly correlated items
modificationindices(threefactor2mod.fit, sort=TRUE, minimum.value=3)
# check loading factors on their own
inspect(threefactor2mod.fit, what = "std")$lambda
# check residuals (want value < .1)
threefactor2mod.residuals <- resid(threefactor2mod.fit, type = "standardized")
# covariance of residuals
vcov(threefactor2mod.residuals)
# print Table of Factor loadings
parameterEstimates(threefactor2mod.fit, ci = TRUE, remove.nonfree = TRUE,  standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Indicator'=rhs, 
         'Beta'=std.all,
         'SE'=se, 'Z'=z, 
         'CI.Lower'=ci.lower,
         'CI.Upper'=ci.upper) %>% 
  kable(digits = 3, format="pandoc", caption="Table X: Factor Loadings")
#print diagram
semPaths(threefactor2mod.fit, what="paths", whatLabels="par")


# 3-factor ALT-bpnf model with other scales ####
alt.bpnf.threefactor2.cfa <- '
pac=~a*trans_pac1 + b*trans_pac2 + c*trans_pac3 + d*trans_pac4
gse=~e*gse1 +f*gse2 + f*gse3 +  h*gse4
discr=~i*discr1 + j*discr2+ k*discr3 + l*discr4
pac~~gse
pac~~discr
gse~~discr
'
alt.bpnf.threefactor2.fit <- cfa(alt.bpnf.threefactor2.cfa, data = ardraw2factors, sample.nobs = 211, meanstructure = TRUE, std.lv = TRUE)
summary(alt.bpnf.threefactor2.fit, fit.measures=TRUE, rsquare=TRUE, standardized=TRUE, ci=TRUE)
# check mindices for highly correlated items
modificationindices(alt.bpnf.threefactor2.fit, sort=TRUE, minimum.value=3)
# check loading factors on their own
inspect(alt.bpnf.threefactor2.fit, what = "std")$lambda
# check residuals (want value < .1)
alt.bpnf.threefactor2.residuals <- resid(bpn.threefactor2.fit, type = "standardized")
# covariance of residuals
vcov(alt.bpnf.threefactor2.fit)
# print Table of Factor loadings
parameterEstimates(alt.bpnf.threefactor2.fit, ci = TRUE, remove.nonfree = TRUE,  standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('Indicator'=rhs, 
         'Beta'=std.all,
         'SE'=se, 'Z'=z, 
         'CI.Lower'=ci.lower,
         'CI.Upper'=ci.upper) %>% 
  kable(digits = 3, format="pandoc", caption="Table X: Factor Loadings")
semPaths(alt.bpnf.threefactor2.fit, what="paths", whatLabels="par", rotation = 2, label.prop=0.9, edge.label.color = "black", edge.width = 0.8, shapeMan = "rectangle", shapeLat = "ellipse", sizeMan = 10, sizeLat = 10,  curve=1.5)


# TTBPN scale validity measures####
# TTBPN indicator and composite reliability #
# Define the model
library(lavaan)
ttbpnmod2.mod <- '
aut=~a*aut2 + b*aut3+ c*aut5+ d*aut6
rel=~e*rel2 + f*rel3 + g*rel4+ h*rel6 
com=~i*com1 + j*com2+ k*com3+ l*com4
'
# Fit the model
ttbpnmod2.fit <- cfa(ttbpnmod2.mod, data = ttbpnmod2, estimator = "MLR", WLS.V = TRUE, std.lv=TRUE)
sl <- standardizedSolution(ttbpnmod2.fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(ttbpnmod2)
sl
# Compute residual variance of each item
re <- 1 - sl^2
# Compute composite reliability
sum(sl)^2 / (sum(sl)^2 + sum(re))
# Extract the standardized loading matrix
loadMatrix <- lavInspect(ttbpnmod2.fit, what="std")$lambda
# Clear the zero loadings
loadMatrix[loadMatrix==0] <- NA
# Calculate mean squared loadings (i.e. AVEs)
apply(loadMatrix^2,2,mean, na.rm = TRUE)
library(semTools)
reliability(ttbpnmod2.fit)
discriminantValidity(ttbpnmod2.fit, cutoff = 0.9, merge = FALSE, level = 0.95)
htmt(ttbpnmod2.mod, data = ttbpnmod2, sample.cov = NULL, missing = "listwise",
     ordered = NULL, absolute = TRUE)

#aut CR calculation
#select final aut vars 
altaut2 <- select(aut2, aut2, aut3, aut5, aut6 )
aut2.model <- ' aut  =~ aut2 + aut3+ aut5+ aut6'
aut2.fit <- cfa(aut2.model, data = ttbpnmod2, meanstructure = TRUE)
sl <- standardizedSolution(aut2.fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(altaut2)
sl
# Compute residual variance of each item
re <- 1 - sl^2
# calculate AVE for aut
ave2.aut <- (sum(sl^2))/6
ave2.aut
# Compute composite reliability
sum(sl)^2 / (sum(sl)^2 + sum(re))
# Extract the standardized loading matrix
loadMatrix <- inspect(aut2.fit, "std")$lambda
# Clear the zero loadings
loadMatrix[loadMatrix==0] <- NA
# Calculate mean squared loadings (i.e. AVEs)
apply(loadMatrix^2,2,mean, na.rm = TRUE)


#rel CR calculation 
#select final rel vars 
altrel2 <- select(rel2, rel2, rel3, rel4, rel6)
rel2.model <- ' rel  =~ rel2 + rel3 + rel4+ rel6'
rel2.fit <- cfa(rel2.model, data = ttbpnmod2)
sl <- standardizedSolution(rel2.fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(altrel2)
sl
# Compute residual variance of each item
re <- 1 - sl^2
# calculate AVE for rel
ave2.rel <- (sum(sl^2))/6
ave2.rel
# Compute composite reliability
sum(sl)^2 / (sum(sl)^2 + sum(re))
# Extract the standardized loading matrix
loadMatrix <- inspect(rel2.fit, "std")$lambda
# Clear the zero loadings
loadMatrix[loadMatrix==0] <- NA
# Calculate mean squared loadings (i.e. AVEs)
apply(loadMatrix^2,2,mean, na.rm = TRUE)

#com CR calculation 
com2.model <- ' com  =~ com1 + com2 + com3 + com4'
com2.fit <- cfa(com2.model, data = ttbpnmod2)
sl <- standardizedSolution(com2.fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(com2)
sl
# Compute residual variance of each item
re <- 1 - sl^2
# calculate AVE for com
ave2.com <- (sum(sl^2))/6
ave2.com
# Compute composite comiability
sum(sl)^2 / (sum(sl)^2 + sum(re))
# Extract the standardized loading matrix
loadMatrix <- inspect(com2.fit, "std")$lambda
# Clear the zero loadings
loadMatrix[loadMatrix==0] <- NA
# Calculate mean squared loadings (i.e. AVEs)
apply(loadMatrix^2,2,mean, na.rm = TRUE)

# compute final 3-factor model validity
cr2.threef.fit <- cfa(ttbpnmod2.mod, data = ttbpnmod2, meanstructure = TRUE)
sl <- standardizedSolution(cr2.threef.fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(ttbpnmod2)
# Compute residual variance of each item
re <- 1 - sl^2
# Compute complete final scale composite reliability
cr.threef2 <- sum(sl)^2 / (sum(sl)^2 + sum(re))
# Extract the standardized loading matrix
loadMatrix <- inspect(cr2.threef.fit, "std")$lambda
# Clear the zero loadings
loadMatrix[loadMatrix==0] <- NA
# Calculate mean squared loadings (i.e. AVEs)
apply(loadMatrix^2,2,mean, na.rm = TRUE)

# create mean correlation table and test for discriminant validity by squaring correlation
ttbpn.factors <- ardraw2factors[, c("autmean", "relmean", "commean")]
corr.ttbpn2 <- cor(ttbpnmod2, use = "pairwise.complete.obs")
sq.corr.ttbpn2 <- corr.ttbpn2^2
sq.corr.ttbpn2




# ALT-BPNF scale validity measures for final ####
# Define the model
alt.bpnf2.model <- '
pac=~trans_pac1 + trans_pac2 + trans_pac3 + trans_pac4
gse=~gse1 +gse2 + gse3 +  gse4
discr=~discr1 + discr2+ discr3 + discr4
pac~~gse
pac~~discr
gse~~discr
'
# Fit the model
alt.bpnf2.fit <- cfa(alt.bpnf2.model, data = altbpnf2)
sl <- standardizedSolution(alt.bpnf2.fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(altbpnf2)
sl
# Compute residual variance of each item
re <- 1 - sl^2
# Compute composite reliability
sum(sl)^2 / (sum(sl)^2 + sum(re))
# Extract the standardized loading matrix
loadMatrix <- inspect(alt.bpnf2.fit, "std")$lambda
# Clear the zero loadings
loadMatrix[loadMatrix==0] <- NA
# Calculate mean squared loadings (i.e. AVEs)
apply(loadMatrix^2,2,mean, na.rm = TRUE)

#pac CR calculation
pac.model <- 'pac=~trans_pac1 + trans_pac2 + trans_pac3 + trans_pac4'
pac.fit <- cfa(pac.model, data = altbpnf2, meanstructure = TRUE)
sl <- standardizedSolution(pac.fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(pac)
sl
# Compute residual variance of each item
re <- 1 - sl^2
# calculate AVE for aut
ave.pac <- (sum(sl^2))/6
ave.pac
# Compute composite reliability
sum(sl)^2 / (sum(sl)^2 + sum(re))
# Extract the standardized loading matrix
loadMatrix <- inspect(pac.fit, "std")$lambda
# Clear the zero loadings
loadMatrix[loadMatrix==0] <- NA
# Calculate mean squared loadings (i.e. AVEs)
apply(loadMatrix^2,2,mean, na.rm = TRUE)


#gse CR calculation 
gse.model <- 'gse=~gse1 +gse2 + gse3 +  gse4'
gse.fit <- cfa(gse.model, data = gse2)
sl <- standardizedSolution(gse.fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(gse)
sl
# Compute residual variance of each item
re <- 1 - sl^2
# calculate AVE for rel
alt.ave.gse <- (sum(sl^2))/6
alt.ave.gse
# Compute composite reliability
sum(sl)^2 / (sum(sl)^2 + sum(re))
# Extract the standardized loading matrix
loadMatrix <- inspect(gse.fit, "std")$lambda
# Clear the zero loadings
loadMatrix[loadMatrix==0] <- NA
# Calculate mean squared loadings (i.e. AVEs)
apply(loadMatrix^2,2,mean, na.rm = TRUE)

#discr CR calculation 
discr.model <- '
discr=~discr1 + discr2+ discr3 + discr4'
discr.fit <- cfa(discr.model, data = discr2)
sl <- standardizedSolution(discr.fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(discr)
sl
# Compute residual variance of each item
re <- 1 - sl^2
# calculate AVE for discr
ave.discr <- (sum(sl^2))/6
ave.discr
# Compute composite comiability
sum(sl)^2 / (sum(sl)^2 + sum(re))
# Extract the standardized loading matrix
loadMatrix <- inspect(discr.fit, "std")$lambda
# Clear the zero loadings
loadMatrix[loadMatrix==0] <- NA
# Calculate mean squared loadings (i.e. AVEs)
apply(loadMatrix^2,2,mean, na.rm = TRUE)

# create mean correlation table and test for discriminant validity by squaring correlation
altbpn2.factors<- ardraw2factors[, c("pacmean", "gsemean", "discrmean")]
corr.altbpn2 <- cor(altbpn2.factors)
sq.corr.altbpn2 <- corr.altbpn2^2
sq.corr.altbpn2



# Kitchen Sink ####
fullfactor.cfa <- '
aut=~aut1 + aut2+ aut3+ aut4+ aut5+ aut6
rel=~rel1+  rel2+ rel3+ rel4+ rel5+ rel6 
com=~com1+ com2+ com3+ com4
pac=~trans_pac1 + trans_pac2 + trans_pac3 + trans_pac4
gse=~gse1 +gse2 + gse3 +  gse4
discr=~discr1 + discr2+ discr3 + discr4
aut~~rel
aut~~com
aut~~gse
aut~~pac
aut~~discr
rel~~com
rel~~gse
rel~~pac
rel~~discr
com~~gse
com~~pac
com~~discr
gse~~pac
gse~~discr
pac~~discr
'
fullfactor.fit <- cfa(fullfactor.cfa, data = ardraw2, sample.nobs = 211, meanstructure = TRUE, estimator = "MLR")
summary(fullfactor.fit, fit.measures=TRUE, rsquare=TRUE, standardized=TRUE, ci=TRUE)
# check mindices for highly correlated items
modificationindices(fullfactor.fit, sort=TRUE, minimum.value=3)
# check loading factors on their own
inspect(fullfactor.fit, what = "std")$lambda
# check residuals (want value < .1)
fullfactor.residuals <- resid(fullfactor.fit, type = "standardized")
# covariance of residuals
vcov(fullfactor.fit)
# print Table of Factor loadings
fullfactorparam <- parameterEstimates(fullfactor.fit, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select(`Latent Factor`=lhs, Indicator=rhs, B=est, SE=se, Z=z, `p-value`=pvalue, Beta=std.all) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings")
threefactor2param