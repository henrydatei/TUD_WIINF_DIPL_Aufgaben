####################################################
# Section C
####################################################
library(ggplot2)

# Constants
set.seed(12956)
observations = 1000
gamma0 = 1.2
gamma1 = 0.015
gamma2 = -0.02
gamma3 = -0.01

m1beta = c()
m2beta = c()
m3beta = c()
m4beta = c()
m5beta = c()

for (i in 1:1000) {
  # Data Generation
  errors = rnorm(observations, mean = 0, sd = 0.55)
  age = as.integer(runif(observations, min = 20, max = 66))
  rho = 0.5 - 0.25 * log(age - 19)/log(46)
  female = rbinom(observations, 1, rho)
  mu = 0.02 + 0.06 * ifelse(age > 43, 1, 0)
  tau = rnorm(observations, mean = mu, sd = sqrt(0.01))
  rho2 = 0.25 + 0.5 * log(age - 19)/log(46)
  W = rbinom(observations, 1, rho2)
  
  Yzero = gamma0 + gamma1 * age + gamma2 * female + gamma3 * age * female + errors
  Y = Yzero + tau * W
  
  # Model 1
  m1 = lm(Y ~ W)
  # summary(m1)
  m1beta = c(m1beta, unname(m1$coefficients[2]))
  
  # Model 2
  m2 = lm(Y ~ W + age + female + age*female)
  # summary(m2)
  m2beta = c(m2beta, unname(m2$coefficients[2]))
  
  # Model 3
  agematrix = matrix(0, nrow = observations, ncol = 65-20+1)
  for (i in 1:observations) {
    currentAge = age[i]
    agematrix[i, currentAge - 20 + 1] = 1
  }
  m3 = lm(Y ~ W + agematrix + 0)
  # summary(m3)
  m3beta = c(m3beta, unname(m3$coefficients[1]))
  
  # Model 4
  logitModel = glm(W ~ age, family = binomial(logit))
  # summary(logitModel)
  propScore = predict.glm(logitModel, newdata = as.list(age), type = "response")
  lambda = 1/(propScore^W * (1 - propScore)^(1-W))
  m4 = lm(Y ~ W, weights = lambda)
  # summary(m4)
  m4beta = c(m4beta, unname(m4$coefficients[2]))
  
  # Model 5
  logitModel2 = glm(W ~ agematrix + 0, family = binomial(logit))
  # summary(logitModel2)
  propScore2 = predict.glm(logitModel2, newdata = as.list(age), type = "response")
  lambda2 = 1/(propScore2^W * (1 - propScore2)^(1-W))
  m5 = lm(Y ~ W, weights = lambda2)
  # summary(m5)
  m5beta = c(m5beta, unname(m5$coefficients[2]))
}

ggplot() + 
  geom_density(aes(m1beta, color = "M1")) + 
  geom_density(aes(m2beta, color = "M2")) + 
  geom_density(aes(m3beta, color = "M3")) + 
  geom_density(aes(m4beta, color = "M4")) + 
  geom_density(aes(m5beta, color = "M5")) + 
  xlab("estimate of tau") + 
  geom_vline(xintercept = 0.05)




####################################################
# Section D
####################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(haven)
library(naniar)
library(ggplot2)
library(tidyverse)
library(GGally)

data = read_dta("lfs_2010_2019_ages1564_20per.dta")

# vis_miss(data, warn_large_data = FALSE) # takes forever
gg_miss_var(data) + labs(y = "Look at all the missing ones")

# Task 2
employmentRateYearly = data.frame(cbind(2010:2019, matrix(0, nrow = 10, ncol = 10)))
colnames(employmentRateYearly) = c("year", "NFL", "PEI", "NS", "NB", "QUE", "ONT", "MAN", "SASK", "ALB", "BC")
for (prov in 1:10) {
  empRateForYear = c()
  for (y in 2010:2019) {
    employed = data %>% filter(province == prov) %>% filter(year == y) %>% filter(between(agegrp, 1, 2)) %>% filter(between(empstat, 1, 2)) %>% nrow()
    # I've seen the post in the forum to late stating that you define employment rate as employed/all population. Literature usually doesn't count people that are not in the workforce as unemployed
    population = data %>% filter(province == prov) %>% filter(year == y) %>% filter(between(agegrp, 1, 2)) %>% filter(between(empstat, 1, 3)) %>% nrow()
    empRateForYear = c(empRateForYear, employed/population)
  }
  if (prov == 1) employmentRateYearly$NFL = empRateForYear
  if (prov == 2) employmentRateYearly$PEI = empRateForYear
  if (prov == 3) employmentRateYearly$NS = empRateForYear
  if (prov == 4) employmentRateYearly$NB = empRateForYear
  if (prov == 5) employmentRateYearly$QUE = empRateForYear
  if (prov == 6) employmentRateYearly$ONT = empRateForYear
  if (prov == 7) employmentRateYearly$MAN = empRateForYear
  if (prov == 8) employmentRateYearly$SASK = empRateForYear
  if (prov == 9) employmentRateYearly$ALB = empRateForYear
  if (prov == 10) employmentRateYearly$BC = empRateForYear
}
ggplot(employmentRateYearly, aes(x = year)) + 
  # geom_line(aes(y = NFL, color = "NFL")) +
  # geom_line(aes(y = PEI, color = "PEI")) +
  # geom_line(aes(y = NS, color = "NS")) +
  # geom_line(aes(y = NB, color = "NB")) +
  geom_line(aes(y = QUE, color = "QUE")) +
  geom_line(aes(y = ONT, color = "ONT"), size = 2) +
  geom_line(aes(y = MAN, color = "MAN")) + 
  # geom_line(aes(y = SASK, color = "SASK")) +
  # geom_line(aes(y = ALB, color = "ALB")) +
  # geom_line(aes(y = BC, color = "BC")) +
  ylab("Employment Rate") + 
  xlab("Year") + 
  scale_x_continuous(breaks = seq(2010, 2019, by = 1)) +
  scale_y_continuous(breaks = seq(0.8, 1, by = 0.01)) + 
  geom_vline(xintercept = 2018)
employmentRateMonthly = data.frame(cbind(1:12, rep(0, 12), rep(0, 12), rep(0, 12)))
colnames(employmentRateMonthly) = c("Month", "QUE", "ONT", "MAN")
for (prov in 5:7) {
  empRateForMonth = c()
  for (m in 1:12) {
    employed = data %>% filter(province == prov) %>% filter(month == m) %>% filter(between(agegrp, 1, 2)) %>% filter(between(empstat, 1, 2)) %>% nrow()
    population = data %>% filter(province == prov) %>% filter(month == m) %>% filter(between(agegrp, 1, 2)) %>% filter(between(empstat, 1, 3)) %>% nrow()
    empRateForMonth = c(empRateForMonth, employed/population)
  }
  if (prov == 5) employmentRateMonthly$QUE = empRateForMonth
  if (prov == 6) employmentRateMonthly$ONT = empRateForMonth
  if (prov == 7) employmentRateMonthly$MAN = empRateForMonth
}
ggplot(employmentRateMonthly, aes(x = Month)) + 
  geom_line(aes(y = QUE, color = "QUE")) +
  geom_line(aes(y = ONT, color = "ONT"), size = 2) +
  geom_line(aes(y = MAN, color = "MAN")) + 
  ylab("Employment Rate") + 
  xlab("Month") + 
  scale_x_continuous(breaks = seq(1, 12, by = 1)) +
  scale_y_continuous(breaks = seq(0.1, 1, by = 0.01))

# Task 3
df = data %>%
  filter(year == 2017 | year == 2018) %>%
  filter(agegrp == 1 | agegrp == 2) %>% # assuming we should include only 15-24s bc these are mostly affected by minimum wage raise
  filter(province == 5 | province == 6)
df$employed = ifelse(df$empstat == 1 | df$empstat == 2, 1, 0)
df$D = ifelse(df$province == 6, 1, 0)
df$T = ifelse(df$year == 2018, 1, 0)

# Model 1
m1 = lm(employed ~ D + T + D*T, data = df)
summary(m1)

# Model 2
monthmatrix = matrix(0, nrow = nrow(df), ncol = 12)
for (i in 1:nrow(df)) {
  currentMonth = as.integer(df$month[i])
  monthmatrix[i, currentMonth] = 1
}
monthmatrix = monthmatrix[,-1] # base month = January
m2 = lm(employed ~ D + T + D*T + monthmatrix, data = df)
summary(m2)

# Model 3
# remove non-full columns
fullColumns = c()
for (colname in colnames(df)) {
  values = df[colname] %>% na.omit() %>% nrow()
  if (values == df[colname] %>% nrow()) {
    fullColumns = c(fullColumns, colname)
  }
}
df = df[fullColumns]
ggcorr(df, label = TRUE)
# + married, edugrp, efamtype, agegrp
m3 = lm(employed ~ D + T + D*T + monthmatrix + edugrp + married + efamtype + agegrp, data = df)
summary(m3)

# Task 4
df = data %>%
  filter(between(year, 2014, 2019)) %>%
  filter(agegrp == 1 | agegrp == 2) %>%
  filter(province == 5 | province == 6)
df$employed = ifelse(df$empstat == 1 | df$empstat == 2, 1, 0)
df$D = ifelse(df$province == 6, 1, 0)
timematrix = matrix(0, nrow = nrow(df), ncol = 6)
colnames(timematrix) = c("2014", "2015", "2016", "2017", "2018", "2019")
for (i in 1:nrow(df)) {
  currentYear = as.integer(df$year[i])
  timematrix[i, currentYear - 2014 + 1] = 1
}
timematrix = timematrix[,-4] # base year = 2017
monthmatrix = matrix(0, nrow = nrow(df), ncol = 12)
for (i in 1:nrow(df)) {
  currentMonth = as.integer(df$month[i])
  monthmatrix[i, currentMonth] = 1
}
monthmatrix = monthmatrix[,-1] # base month = January
m4 = lm(employed ~ D + timematrix + monthmatrix + timematrix*D, data = df)
summary(m4)
estimators = data.frame(2014:2019)
colnames(estimators) = "year"
estimators$beta = c(coef(m4)[19], coef(m4)[20], coef(m4)[21], 0, coef(m4)[22], coef(m4)[23])
estimators$se = c(coef(summary(m4))[, "Std. Error"][19], coef(summary(m4))[, "Std. Error"][20], coef(summary(m4))[, "Std. Error"][21], 0, coef(summary(m4))[, "Std. Error"][22], coef(summary(m4))[, "Std. Error"][23])
estimators$ci = qnorm(0.975) * estimators$se
ggplot(estimators, aes(x = year, y = beta)) +
  geom_errorbar(aes(ymin = beta-ci, ymax = beta+ci), width = 0.1) +
  geom_line(col = "blue") + 
  geom_point(col = "blue") + 
  geom_vline(xintercept = 2017.5, linetype = "dotted")

# Task 6
employmentRateMonthly2017 = data.frame(cbind(1:12, rep(0, 12), rep(0, 12)))
colnames(employmentRateMonthly2017) = c("Month", "QUE", "ONT")
for (prov in 5:6) {
  empRateForMonth = c()
  for (m in 1:12) {
    employed = data %>% filter(province == prov) %>% filter(month == m) %>% filter(year == 2017) %>% filter(between(agegrp, 1, 2)) %>% filter(between(empstat, 1, 2)) %>% nrow()
    population = data %>% filter(province == prov) %>% filter(month == m) %>% filter(year == 2017) %>% filter(between(agegrp, 1, 2)) %>% filter(between(empstat, 1, 3)) %>% nrow()
    empRateForMonth = c(empRateForMonth, employed/population)
  }
  if (prov == 5) employmentRateMonthly2017$QUE = empRateForMonth
  if (prov == 6) employmentRateMonthly2017$ONT = empRateForMonth
}
ggplot(employmentRateMonthly2017, aes(x = Month)) + 
  geom_line(aes(y = QUE, color = "QUE")) +
  geom_line(aes(y = ONT, color = "ONT"), size = 2) +
  ylab("Employment Rate") + 
  xlab("Month of 2017") + 
  scale_x_continuous(breaks = seq(1, 12, by = 1)) +
  scale_y_continuous(breaks = seq(0.1, 1, by = 0.01)) + 
  geom_vline(xintercept = 6)

# Task 8
# step(lm(employed ~ . -year - date - province + D:T, data = df))



####################################################
# Section E
####################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(haven)
library(Synth)
library(tidyverse)
library(ggplot2)

data = read_dta("lfs_2010_2019_ages1564_20per.dta") %>% filter(agegrp <= 2)
data$employed = ifelse(data$empstat <= 2, 1, 0)
data$male = ifelse(data$sex == 1, 1, 0)
data$edu1 = ifelse(data$edugrp == 1, 1, 0)
data$edu2 = ifelse(data$edugrp == 2, 1, 0)
data$edu3 = ifelse(data$edugrp == 3, 1, 0)
efamtype2fam = function(efam) {
  if (efam == 1) return(1)
  if (efam %in% 2:4) return(2)
  if (efam %in% c(14, 16, 17)) return(3)
  if (efam %in% 5:10) return(4)
  if (efam %in% c(11, 12, 13, 15)) return(5)
  if (efam == 18) return(6)
}
data$fam = sapply(as.integer(data$efamtype), efamtype2fam)
table(data$efamtype, data$fam)
data$fam1 = ifelse(data$fam == 1, 1, 0)
data$fam2 = ifelse(data$fam == 2, 1, 0)
data$fam3 = ifelse(data$fam == 3, 1, 0)
data$fam4 = ifelse(data$fam == 4, 1, 0)
data$fam5 = ifelse(data$fam == 5, 1, 0)
data$fam6 = ifelse(data$fam == 6, 1, 0)
dataProvinceYear = data.frame(expand.grid(2010:2019, 1:10), matrix(0, nrow = 100, ncol = 11))
colnames(dataProvinceYear) = c("year", "province", "employed", "male", "edu1", "edu2", "edu3", "fam1", "fam2", "fam3", "fam4", "fam5", "fam6")
row = 1
for (prov in 1:10) {
  for (y in 2010:2019) {
    df = data %>% filter(year == y) %>% filter(province == prov)
    dataProvinceYear[row, "employed"] = weighted.mean(df$employed, df$wgt)
    dataProvinceYear[row, "male"] = weighted.mean(df$male, df$wgt)
    dataProvinceYear[row, "edu1"] = weighted.mean(df$edu1, df$wgt)
    dataProvinceYear[row, "edu2"] = weighted.mean(df$edu2, df$wgt)
    dataProvinceYear[row, "edu3"] = weighted.mean(df$edu3, df$wgt)
    dataProvinceYear[row, "fam1"] = weighted.mean(df$fam1, df$wgt)
    dataProvinceYear[row, "fam2"] = weighted.mean(df$fam2, df$wgt)
    dataProvinceYear[row, "fam3"] = weighted.mean(df$fam3, df$wgt)
    dataProvinceYear[row, "fam4"] = weighted.mean(df$fam4, df$wgt)
    dataProvinceYear[row, "fam5"] = weighted.mean(df$fam5, df$wgt)
    dataProvinceYear[row, "fam6"] = weighted.mean(df$fam6, df$wgt)
    row = row + 1
  }
}

dataPrep = dataprep(dataProvinceYear, 
                    predictors = "employed", 
                    dependent = "employed", 
                    unit.variable = "province",
                    time.variable = "year",
                    treatment.identifier = 6,
                    controls.identifier = c(1:5, 7:10),
                    time.predictors.prior = 2010:2017,
                    time.optimize.ssr = 2010:2017,
                    time.plot = 2010:2019)
synthResult = synth(dataPrep, optimxmethod = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "nlm", "nlminb"))
path.plot(synthResult, dataPrep, Ylim = c(0.46, 0.57), Ylab = "employed", tr.intake = 2018, Legend.position = c("topleft"), Main = "Model 1")

dataPrep2 = dataprep(dataProvinceYear, 
                     predictors = "employed", 
                     dependent = "employed", 
                     unit.variable = "province",
                     time.variable = "year",
                     treatment.identifier = 6,
                     controls.identifier = c(1:5,7:10),
                     time.predictors.prior = 2014:2016,
                     time.optimize.ssr = 2010:2017,
                     time.plot = 2010:2019)
synthResult2 = synth(dataPrep2, optimxmethod = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "nlm", "nlminb"))
path.plot(synthResult2, dataPrep2, Ylim = c(0.49, 0.6), Ylab = "employed", tr.intake = 2018, Legend.position = c("topleft"), Main = "Model 2")

dataPrep3 = dataprep(dataProvinceYear, 
                     predictors = c("male", "edu2", "edu3", "fam2", "fam3", "fam4", "fam5", "fam6"), 
                     dependent = "employed", 
                     unit.variable = "province",
                     time.variable = "year",
                     treatment.identifier = 6,
                     controls.identifier = c(1:5,7:10),
                     time.predictors.prior = 2010:2017,
                     time.optimize.ssr = 2010:2017,
                     time.plot = 2010:2019)
synthResult3 = synth(dataPrep3, optimxmethod = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "nlm", "nlminb"))
path.plot(synthResult3, dataPrep3, Ylim = c(0.49, 0.57), Ylab = "employed", tr.intake = 2018, Legend.position = c("topleft"), Main = "Model 3")

dataPrep4 = dataprep(dataProvinceYear, 
                     predictors = c("male", "edu2", "edu3", "fam2", "fam3", "fam4", "fam5", "fam6"), 
                     dependent = "employed", 
                     unit.variable = "province",
                     time.variable = "year",
                     treatment.identifier = 6,
                     controls.identifier = c(1:5,7:10),
                     time.predictors.prior = 2014:2016,
                     time.optimize.ssr = 2010:2017,
                     time.plot = 2010:2019)
synthResult4 = synth(dataPrep4, optimxmethod = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "nlm", "nlminb"))
path.plot(synthResult4, dataPrep4, Ylim = c(0.49, 0.57), Ylab = "employed", tr.intake = 2018, Legend.position = c("topleft"), Main = "Model 4")

# plot for weights
model = c(rep("Model 1", 9), rep("Model 2", 9), rep("Model 3", 9), rep("Model 4", 9))
province = rep(c("NFL", "PEI", "NS", "NB", "QUE", "MAN", "SASK", "ALB", "BC"),4)
weight = c(synthResult$solution.w[,1], synthResult2$solution.w[,1], synthResult3$solution.w[,1], synthResult4$solution.w[,1])
weights = data.frame(model, province, weight)
colnames(weights) = c("model", "province", "weight")
ggplot(weights, aes(fill=province, y = weight, x = model)) + 
  geom_bar(position="stack", stat="identity")
