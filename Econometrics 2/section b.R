setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(haven)
library(tidyverse)
library(psych)
library(mfx)
library(margins)

data = read_dta("Smoking.dta")

# Task 1
mean(data$smoker)

# Task 2
smoker = data %>% filter(smoker == 1)
smoker %>% describe()
nonSmoker = data %>% filter(smoker == 0)
nonSmoker %>% describe()

sum(data$age > 78)

# Task 3
t.test(smoker$hsdrop, nonSmoker$hsdrop, var.equal = TRUE)
t.test(smoker$female, nonSmoker$female, var.equal = TRUE)

# Task 4
# smoking is not randomly assigned -> simple DiD doesnt work. Maybe use an IV approach?
# 
# other challenges: amount of smoking has an effect (1 cigarette a week vs 10 cigarettes a day)

# Task 5
affected = data %>% filter(smoker == 1 & smkban == 1)
notAffected = data %>% filter(smoker == 0 | smkban == 0)
t.test(affected$smoker, notAffected$smoker, var.equal = TRUE)
# logischerweise ist die smoker rate bei affected 100% (per Definition). Macht aber keinen
# Sinn in Bezug auf Task 6, deswegen split des Datensatzes bei smkban
smokeBan = data %>% filter(smkban == 1)
noSmokeBan = data %>% filter(smkban == 0)
t.test(smokeBan$smoker, noSmokeBan$smoker, var.equal = TRUE)
# effect: -0.0775584

# Task 6
lpm = lm(smoker ~ ., data = data)
summary(lpm)
# effect -0.0453435
# non smoking area attracts more non smokers -> diff is higher than in LPM
# because smoke ban is known beforehand -> pre-emptive behaviour

# Task 7
describe(lpm$fitted.values)
# min = -0.08 => negative probabilities!
sum(lpm$fitted.values < 0) # 34 people have a negative prob

# Task 8
lpmProbit = glm(smoker ~ ., data = data, family = binomial(link = "probit"))
summary(lpmProbit)
# coef for smkban is -0.151762
# only sign interpretable => smokeban reduces smokers

# Task 9
probitmfx(smoker ~ ., data = data, atmean = TRUE)
# MEA = -0.04652485
summary(margins(lpmProbit, type = "response"))
# average marginal effect = -0.0449
# not the same, MEA is "treatment" effect for a average smoker
# AME is average "treatment" effect for all people in dataset

# Task 10
# effect estimated by LPM is very close to effects estimated by probit model




