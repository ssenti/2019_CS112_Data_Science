
###questionnn 1

x <- rnorm(999, 100, 20)
x1 <- 5*x + rnorm(999, 0, 10)

outlier <- 400

df1 <- data.frame(x1)

x_outl <- c(outlier, x)
x2 <- c(-20*outlier, 5*x) + rnorm(1000, 0, 10)
df2 <- data.frame(x2)

lm1_q1 <- lm(x1 ~ x, data = df1)
lm2_q1 <- lm(x2 ~ x_outl, data =df2)

summary(lm1_q1)
summary(lm2_q1)

plot(x_outl, x2,
     main = "Regression of 999 vs 1000 Data Points",
     xlab = "Independent Variables",
     ylab = "Dependent Variables")
abline(lm1_q1)
abline(lm2_q1, col = "red")

###question 2
#referenced Vini Miranda's code: https://gist.githubusercontent.com/viniciusmss/
#a52a9630abd50fa1237060df995f8657/raw/bdd6410bcbf21a3bb1059798b92008b613235b8e/
#Statistics%2520Scavenger%2520Hunt.R?fbclid=IwAR36swGGUYk_yURlN92YIoEvu4Yr8xmw3NOpomq8-oOj10DELfzeeZaaWxA

library(arm)
library(Matching)

data("lalonde")
l_ctrl = lalonde[which(lalonde$treat == 0), ]

lm_q1 <- lm(re78 ~ age + educ + re74 + re75 + I(educ*re74) + I(educ*re75) + I(age*re74) + I(age*re75) + I(age*age) + I(re74*re75), data = l_ctrl)

get_expected <- function(coefs, person) {
  result <- coefs[1] + person[1]*coefs[2] + person[2]*coefs[3] +
    person[3]*coefs[4] + person[4]*coefs[5] + (person[2]*person[3])*coefs[6] +
    (person[2]*person[4])*coefs[7] + (person[1]*person[3])*coefs[8] +
    (person[1]*person[4])*coefs[9] + (person[1]*person[1])*coefs[10] +
    (person[3]*person[4])*coefs[11]
  return(result)
}

get_predicted <- function(coefs, sig, person) {
  result <- coefs[1] + person[1]*coefs[2] + person[2]*coefs[3] +
    person[3]*coefs[4] + person[4]*coefs[5] + (person[2]*person[3])*coefs[6] +
    (person[2]*person[4])*coefs[7] + (person[1]*person[3])*coefs[8] +
    (person[1]*person[4])*coefs[9] + (person[1]*person[1])*coefs[10] +
    (person[3]*person[4])*coefs[11] + sig
  return(result)
}

sim_lm_q1 <- sim(lm_q1, 10000)

storage1 <- matrix(NA, nrow = 10000, ncol = 39)
for (age in c(17:55)) {
  for (i in 1:10000)
  {
    person_median <- c(age, median(l_ctrl$educ), median(l_ctrl$re74), median(l_ctrl$re75))
    storage1[i, age - 16] <- get_expected(sim_lm_q1@coef[i, ], person_median)
  }
}

storage2 <- matrix(NA, nrow = 10000, ncol = 39)
for (age in c(17:55)) {
  for (i in 1:10000)
  {
    person_75quant <- c(age, quantile(l_ctrl$educ, probs = 0.75), quantile(l_ctrl$re74, probs = 0.75), quantile(l_ctrl$re75, probs = 0.75))
    storage2[i, age - 16] <- get_expected(sim_lm_q1@coef[i, ], person_75quant)
  }
}

storage3 <- matrix(NA, nrow = 10000, ncol = 39)
for (age in c(17:55)) {
  for (i in 1:10000)
  {
    person_median <- c(age, median(l_ctrl$educ), median(l_ctrl$re74), median(l_ctrl$re75))
    storage3[i, age - 16] <- get_predicted(sim_lm_q1@coef[i, ], sim_lm_q1@sigma[i], person_median)
  }
}

storage4 <- matrix(NA, nrow = 10000, ncol = 39)
for (age in c(17:55)) {
  for (i in 1:10000)
  {
    person_75quant <- c(age, quantile(l_ctrl$educ, probs = 0.75), quantile(l_ctrl$re74, probs = 0.75), quantile(l_ctrl$re75, probs = 0.75))
    storage4[i, age - 16] <- get_predicted(sim_lm_q1@coef[i, ], sim_lm_q1@sigma[i], person_75quant)
  }
}

conf_exp_med <- apply(storage1, 2, quantile, probs = c(0.025, 0.975))
conf_exp_quant <- apply(storage2, 2, quantile, probs = c(0.025, 0.975))
conf_pred_med <- apply(storage3, 2, quantile, probs = c(0.025, 0.975))
conf_pred_quant <- apply(storage4, 2, quantile, probs = c(0.025, 0.975))

plot(x = c(1:20000), y = c(1:20000), type = "n", xlim = c(17,55), ylim = c(0,20000),
     main = "Re78 CI Expected", xlab = "Age",
     ylab = "Re78")

for (age in 17:55) {
  segments(
    x0 = age,
    y0 = conf_exp_med[1, age - 16],
    x1 = age,
    y1 = conf_exp_med[2, age - 16],
    lty = 2,
    lwd = 2)
}
for (age in 17:55) {
  segments(
    x0 = age,
    y0 = conf_exp_quant[1, age - 16],
    x1 = age,
    y1 = conf_exp_quant[2, age - 16],
    col = "green",
    lwd = 2)
}

plot(x = c(1:20000), y = c(1:20000), type = "n", xlim = c(17,55), ylim = c(0,20000),
     main = "Re78 CI Predicted", xlab = "Age",
     ylab = "Re78")

for (age in 17:55) {
  segments(
    x0 = age,
    y0 = conf_pred_med[1, age - 16],
    x1 = age,
    y1 = conf_pred_med[2, age - 16],
    lty = 2,
    lwd = 2)
}
for (age in 17:55) {
  segments(
    x0 = age,
    y0 = conf_pred_quant[1, age - 16],
    x1 = age,
    y1 = conf_pred_quant[2, age - 16],
    col = "yellow",
    lwd = 2)
}

table_q2 <- data.frame(t(conf_exp_med), t(conf_exp_quant), t(conf_pred_med), t(conf_pred_quant))
colnames(table_q2) <- c("Expected CI Median Lower", "Expected CI Median Upper", "Expected CI Quant75% Lower",
                        "Expected CI Quant75% Upper", "Predicted CI Median Lower", "Predicted CI Median Upper", "Predicted CI Quant75% Lower", "Predicted CI Quant75% Lower")
write.csv(table_q2,'table_q2.csv')

###question 3

library(datasets)
data("PlantGrowth")

pg <- PlantGrowth[which(PlantGrowth$group == "trt1" | PlantGrowth$group == "ctrl"), ]

pg$group <- as.numeric(pg$group)
pg$group <- pg$group - 1

lm_q3 <- lm(weight ~ group, data = pg)

n = length(pg$group)
B = 10000
result = rep(NA, B)
for (i in 1:B) {
  sample_q3 <- sample(n, replace = TRUE)
  result[i] <- summary(lm(weight ~ group, data = pg[sample_q3, ]))$coef[2]
}

quantile(result, probs = c(0.025, 0.975))

summary(result)
summary(confint(lm_q3))

confint(lm_q3)

hist(result,
     main="Bootstrapped Coefficients",
     xlab="Coefficients")

###question 4

library(datasets)
data("PlantGrowth")

pg <- PlantGrowth[which(PlantGrowth$group == "trt1" | PlantGrowth$group == "ctrl"), ]

sample_q4 <- sample(length(pg$group), replace = T)
lm_q4 <- lm(weight ~ group, data = pg[sample_q4, ])
predicted_y <- predict(lm_q4)
y <- pg[sample_q4,]$weight

r_sqr <- 1 - (sum((y - predicted_y) ^ 2) / sum((y - mean(y)) ^ 2))

r_sqr


###question 5

library(foreign)
nsw <- read.dta("nsw.dta")

glm_q5 <- glm(treat ~ age + education + black + hispanic + married + nodegree + re75, data = nsw, family = binomial)

glm_prob = predict(glm_q5, type="response")
nsw$prob <- glm_prob

hist(nsw[which(nsw$treat == 1),]$prob,
     main="Probabilities for Treatment (Treatment Group)",
     xlab="Probability",
     col="red",
     xlim=c(0,1),
     ylim=c(0, 200))

hist(nsw[which(nsw$treat == 0),]$prob,
     main="Probabilities for Treatment (Control Group)",
     xlab="Probability",
     col="blue",
     xlim=c(0,1),
     ylim=c(0, 200))

summary(nsw[which(nsw$treat == 1),]$prob)
summary(nsw[which(nsw$treat == 0),]$prob)
