modified Koehler, S., & König, T. (2014)'s Code. doi:10.1017/psrm.2014.26

library(Matching)

foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")

# remove rows with missing data
foo <- foo[c(-4, -16, -19, -47, -84, -93, -98), ]


# Q2

# logistic regression for normal and including interaction term
glm1 <- glm(pbs2s3~wartype+logcost+factnum+factnum2+trnsfcap
            +treaty+develop+exp+decade+ wardur+untype4,
            data = foo, family=binomial)
glm2 <- glm(pbs2s3~wartype+logcost+factnum+factnum2+trnsfcap
            +treaty+develop+exp+decade+I(logcost*untype4)+wardur+untype4,
            data = foo, family=binomial)

storage <- rep(0,320)
storage_int <- rep(0,320)

# put treatment effects into storage
for (wardur in 0:320) {
  x_treat <- data.frame("wartype"=mean(foo$wartype), "logcost"=mean(foo$logcost), "factnum"=mean(foo$factnum), "factnum2"=mean(foo$factnum2),
                        "trnsfcap"=mean(foo$trnsfcap), "treaty"=mean(foo$treaty), "develop"=mean(foo$develop),
                        "exp"=mean(foo$exp), "decade"=mean(foo$decade), "wardur"=wardur, "untype4" = 1)
  x_control <- data.frame("wartype"=mean(foo$wartype), "logcost"=mean(foo$logcost), "factnum"=mean(foo$factnum), "factnum2"=mean(foo$factnum2),
                          "trnsfcap"=mean(foo$trnsfcap), "treaty"=mean(foo$treaty), "develop"=mean(foo$develop),
                          "exp"=mean(foo$exp), "decade"=mean(foo$decade), "wardur"=wardur, "untype4" = 0)
  
  y_treat <- predict(glm1, x_treat, type="response")
  y_control <- predict(glm1, x_control,type="response")
  storage[wardur] <- y_treat - y_control
}

for (wardur in 0:320) {
  x_treat_int <- data.frame("wartype"=mean(foo$wartype), "logcost"=mean(foo$logcost), "factnum"=mean(foo$factnum), "factnum2"=mean(foo$factnum2),
                        "trnsfcap"=mean(foo$trnsfcap), "treaty"=mean(foo$treaty), "develop"=mean(foo$develop),
                        "exp"=mean(foo$exp), "decade"=mean(foo$decade), "I(logcost*untype4)"=mean(I(foo$logcost*1)), "wardur"=wardur, "untype4" = 1)
  x_control_int <- data.frame("wartype"=mean(foo$wartype), "logcost"=mean(foo$logcost), "factnum"=mean(foo$factnum), "factnum2"=mean(foo$factnum2),
                          "trnsfcap"=mean(foo$trnsfcap), "treaty"=mean(foo$treaty), "develop"=mean(foo$develop),
                          "exp"=mean(foo$exp), "decade"=mean(foo$decade), "I(logcost*untype4)"=mean(I(foo$logcost*0)),"wardur"=wardur, "untype4" = 0)
  
  y_treat_int <- predict(glm2, x_treat_int, type="response")
  y_control_int <- predict(glm2, x_control_int, type="response")
  storage_int[wardur] <- y_treat_int - y_control_int
}

# plot with similar specifications as paper
plot(storage_int, xlab="Duration of wars in months", ylab="Marginal effects of UN peacekeeping operations",
     type="l", main="Causal Effect of Multidimensional UN Peacekeeping Operations", ylim=c(0.0,0.85), xlim=c(0,320))
lines(storage, lty='dotted')
text(x=100, y=0.5, labels="Dotted: Original model")
text(x=200, y=0.73, labels="Model with interaction term")

# Q3
# Tr <- rep(0, length(foo$uncint))
# Tr[which(foo$uncint != 0 & foo$uncint != 1)] <- 1
# What does this mean? What is "treatment"?
 
# the above is changed to this in order to achieve the intended outcome of assigning Nones to control and the rest to treatment 
Tr <- rep(0, length(foo$uncint))
Tr[which(as.numeric(foo$uncint) != 2)] <- 1
foo$uncint <- Tr

# Q4

# make the dependent variables into 0s and 1s
a <- as.numeric(foo$pbs2l)
a <- a-1
foo$pbs2l <- a

# pbs5l has some NAs, so are making foo_5 with the NAs removed
# and making it into 0s and 1s
foo_5 <- foo[!is.na(foo$pbs5l),]
b <- as.numeric(foo_5$pbs5l)
b <- b-1
foo_5$pbs5l <- b


# LOGIT
log_reg <- glm(pbs2l~wartype+logcost+wardur+factnum+factnum2+trnsfcap
               +untype4+treaty+develop+exp+decade, data=foo, family=binomial)

log_reg_5 <- glm(pbs5l~wartype+logcost+wardur+factnum+factnum2+trnsfcap
                 +untype4+treaty+develop+exp+decade, data=foo_5, family=binomial)

# check balance
mb_log <- MatchBalance(uncint~wartype+logcost+wardur+factnum+factnum2+trnsfcap
                       +untype4+treaty+develop+exp+decade, data=foo, nboots=500)

mb_log_5 <- MatchBalance(uncint~wartype+logcost+wardur+factnum+factnum2+trnsfcap
                       +untype4+treaty+develop+exp+decade, data=foo_5, nboots=500)

# treatment effect
te_2 <- mean(predict(log_reg,newdata = foo[which(foo$uncint==1),],type="response")) - mean(predict(log_reg,newdata=foo[which(foo$uncint==0),],type="response"))
te_5 <- mean(predict(log_reg_5,newdata = foo_5[which(foo_5$uncint==1),],type="response")) - mean(predict(log_reg_5,newdata=foo_5[which(foo_5$uncint==0),],type="response"))
te_2
te_5

# PROP SCORE MATCHING

# prop score via logit
prop_score <- glm(uncint~wartype+logcost+wardur+factnum+factnum2+trnsfcap
                  +untype4 +treaty+develop+exp+decade, data = foo, family = binomial)
prop_score_5 <- glm(uncint~wartype+logcost+wardur+factnum+factnum2+trnsfcap
                  +untype4 +treaty+develop+exp+decade, data = foo_5, family = binomial)

X <- prop_score$fitted
X2 <- prop_score_5$fitted
Y_2 <- foo$pbs2l
Y_5 <- foo_5$pbs5l

# 2 year
mout_prop_2 <- Match(Y=Y_2,X=X, Tr=Tr, M=1, replace = TRUE, BiasAdjust = FALSE,estimand = "ATT", ties = TRUE)
mout_prop_2_BA <- Match(Y=Y_2,X=X, Tr=Tr, M=1, replace = TRUE, BiasAdjust = TRUE,estimand = "ATT", ties = TRUE)

mb_prop_2 <- MatchBalance(uncint~ wartype+logcost+wardur+factnum+factnum2+trnsfcap
                        +untype4+treaty+develop+exp+decade, 
                        data=foo, match.out = mout_prop_2_BA, nboots=500)
# 5 year
mout_prop_5 <- Match(Y=Y_5, X=X, Tr=Tr, M=1, replace = TRUE, BiasAdjust = FALSE, estimand = "ATT", ties = TRUE)
mout_prop_5_BA <- Match(Y=Y_5, X=X, Tr=Tr, M=1, replace = TRUE, BiasAdjust = TRUE,estimand = "ATT", ties = TRUE)

mb_prop_5 <- MatchBalance(uncint~ wartype+logcost+wardur+factnum+factnum2+trnsfcap
                        +untype4+treaty+develop+exp+decade, 
                        data=foo_5, match.out = mout_prop_5_BA, nboots=500)

# GEN MATCH

X3 <- cbind(foo$wartype,foo$logcost,foo$wardur,foo$factnum,foo$factnum2,foo$trnsfcap,
            foo$untype4,foo$treaty,foo$develop,foo$exp,foo$decade)

X4 <- cbind(foo_5$wartype,foo_5$logcost,foo_5$wardur,foo_5$factnum,foo_5$factnum2,foo_5$trnsfcap,
            foo_5$untype4,foo_5$treaty,foo_5$develop,foo_5$exp,foo_5$decade)

# 2 year
genout_2 <- GenMatch(X=X3, Tr=Tr, estimand = "ATT", M=1, caliper = 4, replace = TRUE, ties = TRUE, pop.size = 500, max.generations = 50)

mout_gen_2 <- Match(Y=Y_2, X=X3, Tr=Tr, estimand = "ATT", M=1, caliper = 4, replace = TRUE, ties = TRUE, Weight.matrix = genout_2)
mout_gen_2_BA <- Match(Y=Y_2, X=X3, Tr=Tr, estimand = "ATT", M=1, caliper = 4, replace = TRUE, ties = TRUE, BiasAdjust = TRUE, Weight.matrix = genout_2)

mb_gen_2 <- MatchBalance(uncint~ wartype+logcost+wardur+factnum+factnum2+trnsfcap
                         +untype4+treaty+develop+exp+decade, 
                         data=foo, match.out = mout_gen_2_BA, nboots = 500)
summary(mout_gen_2)
summary(mout_gen_2_BA)

# 5 year
genout_3 <- GenMatch(X=X4, Tr=Tr, estimand = "ATT", M=1, caliper = 4, replace = TRUE, ties = TRUE, pop.size = 500, max.generations = 50)

mout_gen_5 <- Match(Y=Y_5, X=X4, Tr=Tr, estimand = "ATT", M=1, caliper = 4, replace = TRUE, ties = TRUE,Weight.matrix = genout_3)
mout_gen_5_BA <- Match(Y=Y_5, X=X4, Tr=Tr, estimand = "ATT", M=1, caliper = 4, replace = TRUE, ties = TRUE, BiasAdjust = TRUE, Weight.matrix = genout_3)

mb_gen_2 <- MatchBalance(uncint~ wartype+logcost+wardur+factnum+factnum2+trnsfcap
                         +untype4+treaty+develop+exp+decade, 
                         data=foo_5, match.out = mout_gen_5_BA, nboots = 500)

summary(mout_gen_5_BA)
summary(mout_gen_5)
