
library(Matching)

### <QUESTION 2>

foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")

grep("logdead", colnames(foo))

# extract relevant columns
foo <- foo[, c(6:8, 11:16, 99, 50, 114, 49, 63, 136, 108, 109, 126, 48, 160, 142, 10)]

# remove 2 rows with missing data
foo <- foo[c(-19, -47), ]

# check that all missing data is gone...
which(is.na(foo) == TRUE)

# take a peek at the data set (identify the columns)
head(foo)

# fit the regression models
glm1 <- glm(pbs2s3 ~ wartype + logcost + logdead + factnum + factnum2 + trnsfcap +
              develop + exp + decade + treaty + untype4, data = foo, family = binomial)
glm2 <- glm(pbs2s3 ~ wartype + logcost + logdead + factnum + factnum2 + trnsfcap +
              develop + exp + decade + treaty + untype4 + logdead:untype4, data = foo, family
            = binomial)

# setting the means
mean_wartype <- mean(foo$wartype)
mean_logcost <- mean(foo$logcost)
mean_factnum <- mean(foo$factnum)
mean_factnum2 <- mean(foo$factnum2)
mean_trnsfcap <- mean(foo$trnsfcap)
mean_develop <- mean(foo$develop);
mean_exp <- mean(foo$exp)
mean_decade <- mean(foo$decade) 
mean_treaty <- mean(foo$treaty)
mean_interaction <- mean(I(foo$logdead * foo$untype4))

# generate the predictions
original_storage <- rep(0, 150)
mod_storage <- rep(0, 150)

# store treatment effects
for (logdead in 0:150) {
  X_treat <- data.frame("wartype"=mean_wartype, "logcost"=mean_logcost, "logdead"=logdead,
                       "factnum"=mean_factnum,"treaty"=mean_treaty,
                       "factnum2"=mean_factnum2, "trnsfcap"=mean_trnsfcap, "develop"=mean_develop, 
                      "exp"=mean_exp, "decade"=mean_decade,"untype4"=1)
X_control <- data.frame("wartype"=mean_wartype, "logcost"=mean_logcost, "logdead"=logdead,
                         "factnum"=mean_factnum,"treaty"=mean_treaty,
                         "factnum2"=mean_factnum2, "trnsfcap"=mean_trnsfcap,
                         "develop"=mean_develop, "exp"=mean_exp, "decade"=mean_decade,
                         "untype4"=0)

# Predict the causal effects
Y_treat <- predict(glm1, X_treat,type= "response" )
Y_control <- predict(glm1, X_control, type= "response" )

original_storage[logdead] <- Y_treat - Y_control 

#same for x
X_treatt <- data.frame( "wartype" =mean_wartype, "logcost" =mean_logcost, "logdead" =logdead, 
"factnum" =mean_factnum, "treaty" =mean_treaty, 
"factnum2" =mean_factnum2, "trnsfcap" =mean_trnsfcap, 
"develop" =mean_develop, "exp" =mean_exp, "decade" =mean_decade, "untype4" = 1, "logdead:untype4"=logdead*1)

X_controll <- data.frame( "wartype" =mean_wartype, "logcost" =mean_logcost, "logdead" =logdead, 
"factnum" =mean_factnum, "treaty" =mean_treaty, 
"factnum2" =mean_factnum2, "trnsfcap" =mean_trnsfcap, 
"develop" =mean_develop, "exp" =mean_exp, "decade" =mean_decade, 
"untype4" =0, "logdead:untype4" =logdead*0) 

Y_treatt <- predict(glm2, X_treatt,type= "response" )
Y_controll <- predict(glm2, X_controll,type= "response" )
mod_storage[logdead] <- Y_treatt - Y_controll 
} 

#plot graph
plot(mod_storage, xlab= "Logdead" , ylab= "Marginal effects of UN peacekeeping operations" , 
     type= "l" , main= "Causal Effect of Multidimensional UN Peacekeeping Operations" ) 
lines(original_storage, lty= 'dotted' )
text(x=100, y=0.2, labels= "Dotted: Original model" )
text(x=100, y=-0.6, labels= "Model with interaction term" ) 


### <QUESTION 4>

#recall the data
foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")
foo <- foo[c(-19, -47,-4, -16, -84, -93, -98), ]
which(is.na(foo$pbs5l) == TRUE)

#(NUMBER 3 MODIFIED)
Tr <- rep(0, length(foo$uncint))
Tr[which(as.numeric(foo$uncint) != 2)] <- 1
foo$uncint <- Tr

foo$pbs2l <- as.numeric(foo$pbs2l)-1
foo5yr$pbs5l <- as.numeric(foo$pbs5l)-1


#LOGIT
glm2 <- glm(foo$pbs2l ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + untype4 + treaty + 
              develop + exp + decade, data=foo, family="binomial")
glm2_mb <- MatchBalance(uncint~wartype+logcost+wardur+factnum+factnum2+trnsfcap
                       +untype4+treaty+develop+exp+decade, data=foo, nboots=500)

glm5 <- glm(foo$pbs5l ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + untype4 + treaty + 
              develop + exp + decade, data=foo5yr, family="binomial")
glm5_mb <- MatchBalance(uncint~wartype+logcost+wardur+factnum+factnum2+trnsfcap
                        +untype4+treaty+develop+exp+decade, data=foo5yr, nboots=500)

#PROPENSITY
prop <- glm(uncint~wartype+logcost+wardur+factnum+factnum2+trnsfcap
           +untype4 +treaty+develop+exp+decade, data = foo, family = "binomial")
X <- prop$fitted.values

prop_mout_2 <- Match(Y=foo$pbs2l, Tr = Tr, X=X,BiasAdjust=TRUE)
prop_mout_5 <- Match(Y=foo$pbs5l, Tr = Tr, X=X, BiasAdjust=TRUE)

prop_mb_2 <- MatchBalance(uncint~ wartype+logcost+wardur+factnum+factnum2+trnsfcap
                          +untype4+treaty+develop+exp+decade, 
                          data=foo, match.out = prop_mout_2, nboots=500)
prop_mb_5 <- MatchBalance(uncint~ wartype+logcost+wardur+factnum+factnum2+trnsfcap
                          +untype4+treaty+develop+exp+decade, 
                          data=foo5yr, match.out = prop_mout_5, nboots=500)

summary(prop_mout_2)
summary(prop_mout_5)


#GENETIC MATCHING
X2 = cbind(foo$wartype+foo$logcost+foo$wardur+foo$factnum+foo$factnum2+foo$trnsfcap
           +foo$untype4 +foo$treaty+foo$develop+foo$exp+foo$decade)
X5 = cbind(foo5yr$wartype+foo5yr$logcost+foo5yr$wardur+foo5yr$factnum+foo5yr$factnum2+foo5yr$trnsfcap
           +foo5yr$untype4 +foo5yr$treaty+foo5yr$develop+foo5yr$exp+foo5yr$decade)
  
genout2 <- GenMatch(X=X2, Tr=Tr, estimand="ATT", M=1, replace=TRUE, ties=TRUE,
                   pop.size=200, max.generations=50, wait.generations=25)
genout5 <- GenMatch(X=X5, Tr=foo5yr$uncint, estimand="ATT", M=1, replace=TRUE, ties=TRUE,
                    pop.size=200, max.generations=50, wait.generations=25)
  
mout2 <- Match(Y=foo$pbs2l, X=X2, Tr=Tr, estimand="ATT", M=1, replace=TRUE, ties=TRUE,
              Weight.matrix = genout2)
mout2_BA <- Match(Y=foo$pbs2l, X=X2, Tr=foo5yr$uncint, estimand="ATT", M=1, BiasAdjust = TRUE, replace=TRUE, ties=TRUE,
                  Weight.matrix = genout2)

mout5 <- Match(Y=foo$pbs5l, X=X5, Tr=Tr, estimand="ATT", M=1, replace=TRUE, ties=TRUE,
               Weight.matrix = genout5)
mout5_BA <- Match(Y=foo$pbs5l, X=X5, Tr=Tr, estimand="ATT", M=1, BiasAdjust = TRUE, replace=TRUE, ties=TRUE,
                  Weight.matrix = genout5)

mb2 <- MatchBalance(Tr~ wartype+logcost+wardur+factnum+factnum2+trnsfcap
                    +untype4+treaty+develop+exp+decade, 
                    data=foo, match.out = mout2_BA, nboots = 500)
mb5 <- MatchBalance(uncint~ wartype+logcost+wardur+factnum+factnum2+trnsfcap
                    +untype4+treaty+develop+exp+decade, 
                    data=foo5yr, match.out = mout5_BA, nboots = 500)

summary(mout2_BA)
summary(mout5_BA)
