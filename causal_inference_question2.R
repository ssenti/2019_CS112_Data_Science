
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
