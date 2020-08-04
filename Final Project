#### load required packages
library(Synth)
library(lattice)
lattice.options(default.theme = modifyList(standard.theme(color = FALSE), list(strip.background = list(col = "transparent"))))

#### clear workspace
rm(list=ls())

#### set working directory to the folder containing the Rdata

#### load dataset (adjust the path to the files on your computer)
load("scdata.Rdata")

#### Define aggregates we need later
Euro12 <- c('Germany','Netherlands','Greece','Spain','Portugal','Italy',#
            'Finland','France','Luxembourg','Belgium','Austria','Ireland')

#### First defining predictors that were used in original study:

pred <- names(scdata)[c(#
  7, # pop65+
  8, # pop15-
  #9,# unemployment (1)
  #10,# system
  11,# yrcurnt
  12,# allhouse
  13,# legelec
  14, # pluralty
  15, # pr
  #16, # checks
  #17, # fri
  #18,# GDP growth
  #19,# growth in multi factor productivity
  20,# labor productivity annual growth
  21, # health expenditure/GDP
  22,# GDP expenditure approach
  #23,# tax revenue %GDP (general)
  24,# taxrev %GDP, no Social Security
  25, # CO2 emissions
  #26, # FDI
  #27, # GDP growth
  #28,# Gini index
  #29,#,# Inflation (Consumer Prices)
  #30, # Poverty
  31,#, # unemployment (World Bank)
  ##32, #Population
  #33,#, #openness (PWT)
  34,#,# openness (expenditure)
  #35, # Expenditure on Families %GDP
  36, # PolconIII
  #37, # PolconV
  38,  # Potrafke ideology
  39, # Majority margin
  #40, # Herfindahl Index Government
  41, #lag debt/gdp (RR)
  42#,# Rae Fractionalisation index (government)
  #43 # Rae Fractionalisation Index (total)
)]

#### define countries used for synthetic control group

contr <- sort(unique(scdata$ccode[is.element(scdata$country,setdiff(scdata$country,c(Euro12,"Euro 11","Slovenia")))]))

#### The following countries have to be excluded due to data constraints (missing values)
contr <- setdiff(contr, c(1111,2222,70,155,225,269,290,310,316,317,349,355,360,366,666,732,sort(unique(scdata$ccode[scdata$eu==0 & scdata$oecd==0]))))

#### REPLICATION

#### Generate Synth object (debt/gdp ratio) to be used to run the analysis
sdata <- dataprep(foo = scdata[scdata$ccode %in% contr | scdata$country == "Euro 11",],
                  predictors = pred,
                  dependent = names(scdata[6]),
                  unit.variable = "ccode",
                  time.variable = "Year",
                  treatment.identifier = 0,
                  controls.identifier = contr,
                  time.predictors.prior = c(1983:1998),
                  time.optimize.ssr = c(1983:1999),
                  unit.names.variable = "country",
                  time.plot = 1983:2010
)

#### Run the synthetic control analysis:
synth.out <- synth(data.prep.obj = sdata, method = "BFGS")

#### calculate output gaps from the results
gaps <- sdata$Y1plot - (sdata$Y0plot %*% synth.out$solution.w)

#### Plot the Path of the Debt to GDP ratio for the Euro11 and the Synthetic control
path.plot(synth.res = synth.out,
          dataprep.res = sdata,
          Ylab="Debt/GDP (Nominal)",
          Xlab="Year",
          Legend=c("Euro 11","Synthetic Euro 11"),
          Legend.position="bottomright", abline(v=1999,lty="dashed")
)

#### Plot the gap of the Debt to GDP ratio for the Euro11 and the Synthetic control
gaps.plot(synth.res = synth.out, dataprep.res = sdata,
          Ylab= "Gap in Debt/GDP (percentage points, 1983-2010)", Xlab="Year",
          Main=NA, abline(v=1999,lty="dashed"))


#### FOR EXTENSION

#### Add in Euro 11 to control unit pool
contr <- c(0, contr)

#### Show countries that are available for Synthetic Control
country <- sort(unique(scdata$country[scdata$ccode %in% contr]))


#### Generate Synth object (debt/gdp ratio) to be used to run the analysis

#### make a place to store the synth.outs
store <- matrix(NA,length(1983:2010),11)
colnames(store) <- unique(scdata$country[scdata$ccode %in% contr])

#### function that does synthetic control analysis for each country in the contr pool (10 controls + Euro 11)
for(i in 1:11){
  code <- contr[i]
  name <- scdata$country[scdata$ccode==code]
  sdata <- dataprep(foo = scdata[scdata$ccode %in% contr | scdata$country == name[1],],
                    predictors = pred,
                    dependent = names(scdata[6]),
                    unit.variable = "ccode",
                    time.variable = "Year",
                    treatment.identifier = code,
                    controls.identifier = contr[-i],
                    time.predictors.prior = c(1983:1998),
                    time.optimize.ssr = c(1983:1999),
                    unit.names.variable = "country",
                    time.plot = 1983:2010
  )

  #### Run the synthetic control analysis:
  synth.out <- synth(data.prep.obj = sdata, method = "BFGS")

  #### store gaps
  store[,i] <- sdata$Y1plot - (sdata$Y0plot %*% synth.out$solution.w)

}

#### now do figure
data <- store
rownames(data) <- 1983:2010

#### Set bounds in gaps data
gap.start     <- 1
gap.end       <- nrow(data)
years         <- 1983:2010
gap.end.pre  <- which(rownames(data)=="1999")

#### Plot
plot(years,data[gap.start:gap.end,which(colnames(data)=="Euro 11")],
     ylim=c(-100,100),xlab="Year",
     xlim=c(1983,2010),ylab="Gap in Debt/GDP (percentage points, 1983-2010)",
     type="l",lwd=2,col="black",
     xaxs="i",yaxs="i")

#### Add lines for control states
for (i in 1:ncol(data)) { lines(years,data[gap.start:gap.end,i],col="gray") }

#### Add Euro 11 Line
lines(years,data[gap.start:gap.end,which(colnames(data)=="Euro 11")],lwd=2,col="black")

#### Add other lines
abline(v=1999,lty="dotted",lwd=1.5)
abline(h=0,lty="dashed",lwd=1.5)
legend("bottomright",legend=c("Euro 11","control regions"),
       lty=c(1,1),col=c("black","gray"),lwd=c(2,1),cex=.8)
arrows(1992,-50,1998,-50,col="black",length=.1)
text(1990,-50,"SGP")
