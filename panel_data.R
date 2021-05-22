#UNIVERSITY OF WARSAW
#Faculty of Economic sciences
#Advanced econometrics project report
#Estimation of the impact of GDP components on total compensation of employees. Panel data approach.
#
#Prepared by:
#Sergey Amarin 

#####################################

#import libraries
if (!require("reshape2")) install.packages("reshape2")
if (!require("plm"))install.packages("plm")
if (!require("Formula"))install.packages("Formula")
if (!require("Formula"))install.packages("stargazer")
if (!require("lmtest"))install.packages("lmtest")
library(reshape2)
library(plm)
library(Formula)
library(stargazer)
library(lmtest)

#Loading and preparation the data
setwd("D:\\UW\\2nd semester\\Econometrics\\Project")
proj <- read.csv("raw series_final.csv", sep=",", header=T, check.names = FALSE)

proj <- melt(proj, id.vars = c('Country','Variable'), variable.name = 'Date')
proj <- dcast(proj, Country+Date~Variable)
proj <- na.omit(proj)

#meke our variables stationary
proj1 <- as.data.frame(sapply(proj[,c("coll_cons_gov","comp_empl","cons_hou","exp_goods","exp_goods_servc","exp_servc",                                                "gross_cap", "imp_goods", "imp_goods_servc", "imp_servc","ind_cons_gov", 
                                        "op_surp", "soc_contr", "subs", "subs_products", "tax_prod_import", "tax_product"                                        ,"value_added")], 
                                       function(x) x/proj$gdp))

#check for correlation 
proj_cor <- as.data.frame(cor(proj1, method = c("spearman"), use = "complete.obs"))

#remove highly-correlated independent variables with coefficient higher 0.7 
proj1 <- as.data.frame(proj1[,c( "coll_cons_gov","comp_empl","cons_hou","exp_goods", "gross_cap", "imp_goods",                                                    "ind_cons_gov", "op_surp", "soc_contr", "subs", "subs_products", "value_added")]
                                        , method = c("spearman"), use = "complete.obs")
#check for correlation 
proj_cor <- as.data.frame(cor(proj1, method = c("spearman"), use = "complete.obs"))

#View(proj_cor)

#add cross-sectional and time variables
proj1['Country'] <- proj['Country']
proj1['Date'] <- proj['Date']
proj <- pdata.frame(proj1,index=c("Country","Date"), drop.index=TRUE, row.names=TRUE)


##########Hypothesis 1 - All variables in model are significant


#estimating fixed effect model
fixed <- plm(comp_empl~coll_cons_gov + cons_hou + exp_goods +
                       gross_cap + imp_goods + ind_cons_gov + op_surp+                                                                                  soc_contr + subs + subs_products + value_added
        , data = proj, model = "within")
summary(fixed)
#R squared is 0.725 which is acceptable level


#estimating random effect model
random <- plm(comp_empl~coll_cons_gov + cons_hou + exp_goods +
                        gross_cap + imp_goods + ind_cons_gov + op_surp+                                                                                  soc_contr + subs + subs_products + value_added
             , data = proj, model = "random")
summary(random)

#Hausmann test to choose the better model
phtest(fixed, random)
#random effect is more apropriate


#Test for autocorrelation in resuduals
pbgtest(random)
#the residuals have autocorrelation - model is not OK

#Test for heteroscedastisity
bptest(comp_empl~coll_cons_gov + cons_hou + exp_goods +
                 gross_cap + imp_goods + ind_cons_gov + op_surp +                                                                                 soc_contr + subs + subs_products + value_added
         , data=proj, studentize=T)
#The residuals are heteroscedastic - model is not OK
#Due to below results we can't trust to model's coefficients and p-values

#Use robust estimator for controlling heteroskedasticity and autocorrelation:
hypothesis1 <- coeftest(random, vcov.=vcovHC(random, method="white1", type="HC0", cluster="group"))
hypothesis1
#We can trust this output

##########Hypothesis 2 - Eliminating insignificant variables


#Limited models
# fixed effect
fixed_cut <- plm(comp_empl~coll_cons_gov + cons_hou + 
                      imp_goods + ind_cons_gov + op_surp+                                                                                              soc_contr + subs + subs_products + value_added
             , data = proj, model = "within")
summary(fixed_cut)

#estimating random effect model
random_cut <- plm(comp_empl~coll_cons_gov + cons_hou + 
                       imp_goods + ind_cons_gov + op_surp+                                                                                              soc_contr + subs + subs_products + value_added
              , data = proj, model = "random")
summary(random_cut)

#Hausmann test
phtest(fixed_cut, random_cut)

#Check if fixed model can be reduced to linear
#POLS
pols <- lm(comp_empl~coll_cons_gov + cons_hou + 
             imp_goods + ind_cons_gov + op_surp+                                                                                              soc_contr + subs + subs_products + value_added
           , data = proj)

#F test for individual effects
pFtest(fixed_cut,pols)
#Individual effects are significant - we can't reduce fixed effect model to linear

#Test for autocorrelation in resuduals
pbgtest(fixed_cut)
#residuals have autocorrelation

#Test for heteroscedastisity
bptest(comp_empl~coll_cons_gov + cons_hou + 
                 imp_goods + ind_cons_gov + op_surp+                                                                                              soc_contr + subs + subs_products + value_added
       , data=proj, studentize=T)
#residuals are heteroscedastic

#robust estimator for controlling heteroskedasticity and autocorrelation:
hypothesis2 <- coeftest(fixed_cut, vcov.=vcovHC(fixed_cut, method="white1", type="HC0", cluster="group"))
hypothesis2
#we can trust these results

##########Quality publication table
stargazer(random,  fixed_cut, title="Results", type="text", align=TRUE)
