library(readxl)
height_1 <- read_excel("C:/Users/hoang/Downloads/height_1.xlsx")
View(height_1)
height_2 <- read_excel("C:/Users/hoang/Downloads/height_2.xlsx")
View(height_2)
#Create a data frame for height_1
cass <-data.frame(height_1)

#Question 1
#(1)
#a) to study the effect of gender on height after controlling for age (using robust standard error)
library(lmtest)
library(sandwich)
library(car)
#Regression models: the effect of gender on smoker after controlling for age
model1 <- lm(height~gender +age ,data=cass) 
summary(model1)
#Robust Standard error
vcov = vcovHC(model1, type="HC1")
robust_se<- sqrt(diag(vcov))
robust_se
#Report the result with Robust Standard Error 
coeftest(model1, vcov. = vcov)

#b)
#Question 2: (1)run testcore  = A + B×HiSTR +C×HiEL + D×(HiSTR×HiEL) regression and report your results. Use the robust standard errors
caschool <- read_excel("C:/Users/hoang/Downloads/caschool.xlsx")
View(caschool)
#Create a data frame for caschool
cas <-data.frame(caschool)
# append HiSTR to CASchools
cas$HiSTR <- as.numeric(cas$str >= 25)
# append HiEL to CASchools
cas$HiEL <- as.numeric(cas$el_pct >= 12)
#estimate the model with a binary interaction term
bi_model <- lm(testscr ~ HiSTR * HiEL, data = cas)
summary(bi_model)
#Robust Standard error
vcov = vcovHC(bi_model, type="HC1")
robust_se_m<- sqrt(diag(vcov))
robust_se_m
#Report the result with Robust Standard Error 
coeftest(bi_model, vcov. = vcov)

#(2)	Test the joint significance of HiSTR and the interaction term. What is your conclusion?
#Hypothesis:
#Ho: Both HiSTR coefficient and the interaction term coefficient is equal 0
#H1: At least HiSTR coefficient or interaction term coefficient is not equal 0
# Run the regression
bi_model1 <- lm(testscr ~ HiSTR + HiEL + HiSTR:HiEL, data = cas)

# Get robust standard errors
coeftest(bi_model1, vcov = vcovHC(bi_model1, type = "HC1"))

# Test the joint significance of HiSTR and the interaction term
linearHypothesis(bi_model1, c("HiSTR=0", "HiSTR:HiEL = 0"))
#F-test = 0.8298 < 3.84 and p-value is 0.4369 > 0.05 => We fail to reject the null hypothesis. HiSTR coefficient and the interaction term are not jointly significant.

#(3)fill in the expected test scores of a student 
663.4297-3.8547*0- 21.8501*0 - 15.6748*(0*0)
663.4297-3.8547*1- 21.8501*0 - 15.6748*(1*0)
663.4297-3.8547*0- 21.8501*1 - 15.6748*(0*1)
663.4297-3.8547*1- 21.8501*1 - 15.6748*(1*1)

#(4)	Now suppose you don’t include the interaction term in the model. You just want to study the effect of STR on test scores after controlling for other variables.

#i.	Construct three different regression models that were not covered in class. Present your models in a tabular form using the stargazer package. (3 points)
#Check the significant regressors 
model <- lm(testscr~ str + comp_stu + enrl_tot + teachers +calw_pct + meal_pct + computer + expn_stu ,data=cas) 
#Robust Standard error
vcov = vcovHC(model, type="HC1")
robust_se_m<- sqrt(diag(vcov))
robust_se_m
#Report the result with Robust Standard Error 
coeftest(model, vcov. = vcov)

# load the stargazer library
library(stargazer)
#Hypothesis model A: 
#Ho: Both str coefficient and comp_stu coefficient is equal 0
#H1: At least str coefficient or comp coefficient is not equal 0
modela <- lm(testscr~ str + comp_stu,data=cas) 
linearHypothesis(modela, c("str=0", "comp_stu=0"), white.adjust = "hc1")
#F-test = 19.769 > 3.84 and p-value is 6.272e-09 <0.05 => We reject the null hypothesis. Str and comp_stu are jointly significant 

#Hypothesis model B: 
#Ho: Both str coefficient and gr_span coefficient is equal 0
#H1: At least str coefficient or enrl_tot coefficient is not equal 0
modelb <- lm(testscr~ str + enrl_tot,data=cas) 
linearHypothesis(modelb, c("str=0", "enrl_tot=0"), white.adjust = "hc1")
#F-test =  13.598 > 3.84 and p-value is 1.901e-06 <0.05 => We reject the null hypothesis. Str and enrl_tot are jointly significant 

#Hypothesis model C : 
#Ho: Both str coefficient and expn_stu coefficient is equal 0
#H1: At least str coefficient or expn_stu coefficient is not equal 0
modelc <- lm(testscr~ str + calw_pct,data=cas) 
linearHypothesis(modelc, c("str=0", "calw_pct=0"), white.adjust = "hc1")
#F-test =  103.43 > 3.84 and p-value is 2.2e-16 <0.05 => We reject the null hypothesis. Str and calw_pct  are jointly significant 

# estimate different model specifications
modela <- lm(testscr~ str + comp_stu,data=cas) 
modelb <- lm(testscr~ str + enrl_tot,data=cas)  
modelc <- lm(testscr~ str + calw_pct,data=cas) 

# gather robust standard errors in a list
rob_se <- list(sqrt(diag(vcovHC(modela, type = "HC1"))),
               sqrt(diag(vcovHC(modelb, type = "HC1"))),
               sqrt(diag(vcovHC(modelc, type = "HC1"))))

# Regression models: in html format
stargazer(modela,modelb,modelc, type="html",
          dep.var.labels=c("Test Score "),
          covariate.labels=c("Student Teacher Ratio","Computer per student","Total Enrollment"," Percent Qualifying for Calworks"), 
          out="modele.htm")

save.image('modele.RData')


#Question 3
library(readxl)
inc <- read_excel("C:/Users/hoang/Downloads/inc.xlsx")
View(inc)
library(plm)
library(stargazer)
library(lmtest)
library(sandwich)
library(jtools) 
#Create a data frame for inc
inc <-data.frame(inc)
#Fit Model_1 
model_1 <- lm(dem_ind ~ log_gdppc, data = inc)
#get the standard errors clustered at state level for country
summ(model_1, robust = TRUE, robust.type = "HC1", cluster = "country", digits = 7)

inc$country <-as.factor(inc$country)
inc$year <-as.factor(inc$year)
# Fit Model_2
model_2 <- plm(dem_ind ~ log_gdppc, 
               data = inc,
               index = c("country", "year"), 
               model = "within",      
               effect = "twoways")

# Report the robust standard error
coeftest(model_2, vcov = vcovHC, type = "HC1")

# Exclude data for Azerbaijan and Andorra
inc_subset <- subset(inc, country != "Azerbaijan" & country != "Andorra")
# Fit Model_3
model_3 <- plm(dem_ind~ log_gdppc, 
               data = inc_subset,
               index = c("country", "year"), 
               model = "within",      
               effect = "twoways")

# Report the robust standard error
coeftest(model_2, vcov = vcovHC, type = "HC1")
#d) 
library(stargazer)
#estimate the 3 model 
model_1 <- lm(dem_ind ~ log_gdppc, data = inc)
model_2 <- plm(dem_ind ~ log_gdppc, 
               data = inc,
               index = c("country", "year"), 
               model = "within",      
               effect = "twoways")
model_3 <- plm(dem_ind~ log_gdppc, 
               data = inc_subset,
               index = c("country", "year"), 
               model = "within",      
               effect = "twoways")

# gather clustered standard errors in a list
rob_see <- list(sqrt(diag(vcovHC(model_1, type = "HC1"))),
                sqrt(diag(vcovHC(model_2, type = "HC1"))),
                sqrt(diag(vcovHC(model_3, type = "HC1"))))
# generate the table (html format)
# Regression models: in html format

stargazer(model_1, model_2,model_3, 
          type="html",
          digits = 3,
          se = rob_se,
          title = "The Effects of Income on Democracy",
          out="models.htm")               
# generate another table with correct R2 and adj R2
####################################################
# estimate all seven models using plm().
model_1 <- lm(dem_ind ~ log_gdppc, data = inc)
model_2 <- plm(dem_ind ~ log_gdppc, 
               data = inc,
               index = c("country", "year"), 
               model = "pool",      
               effect = "twoways")
model_3 <- plm(dem_ind~ log_gdppc, 
               data = inc_subset,
               index = c("country", "year"), 
               model = "pool",      
               effect = "twoways")

stargazer(model_1,model_2, model_3, 
          type="html",
          digits = 3,
          se = rob_se,
          title = "The Effects of Income on Democracy",
          out="models_r2.htm") 
               
