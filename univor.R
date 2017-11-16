library(tidyverse)

library(MASS)

library(broom)

regcoef = function(x){
  df1 = hem2 %>% dplyr::select(x,REBLEED.EPISODE)
  df1
  a = glm(as.factor(REBLEED.EPISODE)~.,family=binomial(),data=df1)
  b= exp(a$coefficient[-1])
  
  d =  exp(confint(a)[-1,]) # removing first row intercept for easy printing numeric
  list(b,d)
}

regcoef1 = function(x){
  df1 = hem2 %>% dplyr::select(x,REBLEED.EPISODE)
  df1
  a = glm(as.factor(REBLEED.EPISODE)~.,family=binomial(),data=df1)
  b= exp(a$coefficient)
  
  d =  exp(confint(a)) # retain intercept for factor for first level
  list(b,d)
}
regcoef(x="GBS.SCORE")

# remove intercept for numeric, dont for factor
regcoef("lactate")


regcoef1("ASA")

regcoef("ASA")



aa





hem2 %>% select_if(is.factor) %>% map(~regcoef(.))



aa = hem2 %>% select_if(is.factor) %>% colnames()

bb = hem2 %>% select_if(is.numeric) %>% colnames()


map(aa,regcoef1) # for factors

map(bb,regcoef)


