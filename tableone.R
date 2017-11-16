library(tidyverse)
library(tableone)

master=hematemesis
# Extract Factor
fac =master %>% select_if(is.factor) %>% names()

str(fac)

fac[2]

# Extract numeric
num =master %>% select_if(is.numeric) %>% names()

str(num)
master$diagnoses = ifelse(master$CKD ==1,"CKD",
                          ifelse(master$HRS==1,"HRS",
                                 ifelse(master$`Intrinsic renal`==1,"iAKI", 
                                        ifelse(master$Prerenal==1,"Prerenal", "Control"))))      

master$diagnoses = factor(master$diagnoses, levels = c("Control","CKD","Prerenal","HRS","iAKI"))

str(master$diagnoses)

summary(master$diagnoses)

vars = c(num,fac)
vars

library(tableone)
tab2 <- CreateTableOne(vars = vars, strata = "REBLEED" , data = master)

#Error due to bad name in table one

master =master %>% 
  # got stuck at spaces so changed all col name white space to _ 
  
  # Bulk change 
  nn = names(master)

library(stringr)
patt = " "

mm = str_replace_all(nn,patt,"_")
mm

names(master)= mm

names(master)

num =master %>% select_if(is.numeric) %>% names()

fac =master %>% select_if(is.factor) %>% names()

vars = c(num,fac)
vars
num
fac
library(tableone)
tab2 <- CreateTableOne(vars = vars, strata = "REBLEED" , data = master)
# stuck at pulse
master= master %>% rename(Pulse=`PULSE>100`)

num =master %>% select_if(is.numeric) %>% names()
num
fac =master %>% select_if(is.factor) %>% names()

vars = c(num,fac)

tab2 <- CreateTableOne(vars = vars, strata = "DEATH" , data = master)
print(tab2,quote = TRUE,noSpaces = TRUE)





master$
  summary(master)



num
tab2

print(tab2,quote = TRUE,noSpaces = TRUE)



# multiple anovas

outcome= c("diagnoses")
anovas = 
  
  
  
  m1 = c("master$feNa","master$NGAL")
m2= c("master$TLC")
m3=expand.grid(m1,m2,stringsAsFactors = FALSE)

m3$Var1[2]

m

library(tidyverse)

# Multiple ANOVAS with single map argument 

# write a custom function

av= function(x){
  
  f=aov(x~master$diagnoses)
  
  
  f1= summary(f)
  
  f2=TukeyHSD(f)
  
  f3=list(f2,f1)
  
  f3
}

# apply map argument


master%>% select_if(is.numeric) %>% map(~av(.))



# save multiple plots at once

library(ggplot2)
plots <- mtcars %>% 
  split(.$cyl) %>% 
  map(~ggplot(., aes(mpg, wt)) + geom_point())
paths <- stringr::str_c(names(plots), ".pdf")

pwalk(list(paths, plots), ggsave, path = tempdir())

# map2 2 arguments for t test ( Multiple t test)

# write function

sum_wrapper <- function (tbl=NULL, var1=NULL, var2=NULL) {
  tbl_pair <- tbl %>%
    select_(var1, var2)
  x <- tbl_pair %>% pull(var1)
  y <- tbl_pair %>% pull(var2)
  sum = x+y
  sum
}

#check
sum_wrapper(var1="HB",var2="BUN",tbl=master)



# execute
allcomb_var <- expand.grid(var1 = c("HB","lactate"), var2 = c("BUN"),
                           stringsAsFactors = FALSE)

allcomb_var %>% 
  purrr::pmap(.f = sum_wrapper, tbl=master)

# ttest - multiple

test_wrapper <- function (tbl=NULL, var1=NULL, var2=NULL) {
  tbl_pair <- tbl %>%
    select_(var1, var2)
  x <- tbl_pair %>% pull(var1)
  y <- tbl_pair %>% pull(var2)
  f=t.test(x~y) # there was fault if no formula mode
  g= colnames(tbl_pair) # get col_names
  h=list(g,f)
  h
}

#check
colnames(master$Name)

test_wrapper(var1="HB", var2 = "DEATH",tbl=master) 


master$REBLEED=as.factor(master$REBLEED)
summary(master$REBLEED.EPISODE)
num
allcomb <- expand.grid(var1 = num, var2 = c("DEATH","REBLEED"),
                       stringsAsFactors = FALSE)

allcomb %>% purrr::pmap(.f = test_wrapper, tbl=master)

# Multiple crosstable and chi square
library(desc)

chichi = master %>% select_if(is.factor) %>% map(~CrossTable(.,master$REBLEED,chisq=TRUE))

print(chichi)

didi = master %>% select_if(is.factor) %>% map(~CrossTable(.,master$DEATH,chisq=TRUE))

sink("chi.txt") # print long output to file inwd
chichi
sink() # go back to terminal output

sink("di.txt") # print long output to file inwd
didi
sink() # g

?xtabs

library(tidyverse)

master =master %>% 
  # got stuck at spaces so changed all col name white space to _ 
  
  # Bulk change 
  nn = names(master)

library(stringr)
patt = " "

mm = str_replace_all(nn,patt,"_")
mm

names(master)= mm

names(master)


allcomb %>%
  purrr::pmap(.f = test_wrapper, tbl=master)


master= master %>% mutate(outcome_31 = case_when(
  is.na(outcome_30)~"a",
  outcome_30=="d"~"d",
  TRUE~"a"))


summary(master$outcome_31)



master$outcome_31= factor(master$outcome_31,levels=c("a","d"))
summary(master$outcome_31)

master = master %>% mutate(outcome_30=outcome_31)
master$outcome_31<- NULL








summary(master)











