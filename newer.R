library(tidyverse)

hematemesis = read.csv('hematemesis.csv')

summary(hematemesis$venous.lactate)

hematemesis$venous.lactate[hematemesis$venous.lactate=="01/Jan"]<- 1

# if you diretly convert factor to numeric absurd values, hence first to character then numeric

hematemesis$venous.lactate = as.character(hematemesis$venous.lactate)

hematemesis$venous.lactate = as.numeric(hematemesis$venous.lactate)

summary(hematemesis$INR)

summary(hematemesis)


hematemesis$ENDOSCOPIC_ETIOLOGY =recode_factor(hematemesis$ENDOSCOPIC_ETIOLOGY, "NA" = "c")

zer = function(x) { ifelse(x==0,1,0)}


# library
(car); recode2(dam, fields = c('re1', 're2'), recodes = "'yes' = 1; 'no' = 2", as.factor.result = TRUE)

library(dplyr)

# abandoning
#starting all over





# replacing na recode_factor fails

hematemesis$ENDOSCOPIC_ETIOLOGY[is.na(hematemesis$ENDOSCOPIC_ETIOLOGY)] <- "c"

# hematemesis$ENDOSCOPIC_ETIOLOGY[hematemesis$ENDOSCOPIC_ETIOLOGY=="NA"] <- "c" # doesnt work
summary(hematemesis$ENDOSCOPIC_ETIOLOGY)

#subsetting works
# just checking if everything works

f1 = glm(DEATH~ENDOSCOPIC_ETIOLOGY,data=hematemesis,family=binomial())

f1

summary(f1)

df1 = data.frame(hematemesis$DEATH,hematemesis$ENDOSCOPIC_ETIOLOGY)
chisq.test(table(df1))

table(df1)



# Look at why age is character reveals 5 months
hematemesis$age[hematemesis$age=="5 months"] <- 5

hematemesis$age = as.numeric(hematemesis$age)

summary(hematemesis$age)

summary(hematemesis)

#https://github.com/tidyverse/dplyr/issues/1845 mutate_at

hematemesis %>% mutate_at(starts_with("y=0"),funs(factor))

# correcting NA in REBLEED.episode
hematemesis$REBLEED.EPISODE[is.na(hematemesis$REBLEED.EPISODE)] <- 1

hematemesis$HBSAG[is.na(hematemesis$HBSAG)]<- 1

hematemesis$ICU[is.na(hematemesis$ICU)]<- 1


hematemesis = hematemesis %>% rename(lactate = `venous lactate`)


hematemesis$lactate[hematemesis$lactate=="01/Jan"]<- 1

hematemesis$lactate = as.numeric(hematemesis$lactate)

summary(hematemesis)


hematemesis %>% select_if(is.numeric) %>% map(~t.test(.~hematemesis$DEATH)$p.value) %>% as.data.frame %>% gather %>% 
  mutate(signf= ifelse(value<0.05,"significant","non-significant")) %>% 
  ggplot(aes(x=reorder(key,value),y=value))+
  geom_point(aes(colour=signf))+
  coord_flip()+ylab("p value")+xlab("Factor")





hematemesis$ICU = factor(hematemesis$ICU, levels=c("0","1"))
hematemesis$bleeding_pr = factor(hematemesis$bleeding_pr, levels=c("0","1"))

summary(hematemesis$Cause)

hematemesis_varices = hematemesis %>%filter(ENDOSCOPIC_ETIOLOGY=="v")


library(stringr)
# https://stackoverflow.com/questions/38088328/how-to-recode-and-revers-code-variables-in-columns-with-dplyr
# https://stackoverflow.com/questions/38809509/recode-and-mutate-all-in-dplyr

h1 =hematemesis %>% select(contains("n=1")) %>% map(~as.factor(.)) %>% as.data.frame() %>% add_rownames(var ="patient")

i1 = hematemesis %>% select(-contains("n=1")) %>%  add_rownames(var ="patient")

l1 =inner_join(h1,i1)

summary(l1)


cor.test(hematemesis$`abg- bicarbonate`,hematemesis$lactate)

summary(as.factor(hematemesis$`DIAGNOSIS`))

carcinoma = c("oma","can","hcc","ca gb","malignancy")
duodenum = c()

#https://stackoverflow.com/questions/26659198/detect-multiple-strings-with-dplyr-and-stringr
hematemesis$Etiology = ifelse(str_detect(hematemesis$DIAGNOSIS,"cld"),"Cirrhosis",
                             ifelse(str_detect(hematemesis$DIAGNOSIS,paste(carcinoma,collapse = '|')),"Neoplasia",
                        ifelse(str_detect(hematemesis$DIAGNOSIS,"weiss"),"Mallory_Weiss",
                           ifelse(str_detect(hematemesis$DIAGNOSIS,"duo"),"Duodenal",  
                                  ifelse(str_detect(hematemesis$DIAGNOSIS,"esophagitis"),"Esophagitis",  
                                         "Others")))))
hematemesis$Cause = ifelse(str_detect(hematemesis$DIAGNOSIS,"cld"),"Cirrhosis",
                              ifelse(str_detect(hematemesis$DIAGNOSIS,paste(carcinoma,collapse = '|')),"Neoplasia",
                                     ifelse(str_detect(hematemesis$DIAGNOSIS,"weiss"),"Mallory_Weiss",  
                                                          "Others")))

summary(as.factor(hematemesis$Etiology))

summary(as.factor(hematemesis$Cause))
# this is how to check if mtch is correct
hematemesis$DIAGNOSIS[hematemesis$Etiology=="Neoplasia"]

xtabs(as.factor(hematemesis$Cause),hematemesis$DEATH)

hematemesis$Cause = as.factor(hematemesis$Cause)
hematemesis$Cause = recode_factor(hematemesis$Cause , levels =c("Mallory_Weiss","Others","Cirrhosis","Neoplasia"))


  
xtabs(~DEATH+Cause ,data=hematemesis) # frequency table 

hematemesis = hematemesis %>% mutate(ascites=
                         case_when(`P/A`=="ascites"~0,
                                   TRUE ~ 1)) %>% mutate(ascites = factor(ascites, levels=c("1","0")))
                       
                       
  summary(hematemesis$ascites)
  
  xtabs(~DEATH+ascites,data=hematemesis) # frequency table 
  
  hematemesis = hematemesis %>% rename( hemat = y.0.n.1HEMATEMESIS , coffe_colored= y.0.n.1.COFFEE.COLOURED.OR.1OT, JVP = y.0..n.1.JVP.raised,
                          
                          painful = y.0.n.1.PAI1FUL.OR.1OT,
                          hematochezia = n.1.y.0HEMATOCHEZIA,
                          hospital_bleeding = n.1.y.0.I1.HOSPITAL.BLEEDI1G,
                          malena = y.0.n.1.MALE1A)
  
  
                          
  xtabs(~DEATH+Cause ,data=hematemesis) # frequency table 
  
  
 hematemesis %>% rename( ROCKALL = ROCKALL SCORE(0-11)) #doesnt work
 
 names(hematemesis)[43]<- "PNED.SCORE"
 names(hematemesis)[52]= "ROCKALL.SCORE"
 names(hematemesis)[53]= "GBS.SCORE"
 names(hematemesis)[54]= "AIMS65"
 names(hematemesis)[7]= "clots"
 names(hematemesis)[9]= "Pallor"
 names(hematemesis)[15]= "bleeding_pr"
 names(hematemesis)[31]= "pH"
 names(hematemesis)[32]= "Bicarbonate"
 
 names(hematemesis)[44]= "ASA"
 

  summary(hematemesis)
  
  hematemesis$Pallor[is.na(hematemesis$Pallor)]<- 1
  
  hematemesis$hemat[is.na(hematemesis$hemat)]<- 1
  
  hematemesis$pH = ifelse(hematemesis$pH==741,7.41,hematemesis$pH) # changing single value
  
  summary(hematemesis$pH)
  
  
  
  
 
  
  
  



