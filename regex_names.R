

# Bulk change , replace whitespace by _ in data frame
nn = names(master)

library(stringr)
patt = " "

mm = str_replace_all(nn,patt,"_")
mm

names(master)= mm

names(master)

#prior bulky

#Error due to bad name



s1 = c("hematemesis$lactate","hematemesis$REBLEED")

gsub("hematemesis\\$", "", s1)

library(stringr)
str_replace_all(s1,"hematemesis\\$", "")

#str_replace more example,str_detect_ can also be detected by case_when

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




