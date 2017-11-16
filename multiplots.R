# save multiple box plots at once

library(ggplot2)
plots <- mtcars %>% 
  split(.$cyl) %>% 
  map(~ggplot(., aes(mpg, wt)) + geom_point())


plots= master%>% select_if(is.numeric) %>% names() %>% map(function(y)ggplot(subset(master,!is.na(DEATH)),aes(DEATH))+ geom_boxplot(aes_string(y=y)))
paths <- stringr::str_c("DEATH",1:length(plots), ".png")
pwalk(list(paths, plots), ggsave, path = getwd())

paths
tempdir()

getwd()

length(plots)



# dodge plots(factor plots)


plots2= master %>% select_if(is.factor) %>% map(~ggplot(master, aes(REBLEED, ..count..)) + geom_bar(aes(fill = .), position = "dodge")+ labs(title=.))
paths2 <- stringr::str_c("REBLEED",fac,1:length(plots2), ".png")
pwalk(list(paths2, plots2), ggsave, path = getwd())

# significance plots


hematemesis%>% select_if(is.factor) %>% map(~chisq.test(.,hematemesis$DEATH)$p.value) %>% as.data.frame %>% gather %>% 
  mutate(signf= ifelse(value<0.05,"significant","non-significant")) %>% 
  ggplot(aes(x=reorder(key,value),y=value))+
  geom_point(aes(colour=signf))+ylim(0,0.30)+
  coord_flip()+ylab("p value")+xlab("Factor") 

master %>% select_if(is.numeric) %>% map(~t.test(.~hematemesis$DEATH)$p.value) %>% as.data.frame %>% gather %>% 
  mutate(signf= ifelse(value<0.05,"significant","non-significant")) %>% 
  ggplot(aes(x=reorder(key,value),y=value))+
  geom_point(aes(colour=signf))+ylim(0,0.30)+
  coord_flip()+ylab("p value")+xlab("Factor")

df %>% select_if(is.factor) %>% map(~chisq.test(.,df$f)$p.value) %>% as.data.frame %>% gather %>% 
  mutate(signf= ifelse(value<0.05,"significant","non-significant")) %>% 
  ggplot(aes(x=reorder(key,value),y=value))+
  geom_point(aes(colour=signf))+
  coord_flip()+ylab("p value")+xlab("Factor")


no = length(plots)+ c(1:length(plots2))

noo = master %>% select_if(is.factor) %>% names()


nmas
str_c(no,".png")

paths <- stringr::str_c(noo, ".png")

pwalk(list(paths, plots2), ggsave, path = getwd())


# logitloess

library(tidyverse)

logitloess <- function(x, y, s) {
  
  logit <- function(pr) {
    log(pr/(1-pr))
  }
  
  if (missing(s)) {
    locspan <- 0.7
  } else {
    locspan <- s
  }
  
  a= loess(y~x,span=locspan)
  loessfit = predict(a)
  pi <- pmax(pmin(loessfit,0.9999),0.0001)
  logitfitted <- logit(pi)
  
  plot(x, logitfitted, ylab="logit")
  
}

a= master$age
b= master$DEATH
logitloess(y=b,x=as.numeric(a))

loess(master$REBLEED~master$age,span=0.7)
str(master$age)



master %>% select_if(is.numeric) %>% map(function(y)ggplot(aes(x=y,as.numeric(DEATH)-1))) +
  stat_smooth(method="loess", formula=y~master$DEATH,alpha=0.2, size=2) +
  geom_point(position=position_jitter(height=0.03, width=0)) +xlab("y")+
   ylab("Pr (DEATH)")

logit <- function(x) log(x)/log(1-x)

master  %>% ggplot(aes(lactate, as.numeric(DEATH)-1)) +
  stat_smooth(method="loess",  formula=y~x,alpha=0.2, size=2) +
  geom_point(position=position_jitter(height=0.03, width=0)) +
  coord_trans(y="logit") + xlab("Age") + ylab("Pr (DEATH)")

as.numeric(as.character(master$DEATH))