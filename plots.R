hematemesis%>% select_if(is.factor) %>% map(~chisq.test(.,hematemesis$DEATH)$p.value) %>% as.data.frame %>% gather %>% 
  mutate(signf= ifelse(value<0.05,"significant","non-significant")) %>% 
  ggplot(aes(x=reorder(key,value),y=value))+
  geom_point(aes(colour=signf))+
  coord_flip()+ylab("p value")+xlab("Factor")

hematemesis %>% select_if(is.numeric) %>% map(~t.test(.~hematemesis$REBLEED.EPISODE)$p.value) %>% as.data.frame %>% gather %>% 
  mutate(signf= ifelse(value<0.05,"significant","non-significant")) %>% 
  ggplot(aes(x=reorder(key,value),y=value))+
  geom_point(aes(colour=signf))+
  coord_flip()+ylab("p value")+xlab("Factor")


hematemesis_non_variceal =  hematemesis %>%filter(!(ENDOSCOPIC_ETIOLOGY=="v"))
hematemesis_varices %>%  select_if(is.numeric)  %>% map(~t.test(.~hematemesis_varices$REBLEED.EPISODE)$p.value) %>% as.data.frame %>% gather %>% 
  mutate(signf= ifelse(value<0.05,"significant","non-significant")) %>% 
  ggplot(aes(x=reorder(key,value),y=value))+
  geom_point(aes(colour=signf))+
  coord_flip()+ylab("p value")+xlab("Factor") +ylim(0,0.5)

hematemesis_non_variceal %>%  select_if(is.numeric)  %>% map(~t.test(.~hematemesis_non_variceal$REBLEED.EPISODE)$p.value) %>% as.data.frame %>% gather %>% 
  mutate(signf= ifelse(value<0.05,"significant","non-significant")) %>% 
  ggplot(aes(x=reorder(key,value),y=value))+
  geom_point(aes(colour=signf))+
  coord_flip()+ylab("p value")+xlab("Factor") +ylim(0,0.5)
