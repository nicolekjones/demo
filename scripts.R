library(dplyr)
library(tidyverse)
pokemon<-read.csv("pokemon.csv")


#strongest against each type

attack <- pokemon %>%
  select(name, starts_with("against_"))


max<-data.frame()
for(i in 2:ncol(attack)){

  max_hold<- attack %>%
    arrange(desc(attack[,i]))

  set<- max_hold %>%
    mutate(`attack type`= names(max_hold)[i], power =max_hold[1,i])  %>%
    select(1,20,21)

  max<-rbind(set[1,],max)

}

weak<-data.frame()
for(i in 2:ncol(attack)){

  min_hold<- attack %>%
    arrange((attack[,i]))

  set<- min_hold %>%
    mutate(`attack type`= names(min_hold)[i], power =min_hold[1,i])  %>%
    select(1,20,21)

  weak<-rbind(set[1,],weak)

}


attack2 <- pokemon %>%
  select(name, type1, starts_with("against_"))

#attack / defense

attk_by_tpye <- pokemon %>%
  group_by(type1) %>%
  arrange(desc(attack)) %>%
  distinct(type1, .keep_all = T) %>%
  select(name, type1, attack)


def_by_tpye <- pokemon %>%
  group_by(type1) %>%
  arrange(desc(defense)) %>%
  distinct(type1, .keep_all = T) %>%
  select(name, type1, defense)

#special attack / defense
sp_attk_by_tpye <- pokemon %>%
  group_by(type1) %>%
  arrange(desc(sp_attack)) %>%
  distinct(type1, .keep_all = T) %>%
  select(name, type1, sp_attack)


sp_def_by_tpye <- pokemon %>%
  group_by(type1) %>%
  arrange(desc(sp_defense)) %>%
  distinct(type1, .keep_all = T) %>%
  select(name, type1, sp_defense)


sp_attdef_comp<-full_join(sp_def_by_tpye %>% select(-name), sp_attk_by_tpye %>% select(-name)) %>%
  pivot_longer(starts_with("sp"), names_to = "type", values_to = "level")

plot <- ggplot(sp_def_by_tpye, aes(x=type1, y=sp_defense, fill = type1))+
geom_bar(stat= "identity") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot


plot2 <- ggplot(sp_attk_by_tpye, aes(x=type1, y=sp_attack, fill = type1))+
  geom_bar(stat= "identity") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot2

plot3 <-ggplot(sp_attdef_comp, aes(x=type1, y=level, fill = type))+
  geom_bar(stat= "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

plot3
