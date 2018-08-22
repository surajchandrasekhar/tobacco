library(foreign)
library(survey)
library(Hmisc)
library(dplyr)
library(ggplot2)
data <- sasxport.get("./data/LLCP2016.XPT")
states <- read.csv("./data/state_table.csv")
states$x.state <- states$value
states$value <- NULL
data_full <- data %>% inner_join(states, by="x.state")
smoke_data <- data_full %>% select(c(state, x.smoker3, smoke100, smokday2,stopsmk2, lastsmk2, usenow3, ecigaret, ecignow, x.llcpwt))
smoke_data <- smoke_data %>% filter(x.smoker3 < 5)
freq <- smoke_data %>% filter(smokday2 < 4)
freqsmoke <- freq %>% group_by(smokday2) %>% summarise(sum(x.llcpwt))
freqsmoke$type <- c("Every Day", "Some Days", "Not-At-All")
freqsmoke <- freqsmoke %>% select(c(type, `sum(x.llcpwt)`))
freqsmoke$value <- freqsmoke$`sum(x.llcpwt)`
freqsmoke$`sum(x.llcpwt)` <- NULL
p1 <- ggplot(data=freqsmoke, aes(x=type, y=value)) + geom_bar(stat="identity") + ggtitle("Frequency of People Smoking With Accounts to Sample Weights")
everyday <- smoke_data %>% filter(smokday2 == 1) 
somedays <- smoke_data %>% filter(smokday2 == 2)
never <- smoke_data %>% filter(smokday2 == 3)
# visual for smokeless tobacco use by everyday smokers
everysmokeless <- everyday %>% filter(usenow3 < 4) %>%  group_by(usenow3) %>% summarise(sum(x.llcpwt))
everysmokeless$type <- c("Every Day", "Some Days", "Not-At-All")
everysmokeless$value <- everysmokeless$`sum(x.llcpwt)`
everysmokeless$percentage <- everysmokeless$value/sum(everysmokeless$value)
everysmokeless$`sum(x.llcpwt)` <- NULL
everysmokeless <- everysmokeless %>% select(c(type, percentage))
p2 <- ggplot(data=everysmokeless, aes(x=type, y=percentage)) + geom_bar(stat="identity") + ggtitle("Use of Smokeless Tobacco by Everyday Smokers")
# visual for smokeless tobacco by people who do not smoke
neversmokeless <- never %>% filter(usenow3 < 4) %>%  group_by(usenow3) %>% summarise(sum(x.llcpwt))
neversmokeless$type <- c("Every Day", "Some Days", "Not-At-All")
neversmokeless$value <- neversmokeless$`sum(x.llcpwt)`
neversmokeless$percentage <- neversmokeless$value/sum(neversmokeless$value)
neversmokeless$`sum(x.llcpwt)` <- NULL
neversmokeless <- neversmokeless %>% select(c(type, percentage))
p3 <- ggplot(data=neversmokeless, aes(x=type, y=percentage)) + geom_bar(stat="identity") + ggtitle("Use of Smokeless Tobacco by people who do not smoke")
# visual for smokeless tobacco use by on and off smokers
somedaysmokeless <- somedays %>% filter(usenow3 < 4) %>%  group_by(usenow3) %>% summarise(sum(x.llcpwt))
somedaysmokeless$type <- c("Every Day", "Some Days", "Not-At-All")
somedaysmokeless$value <- somedaysmokeless$`sum(x.llcpwt)`
somedaysmokeless$percentage <- somedaysmokeless$value/sum(somedaysmokeless$value)
somedaysmokeless$`sum(x.llcpwt)` <- NULL
somedaysmokeless <- somedaysmokeless %>% select(c(type, percentage))
p4 <- ggplot(data=somedaysmokeless, aes(x=type, y=percentage)) + geom_bar(stat="identity") + ggtitle("Use of Smokeless Tobacco by Occassional Smokers")
# visual for e-cigarettes use by everyday smokers
everyecig <- everyday %>% filter(ecigaret < 3) %>%  group_by(ecigaret) %>% summarise(sum(x.llcpwt))
everyecig$type <- c("Yes", "No")
everyecig$value <- everyecig$`sum(x.llcpwt)`
everyecig$percentage <- everyecig$value/sum(everyecig$value)
everyecig$`sum(x.llcpwt)` <- NULL
everyecig <- everyecig %>% select(c(type, percentage))
p5 <- ggplot(data=everyecig, aes(x=type, y=percentage)) + geom_bar(stat="identity") + ggtitle("Use of E-Cigarettes by Everyday Smokers")
p5
# visual for smokeless tobacco by people who do not smoke
noecig <- never %>% filter(ecigaret < 3) %>%  group_by(ecigaret) %>% summarise(sum(x.llcpwt))
noecig$type <- c("Yes", "No")
noecig$value <- noecig$`sum(x.llcpwt)`
noecig$percentage <- noecig$value/sum(noecig$value)
noecig$`sum(x.llcpwt)` <- NULL
noecig <- noecig %>% select(c(type, percentage))
p6 <- ggplot(data=noecig, aes(x=type, y=percentage)) + geom_bar(stat="identity") + ggtitle("Use of E-Cigarettes by People who Never Smoke")
p6
# visual for smokeless tobacco use by on and off smokers
someecig <- somedays %>% filter(ecigaret < 3) %>%  group_by(ecigaret) %>% summarise(sum(x.llcpwt))
someecig$type <- c("Yes", "No")
someecig$value <- someecig$`sum(x.llcpwt)`
someecig$percentage <- someecig$value/sum(someecig$value)
someecig$`sum(x.llcpwt)` <- NULL
someecig <- someecig %>% select(c(type, percentage))
p7 <- ggplot(data=someecig, aes(x=type, y=percentage)) + geom_bar(stat="identity") + ggtitle("Use of E-Cigarettes by Occassional Smokers")
p7
#table showing smoking tendency percentages for each state
everystate <- everyday %>% group_by(state) %>% summarise(sum(x.llcpwt))
everystate$eval <- everystate$`sum(x.llcpwt)`
everystate$`sum(x.llcpwt)` <- NULL
somestate <- somedays %>% group_by(state) %>%  summarise(sum(x.llcpwt))
somestate$sval <- somestate$`sum(x.llcpwt)`
somestate$`sum(x.llcpwt)` <- NULL
nonestate <- never %>% group_by(state) %>% summarise(sum(x.llcpwt))
nonestate$nval <- nonestate$`sum(x.llcpwt)`
nonestate$`sum(x.llcpwt)` <- NULL
statesmoke <- everystate %>% inner_join(somestate, by="state")
statesmoke <- statesmoke %>% inner_join(nonestate, by="state")
for (i in 1:nrow(statesmoke)) {
  tot <- statesmoke[i,]$eval + statesmoke[i,]$sval + statesmoke[i,]$nval
  statesmoke[i,]$eval <- statesmoke[i,]$eval/tot
  statesmoke[i,]$sval <- statesmoke[i,]$sval/tot
  statesmoke[i,]$nval <- statesmoke[i,]$nval/tot
}
statesmoke10 <- statesmoke %>% arrange(desc(eval)) %>% head(10)
statesmokebot10 <- statesmoke %>% arrange(desc(nval)) %>% head(10)
#visualizations
p8 <- ggplot(data=statesmoke, aes(x=nval, y=eval)) + geom_point() + geom_vline(xintercept = mean(statesmoke$nval), color="red")+ ggtitle("People who Smoke Every Day vs People Who Never Smoke for Each State")
p8
#alternatives
vapestate <- smoke_data %>% filter(ecigaret == 1) %>% group_by(state) %>% summarise(sum(x.llcpwt))
vapestate$vapeval <- vapestate$`sum(x.llcpwt)`
vapestate$`sum(x.llcpwt)` <- NULL
novapestate <- smoke_data %>% filter(ecigaret == 2) %>% group_by(state) %>% summarise(sum(x.llcpwt))
novapestate$novapeval <- novapestate$`sum(x.llcpwt)`
novapestate$`sum(x.llcpwt)` <- NULL
dipstate <- smoke_data %>% filter(usenow3 == 1) %>% group_by(state) %>% summarise(sum(x.llcpwt))
dipstate$dipval <- dipstate$`sum(x.llcpwt)`
dipstate$`sum(x.llcpwt)` <- NULL
nodipstate <- smoke_data %>% filter(usenow3 == 2) %>% group_by(state) %>% summarise(sum(x.llcpwt))
nodipstate$nodipval <- nodipstate$`sum(x.llcpwt)`
nodipstate$`sum(x.llcpwt)` <- NULL
alt_table <- vapestate %>% inner_join(novapestate, by="state")
alt_table <- alt_table %>% inner_join(dipstate, by="state")
alt_table <- alt_table %>% inner_join(nodipstate, by="state")
alt_table$vapetot <- alt_table$vapeval + alt_table$novapeval
alt_table$vapeval <- alt_table$vapeval/alt_table$vapetot
alt_table$novapeval <- alt_table$novapeval/alt_table$vapetot
alt_table$diptot <- alt_table$dipval + alt_table$nodipval
alt_table$dipval <- alt_table$dipval/alt_table$diptot
alt_table$nodipval <- alt_table$nodipval/alt_table$vapetot
alt_table$diptot <- NULL
alt_table$vapetot <- NULL
p9 <- ggplot(data = alt_table, aes(x=vapeval, y=dipval)) + geom_point() + ggtitle("Vaping and Dipping Frequency in Each State")
p9
