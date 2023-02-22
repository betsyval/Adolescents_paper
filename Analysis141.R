############################################################################################################
#Analysis Stuck on the last: the last-presented benefit as an index of attentional refreshing in adolescents
###########################################################################################################

rm(list = ls(all.names = TRUE))

packages = c("ggplot2", "scales", "gridExtra", "BayesFactor", "tidyverse","psych")
sapply(packages, library, character.only = TRUE)

#load the data
Data<-read.csv("Data141.csv")


length(unique(Data$ss)) #36 after exclusion 35
unique(Data$ss)

#check accuracy -table accuracy in the results
accuracy<-aggregate(acc ~ ss, data = Data, FUN = "mean")
to_exclude<-accuracy %>% filter(acc<0.55) 
Data<- subset(Data, !(ss %in% to_exclude$ss))
min(accuracy$acc) #0.36, not good  - after exclusion, min 0.6

#accuracy per probe_type as reported in the paper
aggregate(acc~probe_type, data =Data, FUN=function(x) round(mean(x),2))

#only RT of correct responses
Data <- subset(Data, acc==1)
Data <- select(Data,-acc)

min(Data$rt)
Data<-Data %>% filter(rt>=150) #removal of abnormally fast trials

#remove probe_type=new
Data <- subset(Data, probe_type!="new")

means<-aggregate(rt ~ probe_type, data = Data, FUN = "mean")
ster<-aggregate(rt ~ probe_type, data = Data, FUN = "sd")
ster$rt<-ster$rt/sqrt(length(unique(Data$ss)))
colnames(ster)[2] <- "se"

data4plot<-merge(means,ster,by=c("probe_type")) 
data4plot$probe_type<-factor(data4plot$probe_type)
levels(data4plot$probe_type)<-c("last-presented","not-last-presented")
data4plot$probe_type<-ordered(data4plot$probe_type, levels = c("not-last-presented","last-presented"))

# Figure 2a
tiff("bar_graph_141_Fig2.tiff", width = 6, height = 6, units = "in", res = 300)
dodge <- position_dodge(width=0.9)
bar <- ggplot(data4plot, aes(probe_type, rt, fill=probe_type))+
  geom_bar(stat = "identity",position = dodge,colour="black")+
  geom_errorbar(aes(ymin = rt-se, ymax = rt+se), width=0.2, position = dodge)+ylab("REACTION TIME (ms)")+ xlab("PROBE TYPE")+
  scale_y_continuous(breaks=c(200, 400, 600, 800, 1000))+
  theme(panel.background= element_rect(fill="transparent", colour=NA))+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+
  theme(legend.position="none")+
  scale_fill_manual(values = c("white","gray42"))
bar
dev.off()


# t tests as reported in paper
Data.avg<-aggregate(rt~ ss+probe_type, data=Data,FUN="mean")
ttest<-ttestBF(x=Data.avg$rt[Data.avg$probe_type=="lastpresented"], y=Data.avg$rt[Data.avg$probe_type=="otherpresented"], paired=TRUE, nullInterval=c(-Inf,0) )
ttest 

# Figure 2b
Data.avg$probe_type<-factor(Data.avg$probe_type)
levels(Data.avg$probe_type)<-c("last-presented","not-last-presented")
Data.avg$probe_type<-ordered(Data.avg$probe_type, levels = c("not-last-presented","last-presented"))

tiff("point_graph_141_Fig3.tiff", width = 6, height = 6, units = "in", res = 300)
point <- ggplot(Data.avg, aes(probe_type, rt))+
    geom_point()+
  ylab("REACTION TIME (ms)")+ 
  xlab("PROBE TYPE")+
  geom_line(aes(group=ss))
point
dev.off()


