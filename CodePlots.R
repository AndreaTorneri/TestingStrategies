
library(ggplot2)
library(latex2exp)
library(ggpubr)

#FIGURE 1
finalSizedata.525<-read.csv("~/Desktop/Testing/5-2.5/base_FinSizeProp.csv")
schooldaylost.525<-read.csv("~/Desktop/Testing/5-2.5/base_n.schooldayslost.csv")
scaling.parm<-400
data.525<-data.frame("Label"=c(rep("FinalSize",length(finalSizedata.525$output)),rep("SchoolDaysLost",length(schooldaylost.525$output))), "Value"=c(finalSizedata.525$output,schooldaylost.525$output/scaling.parm), "Strategy"=c(as.character(finalSizedata.525$strategy),as.character(schooldaylost.525$strategy))   )

scaling.parm2<-400
finalSizedata.58<-read.csv("~/Desktop/Testing/5-8/base_FinSizeProp.csv")
schooldaylost.58<-read.csv("~/Desktop/Testing/5-8/base_n.schooldayslost.csv")
data.58<-data.frame("Label"=c(rep("FinalSize",length(finalSizedata.58$output)),rep("SchoolDaysLost",length(schooldaylost.58$output))), "Value"=c(finalSizedata.58$output,schooldaylost.58$output/scaling.parm2), "Strategy"=c(as.character(finalSizedata.58$strategy),as.character(schooldaylost.58$strategy))   )

data.overall<-data.frame("Scenario"=c(rep("Wuhan",length(data.525$Label)),rep("Delta",length(data.525$Label))), "SummaryMeasure"=c(as.character(data.525$Label), as.character(data.58$Label)), "Value"=c(data.525$Value,data.58$Value), "Strategy"=c(as.character(data.525$Strategy),as.character(data.58$Strategy))     )
data.overall$Scenario<-factor(data.overall$Scenario, levels = c("Wuhan","Delta"))
data.overall$Strategy<-factor(data.overall$Strategy, levels = c("SI","RS","RS_A"))



ggplot(data = data.overall, aes(x=Strategy, fill=SummaryMeasure,y=Value))+geom_boxplot()+scale_fill_brewer(palette="Dark2", labels=c(" Attack Rate","NSDL") )+stat_summary(fun.y=mean, geom="point", shape=c(17,15,17,15,17,15,17,15,17,15,17,15), size=3, color=rep(c("royalblue","gold"),6), fill="black", aes(group=SummaryMeasure),position=position_dodge(.75)) +theme(
  panel.background = element_rect(fill = "white",
                                  colour = "gray",
                                  size = 0.75, linetype = "solid"),
  panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                  colour = "gray"), 
  panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"),
  legend.key.size = unit(1, "cm"),
  legend.key.width = unit(1,"cm"),
  legend.background = element_rect(fill = "white"),
  legend.key = element_rect(fill = "white", color = NA),
  legend.text = element_text(size=14),
  legend.position = "top",
  #legend.position = "top",
  legend.title = element_text(size=15, face = "bold"),
  axis.text.x = element_text(size=14, angle = 45, vjust = 0.5, hjust = 0.5),
  axis.title = element_text(size=15),
  axis.text.y = element_text(size=14),
  strip.text = element_text(size = 15)
)+ylab(("Attack Rate"))+xlab("Testing Strategy")+scale_y_continuous(sec.axis = sec_axis(~.*scaling.parm, name = "NSDL"))+facet_wrap( ~ Scenario, scales = "fixed")+scale_x_discrete(labels=c("SI","ReaS","RepS"))+labs(fill="Model Outcome")
#save 9:5.5



#FIGURE 2
scaling.parm<-100
finalSizedata.58SA6<-read.csv("~/Desktop/Testing/5-8/sa6_FinSizeProp.csv")
schooldaylost.58SA6<-read.csv("~/Desktop/Testing/5-8/sa6_n.schooldayslost.csv")
finalSizedata.58SA6<-finalSizedata.58SA6[which(finalSizedata.58SA6$strategy=="RS_A"),]
schooldaylost.58SA6<-schooldaylost.58SA6[which(schooldaylost.58SA6$strategy=="RS_A"),]


data.58SA6<-data.frame("SummaryMeasure"=c(rep("FinalSize",length(finalSizedata.58SA6$output)),rep("SchoolDaysLost",length(schooldaylost.58SA6$output))), "Value"=c(finalSizedata.58SA6$output,schooldaylost.58SA6$output/scaling.parm), "Threshold"=c(as.character(finalSizedata.58SA6$threshold_class),as.character(schooldaylost.58SA6$threshold_class))   )

data.58SA6$Threshold<-factor(data.58SA6$Threshold, levels = c(2,4,6,8,1000))


ggplot(data = data.58SA6, aes(x=Threshold, fill=SummaryMeasure,y=Value))+geom_boxplot()+scale_fill_brewer(palette="Dark2", labels=c(" Attack Rate","NSDL") )+stat_summary(fun.y=mean, geom="point", shape=c(17,15,17,15,17,15,17,15,17,15), size=3, color=rep(c("royalblue","gold"),5), fill="black", aes(group=SummaryMeasure),position=position_dodge(.75)) +theme(
  panel.background = element_rect(fill = "white",
                                  colour = "gray",
                                  size = 0.75, linetype = "solid"),
  panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                  colour = "gray"), 
  panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"),
  legend.key.size = unit(1, "cm"),
  legend.key.width = unit(1,"cm"),
  legend.background = element_rect(fill = "white"),
  legend.key = element_rect(fill = "white", color = NA),
  legend.text = element_text(size=14),
  #legend.position = "top",
  #legend.position = "top",
  legend.title = element_text(size=15, face = "bold"),
  axis.text.x = element_text(size=14, angle = 45, vjust = 0.5, hjust = 0.5),
  axis.title = element_text(size=15),
  axis.text.y = element_text(size=14),
  strip.text = element_text(size = 15)
)+ylab(("Attack Rate"))+xlab("Class Closure Threshold")+scale_y_continuous(sec.axis = sec_axis(~.*scaling.parm, name = "NSDL"))+labs(fill="Model Outcome")+scale_x_discrete(labels = c("2","4","6","8","No Threshold"))


#FIGURE 3
scaling.parm<-30
finalSizedata.58SA7<-read.csv("~/Desktop/Testing/5-8/sa7_FinSizeProp.csv")
finalSizedata.58SA7RepS<-finalSizedata.58SA7[which(finalSizedata.58SA7$strategy=="RS_A"),]
finalSizedata.58SA6<-read.csv("~/Desktop/Testing/5-8/sa6_FinSizeProp.csv")
finalSizedata.58SA6RepS<-finalSizedata.58SA6[which(finalSizedata.58SA6$strategy=="RS_A"),]
finalSizedata.58SA6RepS<-finalSizedata.58SA6RepS[which(finalSizedata.58SA6RepS$threshold_class==8),]
schooldaylost.58SA7<-read.csv("~/Desktop/Testing/5-8/sa7_n.schooldayslost.csv")
schooldaylost.58SA7RepS<-schooldaylost.58SA7[which(schooldaylost.58SA7$strategy=="RS_A"),]
schooldaylost.58SA6<-read.csv("~/Desktop/Testing/5-8/sa6_n.schooldayslost.csv")
schooldaylost.58SA6RepS<-schooldaylost.58SA6[which(schooldaylost.58SA6$strategy=="RS_A"),]
schooldaylost.58SA6RepS<-schooldaylost.58SA6RepS[which(schooldaylost.58SA6RepS$threshold_class==8),]
data.58SA7<-data.frame("SummaryMeasure"=c(rep("FinalSize",length(finalSizedata.58SA7RepS$output)+length(finalSizedata.58SA6RepS$output)),rep("SchoolDaysLost",length(schooldaylost.58SA6RepS$output)+length(schooldaylost.58SA7RepS$output))), "Value"=c(finalSizedata.58SA7RepS$output,finalSizedata.58SA6RepS$output,schooldaylost.58SA7RepS$output/scaling.parm,schooldaylost.58SA6RepS$output/scaling.parm), "NTest"=c(rep("2",length(finalSizedata.58SA7RepS$output)),rep("1",length(finalSizedata.58SA6RepS$output)),rep("2",length(schooldaylost.58SA7RepS$output)),rep("1",length(schooldaylost.58SA6RepS$output)) )   )
#data.58SA6$Threshold<-factor(data.58SA6$Threshold, levels = c(2,4,6,8,1000))


ggplot(data = data.58SA7, aes(x=NTest, fill=SummaryMeasure,y=Value))+geom_boxplot()+scale_fill_brewer(palette="Dark2", labels=c(" Attack Rate","NSDL") )+stat_summary(fun.y=mean, geom="point", shape=c(17,15,17,15), size=3, color=rep(c("royalblue","gold"),2), fill="black", aes(group=SummaryMeasure),position=position_dodge(.75)) +theme(
  panel.background = element_rect(fill = "white",
                                  colour = "gray",
                                  size = 0.75, linetype = "solid"),
  panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                  colour = "gray"), 
  panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"),
  legend.key.size = unit(1, "cm"),
  legend.key.width = unit(1,"cm"),
  legend.background = element_rect(fill = "white"),
  legend.key = element_rect(fill = "white", color = NA),
  legend.text = element_text(size=14),
  #legend.position = "top",
  #legend.position = "top",
  legend.title = element_text(size=15, face = "bold"),
  axis.text.x = element_text(size=14, angle = 45, vjust = 0.5, hjust = 0.5),
  axis.title = element_text(size=15),
  axis.text.y = element_text(size=14),
  strip.text = element_text(size = 15)
)+ylab(("Attack Rate"))+xlab("Number of Tests Per Week")+scale_y_continuous(sec.axis = sec_axis(~.*scaling.parm, name = "NSDL"))+labs(fill="Model Outcome")



#FIGURE SA1
finalSizedata.18<-read.csv("~/Desktop/Testing/1-8/base_FinSizeProp.csv")
schooldaylost.18<-read.csv("~/Desktop/Testing/1-8/base_n.schooldayslost.csv")
scaling.parm<-300
data.18<-data.frame("Label"=c(rep("FinalSize",length(finalSizedata.18$output)),rep("SchoolDaysLost",length(schooldaylost.18$output))), "Value"=c(finalSizedata.18$output,schooldaylost.18$output/scaling.parm), "Strategy"=c(as.character(finalSizedata.18$strategy),as.character(schooldaylost.18$strategy))   )

scaling.parm2<-300
finalSizedata.108<-read.csv("~/Desktop/Testing/10-8/base_FinSizeProp.csv")
schooldaylost.108<-read.csv("~/Desktop/Testing/10-8/base_n.schooldayslost.csv")
data.108<-data.frame("Label"=c(rep("FinalSize",length(finalSizedata.108$output)),rep("SchoolDaysLost",length(schooldaylost.108$output))), "Value"=c(finalSizedata.108$output,schooldaylost.108$output/scaling.parm2), "Strategy"=c(as.character(finalSizedata.108$strategy),as.character(schooldaylost.108$strategy))   )

data.overall<-data.frame("Seeds"=c(rep("1 Seed",length(data.18$Label)),rep("10 Seeds",length(data.108$Label))), "SummaryMeasure"=c(as.character(data.18$Label), as.character(data.108$Label)), "Value"=c(data.18$Value,data.108$Value), "Strategy"=c(as.character(data.18$Strategy),as.character(data.108$Strategy))     )
data.overall$Scenario<-factor(data.overall$Seeds, levels = c("1 Seed","10 Seeds"))
data.overall$Strategy<-factor(data.overall$Strategy, levels = c("SI","RS","RS_A"))



ggplot(data = data.overall, aes(x=Strategy, fill=SummaryMeasure,y=Value))+geom_boxplot()+scale_fill_brewer(palette="Dark2", labels=c(" Attack Rate","NSDL") )+stat_summary(fun.y=mean, geom="point", shape=c(17,15,17,15,17,15,17,15,17,15,17,15), size=3, color=rep(c("royalblue","gold"),6), fill="black", aes(group=SummaryMeasure),position=position_dodge(.75)) +theme(
  panel.background = element_rect(fill = "white",
                                  colour = "gray",
                                  size = 0.75, linetype = "solid"),
  panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                  colour = "gray"), 
  panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"),
  legend.key.size = unit(1, "cm"),
  legend.key.width = unit(1,"cm"),
  legend.background = element_rect(fill = "white"),
  legend.key = element_rect(fill = "white", color = NA),
  legend.text = element_text(size=14),
  legend.position = "top",
  #legend.position = "top",
  legend.title = element_text(size=15, face = "bold"),
  axis.text.x = element_text(size=14, angle = 45, vjust = 0.5, hjust = 0.5),
  axis.title = element_text(size=15),
  axis.text.y = element_text(size=14),
  strip.text = element_text(size = 15)
)+ylab(("Attack Rate"))+xlab("Testing Strategy")+scale_y_continuous(sec.axis = sec_axis(~.*scaling.parm, name = "NSDL"))+facet_wrap( ~ Scenario, scales = "fixed")+scale_x_discrete(labels=c("SI","ReaS","RepS"))+labs(fill="Model Outcome")


#FIGURE SA2
finalSizedata.sa1<-read.csv("~/Desktop/Testing/5-8/sa1_FinSizeProp.csv")
finalSizedata.sa1$prop_lambda_w<-as.character(finalSizedata.sa1$prop_lambda_w)
finalSizedata.sa1$strategy<-factor(finalSizedata.sa1$strategy, levels = c("SI","RS","RS_A"))
finalSizedata.sa1$gruop1<-c(rep("1",100),rep("2",100),rep("3",100),rep("4",100),rep("5",100),rep("6",100),rep("7",100),rep("8",100),rep("9",100))

schooldaylost.sa1<-read.csv("~/Desktop/Testing/5-8/sa1_n.schooldayslost.csv")
schooldaylost.sa1$prop_lambda_w<-as.character(schooldaylost.sa1$prop_lambda_w)
schooldaylost.sa1$strategy<-factor(schooldaylost.sa1$strategy, levels = c("SI","RS","RS_A"))
schooldaylost.sa1$gruop1<-c(rep("1",100),rep("2",100),rep("3",100),rep("4",100),rep("5",100),rep("6",100),rep("7",100),rep("8",100),rep("9",100))


sa1fs<-ggplot(data = finalSizedata.sa1, aes(x=strategy, fill=prop_lambda_w,y=output))+geom_boxplot()+scale_fill_brewer(palette="Accent", name="Between-Classes Contact Rate", labels=c(unname(TeX(c("$0.2\\lambda_b$", "$0.5\\lambda_b$","$0.9\\lambda_b$")))) )+stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="royalblue", fill="black",position=position_dodge(.75), aes(group=gruop1)) +theme(
  panel.background = element_rect(fill = "white",
                                  colour = "gray",
                                  size = 0.75, linetype = "solid"),
  panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                  colour = "gray"), 
  panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"),
  legend.key.size = unit(1, "cm"),
  legend.key.width = unit(1,"cm"),
  legend.background = element_rect(fill = "white"),
  legend.key = element_rect(fill = "white", color = NA),
  legend.text = element_text(size=14),
  legend.position = "top",
  #legend.position = "top",
  legend.title = element_text(size=15, face = "bold"),
  axis.text.x = element_text(size=14, angle = 45, vjust = 0.5, hjust = 0.5),
  axis.title = element_text(size=15),
  axis.text.y = element_text(size=14),
  strip.text = element_text(size = 15)
)+ylab(("Attack Rate"))+xlab("Testing Strategy")+scale_x_discrete(labels=c("SI","ReaS","RepS"))

sa1nsdl<-ggplot(data = schooldaylost.sa1, aes(x=strategy, fill=prop_lambda_w,y=output))+geom_boxplot()+scale_fill_brewer(palette="Accent", name="Between-Classes Contact Rate", labels=c(unname(TeX(c("$0.2\\lambda_b$", "$0.5\\lambda_b$","$0.9\\lambda_b$")))) )+stat_summary(fun.y=mean, geom="point", shape=15, size=3, color="royalblue", fill="black",position=position_dodge(.75), aes(group=gruop1)) +theme(
  panel.background = element_rect(fill = "white",
                                  colour = "gray",
                                  size = 0.75, linetype = "solid"),
  panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                  colour = "gray"), 
  panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"),
  legend.key.size = unit(1, "cm"),
  legend.key.width = unit(1,"cm"),
  legend.background = element_rect(fill = "white"),
  legend.key = element_rect(fill = "white", color = NA),
  legend.text = element_text(size=14),
  legend.position = "top",
  #legend.position = "top",
  legend.title = element_text(size=15, face = "bold"),
  axis.text.x = element_text(size=14, angle = 45, vjust = 0.5, hjust = 0.5),
  axis.title = element_text(size=15),
  axis.text.y = element_text(size=14),
  strip.text = element_text(size = 15)
)+ylab(("NSDL"))+xlab("Testing Strategy")+scale_x_discrete(labels=c("SI","ReaS","RepS"))

ggarrange(sa1fs,sa1nsdl,legend = "top",common.legend = TRUE)

#FIGURE SA3
#child
finalSizedata.sa3.ch<-read.csv("~/Desktop/Testing/5-8/sa2_FinSizeProp.csv")
finalSizedata.sa3.ch$prop_immune_child<-as.character(finalSizedata.sa3.ch$prop_immune_child)
finalSizedata.sa3.ch$strategy<-factor(finalSizedata.sa3.ch$strategy, levels = c("SI","RS","RS_A"))
finalSizedata.sa3.ch$gruop1<-c(rep("1",100),rep("2",100),rep("3",100),rep("4",100),rep("5",100),rep("6",100),rep("7",100),rep("8",100),rep("9",100))

schooldaylost.sa3.ch<-read.csv("~/Desktop/Testing/5-8/sa2_n.schooldayslost.csv")
schooldaylost.sa3.ch$prop_immune_child<-as.character(schooldaylost.sa3.ch$prop_immune_child)
schooldaylost.sa3.ch$strategy<-factor(schooldaylost.sa3.ch$strategy, levels = c("SI","RS","RS_A"))
schooldaylost.sa3.ch$gruop1<-c(rep("1",100),rep("2",100),rep("3",100),rep("4",100),rep("5",100),rep("6",100),rep("7",100),rep("8",100),rep("9",100))


sa3fs.ch<-ggplot(data = finalSizedata.sa3.ch, aes(x=strategy, fill=prop_immune_child,y=output))+geom_boxplot()+scale_fill_brewer(palette="Pastel1", name="Immune Proportion Children", labels=c(unname(TeX(c("$0.2", "$0.3$","$0.4$")))) )+stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="royalblue", fill="black",position=position_dodge(.75), aes(group=gruop1)) +theme(
  panel.background = element_rect(fill = "white",
                                  colour = "gray",
                                  size = 0.75, linetype = "solid"),
  panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                  colour = "gray"), 
  panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"),
  legend.key.size = unit(1, "cm"),
  legend.key.width = unit(1,"cm"),
  legend.background = element_rect(fill = "white"),
  legend.key = element_rect(fill = "white", color = NA),
  legend.text = element_text(size=14),
  legend.position = "top",
  #legend.position = "top",
  legend.title = element_text(size=15, face = "bold"),
  axis.text.x = element_text(size=14, angle = 45, vjust = 0.5, hjust = 0.5),
  axis.title = element_text(size=15),
  axis.text.y = element_text(size=14),
  strip.text = element_text(size = 15)
)+ylab(("Attack Rate"))+xlab("Testing Strategy")+scale_x_discrete(labels=c("SI","ReaS","RepS"))

sa3nsdl.ch<-ggplot(data = schooldaylost.sa3.ch, aes(x=strategy, fill=prop_immune_child,y=output))+geom_boxplot()+scale_fill_brewer(palette="Pastel1", name="Proportion Immune Children", labels=c(unname(TeX(c("$0.2$", "$0.3$","$0.4$")))) )+stat_summary(fun.y=mean, geom="point", shape=15, size=3, color="royalblue", fill="black",position=position_dodge(.75), aes(group=gruop1)) +theme(
  panel.background = element_rect(fill = "white",
                                  colour = "gray",
                                  size = 0.75, linetype = "solid"),
  panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                  colour = "gray"), 
  panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"),
  legend.key.size = unit(1, "cm"),
  legend.key.width = unit(1,"cm"),
  legend.background = element_rect(fill = "white"),
  legend.key = element_rect(fill = "white", color = NA),
  legend.text = element_text(size=14),
  legend.position = "top",
  #legend.position = "top",
  legend.title = element_text(size=15, face = "bold"),
  axis.text.x = element_text(size=14, angle = 45, vjust = 0.5, hjust = 0.5),
  axis.title = element_text(size=15),
  axis.text.y = element_text(size=14),
  strip.text = element_text(size = 15)
)+ylab(("NSDL"))+xlab("Testing Strategy")+scale_x_discrete(labels=c("SI","ReaS","RepS"))

ggarrange(sa3fs.ch,sa3nsdl.ch,legend = "top",common.legend = TRUE)

#adult
finalSizedata.sa3.ad<-read.csv("~/Desktop/Testing/5-8/sa3_FinSizeProp.csv")
finalSizedata.sa3.ad$prop_immune_adult<-as.character(finalSizedata.sa3.ad$prop_immune_adult)
finalSizedata.sa3.ad$strategy<-factor(finalSizedata.sa3.ad$strategy, levels = c("SI","RS","RS_A"))
finalSizedata.sa3.ad$gruop1<-c(rep("1",100),rep("2",100),rep("3",100),rep("4",100),rep("5",100),rep("6",100))

schooldaylost.sa3.ad<-read.csv("~/Desktop/Testing/5-8/sa3_n.schooldayslost.csv")
schooldaylost.sa3.ad$prop_immune_adult<-as.character(schooldaylost.sa3.ad$prop_immune_adult)
schooldaylost.sa3.ad$strategy<-factor(schooldaylost.sa3.ad$strategy, levels = c("SI","RS","RS_A"))
schooldaylost.sa3.ad$gruop1<-c(rep("1",100),rep("2",100),rep("3",100),rep("4",100),rep("5",100),rep("6",100))


sa3fs.ad<-ggplot(data = finalSizedata.sa3.ad, aes(x=strategy, fill=prop_immune_adult,y=output))+geom_boxplot()+scale_fill_brewer(palette="Pastel2", name="Immune Proportion Adults", labels=c(unname(TeX(c("$0.2", "$0.3$","$0.4$")))) )+stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="royalblue", fill="black",position=position_dodge(.75), aes(group=gruop1)) +theme(
  panel.background = element_rect(fill = "white",
                                  colour = "gray",
                                  size = 0.75, linetype = "solid"),
  panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                  colour = "gray"), 
  panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"),
  legend.key.size = unit(1, "cm"),
  legend.key.width = unit(1,"cm"),
  legend.background = element_rect(fill = "white"),
  legend.key = element_rect(fill = "white", color = NA),
  legend.text = element_text(size=14),
  legend.position = "top",
  #legend.position = "top",
  legend.title = element_text(size=15, face = "bold"),
  axis.text.x = element_text(size=14, angle = 45, vjust = 0.5, hjust = 0.5),
  axis.title = element_text(size=15),
  axis.text.y = element_text(size=14),
  strip.text = element_text(size = 15)
)+ylab(("Attack Rate"))+xlab("Testing Strategy")+scale_x_discrete(labels=c("SI","ReaS","RepS"))

sa3nsdl.ad<-ggplot(data = schooldaylost.sa3.ad, aes(x=strategy, fill=prop_immune_adult,y=output))+geom_boxplot()+scale_fill_brewer(palette="Pastel2", name="Proportion Immune Adults", labels=c(unname(TeX(c("$0.2$", "$0.3$","$0.4$")))) )+stat_summary(fun.y=mean, geom="point", shape=15, size=3, color="royalblue", fill="black",position=position_dodge(.75), aes(group=gruop1)) +theme(
  panel.background = element_rect(fill = "white",
                                  colour = "gray",
                                  size = 0.75, linetype = "solid"),
  panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                  colour = "gray"), 
  panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"),
  legend.key.size = unit(1, "cm"),
  legend.key.width = unit(1,"cm"),
  legend.background = element_rect(fill = "white"),
  legend.key = element_rect(fill = "white", color = NA),
  legend.text = element_text(size=14),
  legend.position = "top",
  #legend.position = "top",
  legend.title = element_text(size=15, face = "bold"),
  axis.text.x = element_text(size=14, angle = 45, vjust = 0.5, hjust = 0.5),
  axis.title = element_text(size=15),
  axis.text.y = element_text(size=14),
  strip.text = element_text(size = 15)
)+ylab(("NSDL"))+xlab("Testing Strategy")+scale_x_discrete(labels=c("SI","ReaS","RepS"))

ggarrange(sa3fs.ad,sa3nsdl.ad,legend = "top",common.legend = TRUE)



#FIGURE SA4
finalSizedata.sa4<-read.csv("~/Desktop/Testing/5-8/sa4_FinSizeProp.csv")
finalSizedata.sa4$strategy<-factor(finalSizedata.sa4$strategy, levels = c("SI","RS","RS_A"))
finalSizedata.sa4$gruop1<-c(rep("1",100),rep("2",100),rep("3",100))

schooldaylost.sa4<-read.csv("~/Desktop/Testing/5-8/sa4_n.schooldayslost.csv")
schooldaylost.sa4$strategy<-factor(schooldaylost.sa4$strategy, levels = c("SI","RS","RS_A"))
schooldaylost.sa4$gruop1<-c(rep("1",100),rep("2",100),rep("3",100))


sa4fs<-ggplot(data = finalSizedata.sa4, aes(x=strategy, fill=strategy,y=output))+geom_boxplot()+scale_fill_brewer(palette="Accent")+stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="royalblue", fill="black",position=position_dodge(.75), aes(group=gruop1)) +theme(
  panel.background = element_rect(fill = "white",
                                  colour = "gray",
                                  size = 0.75, linetype = "solid"),
  panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                  colour = "gray"), 
  panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"),
  legend.key.size = unit(1, "cm"),
  legend.key.width = unit(1,"cm"),
  legend.background = element_rect(fill = "white"),
  legend.key = element_rect(fill = "white", color = NA),
  legend.text = element_text(size=14),
  legend.position = "none",
  #legend.position = "top",
  legend.title = element_text(size=15, face = "bold"),
  axis.text.x = element_text(size=14, angle = 45, vjust = 0.5, hjust = 0.5),
  axis.title = element_text(size=15),
  axis.text.y = element_text(size=14),
  strip.text = element_text(size = 15)
)+ylab(("Attack Rate"))+xlab("Testing Strategy")+scale_x_discrete(labels=c("SI","ReaS","RepS"))

sa4nsdl<-ggplot(data = schooldaylost.sa4, aes(x=strategy, fill=strategy,y=output))+geom_boxplot()+scale_fill_brewer(palette="Accent")+stat_summary(fun.y=mean, geom="point", shape=15, size=3, color="royalblue", fill="black",position=position_dodge(.75), aes(group=gruop1)) +theme(
  panel.background = element_rect(fill = "white",
                                  colour = "gray",
                                  size = 0.75, linetype = "solid"),
  panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                  colour = "gray"), 
  panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"),
  legend.key.size = unit(1, "cm"),
  legend.key.width = unit(1,"cm"),
  legend.background = element_rect(fill = "white"),
  legend.key = element_rect(fill = "white", color = NA),
  legend.text = element_text(size=14),
  legend.position = "none",
  #legend.position = "top",
  legend.title = element_text(size=15, face = "bold"),
  axis.text.x = element_text(size=14, angle = 45, vjust = 0.5, hjust = 0.5),
  axis.title = element_text(size=15),
  axis.text.y = element_text(size=14),
  strip.text = element_text(size = 15)
)+ylab(("NSDL"))+xlab("Testing Strategy")+scale_x_discrete(labels=c("SI","ReaS","RepS"))

ggarrange(sa4fs,sa4nsdl,legend = "none")


#FIGURE SA5
finalSizedata.sa7<-read.csv("~/Desktop/Testing/5-8/sa7_FinSizeProp.csv")
finalSizedata.sa7.repS<-finalSizedata.sa7[which(finalSizedata.sa7$strategy=="RS_A"),]
finalSizedata.sa6<-read.csv("~/Desktop/Testing/5-8/sa6_FinSizeProp.csv")
finalSizedata.sa6.tc8<-finalSizedata.sa6[which(finalSizedata.sa6$threshold_class==8),]
finalSizedata.sa6.tc8.repS<-finalSizedata.sa6.tc8[which(finalSizedata.sa6.tc8$strategy=="RS_A"),]
finalSizedata.ntest<-data.frame("output"=c(finalSizedata.sa6.tc8.repS$output,finalSizedata.sa7.repS$output),"ntest"=c(rep("1",length(finalSizedata.sa6.tc8.repS$output)),rep("2",length(finalSizedata.sa7.repS$output))) )

schooldaylost.sa7<-read.csv("~/Desktop/Testing/5-8/sa7_n.schooldayslost.csv")
schooldaylost.sa7.repS<-schooldaylost.sa7[which(schooldaylost.sa7$strategy=="RS_A"),]
schooldaylost.sa6<-read.csv("~/Desktop/Testing/5-8/sa6_n.schooldayslost.csv")
schooldaylost.sa6.tc8<-schooldaylost.sa6[which(schooldaylost.sa6$threshold_class==8),]
schooldaylost.sa6.tc8.repS<-schooldaylost.sa6.tc8[which(schooldaylost.sa6.tc8$strategy=="RS_A"),]
schooldaylost.ntest<-data.frame("output"=c(schooldaylost.sa6.tc8.repS$output,schooldaylost.sa7.repS$output),"ntest"=c(rep("1",length(schooldaylost.sa6.tc8.repS$output)),rep("2",length(schooldaylost.sa7.repS$output))) )

#finalSizedata.sa7$gruop1<-c(rep("1",100),rep("2",100),rep("3",100))

#schooldaylost.sa7<-read.csv("~/Desktop/Testing/5-8/sa7_schooldaylost.csv")
#schooldaylost.sa7$strategy<-factor(schooldaylost.sa7$strategy, levels = c("SI","RS","RS_A"))
#schooldaylost.sa7$gruop1<-c(rep("1",100),rep("2",100),rep("3",100))


saNtestfs<-ggplot(data = finalSizedata.ntest, aes(x=ntest, fill=ntest,y=output))+geom_boxplot()+scale_fill_brewer(palette="Set1")+stat_summary(fun.y=mean, geom="point", shape=17, size=3, color="gold", fill="black",position=position_dodge(.75)) +theme(
  panel.background = element_rect(fill = "white",
                                  colour = "gray",
                                  size = 0.75, linetype = "solid"),
  panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                  colour = "gray"), 
  panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"),
  legend.key.size = unit(1, "cm"),
  legend.key.width = unit(1,"cm"),
  legend.background = element_rect(fill = "white"),
  legend.key = element_rect(fill = "white", color = NA),
  legend.text = element_text(size=14),
  legend.position = "none",
  #legend.position = "top",
  legend.title = element_text(size=15, face = "bold"),
  axis.text.x = element_text(size=14, angle = 45, vjust = 0.5, hjust = 0.5),
  axis.title = element_text(size=15),
  axis.text.y = element_text(size=14),
  strip.text = element_text(size = 15)
)+ylab(("Attack Rate"))+xlab("Number of Tests (Per Week)")+scale_x_discrete(labels=c("1","2"))

saNtestnsdl<-ggplot(data = schooldaylost.ntest, aes(x=ntest, fill=ntest,y=output))+geom_boxplot()+scale_fill_brewer(palette="Set1")+stat_summary(fun.y=mean, geom="point", shape=15, size=3, color="gold", fill="black",position=position_dodge(.75)) +theme(
  panel.background = element_rect(fill = "white",
                                  colour = "gray",
                                  size = 0.75, linetype = "solid"),
  panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                  colour = "gray"), 
  panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"),
  legend.key.size = unit(1, "cm"),
  legend.key.width = unit(1,"cm"),
  legend.background = element_rect(fill = "white"),
  legend.key = element_rect(fill = "white", color = NA),
  legend.text = element_text(size=14),
  legend.position = "none",
  #legend.position = "top",
  legend.title = element_text(size=15, face = "bold"),
  axis.text.x = element_text(size=14, angle = 45, vjust = 0.5, hjust = 0.5),
  axis.title = element_text(size=15),
  axis.text.y = element_text(size=14),
  strip.text = element_text(size = 15)
)+ylab(("NSDL"))+xlab("Number of Tests (Per Week)")+scale_x_discrete(labels=c("1","2"))

ggarrange(saNtestfs,saNtestnsdl,legend = "none")












#FIGURE SA6
scaling.parm<-500
finalSizedata.58SA8<-read.csv("~/Desktop/Testing/5-8/sa8_FinSizeProp.csv")
schooldaylost.58SA8<-read.csv("~/Desktop/Testing/5-8/sa8_n.schooldayslost.csv")
finalSizedata.58SA8<-finalSizedata.58SA8[which(finalSizedata.58SA8$strategy=="RS_A"),]
schooldaylost.58SA8<-schooldaylost.58SA8[which(schooldaylost.58SA8$strategy=="RS_A"),]


data.58SA8<-data.frame("SummaryMeasure"=c(rep("FinalSize",length(finalSizedata.58SA8$output)),rep("SchoolDaysLost",length(schooldaylost.58SA8$output))), "Value"=c(finalSizedata.58SA8$output,schooldaylost.58SA8$output/scaling.parm), "Threshold"=c(as.character(finalSizedata.58SA8$threshold_school),as.character(schooldaylost.58SA8$threshold_school))   )

data.58SA8$Threshold<-factor(data.58SA8$Threshold, levels = c(20,50,100,1000))


ggplot(data = data.58SA8, aes(x=Threshold, fill=SummaryMeasure,y=Value))+geom_boxplot()+scale_fill_brewer(palette="Dark2", labels=c(" Attack Rate","NSDL") )+stat_summary(fun.y=mean, geom="point", shape=c(17,15,17,15,17,15,17,15), size=3, color=rep(c("royalblue","gold"),4), fill="black", aes(group=SummaryMeasure),position=position_dodge(.75)) +theme(
  panel.background = element_rect(fill = "white",
                                  colour = "gray",
                                  size = 0.75, linetype = "solid"),
  panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                  colour = "gray"), 
  panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"),
  legend.key.size = unit(1, "cm"),
  legend.key.width = unit(1,"cm"),
  legend.background = element_rect(fill = "white"),
  legend.key = element_rect(fill = "white", color = NA),
  legend.text = element_text(size=14),
  #legend.position = "top",
  #legend.position = "top",
  legend.title = element_text(size=15, face = "bold"),
  axis.text.x = element_text(size=14, angle = 45, vjust = 0.5, hjust = 0.5),
  axis.title = element_text(size=15),
  axis.text.y = element_text(size=14),
  strip.text = element_text(size = 15)
)+ylab(("Attack Rate"))+xlab("School Closure Threshold")+scale_y_continuous(sec.axis = sec_axis(~.*scaling.parm, name = "NSDL"))+labs(fill="Model Outcome")+scale_x_discrete(labels = c("20","50","100","No Threshold"))






