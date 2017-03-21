# Age-related-Difference-in-Gender-and-Facial-Expressive-Task-Measured-with-EEG
Maxi project


########   Behavrioual result
########   read file

rdata contains partcipants` behavioural result (reaction time and percent correct)

library(readr)

rdata <- read_csv("~/Documents/Psychology L4/Maxi/data/rdata.csv")
View(rdata)

######   Scatterplot-Regression line

######   Scatterplot of Reaction time by Task by Subject 

library(ggplot2)

p=ggplot(rdata, aes(x = rt_gender, y = rt_exp))+
  geom_point(size=3, alpha=0.6, aes(color=Subject))

###### add regression line
p1=p+geom_smooth(aes(color=factor(Subject)),method = "lm", se = TRUE)

###### adjust limit/breaks/legent position/labs/font size
p1+scale_y_continuous(limits = c(400, 1300), breaks = seq(400, 1300, 200))+
  scale_x_continuous(limits=c(400,1300), breaks = seq(400, 1300, 200))+
  geom_abline(intercept = 0, alpha=0.6, slope = 1, size=0.2)+
  theme_bw()+
  theme(legend.position='right')+
  labs(x='Gender Task (in ms)',y='Expression Task (in ms)')+
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle = 1))
 

######   Scatterplot of percent correct by Task by Subject 

ggplot(rdata, aes(x = acc_gender, y = acc_exp))+
  geom_point(size=3, alpha=0.6, aes(color=Subject))+
  geom_smooth(aes(color=factor(Subject)), method = "lm", se = TRUE)+
  scale_y_continuous(limits = c(40, 100), breaks = seq(40, 100, 10))+
  scale_x_continuous(limits=c(40,100), breaks = seq(40, 100, 10))+
  labs(x='Gender Task (in percentage)',y='Expression Task(in percentage)')+
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle=90, hjust=1))+
  geom_abline(intercept = 0, alpha=0.6, slope = 1, size=0.2)+
  theme_bw()+
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle=1))

#######   Read another file
#######   rdata1 inserts a column for task, in order to create a scatter plot with categorical x-axis

rdata1 <- read_csv("~/Documents/Psychology L4/Maxi/data/rdata1.csv")
View(rdata1)

#######   Task Accuracy Scatterplot with categorical x-axis (Basic plot/jitter,label)

ggplot(rdata1, aes(x=rdata1$Task, y=acc,colour=Participants))+
  geom_point(size=3, alpha=0.5, position=position_jitter(w=0.2, h=0))+
  scale_y_continuous(limits = c(50, 100), breaks = seq(50, 100, 10))+
  labs(x='Tasks',y='Percent Correct')+
  stat_summary(fun.y = median, geom="line")+
  theme(legend.position='none')+
  theme_bw()+
  theme(text = element_text(size=15),
      axis.text.x = element_text(angle=1))
    

######    Reaction Time Scatterplot with categorical x-axis (Basic plot/jitter,label)

ggplot(rdata1, aes(x=rdata1$Task, y=rt,colour=Participants))+
  geom_point(size=3, alpha=0.5, position=position_jitter(w=0.2, h=0))+
  labs(x='Tasks',y='Reaction Time (in ms)')+
  theme(title = element_text(size=15))+
  theme_bw()+
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle = 1))
        
        
#########  Perform an AVOVA for behavrioual data
summarise(anova, mean(acc_py), mean(acc_po), mean(rt_py), mean(rt_po))
##calculate the rest 
summary(rdata1)
aov(acc~as.factor(Participants)*as.factor(Task),data= rdata1)
etaSquared(aov_acc, type = 2, anova = FALSE )
summary.aov(aov_acc)
