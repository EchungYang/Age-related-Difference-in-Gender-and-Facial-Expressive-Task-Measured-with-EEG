#read file
library(readr)
epy <- read_csv("~/Documents/Psychology L4/Maxi/data/epy.csv")
View(epy)
gpy <- read_csv("~/Documents/Psychology L4/Maxi/data/gpy.csv")
View(gpy)
rdata <- read_csv("~/Documents/Psychology L4/Maxi/data/rdata.csv")
View(rdata)
rdata1 <- read_csv("~/Documents/Psychology L4/Maxi/data/rdata1.csv")
View(rdata1)
epy_m <- read_csv("~/Documents/Psychology L4/Maxi/data/epy_m.csv")
View(epy_m)

# Scatterplot-Regression line
# Scatterplot of Reaction time by Task by Subject 
# Basic plot + point/colour/shape
library(ggplot2)
p=ggplot(rdata, aes(x = rt_gender, y = rt_exp))+
  geom_point(size=3, alpha=0.6, aes(color=Subject))
# add regression line
p1=p+geom_smooth(aes(color=factor(Subject)),method = "lm", se = TRUE)

# adjust limit/breaks/legent position/labs/font size
p1+scale_y_continuous(limits = c(400, 1300), breaks = seq(400, 1300, 200))+
  scale_x_continuous(limits=c(400,1300), breaks = seq(400, 1300, 200))+
  geom_abline(intercept = 0, alpha=0.6, slope = 1, size=0.2)+
  theme_bw()+
  theme(legend.position='right')+
  labs(x='Gender Task (in ms)',y='Expression Task (in ms)')+
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle = 1))


p=ggplot(rdata, aes(x = rt_gender, y = rt_exp))+
  geom_point(size=3, alpha=0.6, aes(color=Participants, shape=Participants))
# add regression line
p1=p+geom_smooth(aes(color=factor(Subject)),method = "lm", se = TRUE)

# Task accuracy by task with regression line
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

#Task Accuracy Scatterplot with categorical x-axis (Basic plot/jitter,label)
ggplot(rdata1, aes(x=rdata1$Task, y=acc,colour=Participants))+
  geom_point(size=3, alpha=0.5, position=position_jitter(w=0.2, h=0))+
  scale_y_continuous(limits = c(50, 100), breaks = seq(50, 100, 10))+
  labs(x='Tasks',y='Percent Correct')+
  stat_summary(fun.y = median, geom="line")+
  theme(legend.position='none')+
  theme_bw()+
  theme(text = element_text(size=15),
      axis.text.x = element_text(angle=1))


#Reaction Time Scatterplot with categorical x-axis (Basic plot/jitter,label)
ggplot(rdata1, aes(x=rdata1$Task, y=rt,colour=Participants))+
  geom_point(size=3, alpha=0.5, position=position_jitter(w=0.2, h=0))+
  labs(x='Tasks',y='Reaction Time (in ms)')+
  theme(title = element_text(size=15))+
  theme_bw()+
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle = 1))


#Perform an AVOVA for behavrioual data
summarise(anova, mean(acc_py), mean(acc_po), mean(rt_py), mean(rt_po))
##calculate the rest 
summary(rdata1)
aov(acc~as.factor(Participants)*as.factor(Task),data= rdata1)
etaSquared(aov_acc, type = 2, anova = FALSE )

###################### plot all participants` MI and highlight their mean/median
#open timepoint MI
library(readr)
epy <- read_csv("~/Documents/Psychology L4/Maxi/data/epy.csv")
gpy <- read_csv("~/Documents/Psychology L4/Maxi/data/gpy.csv")
epo <- read_csv("~/Documents/Psychology L4/Maxi/data/epo.csv")
gpo <- read_csv("~/Documents/Psychology L4/Maxi/data/gpo.csv")

#data reshape
library(reshape2)
epy = melt(epy, id.vars="Timepoint", value.name="MI", variable.name="Subject")
gpy = melt(gpy, id.vars="Timepoint", value.name="MI", variable.name="Subject")
epo = melt(epo, id.vars="Timepoint", value.name="MI", variable.name="Subject")
gpo = melt(gpo, id.vars="Timepoint", value.name="MI", variable.name="Subject")

#open median MI
epy_median <- read_csv("~/Documents/Psychology L4/Maxi/data/epy_median.csv")
gpy_median <- read_csv("~/Documents/Psychology L4/Maxi/data/gpy_median.csv")
epo_median <- read_csv("~/Documents/Psychology L4/Maxi/data/epo_median.csv")
gpo_median <- read_csv("~/Documents/Psychology L4/Maxi/data/gpo_median.csv")

#open mean MI
epy_mean <- read_csv("~/Documents/Psychology L4/Maxi/data/epy_mean.csv")
gpy_mean <- read_csv("~/Documents/Psychology L4/Maxi/data/gpy_mean.csv")
epo_mean <- read_csv("~/Documents/Psychology L4/Maxi/data/epo_mean.csv")
gpo_mean <- read_csv("~/Documents/Psychology L4/Maxi/data/gpo_mean.csv")

#open baseline MI
epybase <- read_csv("~/Documents/Psychology L4/Maxi/data/epybase.csv")
gpybase <- read_csv("~/Documents/Psychology L4/Maxi/data/gpybase.csv")
epobase <- read_csv("~/Documents/Psychology L4/Maxi/data/epobase.csv")
gpobase <- read_csv("~/Documents/Psychology L4/Maxi/data/gpobase.csv")

#GPY
library(ggplot2)
ggplot(gpy,aes(Timepoint, MI, group=Subject)) +
  geom_line(size=0.6)+
  scale_y_continuous(breaks = seq(-1, 0.5, 0.02))+
  scale_x_continuous(breaks = seq(-300, 600, 100))+
  labs(x='Timepoint in ms')+
  geom_line(data = gpy_median, aes(timepoint, median, group=Subject), color=7, size=1)+
  theme(legend.position="none")+
  theme_bw()+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(text = element_text(size=15),
        axis.text.x = element_text(hjust=1))
  


#EPY
ggplot(epy,aes(Timepoint, MI, group=Subject)) +
  geom_line(size=0.6)+
  scale_y_continuous(breaks = seq(-1, 0.5, 0.02))+
  scale_x_continuous(breaks = seq(-300, 600, 100))+
  labs(x='Timepoint in ms')+
  geom_line(data = epy_median, aes(timepoint, median, group=Subject), color=7, size=1)+
  theme(legend.position="none")+
  theme_bw()+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(text = element_text(size=15),
        axis.text.x = element_text(hjust=1))


#GPO
ggplot(gpo,aes(Timepoint, MI, group=Subject)) +
  geom_line(size=0.6)+
  scale_y_continuous(breaks = seq(-1, 0.5, 0.02))+
  scale_x_continuous(breaks = seq(-300, 600, 100))+
  labs(x='Timepoint in ms',y='Mutual Information-Gender Task')+
  geom_line(data = gpo_median, aes(timepoint, median, group=Subject), color=7, size=1)+
  theme(legend.position="none")+
  theme_bw()+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(text = element_text(size=15),
        axis.text.x = element_text(hjust=1))

#EPO
ggplot(epo,aes(Timepoint, MI, group=Subject)) +
  geom_line(size=0.6)+
  scale_y_continuous(breaks = seq(-1, 0.5, 0.02))+
  scale_x_continuous(breaks = seq(-300, 600, 100))+
  labs(x='Timepoint in ms',y='Mutual Information-Expressive Task')+
  geom_line(data = gpy_median, aes(timepoint, median, group=Subject), color=7, size=1)+
  theme(legend.position="none")+ 
  theme_bw()+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(text = element_text(size=15),
        axis.text.x = element_text(hjust=1))

  stat_summary(fun.y = mean, geom = "line",size = 0.5, col)


######################### plot 4 conditions in one graph
#open file
library(readr)
median4 <- read_csv("~/Documents/Psychology L4/Maxi/data/median4.csv")
mean4 <- read_csv("~/Documents/Psychology L4/Maxi/data/mean4.csv")

#data reshape
library(reshape2)
median = melt(median4, id.vars="Timepoint", value.name="MI", variable.name="MI_")
mean = melt(mean4, id.vars="Timepoint", value.name="MI", variable.name="MI_")

######  Median for each group each task
ggplot(median,aes(Timepoint, MI, group=MI_, color=MI_)) +
  geom_line(size=0.6)+
  scale_y_continuous(breaks = seq(-1, 0.5, 0.006))+
  scale_x_continuous(breaks = seq(-300, 600, 100))+
  labs(title='Meadian Mutual Information by two groups two tasks', x='Timepoint',y='Mutual Information')

######  Mean for each group each task
ggplot(mean,aes(Timepoint, MI, group=MI_, color=MI_)) +
  geom_line(size=0.6)+
  scale_y_continuous(breaks = seq(-1, 0.5, 0.006))+
  scale_x_continuous(breaks = seq(-300, 600, 100))+
  labs(title='Mean Mutual Information by two groups two tasks', x='Timepoint',y='Mutual Information')

######################### median by age
#open file & reshape
library(readr)
taskmedian <- read_csv("~/Documents/Psychology L4/Maxi/data/taskmedian.csv")

library(reshape2)
taskmedian = melt(taskmedian, id.vars="Timepoint", value.name="MI", variable.name="task")

library(ggplot2)
ggplot(taskmedian,aes(Timepoint, MI, group=task, color=task)) +
  geom_line(size=0.6)+
  scale_y_continuous(breaks = seq(-1, 0.5, 0.006))+
  scale_x_continuous(breaks = seq(-300, 600, 100))+
  labs(title='Median Mutual Information by Task', x='Timepoint',y='MI of two tasks')

######################### median by task
#open file & reshape
agemedian <- read_csv("~/Documents/Psychology L4/Maxi/data/agemedian.csv")

library(reshape2)
agemedian = melt(agemedian, id.vars="timepoint", value.name="MI", variable.name="age")

library(ggplot2)
ggplot(agemedian,aes(timepoint, MI, group=age, color=age)) +
  geom_line(size=0.6)+
  scale_y_continuous(breaks = seq(-1, 0.5, 0.006))+
  scale_x_continuous(breaks = seq(-300, 600, 100))+
  labs(title='Median Mutual Information by Age group', x='Timepoint',y='MI of two age groups')


######### 50it scatter plot
library(readr)
it <- read_csv("/Volumes/CZ Cgate/Maxidata/matlab_statshd.m/it.csv")
View(it)
scale_y_continuous(limits = c(100, 350))+
  
  library(ggplot2)
ggplot(it, aes(x=Task, y=It,colour=Subject))+
  geom_point(size=3, alpha=0.5, position=position_jitter(w=0.2, h=0))+
  scale_y_continuous(breaks = seq(80, 400, 20))+
  labs(x='Tasks',y='Time to integrate 50% of MIU')+
  stat_summary(fun.y = median, geom="line")+
  theme(legend.position='none')+
  theme(title = element_text(size=20, face = 'bold'), panel.border = element_blank())+
  theme_bw()+
  theme(text = element_text(size=15),
        axis.text.x = element_text(hjust=0.5))









