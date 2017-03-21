########   Behavrioual result
########   read file
########   All graphs can be found in this project repository

########  rdata contains partcipants` behavioural result (reaction time and percent correct)
library(readr)
rdata <- read_csv("~/Documents/Psychology L4/Maxi/data/rdata.csv")
View(rdata)

######   Scatterplot-Regression line
######   scatterplot-RT-regressionline
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


######   scatterplot-acc-regressionline 
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

#######   scatterplot-acc (Basic plot/jitter,label)

ggplot(rdata1, aes(x=rdata1$Task, y=acc,colour=Participants))+
  geom_point(size=3, alpha=0.5, position=position_jitter(w=0.2, h=0))+
  scale_y_continuous(limits = c(50, 100), breaks = seq(50, 100, 10))+
  labs(x='Tasks',y='Percent Correct')+
  stat_summary(fun.y = median, geom="line")+
  theme(legend.position='none')+
  theme_bw()+
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle=1))


######    scatterplot-rt (Basic plot/jitter,label)

ggplot(rdata1, aes(x=rdata1$Task, y=rt,colour=Participants))+
  geom_point(size=3, alpha=0.5, position=position_jitter(w=0.2, h=0))+
  labs(x='Tasks',y='Reaction Time (in ms)')+
  theme(title = element_text(size=15))+
  theme_bw()+
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle = 1))


#########  Run an AVOVA for behavrioual data
summarise(anova, mean(acc_py), mean(acc_po), mean(rt_py), mean(rt_po))
##calculate the rest 
summary(rdata1)
aov(acc~as.factor(Participants)*as.factor(Task),data= rdata1)
etaSquared(aov_acc, type = 2, anova = FALSE )
summary.aov(aov_acc)


##########  Mutual Information Graph
##########  Individual`s MI 

##########  open timepoint MI

epy=expressive task young participants
gpy=gender task young participants
epo=expressive task old participants
gpo=gender task old participants

library(readr)
epy <- read_csv("~/Documents/Psychology L4/Maxi/data/epy.csv")
gpy <- read_csv("~/Documents/Psychology L4/Maxi/data/gpy.csv")
epo <- read_csv("~/Documents/Psychology L4/Maxi/data/epo.csv")
gpo <- read_csv("~/Documents/Psychology L4/Maxi/data/gpo.csv")

#########   data reshape (change from wide format into long format, 3 columns: Timpoint, MI and Subject)

library(reshape2)
epy = melt(epy, id.vars="Timepoint", value.name="MI", variable.name="Subject")
gpy = melt(gpy, id.vars="Timepoint", value.name="MI", variable.name="Subject")
epo = melt(epo, id.vars="Timepoint", value.name="MI", variable.name="Subject")
gpo = melt(gpo, id.vars="Timepoint", value.name="MI", variable.name="Subject")

######### apart from plotting individual`s MI, we need to highlight meadian value, it can be done by writing a function in R
######### here I just save each median in another file and import them seperately
######### open median MI

epy_median <- read_csv("~/Documents/Psychology L4/Maxi/data/epy_median.csv")
gpy_median <- read_csv("~/Documents/Psychology L4/Maxi/data/gpy_median.csv")
epo_median <- read_csv("~/Documents/Psychology L4/Maxi/data/epo_median.csv")
gpo_median <- read_csv("~/Documents/Psychology L4/Maxi/data/gpo_median.csv")

######### plotting mean is an alternative 
######### open mean MI
epy_mean <- read_csv("~/Documents/Psychology L4/Maxi/data/epy_mean.csv")
gpy_mean <- read_csv("~/Documents/Psychology L4/Maxi/data/gpy_mean.csv")
epo_mean <- read_csv("~/Documents/Psychology L4/Maxi/data/epo_mean.csv")
gpo_mean <- read_csv("~/Documents/Psychology L4/Maxi/data/gpo_mean.csv")


#### GPY
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

#### EPY
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

#### GPO
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

#### EPO
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


######################### plot median of 4 groups in one graph
########## open file
library(readr)
median4 <- read_csv("~/Documents/Psychology L4/Maxi/data/median4.csv")
mean4 <- read_csv("~/Documents/Psychology L4/Maxi/data/mean4.csv")

#########  data reshape
library(reshape2)
median = melt(median4, id.vars="Timepoint", value.name="MI", variable.name="MI_")
mean = melt(mean4, id.vars="Timepoint", value.name="MI", variable.name="MI_")

########## Median-4groups
ggplot(median,aes(Timepoint, MI, group=MI_, color=MI_)) +
  geom_line(size=0.6)+
  scale_y_continuous(breaks = seq(-1, 0.5, 0.006))+
  scale_x_continuous(breaks = seq(-300, 600, 100))+
  labs(title='Meadian Mutual Information by two groups two tasks', x='Timepoint',y='Mutual Information')

########## Mean-4groups
ggplot(mean,aes(Timepoint, MI, group=MI_, color=MI_)) +
  geom_line(size=0.6)+
  scale_y_continuous(breaks = seq(-1, 0.5, 0.006))+
  scale_x_continuous(breaks = seq(-300, 600, 100))+
  labs(title='Mean Mutual Information by two groups two tasks', x='Timepoint',y='Mutual Information')

######################### median by age (Task effect)
########### open file & reshape
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

######################### median by task (Age-difference)
######### open file & reshape
agemedian <- read_csv("~/Documents/Psychology L4/Maxi/data/agemedian.csv")

library(reshape2)
agemedian = melt(agemedian, id.vars="timepoint", value.name="MI", variable.name="age")

library(ggplot2)
ggplot(agemedian,aes(timepoint, MI, group=age, color=age)) +
  geom_line(size=0.6)+
  scale_y_continuous(breaks = seq(-1, 0.5, 0.006))+
  scale_x_continuous(breaks = seq(-300, 600, 100))+
  labs(title='Median Mutual Information by Age group', x='Timepoint',y='MI of two age groups')


######### 50 Integration Timepoint Scatter plot
######### 50 Integration Timepoint is the timepoint when participants integrate 50% of MI. It is calculated in matlab
######### reference and explanation can be found in https://github.com/GRousselet/matlab_stats/blob/master/mi50it.m
######### open file

library(readr)
it <- read_csv("/Volumes/CZ Cgate/Maxidata/matlab_statshd.m/it.csv")
View(it)

######### 50it
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
