# Data communication and visualization, final project
# team E R code
# needs the 'all_data.csv' and 'airlines.csv'  to run

# installs
#install.packages('foreign')
#install.packages('ggplot2')
#install.packages('ggrepel')
#install.packages('ggthemes')
#install.packages('readxl')
#install.packages('R.utils')
#install.packages('data.table')
#install.packages('dplyr')
#install.packages('scales')
#install.packages('scales')
#install.packages('gridExtra')
#install.packages('grid')
#install.packages('zoo')

# load
library(foreign)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(readxl)
library('R.utils')
library(data.table)
library(dplyr)
library(scales)
library(gridExtra)
library(grid)
library(zoo)


# Load the complete dataset 2004-07
loaded = fread("all_data.csv")
all_data =loaded
rm(loaded)

# convert to factors etc
all_data <- all_data %>% mutate_if(is.character, as.factor)
all_data <- all_data %>% mutate_if(is.integer, as.numeric)

colnames(all_data)
str(all_data)

# keep only the 2 airports seperated  to Origin and Dest
origins = all_data %>% filter(Origin %in% c("JFK","HOU"))
dests = all_data %>% filter(Dest %in% c("JFK","HOU"))

# = = = = = = = = = = = = = = PLOTS START HERE = = = = = = = = = = = = = =

# = = = = = = = = = = == = =  NEW PLOTS AFTER HANDS ON = = = = = = = = = = 

#Size/rank comparison between JFK/HOU and all others or

d1= all_data %>%
  group_by(Year, Origin) %>%
  summarize(cnt = n())

pp= list()

for (yr in 2004:2007)
{
  dtest = subset(d1, Year==yr)
  dtest = dtest%>%top_n(n = 45, wt = cnt)

  i=yr-2004+1
if (i==1) {
  p  = ggplot(dtest,aes(cnt/1000, reorder(Origin, cnt),
                     fill = ifelse(Origin=='HOU','HOU',ifelse(Origin=='JFK','JFK','Other'))))+
    scale_fill_manual(values = c('#F28E2B','#4E79A7','gray'))+
    geom_col()+
    ggtitle(yr)+
    scale_y_discrete(name="Airport")+
    scale_x_continuous(labels=scales::comma)+
    labs(x="Nr. of flights in '000s")+
      theme(legend.position = 'none',
   axis.text.y= element_text(face="bold",size=8),
   axis.title.y = element_text(colour="black", size=15,face="bold"),
  axis.title.x = element_text(colour="black", size=10,face="bold"),
  axis.text.x = element_text(colour="black",size=8,face="bold"),
  plot.title = element_text(hjust=0.5,colour="black", size=15,face="bold"))
}
 else {
  p  = ggplot(dtest,aes(cnt/1000, reorder(Origin, cnt),
                     fill = ifelse(Origin=='HOU','HOU',ifelse(Origin=='JFK','JFK','Other'))))+
    scale_fill_manual(values = c('#F28E2B','#4E79A7','gray'))+
    geom_col()+
    ggtitle(yr)+
    scale_y_discrete()+
    scale_x_continuous(labels=scales::comma)+
    labs(x="Nr. of flights in '000s")+
      theme(legend.position = 'none', 
   axis.title.y = element_blank(),
   axis.title.x = element_text(colour="black", size=10,face="bold"),
   axis.text.y= element_text(face="bold",size=8),
   axis.text.x = element_text(colour="black",size=8,face="bold"),
  plot.title = element_text(hjust=0.5,colour="black", size=15,face="bold"))

}
  pp[[i]] = p
}
grid.arrange(pp[[1]],pp[[2]],pp[[3]],pp[[4]], ncol=4, top = textGrob('JFK/HOU Ranks 2004-07',gp=gpar(fontsize=15,face='bold')))
# - - - - - - - - - - - - - - - -   - - - - - - - - - - - - --  - - - -   --    - - -

# Aggregated percentage of  annual flights from/by JFK+HOU to total US (2004-07) annual ( R )

e1= all_data %>%
  group_by(Year,Month,Origin) %>%
  summarize(cnt = n())

e2 = e1%>%
  group_by(Year,Month) %>%
  summarize(mean = mean(cnt))

jfk= subset(e1, Origin=='JFK')
hou= subset(e1, Origin=='HOU')

total <- merge(e2,jfk,by=c("Year","Month"))
total <- merge(total,hou,by=c("Year","Month"))
total= subset(total, select=(-c(Origin.x,Origin.y)))

colnames(total)[colnames(total) == 'cnt.x'] <- 'jfk'
colnames(total)[colnames(total) == 'cnt.y'] <- 'hou'

total$Date <- as.yearmon(paste(total$Year, total$Month), "%Y %m")

Sys.setlocale("LC_ALL","English")

ggplot(total)+
  geom_line(aes(Date, jfk,colour='JFK'),size=1.5)+
  geom_line(aes(Date, hou,colour='HOU'),size=1.5)+
  geom_line(aes(Date, mean,colour='mean'),size=1.5)+
  scale_color_manual(
    name = "labels",
    values = c("HOU" = "#F28E2B", "JFK" = "#4E79A7", "mean"= "#404040" )
    )+
  ggtitle('Number of flights evolution')+
 labs(x="Date",y="Nr. of flights")+
 # scale_x_date(labels = date_format("%Y-%m"),
  #             breaks = as.Date("2015-09-01") + 
   #              months(seq(0, 10, by = 2)))+
  
      theme( axis.title.y = element_text(colour="black",face="bold",size=12),
legend.title = element_blank(),
legend.text = element_text(colour="black", size=12,face="bold"),
legend.position =c(0.92,0.65),
legend.key.size=unit(1,'cm'),
legend.background=element_blank(),
axis.title.x = element_text(colour="black", size=12,face="bold"),
axis.text.y= element_text(colour="black",size=12),
axis.text.x = element_text(colour="black",size=12),
plot.title = element_text(hjust=0.5,colour="black", size=15,face="bold"))
  
#     - - - - - - - - - - -   - - - - - - - - - - -   --  - - -   - - - - - - - - -
# boxplot-violin
all_data$new='All'

all_data$new[all_data$Origin=="HOU"]='HOU'
all_data$new[ all_data$Origin=="JFK"]='JFK'


f1=all_data%>%
  select(Year,Month,Origin,DepDelay,new)

#nrow(f1)

all_delay =subset(f1,(DepDelay>15))
all_del_pos= nrow(all_delay)/nrow(f1)

hou_del = subset(f1, Origin=='HOU' & (DepDelay>15))#| DepDelay<(-15))
h_del_pos = nrow(hou_del)/sum(f1$Origin=="HOU",na.rm=TRUE)

jfk_del = subset(f1, Origin=='JFK' & (DepDelay>15))# | DepDelay<(-15)))
j_del_pos= nrow(jfk_del)/sum(f1$Origin=="HOU",na.rm=TRUE)

ggplot(subset(f1, DepDelay>15), aes(x=new, y=DepDelay,col=new))+
  geom_violin(outlier.shape=NA,trim=FALSE)+geom_boxplot(width=0.1, outlier.shape = NA)+
  annotate(geom="text", x=2, y=10, label= paste(round(h_del_pos,4)*100,'%'),
           color="red")+
  annotate(geom="text", x=3, y=10, label= paste(round(j_del_pos,4)*100,'%'),
           color="red")+
  annotate(geom="text", x=1, y=10, label= paste(round(all_del_pos,4)*100,'%'),
           color="red")+
  scale_y_continuous(limits = c(10, 80))+
  scale_color_manual(
    name = "airport",
    values = c("#404040","#F28E2B","#4E79A7" )
  )+
  ggtitle('Distribution of Delays for HOU/JFK and rest 2004-07')

# - - - -- - - - - - - - - - - # - - - -- - - -- - - - - -- 
# plot for the airlines

## find airlines for each airport
airlines = origins %>%
   group_by(Origin,UniqueCarrier) %>%
   summarize(cnt= n()) %>%
   mutate(freq=cnt/sum(cnt),rank = dense_rank(desc(cnt)))

# find common carriers of 2 airports
carriers_jfk = airlines$UniqueCarrier[airlines$Origin=='JFK']
carriers_hou = airlines$UniqueCarrier[airlines$Origin=='HOU']
common = Reduce(intersect, list(carriers_jfk,carriers_hou))

# get the names of the airlines
names_airlines = read.csv('carriers.csv')

airlines = merge(airlines,names_airlines,by.x='UniqueCarrier',by.y='Code')

airlines = airlines %>%  mutate(freq2= ifelse(Origin =="HOU",freq,freq*-1))

# clean airline names
#airlines$Description = gsub(r"{\([^\)]+\)}","",as.character(airlines$Description))
airlines$Description = gsub('Airlines.*', '', airlines$Description)
airlines$Description = gsub('Airways.*', '', airlines$Description)
airlines$Description = gsub('Air .*', '', airlines$Description)


#airlines$rank[airlines$Origin=='HOU']=NA
ggplot(data=airlines,aes(y = reorder(Description,freq2,max), x =freq2,fill=Origin,label=paste0(round(freq*100,2), "%"))) + 
geom_bar(stat='identity',position='identity',width=0.5)+
geom_text(position=position_dodge(width=0.4),
          vjust=ifelse(airlines$freq2<0,0.6,0.2),
          hjust=ifelse(airlines$freq2<0,1.1,-0.2),
          color="black")+
scale_x_continuous(labels = function(x) paste0(abs(x)*100),breaks = pretty(airlines$freq2))+ # Multiply by 100 & add %  
scale_fill_manual(values=c('#f28e2b','#4e79a7'))+
labs(x="percentage of flights (%)",y="Carrier",
title="Flights share per company JFK vs HOU")+
theme(plot.title = element_text(hjust=0.5,colour="black", size=20,face="bold"),
axis.title.x = element_text(colour="black", size=15,face="bold"),
axis.title.y = element_text(colour="black", size=15,face="bold"),
legend.title = element_blank(),
legend.text = element_text(colour="black", size=10,face="bold"),
legend.position =c(0.9,0.45),
legend.key.size=unit(1,'cm'),
legend.background=element_blank(),
axis.text.x = element_text(colour="black",size=15,face="bold"),
axis.text.y = element_text(colour="black",size=12,face="bold"),

panel.background = element_blank())

# - - -- - - -- - - - - - - - - - - END - - - -- - - - - -- - - - - -- - - - 