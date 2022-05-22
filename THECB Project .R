# SET-UP--------------------------------------------------------------------------------------

#import libraries
library(dplyr)
library(ggplot2)
library(readxl)
library(ggthemes)
library(plotly)
library(extrafont)
library(viridis) 
library(RColorBrewer)


#lists
disciplines <- c("Liberal Arts", "Science", "Fine Arts","Teacher Education","Agriculture",
                 "Engineering","Home Economics", "Law","Social Service","Library Science","Veterinary Science",
                 "Vocational Training","Physical Training","Health Services","Pharmacy","Business Admin",
                 "Optometry","Teacher Ed-Practice Teaching","Technology","Nursing")
institutions <- c("UT-San Antonio", "UH", "TTU", "UT-Dallas", "UT-Arlington", "UNT", "UT-El Paso", "TxStU-SM")


#eliminate scientific notation
options(scipen=999)


#read excel file
project <- read_excel("Desktop/Work/R -THECB Project.xlsx")
View(project)

#possible palletes
colourCount = length(unique(mtcars$hp))


# Stacked Bar Chart -------------------------------------------------------

# SCH Total OR FTE OR % of FTE or % of SCH at *institution*  
# 2016-18 

read1 <- readline(prompt = "SCH_Total, FTEs, Perc_of_Total_FTE,or Perc_of_Total_SCH ? ")
read2 <- readline(prompt = "Enter a institution?  ")


inst <- project%>%
  filter(INSTITUTION ==toString(read2))

insti = toString(read2)

regio = c(insti)

r1 <- toString(read1)

if (r1 == "SCH_Total"){
  ggplot(inst, aes(Year, SCH_Total, fill = DISCIPLINE),) +
    geom_col() +
    theme_classic() +
    labs(title = paste("SCH Total at", regio[1]),
         subtitle = " 2016-18",
         y = "SCH Total")+
    scale_fill_discrete(name = "Disciplines") +
    scale_y_continuous(labels = function(x) paste0(x / 1000, "k"))+
    theme(text = element_text(size=13))
 # scale_fill_manual(values = getPalette(colourCount))
} else if (r1 == "FTEs")
  {
  ggplot(inst, aes(Year, FTEs, fill = DISCIPLINE)) +
    geom_col() +
    theme_classic()+
    labs(title = paste("FTEs at", regio[1]),
         subtitle = "2016-2018" )+
    scale_fill_discrete(name = "Disciplines") +
    theme(text = element_text(size=13))+
    scale_y_continuous(labels = function(x) paste0(x / 1000, "k"))

  }else if (r1 == "Perc_of_Total_FTE"){
  ggplot(inst, aes(Year, Perc_of_Total_FTE, fill = DISCIPLINE)) +
    geom_col() +
    theme_classic()+
    labs(title = paste("% of Total FTE at", regio[1] ),
         subtitle = "2016-2018")+
    theme(text = element_text(size=13)) +
    scale_fill_discrete(name = "Disciplines") +
    labs( y = " % of Total FTE") +
    scale_y_continuous(labels = scales::percent)
}else if (r1 == "Perc_of_Total_SCH"){
  ggplot(inst, aes(Year, Perc_of_Total_SCH, fill = DISCIPLINE)) +
    geom_col() +
    theme_classic()+
    labs(title = paste("% of Total SCH at", regio[1] ),
         subtitle = "2016-2018")+
    theme(text = element_text(size=13)) +
    scale_fill_discrete(name = "Disciplines") +
    labs( y = " % of Total SCH") +
    scale_y_continuous(labels = scales::percent)
}else
  print("ERROR: Incorrect Input")









#Stacked bar chart ____________________________________________________________________________



# % of Total FTE or SCH_Total of FTEs or % of Total SCH 
# 2018 from all Emerging Research Universities 


reading <- readline(prompt = "SCH_Total, FTEs, Perc_of_Total_FTE,or Perc_of_Total_SCH ? ")



rea1 <- toString(reading)
data <- project%>%
  filter(Year == 2018)
if (rea1 == "Perc_of_Total_FTE"){
  ggplot(data, aes(INSTITUTION, Perc_of_Total_FTE, fill = DISCIPLINE)) +
    geom_col() +
    theme_classic() +
    labs(title = paste("% of Total FTE by Discipline in 2018"),
         subtitle = "Emerging Research Universities",
         y = "% of Total FTE",
         x = "Institutions") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_discrete(name= "Disciplines") 

  }else if(rea1 == "FTEs"){
  ggplot(data, aes(INSTITUTION, FTEs, fill = DISCIPLINE)) +
    geom_col() +
    theme_classic() +
    labs(title = paste("FTE by Discipline in 2018"),
         subtitle = "Emerging Research Universities",
         y = "FTEs",
         x = "Institutions") +
    #legend title
    scale_fill_discrete(name= "Disciplines") +
    scale_y_continuous(labels = function(x) paste0(x / 1000, "k"))
    

}else if(rea1 =="SCH_Total"){
  ggplot(data, aes(INSTITUTION, SCH_Total, fill = DISCIPLINE)) +
    geom_col() +
    theme_classic() +
    labs(title = paste("SCH Total by Discipline in 2018"),
         subtitle = "Emerging Research Universities",
         y = "SCH Total",
         x = "Institutions") +
    scale_fill_discrete(name= "Disciplines") +
    scale_y_continuous(labels = function(x) paste0(x / 1000, "k"))
  
}else if (rea1 == "Perc_of_Total_SCH"){
  ggplot(data, aes(INSTITUTION, Perc_of_Total_SCH, fill = DISCIPLINE)) +
    geom_col() +
    theme_classic() +
    labs(title = paste("% of Total SCH by Discipline in 2018"),
         subtitle = "Emerging Research Universities",
         y = "% of Total SCH",
         x = "Institutions") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_discrete(name= "Disciplines") 

}else{
  print("Error:Try again")
}





# ggplotly ----------------------------------------------------------------


#previous chart
reading <- readline(prompt = "SCH_Total, FTEs, Perc_of_Total_FTE,or Perc_of_Total_SCH ? ")


rea1 <- toString(reading)
data <- project%>%
  filter(Year == 2018)
anim <- if (rea1 == "Perc_of_Total_FTE"){
  ggplot(data, aes(INSTITUTION, Perc_of_Total_FTE, fill = DISCIPLINE)) +
    geom_col() +
    theme_classic() +
    labs(title = paste("% of Total FTE by Discipline in 2018"),
         subtitle = "Emerging Research Universities",
         y = "% of Total FTE",
         x = "Institutions") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_discrete(name= "Disciplines") 
  
}else if(rea1 == "FTEs"){
  ggplot(data, aes(INSTITUTION, FTEs, fill = DISCIPLINE)) +
    geom_col() +
    theme_classic() +
    labs(title = paste("FTE by Discipline in 2018"),
         subtitle = "Emerging Research Universities",
         y = "FTEs",
         x = "Institutions") +
    #legend title
    scale_fill_discrete(name= "Disciplines") +
    scale_y_continuous(labels = function(x) paste0(x / 1000, "k"))
  
  
}else if(rea1 =="SCH_Total"){
  ggplot(data, aes(INSTITUTION, SCH_Total, fill = DISCIPLINE)) +
    geom_col() +
    theme_classic() +
    labs(title = paste("SCH Total by Discipline in 2018"),
         subtitle = "Emerging Research Universities",
         y = "SCH Total",
         x = "Institutions") +
    scale_fill_discrete(name= "Disciplines") +
    scale_y_continuous(labels = function(x) paste0(x / 1000, "k"))
  
}else if (rea1 == "Perc_of_Total_SCH"){
  ggplot(data, aes(INSTITUTION, Perc_of_Total_SCH, fill = DISCIPLINE)) +
    geom_col() +
    theme_classic() +
    labs(title = paste("% of Total SCH by Discipline in 2018"),
         subtitle = "Emerging Research Universities",
         y = "% of Total SCH",
         x = "Institutions") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_discrete(name= "Disciplines") 
  
}else{
  print("Error:Try again")
}
ggplotly(anim)




# Dodge Chart -------------------------------------------------------------


# % of Total FTE of Top 5 Disciplines in 2018
# FACET COLUMN CHART - DODGE NOT STACK


fac <- readline(prompt = "SCH_Total, FTEs, Perc_of_Total_FTE,or Perc_of_Total_SCH ? ")


f1 <- toString(fac)

if (f1 == "Perc_of_Total_FTE"){ 
  data1 <- project%>%
    filter(Year == 2018)%>%
    group_by(INSTITUTION)%>%
    top_n(5, Perc_of_Total_FTE)
ggplot(data1, aes(DISCIPLINE, Perc_of_Total_FTE, fill = DISCIPLINE)) +
  geom_col(position =  "dodge") +
  theme_bw() +
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_blank()) +
  labs(title = "% of Total FTE of Top 5 Disciplines in 2018",
       subtitle = "Emerging Research Universities") +
  scale_y_continuous(labels = scales::percent) +
  #legend title
  scale_fill_discrete(name= "Disciplines")+
  facet_wrap(.~INSTITUTION)+
  scale_fill_manual(values=cbbPalette)+
  geom_text(aes(label =sprintf("%0.2f %%", Perc_of_Total_FTE * 100)), vjust = -0.5)
 # theme(text=element_text(size=14,  family="Verdana"))
}else if(f1 == "Perc_of_Total_SCH"){

data1 <- project%>%
  filter(Year == 2018)%>%
  group_by(INSTITUTION)%>%
  top_n(5, Perc_of_Total_SCH)

ggplot(data1, aes(DISCIPLINE, Perc_of_Total_SCH, fill = DISCIPLINE)) +
  geom_col(position =  "dodge") +
  theme_bw() +
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_blank()) +
  labs(title = "% of Total SCH of Top 5 Disciplines in 2018",
       subtitle = "Emerging Research Universities") +
  scale_y_continuous(labels = scales::percent) +
  #legend title
  scale_fill_discrete(name= "Disciplines")+
  facet_wrap(.~INSTITUTION)+
  scale_fill_manual(values=cbbPalette)+
  geom_text(aes(label =sprintf("%0.2f %%", Perc_of_Total_SCH * 100)), vjust = -0.5)


}else if (f1 == "SCH_Total"){

data1 <- project%>%
  filter(Year == 2018)%>%
  group_by(INSTITUTION)%>%
  top_n(5, SCH_Total)

ggplot(data1, aes(DISCIPLINE, SCH_Total, fill = DISCIPLINE)) +
  geom_col(position =  "dodge") +
  theme_bw() +
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_blank()) +
  labs(title = "SCH_Total of Top 5 Disciplines in 2018",
       subtitle = "Emerging Research Universities") +
  #legend title
  scale_fill_discrete(name= "Disciplines")+
  facet_wrap(.~INSTITUTION)+
  scale_fill_manual(values=cbbPalette)+
  geom_text(aes(label =sprintf("%.0f ", SCH_Total)), vjust = -0.5)


}else if (f1 == "FTEs"){
data1 <- project%>%
  filter(Year == 2018)%>%
  group_by(INSTITUTION)%>%
  top_n(5, FTEs)

ggplot(data1, aes(DISCIPLINE, FTEs, fill = DISCIPLINE)) +
  geom_col(position =  "dodge") +
  theme_bw() +
  theme( axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_blank()) +
  labs(title = "FTE of Top 5 Disciplines in 2018",
       subtitle = "Emerging Research Universities") +
  #legend title
  scale_fill_discrete(name= "Disciplines")+
  facet_wrap(.~INSTITUTION)+
  scale_fill_manual(values=cbbPalette)+
  geom_text(aes(label =sprintf("%.0f ", FTEs)), vjust = -0.5)

}else
  print("ERROR: Incorrect Input")








# LINE GRAPHS -------------------------------------------------------------




# SCH TOTAL LINE GRAPHS OVER 3 YRS BY DISCIPLINE

# not good

ttu <- project%>%
  filter(INSTITUTION == "TTU")
ggplot(ttu, aes(Year, SCH_Total, col= DISCIPLINE))+
  geom_line()







#SCH Total or FTEs  in *insitution* and *discipline*
#2016-18
institutions
my <- readline(prompt = "SCH_Total or FTEs? ")
my.ins <- readline(prompt= "Enter a institution: ")
my.dis <- readline(prompt="Enter a discipline: ")


tbd <- project%>%
  filter(INSTITUTION == toString(my.ins))%>%
  filter(DISCIPLINE == toString(my.dis))

ins = toString(my.ins)
dis =  toString(my.dis)
m = toString(my)

region = c(ins, dis, m)

if (m  == "SCH_Total") {
ggplot(tbd, aes(x= as.factor(Year), y = SCH_Total, group = 1 ))+
  geom_line(size = 1)+
  theme_bw()+
  labs(title = paste("SCH Total at", region[1],
                     "in the", region[2], "program"),
       subtitle = "Between 2016-2018",
       y = "SCH Total",
       x= "Year") 


} else if (m == "FTEs") {
  ggplot(tbd, aes(x= as.factor(Year), y = FTEs, group = 1 ))+
  geom_line(size = 1)+
  theme_bw()+
  labs(title = paste("FTEs at", region[1],
                     "in the", region[2], "program"),
       subtitle = "Between 2016-2018",
       y = "FTEs",
       x= "Year") 
  
} else
  print("ERROR: Try again")







# SCH Total or FTEs at *institution* 2016-2018
#2016-18
#facet grid


m <- readline(prompt = "SCH_Total or FTEs? ")
my.ins1 <- readline(prompt= "Enter a institution: ")



tbd2 <- project%>%
  filter(INSTITUTION == toString(my.ins1))

m1 = toString(m)
ins = toString(my.ins1)

region = c(ins)

if (m1 == "SCH_Total") {
ggplot(tbd2, aes(x= as.factor(Year), y = SCH_Total, group = 1, col =  DISCIPLINE))+
  geom_line(size = 1)+
  theme_bw()+
  labs(title = paste("SCH Total at", region[1]),
       subtitle = "Between 2016-2018",
       y = "SCH Total",
       x= "Year") +
  facet_wrap(vars(DISCIPLINE), scales = "free")+
    theme(legend.position = "none" )

} else if (m1 == "FTEs"){
  ggplot(tbd2, aes(x= as.factor(Year), y = FTEs, group = 1, col =  DISCIPLINE))+
    geom_line(size = 1)+
    theme_bw()+
    labs(title = paste("FTEs at", region[1]),
         subtitle = "Between 2016-2018",
         y = "FTEs",
         x= "Year") +
    facet_wrap(vars(DISCIPLINE), scales = "free")+
    theme(legend.position = "none" )
}







#SCH Total or FTEs in *discipline* in ALL institutions
#2016-18
#facet grid

my.v <- readline(prompt = "SCH_Total or FTEs? ")
my.dis1 <- readline(prompt= "Enter a discipline: ")



tbd3 <- project%>%
  filter(DISCIPLINE == toString(my.dis1))

v <- toString(my.v)
dis1 = toString(my.dis1)
region = c(dis1)

if (v == "SCH_Total"){
ggplot(tbd3, aes(x= as.factor(Year), y = SCH_Total, group = 1, col =  DISCIPLINE))+
  geom_line(size = 1)+
  theme_bw()+
  labs(title = paste("SCH Total at all institutions by", region[1], "discipline"),
       subtitle = "Between 2016-2018",
       y = "SCH Total",
       x= "Year") +
  facet_wrap(vars(INSTITUTION), scales = "free")+
  theme(legend.position = "none" )
  
}else if (v == "FTEs") {
  ggplot(tbd3, aes(x= as.factor(Year), y = FTEs, group = 1, col =  DISCIPLINE))+
    geom_line(size = 1)+
    theme_bw()+
    labs(title = paste("FTEs at all institutions by", region[1], "discipline"),
         subtitle = "Between 2016-2018",
         y = "FTEs",
         x= "Year") +
    facet_wrap(vars(INSTITUTION), scales = "free")+
    theme(legend.position = "none" )

}else
  print("ERROR: Try again")




