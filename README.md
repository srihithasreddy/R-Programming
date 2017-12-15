# R-Programming
Employee Attrition

1.	How does the experience factor( number of total working years) influence performance factor.
 
setwd("C:/Users/lenovo pc/Desktop/Do not touch these/BE/R")
Employee= read.csv("WA_Fn-UseC_-HR-Employee-Attrition (2).csv")
emp_df<-data.frame(Employee)
performance_rating<-emp_df$Department
department<-emp_df$Department
performance_rating<-emp_df$PerformanceRating
TotalWorkingYears<-emp_df$TotalWorkingYears	
Compare_df<-data.frame(performance_rating, TotalWorkingYears)
install.packages("ggplot2")
library(ggplot2)
install.packages("reshape2")
library(reshape2)
cmp_long<-melt (Compare_df,id.vars = "department")
ggplot(cmp_long,aes(x=variable,y=value,fill=factor(department)))+geom_bar(stat="identity",position="dodge")+scale_fill_discrete(name="Department",breaks=c(1, 2, 3),labels=c("Sales", "Human Resources", "Research & Development"))+xlab("Comparision based on Department")+ylab("Range")

2.	What age group does the highest over time among all the other working age groups?

df_1 <- data.frame(Employee$OverTime,Employee$ï..Age)
mylist <- list(split(df_1,df_1$Employee.OverTime))
otlist <- data.frame(sapply(mylist,'[','Yes'))
colors <- c("mistyrose", "mistyrose2", "mistyrose3", "mistyrose2", "mistyrose1")
Age_Range<-otlist$Yes.Employee.ï..Age
hist(Age_Range,breaks=c(10,20,30,40,50,60),main="Age groups(Working OverTime)",col=colors)

3.	Which job role has highest raise in the salary?

grping <- group_by(Employee,JobRole) %>% summarise(average_hike =  round (mean (PercentSalaryHike)  ,2))
d=data.frame(grping)
hike<-d[,c('JobRole','average_hike')]
print(hike)
p <- plot_ly(hike, labels = ~JobRole, values = ~average_hike, type = 'pie',
 textposition = 'inside',
 textinfo = 'label+percent',
  insidetextfont = list(color = '#FFFFFF'),
  hoverinfo = 'text',
  text = ~paste('$', average_hike, 'is the average'),
  marker = list(colors = colors,
  line = list(color = '#FFFFFF', width = 1)),
  showlegend = FALSE) %>%
  layout(title = 'Average',
  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  print(p)

The same query has been displayed using 3D pie chart.

grping <- group_by(Employee,JobRole) %>% summarise(average_hike = round(mean(PercentSalaryHike),2))
d=data.frame(grping)
hike<-d[,2]
job_Role<-d[,1]
grping1<-table(Employee$JobRole)
pie3D(grping1,hike,labels=job_Role,explode = 0.1, main = "Pie Chart of hike ")

4.	Which people do their jobs for many years based on their education background

attrition<-Employee$Attrition
Educationfield<-Employee$EducationField
TotalWorkingYears<-Employee$TotalWorkingYears
attrition_Workinghors<-data.frame(attrition,TotalWorkingYears,Educationfield)
onlyyes <- list(split(attrition_Workinghors,attrition_Workinghors$attrition))
yeslist <- data.frame(sapply(onlyyes,'[','Yes'))
colors<-c("yellow",  "violet", "orange","blue", "pink", "cyan")
p <- plot_ly(yeslist, x = ~Yes.attrition, y = ~Yes.Educationfield, z = ~Yes.TotalWorkingYears, 
             marker = list(color = ~mpg, colorscale = colors, showscale = TRUE)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Attrition'),
                      yaxis = list(title = 'Education'),
                      zaxis = list(title = 'Years Worked')),
         annotations = list(
           x = 1.13,
           y = 1.05,
           text = 'range',
           xref = 'paper',
           yref = 'paper',
           showarrow = FALSE
         ))
print(p)

5.	Compare Monthly income and Monthly rate of every department.

monthlyrate<-Employee$MonthlyRate
Department<-Employee$Department
avg_grp <- group_by(Employee,Department) %>% summarise(average_monthlyincome = round(mean(MonthlyIncome)))
percentages<-data.frame(avg_grp)
avg_grp1 <- group_by(Employee,Department) %>% summarise(average_monthlyrate = round(mean(monthlyrate)))
percentages1<-data.frame(avg_grp1)

