data=read.csv("C:\\Users\\TANUDIP GHOSH\\OneDrive\\Desktop\\Depression.csv")
library(dplyr)
options("scipen"=999, "digits"=3)
head(data)
# Check the number of unique values
length(unique(data$Depression))

# View the unique values themselves
unique(data$Depression)

# Check the structure of the variable
str(data$Depression)
data=data%>%select(-Depression)
num_cols <- sapply(data, is.numeric)
data$Gender=as.factor(data$Gender)
data$Sleep.Duration=as.factor(data$Sleep.Duration)
data$Dietary.Habits=as.factor(data$Dietary.Habits)
data$Degree=as.factor(data$Degree)
data$Have.you.ever.had.suicidal.thoughts..=as.factor(data$Have.you.ever.had.suicidal.thoughts..)
data$Family.History.of.Mental.Illness=as.factor(data$Family.History.of.Mental.Illness)
library(corrplot)
#numeric-numeric independence test( correlation matrix)
corr_matrix <- cor(data[, num_cols], use="complete.obs")
corrplot(corr_matrix, method="number", type="upper")
#numeric-categorical independence test ( non parametric ANOVA)
#Gender is dependent on Academic Pressure, CGPA , Study.Satisfaction
kruskal.test(Age~Gender,data = data)
kruskal.test(data$Academic.Pressure~Gender,data=data)#
kruskal.test(data$CGPA~Gender,data=data)#
kruskal.test(data$Study.Satisfaction~Gender,data = data)#
kruskal.test(data$Work.Study.Hours~Gender,data=data)
kruskal.test(data$Financial.Stress~Gender,data=data)
#Sleep Duration is dependent on Academic Pressure ,CGPA , Work Study Hours , Financial Stress
kruskal.test(Age~data$Sleep.Duration,data=data)
kruskal.test(data$Academic.Pressure~data$Sleep.Duration,data=data)#
kruskal.test(data$CGPA~data$Sleep.Duration,data = data)#
kruskal.test(data$Study.Satisfaction~data$Sleep.Duration,data = data)
kruskal.test(data$Work.Study.Hours~data$Sleep.Duration,data=data)#
kruskal.test(data$Financial.Stress~data$Sleep.Duration,data=data)#
#Dietary Habits is dependent on Age , Academic Pressure , Study Satisfaction  ,Work Study Hours , Financial Stress 
kruskal.test(Age~data$Dietary.Habits,data=data)#
kruskal.test(data$Academic.Pressure~data$Dietary.Habits,data=data)#
kruskal.test(data$CGPA~data$Dietary.Habits,data=data)
kruskal.test(data$Study.Satisfaction~data$Dietary.Habits,data=data)#
kruskal.test(data$Work.Study.Hours~data$Dietary.Habits,data=data)#
kruskal.test(data$Financial.Stress~data$Dietary.Habits,data=data)#
#Degree is dependent on All
kruskal.test(Age~data$Degree,data=data)#
kruskal.test(data$Academic.Pressure~data$Degree,data=data)#
kruskal.test(data$CGPA~data$Degree,data=data)#
kruskal.test(data$Study.Satisfaction~data$Degree,data=data)#
kruskal.test(data$Work.Study.Hours~data$Degree,data=data)#
kruskal.test(data$Financial.Stress~data$Degree,data=data)#
#Suicidal Thoughts dependent on Age , Academic Pressure , Study Satisfaction  ,Work Study Hours , Financial Stress 
kruskal.test(Age~data$Have.you.ever.had.suicidal.thoughts..,data=data)#
kruskal.test(data$Academic.Pressure~data$Have.you.ever.had.suicidal.thoughts..,data=data)#
kruskal.test(data$CGPA~data$Have.you.ever.had.suicidal.thoughts..,data=data)
kruskal.test(data$Study.Satisfaction~data$Have.you.ever.had.suicidal.thoughts..,data=data)#
kruskal.test(data$Work.Study.Hours~data$Have.you.ever.had.suicidal.thoughts..,data=data)#
kruskal.test(data$Financial.Stress~data$Have.you.ever.had.suicidal.thoughts..,data=data)#
#Family Mental History dependent on Academic Pressure, CGPA, Study Satisfaction,  Work Study Hours
kruskal.test(Age~data$Family.History.of.Mental.Illness,data=data)
kruskal.test(data$Academic.Pressure~data$Family.History.of.Mental.Illness,data=data)#
kruskal.test(data$CGPA~data$Family.History.of.Mental.Illness,data=data)#
kruskal.test(data$Study.Satisfaction~data$Family.History.of.Mental.Illness,data=data)#
kruskal.test(data$Work.Study.Hours~data$Family.History.of.Mental.Illness,data=data)#
kruskal.test(data$Financial.Stress~data$Family.History.of.Mental.Illness,data=data)


