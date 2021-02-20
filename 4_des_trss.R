#desision trees are offer greater interpretability
# and opportunity to trace and verify split rules

#decisions rules can be converietnly fitted
# toigether to tree structure with bibary splity of 
# decisins
#https://www.quora.com/What-is-the-TDIDT-algorithm
#

library(tibble)
library(tidyr)
library(readr)
library(dplyr)
library(rpart)
library(rpart.plot)


path_golf<-r"(C:\Users\UACecetoAl\Desktop\R_Input\2_ML_Course\1_sample_golf.csv)"
d1<-readr::read_csv(path_golf,col_names = TRUE)
View(d1)


d1<-d1 %>% mutate( Play=ifelse(grepl("^play",Play) ,"pl", "dp" ) )

# gere 4 features are available and we nned to 
# predict outcomu, play or not to play
# in other owrds what would be decision diven ,
# 4 features

# $threrefore qe need to buoid decision tree
# to determine outcome, or classify [lay oor not or not to plat
# given weather conditions

features are referred as variables or attributes, 
which couk dbe catetegorical variable

Each row form instance

#trees are created given split on attributes
splittin continues until branch is labelled with just one
classification, no split is possible

decision tree can be viewed as not merely 
equivalent to the original
training set but as a generalisation 
of it which can be used to predict the
classification of other instances.

m1<-rpart::rpart(Play~.,data=d1,method="class",control=rpart.control(minsplit = 5)  )
rpart.plot(m1)

# instance is a set
# of valus of attributes

                                   
d1 %>% arrange(Outlook,Wind,Humidity,Temperature,)
# classification is easy if
#This condition is that no two instances
# with identical attribute values 
#have different classifications.
# This is adewucy condition

# If adewucy cidtions is satisfied any methods
# of choosing attributes wil lproduce
# decision tree

# The quality of the strategy used to select the attribute to split
# on at each stage is clearly of vital importance. This is the topic to which we
# now turn.
# 5.3

# One commonly used method is to
# select the attribute that minimises the value of entropy
# thus maximising information gain

# K is number of classes , pi is the q of class i
# divided by the total number of instances
entropy measures biits of information 
 
cat(" -sum(pi*log2(pi))  ")
# this is expected numebr of biits for system

s1<-c("a","b","c","d","e","f")
table(s1)
pia<-prop.table(table(s1))
-sum(pia*log2(pia))

s1<-c("a","a","a","a","a","a")
table(s1)
pia<-prop.table(table(s1))
-sum(pia*log2(pia))

log2(6) # if each class has unique value of
# apperance
s1<-c(rep("1",4),rep("2",5) , rep("3",15) )
table(s1)
pia<-prop.table(table(s1))
-sum(pia*log2(pia))

# answer is 1.32 bith for system with 
# single feature
















