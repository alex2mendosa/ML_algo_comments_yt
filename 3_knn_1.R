# we would require 5 libraries to explore and 
# learn mechanics of k nearest neighbours

library(dplyr)  # data wrangling
library(tidyr)  # data wrangling
library(tibble) # data wrangling
library(ggplot2) # visuals
library(class)  # model building


#######################  PART_1
# knn is example of supervised classification technique,
# supervised means that all rows or instances  
# are labeled with specific class or outcome

#  To initiate knn the follwong action shoul be taken:
#  Data inspection 
#  Splitting data into training and test datasets
#  Fitting the model to training data
#  Evaluate perfomance on test data
#  Model tuning , specifically, for knn, to determine optimal values of k


# Goal of Knn is to 
# Estimate similarity between records with defined classes and 
# records without defined class

#  Now, how to measure similary of records?
#  We need to employ distance formula,
#  Euclidian distance is a common approach

# assume 2 dimentional space with 2 points

sample_1<-data.frame(  feature_BMI=c( runif(1,18.5,24.9),
                                      runif(1, 25.0,29.9)),
                       class=c( "Healthy weight","Overweight" ),
                       row.names=c("inst_1  ","inst_2  ")   )

#  code sampel is available at github ,fill free to replicate it.
# we can use View to explicitly check all records
View(sample_1)

# Greate job 
# Now lets visualise situation we are dealing with 
ggplot2::ggplot(data=sample_1,aes(y= feature_BMI ,x=class))+
  geom_point(col="red",cex=10)+
  geom_label(aes(label=round(feature_BMI,2)  ),nudge_y = 0.5 )+
  theme(axis.text=element_text(size=15) )

# we have 2 values for single feature,
# each value corresponds to specific class
# Now assume , we need to decide , to what class 
# we should assign new point , should it belong to 
# Forst or second class: Healhy or overweight

set.seed(123)
new_point<-runif(1,18.5,29.9) # generate random point

# lets take advantage of ggplot2 again
ggplot2::ggplot(data=sample_1,aes(y= feature_BMI ,x=class))+
  geom_point(aes(col=class),cex=10)+
  geom_label(aes(label=round(feature_BMI,2)  ),nudge_y = 0.1,nudge_x=0.2 )+
  geom_point(col="red")+geom_point(aes(y=new_point),col="blue",cex=11) +
  geom_segment(x="Healthy weight",y=new_point,xend = "Healthy weight", yend = sample_1$feature_BMI[1],
               color="green")+
  geom_segment(x="Overweight",y=new_point,xend = "Overweight", yend = sample_1$feature_BMI[2],
               color="green")+
  geom_label(aes(label=round(new_point,2),x=class,y=new_point  ),nudge_y = 0.1,nudge_x=0.2 )+
  theme(axis.text=element_text(size=15) )

# blue point can potentially belong to class 1 or 2
# we need to measure distance between red and blue point, 
# green and blue point

# Lets estimate Euclidian Distance between 2 poins
# if only 2 values are present in formula, we simply estimate absolute value of diference
dist_1<- sqrt( (new_point-sample_1$feature_BMI[1])^2 )
dist_2<- sqrt( (new_point-sample_1$feature_BMI[2])^2 )

#lets add new column to sample_1
sample_1<-sample_1 %>% dplyr::mutate(Eu_Dist=sqrt( (new_point-feature_BMI)^2 ))

# now add new record with class we dont know
sample_1<-sample_1 %>%
  dplyr::bind_rows( tibble(feature_BMI=new_point,class=NA,Eu_Dist=NA) )
row.names(sample_1)[3]<-3

# Value for "Healthy weight"??? is smaller, it is closer, therefore
# blue dot should belong to Healthy weight class

sample_1$class[3]<-sample_1$class[ which.min(sample_1$Eu_Dist) ] 
# use which.min, # ignores NA


# how to implement the process with buid in r fucntion
# we would call for knn fucntion from class package
class::knn( train=sample_1[c(1,2),1], # we define only training set, and exclude column with classes 
            test=new_point, # define value we need to classify
            cl=sample_1[c(1,2),2], # indicate classes which coreslond to known record
            k=1 ) # how many neighbours to consider

# to sum up , lets proceed to ggplot 2
ggplot(data=sample_1,aes(y= feature_BMI ,x=class))+
  geom_point(col="red",cex=8)+
  geom_label(aes(label=round(feature_BMI,2)  ),nudge_y = 0.1,nudge_x=0.2 )+
  geom_point(col="red")+geom_point(aes(y=new_point),col="blue",cex=9) +
  geom_segment(x="Healthy weight",y=new_point,xend = "Healthy weight", yend = sample_1$feature_BMI[1],
               color="green")+
  geom_segment(x="Overweight",y=new_point,xend = "Overweight", yend = sample_1$feature_BMI[2],
               color="green")+
  geom_label(aes(label=round(new_point,2),x=class,y=new_point  ),nudge_y = 0.1,nudge_x=0.2 )+
  geom_label(aes(label=paste( "sqrt","(","(",round(new_point,2),"-",
                              round(feature_BMI[1],2) ,")","^2",")"),
                 x="Healthy weight",y=new_point  ),nudge_y = 2,nudge_x=0,size=5 )+
  geom_label(aes(label=paste( "sqrt","(","(",round(new_point,2),"-",
                              round(feature_BMI[2],2) ,")","^2",")"),
                 x="Overweight",y=new_point  ),nudge_y = 2,nudge_x=0,size=5 )


#######################  PART_2

# Horoso , now its time to check example with 1 feature, 
# BMI , but with 
# multiple records for each class
set.seed(123)
sample_3<-data.frame(  feature_BMI=c( runif(3,18.5,24.9),
                                      runif(3, 25.0,29.9)),
                       class=c( rep("Healthy weight",3),rep("Overweight",3) ) )

new_point<-24.5
# We use Visuals
ggplot(data=sample_3,aes(y= feature_BMI ,x=class))+
  geom_point(col="red",cex=6)+
  geom_label(aes(label=round(feature_BMI,2)  ),nudge_y = 0.1,nudge_x=0.2 )+
  geom_point(col="red")+geom_point(aes(y=new_point),col="blue",cex=5) +
  geom_label(aes(label=round(new_point,2),x=class,y=new_point  ),nudge_y = 0.1,nudge_x=0.2,color="blue" )+
  theme(axis.text=element_text(size=15) )
# Again , now we need to use 6 red poins  to decide 
# to which class blue point belongs, therefore, here we estimate
# 6 values for Euclidian distacne

#mutate fucntion would be quite handy
sample_3<-sample_3 %>% dplyr::mutate(Eu_Dist=sqrt( (new_point-feature_BMI)^2 ))

# to solididy inderstanding , lets plot the equations for single class
ggplot2::ggplot(data=sample_3,aes(y= feature_BMI ,x=class))+
  geom_point(col="red",cex=6)+
  geom_label(aes(label=round(feature_BMI,2)  ),nudge_y = 0.1,nudge_x=0.2 )+
  geom_point(col="red")+geom_point(aes(y=new_point),col="blue",cex=5) +
  geom_label(aes(label=round(new_point,2),x=class,y=new_point  ),nudge_y = 0.1,nudge_x=0.2,color="blue" )+
  geom_label(aes(label=paste("sqrt","(","(",round(new_point,2),"-",round(feature_BMI[1],2) ,")","^2",")","\n",
                             "sqrt","(","(",round(new_point,2),"-",round(feature_BMI[2],2) ,")","^2",")","\n",
                             "sqrt","(","(",round(new_point,2),"-",round(feature_BMI[3],2) ,")","^2",")"),
                 x="Healthy weight",y=new_point  ),nudge_y = 5,nudge_x=0,size=5 )+
  theme(axis.text=element_text(size=15) )
# same calculation are aplied to column Overweignt

# Assume that k equals 3 , threrefore we need to choode
# free nearest neighbours,
# or 3 lowers values for distance

sample_3 <-sample_3 %>% dplyr::arrange(Eu_Dist) %>% dplyr::slice(1:3)

table(sample_3$class) # now we use table to count uniqu classes
prop.table( table(sample_3$class) ) # prop.table to wrup table is used 
# to check share of class
# among other classes, we divide count of Healthy weight records by total
# number of records

# here is the solution for knn fucntion
class::knn( train=sample_3[,1],
            test=new_point,
            cl=sample_3[,2],
            k=3, 
            prob=TRUE )
# probality shows share of class, this is identical
# to the output of
# prop.table , proportion table 

# Greate Job let us proceed

############ PART 3

# Knn is great classification tool for data with homogenous 
# records, for example in iris dataset all features are
#of numeric type
sample_4<-iris
View(sample_4)

# Now , lets check how exactly features are dissimilar in 
# terms  of central tendency and spread
summary(sample_4[,-5])

#judjing from summary, we can say that only Petal.Width 
# is notably smaller compare to other features, but overall
# we can not conclude that particula feature
# is extremely far from other 

# goind back to sample_4
tibble::glimpse(sample_4)
# in this example there are 4 features which 
# are characeristics of 3 different plants, 
# out target is to find class[Species] of 
# plant given its 4 featues


# lets sample single row we would like to predict
set.seed(321)
loc<-sample(1:nrow(sample_4),1 ) # we randomly generate index
test_row_1<-sample_4[ loc[1]   , -5  ] # here we store 1 record we need to classify
test_row_cl<-sample_4[ loc[1]   , 5  ] # here we store actual classes for test record

# what classes are available 
sample_4$Species %>% unique()
# we have 3 unique classes, for this particular example
# our traininig set should contain 1 record for each class

# we would require to group data and take 1 sample from each group
train_row_3<-sample_4[-loc,] %>% dplyr::group_by(Species) %>%
  slice(1)
# here I extract first record from each group


# First off all wee ned to draw 4 graphs, 
# 1 graph per feature where each graph contains
# 1 point from test set and 3 form training set

train_row_3
test_row_1

# here we have 3 instances from training set and 
# we need to decide which is closer in terms of 
# euclidian distance to test_row_1

# we need to convert train_row_3 and test_row_1 to long format
# This transformation is requred 
# to apply ggplot with facets
gg_train<-train_row_3 %>% tidyr::pivot_longer(Sepal.Length:Petal.Width ,
                                              names_to="Feature") %>%
  rename("Class"="Species")  
# here we transform train set from wide format, with 5 columns
# to long format with 3 column

gg_test<-test_row_1 %>% tidyr::pivot_longer(Sepal.Length:Petal.Width ,
                                            names_to="Feature") %>%
  mutate(Class="Unknown") %>% dplyr::select(Class,Feature,value)

gg_in<-dplyr::bind_rows(gg_train,gg_test)

## here our  goal to create long table  where each column is
# separate variale, this is only done to use fucntionality of ggplot2

ggplot2::ggplot(data=gg_in,aes(y=Class,x=value))+ 
  geom_point(aes(col= Class),cex=5)+
  geom_text(aes(label=round(value,2),color=Class ),cex=4,nudge_y = 0.2 ) + 
  facet_grid(~Feature,  space="free") +
  theme_bw(base_size = 15)

#  Unknown  Class  is present per each feature,
#  other  dots are features from 3 instances  
#  of trainini set with defined class

# 3 train records and 1 test record means 3 equations, 4 features per record,
# mean 4 operand in equation
# here is example of Eu for single row

cat( "sqrt(  (4.8-6)^2+(1.4-2.5)^2+(6.8-6.3)^2+(2.8-3.3)^2    )"    )

#  Here we need to substract train set form test
train_row_3[,-5]-test_row_1 # both have different rows , we need to triple
# first  record in test set
train_row_3[,-5]-test_row_1[ c(1,1,1),]   # here first row is repeated 3 times
# this is how to extract values element wise when dealing with 
# data frames of different rows numbers

# the whole eaution with rowSums
Euc_Dist=rowSums( (train_row_3[,-5]-test_row_1[ c(1,1,1), ])^2 ) %>% sqrt()

# now we would create additional column and arrange data, then pull 1 nearest
# neighbours , only 1 neigh because number of classes equals number of records
train_row_3 %>% ungroup() %>% mutate(Euc_Dist=Euc_Dist) %>%
  arrange(Euc_Dist) %>% slice(1)
test_row_cl

class::knn( train=train_row_3[,-5],
            test=test_row_1,
            cl=dplyr::pull(train_row_3[,5]), # here I use pull because cl accepts vector as input
            k=1)                             


##### Greate Job, Not we would employ whole 
# iris dataset , with  80,20 split,
# 80% of records for trainig set and 20$ for test, 
# therefore we need to classify 20%
# of records

set.seed(253)
loc<-sample(1:nrow(iris),nrow(iris)*0.2 ) # this are indices for test set
test_set<-iris[ loc   , -5 ]
test_set_cl<-iris[ loc   , 5 ]
train_set<-iris[ -loc   , ] # use use minus
#to select rows which are not part of test set


# How would knn fucntion perform
m1_knn<-knn( train=train_set[,-5],
             test=test_set,
             cl=train_set[,5], k=3,
             prob=TRUE)

# now we stimate accuracy, how many values in 
# m1_knn  equal to values in test set classes
sum(m1_knn==test_set_cl)/ length(test_set_cl)


##  now lest write our own solution
# to soliduty understandng 
## of knn 
n_test<-nrow(test_set)
n_train<-nrow(train_set)
out<-rep("",n_test) # out vecotr would store our result of classification 
# of test  values

# please dont be discourage by loop, it feets perfecty in 
# what we want to do

i<-1
for ( i in 1:n_test) {
  # lets check each operand
  Eu_Dist=rowSums( (train_set[,-5] - test_set[i,][c(rep(1,n_train)), ] ) ^2 ) %>% sqrt()
  train_buf<-train_set %>% mutate(Eu_Dist=Eu_Dist) %>% arrange(Eu_Dist) %>%
    slice(1:3)
  class_buf<-names( which.max(table(train_buf$Species)))
  out[i]<-class_buf
} 

sum(out==test_set_cl)/ n_test


# WE ESTIMATE EUCLIDIAN DISTANCE, WE adjust size of test set to size of
# training set, we use which.max and table to find most 
# common  clan wihtoin 3 rows

## finaly , lets combine knn fucntion and loop to 
## chack if our accuracy can be improved


# Again, lets proceed with loop
# we woudl ckech accuraccy for k range form 1 to 10
for (i in (1:10) )  {
  m1_knn<-knn( train=train_set[,-5],
               test=test_set,
               cl=train_set[,5], k=i,
               prob=TRUE)
  acc<-sum(m1_knn==test_set_cl)/ length(test_set_cl)
  print( paste( i,": Accuracy of ",  acc ) )
}


# Greate job everyone.
# Knn  is straighforward, easy-to-implement 
# supervised machine learning algorithm that can be
# used to solve  classification  challenhes.

# It works especially well  when features form 
#  homogenous numerical data , the point  of concern is to 
# choose oprimal K values to achieve higest accuracy


