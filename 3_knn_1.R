# we would require 5 libraries to explore and 
# learn mechanics of k nearest neighbours
library(dplyr) # data wrangling
library(tidyr) # data wrangling
library(tibble) # data wrangling
library(ggplot2) # visuals
library(class)  # model building

# n<-1000
# sample_1<-tibble( x1=rnorm(n,10,2),
#                   x2=runif(n, min = 100, max = 200) , 
#                   x3=rexp(n,0.1) , 
#                   x4=sample(0:1,n,replace = TRUE) , 
#                   x5=rchisq(n, 5, ncp = 50) ,
#                   x6=log( rnorm(n,10,2) ) )
# 
# glimpse(sample_1)

#######################  PART_1
# knn is example of supervised classification technique
# supervised means than we know all classes which corresponds
# to specific records or instances of feature or variables 

# classification means that we would model output 
# as variable of  character or factor type

#  To initiate knn the follwong action shoul be taken:
#  Data inspection 
#  Splitting data into training and testing datasets
#  Fitting the model to training data
#  Evaluate perfomance on test data
#  Model tuning , specifically, for knn, determining optimal values of k


# knn is extremely powerfull when applied
# to homogenous data, speficically,
# it is suited very well to numerical features

# Goal of Knn is to 
# Estimate similarity between records with degined classes and 
# abd records without defined class, using arithmetic operations

#  Now, how to measure similary of instances or records
#  Traditionally Euclidian distance is applied

# assume 2 dimentional space with 2 points

sample_1<-data.frame(  feature_BMI=c( runif(1,18.5,24.9),
                                      runif(1, 25.0,29.9)),
                       class=c( "Healthy weight","Overweight" ),
                       row.names=c("inst_1  ","inst_2  ")   )
#  code sampel is available at github ,fill free to replicate it
# we can use View to explicitly check all records
View(sample_1)

# Now lets visualise situation we are dealing with 
ggplot2::ggplot(data=sample_1,aes(y= feature_BMI ,x=class))+
  geom_point(col="red",cex=5)+
  geom_label(aes(label=round(feature_BMI,2)  ),nudge_y = 0.5 )

# we have 2 values for signle feature,
# each value corresponds to specific class
# Now assume , we need to decide , to which class 
# we should assign new point , should it belong to 
# Forst or second classm Healhy or overweight

set.seed(123)
new_point<-runif(1,18.5,29.9) # generate random point

# lets take advantage of ggplot2 again
ggplot2::ggplot(data=sample_1,aes(y= feature_BMI ,x=class))+
  geom_point(aes(col=class),cex=5)+
  geom_label(aes(label=round(feature_BMI,2)  ),nudge_y = 0.1,nudge_x=0.2 )+
  geom_point(col="red")+geom_point(aes(y=new_point),col="blue",cex=6) +
  geom_segment(x="Healthy weight",y=new_point,xend = "Healthy weight", yend = sample_1$feature_BMI[1],
               color="green")+
  geom_segment(x="Overweight",y=new_point,xend = "Overweight", yend = sample_1$feature_BMI[2],
               color="green")+
  geom_label(aes(label=round(new_point,2),x=class,y=new_point  ),nudge_y = 0.1,nudge_x=0.2 )

# ble point can potentially belong to class 1 or 2
# we need to measure distance between red and blue point, 
# green and blue point

# Lets estimate Euclidian Distance between 2 poins, we have only 1 instance
# per feature , threfore we would check knn where k in one 

dist_1<- sqrt( (new_point-sample_1$feature_BMI[1])^2 )
dist_2<- sqrt( (new_point-sample_1$feature_BMI[2])^2 )

#lets add new column to sample_1
sample_1["Eu_Dist"]<-c(dist_1 ,dist_2 )

#alternativeluy, we can use mutate fucntion
sample_1<-sample_1 %>% dplyr::mutate(Eu_Dist=sqrt( (new_point-feature_BMI)^2 ))

# now lets add new record with class we dont know

sample_1<-sample_1 %>% dplyr::bind_rows( tibble(feature_BMI=new_point,class=NA,Eu_Dist=NA) )
row.names(sample_1)[3]<-3

# Value for Overweight is smaller, it is closer, therefore
# blue dot should belong to Overweignt class


sample_1$class[3]<-"Healthy weight" # assign poing manually
sample_1$class[3]<-sample_1$class[ which.min(sample_1$Eu_Dist) ] # use which.min, ignores NA


# how to implement the process with r fucntionality
class::knn( train=sample_1[c(1,2),1], # we define training set, and esclude column with classes 
            test=new_point, # define value ww need to classify
            cl=sample_1[c(1,2),2], # indicate classes which coreslond to known record
            k=1 ) # how many neighbours to consider

# to sum up , lets proceed to ggplot 2
ggplot(data=sample_1,aes(y= feature_BMI ,x=class))+
  geom_point(col="red",cex=4)+
  geom_label(aes(label=round(feature_BMI,2)  ),nudge_y = 0.1,nudge_x=0.2 )+
  geom_point(col="red")+geom_point(aes(y=new_point),col="blue",cex=5) +
  geom_segment(x="Healthy weight",y=new_point,xend = "Healthy weight", yend = sample_1$feature_BMI[1],
               color="green")+
  geom_segment(x="Overweight",y=new_point,xend = "Overweight", yend = sample_1$feature_BMI[2],
               color="green")+
  geom_label(aes(label=round(new_point,2),x=class,y=new_point  ),nudge_y = 0.1,nudge_x=0.2 )+
  geom_label(aes(label=paste( "sqrt","(","(",round(new_point,2),"-",
                              round(feature_BMI[1],2) ,")","^2",")"),
                 x="Healthy weight",y=new_point  ),nudge_y = 2,nudge_x=0 )+
  geom_label(aes(label=paste( "sqrt","(","(",round(new_point,2),"-",
                              round(feature_BMI[2],2) ,")","^2",")"),
                 x="Overweight",y=new_point  ),nudge_y = 2,nudge_x=0 )


#######################  PART_2

# Greate jon , now its time to check example with 21 feature , but with 
#multiple records for each class
sample_3<-data.frame(  feature_BMI=c( runif(3,18.5,24.9),
                                      runif(3, 25.0,29.9)),
                       class=c( rep("Healthy weight",3),rep("Overweight",3) ) )

# We inspect Visuals
ggplot(data=sample_3,aes(y= feature_BMI ,x=class))+
  geom_point(col="red",cex=6)+
  geom_label(aes(label=round(feature_BMI,2)  ),nudge_y = 0.1,nudge_x=0.2 )+
  geom_point(col="red")+geom_point(aes(y=new_point),col="blue",cex=5) +
  geom_label(aes(label=round(new_point,2),x=class,y=new_point  ),nudge_y = 0.1,nudge_x=0.2,color="blue" )
# Again , now we need to use 6 red poind in total to decide 
# for which class blue point belongs, therefore, here we estiate
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
                 x="Healthy weight",y=new_point  ),nudge_y = 2,nudge_x=0 )
# same calculation are aplied to column Overweignt

# Assume that k equals 3 , threrefore we need to choode free nearest neighbours,
# or 3 lowers values for distance

sample_3 <-sample_3 %>% dplyr::arrange(Eu_Dist) %>% dplyr::slice(1:3)

table(sample_3$class) # now we use table to count uniqu classes
prop.table( table(sample_3$class) ) # prop.table to wrup table is used to check share of class
                                    # among oter classes, we divide count of overweight records by total
                                    # number of records


# How should hte process be implemented with R fucntionality
class::knn( train=sample_3[,1],
            test=new_point,
            cl=sample_3[,2],k=3, 
            prob=TRUE )

# probality shows share of most commom class, this is the output of
# prop.table , proportion table 


        ############ PART 3

# Knn is great classifying tool gof data with homogenous 
# records, for example in iris dataset all features are
#of numeric type
sample_4<-iris
View(sample_4)

# Now , lets check how exactly features are dissimilart in 
# ters of central tendency and spread
summary(sample_4[,-5])

#judjing from summary, we can say that only Petal.Width 
# is notably smaller that other features, but overall
# we can not conclude that particula feature
# is extremely far from other 

cat(   "sqrt(  (1000-5.0)^2+(2.7-2.0)^2+(3.9-3.5)^2     )"      )
# this is equation fo EU distance for single 
#record with 3 features, first valun in brackest is x
# We are conserned with feature range because 
# EU distance is sensetive to outlies 
# and , if lets say x1,1000, is significantluy greater
# than other features, 
#it would increase Eu distance for partcicual record
# and would lower the changes of 
# class, which correspond to the respecive record,
# to become meaningfull classifier.
# the resoective situations may require use or
# data normalisteion but so far Iris data set in our example
# does not reuqire specia transofrmation

# goind bacj to sample_4
tibble::glimpse(sample_4)
# in this example there are 4 features which 
# are characeristics of 3 different plants, 
# out target is to find class[Species] of 
# plant fiven its 4 featues


# lets sample single row we would like to predict
set.seed(321)
loc<-sample(1:nrow(sample_4),3 ) # we randomly generate 3 indices
test_row_1<-sample_4[ loc[1]   , -5  ] # here we store 1 record we need to classify
test_row_cl<-sample_4[ loc[1]   , 5  ] # here we store actual classes for test row

# what classes are available 
sample_4$Species %>% unique()
# we need 3 recordsm 1 per each class, therefore
#our train set woudl contain samples for each class

# we would require to grpup data and take 1 sample from each froup
train_row_3<-sample_4[-loc,] %>% group_by(Species) %>%
                    slice(1)

# First off all wee ned to draw 4 graphs, 
# 1 graph per feature where each graph contains
# 1 point from test set and 2 form training set

train_row_3
test_row_1

# here we have 3 instances from taining set and 
# we need to decide which is closer in terms of 
# euclidian distance to test_row_1

# we need to bind data into tibble to produce meaningfull plot.
# This transformation is srequired 
# to apply ggplot with facets
gg_train<-train_row_2 %>% tidyr::pivot_longer(Sepal.Length:Petal.Width ,
                                       names_to="Feature") %>%
  rename("Class"="Species")  

gg_test<-test_row_1 %>% tidyr::pivot_longer(Sepal.Length:Petal.Width ,
                                     names_to="Feature") %>%
  mutate(Class="Unknown") %>% dplyr::select(Class,Feature,value)

gg_in<-dplyr::bind_rows(gg_train,gg_test)

## here my goal to create long table  where each column is
# separate variale, this is only done to use fucntionality of ggplot2

ggplot2::ggplot(data=gg_in,aes(y=Class,x=value))+ 
  geom_point(aes(col= Class),cex=5)+
  geom_text(aes(label=round(value,2),color=Class ),cex=4,nudge_y = 0.2 ) + 
  facet_grid(~Feature,  space="free") +
  theme_bw()

#  red poins is  present per each feature,
#  oterh dots are features from 3 instances  
#  of train set

# 3 records and 1 test record means 3 equations, 4 features per record,
# mean 4 operand in equation
# here is Eu for single row

cat( "sqrt(  (4.8-6)^2+(1.4-2.5)^2+(6.8-6.3)^2+(2.8-3.3)^2    )"      )

#  Here we need to substract train set form test
train_row_3[,-5]-test_row_1 # both have different rows , we need to triope
                            # sinfle record in test set
train_row_3[,-5]-test_row_1[ c(1,1,1),]   # here first rows is repeated 3 times
# this is how to extract values element wise when dealing with 
# data frames

(train_row_3[,-5]-test_row_1[ c(1,1,1),] )^2 # nest spet is to  take square

# the whole eaution with rowSums
Euc_Dist=rowSums( (train_row_2[,-5]-test_row_1[ c(1,1,1), ])^2 ) %>% sqrt()

# now we eould create additional column and arrange data, then pull 1 nearest
# neighbours , as number of classes equals number of records
train_row_3<- train_row_3 %>% ungroup() %>% mutate(Euc_Dist=Euc_Dist) %>%
              arrange(Eu_Dist) %>% slice(1)
test_row_cl
# we can conclude that 

class::knn( train=train_row_3[,c(-5,-6)],
            test=test_row_1,k=1,
            cl=train_row_3[,5],
            prob=TRUE)


##### Greate Jon Not we splot shole irus into 80 20 split,
# 80 for trainig set and 20 for test, therefore we need to classify 20
# records

set.seed(253)
loc<-sample(1:nrow(sample_4),20 ) # this are indices for test set
test_set<-sample_4[ loc   , -5 ]
test_set_cl<-sample_4[ loc   , 5 ]
train_set<-sample_4[ -loc   , ]


# How would knn fucntion perform
m1_knn<-knn( train=train_set[,-5],
             test=test_set,
             cl=train_set[,5], k=3,
             prob=TRUE)

# now we stimate accuracy, how many values in 
#m1_knn are equal to values in test set classes
sum(m1_knn==test_set_cl)/ length(test_set_cl)

##  now lest write our own to soliduty understandng 
## of knn 

n_test<-nrow(test_set)
n_train<-nrow(train_set)
out<-rep("",n_test) # out vecotr would store our result of classification 
                    # of train values

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



## finaly , lets combine knn fucntion and loop to 
## chack if our accuracy can be improved

m1_knn<-knn( train=train_set[,-5],
             test=test_set,
             cl=train_set[,5], k=3,
             prob=TRUE)

# now we stimate accuracy, how many values in 
#m1_knn are equal to values in test set classes
sum(m1_knn==test_set_cl)/ length(test_set_cl)


# Again, lets proceed with loop

for (i in (1:10) )  {
  m1_knn<-knn( train=train_set[,-5],
               test=test_set,
               cl=train_set[,5], k=i,
               prob=TRUE)
  acc<-sum(m1_knn==test_set_cl)/ length(test_set_cl)
     print( paste( i,": Accuracy of ",  acc ) )
}

# We can conllude, that accutacy of 1 can be achievedm 
# meaning that we managed to correcly classify all records





# # identical 
# 
# 
# # now lets multiply each column by random number
# loc<-sample(1:nrow(sample_4),20 )
# sample_5<-iris
# # sample_5[,c(1:4)]<-apply(sample_5[,c(1:4)],MARGIN=2, 
# #                        function(x) return( x*runif(1,10,10000))  )
# # 
# # sample_5[,c(1:4)]/iris[,c(1:4)] 
# 
# test_set<-sample_5[ loc   , -5 ]
# test_set_cl<-sample_5[ loc   , 5 ]
# train_set<-sample_5[ -loc   , ]
# lapply(train_set[,1:4],range)
# 
# m1_knn<-knn( train=train_set[,-5],
#              test=test_set,
#              cl=train_set[,5], k=3,
#              prob=TRUE)
# 
# sum(m1_knn==test_set_cl)/ n_test
# 
# 
# normalize<-function(x){
#   return((x-min(x))/(max(x)-min(x)))
# }      
# 
# train_set[,-5]<-apply(train_set[,-5], MARGIN=2 ,normalize)
# test_set<-apply(test_set, MARGIN=2 ,normalize)
# lapply(train_set[,1:4],range)
# 
# 
# m1_knn<-knn( train=train_set[,-5],
#              test=test_set,
#              cl=train_set[,5], k=3,
#              prob=TRUE)
# 
# sum(m1_knn==test_set_cl)/ n_test
# 

