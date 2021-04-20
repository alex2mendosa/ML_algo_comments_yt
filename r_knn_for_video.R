

library(dplyr)   # data wrangling
library(tidyr)   # data wrangling
library(tibble)  # data wrangling
library(ggplot2) # visuals
library(class)   # model building




#######################  PART_1

# data frame with 2 records
sample_1<-data.frame(  feature_BMI=c( runif(1,18.5,24.9),
                                      runif(1, 25.0,29.9)),
                       class=c( "Healthy weight","Overweight" ),
                       row.names=c("inst_1  ","inst_2  ")   )
View(sample_1)

ggplot2::ggplot(data=sample_1,aes(y= feature_BMI ,x=class))+
  geom_point(col="red",cex=10)+
  geom_label(aes(label=round(feature_BMI,2)  ),nudge_y = 0.5 )+
  theme(axis.text=element_text(size=15) )

#  generate random point without known class
set.seed(123)
new_point<-runif(1,18.5,29.9) %>% round(2)

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

# Euclidean distance
dist_1<- sqrt( (new_point-sample_1$feature_BMI[1])^2 )
dist_2<- sqrt( (new_point-sample_1$feature_BMI[2])^2 )

# Absolute difference
abs( new_point-sample_1$feature_BMI[1] )
abs( new_point-sample_1$feature_BMI[2] )

# Add column with Euclidean distance
sample_1<-sample_1 %>% 
         dplyr::mutate(Eu_Dist=sqrt( (new_point-feature_BMI)^2 ))

sample_1<-sample_1 %>%
  dplyr::bind_rows( tibble(feature_BMI=new_point,class=NA,Eu_Dist=NA) )
row.names(sample_1)[3]<-3

# Find index of smallest Euclidean distance
sample_1$class[3]<-sample_1$class[ which.min(sample_1$Eu_Dist) ] 


class::knn( train=sample_1[c(1,2),1], # define features of training set 
            test=new_point, # define value we need to classify
            cl=sample_1[c(1,2),2], # define classes of training set
            k=1 ) # define neighbours to consider


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





#########################  PART_2

set.seed(123)
sample_3<-data.frame(  feature_BMI=c( runif(3,18.5,24.9),
                                      runif(3, 25.0,29.9)),
                       class=c( rep("Healthy weight",3),rep("Overweight",3) ) )

new_point<-24.5

ggplot(data=sample_3,aes(y= feature_BMI ,x=class))+
  geom_point(col="red",cex=6)+
  geom_label(aes(label=round(feature_BMI,2)  ),nudge_y = 0.1,nudge_x=0.2 )+
  geom_point(col="red")+geom_point(aes(y=new_point),col="blue",cex=5) +
  geom_label(aes(label=round(new_point,2),x=class,y=new_point  ),nudge_y = 0.1,nudge_x=0.2,color="blue" )+
  theme(axis.text=element_text(size=15) )

sample_3<-sample_3 %>% dplyr::mutate(Eu_Dist=sqrt( (new_point-feature_BMI)^2 ))

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

sample_3 <-sample_3 %>% dplyr::arrange(Eu_Dist) %>% dplyr::slice(1:3)

table(sample_3$class) # distribution of each unique value 
prop.table( table(sample_3$class) ) 


# Solution with knn function
class::knn( train=sample_3[,1],
            test=new_point,
            cl=sample_3[,2],
            k=3, 
            prob=TRUE )




################## PART 3


sample_4<-iris
View(sample_4)

# summary() produces summary statistics for selected columns
summary(sample_4[,-5])
tibble::glimpse(sample_4)


# select row we want to predict with sample() function
set.seed(321)
loc<-sample(1:nrow(sample_4),1 ) # randomly generate index
test_row_1<-sample_4[ loc[1]   , -5  ] # store 1 record as test set
test_row_cl<-sample_4[ loc[1]   , 5  ] # store actual class for test record

# what classes are available 
sample_4$Species %>% unique()


# group data by Class and take 1 sample from each group
train_row_3<-sample_4[-loc,] %>% dplyr::group_by(Species) %>%
  slice(1)


train_row_3
test_row_1

# convert train_row_3 and test_row_1 to long format
# Transformation is requred 
# to apply ggplot with facets
gg_train<-train_row_3 %>% tidyr::pivot_longer(Sepal.Length:Petal.Width ,
                                              names_to="Feature") %>%
  rename("Class"="Species")  

gg_test<-test_row_1 %>% tidyr::pivot_longer(Sepal.Length:Petal.Width ,
                                            names_to="Feature") %>%
  mutate(Class="Unknown") %>% dplyr::select(Class,Feature,value)

gg_in<-dplyr::bind_rows(gg_train,gg_test)


ggplot2::ggplot(data=gg_in,aes(y=Class,x=value))+ 
  geom_point(aes(col= Class),cex=5)+
  geom_text(aes(label=round(value,2),color=Class ),cex=4,nudge_y = 0.2 ) + 
  facet_grid(~Feature,  space="free") +
  theme_bw(base_size = 15)

cat( "sqrt(  (4.8-6)^2+(1.4-2.5)^2+(6.8-6.3)^2+(2.8-3.3)^2    )"    )

#  substract train set form test
train_row_3[,-5]-test_row_1 # both have different rows

#  repeat first record of test set to match size of train set
train_row_3[,-5]-test_row_1[ c(1,1,1),]   #  first row is repeated 3 times
# this is how to extract values element wise when dealing with 

# equation with rowSums
Euc_Dist=rowSums( (train_row_3[,-5]-test_row_1[ c(1,1,1), ])^2 ) %>% sqrt()

train_row_3 %>% ungroup() %>% mutate(Euc_Dist=Euc_Dist) %>%
  arrange(Euc_Dist) %>% slice(1)
test_row_cl

class::knn( train=train_row_3[,-5],
            test=test_row_1,
            cl=dplyr::pull(train_row_3[,5]), # here I use pull because cl accepts vector as input
            k=1)                             


#####     Solution with 80,20 split,
#####     80% of records for trainig set and 20% for test, 
#####     therefore, we need to classify 20% of records

set.seed(253)
loc<-sample(1:nrow(iris),nrow(iris)*0.2 ) # indices for test set
test_set<-iris[ loc   , -5 ]  # features of test set
test_set_cl<-iris[ loc   , 5 ] # classes of test set
train_set<-iris[ -loc   , ] # train set


# How knn fucntion performs
m1_knn<-knn( train=train_set[,-5],
             test=test_set,
             cl=train_set[,5], k=3,
             prob=TRUE)

# Estimate accuracy, how many values in 
# m1_knn equal to values in test set classes
sum(m1_knn==test_set_cl)/ length(test_set_cl)


# User Defined Solution
n_test<-nrow(test_set)
n_train<-nrow(train_set)
out<-rep("",n_test) #  vector to store result of classification 


i<-1
for ( i in 1:n_test) { # we go through each record of test set
  # estimate Eu Distance between single row of test set and all rows of train set
  Eu_Dist=rowSums( (train_set[,-5] - test_set[i,][c(rep(1,n_train)), ] ) ^2 ) %>% sqrt()
  # we select 3 closest records
  train_buf<-train_set %>% mutate(Eu_Dist=Eu_Dist) %>% arrange(Eu_Dist) %>%
    slice(1:3)
  # class with the larger count
  class_buf<-names( which.max(table(train_buf$Species)))
  out[i]<-class_buf
} 

# check accuracy
sum(out==test_set_cl)/ n_test


# how accuracy changes as value of k changes
# we would try values of k from 1 to 10
for (i in (1:10) )  {
  m1_knn<-knn( train=train_set[,-5],
               test=test_set,
               cl=train_set[,5], k=i,
               prob=TRUE)
  acc<-sum(m1_knn==test_set_cl)/ length(test_set_cl)
  print( paste( i,": Accuracy of ",  acc ) )
}

