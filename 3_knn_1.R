library(dplyr)
library(tibble)
library(ggplot2)
library(tidyr)
library(class)

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


#  To initiate knn the follwong action shoul be taken:
#  Data inspection and normalisation
#  Splitting data into training and testing datasets
#  Fitting the model to training data
#  Evaluate perfomance on test data
#  Model tuning , specifically, for knn, determining optimal values of k
# optimal normalisation procedure

# knn is extremely powerfull when applied
# to homogenous data, speficically,
# it is suited very well to numerical features

# General routone for knn is the following:
## ?

#  Now, how to measure similary of instances or records
#  Traditionally Euclidian distance us applied

# assume 2 dimentional space with 2 points

sample_1<-data.frame(  feature_BMI=c( runif(1,18.5,24.9),
                                      runif(1, 25.0,29.9)),
                       class=c( "Healthy weight","Overweight" ),
                       row.names=c("inst_1  ","inst_2  ")   )

View(sample_1)


ggplot(data=sample_1,aes(y= feature_BMI ,x=class))+
  geom_point(col="red",cex=5)+
  geom_label(aes(label=round(feature_BMI,2)  ),nudge_y = 0.5 )


# Now assume , we need to decide , to which instance 
# we should assign new point , should it belong to 
# Forst or second class

set.seed(123)
new_point<-runif(1,18.5,29.9)

ggplot(data=sample_1,aes(y= feature_BMI ,x=class))+
  geom_point(aes(col=class),cex=5)+
  geom_label(aes(label=round(feature_BMI,2)  ),nudge_y = 0.1,nudge_x=0.2 )+
  geom_point(col="red")+geom_point(aes(y=new_point),col="blue",cex=6) +
  geom_segment(x="Healthy weight",y=new_point,xend = "Healthy weight", yend = sample_1$feature_BMI[1],
               color="green")+
  geom_segment(x="Overweight",y=new_point,xend = "Overweight", yend = sample_1$feature_BMI[2],
               color="green")+
  geom_label(aes(label=round(new_point,2),x=class,y=new_point  ),nudge_y = 0.1,nudge_x=0.2 )


# Lets estimate Euclidian Distance between 2 poins, we have only 1 instance
# per feature , threfore we would check knn where k in one 

dist_1<- sqrt( (new_point-sample_1$feature_BMI[1])^2 )
dist_2<- sqrt( (new_point-sample_1$feature_BMI[2])^2 )

sample_1["Eu_Dist"]<-c(dist_1 ,dist_2 )

sample_1<-sample_1 %>% mutate(Eu_Dist=sqrt( (new_point-feature_BMI)^2 ))

# now lets add new record

sample_1<-sample_1 %>% dplyr::bind_rows( tibble(feature_BMI=new_point,class=NA,Eu_Dist=NA) )
sample_1$class[3]<-"Healthy weight"
sample_1$class[3]<-sample_1$class[ which.min(sample_1$Eu_Dist) ]

class::knn( train=sample_1[c(1,2),1],
            test=new_point,
            cl=sample_1[c(1,2),2],
            k=1, prob=TRUE )


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
                 x="Healthy weight",y=new_point  ),nudge_y = 2,nudge_x=0 )



#######################  PART_2

sample_3<-data.frame(  feature_BMI=c( runif(3,18.5,24.9),
                                      runif(3, 25.0,29.9)),
                       class=c( rep("Healthy weight",3),rep("Overweight",3) ) )

ggplot(data=sample_3,aes(y= feature_BMI ,x=class))+
  geom_point(col="red",cex=6)+
  geom_label(aes(label=round(feature_BMI,2)  ),nudge_y = 0.1,nudge_x=0.2 )+
  geom_point(col="red")+geom_point(aes(y=new_point),col="blue",cex=5) +
  geom_label(aes(label=round(new_point,2),x=class,y=new_point  ),nudge_y = 0.1,nudge_x=0.2,color="blue" )


sample_3<-sample_3 %>% mutate(Eu_Dist=sqrt( (new_point-feature_BMI)^2 ))


ggplot(data=sample_3,aes(y= feature_BMI ,x=class))+
  geom_point(col="red",cex=6)+
  geom_label(aes(label=round(feature_BMI,2)  ),nudge_y = 0.1,nudge_x=0.2 )+
  geom_point(col="red")+geom_point(aes(y=new_point),col="blue",cex=5) +
  geom_label(aes(label=round(new_point,2),x=class,y=new_point  ),nudge_y = 0.1,nudge_x=0.2,color="blue" )+
  geom_label(aes(label=paste( "sqrt","(","(",round(new_point,2),"-",round(feature_BMI[1],2) ,")","^2",")","\n",
                              "sqrt","(","(",round(new_point,2),"-",round(feature_BMI[1],2) ,")","^2",")","\n",
                              "sqrt","(","(",round(new_point,2),"-",round(feature_BMI[1],2) ,")","^2",")"),
                 x="Healthy weight",y=new_point  ),nudge_y = 2,nudge_x=0 )


# Assume that k equals 3 , threrefore we need to choode free nearest neighbours

sample_3 <-sample_3 %>% arrange(Eu_Dist) %>% slice(1:3)

table(sample_3$class)
prop.table( table(sample_3$class) )

class::knn( train=sample_3[,1],test=new_point,cl=sample_3[,2],k=3, prob=TRUE )




############ PART 3

sample_4<-iris
View(sample_4)


# in this example there are 4 features which 
# are characeristics of 3 different plants, 
# out target is to find class[Species] of 
# plant fiven its 4 featues


# lets draw single row we would like to predict

set.seed(321)
loc<-sample(1:nrow(sample_4),3 ) # we randomly generate 3 indices
test_row_1<-sample_4[ loc[1]   , -5  ]
test_row_cl<-sample_4[ loc[1]   , 5  ]

# what classes are available 
sample_4$Species %>% unique()

# in previous example we had only 1 feature 
# here are 4 features which all should be part
# of equation 

# First off all wee ned to draw 4 graphs, 
# 1 graph per feature where each graph contains
# 1 point from test set and 2 form training set

train_row_2<-sample_4[loc[c(2,3)],  ]

# 
train_row_2
test_row_1

# here we have 2 instances from taining set and 
# we need to decide which is closer in terms of 
# euclidian distance to test_row_1

# we need to bind data into tibble to produce meaningfull plot
# this transformation is srequired 
# to apply ggplot with facets
gg_train<-train_row_2 %>% pivot_longer(Sepal.Length:Petal.Width ,
                                       names_to="Feature") %>%
  rename("Class"="Species")  

gg_test<-test_row_1 %>% pivot_longer(Sepal.Length:Petal.Width ,
                                     names_to="Feature") %>%
  mutate(Class="Unknown") %>% select(Class,Feature,value)

gg_in<-bind_rows(gg_train,gg_test)

ggplot(data=gg_in,aes(y=Class,x=value))+ 
  geom_point(aes(col= Class),cex=5)+
  geom_text(aes(label=round(value,2),color=Class ),cex=4,nudge_y = 0.2 ) + 
  facet_grid(~Feature,  space="free") +
  theme_bw()


train_row_2[,-5]-test_row_1 
train_row_2[,-5]-test_row_1[ c(1,1),] 
# this is how to extract values element wise

(train_row_2[,-5]-test_row_1[ c(1,1), ])^2

Eu_Dist=rowSums( (train_row_2[,-5]-test_row_1[ c(1,1),-5 ])^2 ) %>% sqrt()

train_row_2<- train_row_2 %>% 
  mutate(Eu_Dist=Eu_Dist) %>%
  arrange(Eu_Dist)

class::knn( train=train_row_2[,c(-5,-6)],
            test=test_row_1[ ,-5 ],k=2,
            cl=train_row_2[,5],
            prob=TRUE)






##### Lets apply knn to each onbservation 


loc<-sample(1:nrow(sample_4),20 )
test_set<-sample_4[ loc   , -5 ]
test_set_actual<-sample_4[ loc   , 5 ]
train_set<-sample_4[ -loc   , ]


m1_knn<-knn( train=train_set[,-5],
             test=test_set,
             cl=train_set[,5], k=3,
             prob=TRUE)

sum(m1_knn==test_set_actual)/ length(test_set_actual)

##  now lest write our own fucntion to come up with solution 


n_test<-nrow(test_set)
n_train<-nrow(train_set)
out<-rep("",n_test)

for ( i in 1:n_test) {
  
  Eu_Dist=rowSums( (train_set[,-5] - test_set[i,][c(rep(1,n_train)), ] ) ^2 ) %>% sqrt()
  train_buf<-train_set %>% mutate(Eu_Dist=Eu_Dist) %>% arrange(Eu_Dist) %>%
    slice(1:3)
  class_buf<-names( which.max(table(train_buf$Species)))
  out[i]<-class_buf
  
} 

sum(out==test_set_actual)/ n_test

# identical 


# now lets multiply each column by random number
loc<-sample(1:nrow(sample_4),20 )
sample_5<-iris
# sample_5[,c(1:4)]<-apply(sample_5[,c(1:4)],MARGIN=2, 
#                        function(x) return( x*runif(1,10,10000))  )
# 
# sample_5[,c(1:4)]/iris[,c(1:4)] 

test_set<-sample_5[ loc   , -5 ]
test_set_actual<-sample_5[ loc   , 5 ]
train_set<-sample_5[ -loc   , ]
lapply(train_set[,1:4],range)

m1_knn<-knn( train=train_set[,-5],
             test=test_set,
             cl=train_set[,5], k=3,
             prob=TRUE)

sum(m1_knn==test_set_actual)/ n_test


normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}      

train_set[,-5]<-apply(train_set[,-5], MARGIN=2 ,normalize)
test_set<-apply(test_set, MARGIN=2 ,normalize)
lapply(train_set[,1:4],range)


m1_knn<-knn( train=train_set[,-5],
             test=test_set,
             cl=train_set[,5], k=3,
             prob=TRUE)

sum(m1_knn==test_set_actual)/ n_test




