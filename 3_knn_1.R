library(dplyr)
library(tibble)
library(ggplot2)
n<-1000
sample_1<-tibble( x1=rnorm(n,10,2),
                  x2=runif(n, min = 100, max = 200) , 
                  x3=rexp(n,0.1) , 
                  x4=sample(0:1,n,replace = TRUE) , 
                  x5=rchisq(n, 5, ncp = 50) ,
                  x6=log( rnorm(n,10,2) ) )

glimpse(sample_1)

#######################  PART_1
# knn is example of supervised , classification technique


#  To initiate knn the follwong action shoul be taken:
#  Data inspection and normalisation
#  Splitin data into training and testing datasets
#  Fitting the model to training data
#  Evaluate perfomance on test data
#  Model tuning , specifically, for knn, determining optimal values of k

# knn is extremely powerfull when applied
# to homogenous data, speficically,
# it is suited very well to numerical features

# General routone for knn is the following:
  ## ?
  
 #  Now how to measure similary of instances or records
 #  Traditionally euclidian distance us applied

 # assume 2 dimentional space with 2 poins

sample_2<-data.frame(  feature_BMI=c( runif(1,18.5,24.9),
                                      runif(1, 25.0,29.9)),
                       class=c( "Healthy weight","Overweight" ),
                       row.names=c("inst_1  ","inst_2  ")   )


ggplot(data=sample_2,aes(y= feature_BMI ,x=class))+
  geom_point(col="red",cex=3)+
  geom_label(aes(label=round(feature_BMI,2)  ),nudge_y = 0.5 )
  
  
# Now assume , we need to decide , to which instance 
# we should assign new point , should it belong to 
# Forst or second class

set.seed(3211)
new_point<-runif(1,18.5,29.9)

ggplot(data=sample_2,aes(y= feature_BMI ,x=class))+
  geom_point(col="red",cex=4)+
  geom_label(aes(label=round(feature_BMI,2)  ),nudge_y = 0.1,nudge_x=0.2 )+
  geom_point(col="red")+geom_point(aes(y=new_point),col="blue",cex=5) +
  geom_segment(x="Healthy weight",y=new_point,xend = "Healthy weight", yend = sample_2$feature_BMI[1],
                     color="green")+
  geom_segment(x="Overweight",y=new_point,xend = "Overweight", yend = sample_2$feature_BMI[2],
               color="green")




tibble::glimpse(sample_2)
sample_2

# lest estimate Euclidian Distance between 2 poins, we have only 1 instance
#per feature , threfore we would check knn where k in one 

dist_1<- sqrt( (new_point-sample_2$feature_BMI[1])^2 )
dist_2<- sqrt( (new_point-sample_2$feature_BMI[2])^2 )

sample_2["Eu_Dist"]<-c(dist_1 ,dist_2 )


sample_2<-sample_2 %>% mutate(Eu_Dist=sqrt( (new_point-feature_BMI)^2 ))

# now lets add new record

sample_2<-sample_2 %>% dplyr::bind_rows( tibble(feature_BMI=new_point,class=NA,Eu_Dist=NA) )
sample_2$class[3]<-"Healthy weight"
sample_2$class[3]<-sample_2$class[ which.min(sample_2$Eu_Dist) ]


class::knn( sample_2[c(1,2),1],new_point,sample_2[c(1,2),2],k=1, prob=TRUE )

#######################  PART_2

sample_3<-data.frame(  feature_BMI=c( runif(3,18.5,24.9),
                                      runif(3, 25.0,29.9)),
                       class=c( rep("Healthy weight",3),rep("Overweight",3) ) )


ggplot(data=sample_3,aes(y= feature_BMI ,x=class))+
  geom_point(col="red",cex=6)+
  geom_label(aes(label=round(feature_BMI,2)  ),nudge_y = 0.1,nudge_x=0.2 )+
  geom_point(col="red")+geom_point(aes(y=new_point),col="blue",cex=5) 

sample_3<-sample_3 %>% mutate(Eu_Dist=sqrt( (new_point-feature_BMI)^2 ))

# Assume that k equals 3 , threrefore we need to choode free nearest neighbours

sample_3 <-sample_3 %>% arrange(Eu_Dist) %>% slice(1:3)

prop.table( table(sample_3$class) )

class::knn( sample_3[,1],new_point,sample_3[,2],k=3, prob=TRUE )


