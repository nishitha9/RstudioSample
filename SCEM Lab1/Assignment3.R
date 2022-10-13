#Exploratory Data Analysis 

HawksTail <- Hawks$Tail
sampleMeanHawksTail <- mean(HawksTail)
sampleMedianHawksTail <- median(HawksTail)

HawksWingAndWeight <- Hawks %>%
  group_by(Species) %>%
  summarise(Wing_mean = mean(Wing,na.rm = TRUE), Wing_t_mean = mean(Wing,na.rm = TRUE,trim=0.5),Wing_med = median(Wing,na.rm = TRUE), Weight_mean = mean(Weight,na.rm = TRUE), Weight_t_mean = mean(Weight,na.rm = TRUE,trim=0.5), Weight_med = median(Weight,na.rm = TRUE))

HawksTailMean <- mean((HawksTail*2)+3)
VarianceHawksTail <- var(HawksTail)
SDHawksTail<- sd(HawksTail)

# The mean of original Hawks Tail = Twice the Mean of Altered Hawks Tail plus 3
VarianceHawksTailNew <- var(HawksTail*2+3)
SDHawksTailNew<- sd(HawksTail*2+3)

#The case is not the same here. 

hal<-Hawks$Hallux 
hal<-hal[!is.na(hal)]

outlier_val<-100
num_outliers<-10
corrupted_hal<-c(hal,rep(outlier_val,times=num_outliers)) #Adding Outliers inside the hal vector. 

#Median Function
num_outliers_vect <- seq(0,1000)
means_vect <- c()
medians_vect <- c()
t_means_vect <- c()
for(num_outliers in num_outliers_vect){
  corrupted_hal <- c(hal,rep(outlier_val,times=num_outliers))
  means_vect <- c(means_vect, mean(corrupted_hal))
  medians_vect <- c(medians_vect, median(corrupted_hal))
  t_means_vect <- c(t_means_vect, mean(corrupted_hal,trim = 0.1))
}
df_means_medians <- data.frame(num_outliers=num_outliers_vect, mean=means_vect,
                               t_mean=t_means_vect, median=medians_vect)

df_means_medians %>%
  pivot_longer(!num_outliers, names_to = "Estimator", values_to = "Value") %>%
  ggplot(aes(x=num_outliers,color=Estimator, linetype=Estimator,y=Value)) +
  geom_line()+xlab("Number of outliers")

# Ans - Means seems to look more robust 


#BoxPlot 

ggplot(Hawks, aes(x= Species, y = Weight))+geom_boxplot()

Hawks_quantile <- Hawks %>%
  group_by(Species) %>%
  summarise(quantile025 = quantile(Weight, probs = 0.25, na.rm = TRUE), quantile050 = quantile(Weight, probs = 0.50, na.rm=TRUE), quantile075 = quantile(Weight, probs = 0.75, na.rm=TRUE))

#The borders of the box corresponds to qunatile25 and quantile75, the line inside the box is quantile50


#Function for Outlier 
outlier<-0
number_of_outlier <- function(x){
  species_iqr <- IQR(x,na.rm = TRUE)
  q25<-quantile(x, probs = 0.25, na.rm = TRUE)
  q75<-quantile(x, probs = 0.75, na.rm = TRUE)
  q25_cond<-q25-1.5*species_iqr
  q75_cond<-q75+1.5*species_iqr
  for(a in x){
    if(!is.na(a) && a<q25_cond){
      outlier<-outlier+1
    } else if(!is.na(a) && a>q75_cond){
      outlier<-outlier+1
    }
  }
  return(outlier)
}

Hawks_outliers <- Hawks %>%
  group_by(Species) %>%
  summarise(num_outliers_weight = number_of_outlier(Weight))

cov(Hawks$Weight, Hawks$Wing,use = 'complete.obs')
cor(Hawks$Weight, Hawks$Wing,use = 'complete.obs')

variance_new <- cov(Hawks$Weight*2.4+7.1, Hawks$Wing*-1+3,use = 'complete.obs')
realtion_new <- cor(Hawks$Weight*2.4+7.1, Hawks$Wing*-1+3,use = 'complete.obs')
#Set Theory 

# 1.Random Experiment is a procedure which can have a set of possible outcomes. 
# 2. Event is one of the possible outcomes of an experiment. 
# 3. Sample space is the total number of possible outcomes of an experiment
# 4. Random Experiment of throwing a dice twice: 
# Event= (1,2) 
# Sample Space = {(1,1),(1,2),(1,3),(1,4),(1,5),(1,6)
#                 (2,1),(2,2),(2,3),(2,4),(2,5),(2,6)
#                 (3,1),(3,2),(3,3),(3,4),(3,5),(3,6)
#                 (4,1),(4,2),(4,3),(4,4),(4,5),(4,6)
#                 (5,1),(5,2),(5,3),(5,4),(5,5),(5,6)
#                 (6,1),(6,2),(6,3),(6,4),(6,5),(6,6)}
# Yes, empty set is also an event. 
# AUB = {1,2,3,4,6}
# AUC = {1,2,3,4,5,6}
# A intersection B = {2}
# A intersection C = {}
# A complement B = {4,6}
# A complement C = {4,5,6}
# A and B are not disjoint sets but A and C are disjoint sets
# B and A complement B are not disjoint sets. 
# Partition of {1,2,3,4,5,6} Two sets= {1,2,3} and {4,5,6}, Three sets= {1,2},{3,4},{5,6}
# Q2 1.A 2.Empty Set 3.


#Visualisation 


ggplot(Hawks,aes(x=Tail,color=Species))+geom_density()+theme_bw()+xlab("Tail(mm)")+ylab("Density")
ggplot(Hawks,aes(x=Tail, y=Species, fill=Species))+geom_violin()+theme_bw()+xlab("Tail(mm)")+ylab("Species") # 
ggplot(Hawks,aes(x=Tail, y=Weight, color=Species, shape=Species))+geom_point()+theme_bw()+xlab("Tail(mm)")+ylab("Weight(mm)") #4 Aesthetics, 1 gyphs, 2 visual cues- Shape and Color
ggplot(Hawks,aes(x=Tail, y=Weight, color=Species))+geom_point()+geom_smooth()+theme_bw()+xlab("Tail(mm)")+ylab("Weight(mm)")+facet_wrap(~Species) #The Weight and Tail length of the Hawks are positively correlated

max(Hawks$Weight,na.rm=TRUE)
ggplot(Hawks,aes(x=Tail, y=Weight, color=Species))+geom_point()+theme_bw()+xlab("Tail(mm)")+ylab("Weight(mm)") +
  geom_curve(x=230, xend=200, y=1980, yend=2030, arrow=arrow(length=unit(0.3,"cm")),curvature = 0.1)+
  geom_text(x=250,y=1970, label="Heaviest Hawk")

