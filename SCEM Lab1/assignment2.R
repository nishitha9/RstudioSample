hsf <- select(filter(Hawks, Species=="RT" & Weight>=1000),Wing,Weight,Tail)
#Simplifying with pipe operator 
hsfNew <- Hawks %>%
  filter(Species=="RT" & Weight>=1000)%>%
  select(Wing,Weight,Tail)

#Arrange Function 
hsf <- hsf %>% arrange(Wing)
hsf <- head(hsf, n=4)

#Join And Rename 
hawkSpeciesNameCodes <- data.frame(
  Species = c("CH", "RT", "SS"),
  SpeciesFullName = c("Cooper's", " Red-tailed", "Sharp-shinned")
)
hawksFullName <- Hawks  %>%
  inner_join(hawkSpeciesNameCodes)

hawksFullName <- hawksFullName %>%
  select(-Species) %>%
  rename("Species" = "SpeciesFullName")

hawksFullName %>% 
  select(Species, Wing, Weight) %>%
  head(n=7)


#Bird BMI 

hawksWithBMI <- Hawks %>%
  mutate(bird_BMI= Weight*1000/Wing^2)%>%
  select(Species, bird_BMI)%>%
  arrange(desc(bird_BMI))

#Summarize and GroupBy 

summary <- hawksFullName %>%
  group_by(Species) %>%
  summarize(num_rows=n(), mn_wing= mean(Wing), md_wing= median(Wing), t_mn_wing= mean(Wing, trim=0.1))

summary1 <- hawksFullName %>%
  group_by(Species) %>%
  summarize(across(everything(), ~sum(is.na(.x)))) %>%
  select(Species,Wing, Weight, Culmen, Hallux, Tail, StandardTail, Tarsus, Crop)
print(summary1)

  
#Handling Missing Data 

impute_by_median <- function(x){
  medianResult <- median(x, na.rm = TRUE) # To exclude NA values while calculating the median
  print(medianResult)
  
  imputeFunction <- function(z){
    if(is.na(z)){
      return(medianResult)
    }else{
      return(z)
    }
  }
  return (map_dbl(x, imputeFunction))
}

#Creating Data Frame 

x= seq (from = 0.0, to = 10, by = 0.1)
calculateY <- function(z){
  y <- c((5*z)+1)
  return(y)
}
y <- calculateY(x)
df_xy <-  data.frame(x,y)
print(head(calculateY(x), n=5))

#Function to identify NA 

sometimes_missing <- function(index,value){
  if(index %% 5==0){
    return(NA)
  }else{
    return(value)
  }
}

#Missing Function
x= seq (from = 0.0, to = 10, by = 0.1)
df_xy_mutate <- mutate(df_xy, rowNumber=row_number())
df_xy_missing <- mutate(df_xy_mutate,y=map2(rowNumber,y,sometimes_missing))
vector11 <- df_xy_missiinsng$y
impute_by_median(vector11)
df_xy_imputed <- mutate(df_xy_missing, y=map_dbl(vector11,impute_by_median))

##Wins Data Frame 
print(wins_data_frame %>%
        select(1:5) %>%
        head(3))

wins_tidy <- wins_data_frame %>%
  pivot_longer(!...1, names_to = "Year", values_to = "wins_or_total") 
wins_tidy <- separate(wins_tidy, col= wins_or_total, into=c("Wins", "Total"), sep="of")
print(head(wins_tidy,n=5))

looses_tidy <- lose_data_frame %>%
  pivot_longer(!...1, names_to = "Year", values_to = "wins_or_total") 
looses_tidy <- separate(looses_tidy, col= wins_or_total, into=c("Looses", "Total"), sep="of")
print(head(looses_tidy,n=5))

hockey_df <- full_join(wins_tidy,looses_tidy)
hockey_df <- rename(hockey_df, Team = ...1)

hockey_df$Year <- as.integer(hockey_df$Year)
hockey_df$Wins <- as.integer(hockey_df$Wins)
hockey_df$Total <- as.integer(hockey_df$Total)
hockey_df$Looses <- as.integer(hockey_df$Looses)

hockey_df <- hockey_df %>%
  mutate(Draws = Total-(Wins+Looses), Wins_rt = Wins/Total, Looses_rt = Looses/Total, Draws_rt= Draws/Total) %>%
  group_by(Team) %>%
  summarise(W_md = median(Wins_rt), W_mn= mean(Wins_rt), L_md = median(Looses_rt), L_mn= mean(Looses_rt), D_md= median(Draws_rt), D_mn= mean(Draws_rt))

hockey_df <-arrange(hockey_df,desc(W_md))
##Data Visualisation 

wins_tidy$Wins<- as.integer(wins_tidy$Wins)
wins_tidy$Year <- as.integer(wins_tidy$Year)
wins_tidy$Total <- as.integer(wins_tidy$Total)
print(head(wins_tidy,n=5))

##histogram
wins_tidy_duck <- filter(wins_tidy, ...1 == "Ducks")
wins_of_ducks <- ggplot(data=wins_tidy_duck, aes(x=Wins))+xlab("Wins")
wins_of_ducks+geom_histogram(binwidth = 3)+ylab("Count")

#geom density 
wins_of_ducks+geom_density(adjust=0.5)
wins_of_ducks+geom_density(adjust=2)

#geom point
wins_teams <- wins_tidy %>%
  pivot_wider( names_from = "...1" , values_from= "Wins") %>%
  select(!Total)

wins_of_ducks_and_eagle <- ggplot(wins_teams, aes( x= Ducks, y = Eagles))
wins_of_ducks_and_eagle+geom_point(size=2)


