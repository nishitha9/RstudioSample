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
  pivot_longer(wins_data_frame, names_to = "Year", values_to = "Wins/Total")
print(wins_tidy)
