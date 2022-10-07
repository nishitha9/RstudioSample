##Function to First Sum of n Numbers Divisible by 2/7/both
myFirstRFunction <- function(n) {
  count <- n   
  sumOfNumbers <- 0
   for(n in 1:count-1){
     if(n!=0 && n %% 2==0){
       sumOfNumbers <- sumOfNumbers+n
     }else if(n%% 7 ==0){
       sumOfNumbers <- sumOfNumbers+n
     }
   }
  print(sumOfNumbers)
}


