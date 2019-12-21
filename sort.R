# Bubble sort implementation

names = c("Wilson","Reagan","Kennedy","Washington","Hoover","Lincoln","Carter","Jefferson","Roosevelt","Adams") 

swap_made <- 1

while (swap_made == 1) { #  pass throughs
  print(names)
  
  swap_made <- 0
  
  for (i in 1:(length(names)-1)) {
    
    temporal_name <- names[i]
    
    if (names[i] > names[i+1]) {
      names[i] <- names[i+1]
      names[i+1] <- temporal_name
      
      swap_made <- 1
    }
    
  }
  
  #if (swap_made == 1) { message("swap made") }else{ message("swap not made") }
  
}


