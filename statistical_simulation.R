## Statistical simulation
library(ggplot2)

# Flip a coin twenty times, keeping track of the individual outcomes (1 = heads, 0 = tails).  

flips_20 <- function(){
  
  return(sample(c(1,0), 20, replace = TRUE))
  
}

flips_20()

  
# The number of heads observed after 1000 trials

heads <- c()

for (i in 1:1000) {
  
heads <- c(heads, sum(flips_20()))
  
}

table(factor(heads)) # frequency table

# barchart
qplot(factor(heads), 
      xlab = "Number of heads",
      ylab = "Frequency") + labs(title = "Number of heads per 20 flips", subtitle = "Out of 1000 trials")




# The number of trials where the running count of heads exceeds the running count of tails

heads_greater_than_tails <- 0

for (i in 1:1000) {
  
  if (sum(flips_20()) > 10) {
    heads_greater_than_tails <- heads_greater_than_tails + 1
  }
  
}

print(paste0("There were ",heads_greater_than_tails,
             " trails where the running count of heads exceeds the running count of tails."))




# The number of switches in the sequence (a switch is where one changes from heads to tails or from tails to heads)

all_switches <- c()
  
for (i in 1:1000) {
  
  switches <- 0
  
  flips <- flips_20()
  
  for (i in 1:20) {
    
    if (flips[i] != flips[i + 1] && (i + 1) <= 20) {
      
      switches <- switches + 1
      
    }
    
  }
  
  all_switches <- c(all_switches, switches)
  
}


table(factor(all_switches)) # frequency table

# barchart
qplot(factor(all_switches), 
      xlab = "Number of switches",
      ylab = "Frequency") + labs(title = "Number of switches per 20 flips", subtitle = "Out of 1000 trials")

