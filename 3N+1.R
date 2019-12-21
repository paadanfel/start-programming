# The game "3N+1" goes like this.  If N is odd, multiply it by 3 and add 1.  
# If N is even, divide it by 2.  Repeat until N equals 1, if ever.  
# Every value of N that anyone has ever checked eventually leads to 1, 
# but it is an open mathematical problem (known as the Collatz conjecture) 
# whether EVERY value of N eventually leads to 1.Before you start working on 
# these programs, take a sheet of paper and write out what happens when you start 
# with N=5, N=6, N=7, N=9, to get a feel for it.  For example, starting with N=6, the 
# next number is 3, then 10, then 5, then 16, 8, 4, 2, 1 and we are done


# Write a program three_N that defines a starting value of N and plays the game "3N+1" 
# and prints to the screen the values of N visited along the way.  Use a while loop and stop 
# when N=1.  Investigate some starting values of N.  N=27 is particularly interesting

three_N <- function(N){
  
  print(paste0("N = ", N))
  
  while (N != 1){
    
    if (N/2 == as.integer(N/2)) { # N is even
      N <- N/2
    }else{ # N is odd
      N <- 3 * N + 1
    }
    
    print(paste0("N = ", N))
  }
  
  
}

three_N(27)



# Introduce an additional variable to count how many steps are needed 
# to get to 1, and have the program print that counter after it arrives at N=1. 

three_N_with_count <- function(N){
  
  count <- 0
  
  print(paste0("N = ", N))
  
  while (N != 1){
    
    count <- count + 1
    
    if (N/2 == as.integer(N/2)) { # N is even
      N <- N/2
    }else{ # N is odd
      N <- 3 * N + 1
    }
    
    print(paste0("N = ", N))
  }
  
  print("*******")
  
  print(paste0("There were ", count, " iterations!"))
  
}

three_N_with_count(27)


# Introduce an additional variable to keep track of the maximum value 
# of N before the program reaches N=1.  Here is the logic:  Before the while 
# loop, set m=0.  Within the while loop, if N > m, set m equal to N


three_N_with_max_value <- function(N){
  
  print(paste0("N = ", N))
  
  m <- 0
  
  while (N != 1){
    
    if (N > m) {
      m <- N
    }
    
    if (N/2 == as.integer(N/2)) { # N is even
      N <- N/2
    }else{ # N is odd
      N <- 3 * N + 1
    }
    
    print(paste0("N = ", N))
  }
  
  print("*******")
  
  print(paste0("The max value of N is ", m))
  
}

three_N_with_max_value(5)


# Write another program called multiple_three_N that runs the three_N 
# function for N values going from 2 to 400.  List to the screen the numbers 
# of steps taken and the maximum values for each starting value of N

three_N_with_count_and_max_value <- function(N){ # long variable name sorry :)
  
  initial_N <- N
  
  count <- 0
  
  m <- 0
  
  while (N != 1){
    
    count <- count + 1
    
    if (N > m) {
      m <- N
    }
    
    if (N/2 == as.integer(N/2)) { # N is even
      N <- N/2
    }else{ # N is odd
      N <- 3 * N + 1
    }
    
  }
  
  times <- if (count > 1) { " times " }else{ " time " }
  
  print(paste0(initial_N, " run for ", count, times ,"& reached a max of ", m))
  
}


three_N_with_count_and_max_value(27)



multiple_three_N <- function(){
  
  for (i in 2:400) {
    three_N_with_count_and_max_value(i)
  }
  
}

multiple_three_N()




##################
#### GRAPHING ####
##################

three_N_with_max_value_only <- function(N){
  
  m <- 0
  
  while (N != 1){
    
    if (N > m) {
      m <- N
    }
    
    if (N/2 == as.integer(N/2)) { # N is even
      N <- N/2
    }else{ # N is odd
      N <- 3 * N + 1
    }
  
  }
  
  return(m)
  
}


three_N_with_count_only <- function(N){
  
  count <- 0
  
  while (N != 1){
    
    count <- count + 1
    
    if (N/2 == as.integer(N/2)) { # N is even
      N <- N/2
    }else{ # N is odd
      N <- 3 * N + 1
    }
   
  }
  
  return(count)
  
}


data <- data.frame()

for (i in 2:400) {
  data <- rbind(data, c(i,three_N_with_max_value_only(i), three_N_with_count_only(i)))
}

names(data) <- c("N", "max_value", "steps_count")


library(ggplot2)

png(file = "N_vs_max_value.png")

ggplot(data, aes(N, max_value)) +
  geom_point(alpha = 1/1.7, size = 3, col = "steelblue") + 
  labs(title = "Max value as a function of N", y = "Max Value")

dev.off()
  

png(file = "max_value_vs_steps_count.png")

ggplot(data, aes(max_value, steps_count)) +
  geom_point(alpha = 1/3, size = 3, col = "steelblue") + 
  geom_smooth(method = "lm") + 
  labs(title = "Max value Vs Steps count", x = "Max Value", y = "Steps Count")

dev.off()


three_N_visits <- function(N){
  
  visits <- N
  
  #visits <- rbind(visits, N)
  
  while (N != 1){
    
    if (N/2 == as.integer(N/2)) { # N is even
      N <- N/2
    }else{ # N is odd
      N <- 3 * N + 1
    }
    
    visits <- c(visits, N)
    
  }
  
  return(visits)
}

three_N_visits(5)


data <- c()

for (i in 2:400) {
  
  data <- c(data, three_N_visits(i))
  
}

data <- data.frame(data)

png(file = "all_visits.png")

ggplot(data, aes(data)) + 
  geom_histogram(bins = 100) + 
  labs(title = "Visits", subtitle = "All visits for N from 2 to 400", x = "Visits", y = "Frequency")
  
dev.off()
