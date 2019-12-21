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