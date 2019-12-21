## Maximum, minimum, and sorting lists of random numbers

# Generate a list of 20,000 random numbers and store them in a one-dimensional array. 

x <- runif(20000)

print(x)


# Use a for loop to go through the numbers and keep track of the 
# minimum and maximum values, then print those out.

max <- x[1]

min <- x[1]

for (i in 1:length(x)) {
  if (x[i] > max) {
    max <- x[i]
  }
  
  
  if (x[i] < min) {
    min <- x[i]
  }
}

print(paste0("Max: ",max, " & Min: ",min))


# Adapt your sorting procedure to sort the list of numbers.

sort_x <- function(x){
  
  swap_made <- 1
  
  while (swap_made == 1) { #  pass throughs
    
    swap_made <- 0
    
    for (i in 1:(length(x)-1)) {
      
      temporal_x <- x[i]
      
      if (x[i] > x[i+1]) {
        x[i] <- x[i+1]
        x[i+1] <- temporal_x
        
        swap_made <- 1
      }
      
    }
    
  }
  
  return(x)
  
}


sort_x(x)


# Adapt your sorting procedure to record how many passes it takes through 
# the list to get them all sorted.  It should be considerably less than 20,000


sort_x_pass_throughs <- function(x){
  
  count <- 0
  
  swap_made <- 1
  
  while (swap_made == 1) { #  pass throughs
    
    count <- count + 1
    
    swap_made <- 0
    
    for (i in 1:(length(x)-1)) {
      
      temporal_x <- x[i]
      
      if (x[i] > x[i+1]) {
        x[i] <- x[i+1]
        x[i+1] <- temporal_x
        
        swap_made <- 1
      }
      
    }
    
    if(count/1000 == as.integer(count/1000)){message(paste0("Sorting... ", count, " passes so far. Patience-is-a-virtue.org"))}
    
    
  }
  
  print(paste0("Sorting completed! There were ", count, " passthroughs."))
  
}

sort_x_pass_throughs(x)




# Write a for loop that will run 1000 times.  Each time, within 
# the for loop, generate a new list of 20,000 numbers, sort it, and 
# add the number of passes to a list.  When the for loop is done, print 
# out the average value of the numbers in the list and also the minimum and 
# the maximum.  This gives you some idea of the variability in the runtime of a sorting procedure.

sort_x_pass_throughs_value_only <- function(x){
  
  count <- 0
  
  swap_made <- 1
  
  while (swap_made == 1) { #  pass throughs
    
    count <- count + 1
    
    swap_made <- 0
    
    for (i in 1:(length(x)-1)) {
      
      temporal_x <- x[i]
      
      if (x[i] > x[i+1]) {
        x[i] <- x[i+1]
        x[i+1] <- temporal_x
        
        swap_made <- 1
      }
      
    }
    
    #if(count/1000 == as.integer(count/1000)){message(paste0("Sorting... ", count, " passes so far. Patience-is-a-virtue.org"))}
    
    
  }
  
  #print(paste0("Sorting completed! There were ", count, " passthroughs."))
  
  return(count)
  
}

x <- runif(20000)

passes <- sort_x_pass_throughs_value_only(x)

for (i in 1:999) {
  
  x <- runif(20000)
  
  passes <- c(passes, sort_x_pass_throughs_value_only(x))
  
}

print(paste0("Average number of pass throughs: ",mean(passes)))
print(paste0("Maximum number of pass throughs: ",max(passes)))
print(paste0("Minimum number of pass throughs: ",min(passes)))


## TO-DO Plot passes


# Adapt your sorting procedure to record how many swaps it requires.  
# As above, run the sorting procedure repeatedly and print out the minimum, 
# the maximum, and the average number of swaps required.

sort_x_swaps_value_only <- function(x){
  
  swaps <- 0
  
  swap_made <- 1
  
  while (swap_made == 1) { #  pass throughs
    
    swap_made <- 0
    
    for (i in 1:(length(x)-1)) {
      
      temporal_x <- x[i]
      
      if (x[i] > x[i+1]) {
        x[i] <- x[i+1]
        x[i+1] <- temporal_x
        
        swap_made <- 1
        
        swaps <- swaps + 1
      }
      
    }
    
  }
  
  return(swaps)
  
}

x <- runif(20000)

swaps <- sort_x_swaps_value_only(x)

for (i in 1:999) {
  
  x <- runif(20000)
  
  swaps <- c(swaps, sort_x_swaps_value_only(x))
  
}

print(paste0("Average number of pass throughs: ",mean(swaps)))
print(paste0("Maximum number of pass throughs: ",max(swaps)))
print(paste0("Minimum number of pass throughs: ",min(swaps)))


## TO-DO Plot swaps


# Generate a vector of 20,000 values ordered from 1 to 20,000. 
# How many passes does it take your sorting procedure to sort this vector? 
# How many swaps are performed?

asc_ordered <- 1:20000

number_of_passes <- sort_x_pass_throughs_value_only(asc_ordered)

print(paste0("Number of passes when vector is ordered from 1 to 20,000 is ",number_of_passes))

number_of_swaps <- sort_x_swaps_value_only(asc_ordered)

print(paste0("Number of swaps when vector is ordered from 1 to 20,000 is ",number_of_swaps))



# Generate a vector of 20,000 values ordered from 20,000 to 1. 
# How many passes does it take your sorting procedure to sort this vector? 
# How many swaps are performed?

desc_ordered <- 20000:1

number_of_passes <- sort_x_pass_throughs_value_only(desc_ordered)

print(paste0("Number of passes when vector is ordered from 20,000 to 1 is ",number_of_passes))

number_of_swaps <- sort_x_swaps_value_only(desc_ordered)

print(paste0("Number of swaps when vector is ordered from 20,000 to 1 is ",number_of_swaps))









