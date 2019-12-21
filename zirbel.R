#####
### Writing for loops


# Write a program called sum_to_300 that will calculate the sum 1+2+3+...+300.  
# The idea is to create a variable, s, that will store the current value of the sum.  
# Set it to zero, then use a for loop that will create another variable, i, which will run 
# through the numbers 1, 2, 3, ... and add each of these to the current sum.  After the for loop, 
# output the value of the sum.  Hint: the correct answer ends with 150.

sum_to_300 <- function(){
  
  s <- 0 # initialise s
  
  for (i in 1:300) { # loop through 1 to 300
    s <- s + i # add i to s
  }
  
  return(s) # return s
}


sum_to_300() # call s


# Write a program called sum_to_N to calculate the sum 1+2+3+...+N.  
# Set N equal to 1000, 10000, and 100000 to see what sum you get.

sum_to_N <- function(N){
  
  s <- 0 # initialise s
  
  for (i in 1:N) { # loop through 1 to N
    s <- s + i # add i to s
  }
  
  return(s) # return s
}

sum_to_N(1000)
sum_to_N(10000)
sum_to_N(100000)



# Write a program called sum_squares_to_400 to calculate the sum 1^2 + 2^2 +3^2 + ... 400^2.  
# Check:  The sum of the digits in the final answer is 15.


sum_squares_to_400 <- function(){
  
  s <- 0 # initialise s
  
  for (i in 1:400) { # loop through 1 to N
    s <- s + i^2 # add i^2 to s
  }
  
  return(s) # return s
}

sum_squares_to_400()


# Write a program called sum_products_to_250 to calculate the sum 1??2+2??3+3??4+ ... + 249??250.  
# Check:  The correct answer ends with 250.

sum_products_to_250 <- function(){
  
  s <- 0 # initialise s
  
  for (i in 1:249) { # loop through 1 to N
    s <- s + i*(i+1) # add  i*(i+1) to s
  }
  
  return(s) # return s
}

sum_products_to_250()



# Write a program called factorial_10 to calculate 10! ("10 factorial"), 
# which is defined to be 1*2*3*4*5*6*7*8*9*10

factorial_10 <- function(){
  
  s <- 1 # initialise s
  
  for (i in 1:10) { # loop through 1 to N
    s <- s * i 
  }
  
  return(s) # return s
}

factorial_10()


# Write a program called factorial to calculate N! ("N factorial"), 
# where N is a non-negative integer.  Note that 0! is defined to be 1.

factorialN <- function(N){
  
  if (N == 0) {
    
    return(1)
    
  }else{
    
    s <- 1 # initialise s
    
    for (i in 1:N) { # loop through 1 to N
      s <- s * i 
    }
    
    return(s) # return s
  }
  
}

factorialN(10)


# Write a program called factorial_table.py to print out 0!, 1!, 2!, 
# all the way up to 15!. You can do this by writing a for loop over the value
# of N, and within that for loop, use the code you wrote to compute N!.  Then you 
# will have one for loop inside of another for loop, which is called a nested for loop.  

factorial_table <- function(){
  
  for (i in 0:15) {
    
    print(paste0(i,"! is ",factorialN(i)))
    
  }
  
}

factorial_table()


# Write a program called sum_n_to_power_n to calculate 1^1 + 2^2 + 3^3 + ... + 10^10.  
# As an exercise, I suggest that you solve this by writing nested for loops.  I suggest 
# that you print out the values you are calculating for 1^1 , 2^2 ,3^3 , ... 10^10  to make sure 
# you are adding the right values.  Check:  The sum equals 10405071317


sum_n_to_power_n <- function(){
  
  s <- 0
  
  for (i in 1:10) {
    s <- s + i^i
  }
  
  return(s)
  
}


sum_n_to_power_n()



# Write a program called sum_taylor_series to calculate 1/0! + 
# 1/1! + 1/2! + 1/3! + ... + 1/10!.  Display the sum after each 
# term is added.  Use a for loop to create and sum the 11 terms in 
# this sum.  Within the for loop, use another for loop to calculate 
# the factorial.  This sum approximates a number that is well known to 
# calculus students.  Add more terms to get a better approximation.


sum_taylor_series <- function(){
  
  s <- 0
  
  for (i in 0:10) {
    s <- s + (1/factorialN(i))
  }
  
  
  return(s)
  
}


sum_taylor_series()




##### 
### Writing while loops


# Write a program called square_greater_than_1777 that will figure out
# the smallest number n whose square is greater than 1777, without using 
# the square root function.  The idea is to start a variable n at 1, check 
# if n*n is greater than 1777, and if not, increase the value of n, check again, and continue.

square_greater_than_1777 <- function(){
   
  n <- 1
  
  while(n*n < 1777){
    
    n <- n+1
    
  }
  
  return(n)
  
}

square_greater_than_1777()



# Write a program called sum_hits_one_million that will
# figure out how many terms in the sum 1+2+3+... it requires
# for the sum to exceed one million.  The idea is to create a 
# variable that will store the current value of the sum and another 
# variable that keeps track of what number you are adding to the sum.  
# Use a while loop to add the current number to the sum repeatedly, and 
# increase the current number by 1 each time.  The while loop should stop 
# once the sum exceeds one million, then you tell the value of the last term 
# that was added to the sum.  Hint:  the sum will be 1000405.


sum_hits_one_million <- function(){
  
  count <- 1
  
  m <- 0
  
  while (m < 1000000) {
    
    m <- m + count
    
    count <- count + 1
    
  }
  
  return(count)
  
}

sum_hits_one_million()

#####
### If-else statements

# Write a program called sum_to_300_first_sums which will 
# use a for loop to calculate the sum 1+2+3+...+300 and which
# will print the value of the current sum if that value is less 
# than 40000, and which will print "The sum is over 40000" if not.


sum_to_300_first_sums <- function(){
  
  s <- 0
  
  for (i in 1:300) {
    
    s <- s + i
    
    if (s < 40000) {
      print(paste0("The sum is ",s))
    }else{
      print(paste0("The sum is over 40000"))
    }
    
  }
  
  
}

sum_to_300_first_sums()


# Write a program called sum_to_300_updates to calculate 
# the sum 1+2+3+...+300.  Display the total after every 20 
# terms by using an if statement to check if the current number 
# of terms is a multiple of 20.  The basic idea is this:  calculate 
# i/20 as a decimal and also calculate i/20 rounded to the nearest integer.  
# If they are equal, then i is a multiple of 20, and then you print the sum. Different
# programming languages use different commands to round to the nearest integer.  Some programming 
# languages calculate i/20 as a decimal, while others return the integer part of the quotient, so that 35/20 = 1

sum_to_300_updates <- function(){
  
  s <- 0
  
  for (i in 1:300) {
    
    s <- s + i
    
    if (i/20 == as.integer(i/20)) {
      print(paste0("The sum at ",i, "th term is ", s))
    }
    
  }
  
}

sum_to_300_updates()































