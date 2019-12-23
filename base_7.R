# In base 7, the digits 0 to 6 are used.  A number like 125 means 1*7^2 + 2*7 + 5, 
# just like base 10 but with 10's replaced by 7's.  When you count in base 7, you start 
# 0, 1, 2, 3, 4, 5, 6, 10, 11, 12, 13, 14, 15, 16, 20, etc.  Write a program that will print 
# the first N numbers in base 7.  Note that you cannot simply use nested for loops to make each 
# digit go from 0 to 6, since you don't know how many digits you will need ahead of time.  Suggestion:  
# store the digits in an array.  It's good enough to have an array with enough space for 10 digits.

base10_to_base_7 <- function(x){
  
  results <- 0
  
  power <- 0
  
  while ((7^power) <= x) {
    
    power <- power + 1
    
  }
  
  for (i in (power - 1):0) {
    
    results <- results + floor(x/(7 ^ i)) * (10 ^ i)
    
    #print(floor(x/(7 ^ i)))
    
    x <- x %% (7 ^ i) # modulus to get remainder

  }
  
  return(results)
  
}

base10_to_base_7(49)


first_N_base_7 <- function(N = 10){
  
  results <- c()
  
  for (i in 0:N) {
    
    results <- c(results, base10_to_base_7(i))
    
  }
  
  return(results)  
  
}

first_N_base_7(20)
