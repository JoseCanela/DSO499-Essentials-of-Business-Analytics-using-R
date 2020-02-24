# CREATING A NEW FUNCTIONS

# Examples of functions

# sum
sum(3,4)

# Now let's create a function called 'pow2' for squaring numbers

pow2 <- function(x){
  y = x^2
  return(y)
}

pow2(3) # Returns 9

# Now let's create a new function that works for any power value.
# Call it 'pow'.

pow <- function(base, exponent){
  y = base^exponent
  return(y)
}

pow(2,3) # Returns 8

# Look what happens when we only use the base
pow(2)

# WE GET AN ERROR B/C THERE IS NO DEFAULT VALUE FOR EXPONENT!
# Error in pow(2) : argument "exponent" is missing, with no default

# Let's have our default exponent value equal 1:
pow <- function(base, exponent = 1){
  y = base^exponent
  return(y)
} 

pow(2) # Returns 2
pow(2,3) # Returns 8 again
