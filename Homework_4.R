# Homework 4: Vectors in R
# 5 February 2020
# EKJ

# -------------------------------------------------------------------------

# QUESTION 1

# Assign variables to their values
x <- 1.1
a <- 2.2
b <- 3.3

# Assign an expression and store as z
z <- x^a^b 
z <- (x^a)^b
z <- 3*x^3 + 2*x^2 + 1
print(z)

# -------------------------------------------------------------------------

# QUESTION 2

# Use the rep() and seq() functions to create these vectors
?rep
?seq
vec_a <- c(seq(from = 1, to = 8, by = 1),seq(from = 7, to = 1, by = -1))
print(vec_a)
seq(from = 1, to = 8, by = 1)
seq(from = 7, to = 1, by = -1)
