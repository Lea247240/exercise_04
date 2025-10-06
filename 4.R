rm(list=ls())

#Cesta:
setwd('V:/MPA_PRG/exercise_04')

## The Change Problem
### Task 1
# Convert some amount of money into the fewest number of coins.

# Input:
  # an amount of money `M` to be returned

# Output:
  # the minimum number of coins whose total value will be equal to `M`

# Resources:
  # coins in denominations of 50 (`pd`), 20(`dc`), 10(`ds`), 5(`p`), 2(`d`) and 1(`j`) CZK

# Condition:
  # `50 * pd + 20 * dc + 10 * ds + 5 * p + 2 * d + 1 * j = M`, where `pd`, `dc`, `ds`, `p`, `d` and `j` must have the smallest possible value

###
#ReturnCoins(M)
#1   Give the customer the integer result of dividing M by 50 in 50 CZK coins.
#2   Let remainder be the remaining amount due the customer.
#3   Give the customer the integer result of dividing remainder by 20 in 20 CZK coins.
#4   Let remainder be the remaining amount due the customer.
#5   Give the customer the integer result of dividing remainder by 10 in 10 CZK coins.
#6   Let remainder be the remaining amount due the customer.
#7   Give the customer the integer result of dividing remainder by 5 in 5 CZK coins.
#8   Let remainder be the remaining amount due the customer.
#9   Give the customer the integer result of dividing remainder by 2 in 2 CZK coins.
#10  Give the customer remainder after division in 1 CZK coins.
###

M <- 99

pd <- M %/% 50
zbytek <- M %% 50

ReturnCoins <- function(M){
  while (M > 1){
    # 50CZK
    pd <- M %/% 50
    M <- M %% 50
    # 20CZK
    dc <- M %/% 20
    M <- M %% 20
    # 10CZK
    ds <- M %/% 10
    M <- M %% 10
    # 5CZK
    p <- M %/% 5
    M <- M %% 5
    # 2CZK
    d <- M %/% 2
    M <- M %% 2
    # 1CZK
    j <- M %/% 1
  }
  return(list(pd ,'50CZK', dc, '20CZK', ds, '10CZK', p, '5CZK', d , '2CZK',j , '1CZK'))
}

ReturnCoins(99)


### Task 2
# Convert some amount of money `M` into given denominations, using the smallest possible number of coins.

# Input:
  # an amount of money `M` to be returned
  # an array of `d` denominations `c = (c1, c2, ... , cd)` in descending order `(c1 > c2 > ··· > cd)`

# Output:
  # integer values `i1, i2, ... , id`

# Condition:
  # `c1 * i1 + c2 * i2 + ··· + cd * id = M`, where `i1 + i2 + ··· + id` is as small as possible

# Write a pseudocode for the change problem for any currency. Hint: use array indexing. 
# Implement the pseudocode in R as a separate function `UniversalReturnCoins()`. 
# Find input values for which the algorithm will not work correctly, meaning the output will be incorrect.
i <- list()
print(length(c(50,20,10,5,2,1)))

vek <- c(0)
vek <- c(vek,1)

UniversalReturnCoins <- function(M, mena){
  
  for (c in mena) {
    if (M > 1){
      id <- M %/% c
      M <- M %% c
      
    }
    sum <- c(sum,id, c)
    
  }
  id <- M %/% c
  sum <- c(sum,id, c)
  return(sum) #lépe vypsat jako slovnik, ale to uz si nepamatuju jak 
}

UniversalReturnCoins(99, c(50,20,10,5,2,1))


## The Most Chocolate Path
### Task 3
# In R, implement a recursive function `Chocolate()` according to the following pseudocode.

# Input:
  # a matrix with integer values (number of chocolate bars)
  # an index of current row
  # an index of current column

# Output:
  # the maximum number of chocolate bars, that can be collected

# Solve the same problem iteratively.

###############################
#Chocolate(M, r, c)
#   if r = number of rows in M
#     return M[r, c]
#   else
#     bars ← M[r, c]
#     down ← Chocolate(M, r + 1, c)
#     diagonal ← Chocolate(M, r + 1, c + 1)
#     return max(down, diagonal) + bars
###############################

Chocolate <- function(M, r, c){
  if(r == M[0]){
    
  }
}



