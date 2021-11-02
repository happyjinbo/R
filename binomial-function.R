# ===============================================
# title: hw 03
# description: binomial probability functions
# input: numbers
# output: functions
# ===============================================

#' @title function is_integer
#' @description return true if input is interger
#' @param x
#' @return true or false
is_integer <- function(x){
  if(x%%1 == 0){
  TRUE
  }else{
    FALSE
  }
}


#' @title function is_positive
#' @description return true if input is positive
#' @param x
#' @return true or false
is_positive <- function(x){
   if(x%%2 == 0){
   TRUE
   }else{
    FALSE
   }
  }

#' @title function is_nonnegative
#' @description return true if input is nonnegative
#' @param x
#' @return true or false
is_nonnegative <- function(x){
  if(x >= 0){
    TRUE
  }else{
    FALSE
  }
}

#' @title function is_positive_integer
#' @description return true if input is positive and also integer
#' @param x
#' @return true or false
is_positive_integer <- function(x){
  if(is_integer(x)==TRUE && x > 0){
    TRUE
  }else{
    FALSE
  }
}

#' @title function is_nonneg_integer
#' @description return true if input is nonnegative and also integer
#' @param x
#' @return true or false
is_nonneg_integer <- function(x){
  if(is_integer(x)==TRUE && x >= 0){
    TRUE
  }else{
    FALSE
  }
}

#' @title function is_probability
#' @description return true if input is between 0 and 1
#' @param x
#' @return true or false
is_probability <- function(x){
  if(x >= 0 && x <= 1){
    TRUE
  }else{
    FALSE
  }
}

#' @title function bin_factorial
#' @description compute a number's factorial
#' @param x
#' @return a number
prob <- 1
bin_factorial <- function(x){
  if(x==0){
    return(1)
  }else{
    for(i in 1:x){
      prob <- prob *i
    }
  }
  return(prob)
}

#' @title function bin_combinations
#' @description compute a combinations of n and k
#' @param n
#' @param k
#' @return a number
bin_combinations <- function(n,k){
  numerator <- bin_factorial(n)
  denominator <- bin_factorial(k)*bin_factorial(n-k)
  return(numerator/denominator)
}


#' @title function bin_probability
#' @description compute the probability if given trials, prob
#' @param t
#' @param s
#' @param p
#' @return a probability
bin_probability<-function(t,s,p){
  if(is_nonneg_integer(t)==FALSE|is_nonneg_integer(s)==FALSE)
    stop("trials should not be negative")
  if(is_probability(p)==FALSE)
    stop("success should not be negative")
  else
    return(bin_combinations(t,s)*(p^s)*(1-p)^(t-s))
}

#' @title function bin_distribution
#' @description compute the probability if given trials, success and prob
#' @param t
#' @param s
#' @return a probability
bin_distribution <- function(t,p){
  probability <- c(0:t)
  success <- c(0:t)
  for(i in 0:t){
    probability[i+1] <- bin_probability(t,i,p)
    print(probability[i+1])
  }
    colname <- c('success','probability')
    result <- data.frame('success'=success,'probability'=probability)
  return(result)
}

















