#1) Object "die"

#' @title Check sides
#' @description Check validty of sides
#' @param x sides: numeric vector
#' @return Stop or True
#' @export
check_sides <- function(sides){
  if(length(sides) != 6){
    stop("\n'sides' must be a vector of length 6")
  }
  TRUE
}

#' @title Check prob
#' @description Check validty of prob
#' @param x prob : numeric vector
#' @return Stop or True
#' @export
check_prob <- function(prob){
  if(length(prob) != 6 | !is.numeric(prob)){
    stop("\n'prob' must be a numeric vector of length 6")
  }
  if(any(prob < 0) | any(prob > 1)){
    stop("\n'prob' must be values between 0 and 1")
  }
  if(sum(prob) != 1){
    stop("\nelements in 'prob' must add up to 1")
  }
  TRUE
}

#' @title create a object of die class
#' @description create a die object
#' @param x sides
#' @param x prob
#' @return a list contains sides and prob
#' @export
die <- function(sides = c(1,2,3,4,5,6), prob = rep(1/6,6)){
  check_sides(sides)
  check_prob(prob)
  res <- list(sides = sides, prob = prob)
  class(res) <- "die"
  return(res)
}

#' @title print die class
#' @description print results of die class object
#' @param x die class object
#' @return a data frame contains sides and prob
#' @export
print.die <- function(x){
  cat('object "die"\n\n')
  cd <- data.frame(
    side = x$sides, prob = x$prob
  )
  print(cd)
  invisible(x)
}

#2) Object "roll"

#' @title check times
#' @description check validty of times
#' @param x numeric
#' @return stop or true
#' @export
check_times <- function(times){
  if(times %% 1 != 0 | times < 0){
    stop("\ntimes must be a integer")
  }
  TRUE
}

#' @title rolling process
#' @description rolling the die with times
#' @param die class object
#' @param times numeric
#' @return a list contains rolls,sides, prob, and total
#' @export
roll <- function(die,times = 1){
  if(class(die) != 'die'){
    stop("\nroll() requires an object 'die'")
  }
  check_times(times)
  play <- sample(die$sides, size = times, replace = TRUE, prob = die$prob)
  res <- list(
    rolls = play,
    sides = die$sides,
    prob = die$prob,
    total = length(play)
  )
  class(res) <- "roll"
  res
}

#' @title print roll object
#' @description print roll object
#' @param x object of class 'roll'
#' @param times numeric
#' @return a list contains rolls,sides, prob, and total
#' @export
print.roll <- function(x){
  cat('object "roll"\n\n')
  print(x$rolls)
}

#' @title summary roll object
#' @description summary roll object
#' @param x object of class 'roll'
#' @return a data frame contains sides,count, and prob
#' @export
summary.roll <- function(x){
  freqs <- data.frame(
    side = x$sides,
    count = as.vector(table(x$rolls)),
    prop = as.vector(table(x$rolls))/x$total
  )
  res <- list(freqs = freqs)
  class(res) <- "summary.roll"
  return(res)
}

#' @title print summary roll object
#' @description print summary roll object
#' @param x object of class 'roll'
#' @return a data frame contains sides,count, and prob
#' @export
print.summary.roll <- function(x){
  cat('object "summary roll"\n\n')
  print(x$freqs)
  invisible(x)
}

#' @title plot roll object
#' @description plot roll object
#' @param x object of class 'roll'
#' @return a barplot
#' @export
plot.roll <- function(x){
  height <- as.vector(table(x$rolls)/x$total)
  names <- x$sides
  title <- paste0("frequencies in a series of", x$total, "die rolls")
  barplot(height, names.arg = names,  xlab = "sides of die", ylab = "relative frequencies", main = title)
}


#' @title extracting from roll object
#' @description extracting from roll object
#' @param x object of class 'roll'
#' @param i index
#' @return ith item from x
#' @export
"[.roll" <- function(x,i){
  if(i > x$total | i <= 0){
    stop("\nindex should be a nonzero integer and smaller than total")
  }
  x$rolls[i]
}

#' @title replacing item in roll object
#' @description extracting item in roll object
#' @param x object of class 'roll'
#' @param i index
#' @param value replacing value
#' @return new roll after replacing
#' @export
"[<-.roll" <- function(x,i,value){
  if(value != x$sides[1] & value != x$sides[2] & value != x$sides[3] & value != x$sides[4] & value != x$sides[5] & value != x$sides[6]){
    stop("\nreplacing value should be one of the sides")
  }
  if(i > x$total | i <= 0){
    stop("\nindex out of bounds")
  }
  x$rolls[i]<-value
  res <- list(rolls=x$rolls, sides=x$sides, prob=x$prob, total=x$times)
  class(res)<-"roll"
  return(res)
}

#' @title make roll
#' @description make a roll
#' @param x object of class 'roll'
#' @param rolls a rolls
#' @return a list of rolls, sides, prob, and total
#' @export
make_roll <- function(x, rolls) {
  res <- list(
    rolls = rolls,
    sides = x$sides,
    prob= x$prob,
    total = length(rolls)
  )
  class(res)<-"roll"
  res

}

#' @title add elements
#' @description add elements
#' @param x object of class 'roll'
#' @param incr increasing number of elements
#' @return a new list of rolls with adding elements
#' @export
"+.roll" <- function(x,incr){
  if(length(incr) != 1 | incr <= 0){
    stop("\nincreament must positive")
  }
  more_rolls <- roll(die(x$sides,x$prob), times = incr)
  make_roll(x, c(x$rolls, more_rolls$rolls))
}




