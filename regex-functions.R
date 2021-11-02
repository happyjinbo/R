# ======================
# regex-functions
# ======================

#2.1)
### ================================
###' @title split_char
###' @description split string into characters
###' @param a string
###' @return a list of string of each character
###' ===============================
split_chars <- function(x){
  unlist(str_split(x,pattern = ''))
}

#2.2)
### ================================
###' @title num_vowel
###' @description count the vowel of a string
###' @param a string
###' @return a table of the quantitiy of vowel of the string
###' ===============================
num_vowels <- function(x){
  
  count_a <- sum(str_detect(split_chars(x),pattern = 'a|A'))
  count_e <- sum(str_detect(split_chars(x),pattern = 'e|E'))
  count_i <- sum(str_detect(split_chars(x),pattern = 'i|I'))
  count_o <- sum(str_detect(split_chars(x),pattern = 'o|O'))
  count_u <- sum(str_detect(split_chars(x),pattern = 'u|U'))
  
  vowel <- c(a=count_a,e=count_e,i=count_i,o=count_o,u=count_u)
  vowel
}


# 2.3)
### ================================
###' @title count_vowels
###' @description count the vowels of a sentence
###' @param a string
###' @return a table of the quantity of vowel of a sentence
###' ===============================
count_vowels <- function(x){
  words <- unlist(str_split(x,pattern = " "))
  letters <-split_chars(words)
  count <- num_vowels(letters)
  count
}


# 2.4)
### ================================
###' @title reverse_chars
###' @description reverse the character of a string
###' @param a string
###' @return a string of character reversed
###' ===============================
reverse_chars<-function(x){
  chars <- split_chars(x)
  d<-length(chars)
  new_char <- vector(mode = "character", length = d)
  for(i in 1:d){
    new_char[i] = chars[d-i+1]
  }
  paste(new_char,collapse  = "")
}


# 2.5
### ================================
###' @title reverse_words
###' @description reverse the words in other
###' @param a string
###' @return words in reverse order
###' ===============================
reverse_words <- function(x){
  break_word <- unlist(sapply(x, strsplit, " "))
  len <- length(break_word)
  new_string <- vector(length = len)
  for(i in 1:len){
    new_string[i] = break_word[len-i+1]
  }
  paste(new_string,collapse = " ")
}


