library(dplyr)

########## WORK IN PROGRESS --------------------------------------

operation <- "addition"
## allowed values: "addition", "subtraction", "multiplication", "division"

digits <- 7
## allowed values: 1,2,3,4,5,6,7

var_digits <- c(1,2,3,4,5,6,7)
## allowed values: vector containing values 1 to digits
### To Do: digits and var_digits need to be separately for terms a, b, c, r


task_type <- 1
## allowed values: dependent on operation
## addition: 1,2,3,4,5
## subtraction: 1,2,3
## multiplication: 1,2,3
## division: 1,2,3

difficulty_param <- c(2,3,4)
## allowed values: dependent on operation
## addition: any combination of c(1,2,3,4,5,6,7) -> max value == digits
## subtraction: any combination of c(1,2,3,4,5,6,7) -> max value == digits
## multiplication: nothing yet
## division: number of decimal digits as int

zeros_allowed <- 2
## allowed values: 0 to digits-1

a_pool <- seq(111, 999)[grep(0, seq(111,999), invert = T)]
b_pool <- seq(111, 999)[grep(0, seq(111,999), invert = T)]


#################### FUNCTIONS ----------------------------------------

get_numbers <- function(digits = 3, full = F){
  number_pool <- seq((10^(digits-1))-(sum(9*(10^(seq(0,digits-2))))*full), sum(9*(10^(seq(0,digits-1)))))
  
  return(number_pool)
}

get_numbers(2, F)

addition_task <- function(pool_a = get_numbers(2, F), pool_b = get_numbers(2, F)) {
  library(dplyr)
  my_items <- tibble(expand.grid(a = pool_a, b = pool_b, KEEP.OUT.ATTRS = F)) %>%
    mutate(c = a + b)
  return(my_items)
}

my_items <- addition_task(get_numbers(3,F), get_numbers(3,F)) %>%
  filter(c < 10000)

get_carry <- function(data, remove = F, name = "carry") {
  digits <- log10(data[[1,"a"]])+1
  for (i in seq(digits,1)) {
    if (i < digits) {
      add <- data[,paste0(name, i+1)]
    } else {
      add <- rep(0, dim(data)[1])
    }
    data[,paste0(name, i)] <- as.numeric(as.numeric(substr(data[["a"]], i,i)) + as.numeric(substr(data[["b"]], i,i)) + add >= 10) 
  }
  if (remove) data <- data %>%
      filter(rowSums(data[paste0(name, seq(digits,1))]) == 0) %>%
      select(!paste0(name, seq(digits,1)))
  return(data)
}
get_carry(my_items)

get_borrow <- function(data, remove = F, name = "borrow") {
  digits <- log10(data[[1,"a"]])+1
  for (i in seq(digits,1)) {
    if (i < digits) {
      subt <- data[,paste0(name, i+1)]
    } else {
      subt <- rep(0, dim(data)[1])
    }
    data[,paste0(name, i)] <- as.numeric(as.numeric(substr(data[["a"]], i,i)) - as.numeric(substr(data[["b"]], i,i)) - subt < 0)
  }
  if (remove) data <- data %>%
      filter(rowSums(data[paste0(name, seq(digits,1))]) == 0) %>%
      select(!paste0(name, seq(digits,1)))
  return(data)
}
get_borrow(my_items)


my_items <- my_items %>%
  get_carry(remove = F)
