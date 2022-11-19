library(dplyr)

########## WORK IN PROGRESS --------------------------------------
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
