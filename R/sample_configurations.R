library(dplyr)
source(file.path("R", "create_tasks.R"))


# function(digits = 3, full = F)
## This function gets you a vector with all numbers with x digits
### full = include numbers with less than x digits
get_numbers(2, F)


# function(pool_a = get_numbers(2, F), pool_b = get_numbers(2, F))
## This function constructs all addition possible addition tasks in a + b = c format
### pool_a = 
my_items <- addition_task(get_numbers(3,F), get_numbers(3,F)) %>%
  filter(c < 10000)

get_borrow(my_items)
get_carry(my_items)


my_items <- my_items %>%
  get_carry(remove = F)

##### Math Tasks 0 carry operations, 1 carry, 2 carry
my_items <- addition_task(get_numbers(3,F), get_numbers(3,F)) %>%
  filter(a %in% a[grep(0,a, invert = T)]) %>%
  filter(b %in% b[grep(0,b, invert = T)]) %>%
  filter(c %in% c[grep(0,c, invert = T)]) ##%>%
  ##filter(c < 1000)

my_items_easy <- my_items %>%
  get_carry(remove = T)

my_items_medium <- my_items %>%
  get_carry() %>%
  filter(carry1 + carry2 + carry3 == 1) %>%
  select(!c(carry1,carry2,carry3))

my_items_hard <- my_items %>%
  get_carry() %>%
  filter(carry1 + carry2 + carry3 == 2) %>%
  select(!c(carry1,carry2,carry3))
