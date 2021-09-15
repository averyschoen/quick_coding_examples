library(stringr)
library(dplyr)



## Principle Financial Vending Machine Question
vm_string = "A1, Lays, 1.50
A2, Dorritos, 2.00
A3, Pringles, 1.00
B1, Banana, 0.50
B2, Apple, 1.25
B3, Pear, 1.75
C1, Oreos, 3.00
C2, Chips Ahoy, 2.75
C3, Nutella, 4.00"

items = unlist(str_split(vm_string, "\n"))

df <- as.data.frame(items)

vm <- df %>%
  rowwise %>%
  mutate(values = (str_split(items, ", ")),
         key = values[1],
         product = values[2],
         cost = values[3])

vendingmachine <- function(k){
  if (k %in% vm$key) {
    finding <- vm %>% filter(k == key)
    print(paste("Product at", k, "is", finding$product, "and the cost is", finding$cost))


  } else {
    print("No product is associated with given key")
  }
}

vendingmachine("A1")
vendingmachine("C3")
vendingmachine("A7")

