library(dplyr)

inventories = read.csv('data/input_day3.txt', header = F, col.names = 'inventory')

inventories_by_bag = inventories %>%
  dplyr::rowwise() %>%
  dplyr:: mutate(
    bag_1 = substr(inventory, 1, nchar(inventory)/2),
    bag_2 = substr(inventory, nchar(inventory)/2 + 1, nchar(inventory)))

# part 1

priorities = c(letters, LETTERS)
common_items_priority = c()
for (i in seq_len(nrow(inventories_by_bag))){
  
  # find common item
  common_item = intersect(unlist(strsplit(inventories_by_bag$bag_1[i], '')), unlist(strsplit(inventories_by_bag$bag_2[i], '')))
  
  # find corresponding value
  priority = match(common_item, priorities)
  
  # add value to vector
  common_items_priority = append(common_items_priority, priority)
  
}
# sum of all components
sum(common_items_priority)

# Part 2

inventories_vector = inventories %>% 
  dplyr::pull()

elf_groups = list()
i = 1
#  iteratively create list of groups of three inventories
while (i < 299){
  
  elf_groups = append(elf_groups, list(c(inventories_vector[i], inventories_vector[i+1], inventories_vector[i+2])))
  
  i = i + 3

}

badges_priority = c()
for (i in seq_len(length(elf_groups))){
  
  group_of_three = elf_groups[[i]]
  
  # find common item by turning each string of items into a vector with unlist and strsplit
  common_item = intersect(intersect(unlist(strsplit(group_of_three[1], '')), unlist(strsplit(group_of_three[2], ''))), unlist(strsplit(group_of_three[3], '')))
  
  # find corresponding value
  priority = match(common_item, priorities)
  
  # add value to vector
  badges_priority = append(badges_priority, priority)
  
}

sum(badges_priority)
