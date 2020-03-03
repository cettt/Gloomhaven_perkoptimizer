library(tidyverse)

source("perk_functions.R")

#first perk-------------
base_deck0 <- c("miss", "-2", rep("-1", 5), rep("0", 6), rep("1", 5), "2", "2x")

simulation1 <- c("remove two -1", "remove four 0", "replace two +1 with +2", 
                  "replace -2 with 0", "add one +2", "add two rolling +1") %>%
  summarise_perk_stats(base_deck0)

summarise_perk_stats2(simulation1)

#pick remove two -1

#second perk----------------
base_deck1 <- modify_deck(base_deck0, "remove two -1")

simulation2 <- c("remove two -1", "remove four 0", "replace two +1 with +2", 
                 "replace -2 with 0", "add one +2", "add two rolling +1") %>%
  summarise_perk_stats(base_deck1)

summarise_perk_stats2(simulation2)

# pick remove two -1

#third perk-------------
base_deck2 <- modify_deck(base_deck1, "remove two -1")

simulation3 <- c("remove four 0", "replace two +1 with +2", 
                 "replace -2 with 0", "add one +2", "add two rolling +1") %>%
  summarise_perk_stats(base_deck2)

summarise_perk_stats2(simulation3)

#pick replace -2 with 0

#fouth perk--------------
base_deck3 <- modify_deck(base_deck2, "replace -2 with 0")

simulation4 <- c("remove four 0", "replace two +1 with +2", 
                  "add one +2", "add two rolling +1") %>%
  summarise_perk_stats(base_deck3)

summarise_perk_stats2(simulation4)

#pick add two rolling +1

#fifth perk--------------
base_deck4 <- modify_deck(base_deck3, "add two rolling +1")

simulation5 <- c("remove four 0", "replace two +1 with +2", 
                 "add one +2", "add two rolling +1") %>%
  summarise_perk_stats(base_deck4)

summarise_perk_stats2(simulation5)


#pick add two rolling +1

#sixth perk--------------
base_deck5 <- modify_deck(base_deck4, "add two rolling +1")

simulation6 <- c("remove four 0", "replace two +1 with +2", 
                 "add one +2") %>%
  summarise_perk_stats(base_deck5)

summarise_perk_stats2(simulation6)

#pick remove four 0

#seventh perk----------
base_deck6 <- modify_deck(base_deck5, "remove four 0")

simulation7 <- c("replace two +1 with +2", "add one +2") %>%
  summarise_perk_stats(base_deck6)

summarise_perk_stats2(simulation7)

#pick replace two +1 with +2

#eighth perk-----------
base_deck7 <- modify_deck(base_deck6, "replace two +1 with +2")

simulation8 <- c("add one +2") %>%
  summarise_perk_stats(base_deck7)

summarise_perk_stats2(simulation8)

#pick add +2 card

#ninth perk is simply add +2 again