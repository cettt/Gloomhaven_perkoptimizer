library(tidyverse)

source("perk_functions.R")

#first perk-------------
base_deck0 <- c("miss", "-2", rep("-1", 5), rep("0", 6), rep("1", 5), "2", "2x")

simulation1 <- c("remove two -1", "remove four 0", "replace two +1 with +2", 
                  "replace -2 with 0", "add one +2", "add two rolling +1") %>%
  summarise_perk_stats(base_deck0)


simulation1 %>% summarise_at(-1, mean) %>%
  ungroup() %>%
  pivot_longer(everything(), names_to = c("modification", ".value"), names_pattern = "([^_]*)_([^_]*)$") %>%
  mutate(SR = mean / sd) %>%  #use Sharpe ratio to decide for best perk
  arrange(-SR)


#pick remove two -1

#second perk----------------
base_deck1 <- modify_deck(base_deck0, "remove two -1")

simulation2 <- c("remove two -1", "remove four 0", "replace two +1 with +2", 
                 "replace -2 with 0", "add one +2", "add two rolling +1") %>%
  summarise_perk_stats(base_deck1)


simulation2 %>% summarise_at(-1, mean) %>%
  ungroup() %>%
  pivot_longer(everything(), names_to = c("modification", ".value"), names_pattern = "([^_]*)_([^_]*)$") %>%
  mutate(SR = mean / sd) %>%  #use Sharpe ratio to decide for best perk
  arrange(-SR)

# pick remove two -1


#third perk-------------
base_deck2 <- modify_deck(base_deck1, "remove two -1")

simulation3 <- c("remove four 0", "replace two +1 with +2", 
                 "replace -2 with 0", "add one +2", "add two rolling +1") %>%
  summarise_perk_stats(base_deck2)


simulation3 %>% summarise_at(-1, mean) %>%
  ungroup() %>%
  pivot_longer(everything(), names_to = c("modification", ".value"), names_pattern = "([^_]*)_([^_]*)$") %>%
  mutate(SR = mean / sd) %>%  #use Sharpe ratio to decide for best perk
  arrange(-SR)

#pick replace -2 with 0

# fouth perk--------------
base_deck3 <- modify_deck(base_deck2, "replace -2 with 0")

simulation4 <- c("remove four 0", "replace two +1 with +2", 
                  "add one +2", "add two rolling +1") %>%
  summarise_perk_stats(base_deck3)


simulation4 %>% summarise_at(-1, mean) %>%
  ungroup() %>%
  pivot_longer(everything(), names_to = c("modification", ".value"), names_pattern = "([^_]*)_([^_]*)$") %>%
  mutate(SR = mean / sd) %>%  #use Sharpe ratio to decide for best perk
  arrange(-SR)

#pick add two rolling +1

# fifth perk--------------
base_deck4 <- modify_deck(base_deck3, "add two rolling +1")

simulation5 <- c("remove four 0", "replace two +1 with +2", 
                 "add one +2", "add two rolling +1") %>%
  summarise_perk_stats(base_deck4)


simulation5 %>% summarise_at(-1, mean) %>%
  ungroup() %>%
  pivot_longer(everything(), names_to = c("modification", ".value"), names_pattern = "([^_]*)_([^_]*)$") %>%
  mutate(SR = mean / sd) %>%  #use Sharpe ratio to decide for best perk
  arrange(-SR)

#pick add two rolling +1

# sixth perk--------------
base_deck5 <- modify_deck(base_deck4, "add two rolling +1")

simulation6 <- c("remove four 0", "replace two +1 with +2", 
                 "add one +2") %>%
  summarise_perk_stats(base_deck5)


simulation6 %>% summarise_at(-1, mean) %>%
  ungroup() %>%
  pivot_longer(everything(), names_to = c("modification", ".value"), names_pattern = "([^_]*)_([^_]*)$") %>%
  mutate(SR = mean / sd) %>%  #use Sharpe ratio to decide for best perk
  arrange(-SR)

#pick remove four 0

#seventh perk----------
base_deck6 <- modify_deck(base_deck5, "remove four 0")

simulation7 <- c("replace two +1 with +2", "add one +2") %>%
  summarise_perk_stats(base_deck6)

simulation7 %>% summarise_at(-1, mean) %>%
  ungroup() %>%
  pivot_longer(everything(), names_to = c("modification", ".value"), names_pattern = "([^_]*)_([^_]*)$") %>%
  mutate(SR = mean / sd) %>%  #use Sharpe ratio to decide for best perk
  arrange(-SR)

#pick replace two +1 with +2

#eight perk-----------
base_deck7 <- modify_deck(base_deck6, "replace two +1 with +2")

simulation8 <- c("replace two +1 with +2", "add one +2") %>%
  summarise_perk_stats(base_deck7)

simulation8 %>% summarise_at(-1, mean) %>%
  ungroup() %>%
  pivot_longer(everything(), names_to = c("modification", ".value"), names_pattern = "([^_]*)_([^_]*)$") %>%
  mutate(SR = mean / sd) %>%  #use Sharpe ratio to decide for best perk
  arrange(-SR)

#pick add +2 card

#ninth perk is simply add +2 again