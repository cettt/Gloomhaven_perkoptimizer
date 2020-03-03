draw_att_mod <- function(deck, base_attack, base_deck){
  #this function draws cards from the attack modifier deck and shuffles it if necessary.
  
  n <- min(which(deck != "1r"))
  x <- deck[n] #drawn card after +1 rolling
  
  if (x == "miss") {
    deck <<- sample(base_deck, size = length(base_deck))
    return(n-1)
  }
  
  if (x == "2x") {
    deck <<- sample(base_deck, size = length(base_deck))
    return((base_attack + n - 1) * 2)
  } 
  
  deck <<- deck[-(1:n)]
  
  return(max(base_attack + (n-1) + as.numeric(x), 0))
}


calculate_perk_stats <- function(base_attack, base_deck, seed = 1108, N = 1e5){
  # this function simulates N draws from a given deck.
  set.seed(seed)
  deck <<- sample(base_deck, length(base_deck))
  draws <- replicate(N, draw_att_mod(deck, base_attack, base_deck))
  
  tibble(mean = mean(draws) - base_attack, sd = sd(draws))
  
}


modify_deck <- function(deck, modification){
  #this function modifys an attack modifier deck
  
  if (modification == "add two rolling +1") {
    return(c(deck, "1r", "1r"))
  } else if (modification == "add one +2"){
    return(c(deck, "2"))
  } else if (modification == "remove two -1"){
    n <- length(deck[deck == "-1"])
    deck0 <- deck[deck != "-1"]
    return(c(deck0, rep("-1", max(n - 2, 0))))
  } else if (modification == "remove four 0"){
    n <- length(deck[deck == "0"])
    deck0 <- deck[deck != "0"]
    return(c(deck0, rep("0", max(n - 4, 0))))
  } else if (modification == "replace two +1 with +2"){
    n1 <- length(deck[deck == "1"])
    n2 <- length(deck[deck == "2"])
    deck0 <- deck[deck != "1" & deck != "2"]
    return(c(deck0, rep("1", max(n1 -2, 0)), rep("2", n2 + min(n1, 2))))
  } else if (modification == "replace -2 with 0") {
    n1 <- length(deck[deck == "-2"])
    n2 <- length(deck[deck == "0"])
    deck0 <- deck[deck != "-2" & deck != "0"]
    return(c(deck0, rep("-2", max(n1 - 1, 0)), rep("0", n2 + min(n1, 1))))
  } else return(NULL)
  
}


summarise_perk_stats <- function(.modifications, .base_deck, .base_attack = 1:5){
  tibble(modification = .modifications) %>%
    mutate(base_deck = map(modification, modify_deck, deck = .base_deck)) %>%
    crossing(base_attack = .base_attack) %>% #adjust if necessary
    mutate(result = map2(base_attack, base_deck, calculate_perk_stats)) %>%
    unnest(result) %>%
    select(-base_deck) %>%
    pivot_wider(id_cols = base_attack, names_from = modification, values_from = c("mean", "sd")) %>%
    rename_at(-1, ~sub("^([^_]+)_(.*)$", "\\2_\\1", .x)) %>%
    select(sort(colnames(.))) %>%
    select(base_attack, everything())
  
}



summarise_perk_stats2 <- function(my_stats){
  my_stats %>%
    summarise_at(-1, mean) %>%
    ungroup() %>%
    pivot_longer(everything(), names_to = c("modification", ".value"), names_pattern = "([^_]*)_([^_]*)$") %>%
    mutate(SR = mean / sd) %>%  #use Sharpe ratio to decide for best perk
    arrange(-SR)
}