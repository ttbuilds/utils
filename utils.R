library(tidyverse)

hit_chance <- function(atk, AC, adv = 0, disadv = 0, elven_acc = 0)
{
  min_roll_tohit <- AC - atk
  base_tohit <- max(min((21 - min_roll_tohit) / 20, 0.95), 0.05)
  adv_result <- 1 - (1 - base_tohit)^(2+elven_acc)
  disadv_result <- base_tohit^2
  result <- case_when(
    adv > 0 & disadv == 0 ~ adv_result,
    adv == 0 & disadv > 0 ~ disadv_result,
    TRUE                  ~ base_tohit
  )
  return(result)
}

at_least_one_success <- function(n_tries, success_chance)
{
  return(1 - (1 - success_chance)^n_tries)
}

crit_chance <- function(min_to_crit = 20, adv = 0, elven_acc = 0)
{
  base_chance <- (21 - min_to_crit) / 20
  result <- 1 - (1 - base_chance)^(1+adv+adv*elven_acc)
  return(result)
}

get_proficiency <- function(level){
  return(floor((level+3)/4) + 1)
}

baseline_ac <- function(level)
{
  return(11 + (level>=4) + (level>=8) + get_proficiency(level))  
}

check_save_chance <- function(bonus, DC, adv = 0, disadv = 0)
{
  min_roll_tosucceed <- DC - bonus
  base_chance <- max(min((21 - min_roll_tosucceed) / 20, 1.00), 0.00)
  adv_result <- 1 - (1 - base_chance)^2
  disadv_result <- base_chance^2
  result <- case_when(
    adv > 0 & disadv == 0 ~ adv_result,
    adv == 0 & disadv > 0 ~ disadv_result,
    TRUE                  ~ base_chance
  )
  return(result)  
}

mod <- function(ability_score)
{
  return(floor((ability_score - 10)/2))
}

parse_dice_expression <- function(dice_expression)
{
  result = list(
    n_dice = gsub("([0-9]+)d([0-9]+) ?\\+? ?([0-9]+)?", "\\1", dice_expression),
    d_type = gsub("([0-9]+)d([0-9]+) ?\\+? ?([0-9]+)?", "\\2", dice_expression),
    mod    = gsub("([0-9]+)d([0-9]+) ?\\+? ?([0-9]+)?", "\\3", dice_expression)
  )
  if(result$mod == "") result$mod = 0
  result <- lapply(result, as.numeric)
  return(result)
}

dice_avg <- function(dice_object)
{
  return(with(dice_object, n_dice * (d_type + 1) / 2))
}

static_dmg <- function(dice_object)
{
  return(dice_object$mod)
}

total_dmg <- function(dice_object)
{
  return(dice_avg(dice_object) + static_dmg(dice_object))
}

dpa <- function(to_hit, to_crit, dmg_on_hit)
{
  (to_hit + to_crit) * dice_avg(dmg_on_hit) + to_hit * static_dmg(dmg_on_hit)
}

first_hit_is_crit <- function(to_hit, to_crit, n_atks)
{
  if(n_atks == 1) return(to_crit)
  else return(to_crit + (1 - to_hit) * first_hit_is_crit(to_hit, to_crit, n_atks - 1))
}

bonus_dpt <- function(to_hit, to_crit, n_atks, bonus_dmg, crit_fish = TRUE)
{
  if(n_atks == 1 | crit_fish == FALSE) {
    base_result   <- at_least_one_success(n_atks, to_hit) * total_dmg(bonus_dmg)
    bonus_is_crit <- first_hit_is_crit(to_hit, to_crit, n_atks)
    result        <- base_result + bonus_is_crit * dice_avg(bonus_dmg)
  } else {
    greedy_dmg          <- total_dmg(bonus_dmg)
    future_expected_dpr <- bonus_dpt(to_hit, to_crit, n_atks - 1, bonus_dmg, crit_fish = TRUE)
    if(future_expected_dpr > greedy_dmg) print("Delay on hit")
    result <- 
      to_crit            * (dice_avg(bonus_dmg) + greedy_dmg) +
      (1 - to_hit)       * future_expected_dpr  +
      (to_hit - to_crit) * max(greedy_dmg, future_expected_dpr)
  }
  return(result)
}
