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

win_contest_pct <- function(
  bonus, 
  opposing, 
  win_ties = 0, 
  adv      = 0, disadv     = 0, 
  opp_adv  = 0, opp_disadv = 0)
{
  our_probs <- rep(0.05, 20)
  if(adv & !disadv) our_probs <- (39-2*(19:0))/400
  if(!adv & disadv) our_probs <- (39-2*(0:19))/400
  their_probs <- rep(0.05, 20)
  if(opp_adv & !opp_disadv) their_probs <- (39-19:0)/400
  if(!opp_adv & opp_disadv) their_probs <- (39-0:19)/400
  prob_matrix <- outer(our_probs, their_probs, "*")
  success_matrix <- outer(1:20+bonus, 1:20+opposing, `>`)
  win_pct <- sum(prob_matrix * success_matrix)
  return(win_pct)
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

dpa <- function(to_hit, to_crit, dmg_formula)
{
  dmg_on_hit = parse_dice_expression(dmg_formula)
  (to_hit + to_crit) * dice_avg(dmg_on_hit) + to_hit * static_dmg(dmg_on_hit)
}

first_hit_is_crit <- function(to_hit, to_crit, n_atks)
{
  if(n_atks == 1) return(to_crit)
  else return(to_crit + (1 - to_hit) * first_hit_is_crit(to_hit, to_crit, n_atks - 1))
}

bonus_dpt <- function(to_hit, to_crit, n_atks, dmg_formula, crit_fish = TRUE, max_uses = 1)
{
  if(max_uses == 0 | n_atks == 0) return(0)
  bonus_dmg = parse_dice_expression(dmg_formula)
  if(n_atks == 1 | crit_fish == FALSE) {
    base_result   <- at_least_one_success(n_atks, to_hit) * total_dmg(bonus_dmg)
    bonus_is_crit <- first_hit_is_crit(to_hit, to_crit, n_atks)
    result        <- base_result + bonus_is_crit * dice_avg(bonus_dmg)
  } else {
    greedy_dmg          <- 
      total_dmg(bonus_dmg) + 
      bonus_dpt(to_hit, to_crit, n_atks - 1, dmg_formula, crit_fish = TRUE, max_uses = max_uses-1)
    future_expected_dpr <- bonus_dpt(
      to_hit, to_crit, n_atks - 1, dmg_formula, crit_fish = TRUE, max_uses = max_uses)
    # if(future_expected_dpr > greedy_dmg) print("Delay on hit")
    result <- 
      to_crit            * 
          (dice_avg(bonus_dmg) + greedy_dmg + 
           bonus_dpt(to_hit, to_crit, n_atks-1, dmg_formula, crit_fish = TRUE, max_uses = max_uses-1)) +
      (1 - to_hit)       * future_expected_dpr  +
      (to_hit - to_crit) * max(greedy_dmg, future_expected_dpr)
  }
  return(result)
}

make_build_frame <- function(progression, label, feats, feat_at_first){
  build_table <- tibble(
    build_label   = label,
    level         = 1:20,
    class_level   = progression) %>%
    mutate(
      current_class       = gsub("([A-Za-z]+) ([0-9]+)", "\\1", class_level),
      current_class_level = gsub("([A-Za-z]+) ([0-9]+)", "\\2", class_level),
      artificer_level     = cumsum(current_class == "Artificer"),      
      barb_level          = cumsum(current_class == "Barbarian"),
      bard_level          = cumsum(current_class == "Bard"),      
      cleric_level        = cumsum(current_class == "Cleric"),
      druid_level         = cumsum(current_class == "Druid"),      
      fighter_level       = cumsum(current_class == "Fighter"),            
      monk_level          = cumsum(current_class == "Monk"),                  
      paladin_level       = cumsum(current_class == "Paladin"),                        
      ranger_level        = cumsum(current_class == "Ranger"),
      rogue_level         = cumsum(current_class == "Rogue"),
      sorc_level          = cumsum(current_class == "Sorcerer"),
      warlock_level       = cumsum(current_class == "Warlock"),
      wizard_level        = cumsum(current_class == "Wizard"),
      asi_level           = 
        (feat_at_first & level == 1) | current_class_level %in% c(4,8,12,16,19),
      asi_feat            = "",
      prof                = get_proficiency(level),
      default_ac    = baseline_ac(level),
      extra_attack  = 
        pmax(ranger_level, fighter_level, paladin_level, barb_level, monk_level) >= 5,
      action_surge  = fighter_level >= 2)  
  total_asis = with(build_table, sum(asi_level))
  build_table[build_table$asi_level, "asi_feat"] <- feats[1:total_asis]
  return(build_table)  
}