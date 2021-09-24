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