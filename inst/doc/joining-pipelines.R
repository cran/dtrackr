## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(dtrackr)

## -----------------------------------------------------------------------------



people = starwars %>% select(-films, -vehicles, -starships)
vehicles = starwars %>% select(name,vehicles) %>% unnest(cols = c(vehicles))
starships = starwars %>% select(name,starships) %>% unnest(cols = c(starships))
films = starwars %>% select(name,films) %>% unnest(cols = c(films))

tmp1 = people %>% track() %>% comment("People df {.total}")
tmp2 = films %>% track() %>% comment("Films df {.total}") %>% comment("a test comment")

tmp1 %>% inner_join(tmp2, by="name") %>% comment("joined {.total}") %>% flowchart()
# The join message is configurable but defaults to 
# {.count.lhs} on LHS
# {.count.rhs} on RHS
# {.count.out} in linked set


## -----------------------------------------------------------------------------

tmp = people %>% comment("start")

tmp1 = tmp %>% include_any(
  species == "Human" ~ "{.included} humans",
  species == "Droid" ~ "{.included} droids"
  )

tmp2 = tmp %>% include_any(
  species == "Gungan" ~ "{.included} gungans"
) %>% comment("{.count} gungans")

tmp3 = bind_rows(tmp1,tmp2) %>% comment("{.count} human,droids and gungans") 
tmp3 %>% flowchart()


