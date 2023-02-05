
library(tidyverse)
library(statcheck) 

# load PLOS articles --------------------------------------------------------------
plos_archaeology_data_statcheck <- readRDS("data/plos_archaeology_data.rds")

# run statcheck
plos_statcheck_results <- 
  plos_archaeology_data_statcheck %>% 
  mutate(statcheck_results = map(full_text, statcheck))

plos_statcheck_results %>% 
  select(-full_text) %>%  
  unnest(statcheck_results) %>% View

# plot it
plos_statcheck_results_tbl <- 
plos_statcheck_results %>% 
  select(statcheck_results) %>% 
  unnest() 

class(plos_statcheck_results_tbl) <- c("statcheck", "data.frame")
plot(plos_statcheck_results_tbl, APAstyle = TRUE)

# load JAS articles -------------------------------------------------------------
full_text_jas_statcheck <- readRDS("analysis/data/full_text_jas.rds")

# run statcheck
jas_statcheck_results <- 
  full_text_jas_statcheck %>% 
  mutate(statcheck_results = map(full_text_jas, statcheck))

jas_statcheck_results %>% 
  select(-full_text_jas) %>%  
  unnest(statcheck_results) %>% View

# load PNAS articles -------------------------------------------------------------
full_text_pnas_statcheck <- 
  readRDS("data/full_text_pnas.rds")

# run statcheck
pnas_statcheck_results <- 
  full_text_pnas_statcheck %>% 
  mutate(statcheck_results = map(article_text, statcheck))

pnas_statcheck_results %>% 
  select(-article_text,
         - article_abstract) %>%  
  unnest(statcheck_results) %>% View
