
library(tidyverse)
library(statcheck) 

# load PLOS articles --------------------------------------------------------------
plos_archaeology_data_statcheck <- readRDS("data/plos_archaeology_data.rds")

# run statcheck
plos_statcheck_results <- 
  plos_archaeology_data_statcheck %>% 
  mutate(statcheck_results = map(full_text, statcheck))

# how many inconsistencies?
plos_statcheck_results %>% 
  select(-full_text) %>%  
  unnest(statcheck_results) %>% 
  select(
    title, error, decision_error
  ) %>% 
  pivot_longer(-title) %>% 
  filter(value) %>% 
  count(name)

# over how many articles?
plos_statcheck_results %>% 
  select(-full_text) %>%  
  unnest(statcheck_results) %>% 
  select(
    title, error, decision_error
  ) %>% 
  pivot_longer(-title) %>% 
  filter(value) %>% 
  group_by(name) %>% 
  summarise(titles = n_distinct(title))

plos_statcheck_results %>% 
  select(-full_text) %>%  
  unnest(statcheck_results) %>% 
  select(
    title, error, decision_error
  ) %>% 
  pivot_longer(-title) %>% 
  filter(value) %>% 
  arrange(title) %>% View

# plot it
plos_statcheck_results_tbl <- 
plos_statcheck_results %>% 
  select(title, statcheck_results) %>% 
  unnest() 

class(plos_statcheck_results_tbl) <- c("statcheck", "data.frame")
plot(plos_statcheck_results_tbl, 
     APAstyle = TRUE) +
  theme_minimal(base_size = 20)

ggsave(
  "docs/plos_archaeology_statcheck_results.png",
  dpi = 600,
  h = 10, 
  w = 15
)

# load JAS articles -------------------------------------------------------------
full_text_jas_statcheck <- readRDS("data/full_text_jas.rds")

# run statcheck
jas_statcheck_results <- 
  full_text_jas_statcheck %>% 
  mutate(statcheck_results = map(unlist(full_text_jas), statcheck))

jas_statcheck_results %>% 
  select(-full_text_jas) %>%  
  unnest(statcheck_results) %>% View

# plot it
jas_statcheck_results_tbl <- 
  jas_statcheck_results %>% 
  select(statcheck_results) %>% 
  unnest() 

class(jas_statcheck_results_tbl) <- c("statcheck", "data.frame")
plot(jas_statcheck_results_tbl, APAstyle = TRUE)

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
