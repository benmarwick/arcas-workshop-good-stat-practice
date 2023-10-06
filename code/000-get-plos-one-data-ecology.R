library(tidyverse)

#-----------------------------------------------------------------------
# get 1000 archaeology articles from PLOS ------------------------------

library(rplos)
library(rvest)

# scale up

# get DOIs for articles
plos_ecology <- searchplos(q='everything:"ecology"', 
                               fl=c('title', 'id', 'accepted_date'), 
                               fq='doc_type:full',
                               limit = 5000)

# get full text of articles at those DOIs, this takes a long time

plos_ecology_urls <- 
  plos_ecology$data %>% 
  mutate(url = str_c("http://doi.org/", id)) 

plos_ecology$data$full_text <- vector("list", length = nrow(plos_ecology_urls))

# do it 'safely' so if one URL gives an error it doesn't stop the entire sequence
get_and_squish <- 
  function(x) {
    x %>% read_html %>% 
      html_text %>% 
      str_squish
  }

get_and_squish_safely <- safely(get_and_squish)

# loop over all the URLs to get the full text of all the URLs, this takes a very long time

for(i in 1:nrow(plos_ecology_urls)){
  
  plos_ecology$data$full_text[[i]] <- 
    plos_ecology_urls$url[i]  %>% 
    get_and_squish_safely()
  
  # observe the progress
  print(i)
  
}

# save this so we don't have to scrape again
saveRDS(plos_ecology$data,
        "data/plos_ecology_data.rds")

# get p-values from the full text of these articles, takes a minute
plos_ecology_full_text_ps <- 
  plos_ecology$data %>% 
  mutate(ps = map(full_text, ~unlist(str_extract_all(.x, "p = .{6}|p < .{6}|p > .{6}")))) %>% 
  unnest_legacy(ps) 

# take a look at how tests are reported by grabbing the text immediately before the
# p-values, we don't need this for later, it can be skipped
plos_ecology_full_text_ps_test_reporting <- 
  plos_ecology$data %>% 
  mutate(test_reports = map(full_text, ~unlist(str_extract_all(.x, ".{0,50}p = |.{0,50}p < |.{0,50}p > ")))) %>% 
  unnest(test_reports)  %>% 
  select(-full_text)

# types of tests used
# χ2 = ,  chi-square = , X2 = 
# F =
# Shapiro-Wilk test W = , Shapiro-Wilk test W =
# t-test, t = , (t[ , (t( , t-value
# Kruskal-Wallis test H =, Kruskal Wallis chi2 , K-W χ2
# R = , r2 =
# Mann-Whitney U = , Mann-Whitney Rank Sum Test (U = 
# Wilcoxon rank sum test (W =
# ρ = , 
# Spearman’s rho = , rho = , Spearman's r =
# one-way ANOVA (F(
# ANCOVA
# Z = , z =
# A =
# D =
# Pearson's r , Pearson Chi2 
# Fisher’s Exact Test
# MI =
# Kolmogorov-Smirnov test (K-S)
# TOT
# T-test

# clean the p-values and treat inqualitites
plos_ecology_full_text_ps_clean <- 
  plos_ecology_full_text_ps %>% 
  mutate(p_value = case_when(
    str_detect(ps, "e")       ~  0.00001,
    str_detect(ps, "<0.0001") ~  0.00005,
    str_detect(ps, "< 0.0001") ~ 0.00005,
    str_detect(ps, "<0.001") ~   0.0005,
    str_detect(ps, "< 0.001") ~  0.0005,
    str_detect(ps, "< 0.05") ~   0.025,
    str_detect(ps, "<0.05") ~    0.025,
    str_detect(ps, "< 0.01") ~   0.005,
    str_detect(ps, "<0.01") ~    0.005,
    TRUE ~ parse_number(ps)
  )) 

# take a look and browse the table
plos_ecology_p_values_only <- 
  plos_ecology_full_text_ps_clean 

# save this so we don't have to clean again
write.csv(plos_ecology_p_values_only,
        "data/plos_ecology_p_values_only.csv")

