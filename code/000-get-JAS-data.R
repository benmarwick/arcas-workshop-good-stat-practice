
#-----------------------------------------------------------------------
# get X000 archaeology articles from JAS ------------------------------

#  This only works on campus or with VPN
library(rscopus)
options("elsevier_api_key" = "053e6f8f2c1abe6fcdc943ee66759e5e")
have_api_key()

# get a bunch of DOIs for J. Arch. Sci
res = scopus_search(query = "ISSN(0305-4403)", 
                    max_count = 6000, # Total Entries are 6114
                    count = 25)
df = gen_entries_to_df(res$entries)
head(df$df)

full_text_jas <- vector("list", length = nrow(df$df))

library(httr)

# get the full text for each DOI
for(i in 1:nrow(df$df)){
  
  the_doi <- 
    paste0("https://api.elsevier.com/content/article/doi/",
           df$df$`prism:doi`[i], 
           "?APIKey=",
           "053e6f8f2c1abe6fcdc943ee66759e5e",
           "&httpAccept=text/plain")
  
  result <- GET(url = the_doi)
  
  full_text_jas[[i]] <- content(result)
  
  print(i)
  Sys.sleep(0.1)
  
}

ft_df <- 
  tibble(
    doi = df$df$`prism:doi`,
    full_text_jas = full_text_jas
  ) 

# save this so we don't have to scrape again
saveRDS(ft_df,
        "analysis/data/full_text_jas.rds")

ft_df <- readRDS("data/full_text_jas.rds")

# is everything ok in there?
ft_df %>% 
  mutate(nchars = map_int(full_text_jas, ~nchar(.x))) %>% 
  ggplot() +
  aes(nchars) +
  geom_histogram()

ft_df_clean <- 
  ft_df %>% 
  filter(map_lgl(full_text_jas, ~ .x %>%
                   is.character %>% 
                   all)) %>% 
  mutate(full_text_jas = map(full_text_jas, 
                             ~str_squish(str_to_lower(.x)))) %>% 
  mutate(ps = map(full_text_jas, 
                  ~unlist(str_extract_all(.x,
                                          "p *= .{6}|p *< .{6}|p *> .{6}"))))  %>% 
  unnest(ps)


# is the p-value reported exactly or with an inequi?
ft_df_clean_reporting <- 
ft_df_clean %>% 
  mutate(reporting = ifelse(str_detect(ps, "<|>"), "inequ", "exact"))

# take a look at how tests are reported by grabbing the text immediately before the
# p-values
jas_archaeology_full_text_ps_test_reporting <- 
  ft_df %>% 
  mutate(test_reports = map(full_text_jas, ~unlist(str_extract_all(str_squish(.x), ".{50}p = .{50}|.{50}p < .{50}|.{50}p > .{50}")))) %>% 
  unnest(test_reports)  %>% 
  select(-full_text_jas)

# clean the p-values and treat inqualitites
ft_df_ps_clean <- 
  ft_df_clean_reporting %>% 
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
  ))  %>% 
  select(-full_text_jas)

# save this so we don't have to clean again
write.csv(ft_df_ps_clean,
          "data/jas_p_values_only.csv")

# how many p-values per paper?
p_values_per_paper <- 
  ft_df_ps_clean %>% 
  count(doi) 
