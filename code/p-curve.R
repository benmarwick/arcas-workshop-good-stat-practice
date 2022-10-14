library(tidyverse)

#-----------------------------------------------------------------------
# get 1000 archaeology articles from PLOS ------------------------------

library(rplos)

# scale up

# get DOIs for articles
plos_archaeology <- searchplos(q='everything:"archaeology"', 
                                       fl=c('title', 'id', 'accepted_date'), 
                                       fq='doc_type:full',
                               limit = 1000)

# get full text of articles at those DOIs, this takes a long time

plos_archaeology_urls <- 
  plos_archaeology$data %>% 
  mutate(url = str_c("http://doi.org/", id)) 

plos_archaeology$data$full_text <- vector("list", length = nrow(plos_archaeology_urls))

# do it 'safely' so if one URL gives an error it doesn't stop the entire sequence
get_and_squish <- 
  function(x) {
    x %>% read_html %>% 
    html_text %>% 
    str_squish
    }

get_and_squish_safely <- safely(get_and_squish)

# loop over all the URLs to get the full text of all the URLs, this takes a very long time

for(i in 1:nrow(plos_archaeology_urls)){
  
  plos_archaeology$data$full_text[[i]] <- 
    plos_archaeology_urls$url[i]  %>% 
    get_and_squish_safely()
  
  # observe the progress
  print(i)

}


# get p-values from the full text of these articles, takes a minute
plos_archaeology_full_text_ps <- 
  plos_archaeology$data %>% 
  mutate(ps = map(full_text, ~unlist(str_extract_all(.x, "p = .{6}|p < .{6}|p > .{6}")))) %>% 
  unnest(ps) 

# save this so we don't have to scrape again
saveRDS(plos_archaeology_full_text_ps,
        "data/plos_archaeology_data.rds")

# load it
plos_archaeology_data <- readRDS("data/plos_archaeology_data.rds")

# take a look at how tests are reported by grabbing the text immediately before the
# p-values
plos_archaeology_full_text_ps_test_reporting <- 
plos_archaeology_data %>% 
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
plos_archaeology_full_text_ps_clean <- 
  plos_archaeology_full_text_ps %>% 
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
plos_archaeology_full_text_ps_lite <- 
  plos_archaeology_full_text_ps_clean %>% 
  select(-full_text)

# how many p-values per paper?
p_values_per_paper <- 
plos_archaeology_full_text_ps_lite %>% 
  count(id) 

median_number_p_values <- median(p_values_per_paper$n)

  ggplot(p_values_per_paper) +
    aes(n) +
    geom_histogram() +
    geom_vline(xintercept = median_number_p_values, colour = "red") +
    annotate("text", 
             x = median_number_p_values + 12, 
             y = 50, 
             label = str_c("Median number of p-values is ", median_number_p_values),
             colour = "red") +
    ylab("number of p-values in the text of the paper") +
    theme_minimal()
  
# how about change over time?
  library(ggbeeswarm)
  plos_archaeology_full_text_ps_lite %>% 
    mutate(year = parse_number(str_sub(accepted_date, 1, 4))) %>% 
    group_by(year, id) %>% 
    count() %>% View
    ggplot() +
    aes(year, 
        n, 
        group = year) +
    geom_boxplot() +
    geom_quasirandom(alpha = 0.2) +
    scale_y_continuous(
      trans = "log2") +
    ylab("number of p-values in the text of the paper") +
    theme_minimal()
  

# plot distribution
p_values_zero_to_one_plot <- 
ggplot(plos_archaeology_full_text_ps_clean) +
  aes(p_value) +
  geom_histogram(bins = 100) +
  geom_vline(xintercept = 0.05, colour = "red") +
  annotate("text", x = 0.1, y = 200, label = "p = 0.05", colour = "red") +
  scale_x_continuous(limits = c(0, 1)) +
  ylim(0, 250) +
  theme_minimal()

p_values_zero_to_point_five_plot <- 
ggplot(plos_archaeology_full_text_ps_clean) +
  aes(p_value) +
  geom_histogram(bins = 100) +
  geom_vline(xintercept = c(0.005, 0.01, 0.05), colour = "red") +
  annotate("text", x = 0.0575, y = 80, label = "p = 0.05", colour = "red") +
  annotate("text", x = 0.0175, y = 80, label = "p = 0.01", colour = "red") +
  annotate("text", x = 0,      y = 80, label = "p =\n0.005", colour = "red") +
  scale_x_continuous(limits = c(0, 0.1),
                     breaks = seq(0, 0.1, 0.01)) +
  ylim(0, 100) +
  theme_minimal()

library(cowplot)
plot_grid(p_values_zero_to_one_plot, 
          p_values_zero_to_point_five_plot, 
          ncol = 1)  

#-----------------------------------------------------------------------
# get 1000 archaeology articles from JAS ------------------------------

#  This only works on campus or with VPN
library(rscopus)
options("elsevier_api_key" = "053e6f8f2c1abe6fcdc943ee66759e5e")
have_api_key()

# get a bunch of DOIs for J. Arch. Sci
res = scopus_search(query = "ISSN(0305-4403)", 
                    max_count = 5000,
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
Sys.sleep(2)

}

ft_df <- 
tibble(
  doi = df$df$`prism:doi`,
  full_text_jas = full_text_jas
) 

# save this so we don't have to scrape again
saveRDS(ft_df,
        "data/full_text_jas.rds")

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

# take a look at how tests are reported by grabbing the text immediately before the
# p-values
jas_archaeology_full_text_ps_test_reporting <- 
  ft_df %>% 
  mutate(test_reports = map(full_text_jas, ~unlist(str_extract_all(str_squish(.x), ".{50}p = .{50}|.{50}p < .{50}|.{50}p > .{50}")))) %>% 
  unnest(test_reports)  %>% 
  select(-full_text_jas)

# clean the p-values and treat inqualitites
ft_df_ps_clean <- 
  ft_df_clean %>% 
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

# how many p-values per paper?
p_values_per_paper <- 
  ft_df_ps_clean %>% 
  count(doi) 

median_number_p_values <- median(p_values_per_paper$n)

ggplot(p_values_per_paper) +
  aes(n) +
  geom_histogram() +
  geom_vline(xintercept = median_number_p_values, colour = "red") +
  annotate("text", 
           x = median_number_p_values + 12, 
           y = 20, 
           label = str_c("Median number of p-values is ", median_number_p_values),
           colour = "red") +
  ylab("number of p-values in the text of the paper") +
  theme_minimal()


# how about change over time?
library(ggbeeswarm)
ft_df_ps_clean %>% 
  mutate(year = str_remove(doi, "10.1016/j.jas.")) %>% 
  mutate(year = parse_number(str_sub(year, 1, 4))) %>% 
  filter(year > 1900) %>% 
  group_by(year, doi) %>% 
  count() %>% 
ggplot() +
  aes(year, 
      n, 
      group = year) +
  geom_boxplot() +
  geom_quasirandom(alpha = 0.2) +
  scale_y_continuous(
    trans = "log2") +
  ylab("number of p-values in the text of the paper") +
  theme_minimal()

# plot distribution
p_values_zero_to_one_plot <- 
  ggplot(ft_df_ps_clean) +
  aes(p_value) +
  geom_histogram(bins = 100) +
  geom_vline(xintercept = 0.05, colour = "red") +
  annotate("text", x = 0.1, y = 500, label = "p = 0.05", colour = "red") +
  scale_x_continuous(limits = c(0, 1)) +
  ylim(0, 1000) +
  theme_minimal()

p_values_zero_to_point_five_plot <- 
  ggplot(ft_df_ps_clean) +
  aes(p_value) +
  geom_histogram(bins = 100) +
  geom_vline(xintercept = c(0.005, 0.01, 0.05), colour = "red") +
  annotate("text", x = 0.0575, y = 500, label = "p = 0.05", colour = "red") +
  annotate("text", x = 0.0175, y = 500, label = "p = 0.01", colour = "red") +
  annotate("text", x = 0,      y = 500, label = "p =\n0.005", colour = "red") +
  scale_x_continuous(limits = c(0, 0.1),
                     breaks = seq(0, 0.1, 0.01)) +
  ylim(0, 1000) +
  theme_minimal()

library(cowplot)
plot_grid(p_values_zero_to_one_plot, 
          p_values_zero_to_point_five_plot, 
          ncol = 1)  

#-----------------------------------------------------------------------
# get 1000 archaeology articles from PNAS ------------------------------


library(tidyverse)
library(rvest)

search_url <- "https://www.pnas.org/action/doSearch?Concept=500376&Concept=500375&access=on&pageSize=2000&startPage=0"

# this takes a few seconds:
search_results_titles_and_dates_df <- 
  search_url %>% 
  read_html() %>% 
  # extract content from elements on the page that contain the article title and date
  html_nodes(".items-results .card__meta__date , .items-results .animation-underline") %>% 
  # convert to human-readable text
  html_text() %>% 
  # reshape into a data frame with title in one col and date in another col
  matrix(., ncol = 2, byrow = TRUE) %>% 
  tibble() %>% 
  # extract year from first column and convert from character to numeric
  mutate(year = parse_number(str_sub(.[,1],  -4)))

# how many papers per year?
search_results_titles_and_dates_df %>% 
  count(year) %>% 
  ggplot() +
  aes(year, n) +
  geom_col() +
  theme_minimal()

# let's get the URLs to each paper so we can get abstracts, etc.
# this takes a few seconds
search_results_urls_df <- 
  search_url %>% 
  read_html() %>% 
  # extract content from elements on the page that contain the article title link
  html_nodes(".items-results .animation-underline") %>% 
  # convert to human-readable text
  html_attr("href") %>% 
  # construct full URL from these results
  str_c("http://pnas.org", .)

# for each link to a paper, get the content type,title, abstract, 
# subject and date of that paper. 
# This takes quite a few minutes
search_results_abstract_title_date_subject <- 
  map(search_results_urls_df,
      ~.x %>% 
        read_html() %>% 
        html_nodes(".core-date-published span , h1 , .core-container > div:nth-child(1), #bodymatter div") %>% 
        # convert to human-readable text
        html_text() %>% 
        str_squish()
  ) 



# what's the structure?
# map(search_results_abstract_title_date_subject, str)

pnas_text_df <- 
tibble(
  article_type =       map_chr(search_results_abstract_title_date_subject, ~.[1]),
  article_title =      map_chr(search_results_abstract_title_date_subject, ~.[2]),
  article_date =       map_chr(search_results_abstract_title_date_subject, ~.[3]),
  article_abstract =   map_chr(search_results_abstract_title_date_subject, ~.[4]),
  article_text =       map_chr(search_results_abstract_title_date_subject, ~paste0(.[5:200], collapse = " "))
  ) 

# save this so we don't have to scrape again
saveRDS(pnas_text_df,
        "data/full_text_pnas.rds")

pnas_text_ps_df <- 
  readRDS("data/full_text_pnas.rds")
    
pnas_text_ps_df <- 
pnas_text_df %>% 
mutate(article_text = map(article_text, 
                           ~str_squish(str_to_lower(.x)))) %>% 
  mutate(ps = map(article_text, 
                  ~unlist(str_extract_all(.x,
                                          "p *= .{6}|p *< .{6}|p *> .{6}"))))  %>% 
  unnest(ps)
       

# clean the p-values and treat inqualitites
pnas_text_ps_clean <- 
  pnas_text_ps_df %>% 
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
  select(-article_text,
         -article_abstract)


# how many p-values per paper?
p_values_per_paper <- 
  pnas_text_ps_clean %>% 
  count(article_title) 

median_number_p_values <- median(p_values_per_paper$n)

ggplot(p_values_per_paper) +
  aes(n) +
  geom_histogram() +
  geom_vline(xintercept = median_number_p_values, colour = "red") +
  annotate("text", 
           x = median_number_p_values + 12, 
           y = 20, 
           label = str_c("Median number of p-values is ", median_number_p_values),
           colour = "red") +
  ylab("number of p-values in the text of the paper") +
  theme_minimal()


# how about change over time?
library(ggbeeswarm)
pnas_text_ps_clean %>% 
  mutate(year = parse_number(str_sub(article_date, -4, -1))) %>% 
  group_by(year, article_title) %>% 
  count() %>% 
  ggplot() +
  aes(year, 
      n, 
      group = year) +
  geom_boxplot() +
  geom_quasirandom(alpha = 0.2) +
  scale_y_continuous(
    trans = "log2") +
  ylab("number of p-values in the text of the paper") +
  theme_minimal()


# plot distribution
p_values_zero_to_one_plot <- 
  ggplot(pnas_text_ps_clean) +
  aes(p_value) +
  geom_histogram(bins = 100) +
  geom_vline(xintercept = 0.05, colour = "red") +
  annotate("text", x = 0.1, y = 200, label = "p = 0.05", colour = "red") +
  scale_x_continuous(limits = c(0, 1)) +
  ylim(0, 600) +
  theme_minimal()

p_values_zero_to_point_five_plot <- 
  ggplot(pnas_text_ps_clean) +
  aes(p_value) +
  geom_histogram(bins = 100) +
  geom_vline(xintercept = c(0.005, 0.01, 0.05), colour = "red") +
  annotate("text", x = 0.0575, y = 300, label = "p = 0.05", colour = "red") +
  annotate("text", x = 0.0175, y = 300, label = "p = 0.01", colour = "red") +
  annotate("text", x = 0,      y = 300, label = "p =\n0.005", colour = "red") +
  scale_x_continuous(limits = c(0, 0.1),
                     breaks = seq(0, 0.1, 0.01)) +
  ylim(0, 500) +
  theme_minimal()

library(cowplot)
plot_grid(p_values_zero_to_one_plot, 
          p_values_zero_to_point_five_plot, 
          ncol = 1)  













