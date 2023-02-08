
library(tidyverse)

# import the PLOS p-values and metadata
plos_archaeology_p_values_only <- 
  read_csv("data/plos_archaeology_p_values_only.csv")

# how many p-values per paper?
p_values_per_paper <- 
  plos_archaeology_p_values_only %>% 
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
  plos_archaeology_p_values_only %>% 
    mutate(year = parse_number(str_sub(accepted_date, 1, 4))) %>% 
    group_by(year, id) %>% 
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
ggplot(plos_archaeology_p_values_only) +
  aes(p_value) +
  geom_histogram(bins = 100) +
  geom_vline(xintercept = 0.05, colour = "red") +
  annotate("text", x = 0.1, y = 200, label = "p = 0.05", colour = "red") +
  scale_x_continuous(limits = c(0, 1)) +
  ylim(0, 250) +
  theme_minimal()

p_values_zero_to_point_five_plot <- 
ggplot(plos_archaeology_p_values_only) +
  aes(p_value) +
  # stat_bin(geom="step", bins = 100) +
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

# change over time
plos_archaeology_full_text_ps_clean_group <- 
  plos_archaeology_p_values_only %>% 
  mutate(year = parse_number(str_extract(accepted_date, "\\d{1,4}"))) %>% 
  # group into 5 year 
  mutate(five_years = cut(year,seq(2010, 2022, 2))) 

ggplot(plos_archaeology_full_text_ps_clean_group %>% 
         filter( p_value > 0)) +
  aes(five_years,
      p_value  
      ) +
  geom_boxplot() +
  geom_quasirandom(size = 2,
                   alpha = 0.2) +
  scale_y_log10(labels = scales::label_comma(),
                limits = c(0.00001, 1),
                name = "p-values from PLOS One archaeology papers" )  +
  theme_minimal() +
  xlab("Year of publication")

plos_archaeology_full_text_ps_clean_group_anova <- 
plos_archaeology_full_text_ps_clean_group %>% 
  aov(p_value ~ five_years, data = .) 

plos_archaeology_full_text_ps_clean_group_anova %>% 
  broom::tidy()

plos_archaeology_full_text_ps_clean_group_anova %>% 
  TukeyHSD() %>% 
  broom::tidy() %>% 
  arrange(adj.p.value)

# end of PLOS --------------------------------------------------------

# JAS data

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
  annotate("text", 
           x = 0.15, 
           y = 500, 
           label = "p = 0.05", 
           colour = "red") +
  scale_x_continuous(limits = c(0, 1)) +
  ylim(0, 1000) +
  theme_minimal(base_size = 14)

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
  annotate("text", 
           x = 0.15, 
           y = 200, 
           label = "p = 0.05", 
           colour = "red") +
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













