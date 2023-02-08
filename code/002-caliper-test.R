# https://github.com/chartgerink/dissertation/blob/master/02-peerj_reanalyzing.Rmd

# only on exactly reported values

library(tidyverse)

ft_df_ps_clean <-  read_csv("data/jas_p_values_only.csv")

# following http://phd.chjh.nl/distributions-of-p-values-between-01-05-in-psychology-what-is-going-on.html
# .03875-.04] and (.04875-.05

# and https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4830257/
# their example is this: "For example, if 40 and 60 p-values are observed 
# in the intervals .04–.045 and .045–.05, respectively, then Pr = .6 
# and the binomial test results in p-value = .0284, suggesting evidence 
# for a bump below .05. "

confirming <- 
binom.test(40,
           100, 
           alternative = "less",
           p = 1/2)

ft_df_ps_clean_caliper <- 
  ft_df_ps_clean %>% 
  filter(reporting == "exact") %>% 
  mutate(in_caliper = ifelse(p_value %in% c(0.03875, 0.04000), "smaller", 
                             ifelse(p_value %in% c(0.04875, 0.05000), "larger", ""))) %>% 
  filter(in_caliper != "") %>% 
  count(in_caliper)

caliper_test_binom_test <- 
binom.test(ft_df_ps_clean_caliper$n[2],
           sum(ft_df_ps_clean_caliper$n), 
           p = 1/2,
           alternative = "less")

library(ggforce)

ft_df_ps_clean_zoom_plot_data <- 
ft_df_ps_clean %>% 
  filter(reporting == "exact") %>% 
  filter(p_value < 1,
         p_value > 0) 

# zoom facet
# from here: https://www.data-imaginist.com/2019/the-ggforce-awakens-again/
  ggplot() +
  geom_histogram(aes(p_value),
                 mutate(ft_df_ps_clean_zoom_plot_data,
                        z = FALSE),
                 bins = 30)  +
  geom_histogram(aes(p_value),
                 mutate(ft_df_ps_clean_zoom_plot_data,
                        z = TRUE),
                   bins = 400) +   
  facet_zoom(
    xlim = c(0, 0.1), 
    ylim = c(0, 600),
    zoom.data = z,
    horizontal = FALSE) +
  theme(zoom.y = element_blank(), 
        validate = FALSE) +
  scale_x_continuous(breaks = pretty) +
  theme_light(base_size = 18) +
  ylab("frequency") +
  xlab("p-value (exactly reported in the article text)")

ggsave("docs/jas_p_value_distribution.png",
       w = 14,
       h = 6,
       dpi = 600)

caliper_n <- unname(caliper_test_binom_test$parameter)
caliper_k <- unname(caliper_test_binom_test$statistic)
caliper_p <- round(unname(caliper_test_binom_test$p.value), 10)

df <- data.frame(
  x = 0.05,
  y = 140,
  label = paste0(  "N = ", caliper_n, 
                   ", K = ", caliper_k, 
                   ", p = ", knitr:::format_sci(caliper_p, 'html')  , "") 
)


library(ggtext)

ggplot(ft_df_ps_clean_zoom_plot_data) +
  aes(p_value) +
  geom_histogram(bins = 27)  +
  geom_segment(x = 0.05,
            xend = 0.05,
            y = 0,
            yend = 120,
             colour = "red") +
  xlim(0.03, 0.07) +
  ylim(0, 150) +
  theme_minimal() +
  ylab("frequency") +
  xlab("p-value (exactly reported in the article text)") +
  geom_richtext(
    data = df,
    aes(x, y, 
        label = label),
    fill = NA, 
    label.color = NA,
    size = 10
  ) +
  theme_light(base_size = 18) 

ggsave("docs/jas_p_value_caliper_test.png",
       w = 14,
       h = 6,
       dpi = 600)



