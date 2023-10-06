library(tidyverse)

impact_factors <- 
  read_csv("data/Archaeology_journal_IF_JCR_JournalResults_10_2023.csv",
           skip = 2) %>% 
  mutate(journal_name = str_to_title(`Journal name`))


impact_factors%>% 
  mutate(journal_name = str_to_title(`Journal name`)) %>% 
  filter(`2022 JIF` > 1) %>% 
  filter(`2022 JIF` != "N/A") %>% 
  drop_na(`2022 JIF`) %>% 
  mutate(jif = parse_number(`2022 JIF`)) %>% 
ggplot() +
  aes(reorder(journal_name, 
              jif),
      jif) +
  geom_col() +
  coord_flip() +
  labs(y = "Journal Impact Factor",
       x = "") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 15)) 

ggsave("docs/archaeology_jif_plot.png",
       h = 10,
       w = 6,
       bg ="white")



