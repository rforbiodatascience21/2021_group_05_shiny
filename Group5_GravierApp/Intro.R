# Loading and formating the data
library("tidyverse")
library("fs") 
library("broom")
load(file = "gravier.RData")

gravier_data <- gravier$x %>% as.tibble() %>% 
  mutate(outcome = as.integer(gravier$y)-1)

gravier_data_long <- gravier_data %>% 
  pivot_longer(cols = -outcome, names_to = "gene", values_to = "log2_expr_level")

gravier_data_long_nested <- gravier_data_long %>%  
  group_by(gene) %>% 
  nest() %>%  ungroup()

# takes 100 random genes in the data set
set.seed(1234)
gravier_data_long_nested <- gravier_data_long_nested %>%  sample_n(100) 

gravier_data_long_nested <- gravier_data_long_nested %>%  mutate(mdl = map(data, ~ glm(outcome ~ log2_expr_level, 
                                                                                       data = .x, 
                                                                                       family = binomial(link = "logit"))))
gravier_data_long_nested <- gravier_data_long_nested %>%  
  mutate(mdl_tidy = map(mdl, tidy, conf.int = TRUE)) %>% 
  unnest(mdl_tidy)

gravier_data_long_nested <- gravier_data_long_nested %>% filter(term == "log2_expr_level")

gravier_data_long_nested <- gravier_data_long_nested %>%
  mutate(neg_log10_p = -log10(p.value))

saveRDS(gravier_data_long_nested, "gravier_data_long_nested.rds")
