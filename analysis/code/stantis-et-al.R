# https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0123156

# The carbon and nitrogen isotopic results are displayed on Figs 3 and 4. 
# The carbon and nitrogen isotopic values of all individuals were strongly 
# correlated, r(39) = 0.50, p<0.001. The δ34S results in relation to the 
# carbon and nitrogen isotope results are plotted on Figs 5 and 6. 
# Sulfur and carbon were not correlated, r(34) = -0.75, p = 0.543, 
# nor were sulfur and nitrogen, r(34) = 0.03, p = 0.824. There was 
# a significant correlation between δ34S and %S wt, r(34) = 0.27, p = 0.028

library(tidyverse)
library(docxtractr)

stantis_data <- 
  read_docx("data/pone.0123156.s001.docx") %>% 
  docx_extract_tbl %>% 
  mutate(across(5:15, ~parse_number(.)))

#----------------------------------------------------------------------------
# carbon and nitrogen isotopic values of all individuals were strongly 
# correlated
with(stantis_data,
  cor.test(`δ13C`, `δ15N`)) # ok, same as in paper

#----------------------------------------------------------------------------
# "Sulfur and carbon were not correlated, r(34) = -0.75, p = 0.543"
with(stantis_data,
     cor.test(`δ34S`, `δ13C`)) 

# My computed result is different from paper, but decision is the same
# Major problem here is that the p-value for r(34) = -0.75 is 0.00000001, so the decision is different

# Pearson's product-moment correlation
# 
# data:  δ34S and δ13C
# t = 0.3597, df = 34, p-value = 0.7213
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.2724764  0.3823721
# sample estimates:
#        cor 
# 0.06157105 

#----------------------------------------------------------------------------
# "nor were sulfur and nitrogen, r(34) = 0.03, p = 0.824"
with(stantis_data,
     cor.test(`δ34S`, `δ15N`)) 

# My computed result is different from paper, but decision is the same

# Pearson's product-moment correlation
# 
# data:  δ34S and δ15N
# t = 1.6064, df = 34, p-value = 0.1174
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.06894802  0.54645590
# sample estimates:
#       cor 
# 0.2656042

#----------------------------------------------------------------------------
# "There was a significant correlation between δ34S and %S wt, r(34) = 0.27, p = 0.028"

with(stantis_data,
     cor.test(`δ34S`, `X.S`)) 

# My computed result is different from paper, but decision is the same
# Minor problem here that the p-value for r(34) = 0.27 is 0.01, , but decision is the same


#----------------------------------------------------------------------------
# "Regarding δ34S values...There were no significant differences 
# between the age cohorts, F(2,26) = 5.10, p = 0.180."
summary(aov(`δ34S` ~ Age, data = stantis_data))

# Df Sum Sq Mean Sq F value Pr(>F)
# Age          3  10.66   3.554   1.291  0.294
# Residuals   32  88.11   2.753               
# 9 observations deleted due to missingness

# My computed result is different from paper, but decision is the same
# Major problem here is that the p-value for F(2,26) = 5.10 is 0.001, , so the decision is different







