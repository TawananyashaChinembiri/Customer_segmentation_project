csd = read.csv('customer_segmentation_data.csv')

install.packages('tidyverse')

library(tidyverse)

na_free_csd <- csd %>% drop_na()
