library(readr)
openfoodfacts_nl <- read_csv("OtherWordSource/openfoodfacts_nl.csv")
View(openfoodfacts_nl)

library(tidyverse)
library(tidytext)

openfoodfacts_nl_word_list<- openfoodfacts_nl %>%
	unnest_tokens(word, product_name)
o_n_w_l_wordcount <- openfoodfacts_nl_word_list %>% count(word, sort = T)

o_n_w_l_wordcount_clean <- o_n_w_l_wordcount %>%
	filter(!is.na(word)) %>%
	filter(!str_detect(word, "[0-9]")) %>%
	filter(!word %in% stopwords::stopwords("nl"))

# We do have nutrition scores, but they are for products instead of words
# and there are a LOT of irrelevant words in the data set
# problem: some words are more related to health in the food context, but may not be the case in other contexts. (e.g., "bio")
# Decathlon provides a set of sports words (but formal?)
# what about non-sport words? (sit,...) SUBTLEX_NL_PoS %>% WW? Or can we ignore them? Do we really have unhealthy physical activity word?
