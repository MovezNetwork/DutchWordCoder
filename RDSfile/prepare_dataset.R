# set.seed(20210301)
# all_words_random <- sample(SUBTLEX_NL$Word, size = nrow(SUBTLEX_NL))
# head(all_words_random)
#
# length(all_words_random)


all_word_index <- 1:1.6e4
set.seed(20210301)
overlap_index <- sample(all_word_index, size = length(all_word_index)/10)
non_overlap_index <- all_word_index[-overlap_index]

coder_list <- factor(1:4)
set.seed(20210303)
non_overlap_index_by_coder <- split(sample(non_overlap_index, length(non_overlap_index)), coder_list)

word_list_by_coder <- list()
for(i in 1:4){
	word_list_by_coder[[i]] <- c(overlap_index, non_overlap_index_by_coder[[i]])
	set.seed(i)
	word_list_by_coder[[i]] <- sample(word_list_by_coder[[i]], length(word_list_by_coder[[i]]))
}

df_by_coder <- list()
for(i in 1:4){
	df <- data.frame(
		word = SUBTLEX_NL_PoS$Word[word_list_by_coder[[i]]]
	)
	df$cate <- list(list())
	df$rate <- NA_character_
	df$note <- NA_character_
	df$saved <- FALSE
	attr(df, "coder_index") <- i
	attr(df, "assigned_time") <- Sys.time()
	df_by_coder[[i]] <- df

	saveRDS(df, paste0(i, ".RDS"))
}




set.seed(20210318)
word_list_trial <- sample(1:15000, 100)
df <- data.frame(
	word = SUBTLEX_NL_PoS$Word[word_list_trial]
)
df$cate <- list(list())
df$rate <- NA_character_
df$note <- NA_character_
df$saved <- FALSE
attr(df, "coder_index") <- "trial"
attr(df, "assigned_time") <- Sys.time()

saveRDS(df, "trial.RDS")

set.seed(20210322)
df_train <- read_csv("word_in_cate.txt")
word_list_train <- sample(1:nrow(df_train), nrow(df_train))
df <- data.frame(
	word = df_train$word[word_list_train]
)
df$cate <- list(list())
df$rate <- NA_character_
df$note <- NA_character_
df$saved <- FALSE
attr(df, "coder_index") <- "train"
attr(df, "assigned_time") <- Sys.time()

saveRDS(df, "train.RDS")
