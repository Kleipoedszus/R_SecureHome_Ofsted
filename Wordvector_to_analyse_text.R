### Using Wordvector to set flags to the text ####
# Create WordVector to find similar kind of words
library(text2vec)
lexicon <- paste(df$cleaned_text, collapse = " ")
# Create iterator over tokens
tokens <- space_tokenizer(lexicon)
# Create vocabulary. Terms will be unigrams (simple words).
it = itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)
vocab <- prune_vocabulary(vocab, term_count_min = 8L)
# Remove terms that do not carry much informational value from the vocabulary
# Removal List relates to frequently used words
#removal_list <- c("young", "people", "child", "good", "home", "ensure", "also", 
#                  "provide", "support", "work", "well", "need", "make", "use", 
#                  "service", "person", "report", "help", "plan", "take", "care")

# Remove rows containing words from the list in the 'Text' column
#vocab <- vocab %>%
#  filter(!grepl(paste(removal_list, collapse = "|"), term, ignore.case = TRUE))

# Use our vocabulary
vectorizer <- vocab_vectorizer(vocab)
# use window of 5 for context words
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)
# This has created a TCM matrix that can be factorized with the GloVe algorithm
# use a parallel stochastic gradient descent algorithm
# fit the model
glove = GlobalVectors$new(rank = 50, x_max = 10)
wv_main = glove$fit_transform(tcm, n_iter = 10, convergence_tol = 0.01, n_threads = 8)
# this produces word vectors
wv_context = glove$components
word_vectors = wv_main + t(wv_context)

# Create a function to find the nearest neighbours of key words
Find_neighbours <- function(word) {
  near_neighbour <- word_vectors[word, , drop = FALSE]
  cos_sim = sim2(x = word_vectors, y = near_neighbour, method = "cosine", norm = "l2")
  words <- head(sort(cos_sim[,1], decreasing = TRUE), 10)
  words <- paste(names(words), collapse = ",")
  target_words <- unlist(strsplit(words, ","))
  print(target_words)
  unlist(target_words)
  return(target_words)
}

# Explore word neighbours
target_words <- Find_neighbours("areas")














# Create a function to check if a sentence contains any of the target words
check_sentence_contains_word <- function(sentence) {
  any(str_detect(sentence, paste0("\\b", target_words, "\\b")))
}

# Apply the function to the dataframe and create a 'flag' column
# Flag sentence with relations to keywords

target_words <- c("areas", "area", "play", "facilities", "music", "vocational", "swimming", "sports", "pool")
tidy_paragraphs$facilities <- mclapply(tidy_paragraphs$cleaned_text, check_sentence_contains_word, mc.cores = numcores)
tidy_paragraphs$facilities <- unlist(tidy_paragraphs$facilities)


# Flag sentence with relating to staff
target_words <- Find_neighbours("manager")
target_words <- c("manager", "senior", "managers", "management", "leadership", "deputy")
tidy_paragraphs$management <- mclapply(tidy_paragraphs$cleaned_text, check_sentence_contains_word, mc.cores = numcores)
tidy_paragraphs$management <- unlist(tidy_paragraphs$management)


# Flag sentence with relating to education
target_words <- Find_neighbours("education")
target_words <- c("education", "learning", "development", "activities", "teaching", "attendance", "educational", "teachers", "lessons", "skills", "support")
tidy_paragraphs$education <- mclapply(tidy_paragraphs$cleaned_text, check_sentence_contains_word, mc.cores = numcores)
tidy_paragraphs$education <- unlist(tidy_paragraphs$education)


# Flag sentence with relating to health
target_words <- Find_neighbours("health")
target_words <- c("health", "mental", "substance", "emotional", "specialist", "adolescent", "psychological", "physical")
tidy_paragraphs$health <- mclapply(tidy_paragraphs$cleaned_text, check_sentence_contains_word, mc.cores = numcores)
tidy_paragraphs$health <- unlist(tidy_paragraphs$health)


# Flag sentence with relating to diet
target_words <- Find_neighbours("diet")
target_words <- c("diet", "healthy", "balanced", "eating", "nutritious", "exercise", "lifestyle", "lifestyles", "choice", "weight")
tidy_paragraphs$diet <- mclapply(tidy_paragraphs$cleaned_text, check_sentence_contains_word, mc.cores = numcores)
tidy_paragraphs$diet <- unlist(tidy_paragraphs$diet)

# Flag sentence with relating to diet
target_words <- Find_neighbours("restraint")
target_words <- c("restraint", "incidents", "incident", "intervention", "seaparation", "records", "restraints", "techniques", "diversion", "behaviour", "diffusion")
tidy_paragraphs$restraint <- mclapply(tidy_paragraphs$cleaned_text, check_sentence_contains_word, mc.cores = numcores)
tidy_paragraphs$restraint <- unlist(tidy_paragraphs$restraint)