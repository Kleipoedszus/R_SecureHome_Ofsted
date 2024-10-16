# Load required libraries ####
library(tm)  # For text preprocessing
library(dplyr)
library(text2vec)  # For converting text to numerical features
library(caret)  # For scaling features
library(cluster)  # For clustering
library(tidyverse)
library(tokenizers)
library(tidytext)
library(parallel)
library(udpipe)
library(ggthemes)
library(ggplot2)
library(ldatuning)
library(tidytext)
library(tibble)
library(stringr)

#Set Options
options(stringsAsFactors = F)
Sys.setlocale('LC_ALL', 'C')
numcores <- detectCores()

# Load language model for annotations
udmodel_english <- udpipe_load_model(file = 
                                       "/Users/kleie/Library/CloudStorage/OneDrive-BournemouthUniversity/Projects/SecureHomeLondon/R_SecureHome_Ofsted/english-ud-2.0-170801.udpipe")

tidy_paragraphs <- readRDS(
  "/Users/kleie/Library/CloudStorage/OneDrive-BournemouthUniversity/Projects/SecureHomeLondon/R_SecureHome_Ofsted/SCH_tidy_pargraphs_ratings.rds")

# Select only reports of interest
#rating_check <- c("outstanding", "good")
URN_check <- c("SC035648", "SC042921", "SC040500", "SC022447", "SC022448", 
               "SC031490", "SC033362", "SC033457", "SC035409", "SC035500", 
               "SC036740", "SC038719", "SC046276", "SC046524")

df <- tidy_paragraphs
#df <- df %>% filter(rating_overall %in% rating_check)
df <- df %>% filter(URN %in% URN_check)
df <- df[!is.na(df$rating_overall), ]



strings_to_remove <- c("piccadilly", "logos", "archives", "secretary",
                       "nugent", "hatton", "liverpool", "edge", "higgins", 
                       "qualityimprovement", "sussex", "east", "lane", "lewes", "county", 
                       "elizabeth", "landsdowne", "hewitt", "peterborough", "cambridgeschire", 
                       "oglewelbourn", "city", "bridge", "leeds", "civic", "calverley", 
                       "george", "edingburgh", "northumberland", "tinkler", "morpeth", 
                       "macdonald", "karen", "gloucestershire", "south", "bristol", "skyes", 
                       "alison", "thornbury", "durham", "aycliffe", "selwyn", "royston", 
                       "nottinghamshire", "devon", "nottingham", "loughborough", "bridgford", 
                       "thomas", "west", "shelagh", "hampshire", "winchester", "kieran", "sophia", 
                       "salford", "swinton", "manchester", "chorley", "lowry", "lincolnshire",
                       "newland", "lincoln", "clarke", "devon", "exeter", "topsham", "lidicott", 
                       "sheffield", "pinstone", "williams", "kieran", "town", "young",
                       "people", "staff", "childrenes", "child", "good", "es", 
                       "young", "people", "children", "staff", "99>", ">s", "9f", ">.", "98>", 
                       ">", "a", "c", "Nm")

df$cleaned_text <- removeWords(df$cleaned_text, strings_to_remove)
# Remove words from keywords column
df$keywords <- removeWords(df$keywords, strings_to_remove)

# Step 1: Convert text data into numerical features ####
# I am using the reports to create the vocabulary
# Not sure, if I still need to remove the named entities
vocab <- create_vocabulary(itoken(df$cleaned_text))
vocab = prune_vocabulary(vocab, term_count_min = 2,
                         doc_proportion_max = 0.8, doc_proportion_min = 0.001)
vectorizer <- vocab_vectorizer(vocab)
dtm <- create_dtm(itoken(df$cleaned_text), vectorizer)
scaled_dtm <- scale(dtm)  # Scale the features for K-Means
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))
scaled_dtm[is.nan(scaled_dtm)] <- 0

# Remove any Na's or NaN values from the scaled dtm
# Identify rows without any NA's or NaN's
complete_cases <- complete.cases(scaled_dtm)
# Subset the scaled document term matrix, keeping only rows without missing values
scaled_dtm <- scaled_dtm[complete_cases, ]

library(lda)
library(topicmodels)
# cast into a Matrix object
corpus_words <- df %>%
  unnest_tokens(word, cleaned_text) %>%
  count(doc_id, word, sort = TRUE) %>%
  drop_na()

DTM <- corpus_words %>%
  cast_dtm(doc_id, word, n)

# Step 2: Find suitable number of clusters ####
# create models with different number of topics
result <- ldatuning::FindTopicsNumber(
  DTM,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("CaoJuan2009",  
              "Deveaud2014", 
              "Arun2010", 
              "Griffiths2004"),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)
FindTopicsNumber_plot(result)


# Using the kmeans_result from the previous code
#n <- 30
#wcss <- numeric(length = n)
#for (k in 1:n) {
#  kmeans_model <- kmeans(scaled_dtm, centers = k)
#  wcss[k] <- sum(kmeans_model$withinss)
#}
#plot(1:n, wcss, type = "b", xlab = "Number of Clusters", ylab = "WCSS")

# Step 3: Apply K-Means clustering ####
k <- 9  # Number of clusters (adjust as needed)
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(scaled_dtm, centers = k)

# Step 4: Add cluster labels to the dataframe ####
df$cluster <- as.factor(kmeans_result$cluster)
df$cluster <- as.integer(df$cluster)

# Show word frequencies by cluster ####
corpus_words <- df %>%
  unnest_tokens(word, cleaned_text) %>%
  count(cluster, word, sort = TRUE) %>%
  drop_na()

plot_freq <- corpus_words %>%
  group_by(cluster) %>%
  top_n(15, n) %>%
 mutate(word = reorder(word, n)) %>%
  arrange(desc(n)) %>%
  ungroup()

ggplot(plot_freq, aes(x=word, y=n))+
  geom_bar(stat="identity", fill='darkred') +
  coord_flip() + theme_gdocs()+
  geom_text(aes(label=n), colour="white",hjust=1.25, size=5.0)+
  facet_wrap(~ cluster, 
             ncol = 3, 
             scales = "free") +
  ggtitle(paste("Most frequent words for each cluster (",paste(URN_check, collapse = ", "), ")", 
                sep = ""))

# Show tf-idf for words for each cluster
corpus_words <- corpus_words %>%
  bind_tf_idf(word, cluster, n)

plot_freq <- corpus_words %>%
  group_by(cluster) %>%
  top_n(10, tf_idf) %>%
  mutate(word = reorder(word, tf_idf))

ggplot(plot_freq, aes(x=word, y=tf_idf))+
  geom_bar(stat="identity", fill='darkblue') +
  coord_flip() + theme_gdocs()+
  facet_wrap(~ cluster, 
             ncol = 3,  
             scales = "free") +
  ggtitle(paste("Highest tf-idf words for each cluster (", paste(URN_check, collapse = ", "), ")", 
                sep = ""))


## Co-location and co-occurrences ####
# Annotate reports
df <- subset(df,!is.na(cluster))
x <- udpipe_annotate(udmodel_english, 
                     x = df$cleaned_text, 
                     doc_id = df$cluster, 
                     parser = "default", 
                     trace = FALSE)
x <- as.data.frame(x)

# Create a function
library(igraph)
library(ggraph)

create_wordnetwork <- function(n, pattern) {
  x <- x %>%
    filter(doc_id %in% pattern)
  #x <- filter(x, doc_id == rating_check)
  stats <- cooccurrence(x = x$lemma, 
                        relevant = x$upos %in% c("NOUN", "ADJ"), skipgram = 2)
  wordnetwork <- head(stats, n)
  wordnetwork <- graph_from_data_frame(wordnetwork)
  ggraph(wordnetwork, layout = "fr") +
    geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
    geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
    theme_graph(base_family = "Arial Narrow") +
    theme(legend.position = "none") +
    labs(title = paste ("Cooccurrences within 3 words distance for Cluster ", pattern),
         subtitle = paste(URN_check, collapse = ", ")) #add subtitle
}


# Create a loop for number of topics (k)

for (i in 1:k) {
  print(create_wordnetwork(100, i))
}

# Annotate clusters
# Get keywords for each cluster ####
## Bring all text together
# Group by cluster number and concatenate text
clustered_text <- df %>%
  group_by(cluster) %>%
  summarize(cluster_text = paste(cleaned_text, collapse = ". "))  # Concatenate text for each cluster

RAKE_KW <- function(txt) {
  x <- udpipe_annotate(udmodel_english, x = txt)
  x <- as.data.frame(x)
  ## RAKE keywords
  stats <- keywords_rake(x = x, 
                         term = "lemma", group = c("doc_id", "sentence"),
                         relevant = x$upos %in% c("NOUN", "ADJ"),
                         ngram_max = 3, 
                         n_min = 1)
  stats <- subset(stats, ngram >2)
  stats <- subset(stats, rake>=0.8)
  stats <- stats %>%
    select(keyword) 
  stats <- paste(stats$keyword, collapse = ", ")
  return(stats)
} 

#clustered_text$keywords 
clustered_text$keywords <- mclapply(clustered_text$cluster_text, RAKE_KW, mc.cores = numcores)
clustered_text$keywords <- unlist(clustered_text$keywords)
clustered_text <- clustered_text %>%
  select(cluster, cluster_text, cluster_kw = keywords)

clustered_text$cluster_kw <- unlist(clustered_text$cluster_kw)
df <- merge(df, clustered_text, 
            by = "cluster")

# Save clustered and annotated dataframe for selected reports
filename <- paste("/Users/kleie/Library/CloudStorage/OneDrive-BournemouthUniversity/Projects/SecureHomeLondon/R_SecureHome_Ofsted/",
                  "Clusters for ", paste(URN_check, collapse = "_"),".rds", sep = "")
saveRDS(df, filename)

nvivo <- df %>%
  select(URN, doc_id, paragraph, cluster, rating_overall)


library(openxlsx)
filename <- paste("/Users/kleie/Library/CloudStorage/OneDrive-BournemouthUniversity/Projects/SecureHomeLondon/R_SecureHome_Ofsted/",
                  "Clusters for ", paste(URN_check, collapse = "_"),".xlsx", sep = "")
write.xlsx(nvivo, filename)