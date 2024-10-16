### Set environment ####
# Load Libraries required
library(tm)
library(tidytext)
library(stringr)
library(tibble)
library(tidyverse)
library(text2vec)
library(parallel)
library(udpipe)
library(igraph)
library(ggraph)
library(ggplot2)
library(ggthemes)
library(plotrix)
library(wordcloud)
library(kableExtra)

#Set Options
options(stringsAsFactors = F)
Sys.setlocale('LC_ALL', 'C')
numcores <- detectCores()

# Load language model for annotations
udmodel_english <- udpipe_load_model(file = 
                                       "/Users/kleie/Library/CloudStorage/OneDrive-BournemouthUniversity/Projects/SecureHomeLondon/R_SecureHome_Ofsted/english-ud-2.0-170801.udpipe")

# Read prepared datafiles from 1 Read_SCH_Reports.R
tidy_paragraphs <- readRDS(
  '/Users/kleie/Library/CloudStorage/OneDrive-BournemouthUniversity/Projects/SecureHomeLondon/R_SecureHome_Ofsted/SCH_tidy_pargraphs_ratings.rds')

# Create a csv for an overview of the Ofsted Ratings (overall)
df <- tidy_paragraphs %>%
  select(URN, doc_id, rating_overall)
df <- subset(df,!is.na(rating_overall))
#write.csv(df,
#          '/Users/kleie/Library/CloudStorage/OneDrive-BournemouthUniversity/Projects/SecureHomeLondon/R_SecureHome_Ofsted/OfstedRatings.csv')

# Exploratory Data Analysis ####
## Create trajectory of overall rating by Institution ####
# Load required libraries
library(ggplot2)

# Convert Month_yr to Date format

#Create a dataframe that contains the trajectory of all reports
report_ratings <- tidy_paragraphs %>%
  select(URN, doc_id, Month_Yr, rating_overall, rating_progress) %>%
  distinct(URN, doc_id, Month_Yr, rating_overall, rating_progress)
df <- report_ratings %>%
  select(URN, Month_Yr, rating_overall)
df$Month_Yr <- as.Date(paste0(df$Month_Yr, "-01"))

#filtered_df <- df
filtered_df <- subset(df,!is.na(rating_overall)) # & (URN %in% urn))

# Create the plot
ggplot(filtered_df, aes(x = Month_Yr)) +
  geom_line(aes(y = rating_overall, color = URN, group = URN), linetype = "solid") +
  geom_point(aes(x = Month_Yr, y = rating_overall)) +
  labs(x = "Month and Year", y = "Rating", title = "Trajectory of Ratings by Institution",
       color = "URN") +
  scale_color_discrete(name = "URN") +
  facet_wrap(~ URN, scales = "free") +
  theme(legend.position = "none")


# Word Frequencies ####
# Select only reports with overall rating
df <- tidy_paragraphs %>%
  select(URN, doc_id, rating_overall, cleaned_text)
  
# Overall word frequencies for all reports with a rating ####
# Create table with word frequencies
corpus_words <- df %>%
  unnest_tokens(word, cleaned_text) %>%
  count(rating_overall, word, sort = TRUE) %>%
  drop_na()
# A document term matrix
dtm <- corpus_words %>%
  cast_dtm(rating_overall, word, n)
tdm <- as.TermDocumentMatrix(dtm)
dtm.m <- as.matrix(dtm)
tdm.m <- as.matrix(tdm)

dtm_tfidf <- corpus_words %>%
  cast_dtm(rating_overall, word, n, weighting = tm::weightTfIdf)
tdm_tfidf <- as.TermDocumentMatrix(dtm_tfidf)
dtm_tfidf.m <- as.matrix(dtm_tfidf)
tdm_tfidf.m <- as.matrix(tdm_tfidf)



# Word Sums for each rating
RatingSums$Words <- as_tibble(rowSums( dtm.m[,1:ncol(dtm.m)] ))
RatingSums <- unlist(RatingSums)
RatingSums[1:4]

# Plot frequencies
term.freq <- rowSums(tdm.m)
freq.df <- data.frame(word=names(term.freq), frequency=term.freq)
freq.df <- freq.df[order(freq.df[,2], decreasing = T),]
freq.df$word <- factor(freq.df$word, levels=unique(as.character(freq.df$word)))
frequent_terms <- freq.df %>%
  top_n(30) 
ggplot(frequent_terms, aes(x=word, y=frequency))+
  geom_bar(stat="identity", fill='darkred') +
  coord_flip() + theme_gdocs()+
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)+
  ggtitle("Most frequent words for all reports")

# Create a tf-idf comparison cloud for different ratings ####
comparison.cloud(tdm_tfidf.m, max.words = 500,
                 random.order=FALSE, title.size=1.0, rot.per=0.35, use.r.layout=FALSE,
                 colors=brewer.pal(ncol(tdm.m), "Dark2"))

# Create commonalities pyramid plot ####
# First between inadequate and outstanding
common.words <- subset(tdm.m, tdm.m[, 1] >0 & tdm.m[,4] >0)
common.words <- as.data.frame(common.words)
common.words <- select(common.words, inadequate, outstanding)

difference <- abs(common.words[,1] - common.words[,2])
common.words <- cbind(common.words, difference)
common.words <- common.words[order(common.words[,3], decreasing = TRUE),]
top25.df <- data.frame(x= common.words[1:25,1], y= common.words[1:25, 2], 
                       labels = rownames(common.words[1:25,]))
pyramid.plot(top25.df$x, top25.df$y, 
             labels=top25.df$labels, 
             gap=300, top.labels= c("Inadequate", "Words", "Outstanding"), 
             main = "Common Words in Inadequate and Outstanding", laxlab = NULL, raxlab = NULL, unit = NULL)


# Second between requires improvement and good
common.words <- subset(tdm.m, tdm.m[, 2] >0 & tdm.m[,3] >0)
common.words <- as.data.frame(common.words)
common.words <- select(common.words, 'requires improvement', good)

difference <- abs(common.words[,1] - common.words[,2])
common.words <- cbind(common.words, difference)
common.words <- common.words[order(common.words[,3], decreasing = TRUE),]
top25.df <- data.frame(x= common.words[1:25,1], y= common.words[1:25, 2], 
                       labels = rownames(common.words[1:25,]))
pyramid.plot(top25.df$x, top25.df$y, 
             labels=top25.df$labels, 
             gap=1600, top.labels= c("Requires Improvement", "Words", "Good"), 
             main = "Common Words in Requires Improvement and Good", laxlab = NULL, raxlab = NULL, unit = NULL)

# Third between good and outstanding
common.words <- subset(tdm.m, tdm.m[, 3] >0 & tdm.m[,4] >0)
common.words <- as.data.frame(common.words)
common.words <- select(common.words, good, outstanding)

difference <- abs(common.words[,1] - common.words[,2])
common.words <- cbind(common.words, difference)
common.words <- common.words[order(common.words[,3], decreasing = TRUE),]
top25.df <- data.frame(x= common.words[1:25,1], y= common.words[1:25, 2], 
                       labels = rownames(common.words[1:25,]))
pyramid.plot(top25.df$x, top25.df$y, 
             labels=top25.df$labels, 
             gap=1600, top.labels= c("Good", "Words", "Outstanding"), 
             main = "Common Words in Good and Outstanding", laxlab = NULL, raxlab = NULL, unit = NULL)

# Fourth between inadequate and requiring improvement
common.words <- subset(tdm.m, tdm.m[, 1] >0 & tdm.m[,2] >0)
common.words <- as.data.frame(common.words)
common.words <- select(common.words, inadequate, 'requires improvement')

difference <- abs(common.words[,1] - common.words[,2])
common.words <- cbind(common.words, difference)
common.words <- common.words[order(common.words[,3], decreasing = TRUE),]
top25.df <- data.frame(x= common.words[1:25,1], y= common.words[1:25, 2], 
                       labels = rownames(common.words[1:25,]))
pyramid.plot(top25.df$x, top25.df$y, 
             labels=top25.df$labels, 
             gap=1600, top.labels= c("Inadquate", "Words", "Requires Improvement"), 
             main = "Common Words in Inadequate and Requiring Improvement", laxlab = NULL, raxlab = NULL, unit = NULL)

# The above is not that useful. Maybe we could merge RI with Inadequate and Good with Outstanding to compare. 

## Most frequent word by rating ####
# remove common words without surprises
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
                       "people", "staff", "childrenes", "child", "good")

df$cleaned_text <- removeWords(df$cleaned_text, strings_to_remove)

report_words <- df %>%
  unnest_tokens(word, cleaned_text) %>%
  count(rating_overall, word, sort = TRUE)

total_words <- report_words %>% 
  group_by(rating_overall) %>% 
  summarize(total = sum(n))

report_words <- left_join(report_words, total_words)
rm(total_words)

# Simple word frequency count 
report_words <- report_words[order(report_words$rating_overall, -report_words$n), ]
report_words %>%
  group_by(rating_overall) %>%
  top_n(15, n) %>%
  ungroup() %>%
  ggplot(aes(word, n, fill = rating_overall)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ rating_overall, scales = "free") +
  ylab("n") +
  coord_flip() +
  ggtitle("Simple word count by rating")

# by tf-idf
report_words <- report_words %>%
  bind_tf_idf(word, rating_overall, n)
plot_reports <- report_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(rating_overall, word = factor(word, levels = rev(unique(word))))

report_words %>%
  group_by(rating_overall) %>%
  top_n(10, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = rating_overall)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ rating_overall, scales = "free") +
  ylab("tf-idf") +
  coord_flip() +
  ggtitle("High tf_idf words by overall rating")

# Create Document Term Matrix and Term Document Matrix on Basis of URNs
# Words by URN
library(igraph)
library(ggraph)
# Initial focus focus of analysis on 
#urn <- c("SC035648", "SC042921", "SC040500")
df <- tidy_paragraphs %>%
  select(URN, Month_Yr, cleaned_text, rating_overall)
df$Month_Yr <- as.Date(paste0(df$Month_Yr, "-01"))

## Co-location and co-occurrences ####
# Annotate reports
df <- subset(tidy_paragraphs,!is.na(rating_overall))
x <- udpipe_annotate(udmodel_english, 
                     x = df$paragraph, 
                     doc_id = df$rating_overall, 
                     parser = "default", 
                     trace = FALSE)
x <- as.data.frame(x)

## Visualize most occurring nouns ####
library(lattice)
stats <- subset(x, upos %in% "NOUN")
report_words <- stats %>%
  count(doc_id, lemma, sort = TRUE)
report_words <- filter(report_words, lemma != "child")
report_words <- filter(report_words, lemma != "people")
report_words <- filter(report_words, lemma != "staff")
report_words <- filter(report_words, lemma != "home")
report_words <- filter(report_words, lemma != "99>")
report_words <- filter(report_words, lemma != "98>")
report_words <- filter(report_words, lemma != "h")
report_words <- filter(report_words, lemma != "Nm")
report_words <- filter(report_words, lemma != "b")
report_words <- filter(report_words, lemma != "9d")
total_words <- report_words %>% 
  group_by(doc_id) %>% 
  summarize(total = sum(n))

report_words <- left_join(report_words, total_words)
rm(total_words)

# Simple word frequency count 
report_words <- report_words[order(report_words$doc_id, -report_words$n), ]
report_words %>%
  group_by(doc_id) %>%
  top_n(15, n) %>%
  ungroup() %>%
  ggplot(aes(lemma, n, fill = doc_id)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ doc_id, scales = "free") +
  ylab("n") +
  coord_flip() +
  ggtitle("Most occurring nouns in Ofsted Reports by rating")

# by tf-idf
report_words <- report_words %>%
  bind_tf_idf(lemma, doc_id, n)
plot_reports <- report_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(doc_id, word = factor(lemma, levels = rev(unique(lemma))))

report_words %>%
  group_by(doc_id) %>%
  top_n(10, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(lemma, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = doc_id)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ doc_id, scales = "free") +
  ylab("tf-idf") +
  coord_flip() +
  ggtitle("High tf_idf nouns by overall rating")



## Visualise most occurring adjectives ####
stats <- subset(x, upos %in% "ADJ")
report_words <- stats %>%
  count(doc_id, lemma, sort = TRUE)
report_words <- filter(report_words, lemma != "young")
report_words <- filter(report_words, lemma != "good")
total_words <- report_words %>% 
  group_by(doc_id) %>% 
  summarize(total = sum(n))

report_words <- left_join(report_words, total_words)
rm(total_words)

# Simple word frequency count 
report_words <- report_words[order(report_words$doc_id, -report_words$n), ]
report_words %>%
  group_by(doc_id) %>%
  top_n(15, n) %>%
  ungroup() %>%
  ggplot(aes(lemma, n, fill = doc_id)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ doc_id, scales = "free") +
  ylab("n") +
  coord_flip() +
  ggtitle("Most occurring adjectives in Ofsted Reports by rating")

# by tf-idf
report_words <- report_words %>%
  bind_tf_idf(lemma, doc_id, n)
plot_reports <- report_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(doc_id, word = factor(lemma, levels = rev(unique(lemma))))

report_words %>%
  group_by(doc_id) %>%
  top_n(10, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(lemma, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = doc_id)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ doc_id, scales = "free") +
  ylab("tf-idf") +
  coord_flip() +
  ggtitle("High tf_idf adjectives by overall rating")


# Create the option to explore some words in context
library(flextable)
library(quanteda)
find_concordances <- function(strings_to_analyse) {
  # Convert the list of words to a regular expression pattern
  pattern <- paste0(strings_to_analyse, collapse = "|")
  kwic_test <- kwic(
    # define text
    df$cleaned_text, 
    pattern, 
    window = 5,
    valuetype = "regex")
  kwic_test <- as.data.frame(kwic_test)
  kwic_test <- select(kwic_test, pre, keyword, post)
  return(kwic_test)
}

backup <- df # Create a backup of the dataframe
df <- df %>%
  filter(rating_overall == "requires improvement")

# to explore the concordances of certain words, add words to the list strings_to_analyse
strings_to_analyse <- c("woman", "women")
concordances <- find_concordances(strings_to_analyse)
concordances %>%
  kbl() %>%
  kable_styling()



# Explore words in their context ####
# Create a function
create_wordnetwork <- function(n, rating_check) {
  x <- filter(x, doc_id == rating_check)
  stats <- cooccurrence(x = x$lemma, 
                        relevant = x$upos %in% c("NOUN", "ADJ"), skipgram = 2)
  wordnetwork <- head(stats, n)
  wordnetwork <- graph_from_data_frame(wordnetwork)
  ggraph(wordnetwork, layout = "fr") +
    geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
    geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
    theme_graph(base_family = "Arial Narrow") +
    theme(legend.position = "none") +
    labs(title = paste ("Cooccurrences within 3 words distance for", rating_check, "reports")) #add subtitle
}

create_wordnetwork(200, 'outstanding')
create_wordnetwork(200, 'good')
create_wordnetwork(200, 'requires improvement')
create_wordnetwork(200, 'inadequate')