# Script to extract Ofsted ratings
### Set environment ####
# Load Libraries required
library(tm)
library(tidytext)
library(stringr)
library(tibble)
library(tidyverse)
library(parallel)
library(kableExtra)

#Set Options
options(stringsAsFactors = F)
Sys.setlocale('LC_ALL', 'C')
numcores <- detectCores()

# Read prepared datafiles from 1 Read_SCH_Reports.R ####
#x <- readRDS(
#  "/Users/kleie/Library/CloudStorage/OneDrive-BournemouthUniversity/Projects/SecureHomeLondon/R_SecureHome_Ofsted/x.RDS")
tidy_sentences <- readRDS(
  "/Users/kleie/Library/CloudStorage/OneDrive-BournemouthUniversity/Projects/SecureHomeLondon/R_SecureHome_Ofsted/SCH_tidy_sentences.RDS")
#tidy_corpus <- readRDS(
#  "/Users/kleie/Library/CloudStorage/OneDrive-BournemouthUniversity/Projects/SecureHomeLondon/R_SecureHome_Ofsted/TidyCorpus.RDS")
#tidy_reports <- readRDS(
#  "/Users/kleie/Library/CloudStorage/OneDrive-BournemouthUniversity/Projects/SecureHomeLondon/R_SecureHome_Ofsted/SCH_tidy_reports.RDS")
tidy_paragraphs <- readRDS(
  "/Users/kleie/Library/CloudStorage/OneDrive-BournemouthUniversity/Projects/SecureHomeLondon/R_SecureHome_Ofsted/SCH_tidy_paragraphs.RDS")
#tidy_corpus <- readRDS(
#  "/Users/kleie/Library/CloudStorage/OneDrive-BournemouthUniversity/Projects/SecureHomeLondon/R_SecureHome_Ofsted/SCH_tidy_corpus.RDS")

# Extract rating ####
df <- tidy_sentences

# Find ratings using grepl
# Identify if sentence has certain phrases
Rating_Progress <- c("offsted judge that it has.*", 
                    "ofsted judges that it has.*",
                    "ofsted judged that it has.*",
                    "the service is judged to be making.*", 
                    "at this interim inspection.*",
                    "outcomes in education are.*",
                    "overall levels of care.*",
                    "this monitoring visit.*") 

Rating_Overall <- c("overall quality rating is.*",
                    "this inspection Overall effectiveness.*",
                    "overall experiences and progress of children and young people.*",
                    "overall effectiveness is.*",
                    "overall effectiveness judgement outcome.*",
                    "overall quality rating is.*") 

Rating_Other <- c("quality of the care.*",
                  "childs health.*",
                  ".*at keeping children and young people safe and feeling safe",
                  "equality and diversity practice is.*",
                  "health care arrangements.*",
                  "education and learning.*",
                  "education and relaated learning.*",
                  "outcomes in education are.*",
                  "the impact and effectiveness of leaders and managers.*",
                  "outcomes for young people are.*", 
                  "outcomes for children are.*", 
                  "this monitoring visit.*") 

Rating_Safety <- c(".*at keeping children and young people safe and feeling safe") 


# Function to extract sentence with rating
extract_rating_sentence <- function(sentence, Rating_Phrase) {
  for (phrase in Rating_Phrase) {
    if (str_detect(sentence, regex(phrase, ignore_case = TRUE))) {
      return(str_match(sentence, phrase)[,1])
    }
  }
  return(NA)
}

# Apply function to dataframe
df$rating_overall <- mclapply(df$sentence, extract_rating_sentence, Rating_Overall, mc.cores = numcores)
df$rating_overall <- unlist(df$rating_overall)
df$rating_progress <- mclapply(df$sentence, extract_rating_sentence, Rating_Progress, mc.cores = numcores)
df$rating_progress <- unlist(df$rating_progress)
df$rating_other <- mclapply(df$sentence, extract_rating_sentence, Rating_Other, mc.cores = numcores)
df$rating_other <- unlist(df$rating_other)
df$rating_safety <- mclapply(df$sentence, extract_rating_sentence, Rating_Safety, mc.cores = numcores)
df$rating_safety <- unlist(df$rating_safety)

# Remove duplicates
df <- df %>%
  distinct(URN, doc_id, rating_overall, rating_progress, rating_safety, rating_other)


## Merge all rating sentences into one for each report ####
# Remove all NAs
df <- df[!with(df,is.na(rating_overall)
               & is.na(rating_progress)
               & is.na(rating_safety)
               & is.na(rating_other)
               ),]
# Remove NAs
df[c("rating_overall", "rating_progress", "rating_safety", "rating_other")][is.na(df[c("rating_overall", "rating_progress", "rating_safety", "rating_other")])] <- ""

# merge strings 
df <- df %>% 
  group_by(doc_id) %>% 
  mutate(rating_overall = paste0(rating_overall, collapse = " ")) %>%
  mutate(rating_progress = paste0(rating_progress, collapse = " ")) %>%
  mutate(rating_safety = paste0(rating_safety, collapse = " ")) %>%
  mutate(rating_other = paste0(rating_other, collapse = " ")) %>%
  distinct(URN, doc_id, rating_overall, rating_progress, rating_safety, rating_other)

# Clean the ratings columns ####
# Function to remove words not in the list
remove_unwanted_words <- function(text, pattern) {
  # Remove unwanted words from the text
  cleaned_text <- gsub(pattern, "", text, perl = TRUE)
  cleaned_text <- stripWhitespace(cleaned_text)
  cleaned_text <- removePunctuation(cleaned_text)
  return(cleaned_text)
}

# Function to remove duplicate words from text
remove_duplicates <- function(text) {
  # Split text into words
  words <- unlist(strsplit(text, "\\s+"))
  
  # Remove duplicate words
  unique_words <- unique(words)
  
  # Reconstruct text from unique words
  cleaned_text <- paste(unique_words, collapse = " ")
  
  return(cleaned_text)
}

# Apply the functions to each row of the dataframe
## Removal words overall
ratings <- c("outstanding", "good", "requires", "improvement", "satisfactory", 
             "sufficient", "adequate", "inadequate", "declined", "sustained", 
             "improved")
# Create a pattern to match words not in the list
pattern_to_remove <- paste0("\\b(?!(?:", paste(ratings, collapse = "|"), ")\\b)\\w+")

df$rating_overall <- mapply(remove_unwanted_words, df$rating_overall, pattern_to_remove)
df$rating_overall <- unlist(df$rating_overall)
df$rating_overall <- sapply(df$rating_overall, remove_duplicates)

## Removal words progress
ratings <- c("outstanding", "good", "requires", "improvement", "satisfactory", 
             "sufficient", "adequate", "inadequate", "declined", "sustained", 
             "improved", "progress", "serious", "failings")
# Create a pattern to match words not in the list
pattern_to_remove <- paste0("\\b(?!(?:", paste(ratings, collapse = "|"), ")\\b)\\w+")

df$rating_progress <- mapply(remove_unwanted_words, df$rating_progress, pattern_to_remove)
df$rating_progress <- unlist(df$rating_progress)
df$rating_progress <- sapply(df$rating_progress, remove_duplicates)
df$rating_safety <- mapply(remove_unwanted_words, df$rating_safety, pattern_to_remove)
df$rating_safety <- unlist(df$rating_safety)
df$rating_safety <- sapply(df$rating_safety, remove_duplicates)


# Returns string without leading white space
trim.leading <- function (x)  sub("^\\s+", "", x)
df$rating_overall <- trim.leading(df$rating_overall)
df$rating_progress <- trim.leading(df$rating_progress)
df$rating_safety <- trim.leading(df$rating_safety)
df$rating_other <- trim.leading(df$rating_other)

# Merge different labels for ratings
df$rating_overall <- df$rating_overall %>% 
  str_replace_all("^adequate", "requires improvement") %>% 
  str_replace_all("satisfactory", "requires improvement") %>%
  str_replace_all("requires improvement good", "requires improvement")

df$rating_progress <- df$rating_progress %>%
  str_replace_all("satisfactory progress", "satisfactory") %>%
  str_replace_all("inadequate progress", "inadequate") %>%
  str_replace_all("good progress", "good") %>% 
  str_replace_all("declined progress", "declined") %>%
  str_replace_all("progress serious", "")

# Replace empty strings with NA
df[df==""]<-NA

df$rating_overall <- as.factor(df$rating_overall)
df$rating_overall <- factor(df$rating_overall, levels = c("inadequate", "requires improvement", "good", "outstanding"))
df$rating_progress <- as.factor(df$rating_progress)
df$rating_progress <- factor(df$rating_progress, levels = c("serious failings","inadequate", "declined", "satisfactoy", "improved", "sustained", "good"))

# Merge extracted ratings with tidy_paragraphs
tidy_paragraphs <- merge(tidy_paragraphs, df, by = c("URN", "doc_id"), all = TRUE)

df <- tidy_paragraphs %>%
  distinct(URN, doc_id, rating_overall, rating_progress)

kbl(df) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# Save file
saveRDS(tidy_paragraphs, '/Users/kleie/Library/CloudStorage/OneDrive-BournemouthUniversity/Projects/SecureHomeLondon/SCH_tidy_pargraphs_ratings.rds')
write.csv(tidy_paragraphs,
          '/Users/kleie/Library/CloudStorage/OneDrive-BournemouthUniversity/Projects/SecureHomeLondon/R_SecureHome_Ofsted/SCH_tidy_pargraphs_ratings.csv')


