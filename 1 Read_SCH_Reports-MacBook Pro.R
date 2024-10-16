### This script reads in PDF documents of Ofsted reports, creates a corpus for textminging and tokenizes the corpus into sentences

### Set environment ####
# Load Libraries required
library(tm)
library(tidytext)
library(stringr)
library(tibble)
library(tidyverse)
library(parallel)
library(tokenizers)
library(udpipe)

# Load language model for annotations
udmodel_english <- udpipe_load_model(file = 
  "/Users/kleie/Library/CloudStorage/OneDrive-BournemouthUniversity/Projects/SecureHomeLondon/R_SecureHome_Ofsted/english-ud-2.0-170801.udpipe")

#Set Options
options(stringsAsFactors = F)
Sys.setlocale('LC_ALL', 'C')
numcores <- detectCores()

named_entities = c("ofsted", "inspector") #This list will later be extended after corpus was annotated. 

# Define functions ####
# Function to clean a corpus 
clean_corpus <- function(corpus_in) {
  corpus_in <- removeNumbers(corpus_in)
  corpus_in <- tolower(corpus_in)
  corpus_in <- removePunctuation(corpus_in)
  corpus_in <- removeWords(corpus_in, removalwords)
  corpus_in <- removeWords(corpus_in, named_entities)
  corpus_in <- removeWords(corpus_in, stopwords("en"))
  corpus_in <- gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", corpus_in)
  corpus_in <- stripWhitespace(corpus_in)
  corpus_in <- sub("^\\s+", "", corpus_in)
}

# Define a list of removal words
removalwords <- c("january", "february", "march", "april", 
                  "may", "june", "july", "august", "september", 
                  "october", "november", "december", "crown", "copyright", 
                  "licence", "framework", "inspection", "inspector", "page", 
                  "paragraph", "publication", "enquiriesofstedgovuk", 
                  "email", "psinternationalarchivesgsigovuk")

### Read in PDF's and create corpus ####
library(pdftools)
library(quanteda)

# Define file path
myfiles_path <- "/Volumes/MobileTB/OfstedReports210324"

# List PDF files
files <- list.files(myfiles_path, pattern = "\\.pdf$", 
                    full.names = TRUE, recursive = TRUE)

# Read PDFs and create corpus
# Read all PDFs and store the content in a vector
Rpdf <- readPDF(control = list(text = "-layout"))
corpus <- Corpus(URISource(files), 
                 readerControl = list(reader = Rpdf))
corpus <- tm_map(corpus, content_transformer(function(x) iconv(enc2utf8(x), sub = "byte")))

#corpus <- clean_corpus(corpus)

# Create a tidy corpus
library(stringi)
tidy_corpus <- tidy(corpus)
tidy_corpus <- tidy_corpus %>%
  select(datetimestamp, id, text)
tidy_corpus$text <- stri_trans_general(tidy_corpus$text, "latin-ascii")

# Some basic cleaning
# Remove strings like "9 of 10" from the text column
tidy_corpus$text <- gsub("\\b\\d+\\s+of\\s+\\d+\\b", "", tidy_corpus$text)
tidy_corpus$text <- gsub("Website: www.ofsted.gov.uk", "", tidy_corpus$text)
# Extract URN from the reports
tidy_corpus$URN <- str_extract(tidy_corpus$text, "\\bSC\\d{6}")
# Clean doc_id
tidy_corpus$id <- sub("\\.pdf$", "", tidy_corpus$id)
tidy_corpus <- tidy_corpus %>%
  select(URN, doc_id = id, datetimestamp, text)
# Save a copy 
saveRDS(tidy_corpus, 
        file = 
          "/Users/kleie/Library/CloudStorage/OneDrive-BournemouthUniversity/Projects/SecureHomeLondon/R_SecureHome_Ofsted/SCH_tidy_corpus.RDS")
write.csv(tidy_corpus,
          '/Users/kleie/Library/CloudStorage/OneDrive-BournemouthUniversity/Projects/SecureHomeLondon/R_SecureHome_Ofsted/SCH_tidy_corpus.csv')

tidy_corpus <- readRDS(
  "/Users/kleie/Library/CloudStorage/OneDrive-BournemouthUniversity/Projects/SecureHomeLondon/R_SecureHome_Ofsted/SCH_tidy_corpus.RDS")

# Annotate tidy_corpus ####
x <- udpipe_annotate(udmodel_english, 
                     x = tidy_corpus$text, 
                     doc_id = tidy_corpus$doc_id, 
                     parser = "default", 
                     trace = FALSE)
x <- as.data.frame(x)

# Prepare annotated data frame - Cleaning ####
# Include datetimestamp of reports in annotated table as needed later
df <- tidy_corpus %>%
  select(URN, doc_id, datetimestamp)
x <- merge(x, df, by = "doc_id", all = TRUE)
rm(df)

# Replace '???'
x$sentence <- gsub("\\?\\?\\?", "", x$sentence)

# Identify sentences that are highly repetitive
df <- x %>%  
  group_by(doc_id) %>% 
  distinct(sentence)
# Step 1: Count the number of documents each sentence occurs in
sentence_counts <- table(df$sentence)
# Step 2: Identify sentences that occur in more than n documents
n <- 50
strings_to_remove <- names(sentence_counts[sentence_counts > n])
# Step 3: Remove sentences that occur in more than n documents
x <- x[!x$sentence %in% strings_to_remove, ]

# remove dates
# Regular expression pattern to match dates in the format DD/MM/YYYY
date_pattern <- "\\b\\d{1,2}/\\d{1,2}/\\d{4}\\b"
# Identify sentences containing dates in the format DD/MM/YYYY
sentences_with_dates <- grep(date_pattern, x$sentence, value = TRUE)
# Remove sentences containing dates from the dataframe
x <- x[!grepl(date_pattern, x$sentence), ]

# Extract information for further cleaning
# Clean text further by removing named entities
named_entities <- subset(x, upos == "PROPN" & dep_rel == "root")
named_entities <- unique(named_entities$lemma)
named_entities <- tolower(named_entities)
print(named_entities)
# after review of list, define words to keep that may be of interest
words_to_keep <- c("adequate", "good", "inadequate", "runaway", "adolescence", 
                   "outstanding", "teacher", "week", "assessment", "group", "emotional", 
                   "st.???s", "health", "mental", "development", "education", "satisfactory", 
                   "security", "gymnastics", "cctv", "inspectio???s", "advocacy", "justice", 
                   "food", "leadership", "alcohol", "accomodation", "zumba", "enrichment", 
                   "inclusion", "terrorism", "safeguard", "autism", "incident", "critcial", 
                   "tools", "medicine", "bmx", 'pharmaceutical', "minority", "holocaust", 
                   "platinum", "arts", "personalise", "inspecti???s", "shakespeare", 
                   "participation", "football", "vacant", "diagnostic", "building", "better", 
                   "futures", "brighter", "zone", "tea", "teaching", "art", "award", "temporary", 
                   "neurone", "disease", "restraint", "crisis", "managements", "management", 
                   "delivery", "operational", "maintain", "business", "communication", "record", 
                   "information", "equality", "language", "lively", "cohesive", "runaway", "cse", 
                   "network", "gcse", "enthusiasm", "incentive", "teacher")

# Remove words from 'named_entities' that are present in 'words_to_keep'
named_entities <- named_entities[!named_entities %in% words_to_keep]
named_entities <- removePunctuation(named_entities)
# View the filtered named entities
print(named_entities)
# Save month and year
x$Month_Yr <- format(as.Date(x$datetimestamp), "%Y-%m")

# Create a corpus with paragraphs
tidy_paragraphs <- x %>%
  group_by(doc_id, paragraph_id) %>%
  distinct(sentence_id, sentence, .keep_all = TRUE) %>%
  mutate(paragraph = paste0(sentence, collapse = " ")) %>%
  distinct(doc_id, paragraph, .keep_all = TRUE) %>%
  ungroup() %>%
  select(URN, doc_id, Month_Yr, sentence_id, paragraph_id, paragraph)


# Remove strings the include technical information about the report####
# List of strings to check for
strings_to_remove <- c("This document may be reproduced in whole or in part for non-commercial educational purposes,",
                       "The inspection was carried out under the Care Standards Act 2000",
                       "This is in line with the Inspection", 
                       "Any complaints about the inspection or the report", 
                       "What the inspection judgements mean",
                       "The inspection judgements and what they mean",
                       "The inspection was carried out",
                       "The report details",
                       "Date of last inspection",
                       "Unique reference number", 
                       "You can obtain copies of The Children Act",
                       "The judgements included in the report are made", 
                       "Outstanding A service of exceptional quality that significantly exceeds minimum requirements",
                       "framework for inspection and the evaluation", 
                       "Crown copyright", 
                       "Inspection Report",
                       "Inspection report", 
                       "Regulation 12", 
                       "Standard Action Due date", 
                       "Type of inspect ion", 
                       "Type of inspection", 
                       "It regulates and inspects childcare and", 
                       "Inspector",
                       "Provision sub-s home", 
                       "Inspecs home", 
                       "Inspections home", 
                       "Inspection reps home", 
                       "Registered provider", 
                       "Inspection rs home:", 
                       "(Regulation 34 (2))",
                       "Registered person",
                       "If you would like Ofsted to send you a copy of the guidance", 
                       "Information about this inspection", 
                       "Provision subtype",
                       "Responsible individual",
                       "Secretary of State", 
                       "The care planning st", 
                       "in particular the regi", 
                       "The leadership and management standard", 
                       "(Regulation 32 (1) (e))", 
                       "Good: a service of high quality that exceeds minimum requirements Adequate: a service that only meets minimum requirements Inadequate: a service that does not meet minimum requirements")

# Remove rows containing any of the strings from the list
tidy_paragraphs <- tidy_paragraphs[!apply(sapply(strings_to_remove, grepl, 
                                                 x = tidy_paragraphs$paragraph, 
                                                 ignore.case = TRUE), 1, any), ]

# Clean text and keep original
tidy_paragraphs$cleaned_text <- mclapply(tidy_paragraphs$paragraph, clean_corpus, mc.cores = numcores)
tidy_paragraphs$cleaned_text <- unlist(tidy_paragraphs$cleaned_text)
# Use named entities to clean further
tidy_paragraphs$cleaned_text <- removeWords(tidy_paragraphs$cleaned_text, named_entities)
tidy_paragraphs$cleaned_text <- stripWhitespace(tidy_paragraphs$cleaned_text)

# A sentiment score of each paragraph ####
library(syuzhet)
tidy_paragraphs$syuzhet_sentiment <- get_sentiment(tidy_paragraphs$cleaned_text, method= "syuzhet")

# Create a corpus for reports
tidy_reports <- tidy_paragraphs %>%
  group_by(doc_id) %>%
  mutate(report = paste0(paragraph, collapse = " ")) %>%
  mutate(report_cleaned = paste0(cleaned_text, collapse = " ")) %>%
  distinct(doc_id, report, report_cleaned, .keep_all = TRUE) %>%
  ungroup() %>%
  select(URN, doc_id, Month_Yr, report, report_cleaned)

# Create Keywords for reports and paragraphs ####
# For comparatative keyword analysis 
# Function to RAKE Keywords from paragraphs
RAKE_KW <- function(txt) {
  x <- udpipe_annotate(udmodel_english, x = txt)
  x <- as.data.frame(x)
  ## RAKE keywords
  stats <- keywords_rake(x = x, 
                         term = "lemma", group = c("doc_id", "sentence"),
                         relevant = x$upos %in% c("NOUN", "VERB"),
                         ngram_max = 3, 
                         n_min = 1)
  stats <- subset(stats, ngram >1)
  stats <- stats %>%
    select(keyword) 
  stats <- paste(stats$keyword, collapse = ", ")
  return(stats)
} 

# Keywords for reports
tidy_reports$keywords <- mclapply(tidy_reports$report, RAKE_KW, mc.cores = numcores)
tidy_reports$keywords <- unlist(tidy_reports$keywords)

# Keywords for paragraphs
tidy_paragraphs$keywords <- mclapply(tidy_paragraphs$paragraph, RAKE_KW, mc.cores = numcores)
tidy_paragraphs$keywords <- unlist(tidy_paragraphs$keywords)

# Create a corpus with sentences
tidy_sentences <- tidy_paragraphs %>%
  unnest_tokens(sentence, paragraph, token = "sentences") %>%
  select(URN, doc_id, Month_Yr, sentence_id, sentence, keywords)

tidy_sentences$cleaned_text <- mclapply(tidy_sentences$sentence, clean_corpus, mc.cores = numcores)
tidy_sentences$cleaned_text <- unlist(tidy_sentences$cleaned_text)
# A sentiment analysis of each sentence ####
tidy_sentences$syuzhet_sentiment <- get_sentiment(tidy_sentences$cleaned_text, method= "syuzhet")

# Merge dataframes with contextual data ####
### Read contextual data from https://www.gov.uk/government/statistics/local-authority-and-childrens-homes-in-england-inspections-and-outcomes-autumn-2021
# A CSV file was created from this data to be imported here.
library(readr)
ofsted_data <- read_csv(
  "/Users/kleie/Library/CloudStorage/OneDrive-BournemouthUniversity/Projects/SecureHomeLondon/R_SecureHome_Ofsted/OfstedProvider_Level_at_310821.csv")
ofsted_data <- ofsted_data %>%
  select(URN, `Ofsted region`, `Local authority`, Sector, Places, `Organisation which owns the provider`)
# Merge with dataframes
x <- merge(x, ofsted_data, 
           by.x = "URN", 
           by.y = "URN", 
           all.x = TRUE)
tidy_paragraphs <- merge(tidy_paragraphs, ofsted_data, 
           by.x = "URN", 
           by.y = "URN", 
           all.x = TRUE)
tidy_sentences <- merge(tidy_sentences, ofsted_data, 
                         by.x = "URN", 
                         by.y = "URN", 
                         all.x = TRUE)
tidy_reports <- merge(tidy_reports, ofsted_data, 
                         by.x = "URN", 
                         by.y = "URN", 
                         all.x = TRUE)


# Save files for later analysis ####
saveRDS(tidy_sentences, file = 
          "/Users/kleie/Library/CloudStorage/OneDrive-BournemouthUniversity/Projects/SecureHomeLondon/R_SecureHome_Ofsted/SCH_tidy_sentences.RDS")
saveRDS(tidy_paragraphs, file = 
          "/Users/kleie/Library/CloudStorage/OneDrive-BournemouthUniversity/Projects/SecureHomeLondon/R_SecureHome_Ofsted/SCH_tidy_paragraphs.RDS")
saveRDS(tidy_reports, file = 
          "/Users/kleie/Library/CloudStorage/OneDrive-BournemouthUniversity/Projects/SecureHomeLondon/R_SecureHome_Ofsted/SCH_tidy_reports.RDS")
saveRDS(x, file = 
          "/Users/kleie/Library/CloudStorage/OneDrive-BournemouthUniversity/Projects/SecureHomeLondon/R_SecureHome_Ofsted/x.RDS")
