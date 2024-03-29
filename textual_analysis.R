### After running the jobscraper.R script
### create a corpus for text mining

## create corpus ----

words <- Corpus(VectorSource(postings1$job_description))

clean_corpus <- function(corpus) {
  # convert to lower case
  corpus <- tm_map(
    corpus,
    content_transformer(tolower)
  )
  # convert hypens to spaces
  corpus <- tm_map(corpus,
                   content_transformer(gsub),
                   pattern = "-",
                   replace = " "
  )
  # remove words between < >
  corpus <- tm_map(corpus,
                   content_transformer(gsub),
                   pattern = "<[^[:space:]]*",
                   replace = ""
  )
  # remove numbers
  corpus <- tm_map(
    corpus,
    removeNumbers
  )
  # remove anything other than English letters or space
  corpus <- tm_map(corpus,
                   content_transformer(gsub),
                   pattern = "[^[:alpha:][:space:]]*",
                   replace = ""
  )
  # remove stopwords
  corpus <- tm_map(
    corpus,
    removeWords,
    c("the", "and", "yes", "etc", stopwords("english"))
  )
  # remove single letter words
  corpus <- tm_map(corpus,
                   content_transformer(gsub),
                   pattern = " . ",
                   replace = " "
  )
  # remove extra whitespace
  corpus <- tm_map(
    corpus,
    stripWhitespace
  )
  return(corpus)
}

clean_stem_corpus <- function(corpus) {
  # convert to lower case
  corpus <- tm_map(
    corpus,
    content_transformer(tolower)
  )
  # convert hypens to spaces
  corpus <- tm_map(corpus,
                   content_transformer(gsub),
                   pattern = "-",
                   replace = " "
  )
  # remove words between < >
  corpus <- tm_map(corpus,
                   content_transformer(gsub),
                   pattern = "<[^[:space:]]*",
                   replace = ""
  )
  # remove numbers
  corpus <- tm_map(
    corpus,
    removeNumbers
  )
  # remove anything other than English letters or space
  corpus <- tm_map(corpus,
                   content_transformer(gsub),
                   pattern = "[^[:alpha:][:space:]]*",
                   replace = ""
  )
  # remove stopwords
  corpus <- tm_map(
    corpus,
    removeWords,
    c("the", "and", "yes", "etc", stopwords("english"))
  )
  # remove single letter words
  corpus <- tm_map(corpus,
                   content_transformer(gsub),
                   pattern = " . ",
                   replace = " "
  )
  # remove extra whitespace
  corpus <- tm_map(
    corpus,
    stripWhitespace
  )
  # reduce words to basic forms
  corpus <- tm_map(
    corpus,
    stemDocument
  )
  return(corpus)
}

words <- clean_corpus(words)
stems <- clean_stem_corpus(words)

## create dtm ----

# document-term matrix (DTM) - lists all occurrences of words in corpus
words_dtm <- TermDocumentMatrix(words)
stems_dtm <- TermDocumentMatrix(stems)
rm(words, stems)

# convert dtm to matrix
words_m <- as.matrix(words_dtm)
stems_m <- as.matrix(stems_dtm)

# identify most frequent terms
words_freq <- data.frame(sort(rowSums(as.matrix(words_dtm)),
                              decreasing = T
))

stems_freq <- data.frame(sort(rowSums(as.matrix(stems_dtm)),
                              decreasing = T
))

words_freq <- rownames_to_column(words_freq,
                                 var = "term"
)

stems_freq <- rownames_to_column(stems_freq,
                                 var = "term"
)

names(words_freq)[2] <- "num"
names(stems_freq)[2] <- "num"

## correlations ----

# find associations/correlations with term "advanced"
findAssocs(words_dtm,
           terms = "unnecessary",
           corlimit = 0.2
)
findAssocs(words_dtm,
           terms = "reqire",
           corlimit = 0.2
)

x <- factor(with(postings1, paste(job_title, sep = "|")))
word_cor(postings1$job_description, x, "detail", .7)

# Multiple terms
words <- c("programming", "presentation")
with(postings1, word_cor(job_description, x, words, r = .8))
words <- c("programming", "presentation")
with(postings1, word_cor(job_description, x, words, r = .8))

dat <- wfm(postings1$job_description, x)
cor(t(dat)[, c("advanced", "programming", "presentation", "detail")])

with(postings1, word_cor(job_description, list(job_title), "advanced"))

# negative correlation
word_cor(postings1$job_description, x, "advanced", -.05)
