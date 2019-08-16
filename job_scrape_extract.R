### Scraping and extracting data science job listings for Boston
### In part adapted from Brian Ward in his work on scraping job data from Indeed

library(rvest)
library(xml2)
library(tidyverse)
library(qdap)
library(data.table)
library(tm)
library(stringi)

## web scraping ----

search <- c(
  "q=data+analyst&l=Boston,+MA",
  "q=data+scientist&l=Boston,+MA"
)
page_index <- seq(
  from = 0,
  to = 990,
  by = 10
)

full_df1 <- data.frame()
for (i in 1:length(search)) {
  first_page_url <- paste0(
    "https://www.indeed.com/jobs?",
    search[i]
  )
  for (i in page_index) {
    url <- paste0(
      first_page_url,
      "&start=",
      page_index[i]
    )
    try(page <- read_html(url))
    Sys.sleep(2)
    try(job_title <- page %>%
      html_nodes("div") %>%
      html_nodes(xpath = '//a[@data-tn-element="jobTitle"]') %>%
      html_attr("title"))
    try(company_name <- page %>%
      html_nodes("span") %>%
      html_nodes(xpath = '//*[@class="company"]') %>%
      html_text() %>%
      stri_trim_both())
    try(links <- page %>%
      html_nodes("div") %>%
      html_nodes(xpath = '//*[@data-tn-element="jobTitle"]') %>%
      html_attr("href"))
    job_description <- c()
    footer <- c()
    for (i in 1:length(links)) {
      try(url2 <- paste0(
        "https://indeed.com/",
        links[i]
      ))
      page2 <- read_html(url2)
      try(job_description[i] <- page2 %>%
        html_nodes("span") %>%
        html_nodes(xpath = '//*[@class="jobsearch-JobComponent-description  icl-u-xs-mt--md  "]') %>%
        html_text())
      try(footer[i] <- page2 %>%
        html_nodes("span") %>%
        html_nodes(xpath = '//*[@class="jobsearch-JobMetadataFooter"]') %>%
        html_text())
    }

    vectorList <- list(
      job_title,
      company_name,
      job_description,
      footer
    )
    maxlen <- max(lengths(vectorList))
    vectorList2 <- lapply(
      vectorList,
      function(lst) c(
          lst,
          rep(
            NA,
            maxlen - length(lst)
          )
        )
    )
    vectorList3 <- lapply(
      vectorList2,
      unlist
    )
    df <- as.data.frame(vectorList3)
    names(df) <- (c(
      "job_title",
      "company_name",
      "job_description",
      "footer"
    ))
    df$date_scraped <- lubridate::today()
    full_df1 <- rbind(full_df1, df)
  }
}

## save data for later
write.csv(full_df1, "C:/Users/unlim/Desktop/scrape_2019_08_11.csv")

## load data ----
postings1 <- fread("C:/Users/unlim/Desktop/scrape_2019_08_11.csv",
  encoding = "Latin-1",
  header = TRUE
)

postings1$V1 <- NULL
postings1 <- filter(postings1, !is.na(postings1$job_description))

# fix job titles, ignore warning
postings1 <- postings1 %>%
  separate(job_title, c("job_title"), ",")
## data cleaning ----

clean_vector <- function(vector) {
  vector <- tolower(vector)
  vector <- mgsub(c(" ml ", "machinelearning"), " machine learning ", vector)
  vector <- mgsub(" ai ", " artificial intelligence ", vector)
  vector <- mgsub("large data", "big data", vector)
  vector <- mgsub(" r ", " rprogramming", vector)
  vector <- mgsub(c("written", "writing"), " write ", vector)
  vector <- mgsub(" bi ", " business intelligence ", vector)
  vector <- mgsub(" aws", " amazon web services ", vector)
  vector <- mgsub("nlp", " natural language processing ", vector)
  vector <- mgsub("text analysis", " textual ", vector)
  vector <- mgsub("full-time", " fulltime ", vector)
  vector <- mgsub("part-time", " parttime ", vector)

  vector <- mgsub(c("not required", "not needed", "not necessar"), " unnecessar", vector)
  vector <- mgsub(c("electronic medical record", " emr "), " electronic medical records ", vector)
  vector <- mgsub(" py ", " python ", vector)
  vector <- mgsub("java script", " javascript ", vector)
  vector <- clean(vector)
  vector <- replace_abbreviation(vector)
  vector <- mgsub("[[:punct:][:blank:]]+", " ", vector)
  vector <- mgsub(c(" – ", " — ", "-"), " - ", vector)
  vector <- removePunctuation(vector)
  vector <- mgsub(c("00000"), "k ", vector)
  vector <- mgsub("&", " and ", vector)
  vector <- str_replace_all(vector, regex('\r\n|\n|\t|\r|,|/|<|>|\\.|[:space:]'), ' ')
  vector <- removeWords(vector, stopwords("en"))
  vector <- mgsub(c("’", "“", "”"), "", vector)
  vector <- tolower(vector) # just in case
  vector <- str_squish(vector)
  return(vector)
}

# clean the job description strings
postings1$job_description <- clean_vector(postings1$job_description)
postings1 <- as.data.frame(postings1)
# preview text
postings1$job_description[5]

# pull education data from the job descriptions
words_bachelors <- c(" ba bs ", " bs", " ba", "bachelor")
words_masters <- c(" mba", " msc", " ms", " mba ", "master")
words_doctorate <- c(" phd ", "doctor")

for (i in 1:nrow(postings1)) {
  bach <- c()
  for (word in words_bachelors) {
    # start with bachelors
    bach1 <- str_detect(
      postings1$job_description[[i]],
      word
    )
    bach <- c(
      bach,
      bach1
    )
  }
  mast <- c()
  for (word in words_masters) {
    # start with bachelors
    mast1 <- str_detect(
      postings1$job_description[[i]],
      word
    )
    mast <- c(
      mast,
      mast1
    )
  }
  doc <- c()
  for (word in words_doctorate) {
    # start with bachelors
    doc1 <- str_detect(
      postings1$job_description[[i]],
      word
    )
    doc <- c(
      doc,
      doc1
    )
  }

  postings1$bachelors[[i]] <- ifelse(sum(bach) > 1, TRUE, FALSE)
  postings1$masters[[i]] <- ifelse(sum(mast) > 1, TRUE, FALSE)
  postings1$doctorate[[i]] <- ifelse(sum(doc) > 1, TRUE, FALSE)
}

postings1[
  which(postings1$doctorate == TRUE),
  "min_ed"
] <- "doctorate"
postings1[
  which(postings1$masters == TRUE),
  "min_ed"
] <- "masters"
postings1[
  which(postings1$bachelors == TRUE),
  "min_ed"
] <- "bachelors"

# clean strings from job title

postings1$job_title <- clean_vector(postings1$job_title)

# remove redundancies from titles, ignore warning
postings1 <- postings1 %>%
  separate(job_title, c("job_title"), " - ")
postings1$job_title <- str_squish(postings1$job_title)

# remove words with less than 3 characters
postings1$job_title <- rm_nchar_words(postings1$job_title, "1, 2")
postings1$job_description <- rm_nchar_words(postings1$job_description, "1, 2")


# filter jobs based on my preferences
postings1 <- postings1 %>%
  filter(!str_detect(job_title, "sr|senior|lead|finance|bank|intern|advertising|industrial|pharma|telecom|marketing|sports|consumer|supply chain|hotel"), !str_detect(job_description, "finance|investment|bank|intern|advertising|industrial|pharma|telecom|marketing|sports|consumer|supply chain|hotel"))

# classify jobs
postings1[
  str_which(
    postings1$job_title,
    "analy"
  ),
  "title_term"
] <- "analyst"

postings1[
  str_which(
    postings1$job_title,
    "scien"
  ),
  "title_term"
] <- "science"

postings1[
  str_which(
    postings1$job_title,
    "engineer"
  ),
  "title_term"
] <- "engineer"

postings1[
  (which(is.na(postings1$title_term))),
  "title_term"
] <- "neither"

# create variable `days_ago` from the footer.
for (i in 1:nrow(postings1)) {
  var <- str_sub(postings1[i, "footer"],
    start = if_else((as.vector(str_locate(
      postings1[i, "footer"],
      " ago"
    ))[1] - 10) < 0, 0,
    (as.vector(str_locate(
      postings1[i, "footer"],
      " ago"
    ))[1] - 10)
    ),
    end = as.vector(str_locate(
      postings1[i, "footer"],
      " ago"
    ))[2]
  )
  var1 <- unlist(var)
  var2 <- str_split(var, "-")
  var3 <- unlist(var2)
  var4 <- var3[if_else(length(var3) < 2, 1, 2)]
  var5 <- str_trim(var4,
    side = "both"
  )
  postings1[i, "time_ago"] <- var5
}

# Convert `time_ago` to a `days_ago` Number
postings1[
  str_which(
    postings1$time_ago,
    "month"
  ),
  "days_ago"
] <- 30

postings1[
  str_which(
    postings1$time_ago,
    "hour"
  ),
  "days_ago"
] <- 1

# find the relevant rows
which_days <- str_which(
  postings1$time_ago,
  "day"
)

# assign the first number from each value
for (i in which_days) {
  postings1[i, "days_ago"] <- regmatches(
    postings1[i, "time_ago"],
    gregexpr(
      "[[:xdigit:]]+",
      postings1[i, "time_ago"]
    )
  )[[1]][1]
}

# create variable `min_exp` (minimum years of experience) from job descriptions
words_experience <- c(
  "0-1+ year",
  "0-2+ year",
  "0-3+ year",
  "0-4+ year",
  "0-5+ year",
  "0-1 year",
  "0-2 year",
  "0-3 year",
  "0-4 year",
  "0-5 year",
  "0 - 1+ year",
  "0 - 2+ year",
  "0 - 3+ year",
  "0 - 4+ year",
  "0 - 5+ year",
  "0 - 1 year",
  "0- 2 year",
  "0- 3 year",
  "0- 4 year",
  "0- 5 year",
  ### 1+
  " 1+ year",
  "1-2+ year",
  "1-3+ year",
  "1-4+ year",
  "1-5+ year",
  " 1 year",
  "1-2 year",
  "1-3 year",
  "1-4 year",
  "1-5 year",
  "1 - 2+ year",
  "1 - 3+ year",
  "1 - 4+ year",
  "1 - 5+ year",
  "1 - 2 year",
  "1 - 3 year",
  "1 - 4 year",
  "1 - 5 year",
  ### 2+
  " 2+ year",
  "2-3+ year",
  "2-4+ year",
  "2-5+ year",
  " 2 year",
  "2-3 year",
  "2-4 year",
  "2-5 year",
  "2 - 3+ year",
  "2 - 4+ year",
  "2 - 5+ year",
  "2 - 3 year",
  "2 - 4 year",
  "2 - 5 year",
  ### 3+
  " 3+ year",
  "3-4+ year",
  "3-5+ year",
  " 3 year",
  "3-4 year",
  "3-5 year",
  "3 - 4+ year",
  "3 - 5+ year",
  "3 - 4 year",
  "3 - 5 year",
  ### 4+
  " 4+ year",
  "4-5+ year",
  " 4 year",
  "4-5 year",
  "4 - 5+ year",
  "4 - 5 year",
  ### 5+
  " 5+ year",
  " 6+ year",
  " 7+ year",
  " 8+ year",
  " 9+ year",
  " 10+ year"
)

for (i in 1:nrow(postings1)) {
  for (term in words_experience) {
    if (str_detect(
      postings1$job_description[[i]],
      term
    )) {
      postings1$exp[i] <- term
    }
    else {
      next
    }
  }
}

# pull out minimums
postings1$exp <- Trim(postings1$exp)

for (i in 1:nrow(postings1)) {
  postings1$min_exp[i] <- str_sub(postings1$exp[i], 1, 1)
}

jobs_field <- c(
  "psych",
  "health",
  "startup",
  "engineering",
  "biostatistics",
  "bioinformatics",
  "physics",
  "economics",
  "computer science",
  "transportation",
  "consulting",
  "education"
)

skills_hard <- c(
  "coding",
  "neural",
  "natural language processing",
  "deep learning",
  "artificial intelligence",
  "visualiz",
  " math",
  "research",
  "statistic",
  "machine learning",
  "advanced",
  "reporting",
  "wrangl",
  "algebr",
  "hypoth",
  "risk ",
  "business intelligence",
  "cloud",
  "testing",
  "image process",
  "textual",
  "model",
  "time series",
  "stream",
  "mining",
  "assess",
  "evaluat",
  "instruct",
  "curric",
  "server",
  "collect",
  "program",
  "transform",
  "automat",
  "workflow",
  "processing",
  "clean",
  "forecast",
  "predict",
  "classificat",
  "dashboard",
  "algorithm",
  "extrac",
  "regress",
  "network",
  "big data",
  "write"
)

skills_soft <- c(
  "multitask",
  "communicat",
  "collaborat",
  "team",
  "critical think",
  "learn",
  "creativ",
  "train",
  "facilitat",
  "present",
  "mentor",
  "insight",
  "managing",
  "plan",
  "translat",
  "strateg",
  "leadership",
  "problem solv",
  "coach",
  "curious",
  "intellect",
  "quick",
  "passion",
  "motivat",
  "vision",
  "design",
  "disciplin",
  "detail",
  "organized",
  "meaning",
  "thinking",
  "priorit",
  "efficien",
  "listening",
  "adaptab",
  "independ",
  "supervis",
  "innovat"
)

skills_tools <- c(
  "python",
  "rprogramming",
  "tableau",
  "scala",
  "java",
  "sql",
  "excel",
  "powerpoint",
  "matlab",
  " sas ",
  "hadoop",
  "tensorflow",
  "hive",
  "spark",
  "scikit",
  "ruby",
  "javascript",
  "spss",
  "pandas",
  "keras",
  "numpy",
  "caffe ",
  "pytorch",
  "cassandra",
  "docker",
  " git ",
  "linux",
  " c++",
  "markdown",
  "latex",
  "unix",
  "css",
  " gis ",
  "stata",
  "shiny",
  "oracle",
  "graphml",
  "jupyter"
)

jobs_type <- c(
  "consultant",
  "fulltime",
  "parttime",
  "temp",
  "contract"
)

# create the columns

postings_field <- postings1
postings_hard <- postings1
postings_soft <- postings1
postings_tools <- postings1
postings_type <- postings1

for (h in jobs_field) {
  postings_field[paste0(h)] <- jobs_field[h]
}
for (h in skills_hard) {
  postings_hard[paste0(h)] <- skills_hard[h]
}
for (h in skills_soft) {
  postings_soft[paste0(h)] <- skills_soft[h]
}
for (h in skills_tools) {
  postings_tools[paste0(h)] <- skills_tools[h]
}
for (h in jobs_type) {
  postings_type[paste0(h)] <- jobs_type[h]
}

# fill them in
n <- nrow(postings1)

for (i in seq_len(n)) {
  for (j in names(postings_field)[16:27]) {
    postings_field[[j]][i] <- if_else(str_detect(
      postings1$job_description[i],
      j
    ), TRUE, FALSE)
  }
}

for (i in seq_len(n)) {
  for (j in names(postings_hard)[16:62]) {
    postings_hard[[j]][i] <- if_else(str_detect(
      postings1$job_description[i],
      j
    ), TRUE, FALSE)
  }
}

for (i in seq_len(n)) {
  for (j in names(postings_soft)[16:53]) {
    postings_soft[[j]][i] <- if_else(str_detect(
      postings1$job_description[i],
      j
    ), TRUE, FALSE)
  }
}

for (i in seq_len(n)) {
  for (j in names(postings_tools)[16:53]) {
    postings_tools[[j]][i] <- if_else(str_detect(
      postings1$job_description[i],
      j
    ), TRUE, FALSE)
  }
}

for (i in seq_len(n)) {
  for (j in names(postings_type)[16:20]) {
    postings_type[[j]][i] <- if_else(str_detect(
      postings1$job_description[i],
      j
    ), TRUE, FALSE)
  }
}

names(postings_field) <- Trim(names(postings_field))
names(postings_hard) <- Trim(names(postings_hard))
names(postings_soft) <- Trim(names(postings_soft))
names(postings_tools) <- Trim(names(postings_tools))
names(postings_type) <- Trim(names(postings_type))
postings_field[16:27] <- lapply(postings_field[16:27], as.logical)
postings_hard[16:62] <- lapply(postings_hard[16:62], as.logical)
postings_soft[16:53] <- lapply(postings_soft[16:53], as.logical)
postings_tools[16:53] <- lapply(postings_tools[16:53], as.logical)
postings_type[16:20] <- lapply(postings_type[16:20], as.logical)

# see how it worked
postings_field[16:27] %>% colSums(na.rm = TRUE)
postings_hard[16:62] %>% colSums(na.rm = TRUE)
postings_soft[16:53] %>% colSums(na.rm = TRUE)
postings_tools[16:53] %>% colSums(na.rm = TRUE)
postings_type[16:20] %>% colSums(na.rm = TRUE)
