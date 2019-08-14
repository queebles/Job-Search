### Scraping and extracting data science job listings for Boston
### In part adapted from Brian Ward in his work on scraping job data from Indeed

library(rvest)
library(xml2)
library(tidyverse)
library(qdap)
library(data.table)
library(tm)
library(viridis)

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
    try(page <- xml2::read_html(url))
    Sys.sleep(2)
    try(job_title <- page %>%
      rvest::html_nodes("div") %>%
      rvest::html_nodes(xpath = '//a[@data-tn-element = "jobTitle"]') %>%
      rvest::html_attr("title"))
    try(company_name <- page %>%
      rvest::html_nodes("span") %>%
      rvest::html_nodes(xpath = '//*[@class="company"]') %>%
      rvest::html_text() %>%
      stringi::stri_trim_both())
    try(job_location <- page %>%
      rvest::html_nodes("span") %>%
      rvest::html_nodes(xpath = '//*[@class="location"]') %>%
      rvest::html_text() %>%
      stringi::stri_trim_both())
    try(links <- page %>%
      rvest::html_nodes("div") %>%
      rvest::html_nodes(xpath = '//*[@data-tn-element="jobTitle"]') %>%
      rvest::html_attr("href"))
    job_description <- c()
    footer <- c()
    for (i in 1:length(links)) {
      try(url2 <- paste0(
        "https://indeed.com/",
        links[i]
      ))
      page2 <- xml2::read_html(url2)
      try(job_description[i] <- page2 %>%
        rvest::html_nodes("span") %>%
        rvest::html_nodes(xpath = '//*[@class = "jobsearch-JobComponent-description icl-u-xs-mt--md"]') %>%
        rvest::html_text() %>%
        stringi::stri_trim_both())
      try(footer[i] <- page2 %>%
        rvest::html_nodes("span") %>%
        rvest::html_nodes(xpath = '//*[@class="jobsearch-JobMetadataFooter"]') %>%
        rvest::html_text())
    }

    vectorList <- list(
      job_title,
      company_name,
      job_location,
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
      "job_location",
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
postings1$job_description <- tolower(postings1$job_description)

# pull education data from the job descriptions
words_bachelors <- c("ba/bs", " bs", " ba", "b.s.", "b.a.", "bachelors", "bachelor", "bachelor's", "b.a./b.s.")
words_masters <- c(" mba", "masters", " msc", " ms", "m.s.", "mba", "m.b.a.", "master's", "master")
words_doctorate <- c(
  "phd", "p.h.d.", "ph.d", "ph.d.", "ph.d", "phds", "ph.ds", "phd's", "ph.d's", "ph.d.'s",
  "doctorate", "doctoral", "doctorates", "doctorate's", "doctor", "doctors", "doctor's"
)

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

## data cleaning ----

clean_vector <- function(vector) {
  vector <- mgsub(c("machine learning"), " ml ", vector)
  vector <- mgsub(c("artificial intelligence"), " ai ", vector)
  vector <- mgsub(c("business intelligence"), " bi ", vector)
  vector <- mgsub(c("amazon web services"), " aws ", vector)
  vector <- mgsub(c("natural language processing"), " nlp ", vector)
  vector <- mgsub(c("internet of things"), " iot ", vector)
  vector <- mgsub(c("amazon web services"), " aws ", vector)
  vector <- mgsub(c("text analysis"), " textual ", vector)
  vector <- mgsub(c("electronic medical record", "electronic medical records"), " emr ", vector)
  vector <- mgsub(c("google cloud", "google cloud platform", "google cloud platforms"), " gcp ", vector)
  vector <- mgsub(c("power bi "), " powerbi ", vector)
  vector <- mgsub(c(" py "), " python ", vector)
  vector <- mgsub(c("java script"), " javascript ", vector)
  vector <- clean(vector)
  vector <- mgsub(c(",", "/", "(", ")", ":", "!", "?", "."), " ", vector)
  vector <- mgsub(c(" 000 ", " 000 00 "), "k ", vector)
  vector <- replace_contraction(vector)
  vector <- removeWords(vector, stopwords("en"))
  vector <- Trim(vector)
  return(vector)
}

# clean the job description strings
postings1$job_description <- clean_vector(postings1$job_description)
postings1 <- as.data.frame(postings1)

# preview text
postings1$job_description[5]

# clean the job title strings
postings1$job_title <- replace_abbreviation(postings1$job_title)
postings1$job_title <- mgsub(c(" – ", " — ", "-", "("), " - ", postings1$job_title)

# remove redundancies from titles
postings1 <- postings1 %>%
  separate(job_title, c("job_title"), " - ")
postings1 <- postings1 %>%
  separate(job_title, c("job_title"), ",")
postings1$job_title <- tolower(postings1$job_title)
postings1$job_title <- Trim(postings1$job_title)

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
  "finance",
  "engineering",
  "biostatistics",
  "bioinformatics",
  "physics",
  "economics",
  "computer scien",
  "marketing",
  "consumer",
  "transportation",
  "consulting"
)

skills_hard <- c(
  "coding",
  "neural",
  " nlp ",
  "deep learning",
  " ai ",
  "visualiz",
  " math",
  "research",
  "statistic",
  " ml ",
  "advanced",
  "reporting",
  "wrangl",
  "algebr",
  "hypoth",
  "risk ",
  " bi ",
  "cloud",
  " iot ",
  " ux design",
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
  "logistic",
  "curric",
  "server",
  "collect",
  "program",
  "transform",
  "calculus",
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
  "multivari",
  "network",
  "big data",
  "writ"
)

skills_soft <- c(
  "multitask",
  "reasoning",
  "communicat",
  "collaborat",
  " team",
  "critical think",
  "learn",
  "creativ",
  "train",
  "facilitat",
  "present",
  "advis",
  "mentor",
  "persua",
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
  "launch",
  "disciplin",
  "detail",
  "organized",
  "meaning",
  "storytel",
  "thinking",
  "priorit",
  "efficien",
  "precision",
  "listening",
  "adaptab",
  "independ",
  "supervis",
  "innovat",
  "discreti"
)

skills_tools <- c(
  "python",
  " r ",
  "tableau",
  "scala",
  "google analytics",
  "java ",
  " sql ",
  "mysql",
  "nosql",
  "mongodb",
  "excel",
  "powerpoint",
  "matlab",
  " sas ",
  "hadoop",
  "tensorflow",
  "postgres",
  "linux",
  " aws ",
  "hive",
  "spark",
  "powerbi",
  "scikit",
  "azure",
  " emr ",
  "perl",
  "ruby",
  "javascript",
  "spss",
  "d3",
  "pandas",
  " pig ",
  "keras",
  "numpy",
  " caffe",
  " hbase",
  "pytorch",
  "cassandra",
  "docker",
  " git ",
  "linux",
  " c++",
  "markdown",
  "latex",
  "unix",
  "html",
  "css",
  " gcp ",
  " gis ",
  "oozie",
  "stata",
  " php ",
  "shiny",
  "oracle"
)

jobs_type <- c(
  "consultant",
  "full-time",
  "part-time",
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
  for (j in names(postings_field)[16:29]) {
    postings_field[[j]][i] <- if_else(str_detect(
      postings1$job_description[i],
      j
    ), TRUE, FALSE)
  }
}

for (i in seq_len(n)) {
  for (j in names(postings_hard)[16:67]) {
    postings_hard[[j]][i] <- if_else(str_detect(
      postings1$job_description[i],
      j
    ), TRUE, FALSE)
  }
}

for (i in seq_len(n)) {
  for (j in names(postings_soft)[16:60]) {
    postings_soft[[j]][i] <- if_else(str_detect(
      postings1$job_description[i],
      j
    ), TRUE, FALSE)
  }
}

for (i in seq_len(n)) {
  for (j in names(postings_tools)[16:68]) {
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
postings_field[16:29] <- lapply(postings_field[16:29], as.logical)
postings_hard[16:67] <- lapply(postings_hard[16:67], as.logical)
postings_soft[16:60] <- lapply(postings_soft[16:60], as.logical)
postings_tools[16:68] <- lapply(postings_tools[16:68], as.logical)
postings_type[16:20] <- lapply(postings_type[16:20], as.logical)

# see how it worked
postings_field[16:29] %>% colSums(na.rm = TRUE)
postings_hard[16:67] %>% colSums(na.rm = TRUE)
postings_soft[16:60] %>% colSums(na.rm = TRUE)
postings_tools[16:68] %>% colSums(na.rm = TRUE)
postings_type[16:20] %>% colSums(na.rm = TRUE)

postings1$job_description <- removePunctuation(postings1$job_description)
postings1$job_title <- removePunctuation(postings1$job_title)
