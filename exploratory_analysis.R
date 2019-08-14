### After running the jobscraper.R script
### perform exploratory data analysis

# top jobs
temp <- group_by(
  postings1,
  job_title
) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(n = 10)

temp$job_title <- parse_factor(
  as.character(temp$job_title),
  rev(as.character(temp$job_title))
)

# top companies
temp2 <- postings1 %>%
  group_by(company_name) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(n = 10)
temp3 <- subset(
  postings1,
  postings1$company_name %in%
    temp2$company_name
) %>%
  select(
    company_name,
    title_term
  )
# fix levels
temp3$company_name <- parse_factor(
  as.character(temp3$company_name),
  rev(as.character(temp2$company_name))
)

# minimum education
nrow(postings1[postings1$bachelors == TRUE |
                 postings1$masters == TRUE |
                 postings1$doctorate == TRUE, ])

nrow(postings1[postings1$bachelors == TRUE |
                 postings1$masters == TRUE |
                 postings1$doctorate == TRUE, ]) / nrow(postings1)

# tools and tech
temp4 <- subset(
  postings_tools,
  postings_tools$title_term == "analyst"
)

temp5 <- subset(
  postings_tools,
  postings_tools$title_term == "science"
)

temp6 <- subset(
  postings_tools,
  postings_tools$title_term == "engineer"
)

tools.df <- colSums(temp4[16:68],
                    na.rm = TRUE
) %>%
  as.data.frame()
tools.df <- rownames_to_column(tools.df)
colnames(tools.df) <- c(
  "tech",
  "count"
)

tools.df <- as_tibble(tools.df)
tools.df <- mutate(tools.df,
                   percent = paste(
                     ceiling(count / nrow(temp4) * 100),
                     "%"
                   )
)

tools.df <- arrange(
  tools.df,
  desc(count)
)

# fix levels
tools.df$tech <- parse_factor(
  as.character(tools.df$tech),
  rev(as.character(tools.df$tech))
)

tools2.df <- colSums(temp5[16:68],
                     na.rm = TRUE
) %>%
  as.data.frame()
tools2.df <- rownames_to_column(tools2.df)
colnames(tools2.df) <- c(
  "tech",
  "count"
)

tools2.df <- as_tibble(tools2.df)
tools2.df <- mutate(tools2.df,
                    percent = paste(
                      ceiling(count / nrow(temp5) * 100),
                      "%"
                    )
)

tools2.df <- arrange(
  tools2.df,
  desc(count)
)

# fix levels
tools2.df$tech <- parse_factor(
  as.character(tools2.df$tech),
  rev(as.character(tools2.df$tech))
)

tools3.df <- colSums(temp6[16:68],
                     na.rm = TRUE
) %>%
  as.data.frame()
tools3.df <- rownames_to_column(tools3.df)
colnames(tools3.df) <- c(
  "tech",
  "count"
)

tools3.df <- as_tibble(tools3.df)
tools3.df <- mutate(tools3.df,
                    percent = paste(
                      ceiling(count / nrow(temp6) * 100),
                      "%"
                    )
)

tools3.df <- arrange(
  tools3.df,
  desc(count)
)

# fix levels
tools3.df$tech <- parse_factor(
  as.character(tools3.df$tech),
  rev(as.character(tools3.df$tech))
)

soft.df <- colSums(postings_soft[16:60],
                   na.rm = TRUE
) %>%
  as.data.frame()
soft.df <- rownames_to_column(soft.df)
colnames(soft.df) <- c(
  "skills",
  "count"
)

soft.df <- as_tibble(soft.df)
soft.df <- mutate(soft.df,
                  percent = paste(
                    ceiling(count / nrow(postings1) * 100),
                    "%"
                  )
)

soft.df <- arrange(
  soft.df,
  desc(count)
)

# fix levels
soft.df$skills <- parse_factor(
  as.character(soft.df$skills),
  rev(as.character(soft.df$skills))
)

hard.df <- colSums(postings_hard[16:67],
                   na.rm = TRUE
) %>%
  as.data.frame()
hard.df <- rownames_to_column(hard.df)
colnames(hard.df) <- c(
  "skills",
  "count"
)

hard.df <- as_tibble(hard.df)
hard.df <- mutate(hard.df,
                  percent = paste(
                    ceiling(count / nrow(postings1) * 100),
                    "%"
                  )
)

hard.df <- arrange(
  hard.df,
  desc(count)
)

# fix levels
hard.df$skills <- parse_factor(
  as.character(hard.df$skills),
  rev(as.character(hard.df$skills))
)


## plots ----

# top 10, descending frequency
barplot(words_freq[1:10, ]$num,
        las = 2,
        names.arg = words_freq[1:10, ]$term,
        col = viridis(10),
        cex.axis = 0.8,
        main = "Most frequent words",
        ylab = "Word frequencies"
)

# top 30, descending alpha
ggplot(
  words_freq[1:30, ],
  aes(
    x = term,
    y = num,
    fill = term
  )
) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(
    angle = 90,
    hjust = 1
  )) +
  ylab("Word frequencies") +
  xlab("Most frequent words") +
  guides(fill = FALSE)

# Most Popular Job Titles
ggplot(
  data = temp,
  aes(
    x = job_title,
    y = n,
    fill = "Blues"
  )
) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n),
            size = 2.75,
            hjust = -.15
  ) +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank()
  ) +
  labs(
    x = "Job Title",
    y = "Number of Listings"
  ) +
  ggtitle("Most Popular Job Titles") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_blank()
  )

# Top Companies
ggplot(
  data = temp3,
  aes(
    x = company_name,
    fill = title_term
  )
) +
  geom_bar(stat = "count") +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal() +
  theme(panel.grid.major = element_blank()) +
  labs(
    x = "Company Name",
    y = "Number of Listings"
  ) +
  ggtitle("Hiring Companies in Boston") +
  theme(plot.title = element_text(hjust = 0.5))


# Distribution of the `Days_ago` Feature
ggplot(
  data = subset(
    postings1,
    !is.na(postings1$title_term)
  ),
  aes(
    x = fct_infreq(factor(days_ago)),
    fill = title_term
  )
) +
  geom_bar(stat = "count") +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal() +
  theme(panel.grid.major = element_blank()) +
  labs(
    x = "Days Ago",
    y = "Number of Listings"
  ) +
  ggtitle("Days Ago") +
  theme(plot.title = element_text(hjust = 0.5))

# Minimum Years of Experience
ggplot(
  data = subset(
    postings1,
    !is.na(postings1$min_exp)
  ),
  aes(
    x = min_exp,
    fill = title_term
  )
) +
  geom_bar() +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal() +
  theme(panel.grid.major = element_blank()) +
  labs(
    x = "Years of Experience",
    y = "Number of Listings"
  ) +
  ggtitle("Minumum Years of Experience") +
  theme(plot.title = element_text(hjust = 0.5))

# Minimum Level of Education
ggplot(
  data = subset(
    postings1,
    !is.na(min_ed)
  ),
  aes(
    x = min_ed,
    fill = title_term
  )
) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal() +
  theme(panel.grid.major = element_blank()) +
  labs(
    x = "Minimum Level of Education",
    y = "Number of Listings"
  ) +
  ggtitle("Minimum Level of Education") +
  theme(plot.title = element_text(hjust = 0.5))

# Popular Tools & Technolgy
ggplot(
  data = tools.df,
  aes(
    x = tech,
    y = count,
    fill = "Blues"
  )
) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percent),
            size = 2.75,
            hjust = -.1
  ) +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank()
  ) +
  labs(
    x = "Tools",
    y = "Number of Listings"
  ) +
  ggtitle("Most Popular Tools & Tech for Data Analysts") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(
  data = tools2.df,
  aes(
    x = tech,
    y = count,
    fill = "Blues"
  )
) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percent),
            size = 2.75,
            hjust = -.1
  ) +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank()
  ) +
  labs(
    x = "Tools",
    y = "Number of Listings"
  ) +
  ggtitle("Most Popular Tools & Tech for Data Scientists") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(
  data = tools3.df,
  aes(
    x = tech,
    y = count,
    fill = "Blues"
  )
) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percent),
            size = 2.75,
            hjust = -.1
  ) +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank()
  ) +
  labs(
    x = "Tools",
    y = "Number of Listings"
  ) +
  ggtitle("Most Popular Tools & Tech for Data Engineers") +
  theme(plot.title = element_text(hjust = 0.5))


# Soft skills

ggplot(
  data = soft.df,
  aes(
    x = skills,
    y = count,
    fill = "Blues"
  )
) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percent),
            size = 2.75,
            hjust = -.1
  ) +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank()
  ) +
  labs(
    x = "Tools",
    y = "Number of Listings"
  ) +
  ggtitle("Most Popular Soft Skills") +
  theme(plot.title = element_text(hjust = 0.5))


# Hard skills

ggplot(
  data = hard.df,
  aes(
    x = skills,
    y = count,
    fill = "Blues"
  )
) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percent),
            size = 2.75,
            hjust = -.1
  ) +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank()
  ) +
  labs(
    x = "Tools",
    y = "Number of Listings"
  ) +
  ggtitle("Most Popular Hard Skills") +
  theme(plot.title = element_text(hjust = 0.5))
