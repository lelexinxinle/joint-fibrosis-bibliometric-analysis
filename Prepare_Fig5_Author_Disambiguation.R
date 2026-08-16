setwd("D:/research/文献计量学/关节纤维化_大修")
library(bibliometrix)
library(dplyr)
library(openxlsx)

openalex_api_key <- "iLDn6OajInzcUtMNGp2Ewm"
Sys.setenv(
  openalexR.apikey = openalex_api_key,
  openalexR_apikey = openalex_api_key
)

M <- readRDS("Data/scopus_wos.rds")
doi <- unique(na.omit(M$DI))

author_list <- lapply(doi, function(x) {
  tryCatch(
    get_authors_summary(x),
    error = function(e) NULL
  )
})
saveRDS(author_list,file = "Data/author_list.rds")

author_list[[1]]

success <- lengths(author_list) > 0
sum(success)
sum(!success)


authors <- bind_rows(author_list, .id = "Document_ID") %>%
  mutate(
    Author_ID = ifelse(
      !is.na(orcid),
      orcid,
      openalex_id
    )
  ) %>%
  filter(!is.na(Author_ID)) %>%
  distinct(Document_ID, Author_ID, .keep_all = TRUE)

author_summary <- authors %>%
  count(Author_ID, display_name, name = "Articles") %>%
  arrange(desc(Articles))

write.xlsx(
  author_summary,
  "Results/Fig5/Most_Relevant_Authors_Disambiguated.xlsx",
  overwrite = TRUE
)
