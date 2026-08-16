setwd("")
library(dplyr)
library(ggplot2)
library(openxlsx)
library(stringr)

input_file <- "Data/scopus_wos.rds"
output_dir <- "Results/Fig12_unique_documents"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

M <- readRDS(input_file)

if (!all(c("TI", "AB", "DE") %in% names(M))) {
  stop("The data must contain TI, AB, and DE.")
}


documents <- M %>%
  transmute(
    Document_ID = row_number(),
    Title = coalesce(TI, ""),
    Text = str_to_lower(str_c(
      coalesce(TI, ""),
      coalesce(AB, ""),
      coalesce(DE, ""),
      sep = " ; "
    ))
  )


site_patterns <- c(
  "Acromioclavicular Joint" = "\\bacromioclavicular(\\s+joint(s)?)?\\b",
  "Atlanto-Axial Joint" = "\\b(atlanto-axial|atlantoaxial)(\\s+joint(s)?)?\\b",
  "Atlanto-Occipital Joint" = "\\b(atlanto-occipital|atlantooccipital)(\\s+joint(s)?)?\\b",
  "Elbow Joint" = "\\belbow(\\s+joint(s)?)?\\b",
  "Foot Joints" = paste0(
    "\\b(foot\\s+joint(s)?|toe\\s+joint(s)?|ankle|talocrural|subtalar|",
    "tibiotalar|metatarsophalangeal)\\b|",
    "\\b(foot|toe)\\b.{0,60}\\binterphalangeal\\b|",
    "\\binterphalangeal\\b.{0,60}\\b(foot|toe)\\b"
  ),
  "Hand Joints" = paste0(
    "\\b(hand\\s+joint(s)?|finger\\s+joint(s)?|wrist|radiocarpal|",
    "metacarpophalangeal)\\b|",
    "\\b(hand|finger)\\b.{0,60}\\binterphalangeal\\b|",
    "\\binterphalangeal\\b.{0,60}\\b(hand|finger)\\b"
  ),
  "Hip Joint" = "\\b(hip(\\s+joint(s)?)?|acetabulofemoral|coxofemoral)\\b",
  "Knee Joint" = "\\b(knee(\\s+joint(s)?)?|tibiofemoral|patellofemoral)\\b",
  "Pubic Symphysis" = "\\b(pubic\\s+symphysis|symphysis\\s+pubis)\\b",
  "Sacroiliac Joint" = "\\bsacroiliac(\\s+joint(s)?)?\\b",
  "Shoulder Joint" = "\\b(shoulder|glenohumeral)\\b",
  "Sternoclavicular Joint" = "\\bsternoclavicular(\\s+joint(s)?)?\\b",
  "Sternocostal Joints" = "\\bsternocostal(\\s+joint(s)?)?\\b",
  "Temporomandibular Joint" = "\\b(temporomandibular(\\s+joint(s)?)?|tmj)\\b",
  "Zygapophyseal Joint" = "\\b(zygapophyseal\\s+joint(s)?|facet\\s+joint(s)?)\\b"
)

document_site <- bind_rows(lapply(names(site_patterns), function(site) {
  documents %>%
    filter(str_detect(Text, regex(site_patterns[[site]], ignore_case = TRUE))) %>%
    transmute(Document_ID, Title, Site = site)
})) %>%
  distinct(Document_ID, Site, .keep_all = TRUE)

corpus_size <- nrow(documents)

site_summary <- tibble(Site = names(site_patterns)) %>%
  left_join(count(document_site, Site, name = "Unique_publications"), by = "Site") %>%
  mutate(
    Unique_publications = coalesce(Unique_publications, 0L),
    Corpus_size = corpus_size,
    Percentage_of_corpus = 100 * Unique_publications / Corpus_size
  ) %>%
  arrange(desc(Unique_publications))

write.xlsx(
  list(
    Unique_publications_by_site = site_summary,
    Document_site_assignments = document_site
  ),
  file = file.path(output_dir, "Anatomical_Sites_Unique_Publications.xlsx"),
  overwrite = TRUE
)

p <- ggplot(
  site_summary,
  aes(x = reorder(Site, Unique_publications), y = Unique_publications)
) +
  geom_col(fill = "#4E79A7", color = "black", linewidth = 0.3, width = 0.7) +
  geom_text(aes(label = Unique_publications), hjust = -0.12, size = 3.8) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    x = NULL,
    y = "Number of unique publications",
    caption = str_wrap(paste0(
      "Classification was based on titles, abstracts, and author keywords (n = ",
      corpus_size,
      "). Publications could be assigned to more than one site."
    ), width = 95)
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 8)),
    plot.margin = margin(10, 20, 10, 15)
  )

ggsave(
  file.path(output_dir, "Anatomical_Sites_Unique_Publications.png"),
  p,
  width = 10,
  height = 7.5,
  dpi = 600
)

message("Corpus size: ", corpus_size)
message("Document-site assignments: ", nrow(document_site))
