setwd("")

library(dplyr)
library(stringr)
library(ggplot2)
library(openxlsx)

M <- readRDS("Data/scopus_wos.rds")
if (!all(c("TI", "AB", "DE") %in% names(M))) stop("The data must contain TI, AB, and DE.")

output_dir <- "Results/Fig12"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

documents <- M %>%
  transmute(
    Document_ID = row_number(),
    Title = coalesce(TI, ""),
    Author_Keywords = coalesce(DE, ""),
    Text = str_to_lower(str_c(coalesce(TI, ""), coalesce(AB, ""), coalesce(DE, ""), sep = " ; "))
  )

site_patterns <- c(
  "Acromioclavicular Joint" = "\\bacromioclavicular(\\s+joint(s)?)?\\b",
  "Atlanto-Axial Joint" = "\\b(atlanto-axial|atlantoaxial)(\\s+joint(s)?)?\\b",
  "Atlanto-Occipital Joint" = "\\b(atlanto-occipital|atlantooccipital)(\\s+joint(s)?)?\\b",
  "Elbow Joint" = "\\belbow(\\s+joint(s)?)?\\b",
  "Foot Joints" = paste0(
    "\\b(foot\\s+joint(s)?|toe\\s+joint(s)?|ankle|talocrural|subtalar|tibiotalar|metatarsophalangeal)\\b|",
    "\\b(foot|toe)\\b.{0,60}\\binterphalangeal\\b|\\binterphalangeal\\b.{0,60}\\b(foot|toe)\\b"
  ),
  "Hand Joints" = paste0(
    "\\b(hand\\s+joint(s)?|finger\\s+joint(s)?|wrist|radiocarpal|metacarpophalangeal)\\b|",
    "\\b(hand|finger)\\b.{0,60}\\binterphalangeal\\b|\\binterphalangeal\\b.{0,60}\\b(hand|finger)\\b"
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

selected_sites <- c("Knee Joint", "Elbow Joint", "Shoulder Joint", "Foot Joints", "Hip Joint")

site_summary <- tibble(Site = selected_sites) %>%
  left_join(count(document_site, Site, name = "Unique_publications"), by = "Site") %>%
  mutate(
    Unique_publications = coalesce(Unique_publications, 0L),
    Corpus_size = corpus_size,
    Percentage_of_corpus = 100 * Unique_publications / Corpus_size
  ) %>%
  arrange(desc(Unique_publications))

term_patterns <- list(
  "Knee Joint" = c(
    "Knee Joint" = "\\bknee\\s+joint(s)?\\b", "Knee" = "\\bknee\\b",
    "Tibiofemoral" = "\\btibiofemoral\\b", "Patellofemoral" = "\\bpatellofemoral\\b"
  ),
  "Elbow Joint" = c(
    "Elbow Joint" = "\\belbow\\s+joint(s)?\\b", "Elbow" = "\\belbow\\b"
  ),
  "Shoulder Joint" = c(
    "Shoulder" = "\\bshoulder\\b", "Glenohumeral" = "\\bglenohumeral\\b"
  ),
  "Foot Joints" = c(
    "Foot Joint" = "\\bfoot\\s+joint(s)?\\b", "Toe Joint" = "\\btoe\\s+joint(s)?\\b",
    "Ankle" = "\\bankle\\b", "Talocrural" = "\\btalocrural\\b", "Subtalar" = "\\bsubtalar\\b",
    "Tibiotalar" = "\\btibiotalar\\b", "Metatarsophalangeal" = "\\bmetatarsophalangeal\\b",
    "Interphalangeal" = "\\b(foot|toe)\\b.{0,60}\\binterphalangeal\\b|\\binterphalangeal\\b.{0,60}\\b(foot|toe)\\b"
  ),
  "Hip Joint" = c(
    "Hip Joint" = "\\bhip\\s+joint(s)?\\b", "Hip" = "\\bhip\\b",
    "Acetabulofemoral" = "\\bacetabulofemoral\\b", "Coxofemoral" = "\\bcoxofemoral\\b"
  )
)

term_counts <- bind_rows(lapply(names(term_patterns), function(site) {
  bind_rows(lapply(names(term_patterns[[site]]), function(term) {
    documents %>%
      filter(str_detect(Text, regex(term_patterns[[site]][[term]], ignore_case = TRUE))) %>%
      transmute(Document_ID, Site = site, Anatomical_term = term)
  }))
})) %>%
  distinct(Document_ID, Site, Anatomical_term) %>%
  count(Site, Anatomical_term, name = "Unique_publications")

site_keywords <- bind_rows(lapply(names(term_patterns), function(site) {
  tibble(Site = site, Anatomical_term = names(term_patterns[[site]]))
})) %>%
  left_join(term_counts, by = c("Site", "Anatomical_term")) %>%
  mutate(Unique_publications = coalesce(Unique_publications, 0L)) %>%
  arrange(Site, desc(Unique_publications))

top_site_keywords <- site_keywords %>%
  mutate(Site = factor(Site, levels = selected_sites)) %>%
  arrange(Site, desc(Unique_publications), Anatomical_term) %>%
  mutate(
    Plot_term = paste(Site, Anatomical_term, sep = "___"),
    Plot_term = factor(Plot_term, levels = rev(unique(Plot_term)))
  )

write.xlsx(site_keywords, file.path(output_dir, "Fibrosis_Site_Related_Keywords.xlsx"), overwrite = TRUE)
write.xlsx(site_summary, file.path(output_dir, "Fibrosis_Site_Summary.xlsx"), overwrite = TRUE)
write.xlsx(top_site_keywords, file.path(output_dir, "Fibrosis_Site_Top_Keywords.xlsx"), overwrite = TRUE)

caption_text <- paste0(
  "Classification used titles, abstracts, and author keywords (n = ", corpus_size,
  "). Publications could be assigned to more than one site. Term counts may overlap within a site."
)

p1 <- ggplot(site_summary, aes(reorder(Site, Unique_publications), Unique_publications)) +
  geom_col(fill = "#2166AC", color = "black", width = 0.72) +
  geom_text(aes(label = Unique_publications), hjust = -0.12, size = 3.8) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
  labs(x = NULL, y = "Number of unique publications", caption = str_wrap(caption_text, 105)) +
  theme_classic(base_size = 14) +
  theme(plot.caption = element_text(hjust = 0, size = 9), plot.margin = margin(10, 22, 10, 10))

p2 <- ggplot(top_site_keywords, aes(Unique_publications, Plot_term,
                                    size = Unique_publications, color = Unique_publications)) +
  geom_segment(aes(x = 0, xend = Unique_publications, y = Plot_term, yend = Plot_term),
               inherit.aes = FALSE, color = "grey85", linewidth = 0.5) +
  geom_point(alpha = 0.95) +
  facet_wrap(~ Site, scales = "free", ncol = 3) +
  scale_y_discrete(labels = function(x) sub(".*___", "", x)) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.18))) +
  scale_size_continuous(trans = "sqrt", range = c(3.5, 11)) +
  scale_color_gradient(low = "#92C5DE", high = "#053061") +
  guides(size = "none") +
  labs(x = "Number of unique publications", y = NULL,
       size = "Publications", color = "Publications") +
  theme_classic(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold", size = 12.5),
    strip.background = element_rect(fill = "grey95", color = "grey30"),
    axis.text.y = element_text(size = 11),
    panel.spacing = unit(1.2, "lines")
  )

p3 <- ggplot(top_site_keywords, aes(Unique_publications, Plot_term)) +
  geom_col(fill = "#2166AC", color = "black", width = 0.7) +
  geom_text(aes(label = Unique_publications), hjust = -0.12, size = 3.4) +
  facet_wrap(~ Site, scales = "free", ncol = 3) +
  scale_y_discrete(labels = function(x) sub(".*___", "", x)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.20))) +
  labs(x = "Number of unique publications", y = NULL) +
  theme_classic(base_size = 12.5) +
  theme(
    strip.text = element_text(face = "bold", size = 12.5),
    strip.background = element_rect(fill = "grey95", color = "grey30"),
    axis.text.y = element_text(size = 10.5),
    panel.spacing = unit(1.2, "lines")
  )

p4 <- ggplot(top_site_keywords, aes("Publications", Plot_term, fill = Unique_publications)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = Unique_publications), size = 4, fontface = "bold") +
  facet_wrap(~ Site, scales = "free_y", ncol = 3) +
  scale_y_discrete(labels = function(x) sub(".*___", "", x)) +
  scale_fill_gradient(low = "#DCEAF4", high = "#053061") +
  labs(x = NULL, y = NULL, fill = "Publications") +
  theme_classic(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold", size = 12.5),
    strip.background = element_rect(fill = "grey95", color = "grey30"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 10.5),
    panel.spacing = unit(1.2, "lines")
  )

plots <- list(
  Fibrosis_Site_Total_Keyword_Occurrences = list(p1, 9, 7),
  Fibrosis_Site_Top_Keywords_Bubbleplot = list(p2, 12, 8),
  Fibrosis_Site_Top_Keywords_Barplot = list(p3, 12, 8),
  Fibrosis_Site_Top_Keywords_Heatmap = list(p4, 12, 8)
)

for (name in names(plots)) {
  ggsave(file.path(output_dir, paste0(name, ".png")), plots[[name]][[1]],
         width = plots[[name]][[2]], height = plots[[name]][[3]], dpi = 600)
  ggsave(file.path(output_dir, paste0(name, ".pdf")), plots[[name]][[1]],
         width = plots[[name]][[2]], height = plots[[name]][[3]])
}

message("Corpus size: ", corpus_size)
message("Document-site assignments: ", nrow(document_site))
message("Output saved in: ", output_dir)
