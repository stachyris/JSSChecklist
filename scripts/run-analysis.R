library(readr)
library(dplyr)
library(skimmr)
library(showtext)
library(ggplot2)
library(forcats)
library(sysfonts)
library(calecopal)
library(patchwork)
library(here)
library(auk)
library(lubridate)
# Load your species list
jss_sp <- read_csv("data/JSS_Sp_list.csv")

# Load Clements checklist
clements_sp <- read_csv("data/eBird-Clements-v2024-integrated-checklist-October-2024-rev.csv")

# Join to add order and family
annotated_species <- jss_sp %>%
  left_join(clements_sp %>% select(`scientific name`, order, family), 
            by = c("Scientific Name" = "scientific name"))

# remove all NA's
annotated_species <- annotated_species[, colSums(is.na(annotated_species)) < nrow(annotated_species)]
annotated_species <- annotated_species[rowSums(is.na(annotated_species)) < ncol(annotated_species), ]

# Let's get SOIB and IUCN info. 
# SOIB report has both. https://stateofindiasbirds.in/#soib_report
# skimmr R package provides the latest information as an R object. 

soib # just checking

# merge the information
annotated_species <- annotated_species %>%
  left_join(skimmr::soib %>%
              dplyr::select(`Scientific Name`, `IUCN Category`, `SoIB 2023 Priority Status`),
            by = c("Scientific Name" = "Scientific Name"))

# ah the changes to taxonomy strikes. 3 entries are either missing or did not align due to name mismatches, let's add them manually

annotated_species <- annotated_species %>%
  mutate(
    `IUCN Category` = case_when(
      `Scientific Name` == "Tachyspiza badia" ~ "Least Concern",
      `Scientific Name` == "Ardea coromanda" ~ "Least Concern",
      `Scientific Name` == "Ficedula albicilla" ~ "Least Concern",
      TRUE ~ `IUCN Category`
    ),
    `SoIB 2023 Priority Status` = case_when(
      `Scientific Name` == "Tachyspiza badia" ~ "Low",
      `Scientific Name` == "Ardea coromanda" ~ "Low",
      `Scientific Name` == "Ficedula albicilla" ~ "Not Assessed",
      TRUE ~ `SoIB 2023 Priority Status`
    )
  )

annotated_species$family <- sub("\\s*\\(.*\\)", "", annotated_species$family)

#let's save this
write.csv(annotated_species, "data/annotated_species.csv")


### Plots
font_add_google("EB Garamond", "EB Garamond")
showtext_auto()
showtext_opts(dpi = 300)
# color palette
pal <- calecopal::cal_palette("sierra1", n = 38, type = "continuous")
pal2 <- calecopal::cal_palette("superbloom3")


# Plot 1 Simple family and order counts. 
df_family_counts <- annotated_species %>%
  count(family, name = "species_count") %>%
  arrange(desc(species_count)) %>%
  mutate(family = fct_reorder(family, species_count))

p_family_counts <- ggplot(df_family_counts, aes(x = family, y = species_count)) +
  geom_segment(aes(x = family, xend = family, y = 0, yend = species_count),
               colour = scales::alpha("black", 0.9), linewidth = 0.8) +
  geom_point(size = 3.5, colour = pal2[1], fill = pal2[1], shape = 21, stroke = 0.1) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Number of Species per Family",
    x = NULL, y = "Species Count"
  ) +
  theme_minimal(base_size = 14, base_family = "EB Garamond") +
  theme(
    plot.title = element_text(face = "bold", size = 18, margin = margin(b = 8)),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 14, 10, 14)
  )

df_order_counts <- annotated_species %>%
  count(order, name = "species_count") %>%
  arrange(desc(species_count)) %>%
  mutate(order = fct_reorder(order, species_count))

p_order_counts <- ggplot(df_order_counts, aes(x = order, y = species_count)) +
  geom_segment(aes(x = order, xend = order, y = 0, yend = species_count),
               colour = scales::alpha("black", 0.9), linewidth = 0.8) +
  geom_point(size = 3.5, colour = pal2[1], fill = pal2[1], shape = 21, stroke = 0.1) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Number of Species per Order",
    x = NULL, y = "Species Count"
  ) +
  theme_minimal(base_size = 14, base_family = "EB Garamond") +
  theme(
    plot.title = element_text(face = "bold", size = 18, margin = margin(b = 8)),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 14, 10, 14)
  )

p_family_counts
p_order_counts
plot_1 <- p_family_counts | p_order_counts
plot_1
ggsave("./figures/Family-Order-Counts.png", plot_1, dpi = 300, height = 10, width = 9)


# Let's look at the SoIB status
status_counts <- annotated_species %>%
  count(`SoIB 2023 Priority Status`, sort = TRUE) %>%
  mutate(`SoIB 2023 Priority Status` = fct_reorder(`SoIB 2023 Priority Status`, n))


SoIB_status <- ggplot(status_counts,
             aes(x = `SoIB 2023 Priority Status`, y = n, fill = `SoIB 2023 Priority Status`)) +
  geom_col(width = 0.7, alpha = 0.9, colour = scales::alpha("grey15", 0.8), linewidth = 0.25) +
  coord_flip() +
  scale_fill_manual(values = pal2, guide = "none") +
  labs(
    title = "SoIB 2023 Priority Status",
    x = NULL, y = "Species count"
  ) +
  theme_minimal(base_size = 14, base_family = "EB Garamond") +
  theme(
    plot.title = element_text(hjust = 0, face = "bold", size = 18, margin = margin(b = 6)),
    axis.title.y = element_blank(),
    axis.title.x = element_text(margin = margin(t = 6)),
    axis.text = element_text(size = 13),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 14, 10, 14)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

SoIB_status

iucn_counts <- annotated_species %>%
  count(`IUCN Category`, sort = TRUE) %>%
  mutate(`IUCN Category` = fct_reorder(`IUCN Category`, n))

# Plot
IUCN_status <- ggplot(iucn_counts,
                      aes(x = `IUCN Category`, y = n, fill = `IUCN Category`)) +
  geom_col(width = 0.7, alpha = 0.9, colour = scales::alpha("grey15", 0.8), linewidth = 0.25) +
  coord_flip() +
  scale_fill_manual(values = pal2, guide = "none") +
  labs(
    title = "IUCN Category",
    x = NULL, y = "Species count"
  ) +
  theme_minimal(base_size = 14, base_family = "EB Garamond") +
  theme(
    plot.title = element_text(hjust = 0, face = "bold", size = 18, margin = margin(b = 6)),
    axis.title.y = element_blank(),
    axis.title.x = element_text(margin = margin(t = 6)),
    axis.text = element_text(size = 13),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 14, 10, 14)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
IUCN_status

plot_2 <- IUCN_status / SoIB_status  
plot_2
ggsave("./figures/status.png", plot_2, dpi = 300, height = 10, width = 9)


### Let's look at the ebd dataset to try and figure out the total time, checklists etc
mysuru_ebd <- read_ebd("data/ebd_IN-KA-MY_201704_201905_unv_smp_relJun-2025/ebd_IN-KA-MY_201704_201905_unv_smp_relJun-2025.txt")
mysuru_ebd_filtered <- mysuru_ebd %>%
  mutate(observation_date = ymd(observation_date)) %>%
  filter(
    observation_date >= as.Date("2017-04-01") &
      observation_date <= as.Date("2019-05-31") &
      locality_id == "L5915465"
  )

# How many lists per year?
checklists_per_year <- mysuru_ebd_filtered %>%
  distinct(sampling_event_identifier, .keep_all = TRUE) %>%
  mutate(year = year(observation_date)) %>%
  count(year, name = "n_checklists")

# Total time spent?
total_hours <- mysuru_ebd_filtered %>%
  distinct(sampling_event_identifier, .keep_all = TRUE) %>%
  summarise(total_hours = sum(duration_minutes, na.rm = TRUE) / 60)

checklists_per_year
total_hours
save.image(file = here(paste0("JSS_Checklist.RData")))
