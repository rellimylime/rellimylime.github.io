library(tidyverse)
library(readxl)
library(showtext)
library(sysfonts)
library(patchwork)

font_add_google("Urbanist", "urbanist")
showtext_auto()
showtext_opts(dpi = 400)

invert_raw <- read_excel("data/Atristainetal_JAE_Data.xlsx", sheet = "Invertebrates")
colnames(invert_raw)[1] <- "site_name"
colnames(invert_raw)[5] <- "site_code"

invert_baci <- invert_raw |>
  select(site_name, Replicate, Period, Reach, site_code, S, `H´`, TD, IASPT) |>
  rename(richness = S, shannon = `H´`, density = TD) |>
  mutate(
    reach_type = case_when(
      Reach == "C" ~ "C",
      Reach == "R" ~ "R",
      TRUE ~ "I"
    ),
    period = factor(Period, levels = c("B", "D", "A"),
                    labels = c("Before", "Drawdown", "After")),
    reach_type = factor(reach_type, levels = c("C", "I", "R"))
  ) |>
  filter(reach_type %in% c("C", "I"))

did_means <- invert_baci |>
  group_by(period, reach_type) |>
  summarise(
    mean_richness = mean(richness),
    se_richness   = sd(richness) / sqrt(n()),
    mean_iaspt    = mean(IASPT, na.rm = TRUE),
    se_iaspt      = sd(IASPT, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

colors <- c("C" = "#1a8c7a", "I" = "#c0392b")
labels <- c("Control", "Impact")

p_richness <- ggplot(did_means, aes(x = period, y = mean_richness,
                                    color = reach_type, group = reach_type)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  geom_errorbar(aes(ymin = mean_richness - 1.96 * se_richness,
                    ymax = mean_richness + 1.96 * se_richness), width = 0.1) +
  scale_color_manual(values = colors, labels = labels) +
  labs(x = "Period", y = "Mean taxa richness (S)", color = "Reach type") +
  theme_minimal(base_size = 13, base_family = "urbanist")

p_iaspt <- ggplot(did_means, aes(x = period, y = mean_iaspt,
                                  color = reach_type, group = reach_type)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  geom_errorbar(aes(ymin = mean_iaspt - 1.96 * se_iaspt,
                    ymax = mean_iaspt + 1.96 * se_iaspt), width = 0.1) +
  scale_color_manual(values = colors, labels = labels) +
  labs(
    x = "Period",
    y = "Mean IASPT",
    color = "Reach type",
    subtitle = "IASPT: average pollution sensitivity of taxa present\n(higher = more sensitive taxa, indicating better water quality)"
  ) +
  theme_minimal(base_size = 13, base_family = "urbanist") +
  theme(plot.subtitle = element_text(size = 9, color = "grey40"))

p_combined <- p_richness + p_iaspt +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("fig_did.png", p_combined, width = 12, height = 4, dpi = 300)
message("Saved to fig_did.png")
