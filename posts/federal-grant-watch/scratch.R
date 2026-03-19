library(targets)
library(dplyr)
library(stringr)
library(lubridate)
library(scales)
library(ggplot2)

g <- tar_read(doge_grants)
u <- tar_read(usaspending_awards)

core_agencies <- c(
  "Department Of Education",
  "National Science Foundation",
  "National Endowment for the Humanities",
  "National Endowment for the Arts",
  "Institute of Museum and Library Services"
)

df <- g |>
  mutate(
    date = as.Date(date),
    savings = as.numeric(savings),
    value = as.numeric(value),
    usaspending_award_id = str_match(link, "usaspending\\.gov/award/([^/?#]+)")[, 2]
  ) |>
  filter(date >= as.Date("2025-01-20")) |>
  left_join(u, by = "usaspending_award_id") |>
  mutate(
    verified_remaining_proxy = pmax(usaspending_total_obligation - usaspending_total_outlay, 0),
    text = str_to_lower(str_c(coalesce(agency, ""), coalesce(recipient, ""), coalesce(description, ""), sep = " || ")),
    in_education_scope =
      agency %in% core_agencies |
      str_detect(text, "\\buniversity\\b|\\bcollege\\b|\\bschool district\\b|\\bboard of education\\b|\\beducationusa\\b|\\bfulbright\\b|\\bstudent aid\\b|\\bfellowship\\b|\\bscholarship\\b|\\bk-12\\b|\\bclassroom\\b")
  ) |>
  filter(in_education_scope) |>
  mutate(
    primary_area = case_when(
      agency == "National Science Foundation" |
        str_detect(text, "\\bresearch\\b|\\blaboratory\\b|\\br&d\\b|\\bscience\\b") ~ "Research & R&D",
      agency %in% c("National Endowment for the Humanities", "Institute of Museum and Library Services", "National Endowment for the Arts") |
        str_detect(text, "\\bhumanities\\b|\\barts\\b|\\bmuseum\\b|\\blibrary\\b|\\bcultural\\b|\\barchive\\b") ~ "Arts, Humanities & Libraries",
      str_detect(text, "\\bk-12\\b|\\belementary\\b|\\bsecondary\\b|\\bhigh school\\b|\\bschool district\\b|\\bteacher\\b|\\bclassroom\\b") ~ "K-12 Schools",
      agency == "Department Of Education" |
        str_detect(text, "\\bhigher education\\b|\\bpostsecondary\\b|\\bcollege\\b|\\buniversity\\b|\\bstudent aid\\b|\\bpell\\b|\\bfafsa\\b|\\btuition\\b|\\bscholarship\\b|\\bfellowship\\b|\\bdoctoral\\b|\\bphd\\b|\\bpostdoc\\b") ~ "Higher Ed & Student Support",
      str_detect(text, "\\bworkforce\\b|\\bapprenticeship\\b|\\bcareer\\b|\\btechnical education\\b|\\bcte\\b|\\btraining\\b") ~ "Workforce & CTE",
      str_detect(text, "\\beducationusa\\b|\\bfulbright\\b|\\binternational education\\b|\\bexchange\\b") ~ "International Education",
      TRUE ~ "Other Education"
    )
  )

# 1) total cuts in education scope
totals <- df |>
  summarise(
    n_grants = n(),
    claimed_savings_total = sum(savings, na.rm = TRUE),
    matched_verified_rows = sum(!is.na(verified_remaining_proxy)),
    verified_remaining_total = sum(verified_remaining_proxy, na.rm = TRUE)
  )

# 2) which areas are cut most
area_summary <- df |>
  group_by(primary_area) |>
  summarise(
    n_grants = n(),
    claimed_savings = sum(savings, na.rm = TRUE),
    median_claimed_savings = median(savings, na.rm = TRUE),
    verified_rows = sum(!is.na(verified_remaining_proxy)),
    verified_remaining = sum(verified_remaining_proxy, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    pct_of_claimed = claimed_savings / sum(claimed_savings),
    pct_of_count = n_grants / sum(n_grants)
  ) |>
  arrange(desc(claimed_savings))

# 3) trend over time by area
monthly_area <- df |>
  mutate(month = floor_date(date, "month")) |>
  group_by(month, primary_area) |>
  summarise(
    n_grants = n(),
    claimed_savings = sum(savings, na.rm = TRUE),
    .groups = "drop"
  )

totals
area_summary
monthly_area

ggplot(monthly_area, aes(month, n_grants, color = primary_area)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  labs(title = "Education-Related Grant Cuts Over Time", x = NULL, y = "Grant count", color = "Area") +
  theme_minimal(base_size = 12)

ggplot(area_summary, aes(reorder(primary_area, claimed_savings), claimed_savings)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = dollar) +
  labs(title = "Claimed Savings by Education Area", x = NULL, y = "DOGE claimed savings") +
  theme_minimal(base_size = 12)
