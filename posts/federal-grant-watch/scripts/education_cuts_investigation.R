# Education Cuts Investigation (Replicable Walkthrough)
#
# This script reproduces the investigative workflow we built interactively:
# 1) Pull DOGE grants + USAspending enrichment from targets cache.
# 2) Define education scope since January 20, 2025.
# 3) Classify into primary education areas.
# 4) Add sub-areas for Higher Ed and Research & R&D.
# 5) Refine "Unspecified" rows into meaningful categories.
# 6) Export summary tables and figures.

suppressPackageStartupMessages({
    library(targets)
    library(dplyr)
    library(stringr)
    library(lubridate)
    library(ggplot2)
    library(scales)
    library(readr)
})

# -------------------------------------------------------------------
# STEP 1: Load source data from targets
# -------------------------------------------------------------------
start_date <- as.Date("2025-01-20")

g <- tar_read(doge_grants)
u <- tar_read(usaspending_awards)

core_agencies <- c(
    "Department Of Education",
    "National Science Foundation",
    "National Endowment for the Humanities",
    "National Endowment for the Arts",
    "Institute of Museum and Library Services"
)

# -------------------------------------------------------------------
# STEP 2: Build education-scope dataset
# -------------------------------------------------------------------
df <- g |>
    mutate(
        row_id = row_number(),
        date = as.Date(date),
        savings = as.numeric(savings),
        value = as.numeric(value),
        usaspending_award_id = str_match(link, "usaspending\\.gov/award/([^/?#]+)")[, 2]
    ) |>
    filter(date >= start_date) |>
    left_join(u, by = "usaspending_award_id") |>
    mutate(
        verified_remaining_proxy = pmax(usaspending_total_obligation - usaspending_total_outlay, 0),
        text = str_to_lower(str_c(
            coalesce(agency, ""),
            coalesce(recipient, ""),
            coalesce(description, ""),
            sep = " || "
        )),
        in_education_scope =
            agency %in% core_agencies |
            str_detect(
                text,
                "\\buniversity\\b|\\bcollege\\b|\\bschool district\\b|\\bboard of education\\b|\\beducationusa\\b|\\bfulbright\\b|\\bstudent aid\\b|\\bfellowship\\b|\\bscholarship\\b|\\bk-12\\b|\\bclassroom\\b"
            )
    ) |>
    filter(in_education_scope)

# -------------------------------------------------------------------
# STEP 3: Classify primary education area
# -------------------------------------------------------------------
df <- df |>
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

# -------------------------------------------------------------------
# STEP 4: Add sub-areas for Higher Ed + Research & R&D
# -------------------------------------------------------------------
df_sub2 <- df |>
    mutate(
        he_subarea = case_when(
            primary_area != "Higher Ed & Student Support" ~ NA_character_,
            str_detect(text, regex("pell|fafsa|federal student aid|student loan|title iv|financial aid|tuition assistance|scholarship", TRUE)) ~ "Financial Aid & Affordability",
            str_detect(text, regex("doctoral|phd|postdoc|fellowship|traineeship|graduate", TRUE)) ~ "Graduate & Fellowship Support",
            str_detect(text, regex("hbcu|hsi|tribal college|minority-serving|msi", TRUE)) ~ "MSI/HBCU/Tribal Capacity",
            str_detect(text, regex("teacher preparation|teacher training|educator|principal", TRUE)) ~ "Teacher Pipeline",
            str_detect(text, regex("campus|student services|retention|advising|mental health|disability services", TRUE)) ~ "Campus & Student Services",
            str_detect(recipient, regex("university|college|institute|school|board of education", TRUE)) ~ "Institutional Awards (Higher Ed)",
            TRUE ~ "Higher-Ed Unspecified"
        ),
        rd_subarea = case_when(
            primary_area != "Research & R&D" ~ NA_character_,
            agency == "National Science Foundation" ~ "Core Science & Engineering",
            agency == "Department of Energy" ~ "Energy & Climate R&D",
            agency == "Department of Health and Human Services" ~ "Health & Biomedical R&D",
            agency %in% c("Department of Agriculture", "Environmental Protection Agency") ~ "Ag/Food/Environment R&D",
            agency %in% c("Department of Defense", "National Aeronautics and Space Administration", "Department of Homeland Security") ~ "Defense, Space & Security R&D",
            str_detect(text, regex("energy|battery|grid|nuclear|hydrogen|carbon|climate|emissions|renewable", TRUE)) ~ "Energy & Climate R&D",
            str_detect(text, regex("biomedical|clinical|nih|disease|vaccine|epidemiology|public health", TRUE)) ~ "Health & Biomedical R&D",
            str_detect(text, regex("agriculture|crop|soil|food|forestry|water|fisheries", TRUE)) ~ "Ag/Food/Environment R&D",
            str_detect(text, regex("defense|military|navy|air force|security|cyber|space|aerospace|satellite", TRUE)) ~ "Defense, Space & Security R&D",
            str_detect(text, regex("equipment|instrument|facility|laboratory|testbed|infrastructure", TRUE)) ~ "R&D Infrastructure & Equipment",
            TRUE ~ "R&D Unspecified"
        ),
        sub_area2 = coalesce(he_subarea, rd_subarea)
    ) |>
    filter(!is.na(sub_area2))

# -------------------------------------------------------------------
# STEP 5: Refine unspecified sub-areas
# -------------------------------------------------------------------
unknown <- df_sub2 |>
    filter(str_detect(sub_area2, "Unspecified"))

unknown_refined2 <- unknown |>
    mutate(
        desc_l = str_to_lower(coalesce(description, "")),
        rec_l = str_to_lower(coalesce(recipient, "")),
        unknown_type2 = case_when(
            str_detect(desc_l, "westat|mcrel|education northwest|child trends|icf|research triangle institute|evaluation|technical assistance|capacity building|implementation|professional development") ~
                "Ed Program Evaluation & Technical Assistance",
            agency %in% c("USAID", "Department of State", "International Assistance Programs") |
                str_detect(desc_l, "educationusa|fulbright|exchange|international education") ~
                "International Education Programs",
            agency %in% c("National Science Foundation", "National Endowment for the Humanities", "Institute of Museum and Library Services") |
                str_detect(desc_l, "research grant|research project|open science|scholarly|humanities") ~
                "General Academic Research (Unspecified Field)",
            agency == "Department Of Education" &
                str_detect(desc_l, "comprehensive center|equity assistance|teacher-?t|teacher training|fiscal equity|technical assistance|capacity building") ~
                "K-12 Systems Support & Teacher Development",
            agency == "Department Of Education" &
                str_detect(rec_l, "american institutes for research|wested|mcrel|child trends|research foundation|education northwest") ~
                "Education Program Evaluation Contractors",
            str_detect(desc_l, "community college|school district|student support|teacher training|college access") ~
                "Higher-Ed / Student Program Operations",
            !str_detect(desc_l, "education|student|school|teacher|college|university|k-12|curriculum|scholarship|fellowship|research") ~
                "Out-of-Scope (Non-Education)",
            TRUE ~ "Other Education-Unspecified"
        )
    )

final_area <- df_sub2 |>
    left_join(
        unknown_refined2 |>
            select(row_id, unknown_type2),
        by = "row_id"
    ) |>
    mutate(
        final_area = case_when(
            str_detect(sub_area2, "Unspecified") & !is.na(unknown_type2) ~ unknown_type2,
            TRUE ~ sub_area2
        )
    )

# -------------------------------------------------------------------
# STEP 6: Output summary tables
# -------------------------------------------------------------------
totals <- df |>
    summarise(
        n_grants = n(),
        claimed_savings_total = sum(savings, na.rm = TRUE),
        matched_verified_rows = sum(!is.na(verified_remaining_proxy)),
        verified_remaining_total = sum(verified_remaining_proxy, na.rm = TRUE)
    )

primary_area_summary <- df |>
    group_by(primary_area) |>
    summarise(
        n_grants = n(),
        claimed_savings = sum(savings, na.rm = TRUE),
        median_claimed_savings = median(savings, na.rm = TRUE),
        verified_rows = sum(!is.na(verified_remaining_proxy)),
        verified_remaining = sum(verified_remaining_proxy, na.rm = TRUE),
        .groups = "drop"
    ) |>
    arrange(desc(claimed_savings))

final_summary <- final_area |>
    group_by(primary_area, final_area) |>
    summarise(
        n_grants = n(),
        claimed_savings = sum(savings, na.rm = TRUE),
        median_savings = median(savings, na.rm = TRUE),
        .groups = "drop"
    ) |>
    arrange(primary_area, desc(claimed_savings))

monthly_area <- df |>
    mutate(month = floor_date(date, "month")) |>
    group_by(month, primary_area) |>
    summarise(
        n_grants = n(),
        claimed_savings = sum(savings, na.rm = TRUE),
        .groups = "drop"
    )

# -------------------------------------------------------------------
# STEP 7: Save outputs
# -------------------------------------------------------------------
run_stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
out_dir <- file.path("data", "processed", paste0("education_cuts_investigation_", run_stamp))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

write_csv(totals, file.path(out_dir, "totals.csv"))
write_csv(primary_area_summary, file.path(out_dir, "primary_area_summary.csv"))
write_csv(final_summary, file.path(out_dir, "final_area_summary.csv"))
write_csv(monthly_area, file.path(out_dir, "monthly_area.csv"))

p_count <- ggplot(monthly_area, aes(month, n_grants, color = primary_area)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.5) +
    labs(
        title = "Education-Related DOGE Grant Cuts Over Time",
        x = NULL,
        y = "Grant count",
        color = "Area"
    ) +
    theme_minimal(base_size = 12)

p_savings <- ggplot(final_summary, aes(reorder(final_area, claimed_savings), claimed_savings, fill = primary_area)) +
    geom_col() +
    coord_flip() +
    scale_y_continuous(labels = dollar) +
    labs(
        title = "Claimed Savings by Education Sub-Area",
        x = NULL,
        y = "DOGE claimed savings",
        fill = "Primary area"
    ) +
    theme_minimal(base_size = 12)

ggsave(filename = file.path(out_dir, "monthly_counts_by_primary_area.png"), plot = p_count, width = 11, height = 6, dpi = 160)
ggsave(filename = file.path(out_dir, "claimed_savings_by_final_area.png"), plot = p_savings, width = 11, height = 8, dpi = 160)

# -------------------------------------------------------------------
# STEP 8: Print key outputs in console
# -------------------------------------------------------------------
cat("\nSaved investigation outputs to:\n", out_dir, "\n", sep = "")
cat("\n=== Totals ===\n")
print(totals)
cat("\n=== Primary Area Summary ===\n")
print(primary_area_summary)
cat("\n=== Final Sub-Area Summary (Top 20 by savings) ===\n")
print(final_summary |>
    arrange(desc(claimed_savings)) |>
    slice_head(n = 20))
