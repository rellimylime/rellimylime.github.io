build_education_scope <- function(doge_grants, usaspending_awards, start_date = as.Date("2025-01-20")) {
    library(dplyr)
    library(stringr)

    core_agencies <- c(
        "Department Of Education",
        "National Science Foundation",
        "National Endowment for the Humanities",
        "National Endowment for the Arts",
        "Institute of Museum and Library Services"
    )

    doge_grants |>
        mutate(
            row_id = row_number(),
            date = as.Date(date),
            savings = as.numeric(savings),
            value = as.numeric(value),
            usaspending_award_id = str_match(link, "usaspending\\.gov/award/([^/?#]+)")[, 2]
        ) |>
        filter(!is.na(date), date >= start_date) |>
        left_join(usaspending_awards, by = "usaspending_award_id") |>
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
}

classify_primary_education_area <- function(education_scope) {
    library(dplyr)
    library(stringr)

    education_scope |>
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
}

classify_education_subareas <- function(education_primary) {
    library(dplyr)
    library(stringr)

    education_primary |>
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
}

refine_unknown_subareas <- function(education_subareas) {
    library(dplyr)
    library(stringr)

    education_subareas |>
        filter(str_detect(sub_area2, "Unspecified")) |>
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
}

build_final_education_area <- function(education_subareas, education_unknown_refined) {
    library(dplyr)
    library(stringr)

    education_subareas |>
        left_join(
            education_unknown_refined |>
                select(row_id, unknown_type2),
            by = "row_id"
        ) |>
        mutate(
            final_area = case_when(
                str_detect(sub_area2, "Unspecified") & !is.na(unknown_type2) ~ unknown_type2,
                TRUE ~ sub_area2
            )
        )
}

summarize_education_totals <- function(education_scope) {
    library(dplyr)

    education_scope |>
        summarise(
            n_grants = n(),
            claimed_savings_total = sum(savings, na.rm = TRUE),
            matched_verified_rows = sum(!is.na(verified_remaining_proxy)),
            verified_remaining_total = sum(verified_remaining_proxy, na.rm = TRUE)
        )
}

summarize_primary_areas <- function(education_primary) {
    library(dplyr)

    education_primary |>
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
}

summarize_money_by_group <- function(education_primary) {
    library(dplyr)

    education_primary |>
        group_by(primary_area) |>
        summarise(
            n_grants = n(),
            matched_verified_rows = sum(!is.na(verified_remaining_proxy)),
            claimed_savings_total = sum(savings, na.rm = TRUE),
            claimed_savings_matched = sum(savings[!is.na(verified_remaining_proxy)], na.rm = TRUE),
            verified_remaining_total = sum(verified_remaining_proxy, na.rm = TRUE),
            discrepancy_matched = claimed_savings_matched - verified_remaining_total,
            .groups = "drop"
        ) |>
        arrange(desc(claimed_savings_total))
}

summarize_final_areas <- function(education_final) {
    library(dplyr)

    education_final |>
        group_by(primary_area, final_area) |>
        summarise(
            n_grants = n(),
            claimed_savings = sum(savings, na.rm = TRUE),
            median_savings = median(savings, na.rm = TRUE),
            .groups = "drop"
        ) |>
        arrange(primary_area, desc(claimed_savings))
}

summarize_discrepancy_over_time <- function(education_primary) {
    library(dplyr)
    library(lubridate)

    education_primary |>
        mutate(month = floor_date(date, "month")) |>
        group_by(month, primary_area) |>
        summarise(
            n_grants = n(),
            matched_verified_rows = sum(!is.na(verified_remaining_proxy)),
            claimed_savings_matched = sum(savings[!is.na(verified_remaining_proxy)], na.rm = TRUE),
            verified_remaining_total = sum(verified_remaining_proxy, na.rm = TRUE),
            discrepancy_matched = claimed_savings_matched - verified_remaining_total,
            .groups = "drop"
        )
}

summarize_monthly_totals <- function(education_primary) {
    library(dplyr)
    library(lubridate)

    education_primary |>
        mutate(month = floor_date(date, "month")) |>
        group_by(month) |>
        summarise(
            n_grants = n(),
            matched_verified_rows = sum(!is.na(verified_remaining_proxy)),
            claimed_savings_total = sum(savings, na.rm = TRUE),
            claimed_savings_matched = sum(savings[!is.na(verified_remaining_proxy)], na.rm = TRUE),
            verified_remaining_total = sum(verified_remaining_proxy, na.rm = TRUE),
            discrepancy_matched = claimed_savings_matched - verified_remaining_total,
            .groups = "drop"
        )
}

build_policy_events <- function() {
    library(tibble)

    # Events selected to explain major shifts in education-related grant activity.
    tribble(
        ~event_date, ~event_group, ~event_label, ~event_short, ~plot_label, ~ref_id, ~ref_short, ~ref_url,
        as.Date("2025-01-20"), "White House", "DOGE established; foreign aid review EO", "Jan 20: DOGE + aid review EO", TRUE, "S1", "White House EO on foreign aid review (Jan 20, 2025)", "https://www.whitehouse.gov/presidential-actions/2025/01/reevaluating-and-realigning-united-states-foreign-aid/",
        as.Date("2025-01-27"), "OMB", "OMB M-25-13 pause memo", "Jan 27: OMB pause memo", FALSE, "S2", "OMB M-25-13 temporary pause memo", "https://www.whitehouse.gov/wp-content/uploads/2025/03/M-25-13-Temporary-Pause-to-Review-Agency-Grant-Loan-and-Other-Financial-Assistance-Programs.pdf",
        as.Date("2025-01-29"), "OMB", "OMB M-25-13 rescinded (M-25-14)", "Jan 29: OMB rescission", FALSE, "S3", "OMB M-25-14 rescission memo", "https://www.whitehouse.gov/wp-content/uploads/2025/03/M-25-14-Rescission-of-M-25-13.pdf",
        as.Date("2025-02-13"), "Education", "ED: $350M+ cancellations", "Feb 13: ED $350M+ cuts", TRUE, "S4", "ED press release: additional $350M canceled", "https://www.ed.gov/about/news/press-release/us-department-of-education-cancels-additional-350-million-woke-spending",
        as.Date("2025-02-17"), "Education", "ED: $600M+ teacher-training cuts", "Feb 17: ED $600M+ teacher cuts", FALSE, "S5", "ED press release: $600M teacher-training cuts", "https://www.ed.gov/about/news/press-release/us-department-of-education-cuts-over-600-million-divisive-teacher-training-grants",
        as.Date("2025-02-19"), "Education", "ED: $226M comprehensive center cuts", "Feb 19: ED $226M cuts", FALSE, "S6", "ED press release: comprehensive centers cuts", "https://www.ed.gov/about/news/press-release/us-department-of-education-cancels-divisive-and-wasteful-grants-under-comprehensive-centers-program",
        as.Date("2025-02-26"), "White House", "DOGE cost-efficiency EO (30-day review window)", "Feb 26: DOGE efficiency EO", TRUE, "S7", "White House DOGE cost-efficiency EO (Feb 26, 2025)", "https://www.whitehouse.gov/presidential-actions/2025/02/implementing-the-presidents-department-of-government-efficiency-cost-efficiency-initiative/",
        as.Date("2025-03-06"), "Courts", "Court extends block on broad freeze", "Mar 6: court extends freeze block", FALSE, "S8", "AP: court extends block on broad freeze", "https://apnews.com/article/federal-grants-loans-spending-freeze-trump-administration-6620ef49a21f88f83f8f998805b92677",
        as.Date("2025-03-10"), "State/USAID", "State: ~83% of USAID programs cut", "Mar 10: State says 83% USAID cut", TRUE, "S9", "AP: State says ~83% USAID programs cut", "https://apnews.com/article/trump-musk-rubio-usaid-foreign-aid-bf442d62af67918a6fc5eee839074601",
        as.Date("2025-04-02"), "NEH/IMLS", "NEH/IMLS termination wave reported", "Apr 2: NEH/IMLS terminations", TRUE, "S10", "NEH/IMLS termination wave (court filing context)", "https://www.govinfo.gov/content/pkg/USCOURTS-ord-3_25-cv-00829/pdf/USCOURTS-ord-3_25-cv-00829-0.pdf"
    )
}

build_event_caption <- function(event_data, width = 115) {
    library(dplyr)
    library(stringr)

    event_lines <- event_data |>
        arrange(event_id) |>
        mutate(line = paste0(event_id, ". ", dplyr::coalesce(event_short, event_label), " [", ref_id, "]")) |>
        pull(line) |>
        vapply(\(x) stringr::str_wrap(x, width = width, exdent = 2), character(1))

    source_lines <- event_data |>
        distinct(ref_id, ref_short) |>
        mutate(ref_num = as.integer(gsub("^S", "", ref_id))) |>
        arrange(ref_num) |>
        mutate(line = paste0(ref_id, ": ", ref_short)) |>
        pull(line) |>
        vapply(\(x) stringr::str_wrap(x, width = width, exdent = 2), character(1))

    c(
        "Events (numbers match dashed lines):",
        event_lines,
        "Sources:",
        source_lines
    ) |>
        paste(collapse = "\n")
}

summarize_monthly_areas <- function(education_primary) {
    library(dplyr)
    library(lubridate)

    education_primary |>
        mutate(month = floor_date(date, "month")) |>
        group_by(month, primary_area) |>
        summarise(
            n_grants = n(),
            claimed_savings = sum(savings, na.rm = TRUE),
            .groups = "drop"
        )
}

plot_money_by_group <- function(education_group_money_summary) {
    library(dplyr)
    library(ggplot2)
    library(scales)
    library(tidyr)

    area_order <- education_group_money_summary |>
        arrange(claimed_savings_matched) |>
        pull(primary_area)

    plot_data <- education_group_money_summary |>
        select(primary_area, claimed_savings_matched, verified_remaining_total) |>
        pivot_longer(
            cols = c(claimed_savings_matched, verified_remaining_total),
            names_to = "series",
            values_to = "amount"
        ) |>
        mutate(
            primary_area = factor(primary_area, levels = area_order),
            series = dplyr::recode(
                series,
                claimed_savings_matched = "DOGE claimed savings (matched rows)",
                verified_remaining_total = "USAspending remaining obligation proxy"
            )
        )

    ggplot(plot_data, aes(primary_area, amount, fill = series)) +
        geom_col(position = "dodge") +
        coord_flip() +
        scale_y_continuous(labels = dollar) +
        labs(
            title = "Education Cuts by Group: DOGE Claims vs USAspending Proxy",
            x = NULL,
            y = "Dollars",
            fill = NULL
        ) +
        theme_minimal(base_size = 12)
}

plot_monthly_primary_counts <- function(education_monthly_summary) {
    library(ggplot2)

    ggplot(education_monthly_summary, aes(month, n_grants, color = primary_area)) +
        geom_line(linewidth = 0.9) +
        geom_point(size = 1.5) +
        labs(
            title = "Education-Related DOGE Grant Cuts Over Time",
            x = NULL,
            y = "Grant count",
            color = "Area"
        ) +
        theme_minimal(base_size = 12)
}

plot_monthly_stacked_bar_line <- function(education_monthly_summary,
                                          policy_events) {
    library(dplyr)
    library(ggplot2)
    library(scales)

    monthly_total <- education_monthly_summary |>
        group_by(month) |>
        summarise(
            total_claimed_savings = sum(claimed_savings, na.rm = TRUE),
            .groups = "drop"
        ) |>
        mutate(tip_total = paste0(
            "<b>", format(month, "%b %Y"), "</b><br>",
            "Total claimed: ",
            dollar(round(total_claimed_savings / 1e6, 1), suffix = "M")
        ))

    bar_data <- education_monthly_summary |>
        mutate(tip_bar = paste0(
            "<b>", primary_area, "</b><br>",
            format(month, "%b %Y"), "<br>",
            "Claimed: ", dollar(round(claimed_savings / 1e6, 1), suffix = "M"),
            "<br>Grants: ", scales::comma(n_grants)
        ))

    y_lo <- 0
    y_hi <- max(monthly_total$total_claimed_savings, na.rm = TRUE)

    event_data <- policy_events |>
        arrange(event_date) |>
        mutate(
            event_id  = row_number(),
            tip_event = paste0(
                "<b>", format(event_date, "%B %d, %Y"), "</b><br>",
                event_label
            ),
            y_lo = y_lo,
            y_hi = y_hi
        )

    ggplot(bar_data, aes(month, claimed_savings,
                         fill = primary_area, text = tip_bar)) +
        geom_col(alpha = 0.85) +
        geom_line(
            data = monthly_total,
            aes(month, total_claimed_savings, group = 1, text = tip_total),
            inherit.aes = FALSE, color = "black", linewidth = 1
        ) +
        geom_point(
            data = monthly_total,
            aes(month, total_claimed_savings),
            inherit.aes = FALSE, color = "black", size = 1.8
        ) +
        geom_vline(
            data = event_data,
            aes(xintercept = event_date),
            linetype = "dashed", linewidth = 0.4, color = "gray40", alpha = 0.7
        ) +
        geom_rect(
            data = event_data,
            aes(xmin = event_date - 4, xmax = event_date + 4,
                ymin = y_lo, ymax = y_hi, text = tip_event),
            alpha = 0.01, fill = "gray",
            inherit.aes = FALSE
        ) +
        scale_y_continuous(labels = dollar) +
        labs(
            title    = "Monthly Education-Sector Grant Cuts by Area",
            subtitle = "Stacked bars by primary area; black line = total monthly claimed savings",
            x        = NULL,
            y        = "DOGE claimed savings",
            fill     = "Primary area"
        ) +
        theme_minimal(base_size = 12) +
        theme(
            legend.position = "right",
            plot.margin     = margin(t = 10, r = 10, b = 10, l = 10)
        )
}

plot_monthly_claimed_with_events <- function(education_monthly_totals, policy_events) {
    library(dplyr)
    library(ggplot2)
    library(scales)

    event_data <- policy_events |>
        arrange(event_date) |>
        mutate(event_id = row_number())

    ggplot(education_monthly_totals, aes(month, claimed_savings_total)) +
        geom_col(fill = "#4E79A7", alpha = 0.85) +
        geom_vline(
            data = event_data,
            aes(xintercept = event_date, color = event_group),
            linetype = "dashed",
            linewidth = 0.5,
            alpha = 0.8,
            show.legend = TRUE
        ) +
        scale_y_continuous(labels = dollar) +
        scale_color_manual(
            values = c(
                "White House" = "#F28E2B", "OMB" = "#76B7B2",
                "Education"   = "#59A14F", "Courts" = "#B07AA1",
                "State/USAID" = "#9C755F", "NEH/IMLS" = "#BAB0AC"
            )
        ) +
        labs(
            title = "Monthly Education-Related DOGE Claimed Savings",
            subtitle = "Policy and legal events shown as dashed lines",
            x = NULL,
            y = "DOGE claimed savings",
            color = "Event type"
        ) +
        theme_minimal(base_size = 12) +
        theme(
            plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
        )
}

plot_discrepancy_over_time <- function(education_discrepancy_monthly) {
    library(ggplot2)
    library(scales)

    ggplot(education_discrepancy_monthly, aes(month, discrepancy_matched, color = primary_area)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
        geom_line(linewidth = 0.9) +
        geom_point(size = 1.4) +
        scale_y_continuous(labels = dollar) +
        labs(
            title = "Monthly Discrepancy: DOGE Claims Minus USAspending Proxy",
            subtitle = "Calculated on rows with USAspending matches",
            x = NULL,
            y = "Discrepancy (claimed - proxy remaining obligation)",
            color = "Group"
        ) +
        theme_minimal(base_size = 12)
}

plot_discrepancy_with_events <- function(education_monthly_totals, policy_events) {
    library(dplyr)
    library(ggplot2)
    library(scales)

    event_data <- policy_events |>
        arrange(event_date) |>
        mutate(event_id = row_number())

    ggplot(education_monthly_totals, aes(month, discrepancy_matched)) +
        geom_hline(yintercept = 0, linetype = "solid", color = "gray40") +
        geom_line(color = "#E15759", linewidth = 1) +
        geom_point(color = "#E15759", size = 1.8) +
        geom_vline(
            data = event_data,
            aes(xintercept = event_date, color = event_group),
            linetype = "dashed",
            linewidth = 0.5,
            alpha = 0.8,
            show.legend = TRUE
        ) +
        scale_y_continuous(labels = dollar) +
        scale_color_manual(
            values = c(
                "White House" = "#F28E2B", "OMB" = "#76B7B2",
                "Education"   = "#59A14F", "Courts" = "#B07AA1",
                "State/USAID" = "#9C755F", "NEH/IMLS" = "#BAB0AC"
            )
        ) +
        labs(
            title = "Monthly Discrepancy: DOGE Claims Minus USAspending Proxy",
            subtitle = "Calculated on matched rows only",
            x = NULL,
            y = "Discrepancy (claimed - proxy remaining obligation)",
            color = "Event type"
        ) +
        theme_minimal(base_size = 12) +
        theme(
            plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
        )
}

plot_final_area_savings <- function(education_final_summary) {
    library(dplyr)
    library(ggplot2)
    library(scales)

    plot_data <- education_final_summary |>
        mutate(
            tip_bar = paste0(
                "<b>", final_area, "</b><br>",
                "Primary area: ", primary_area, "<br>",
                "Claimed savings: ",
                dollar(round(claimed_savings / 1e9, 2), suffix = "B"), "<br>",
                "Grants: ", scales::comma(n_grants)
            )
        )

    ggplot(
        plot_data,
        aes(reorder(final_area, claimed_savings), claimed_savings,
            fill = primary_area, text = tip_bar)
    ) +
        geom_col() +
        coord_flip() +
        scale_y_continuous(
            labels = dollar_format(scale = 1e-9, suffix = "B")
        ) +
        labs(
            title    = "DOGE Claimed Savings by Education Sub-Area",
            subtitle = "Hover bars for grant counts. Color = primary classification.",
            x        = NULL,
            y        = "DOGE claimed savings (billions)",
            fill     = "Primary area"
        ) +
        theme_minimal(base_size = 12) +
        theme(
            legend.position = "right",
            plot.margin     = margin(t = 10, r = 10, b = 10, l = 10)
        )
}
