plot_academic_cumulative <- function(academic_monthly_totals, policy_events) {
    library(dplyr)
    library(ggplot2)
    library(scales)

    plot_data <- academic_monthly_totals |>
        arrange(month) |>
        mutate(
            cum_claimed  = cumsum(claimed_savings_total),
            cum_verified = cumsum(verified_remaining_total),
            tip_claimed  = paste0(
                "<b>", format(month, "%b %Y"), "</b><br>",
                "DOGE claimed (cumulative): ",
                dollar(round(cum_claimed / 1e9, 2), suffix = "B")
            ),
            tip_verified = paste0(
                "<b>", format(month, "%b %Y"), "</b><br>",
                "Verified remaining (cumulative): ",
                dollar(round(cum_verified / 1e9, 2), suffix = "B")
            )
        )

    y_lo <- 0
    y_hi <- max(c(plot_data$cum_claimed, plot_data$cum_verified), na.rm = TRUE)

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

    ggplot(plot_data, aes(x = month)) +
        geom_area(aes(y = cum_claimed), fill = "#4E79A7", alpha = 0.20) +
        geom_line(
            aes(y = cum_claimed, color = "DOGE claimed savings",
                text = tip_claimed),
            linewidth = 1.2
        ) +
        geom_line(
            aes(y = cum_verified,
                color = "Verified remaining (USAspending proxy)",
                text = tip_verified),
            linewidth = 1, linetype = "dashed"
        ) +
        geom_vline(
            data = event_data,
            aes(xintercept = event_date),
            color = "gray60",
            linetype = "dashed", linewidth = 0.5, alpha = 0.7
        ) +
        geom_rect(
            data = event_data,
            aes(xmin = event_date - 4, xmax = event_date + 4,
                ymin = y_lo, ymax = y_hi, text = tip_event),
            alpha = 0.01, fill = "gray",
            inherit.aes = FALSE
        ) +
        scale_y_continuous(labels = label_dollar(scale = 1e-9, suffix = "B", accuracy = 1)) +
        scale_color_manual(
            values = c(
                "DOGE claimed savings"                   = "#4E79A7",
                "Verified remaining (USAspending proxy)" = "#E15759"
            )
        ) +
        labs(
            title    = "Cumulative Academic Grant Funding Loss Under DOGE",
            subtitle = "University and research-institution grants since January 20, 2025",
            x        = NULL,
            y        = "Cumulative dollars",
            color    = NULL
        ) +
        theme_minimal(base_size = 12) +
        theme(
            legend.position = "top",
            plot.margin     = margin(t = 20, r = 30, b = 20, l = 20)
        )
}

plot_academic_monthly_bars <- function(academic_monthly_by_domain,
                                       policy_events) {
    library(dplyr)
    library(ggplot2)
    library(scales)
    library(RColorBrewer)

    monthly_total <- academic_monthly_by_domain |>
        group_by(month) |>
        summarise(
            total_claimed = sum(claimed_savings, na.rm = TRUE),
            .groups = "drop"
        ) |>
        mutate(tip_total = paste0(
            "<b>", format(month, "%b %Y"), "</b><br>",
            "Total claimed: ",
            dollar(round(total_claimed / 1e6, 1), suffix = "M")
        ))

    domain_data <- academic_monthly_by_domain |>
        mutate(tip_bar = paste0(
            "<b>", academic_domain, "</b><br>",
            format(month, "%b %Y"), "<br>",
            "Claimed: ", dollar(round(claimed_savings / 1e6, 1), suffix = "M")
        ))

    y_lo <- 0
    y_hi <- max(monthly_total$total_claimed, na.rm = TRUE)

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

    n_domains <- length(unique(domain_data$academic_domain))
    domain_colors <- setNames(
        colorRampPalette(brewer.pal(min(n_domains, 10), "Paired"))(n_domains),
        sort(unique(domain_data$academic_domain))
    )

    ggplot(domain_data, aes(month, claimed_savings,
                            fill = academic_domain, text = tip_bar)) +
        geom_col(alpha = 0.85) +
        geom_line(
            data = monthly_total,
            aes(month, total_claimed, group = 1, text = tip_total),
            inherit.aes = FALSE, color = "black", linewidth = 1
        ) +
        geom_point(
            data = monthly_total,
            aes(month, total_claimed),
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
        scale_fill_manual(values = domain_colors) +
        scale_y_continuous(labels = label_dollar(scale = 1e-9, suffix = "B", accuracy = 1)) +
        labs(
            title    = "Monthly Academic Grant Cuts by Research Domain",
            subtitle = "Stacked bars by domain; black line = total monthly claimed savings",
            x        = NULL,
            y        = "DOGE claimed savings",
            fill     = "Domain"
        ) +
        theme_minimal(base_size = 12) +
        theme(
            legend.position = "right",
            plot.margin     = margin(t = 20, r = 20, b = 30, l = 20)
        )
}

plot_academic_discrepancy <- function(academic_monthly_totals, policy_events) {
    library(dplyr)
    library(ggplot2)
    library(scales)

    plot_data <- academic_monthly_totals |>
        mutate(tip_disc = paste0(
            "<b>", format(month, "%b %Y"), "</b><br>",
            "Discrepancy: ",
            dollar(round(discrepancy_matched / 1e6, 1), suffix = "M"), "<br>",
            "<i>positive = DOGE overstates savings</i>"
        ))

    y_lo <- min(plot_data$discrepancy_matched, na.rm = TRUE)
    y_hi <- max(plot_data$discrepancy_matched, na.rm = TRUE)

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

    ggplot(plot_data, aes(month, discrepancy_matched)) +
        geom_hline(yintercept = 0, linetype = "solid", color = "gray40") +
        geom_line(aes(text = tip_disc), color = "#E15759", linewidth = 1) +
        geom_point(aes(text = tip_disc), color = "#E15759", size = 2) +
        geom_vline(
            data = event_data,
            aes(xintercept = event_date, color = event_group),
            linetype = "dashed", linewidth = 0.5, alpha = 0.7,
            show.legend = TRUE
        ) +
        geom_rect(
            data = event_data,
            aes(xmin = event_date - 4, xmax = event_date + 4,
                ymin = y_lo, ymax = y_hi, text = tip_event),
            alpha = 0.01, fill = "gray",
            inherit.aes = FALSE
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
            title    = "Monthly Discrepancy: DOGE Claims vs. Verified Remaining Obligation",
            subtitle = "Academic grants matched to USAspending only. Positive = DOGE claims exceed verified remaining.",
            x        = NULL,
            y        = "Discrepancy (claimed \u2212 USAspending proxy)",
            color    = "Event type"
        ) +
        theme_minimal(base_size = 12) +
        theme(
            legend.position = "right",
            plot.margin     = margin(t = 10, r = 10, b = 10, l = 10)
        )
}

plot_academic_domain_comparison <- function(academic_domain_summary) {
    library(dplyr)
    library(ggplot2)
    library(scales)
    library(forcats)

    scrutinized <- c(
        "DEI & Social Equity",
        "Environmental Science",
        "Conservation & Sustainability"
    )

    plot_data <- academic_domain_summary |>
        mutate(
            group = if_else(
                academic_domain %in% scrutinized,
                "Politically scrutinized",
                "Other academic domains"
            ),
            academic_domain = fct_reorder(academic_domain, claimed_savings),
            tip_bar = paste0(
                "<b>", academic_domain, "</b><br>",
                "Claimed savings: ",
                dollar(round(claimed_savings / 1e9, 2), suffix = "B"), "<br>",
                "Grants terminated: ", scales::comma(n_grants)
            )
        )

    ggplot(plot_data, aes(academic_domain, claimed_savings,
                          fill = group, text = tip_bar)) +
        geom_col() +
        coord_flip() +
        scale_fill_manual(
            values = c(
                "Politically scrutinized" = "#E15759",
                "Other academic domains"  = "#BAB0AC"
            )
        ) +
        scale_y_continuous(labels = label_dollar(scale = 1e-9, suffix = "B", accuracy = 1)) +
        labs(
            title    = "DOGE Grant Cuts by Academic Research Domain",
            subtitle = "Red = fields targeted in executive orders / agency communications. Hover for grant counts.",
            x        = NULL,
            y        = "DOGE claimed savings (billions)",
            fill     = NULL
        ) +
        theme_minimal(base_size = 12) +
        theme(
            legend.position = "bottom",
            plot.margin     = margin(t = 20, r = 30, b = 20, l = 20)
        )
}
