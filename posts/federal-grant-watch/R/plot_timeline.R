plot_termination_timeline <- function(combined, policy_events) {
    library(dplyr)
    library(ggplot2)
    library(lubridate)
    library(scales)

    grant_monthly <- combined |>
        filter(source == "doge_grants") |>
        mutate(month = floor_date(as.Date(date), "month")) |>
        group_by(month) |>
        summarise(
            claimed_savings = sum(savings, na.rm = TRUE),
            .groups = "drop"
        ) |>
        mutate(tip_bar = paste0(
            "<b>", format(month, "%b %Y"), "</b><br>",
            "Grant savings claimed: ",
            dollar(round(claimed_savings / 1e9, 2), suffix = "B")
        ))

    news_monthly <- combined |>
        filter(source == "govexec_news") |>
        mutate(month = floor_date(as.Date(date), "month")) |>
        count(month, name = "n_articles") |>
        mutate(tip_news = paste0(
            "<b>", format(month, "%b %Y"), "</b><br>",
            "News articles: ", n_articles
        ))

    # Scale factor to align secondary axis with primary
    scale_factor <- max(grant_monthly$claimed_savings, na.rm = TRUE) /
        max(news_monthly$n_articles, na.rm = TRUE)

    y_lo <- 0
    y_hi <- max(grant_monthly$claimed_savings, na.rm = TRUE)

    event_data <- policy_events |>
        arrange(event_date) |>
        mutate(
            tip_event = paste0(
                "<b>", format(event_date, "%B %d, %Y"), "</b><br>",
                event_label
            ),
            y_lo = y_lo,
            y_hi = y_hi
        )

    news_scaled <- news_monthly |>
        mutate(n_scaled = n_articles * scale_factor)

    ggplot() +
        geom_col(
            data = grant_monthly,
            aes(x = month, y = claimed_savings, text = tip_bar),
            fill = "#4E79A7", alpha = 0.75, width = 25
        ) +
        geom_line(
            data = news_scaled,
            aes(x = month, y = n_scaled, text = tip_news,
                color = "GovExec articles (right axis)"),
            linewidth = 1.1
        ) +
        geom_point(
            data = news_scaled,
            aes(x = month, y = n_scaled, text = tip_news),
            color = "#E15759", size = 2.5
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
        scale_y_continuous(
            labels = dollar_format(scale = 1e-9, suffix = "B"),
            sec.axis = sec_axis(
                ~ . / scale_factor,
                name = "GovExec articles per month",
                labels = label_number(accuracy = 1)
            )
        ) +
        scale_color_manual(values = c("GovExec articles (right axis)" = "#E15759")) +
        labs(
            title = "Grant Terminations and News Coverage Tracked Together",
            subtitle = "Bars: DOGE claimed savings (left axis). Line: GovExec articles about DOGE (right axis).",
            x = NULL,
            y = "DOGE claimed savings",
            color = NULL
        ) +
        theme_minimal(base_size = 12) +
        theme(
            legend.position = "top",
            axis.title.y.right = element_text(color = "#E15759"),
            axis.text.y.right  = element_text(color = "#E15759"),
            plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
        )
}
