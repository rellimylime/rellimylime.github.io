combine_sources <- function(doge_grants, news_articles, usaspending_awards) {
    library(dplyr)
    library(stringr)

    validate_inputs(doge_grants, news_articles)

    grants_tbl <- doge_grants |>
        mutate(usaspending_award_id = str_match(link, "usaspending\\.gov/award/([^/?#]+)")[,2]) |>
        left_join(usaspending_awards, by = "usaspending_award_id") |>
        transmute(
        date = as.Date(date),
        source = "doge_grants",
        title = str_squish(paste(agency, recipient, sep = " - ")),
        value = as.numeric(value),
        savings = as.numeric(savings),
        url = link,
        usaspending_award_id,
        usaspending_total_obligation,
        usaspending_total_outlay,
        discrepancy = savings - usaspending_total_obligation
        )

    news_tbl <- news_articles |>
        transmute(
        date = as.Date(date),
        source = "govexec_news",
        title = str_squish(title),
        value = NA_real_,
        savings = NA_real_,
        url = url,
        usaspending_award_id = NA_character_,
        usaspending_total_obligation = NA_real_,
        usaspending_total_outlay = NA_real_,
        discrepancy = NA_real_
        )

    bind_rows(grants_tbl, news_tbl) |>
        filter(!is.na(date), !is.na(title), title != "")
}
