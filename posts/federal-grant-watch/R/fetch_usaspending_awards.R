fetch_usaspending_awards <- function(doge_grants, max_ids = 200, pause_seconds = 0.15) {
    library(dplyr)
    library(stringr)
    library(httr2)
    library(purrr)
    library(tibble)

    ids <- doge_grants |>
        mutate(usaspending_award_id = str_match(link, "usaspending\\.gov/award/([^/?#]+)")[,2]) |>
        filter(!is.na(usaspending_award_id)) |>
        distinct(usaspending_award_id) |>
        slice_head(n = max_ids) |>
        pull(usaspending_award_id)

    map_dfr(ids, function(id) {
        Sys.sleep(pause_seconds)
        b <- tryCatch(
        request(paste0("https://api.usaspending.gov/api/v2/awards/", id, "/")) |>
            req_perform() |>
            resp_body_json(simplifyVector = TRUE),
        error = function(e) NULL
        )

        tibble(
        usaspending_award_id = id,
        usaspending_total_obligation = if (is.null(b)) NA_real_ else b$total_obligation,
        usaspending_total_outlay = if (is.null(b)) NA_real_ else b$total_outlay,
        usaspending_recipient = if (is.null(b)) NA_character_ else b$recipient$recipient_name
        )
    })
}
