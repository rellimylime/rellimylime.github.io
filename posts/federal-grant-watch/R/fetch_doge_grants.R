fetch_doge_grants <- function(per_page = 100, max_pages = NULL, pause_seconds = 0.5) {
    library(httr2)
    library(dplyr)
    library(lubridate)

    base_url <- "https://api.doge.gov/savings/grants"
    all_results <- list()
    page <- 1
    
    first_body <- request(base_url) |> 
    req_url_query(page = 1, per_page = per_page) |>
    req_perform() |>
    resp_body_json()

    total_pages <- as.integer(first_body$meta$pages)
    if (is.null(total_pages) || is.na(total_pages) || total_pages < 1) {
        total_pages <- 1L
    }

    pages_to_fetch <- if (is.null(max_pages)) total_pages else min(as.integer(max_pages), total_pages)

    all_results <- vector("list", pages_to_fetch)
    all_results[[1]] <- bind_rows(first_body$result$grants)

    if (pages_to_fetch > 1) {
        for (page in 2:pages_to_fetch) {
            message("Fetching DOGE grants page ", page, " of ", pages_to_fetch, "...")
            body <- request(base_url) |>
                req_url_query(page = page, per_page = per_page) |>
                req_perform() |>
                resp_body_json()

            grants <- body$result$grants
            if (length(grants) == 0L) break

            all_results[[page]] <- bind_rows(grants)
            Sys.sleep(pause_seconds)
    }
}

bind_rows(all_results) |> 
    mutate(
        date = mdy(date),
        value = suppressWarnings(as.numeric(value)),
        savings = suppressWarnings(as.numeric(savings))
    )
}
