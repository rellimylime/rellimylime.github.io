fetch_federal_register_events <- function(
    start_date    = as.Date("2025-01-20"),
    search_terms  = c("DOGE", "Department of Government Efficiency", "grant freeze"),
    doc_types     = c("PRESDOCU", "NOTICE", "RULE"),
    per_page      = 20,
    max_pages     = 5,
    pause_seconds = 0.3
) {
    library(httr2)
    library(dplyr)
    library(purrr)
    library(lubridate)

    base_url <- "https://www.federalregister.gov/api/v1/documents.json"

    fetch_one_term <- function(term) {
        results <- list()

        for (page in seq_len(max_pages)) {
            req <- request(base_url) |>
                req_url_query(
                    `conditions[term]`                  = term,
                    `conditions[publication_date][gte]` = format(start_date, "%Y-%m-%d"),
                    `conditions[type][]`                = doc_types,
                    `fields[]`                          = c("title", "publication_date",
                                                            "document_number", "type",
                                                            "abstract", "html_url"),
                    per_page                            = per_page,
                    page                                = page,
                    order                               = "oldest",
                    .multi                              = "explode"
                )

            body <- tryCatch(
                req |> req_perform() |> resp_body_json(simplifyVector = FALSE),
                error = function(e) {
                    message("Federal Register API error (term='", term, "', page=", page, "): ", e$message)
                    NULL
                }
            )

            if (is.null(body) || length(body$results) == 0L) break

            page_rows <- map_dfr(body$results, function(r) {
                tibble(
                    publication_date  = as.Date(r$publication_date),
                    title             = r$title %||% NA_character_,
                    document_number   = r$document_number %||% NA_character_,
                    type              = r$type %||% NA_character_,
                    abstract          = r$abstract %||% NA_character_,
                    html_url          = r$html_url %||% NA_character_
                )
            })

            results[[page]] <- page_rows
            Sys.sleep(pause_seconds)

            if (page >= body$total_pages) break
        }

        bind_rows(results)
    }

    all_results <- map_dfr(search_terms, fetch_one_term)

    all_results |>
        filter(!is.na(document_number)) |>
        distinct(document_number, .keep_all = TRUE) |>
        filter(publication_date >= start_date) |>
        arrange(publication_date)
}
