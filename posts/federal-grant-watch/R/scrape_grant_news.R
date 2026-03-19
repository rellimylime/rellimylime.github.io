scrape_one_govexec_page <- function(page_num = 1) {
    library(rvest)
    library(dplyr)
    library(stringr)
    library(lubridate)    

    url <- paste0("https://www.govexec.com/topic/doge/more/?page=", page_num)
    page <- read_html(url)

    titles <- page |> html_elements("a.river-item-hed-link") |> html_text2() |> str_squish()

    datetimes <- page |> html_elements("li.story-meta-date time") |> html_attr("datetime")

    links <- page |> html_elements("a.river-item-hed-link") |> html_attr("href")

    if (length(titles) == 0L || length(datetimes) == 0L) {
        return(tibble())
    }

    n <- min(length(titles), length(datetimes), length(links))

    tibble(
        title = titles[seq_len(n)],
        date = ymd_hms(datetimes[seq_len(n)]),
        url = ifelse(
            str_starts(links[seq_len(n)], "http"),
            links[seq_len(n)],
            paste0("https://www.govexec.com", links[seq_len(n)])
        )
    )
}

scrape_grant_news <- function(max_pages = 3, pause_seconds = 0.4) {
  library(dplyr)
  library(purrr)

  max_pages <- as.integer(max_pages)
  if (is.na(max_pages) || max_pages < 1) {
    stop("`max_pages` must be an integer >= 1.")
  }
  if (!is.numeric(pause_seconds) || length(pause_seconds) != 1 || is.na(pause_seconds) || pause_seconds < 0) {
    stop("`pause_seconds` must be a single number >= 0.")
  }

  pages <- map(
    seq_len(max_pages),
    function(i) {
        message("Scraping page ", i, "...")
        out <- tryCatch(scrape_one_govexec_page(i), error = function(e) {
            message("Error on page ", i, ": ", e$message)
            tibble::tibble()
        })
        Sys.sleep(pause_seconds)
        out
    }
  )

    bind_rows(pages) |>
    distinct(url, .keep_all = TRUE) |>
    arrange(desc(date))
}
