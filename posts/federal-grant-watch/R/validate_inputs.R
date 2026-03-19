validate_inputs <- function(doge_grants, news_articles) {
  required_doge <- c("date", "agency", "recipient", "value", "savings")
  required_news <- c("title", "date", "url")

  missing_doge <- setdiff(required_doge, names(doge_grants))
  missing_news <- setdiff(required_news, names(news_articles))

  if (length(missing_doge) > 0) {
    stop("doge_grants missing columns: ", paste(missing_doge, collapse = ", "))
  }
  if (length(missing_news) > 0) {
    stop("news_articles missing columns: ", paste(missing_news, collapse = ", "))
  }
  if (!inherits(doge_grants$date, "Date")) {
    stop("doge_grants$date must be Date.")
  }
  if (!inherits(news_articles$date, c("POSIXct", "POSIXt", "Date"))) {
    stop("news_articles$date must be POSIXct/POSIXt/Date.")
  }
}
