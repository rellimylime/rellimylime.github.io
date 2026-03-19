build_academic_scope_full <- function(doge_grants, usaspending_awards, start_date = as.Date("2025-01-20")) {
    library(dplyr)
    library(stringr)

    tagged <- tag_academic_scope(doge_grants, start_date)

    tagged |>
        filter(academic_any) |>
        mutate(
            row_id = row_number(),
            usaspending_award_id = str_match(link, "usaspending\\.gov/award/([^/?#]+)")[, 2]
        ) |>
        left_join(usaspending_awards, by = "usaspending_award_id") |>
        mutate(
            verified_remaining_proxy = pmax(usaspending_total_obligation - usaspending_total_outlay, 0),
            text = str_to_lower(str_c(
                coalesce(agency, ""),
                coalesce(recipient, ""),
                coalesce(description, ""),
                sep = " || "
            ))
        )
}

classify_academic_domain <- function(academic_scope_full) {
    library(dplyr)
    library(stringr)

    academic_scope_full |>
        mutate(
            academic_domain = case_when(
                # DEI checked first — many grants mention diversity in a secondary context
                # (e.g. biodiversity), so we require co-occurrence with equity/inclusion signals
                str_detect(text, "\\bdiversity\\b") &
                    str_detect(text, "\\bequity\\b|\\binclusion\\b|\\bdei\\b|\\bunderrepresented\\b|\\bbelonging\\b") ~ "DEI & Social Equity",
                str_detect(text, "\\bdei\\b|\\bminority students?\\b|\\bwomen in stem\\b|\\blgbtq\\b|\\bracial justice\\b|\\bhistorically underrepresented\\b") ~ "DEI & Social Equity",
                str_detect(text, "\\bclimate\\b|\\becolog|\\batmospher|\\bocean\\b|\\bwatershed\\b|\\bbiodiversity\\b|\\bpollution\\b|\\benvironmental monitoring\\b|\\bearth science\\b") ~ "Environmental Science",
                str_detect(text, "\\bconservation\\b|\\bsustainab|\\brenewable energy\\b|\\bhabitat\\b|\\bendangered species\\b|\\bclean energy\\b|\\bwildlife\\b") ~ "Conservation & Sustainability",
                str_detect(text, "\\bastrophysics\\b|\\bastronom|\\bnuclear\\b|\\bspace science\\b|\\bparticle physics\\b|\\bcosmolog|\\btelescope\\b|\\bobservator") ~ "Astrophysics, Space & Nuclear",
                str_detect(text, "\\bbiology\\b|\\bbiomedical\\b|\\bmedicine\\b|\\bhealth research\\b|\\bgenomics\\b|\\bneuroscience\\b|\\bcancer\\b|\\bpublic health\\b|\\bepidemiolog") ~ "Biological & Health Sciences",
                str_detect(text, "\\bengineering\\b|\\bmaterials science\\b|\\bphysics\\b|\\bchemistry\\b|\\bsemiconductor\\b|\\bmanufacturing\\b|\\brobotics\\b|\\bcomputer science\\b") ~ "Engineering & Physical Sciences",
                str_detect(text, "\\bsocial science\\b|\\bpsycholog|\\beducation research\\b|\\bpolitical science\\b|\\bsociology\\b|\\banthropolog") ~ "Social Sciences & Ed Research",
                str_detect(text, "\\bbusiness\\b|\\bEntrepreneurship\\b|\\bfinance\\b|\\bmarket\\b|\\bcommerce\\b") ~ "Business & Economics",
                str_detect(text, "\\barts\\b|\\bhumanities\\b|\\bliterature\\b|\\bhistory\\b|\\bmuseum\\b|\\bcultural\\b") ~ "Arts, Humanities & Culture",
                # Additional categories to reduce the "Other Academic" residual
                str_detect(text, "\\bclinical\\b|\\bpatient\\b|\\bcommunity health\\b|\\bhospital\\b|\\bnursing\\b|\\bpharmacy\\b|\\bpharmacolog|\\brehabilitation\\b|\\bsubstance abuse\\b|\\baddiction\\b") ~ "Clinical & Community Health",
                str_detect(text, "\\bstatistics\\b|\\bstatistical\\b|\\bmathematics\\b|\\bmathematical\\b|\\bdata science\\b|\\bmachine learning\\b|\\bartificial intelligence\\b|\\bcomputational science\\b|\\binformation science\\b") ~ "Mathematics, Statistics & Data Science",
                str_detect(text, "\\bagriculture\\b|\\bagricultural\\b|\\bfarming\\b|\\brural development\\b|\\bfood science\\b|\\bfood security\\b|\\baquaculture\\b|\\bveterinary\\b|\\banimal science\\b") ~ "Agriculture & Food Science",
                str_detect(text, "\\blaw school\\b|\\blegal\\b|\\bjurisprudence\\b|\\bpublic policy\\b|\\bpublic administration\\b|\\bgovernance\\b|\\bcriminal justice\\b") ~ "Law, Policy & Governance",
                str_detect(text, "\\binternational development\\b|\\bglobal health\\b|\\bhumanitarian\\b|\\bforeign assistance\\b|\\bpeace\\b|\\bconflict\\b|\\brefugee\\b") ~ "International & Global Programs",
                TRUE ~ "Other Academic"
            )
        )
}

summarize_academic_domain <- function(academic_domain_classified) {
    library(dplyr)

    academic_domain_classified |>
        group_by(academic_domain) |>
        summarise(
            n_grants = n(),
            claimed_savings = sum(savings, na.rm = TRUE),
            verified_remaining = sum(verified_remaining_proxy, na.rm = TRUE),
            discrepancy = sum(savings[!is.na(verified_remaining_proxy)], na.rm = TRUE) -
                sum(verified_remaining_proxy, na.rm = TRUE),
            .groups = "drop"
        ) |>
        arrange(desc(claimed_savings))
}

summarize_academic_monthly_totals <- function(academic_domain_classified) {
    library(dplyr)
    library(lubridate)

    academic_domain_classified |>
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

summarize_academic_monthly_by_domain <- function(academic_domain_classified) {
    library(dplyr)
    library(lubridate)

    academic_domain_classified |>
        mutate(month = floor_date(date, "month")) |>
        group_by(month, academic_domain) |>
        summarise(
            n_grants = n(),
            claimed_savings = sum(savings, na.rm = TRUE),
            .groups = "drop"
        )
}
