tag_academic_scope <- function(doge_grants, start_date = as.Date("2025-01-20")) {
    library(dplyr)
    library(stringr)
    library(tibble)
    library(purrr)

    required_cols <- c("date", "agency", "recipient", "description", "value", "savings", "link")
    missing_cols <- setdiff(required_cols, names(doge_grants))
    if (length(missing_cols) > 0) {
        stop("doge_grants missing required columns: ", paste(missing_cols, collapse = ", "))
    }

    core_agencies <- c(
        "Department Of Education",
        "National Science Foundation",
        "National Endowment for the Humanities",
        "National Endowment for the Arts",
        "Institute of Museum and Library Services"
    )

    # Tags include agency, office/program names, and recipient/keyword signals.
    tag_patterns <- tribble(
        ~tag, ~pattern,
        "department_of_education", "\\bdepartment of education\\b|\\bdept\\.? of education\\b",
        "federal_student_aid_fsa", "\\bfederal student aid\\b|\\bfsa\\b",
        "national_science_foundation_nsf", "\\bnational science foundation\\b|\\bnsf\\b",
        "institute_of_education_sciences_ies", "\\binstitute of education sciences\\b|\\bies\\b",
        "national_endowment_for_the_humanities_neh", "\\bnational endowment for the humanities\\b|\\bneh\\b",
        "national_endowment_for_the_arts_nea", "\\bnational endowment for the arts\\b|\\bnea\\b",
        "office_of_postsecondary_education_ope", "\\boffice of postsecondary education\\b|\\bope\\b",
        "office_for_civil_rights_ocr", "\\boffice for civil rights\\b",
        "federal_trio_programs", "\\btrio\\b|\\bupward bound\\b|\\bgear up\\b|\\bmcnair\\b",
        "office_of_english_language_acquisition_oela", "\\boffice of english language acquisition\\b|\\boela\\b",
        "state_department_of_education", "\\bstate department of education\\b|\\bdept\\.? of education\\b",
        "educationusa", "\\beducationusa\\b",
        "recipient_higher_ed_or_school_system", "\\buniversity\\b|\\bcollege\\b|\\bcommunity college\\b|\\bschool district\\b|\\bpublic schools\\b|\\bstate university\\b|\\binstitute of technology\\b|\\bboard of education\\b",
        "academic_research_or_student_keyword", "\\bphd\\b|\\bdoctoral\\b|\\bpostdoc\\b|\\bfellowship\\b|\\bscholarship\\b|\\btuition\\b|\\bcurriculum\\b|\\bk-12\\b|\\bstudent\\b|\\bteacher\\b|\\bclassroom\\b|\\bcampus\\b|\\bresearch\\b|\\blaboratory\\b"
    )

    grants <- doge_grants |>
        mutate(
            date = as.Date(date),
            value = as.numeric(value),
            savings = as.numeric(savings)
        ) |>
        filter(!is.na(date), date >= start_date) |>
        mutate(
            text_all = str_c(
                dplyr::coalesce(agency, ""),
                dplyr::coalesce(recipient, ""),
                dplyr::coalesce(description, ""),
                sep = " || "
            ),
            core_agency_match = agency %in% core_agencies
        )

    tag_matrix <- map(tag_patterns$pattern, function(pat) {
        str_detect(grants$text_all, regex(pat, ignore_case = TRUE))
    })
    names(tag_matrix) <- tag_patterns$tag

    grants_tagged <- bind_cols(grants, as_tibble(tag_matrix)) |>
        mutate(
            office_or_program_match =
                federal_student_aid_fsa |
                institute_of_education_sciences_ies |
                office_of_postsecondary_education_ope |
                office_for_civil_rights_ocr |
                federal_trio_programs |
                office_of_english_language_acquisition_oela |
                educationusa,
            recipient_signal = recipient_higher_ed_or_school_system,
            keyword_signal = academic_research_or_student_keyword,
            academic_any = core_agency_match | office_or_program_match | recipient_signal | keyword_signal,
            cohort = case_when(
                core_agency_match ~ "strict_core_agency",
                office_or_program_match ~ "office_or_program_signal",
                recipient_signal ~ "recipient_signal",
                keyword_signal ~ "keyword_signal",
                TRUE ~ "non_academic"
            )
        ) |>
        select(-text_all)

    grants_tagged
}

summarize_academic_tag_counts <- function(academic_scope) {
    library(dplyr)
    library(tidyr)

    tag_cols <- c(
        "core_agency_match",
        "department_of_education",
        "federal_student_aid_fsa",
        "national_science_foundation_nsf",
        "institute_of_education_sciences_ies",
        "national_endowment_for_the_humanities_neh",
        "national_endowment_for_the_arts_nea",
        "office_of_postsecondary_education_ope",
        "office_for_civil_rights_ocr",
        "federal_trio_programs",
        "office_of_english_language_acquisition_oela",
        "state_department_of_education",
        "educationusa",
        "recipient_higher_ed_or_school_system",
        "academic_research_or_student_keyword",
        "academic_any"
    )

    missing_tag_cols <- setdiff(tag_cols, names(academic_scope))
    if (length(missing_tag_cols) > 0) {
        stop("academic_scope missing tag columns: ", paste(missing_tag_cols, collapse = ", "))
    }

    academic_scope |>
        summarise(across(all_of(tag_cols), ~ sum(.x, na.rm = TRUE))) |>
        pivot_longer(everything(), names_to = "tag", values_to = "n_matches") |>
        arrange(desc(n_matches))
}
