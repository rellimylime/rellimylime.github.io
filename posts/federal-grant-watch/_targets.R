library(targets)

tar_source("R/")

list(
    # 0) Analysis parameter.
    tar_target(
        analysis_start_date,
        as.Date("2025-01-20")
    ),

    # 1) Fetch raw sources.
    tar_target(
        doge_grants,
        fetch_doge_grants()
    ),

    tar_target(
        federal_register_docs,
        fetch_federal_register_events(start_date = analysis_start_date)
    ),

    tar_target(
        usaspending_awards,
        fetch_usaspending_awards(doge_grants, max_ids = 12000, pause_seconds = 0.1)
    ),

    tar_target(
        news_articles,
        scrape_grant_news()
    ),

    # 2) Build the baseline merged dataset and QC summaries.
    tar_target(
        combined,
        combine_sources(doge_grants, news_articles, usaspending_awards)
    ),

    tar_target(
        combined_summary,
        combined |>
            dplyr::count(source, sort = TRUE)
    ),

    tar_target(
        enrichment_summary,
        combined |>
            dplyr::filter(source == "doge_grants") |>
            dplyr::summarise(
                total = dplyr::n(),
                with_usaspending = sum(!is.na(usaspending_award_id)),
                matched_obligation = sum(!is.na(usaspending_total_obligation))
            )
    ),

    tar_target(
        timeline_plot,
        plot_termination_timeline(combined, policy_events)
    ),

    tar_target(
        policy_events,
        build_policy_events()
    ),

    # 2b) Academic domain analysis: university/research grants classified by field.
    tar_target(
        academic_scope_full,
        build_academic_scope_full(doge_grants, usaspending_awards, analysis_start_date)
    ),

    tar_target(
        academic_domain_classified,
        classify_academic_domain(academic_scope_full)
    ),

    tar_target(
        academic_domain_summary,
        summarize_academic_domain(academic_domain_classified)
    ),

    tar_target(
        academic_monthly_totals,
        summarize_academic_monthly_totals(academic_domain_classified)
    ),

    tar_target(
        academic_monthly_by_domain,
        summarize_academic_monthly_by_domain(academic_domain_classified)
    ),

    tar_target(
        academic_cumulative_plot,
        plot_academic_cumulative(academic_monthly_totals, policy_events)
    ),

    tar_target(
        academic_monthly_bars_plot,
        plot_academic_monthly_bars(academic_monthly_by_domain, policy_events)
    ),

    tar_target(
        academic_discrepancy_plot,
        plot_academic_discrepancy(academic_monthly_totals, policy_events)
    ),

    tar_target(
        academic_domain_comparison_plot,
        plot_academic_domain_comparison(academic_domain_summary)
    ),

    # 3) Education investigation: scope -> primary areas -> subareas -> refinement.
    tar_target(
        education_scope,
        build_education_scope(doge_grants, usaspending_awards, analysis_start_date)
    ),

    tar_target(
        education_totals,
        summarize_education_totals(education_scope)
    ),

    tar_target(
        education_primary,
        classify_primary_education_area(education_scope)
    ),

    tar_target(
        education_primary_summary,
        summarize_primary_areas(education_primary)
    ),

    tar_target(
        education_group_money_summary,
        summarize_money_by_group(education_primary)
    ),

    tar_target(
        education_group_money_plot,
        plot_money_by_group(education_group_money_summary)
    ),

    tar_target(
        education_discrepancy_monthly,
        summarize_discrepancy_over_time(education_primary)
    ),

    tar_target(
        education_discrepancy_plot,
        plot_discrepancy_over_time(education_discrepancy_monthly)
    ),

    tar_target(
        education_monthly_summary,
        summarize_monthly_areas(education_primary)
    ),

    tar_target(
        education_monthly_totals,
        summarize_monthly_totals(education_primary)
    ),

    tar_target(
        education_claimed_events_plot,
        plot_monthly_claimed_with_events(education_monthly_totals, policy_events)
    ),

    tar_target(
        education_discrepancy_events_plot,
        plot_discrepancy_with_events(education_monthly_totals, policy_events)
    ),

    tar_target(
        education_monthly_plot,
        plot_monthly_primary_counts(education_monthly_summary)
    ),

    tar_target(
        education_monthly_stacked_bar_line_plot,
        plot_monthly_stacked_bar_line(education_monthly_summary, policy_events)
    ),

    tar_target(
        education_subareas,
        classify_education_subareas(education_primary)
    ),

    tar_target(
        education_unknown_refined,
        refine_unknown_subareas(education_subareas)
    ),

    tar_target(
        education_final,
        build_final_education_area(education_subareas, education_unknown_refined)
    ),

    # 4) Investigation outputs for reporting.
    tar_target(
        education_final_summary,
        summarize_final_areas(education_final)
    ),

    tar_target(
        education_final_area_plot,
        plot_final_area_savings(education_final_summary)
    )
)
