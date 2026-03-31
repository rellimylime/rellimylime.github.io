# Federal Grant Watch Functions

This folder contains the reusable R functions sourced by `_targets.R` and the Quarto post.

Broadly:

- `fetch_*`: collect data from APIs and scraped pages
- `combine_*` / `validate_*`: clean and merge sources
- `plot_*`: generate figures saved to `figures/`
- `classify_*` / `tag_*`: derive academic and education labels used in the analysis
