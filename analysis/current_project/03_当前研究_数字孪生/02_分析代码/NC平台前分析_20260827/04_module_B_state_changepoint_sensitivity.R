options(warn = 1)
options(encoding = "UTF-8")

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(pdftools)
  library(readr)
  library(readxl)
  library(scales)
  library(stringr)
  library(tidyr)
})

input_dir <- Sys.getenv("TCM_NC_B_INPUT_DIR", unset = "")
output_dir <- Sys.getenv("TCM_NC_OUTPUT_DIR", unset = "")
if (!nzchar(input_dir) || !nzchar(output_dir)) stop("TCM_NC_B_INPUT_DIR and TCM_NC_OUTPUT_DIR are required")
tables_dir <- file.path(output_dir, "tables")
figures_dir <- file.path(output_dir, "figures")
docs_dir <- file.path(output_dir, "docs")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(docs_dir, recursive = TRUE, showWarnings = FALSE)

norm_batch <- function(x) {
  s <- as.character(x) |> str_trim() |> str_replace("\\.0$", "")
  s[is.na(s)] <- ""
  s
}

segment_loglik <- function(event_n, total_n, starts, ends) {
  ll <- 0
  for (i in seq_along(starts)) {
    e <- sum(event_n[starts[[i]]:ends[[i]]])
    n <- sum(total_n[starts[[i]]:ends[[i]]])
    p <- pmin(pmax(e / n, 1e-8), 1 - 1e-8)
    ll <- ll + e * log(p) + (n - e) * log1p(-p)
  }
  ll
}

best_segmentation <- function(month_df, min_segment_months = 2, criterion = "BIC", max_segments = 5) {
  m <- nrow(month_df)
  candidates <- list(); id <- 1L
  for (k in seq_len(min(max_segments, floor(m / min_segment_months)))) {
    cut_sets <- if (k == 1) list(integer(0)) else {
      raw <- combn(seq_len(m - 1), k - 1, simplify = FALSE)
      Filter(function(cuts) {
        starts <- c(1, cuts + 1); ends <- c(cuts, m)
        all(ends - starts + 1 >= min_segment_months)
      }, raw)
    }
    for (cuts in cut_sets) {
      starts <- c(1, cuts + 1); ends <- c(cuts, m)
      ll <- segment_loglik(month_df$event_n, month_df$total_n, starts, ends)
      parameter_n <- 2 * k - 1
      score <- if (criterion == "BIC") -2 * ll + parameter_n * log(sum(month_df$total_n)) else -2 * ll + 2 * parameter_n
      candidates[[id]] <- tibble(
        segment_n = k, cuts = paste(cuts, collapse = ";"), log_likelihood = ll,
        parameter_n = parameter_n, criterion = criterion, criterion_value = score
      )
      id <- id + 1L
    }
  }
  bind_rows(candidates) |> arrange(criterion_value) |> slice(1)
}

save_pdf_png <- function(plot, stem, width = 13.4, height = 6.8, dpi = 240) {
  pdf_file <- file.path(figures_dir, paste0(stem, ".pdf"))
  png_file <- file.path(figures_dir, paste0(stem, "_preview.png"))
  ggsave(pdf_file, plot, width = width, height = height, device = cairo_pdf, bg = "white")
  suppressWarnings(pdftools::pdf_convert(pdf_file, format = "png", pages = 1, dpi = dpi, filenames = png_file))
  if (!file.exists(pdf_file) || !file.exists(png_file) || file.info(pdf_file)$size == 0 || file.info(png_file)$size == 0) stop("Empty figure")
}

d2_file <- list.files(input_dir, pattern = "^D2.*\\.xlsx$", full.names = TRUE)
if (length(d2_file) != 1) stop("Missing staged D2 file")
d2_raw <- suppressWarnings(read_xlsx(d2_file[[1]]))
d2 <- tibble(
  finished_batch = norm_batch(d2_raw$batch_no),
  observation_date = as.Date(d2_raw$production_date),
  dosage_strength = str_replace_all(as.character(d2_raw$dosage_strength), "\\s+", ""),
  disintegration_time_min = suppressWarnings(as.numeric(d2_raw$disintegration_time_min))
) |>
  filter(finished_batch != "", dosage_strength == "0.8g") |>
  group_by(finished_batch) |>
  summarise(
    observation_date = max(observation_date, na.rm = TRUE),
    disintegration_time_min = mean(disintegration_time_min, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(observation_month = as.Date(format(observation_date, "%Y-%m-01")))

thresholds <- c(9, 10, 11)
monthly <- bind_rows(lapply(thresholds, function(threshold) {
  d2 |>
    group_by(observation_month) |>
    summarise(
      total_n = n(), event_n = sum(disintegration_time_min > threshold),
      event_rate = event_n / total_n, mean_disintegration = mean(disintegration_time_min),
      median_disintegration = median(disintegration_time_min), .groups = "drop"
    ) |>
    mutate(threshold_min = threshold)
}))
write_csv(monthly, file.path(tables_dir, "01_monthly_state_endpoints.csv"), na = "")

configs <- expand_grid(threshold_min = thresholds, min_segment_months = c(1, 2, 3), criterion = c("AIC", "BIC"))
segmentation <- bind_rows(lapply(seq_len(nrow(configs)), function(i) {
  cfg <- configs[i, ]
  d <- monthly |> filter(threshold_min == cfg$threshold_min) |> arrange(observation_month)
  best <- best_segmentation(d, min_segment_months = cfg$min_segment_months, criterion = cfg$criterion, max_segments = 5)
  cut_idx <- if (best$cuts == "") integer(0) else as.integer(str_split(best$cuts, ";")[[1]])
  cut_months <- if (length(cut_idx) == 0) "" else paste(format(d$observation_month[cut_idx], "%Y-%m"), collapse = ";")
  best |>
    mutate(
      threshold_min = cfg$threshold_min,
      min_segment_months = cfg$min_segment_months,
      cut_after_months = cut_months
    ) |>
    select(threshold_min, min_segment_months, criterion, segment_n, cut_after_months, log_likelihood, parameter_n, criterion_value)
}))
write_csv(segmentation, file.path(tables_dir, "02_changepoint_sensitivity_grid.csv"), na = "")

primary <- segmentation |>
  filter(threshold_min == 10, min_segment_months == 2, criterion == "BIC") |>
  slice(1)
primary_monthly <- monthly |> filter(threshold_min == 10) |> arrange(observation_month)
primary_cut_months <- if (primary$cut_after_months == "") character(0) else str_split(primary$cut_after_months, ";")[[1]]
primary_cut_idx <- match(primary_cut_months, format(primary_monthly$observation_month, "%Y-%m"))
primary_starts <- c(1, primary_cut_idx + 1); primary_ends <- c(primary_cut_idx, nrow(primary_monthly))
primary_states <- bind_rows(lapply(seq_along(primary_starts), function(i) {
  rows <- primary_starts[[i]]:primary_ends[[i]]
  tibble(
    state_id = paste0("S", i),
    start_month = primary_monthly$observation_month[min(rows)],
    end_month = primary_monthly$observation_month[max(rows)],
    batch_n = sum(primary_monthly$total_n[rows]),
    event_n = sum(primary_monthly$event_n[rows]),
    event_rate = event_n / batch_n,
    mean_disintegration = weighted.mean(primary_monthly$mean_disintegration[rows], primary_monthly$total_n[rows])
  )
}))
write_csv(primary_states, file.path(tables_dir, "03_primary_data_driven_states.csv"), na = "")

cut_frequency <- segmentation |>
  select(threshold_min, min_segment_months, criterion, cut_after_months) |>
  separate_rows(cut_after_months, sep = ";") |>
  filter(cut_after_months != "") |>
  count(cut_after_months, name = "configuration_n") |>
  mutate(configuration_rate = configuration_n / nrow(configs), cut_month = as.Date(paste0(cut_after_months, "-01"))) |>
  arrange(cut_month)
write_csv(cut_frequency, file.path(tables_dir, "04_changepoint_consensus_frequency.csv"), na = "")

nejm <- c("#BC3C29", "#0072B5", "#E18727", "#20854E", "#7876B1", "#6F99AD")
base_theme <- theme_classic(base_family = "Arial", base_size = 14) +
  theme(
    axis.title = element_text(size = 15), axis.text = element_text(size = 12, colour = "black"),
    legend.position = "top", legend.title = element_blank(), legend.text = element_text(size = 12),
    plot.tag = element_text(face = "bold", size = 16), plot.margin = margin(7, 10, 7, 8)
  )
plot_trend <- ggplot(monthly, aes(observation_month, event_rate, colour = factor(threshold_min), group = threshold_min)) +
  geom_line(linewidth = 0.9) + geom_point(size = 2) +
  geom_vline(xintercept = as.Date(paste0(primary_cut_months, "-01")), linetype = "dashed", colour = "grey30") +
  scale_colour_manual(values = nejm[c(3, 1, 5)], labels = c(">9 min", ">10 min (primary)", ">11 min")) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
  labs(x = "Quality-observation month", y = "Monthly event rate") +
  base_theme + theme(axis.text.x = element_text(angle = 30, hjust = 1))

plot_consensus <- ggplot(cut_frequency, aes(cut_month, configuration_rate)) +
  geom_col(fill = nejm[2], width = 24) +
  geom_text(aes(label = paste0(configuration_n, "/", nrow(configs))), vjust = -0.35, size = 3.8, family = "Arial") +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1.10)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
  labs(x = "Candidate change after month", y = "Sensitivity configurations selecting change") +
  base_theme + theme(axis.text.x = element_text(angle = 30, hjust = 1))

combined <- (plot_trend | plot_consensus) + plot_annotation(tag_levels = "A")
save_pdf_png(combined, "Figure_B_state_changepoint_sensitivity", width = 14.0, height = 6.8)

qa <- tibble(
  check = c("D2 unique 0.8-g batches", "Primary endpoint months", "Sensitivity configurations", "Primary segmentation available", "Figure PDF exists", "Figure PNG exists"),
  observed = c(nrow(d2), nrow(primary_monthly), nrow(segmentation), nrow(primary),
               as.integer(file.exists(file.path(figures_dir, "Figure_B_state_changepoint_sensitivity.pdf"))),
               as.integer(file.exists(file.path(figures_dir, "Figure_B_state_changepoint_sensitivity_preview.png")))),
  expected = c(4296, length(unique(d2$observation_month)), 18, 1, 1, 1)
) |>
  mutate(pass = observed == expected)
write_csv(qa, file.path(docs_dir, "module_B_QA_checks.csv"), na = "")
if (!all(qa$pass)) stop("Module B QA failed")

report <- c(
  "# Module B: manufacturing-state and changepoint sensitivity",
  "",
  paste0("The primary binomial BIC segmentation (event >10 min; minimum two months per segment) selected ", primary$segment_n, " states with changes after: ", ifelse(primary$cut_after_months == "", "none", primary$cut_after_months), "."),
  paste0("Across ", nrow(configs), " prespecified combinations of endpoint threshold, minimum segment length and information criterion, the most frequently selected change months were: ", paste0(cut_frequency$cut_after_months[order(-cut_frequency$configuration_n)][seq_len(min(5, nrow(cut_frequency)))], collapse = ", "), "."),
  "",
  "The high-risk period is treated as a data-supported manufacturing state only when its boundaries recur across sensitivity configurations. Calendar month remains a state descriptor, not a deployable causal predictor."
)
write_lines(report, file.path(docs_dir, "module_B_findings_and_boundaries.md"), na = "")
write_lines(capture.output(sessionInfo()), file.path(docs_dir, "R_session_info.txt"), na = "")
message("Module B completed: ", output_dir)
