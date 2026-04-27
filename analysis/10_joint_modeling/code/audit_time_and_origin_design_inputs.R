options(encoding = "UTF-8")

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(readxl)
  library(stringr)
  library(tidyr)
})

project_root <- normalizePath(file.path(dirname(sub("^--file=", "", commandArgs(FALSE)[grep("^--file=", commandArgs(FALSE))])), "..", ".."), winslash = "/", mustWork = TRUE)
data_dir <- file.path(project_root, "定稿数据_中文")

read_final <- function(file) {
  read_xlsx(file.path(data_dir, file))
}

season_label <- function(date) {
  m <- month(date)
  case_when(
    m %in% c(3, 4, 5) ~ "Spring",
    m %in% c(6, 7, 8) ~ "Summer",
    m %in% c(9, 10, 11) ~ "Autumn",
    m %in% c(12, 1, 2) ~ "Winter",
    TRUE ~ NA_character_
  )
}

date_audit <- tibble::tribble(
  ~dataset, ~file, ~date_col,
  "Finished-product quality testing", "D2_成品理化数据_定稿.xlsx", "生产日期",
  "Finished-product MES production records", "D3_成品MES主表_定稿.xlsx", "生产日期",
  "Chenpi quality testing", "D6_陈皮检测数据_定稿.xlsx", "日期",
  "Chinese yam powder MES production records", "D7_山药粉MES主表_定稿.xlsx", "生产日期"
) |>
  rowwise() |>
  mutate(
    tmp = list({
      df <- read_final(file)
      d <- as.Date(df[[date_col]])
      tibble(
        n = nrow(df),
        date_nonmissing = sum(!is.na(d)),
        min_date = min(d, na.rm = TRUE),
        max_date = max(d, na.rm = TRUE),
        months = n_distinct(format(d[!is.na(d)], "%Y-%m")),
        season_counts = paste(names(table(season_label(d))), as.integer(table(season_label(d))), collapse = "; ")
      )
    })
  ) |>
  unnest(tmp) |>
  ungroup()

D2 <- read_final("D2_成品理化数据_定稿.xlsx") |>
  mutate(
    finished_batch = as.character(批号),
    date = as.Date(生产日期),
    month = format(date, "%Y-%m"),
    season = season_label(date),
    issue_gt10 = `崩解时限(min)` > 10
  )

D3 <- read_final("D3_成品MES主表_定稿.xlsx") |>
  mutate(
    finished_batch = as.character(批号),
    extract_batch = as.character(`健胃消食片浸膏粉_批号`)
  )

D5 <- read_final("D5_浸膏粉关联批次追溯_定稿.xlsx") |>
  mutate(
    extract_batch = as.character(浸膏粉批号),
    raw_batch = as.character(原料批号)
  )

D6 <- read_final("D6_陈皮检测数据_定稿.xlsx") |>
  mutate(raw_batch = as.character(批号))

D7 <- read_final("D7_山药粉MES主表_定稿.xlsx") |>
  mutate(yam_batch = as.character(批号))

finished_08 <- D2 |>
  filter(规格 == "0.8g")

monthly_issue <- finished_08 |>
  group_by(month) |>
  summarise(
    n = n(),
    issue_n = sum(issue_gt10, na.rm = TRUE),
    issue_rate = mean(issue_gt10, na.rm = TRUE),
    disintegration_mean = mean(`崩解时限(min)`, na.rm = TRUE),
    disintegration_sd = sd(`崩解时限(min)`, na.rm = TRUE),
    .groups = "drop"
  )

season_issue <- finished_08 |>
  group_by(season) |>
  summarise(
    n = n(),
    issue_n = sum(issue_gt10, na.rm = TRUE),
    issue_rate = mean(issue_gt10, na.rm = TRUE),
    disintegration_mean = mean(`崩解时限(min)`, na.rm = TRUE),
    disintegration_sd = sd(`崩解时限(min)`, na.rm = TRUE),
    .groups = "drop"
  )

chenpi_origin <- D6 |>
  count(产地, sort = TRUE, name = "chenpi_batches")

chenpi_finished_link <- finished_08 |>
  select(finished_batch, issue_gt10, disintegration = `崩解时限(min)`) |>
  inner_join(D3 |> select(finished_batch, extract_batch), by = "finished_batch") |>
  inner_join(D5 |> select(extract_batch, raw_batch), by = "extract_batch") |>
  inner_join(D6 |> select(raw_batch, origin = 产地, chenpi_moisture = `水分%`, hesperidin = `橙皮苷%`, impurity = `杂质%`), by = "raw_batch") |>
  group_by(origin) |>
  summarise(
    finished_records = n_distinct(finished_batch),
    chenpi_batches = n_distinct(raw_batch),
    issue_n = sum(issue_gt10, na.rm = TRUE),
    issue_rate = mean(issue_gt10, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(finished_records))

print(list(
  date_audit = date_audit,
  finished_08_summary = finished_08 |>
    summarise(
      n = n(),
      issue_n = sum(issue_gt10, na.rm = TRUE),
      issue_rate = mean(issue_gt10, na.rm = TRUE),
      min_date = min(date, na.rm = TRUE),
      max_date = max(date, na.rm = TRUE),
      months = n_distinct(month)
    ),
  monthly_issue = monthly_issue,
  season_issue = season_issue,
  chenpi_origin = chenpi_origin,
  chenpi_finished_link = chenpi_finished_link
))
