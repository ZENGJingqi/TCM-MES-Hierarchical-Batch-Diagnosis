options(encoding = "UTF-8")

suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
})

base_dir <- "D:/博士后文件/论文撰写/华润江中大数据分析/定稿数据_中文"
read_final <- function(file) read_xlsx(file.path(base_dir, file))

D2 <- read_final("D2_成品理化数据_定稿.xlsx") |>
  mutate(
    finished_batch = as.character(批号),
    issue_gt10 = `崩解时限(min)` > 10
  ) |>
  filter(规格 == "0.8g")

D3 <- read_final("D3_成品MES主表_定稿.xlsx") |>
  mutate(
    finished_batch = as.character(批号),
    extract_batch = as.character(`健胃消食片浸膏粉_批号`),
    yam_batch = as.character(`山药粉-罗亭_批号`)
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

cat("D7 columns:\n")
print(names(D7))

cat("\nD7 supplier summary:\n")
print(D7 |> count(供应商名称, sort = TRUE))

cat("\nD3 yam supplier summary:\n")
print(D3 |> count(`山药粉-罗亭_供应商`, sort = TRUE))

finished_mes <- D2 |>
  select(finished_batch, issue_gt10, disintegration = `崩解时限(min)`) |>
  inner_join(
    D3 |> select(finished_batch, extract_batch, yam_batch),
    by = "finished_batch"
  )

extract_downstream <- finished_mes |>
  filter(!is.na(extract_batch), extract_batch != "NA") |>
  group_by(extract_batch) |>
  summarise(
    finished_n = n_distinct(finished_batch),
    issue_n = sum(issue_gt10, na.rm = TRUE),
    issue_rate = mean(issue_gt10, na.rm = TRUE),
    .groups = "drop"
  )

yam_downstream <- finished_mes |>
  filter(!is.na(yam_batch), yam_batch != "NA") |>
  group_by(yam_batch) |>
  summarise(
    finished_n = n_distinct(finished_batch),
    issue_n = sum(issue_gt10, na.rm = TRUE),
    issue_rate = mean(issue_gt10, na.rm = TRUE),
    .groups = "drop"
  )

chenpi_downstream <- finished_mes |>
  inner_join(
    D5 |> select(extract_batch, raw_batch),
    by = "extract_batch",
    relationship = "many-to-many"
  ) |>
  inner_join(
    D6 |> select(raw_batch, origin = 产地),
    by = "raw_batch",
    relationship = "many-to-many"
  ) |>
  group_by(raw_batch, origin) |>
  summarise(
    finished_n = n_distinct(finished_batch),
    extract_n = n_distinct(extract_batch),
    issue_n = sum(issue_gt10, na.rm = TRUE),
    issue_rate = mean(issue_gt10, na.rm = TRUE),
    .groups = "drop"
  )

summary_fun <- function(df, layer) {
  tibble(
    layer = layer,
    upstream_batches = nrow(df),
    downstream_finished_records = sum(df$finished_n),
    median_downstream = median(df$finished_n),
    q75_downstream = as.numeric(quantile(df$finished_n, 0.75)),
    max_downstream = max(df$finished_n),
    batches_with_ge2 = sum(df$finished_n >= 2),
    batches_with_ge5 = sum(df$finished_n >= 5),
    batches_with_ge10 = sum(df$finished_n >= 10)
  )
}

cat("\nUpstream-to-downstream multiplicity:\n")
print(bind_rows(
  summary_fun(extract_downstream, "Extract-powder batch"),
  summary_fun(chenpi_downstream, "Chenpi batch"),
  summary_fun(yam_downstream, "Yam powder batch")
))

cat("\nTop extract-powder batches by downstream finished records:\n")
print(extract_downstream |> arrange(desc(finished_n)) |> head(10))

cat("\nTop Chenpi batches by downstream finished records:\n")
print(chenpi_downstream |> arrange(desc(finished_n)) |> head(10))

cat("\nTop yam-powder batches by downstream finished records:\n")
print(yam_downstream |> arrange(desc(finished_n)) |> head(10))
