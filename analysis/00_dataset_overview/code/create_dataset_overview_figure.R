library(grid)

out_dir <- file.path(getwd(), "数据概况图_英文", "figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

clr <- list(
  ink = "#25313A",
  muted = "#65737C",
  line = "#A83B2B",
  blue = "#0A5C7A",
  grey = "#8C8C8C",
  raw = "#F6EEE7",
  extract = "#F5F1E8",
  mes = "#EAF2F3",
  finish = "#F6F6F6",
  process = "#EFF4EA",
  border = "#263238",
  pale = "#FAFAFA"
)

txt <- function(label, x, y, size = 12, face = "plain", color = clr$ink, just = "center", lh = 1.0) {
  grid.text(
    label,
    x = unit(x, "npc"), y = unit(y, "npc"),
    just = just,
    gp = gpar(fontfamily = "Arial", fontsize = size, fontface = face, col = color, lineheight = lh)
  )
}

card <- function(x, y, w, h, fill) {
  grid.roundrect(
    unit(x, "npc"), unit(y, "npc"),
    width = unit(w, "npc"), height = unit(h, "npc"),
    r = unit(0.02, "npc"),
    gp = gpar(fill = fill, col = clr$border, lwd = 1.2)
  )
}

edge <- function(x0, y0, x1, y1, label, lx, ly, color = clr$line, lty = 1) {
  grid.segments(
    unit(x0, "npc"), unit(y0, "npc"),
    unit(x1, "npc"), unit(y1, "npc"),
    gp = gpar(col = color, lwd = 2.7, lty = lty),
    arrow = grid::arrow(length = unit(0.016, "npc"), type = "closed")
  )
  txt(label, lx, ly, size = 10.4, color = clr$ink, lh = 0.98)
}

icon_tablet <- function(x, y, s) {
  grid.circle(unit(x - s * 0.18, "npc"), unit(y, "npc"), r = unit(s * 0.18, "npc"),
              gp = gpar(fill = "white", col = clr$line, lwd = 2))
  grid.circle(unit(x + s * 0.18, "npc"), unit(y, "npc"), r = unit(s * 0.18, "npc"),
              gp = gpar(fill = "white", col = clr$blue, lwd = 2))
  grid.segments(unit(x - s * 0.18, "npc"), unit(y - s * 0.18, "npc"),
                unit(x - s * 0.18, "npc"), unit(y + s * 0.18, "npc"),
                gp = gpar(col = clr$line, lwd = 1.4))
}

icon_clipboard <- function(x, y, s) {
  grid.roundrect(unit(x, "npc"), unit(y, "npc"), width = unit(s * 0.62, "npc"), height = unit(s * 0.76, "npc"),
                 r = unit(s * 0.05, "npc"), gp = gpar(fill = "white", col = clr$blue, lwd = 2))
  grid.roundrect(unit(x, "npc"), unit(y + s * 0.38, "npc"), width = unit(s * 0.28, "npc"), height = unit(s * 0.12, "npc"),
                 r = unit(s * 0.03, "npc"), gp = gpar(fill = clr$blue, col = clr$blue))
  for (i in seq(-0.18, 0.18, by = 0.12)) {
    grid.segments(unit(x - s * 0.20, "npc"), unit(y + i * s, "npc"),
                  unit(x + s * 0.20, "npc"), unit(y + i * s, "npc"),
                  gp = gpar(col = clr$grey, lwd = 1.4))
  }
}

icon_powder <- function(x, y, s) {
  grid.roundrect(unit(x, "npc"), unit(y, "npc"), width = unit(s * 0.62, "npc"), height = unit(s * 0.65, "npc"),
                 r = unit(s * 0.06, "npc"), gp = gpar(fill = "white", col = clr$line, lwd = 2))
  grid.rect(unit(x, "npc"), unit(y + s * 0.18, "npc"), width = unit(s * 0.54, "npc"), height = unit(s * 0.16, "npc"),
            gp = gpar(fill = "#EFE2D2", col = NA))
  grid.circle(unit(x - s * 0.16, "npc"), unit(y - s * 0.12, "npc"), r = unit(s * 0.035, "npc"),
              gp = gpar(fill = clr$line, col = NA))
  grid.circle(unit(x, "npc"), unit(y - s * 0.10, "npc"), r = unit(s * 0.035, "npc"),
              gp = gpar(fill = clr$line, col = NA))
  grid.circle(unit(x + s * 0.16, "npc"), unit(y - s * 0.12, "npc"), r = unit(s * 0.035, "npc"),
              gp = gpar(fill = clr$line, col = NA))
}

icon_orange <- function(x, y, s) {
  grid.circle(unit(x, "npc"), unit(y, "npc"), r = unit(s * 0.27, "npc"),
              gp = gpar(fill = "#F6D6B8", col = clr$line, lwd = 2))
  grid.circle(unit(x, "npc"), unit(y, "npc"), r = unit(s * 0.12, "npc"),
              gp = gpar(fill = "white", col = clr$line, lwd = 1.5))
  for (a in seq(0, 300, by = 60)) {
    dx <- cos(a * pi / 180) * s * 0.24
    dy <- sin(a * pi / 180) * s * 0.24
    grid.segments(unit(x, "npc"), unit(y, "npc"), unit(x + dx, "npc"), unit(y + dy, "npc"),
                  gp = gpar(col = clr$line, lwd = 1.1))
  }
  grid.circle(unit(x + s * 0.23, "npc"), unit(y + s * 0.22, "npc"), r = unit(s * 0.055, "npc"),
              gp = gpar(fill = clr$blue, col = NA))
}

icon_yam <- function(x, y, s) {
  grid.ellipse <- function(cx, cy, rx, ry, fill, border) {
    theta <- seq(0, 2 * pi, length.out = 80)
    grid.polygon(unit(cx + rx * cos(theta), "npc"), unit(cy + ry * sin(theta), "npc"),
                 gp = gpar(fill = fill, col = border, lwd = 2))
  }
  grid.ellipse(x, y, s * 0.27, s * 0.12, "#EAD7C2", clr$blue)
  grid.ellipse(x + s * 0.18, y + s * 0.03, s * 0.20, s * 0.09, "#EAD7C2", clr$blue)
  grid.segments(unit(x - s * 0.05, "npc"), unit(y + s * 0.14, "npc"),
                unit(x + s * 0.02, "npc"), unit(y + s * 0.28, "npc"),
                gp = gpar(col = clr$blue, lwd = 1.6))
  grid.segments(unit(x + s * 0.02, "npc"), unit(y + s * 0.28, "npc"),
                unit(x + s * 0.12, "npc"), unit(y + s * 0.36, "npc"),
                gp = gpar(col = clr$blue, lwd = 1.6))
}

draw_dataset_figure <- function() {
  grid.newpage()
  grid.rect(gp = gpar(fill = "white", col = NA))

  card(0.5, 0.93, 0.86, 0.075, clr$pale)
  txt("5,975 valid batch-level records from real-world quality-control testing and MES production records\nAugust 2024 to March 2026",
      0.5, 0.93, size = 11.8, face = "bold", lh = 1.05)

  # Cards
  card(0.12, 0.62, 0.19, 0.23, clr$raw)
  icon_orange(0.12, 0.69, 0.105)
  txt("Chenpi\nquality testing", 0.12, 0.61, size = 13.8, face = "bold", lh = 1.02)
  txt("44 records", 0.12, 0.525, size = 11.8, color = clr$muted)

  card(0.37, 0.62, 0.21, 0.23, clr$extract)
  icon_powder(0.37, 0.69, 0.105)
  txt("Jianwei Xiaoshi\nextract-powder testing", 0.37, 0.61, size = 13.2, face = "bold", lh = 1.02)
  txt("618 records", 0.37, 0.525, size = 11.8, color = clr$muted)

  card(0.63, 0.62, 0.21, 0.23, clr$mes)
  icon_clipboard(0.63, 0.69, 0.105)
  txt("Jianwei Xiaoshi tablet\nMES production records", 0.63, 0.61, size = 13.2, face = "bold", lh = 1.02)
  txt("1,243 records", 0.63, 0.525, size = 11.8, color = clr$muted)

  card(0.88, 0.62, 0.19, 0.23, clr$finish)
  icon_tablet(0.88, 0.69, 0.105)
  txt("Jianwei Xiaoshi tablet\nquality testing", 0.88, 0.61, size = 13.2, face = "bold", lh = 1.02)
  txt("3,728 records", 0.88, 0.525, size = 11.8, color = clr$muted)

  card(0.63, 0.29, 0.25, 0.22, clr$process)
  icon_yam(0.63, 0.355, 0.115)
  txt("Chinese yam powder\nMES production records", 0.63, 0.285, size = 13.3, face = "bold", lh = 1.02)
  txt("342 records", 0.63, 0.205, size = 11.8, color = clr$muted)

  # Linkage arrows
  edge(0.215, 0.62, 0.265, 0.62,
       "Chenpi -> extract powder\n41 Chenpi batches traced\n578 extract-powder batches mapped",
       0.24, 0.775, color = clr$grey, lty = 2)

  edge(0.475, 0.62, 0.525, 0.62,
       "Extract powder -> tablet MES\n933 MES records linked\n273 extract-powder batches used",
       0.50, 0.775, color = clr$line)

  edge(0.735, 0.62, 0.785, 0.62,
       "Tablet MES -> quality testing\n908 tablet records linked",
       0.76, 0.775, color = clr$line)

  grid.segments(
    unit(0.63, "npc"), unit(0.40, "npc"),
    unit(0.63, "npc"), unit(0.505, "npc"),
    gp = gpar(col = clr$blue, lwd = 2.9),
    arrow = grid::arrow(length = unit(0.016, "npc"), type = "closed")
  )
  txt("Chinese yam powder -> tablet MES\n1,159 MES records linked\n176 yam powder batches used",
      0.83, 0.41, size = 10.4, lh = 0.98)

  # End-to-end linkage summary
  card(0.50, 0.095, 0.84, 0.105, "#FBFBFB")
  txt("End-to-end tracing\n804 tablet records traced to extract powder; 839 tablet records traced to Chinese yam powder",
      0.50, 0.095, size = 10.6, color = clr$ink, lh = 1.05)
}

pdf_file <- file.path(out_dir, "Figure_1_dataset_overview_and_batch_linkage_simplified.pdf")
png_file <- file.path(out_dir, "Figure_1_dataset_overview_and_batch_linkage_simplified.png")

cairo_pdf(pdf_file, width = 12, height = 7, family = "Arial")
draw_dataset_figure()
dev.off()

png(png_file, width = 12, height = 7, units = "in", res = 600, type = "cairo")
draw_dataset_figure()
dev.off()

message("Saved: ", pdf_file)
message("Saved: ", png_file)
