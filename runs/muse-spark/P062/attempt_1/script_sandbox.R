out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tibble)
})

# ---- helpers mirroring SAS macros ----

diagram_box <- function(boxid = 1, left_x = 10, top_y = 100, right_x = 30, bottom_y = 90,
                        text = "", text_just = "center", text_size = 10,
                        text_color = "black", linecolor = "black",
                        linepattern = "solid", linethickness = 1) {
  boxid <- as.numeric(boxid)
  left_x <- as.numeric(left_x); top_y <- as.numeric(top_y)
  right_x <- as.numeric(right_x); bottom_y <- as.numeric(bottom_y)

  box_df <- data.frame(
    DIAGID = as.character(boxid),
    boxid  = boxid,
    box_x  = c(left_x, right_x, right_x, left_x),
    box_y  = c(top_y,  top_y,   bottom_y, bottom_y),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  x1 <- left_x;  y1 <- top_y
  x2 <- right_x; y2 <- top_y
  x3 <- right_x; y3 <- bottom_y
  x4 <- left_x;  y4 <- bottom_y

  den <- (x1 - x3) * (y2 - y4) - (y1 - y3) * (x2 - x4)
  text_x <- NA_real_; text_y <- NA_real_
  text_x2 <- NA_real_; text_y2 <- NA_real_
  text_v <- NA_character_; text2_v <- NA_character_
  if (den == 0) {
    warning("The diagonals are parallel or coincide. A quadrilateral cannot be formed.")
  } else {
    num1 <- (x1 * y3 - y1 * x3)
    num2 <- (x2 * y4 - y2 * x4)
    cx <- (num1 * (x2 - x4) - (x1 - x3) * num2) / den
    cy <- (num1 * (y2 - y4) - (y1 - y3) * num2) / den
    if (toupper(text_just) != "LEFT") {
      text_x <- cx
      text_y <- cy
      text_v <- as.character(text)
    }
    if (toupper(text_just) == "LEFT") {
      text_x2 <- x1 + 1
      text_y2 <- cy
      text2_v <- as.character(text)
    }
  }

  text_df <- data.frame(
    DIAGID = as.character(boxid),
    text_x = text_x,
    text_y = text_y,
    text_x2 = text_x2,
    text_y2 = text_y2,
    text = text_v,
    text2 = text2_v,
    sizeresponse = as.numeric(text_size),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  style_df <- data.frame(
    id = "DIAGID",
    value = as.character(boxid),
    textcolor = as.character(text_color),
    linecolor = as.character(linecolor),
    linepattern = as.character(linepattern),
    linethickness = as.numeric(linethickness),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  list(box = box_df, textdf = text_df, style = style_df)
}

diagram_line <- function(lineid = 1, start_x = 30, start_y = 90,
                         midpoint_x = NA, midpoint_y = NA,
                         end_x = 30, end_y = 60,
                         linecolor = "black", linepattern = "solid", linethickness = 1) {
  lineid <- as.numeric(lineid)
  xs <- c(as.numeric(start_x))
  ys <- c(as.numeric(start_y))
  if (!is.na(midpoint_x) && !is.na(midpoint_y) &&
      nzchar(trimws(as.character(midpoint_x))) && nzchar(trimws(as.character(midpoint_y)))) {
    xs <- c(xs, as.numeric(midpoint_x))
    ys <- c(ys, as.numeric(midpoint_y))
  }
  xs <- c(xs, as.numeric(end_x))
  ys <- c(ys, as.numeric(end_y))
  n <- length(xs)
  line_df <- data.frame(
    `_LINEID` = rep(as.character(lineid), n),
    lineid = rep(lineid, n),
    line_x = xs,
    line_y = ys,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  lstyle_df <- data.frame(
    id = "_LINEID",
    value = as.character(lineid),
    linecolor = as.character(linecolor),
    linepattern = as.character(linepattern),
    linethickness = as.numeric(linethickness),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  list(line = line_df, style = lstyle_df)
}

# ---- benchmark driver: usage example from header ----
b1 <- diagram_box(boxid = 1, left_x = 30, top_y = 100, right_x = 70, bottom_y = 90,
                  text = "Assessed for eligibility (n=100)")
l1 <- diagram_line(lineid = 1, start_x = 50, start_y = 90, end_x = 50, end_y = 55)
b2 <- diagram_box(boxid = 2, left_x = 37, top_y = 55, right_x = 63, bottom_y = 45,
                  text = "Randomized (n=90)")

# ---- combine like SAS: set diagram_:  ----
plot_diag <- dplyr::bind_rows(b1$box, b1$textdf, b2$box, b2$textdf, l1$line)

plot_diag_attr <- dplyr::bind_rows(b1$style, b2$style, l1$style)

# order columns SAS-like for CSV
desired <- c("DIAGID", "boxid", "box_x", "box_y",
             "text_x", "text_y", "text", "text_x2", "text_y2", "text2",
             "sizeresponse", "_LINEID", "lineid", "line_x", "line_y")
for (cl in desired) {
  if (!cl %in% names(plot_diag)) plot_diag[[cl]] <- NA
}
plot_diag <- plot_diag[, desired]

write.csv(plot_diag, file.path(out, "sgplot.csv"), row.names = FALSE, na = "")

# ---- SGPLOT equivalent with ggplot2 ----
boxes_df <- dplyr::bind_rows(b1$box, b2$box)
texts_c  <- dplyr::bind_rows(b1$textdf, b2$textdf)
# center-justified texts
texts_c <- texts_c[!is.na(texts_c$text), , drop = FALSE]
# left-justified texts (none in this example, kept for fidelity)
texts_l <- dplyr::bind_rows(b1$textdf, b2$textdf)
texts_l <- texts_l[!is.na(texts_l$text2), , drop = FALSE]
lines_df <- l1$line

minx <- 0; maxx <- 100; miny <- 0; maxy <- 100

p <- ggplot() +
  geom_polygon(data = boxes_df,
               aes(x = box_x, y = box_y, group = boxid),
               fill = "white", colour = "black",
               linetype = "solid", linewidth = 0.8) +
  geom_path(data = lines_df,
            aes(x = line_x, y = line_y, group = lineid),
            colour = "black", linetype = "solid", linewidth = 0.6,
            arrow = grid::arrow(type = "closed", length = grid::unit(0.15, "inches"))) +
  geom_text(data = texts_c,
            aes(x = text_x, y = text_y, label = text),
            family = "mono", size = 3.5, colour = "black") +
  coord_cartesian(xlim = c(minx, maxx), ylim = c(miny, maxy), expand = FALSE) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.border = element_blank(),
        legend.position = "none")

ggsave(file.path(out, "sgplot.png"), plot = p, width = 8, height = 6, dpi = 300, device = "png")
