out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

library(ggplot2)
library(dplyr)
library(grid)

# ---- helpers emulating macros/morioka/diagram_box.sas and diagram_line.sas ----

diagram_box <- function(boxid=1, left_x=10, top_y=100, right_x=30, bottom_y=90,
                        text="", text_just="center", text_size=10,
                        text_color="black", linecolor="black",
                        linepattern="solid", linethickness=1) {
  box_df <- data.frame(
    DIAGID = as.character(boxid),
    boxid  = boxid,
    box_x  = c(left_x, right_x, right_x, left_x),
    box_y  = c(top_y,  top_y,   bottom_y, bottom_y),
    stringsAsFactors = FALSE
  )
  x1 <- left_x;  y1 <- top_y
  x2 <- right_x; y2 <- top_y
  x3 <- right_x; y3 <- bottom_y
  x4 <- left_x;  y4 <- bottom_y
  den <- (x1 - x3) * (y2 - y4) - (y1 - y3) * (x2 - x4)
  if (den == 0) {
    warning("The diagonals are parallel or coincide. A quadrilateral cannot be formed.")
    text_x <- NA_real_; text_y <- NA_real_
    text_x2 <- NA_real_; text_y2 <- NA_real_
  } else {
    num1 <- (x1 * y3 - y1 * x3)
    num2 <- (x2 * y4 - y2 * x4)
    cx <- (num1 * (x2 - x4) - (x1 - x3) * num2) / den
    cy <- (num1 * (y2 - y4) - (y1 - y3) * num2) / den
    if (toupper(text_just) != "LEFT") {
      text_x <- cx; text_y <- cy
      text_x2 <- NA_real_; text_y2 <- NA_real_
    } else {
      text_x <- NA_real_; text_y <- NA_real_
      text_x2 <- x1 + 1; text_y2 <- cy
    }
  }
  if (toupper(text_just) != "LEFT") {
    text_df <- data.frame(
      text_x = text_x, text_y = text_y,
      text_x2 = NA_real_, text_y2 = NA_real_,
      text = as.character(text), text2 = NA_character_,
      sizeresponse = text_size,
      DIAGID = as.character(boxid),
      stringsAsFactors = FALSE
    )
  } else {
    text_df <- data.frame(
      text_x = NA_real_, text_y = NA_real_,
      text_x2 = text_x2, text_y2 = text_y2,
      text = NA_character_, text2 = as.character(text),
      sizeresponse = text_size,
      DIAGID = as.character(boxid),
      stringsAsFactors = FALSE
    )
  }
  style_df <- data.frame(
    id = "DIAGID", value = as.character(boxid),
    textcolor = text_color, linecolor = linecolor,
    linepattern = linepattern, linethickness = linethickness,
    stringsAsFactors = FALSE
  )
  list(box = box_df, text = text_df, style = style_df)
}

diagram_line <- function(lineid=1, start_x=30, start_y=90,
                         midpoint_x=NULL, midpoint_y=NULL,
                         end_x=30, end_y=60,
                         linecolor="black", linepattern="solid", linethickness=1) {
  if (!is.null(midpoint_x) && !is.null(midpoint_y) &&
      !is.na(midpoint_x) && !is.na(midpoint_y)) {
    lx <- c(start_x, midpoint_x, end_x)
    ly <- c(start_y, midpoint_y, end_y)
  } else {
    lx <- c(start_x, end_x)
    ly <- c(start_y, end_y)
  }
  line_df <- data.frame(
    lineid = lineid,
    line_x = lx,
    line_y = ly,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  line_df[["_LINEID"]] <- as.character(lineid)
  line_df <- line_df[, c("_LINEID", "lineid", "line_x", "line_y")]
  style_df <- data.frame(
    id = "_LINEID", value = as.character(lineid),
    linecolor = linecolor, linepattern = linepattern,
    linethickness = linethickness,
    stringsAsFactors = FALSE
  )
  list(line = line_df, style = style_df)
}

# ---- benchmark driver: usage example from header ----
# %diagram_box(boxid=1, left_x=30, top_y=100, right_x=70, bottom_y=90, text=Assessed for eligibility (n=100));
# %diagram_line(lineid=1, start_x=50, start_y=90, end_x=50, end_y=55);
# %diagram_box(boxid=2, left_x=37, top_y=55, right_x=63, bottom_y=45, text=Randomized (n=90));
# %diagram_plot(minx=0, maxx=100, miny=0, maxy=100);

b1 <- diagram_box(boxid=1, left_x=30, top_y=100, right_x=70, bottom_y=90,
                  text="Assessed for eligibility (n=100)")
l1 <- diagram_line(lineid=1, start_x=50, start_y=90, end_x=50, end_y=55)
b2 <- diagram_box(boxid=2, left_x=37, top_y=55, right_x=63, bottom_y=45,
                  text="Randomized (n=90)")

diagram_box_1 <- b1$box
diagram_box_text_1 <- b1$text
diagram_box_2 <- b2$box
diagram_box_text_2 <- b2$text
diagram_line_1 <- l1$line

# ---- plot_diag: SET diagram_: (all datasets prefixed diagram_) ----
plot_diag <- dplyr::bind_rows(diagram_box_1, diagram_box_text_1,
                              diagram_line_1, diagram_box_2, diagram_box_text_2)

# order columns like SAS SET would expose them
desired <- c("DIAGID","boxid","box_x","box_y","text_x","text_y","text_x2",
             "text_y2","text","text2","sizeresponse","_LINEID","lineid","line_x","line_y")
for (nm in desired) {
  if (!nm %in% names(plot_diag)) plot_diag[[nm]] <- NA
}
plot_diag <- plot_diag[, desired]

# CSV of the plotted data
write.csv(plot_diag, file.path(out, "sgplot.csv"), row.names = FALSE, na = "")

# ---- prepare data for ggplot (emulate PROC SGPLOT polygon / text / series) ----
boxes <- plot_diag[!is.na(plot_diag$box_x) & !is.na(plot_diag$box_y), ]
box_text_center <- plot_diag[!is.na(plot_diag$text_x) & !is.na(plot_diag$text) & !is.na(plot_diag$text_y), ]
box_text_left <- plot_diag[!is.na(plot_diag$text_x2) & !is.na(plot_diag$text2), ]
lines <- plot_diag[!is.na(plot_diag$line_x) & !is.na(plot_diag$line_y), ]

# SPLITCHAR='#' with SPLITALWAYS -> line breaks
if (nrow(box_text_center) > 0) {
  box_text_center$label <- gsub("#", "\n", box_text_center$text, fixed = TRUE)
}
if (nrow(box_text_left) > 0) {
  box_text_left$label2 <- gsub("#", "\n", box_text_left$text2, fixed = TRUE)
}

minx <- 0; maxx <- 100; miny <- 0; maxy <- 100
text_font <- "mono"

p <- ggplot() +
  geom_polygon(data = boxes,
               aes(x = box_x, y = box_y, group = boxid),
               fill = "white", colour = "black", linewidth = 0.8, lineend = "square") +
  geom_path(data = lines,
            aes(x = line_x, y = line_y, group = lineid),
            colour = "black", linewidth = 0.6, lineend = "butt",
            arrow = grid::arrow(type = "closed", length = grid::unit(0.25, "cm"))) +
  geom_text(data = box_text_center,
            aes(x = text_x, y = text_y, label = label),
            family = text_font, size = 3.5, colour = "black",
            hjust = 0.5, vjust = 0.5, lineheight = 1.0) +
  coord_cartesian(xlim = c(minx, maxx), ylim = c(miny, maxy), expand = FALSE) +
  theme_void() +
  theme(plot.margin = grid::unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.background = element_rect(fill = "white", colour = NA))

if (nrow(box_text_left) > 0) {
  p <- p + geom_text(data = box_text_left,
                     aes(x = text_x2, y = text_y2, label = label2),
                     family = text_font, size = 3.5, colour = "black",
                     hjust = 0, vjust = 0.5, lineheight = 1.0)
}

ggsave(file.path(out, "sgplot.png"), plot = p, width = 8, height = 8, dpi = 300, bg = "white")
