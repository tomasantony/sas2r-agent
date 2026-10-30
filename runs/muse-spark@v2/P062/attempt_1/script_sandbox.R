out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

library(ggplot2)

# ---- helpers mimicking SAS macros ----
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
  # centre = intersection of diagonals (same as mean for rectangle)
  cx <- (left_x + right_x) / 2
  cy <- (top_y + bottom_y) / 2
  if (toupper(text_just) != "LEFT") {
    text_df <- data.frame(
      DIAGID = as.character(boxid),
      text_x = cx,
      text_y = cy,
      text_x2 = NA_real_,
      text_y2 = NA_real_,
      text = text,
      text2 = NA_character_,
      sizeresponse = text_size,
      stringsAsFactors = FALSE
    )
  } else {
    text_df <- data.frame(
      DIAGID = as.character(boxid),
      text_x = NA_real_,
      text_y = NA_real_,
      text_x2 = left_x + 1,
      text_y2 = cy,
      text = NA_character_,
      text2 = text,
      sizeresponse = text_size,
      stringsAsFactors = FALSE
    )
  }
  style_df <- data.frame(
    id = "DIAGID",
    value = as.character(boxid),
    textcolor = text_color,
    linecolor = linecolor,
    linepattern = linepattern,
    linethickness = linethickness,
    stringsAsFactors = FALSE
  )
  list(box=box_df, text=text_df, style=style_df)
}

diagram_line <- function(lineid=1, start_x=30, start_y=90,
                         midpoint_x=NULL, midpoint_y=NULL,
                         end_x=30, end_y=60,
                         linecolor="black", linepattern="solid", linethickness=1) {
  if (!is.null(midpoint_x) && !is.null(midpoint_y) &&
      !is.na(midpoint_x) && !is.na(midpoint_y)) {
    line_df <- data.frame(
      `_LINEID` = as.character(lineid),
      lineid = lineid,
      line_x = c(start_x, midpoint_x, end_x),
      line_y = c(start_y, midpoint_y, end_y),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  } else {
    line_df <- data.frame(
      `_LINEID` = as.character(lineid),
      lineid = lineid,
      line_x = c(start_x, end_x),
      line_y = c(start_y, end_y),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }
  names(line_df)[1] <- "_LINEID"
  style_df <- data.frame(
    id = "_LINEID",
    value = as.character(lineid),
    linecolor = linecolor,
    linepattern = linepattern,
    linethickness = linethickness,
    stringsAsFactors = FALSE
  )
  list(line=line_df, style=style_df)
}

# ---- build header usage example ----
b1 <- diagram_box(boxid=1, left_x=30, top_y=100, right_x=70, bottom_y=90,
                  text="Assessed for eligibility (n=100)")
l1 <- diagram_line(lineid=1, start_x=50, start_y=90, end_x=50, end_y=55)
b2 <- diagram_box(boxid=2, left_x=37, top_y=55, right_x=63, bottom_y=45,
                  text="Randomized (n=90)")

boxes <- rbind(b1$box, b2$box)
texts <- rbind(b1$text, b2$text)
lines <- l1$line

# ---- plot_diag : SET diagram_: equivalent (stack with missing fill) ----
# Build a combined data set for CSV export (one row per polygon vertex / text / line point)
mk_na <- function(df, cols) {
  for (cl in cols) if (!cl %in% names(df)) df[[cl]] <- NA
  df
}
all_cols <- c("DIAGID","boxid","box_x","box_y",
              "text_x","text_y","text_x2","text_y2","text","text2","sizeresponse",
              "_LINEID","lineid","line_x","line_y")
box_all  <- mk_na(cbind(source="diagram_box", boxes), all_cols)
text_all <- mk_na(cbind(source="diagram_box_text", texts), all_cols)
line_all <- mk_na(cbind(source="diagram_line", lines), all_cols)
plot_diag <- rbind(
  data.frame(source=box_all$source, box_all[, all_cols], stringsAsFactors=FALSE, check.names=FALSE),
  data.frame(source=text_all$source, text_all[, all_cols], stringsAsFactors=FALSE, check.names=FALSE),
  data.frame(source=line_all$source, line_all[, all_cols], stringsAsFactors=FALSE, check.names=FALSE)
)

# CSV of the plotted data
write.csv(plot_diag, file.path(out, "sgplot.csv"), row.names=FALSE, na="")

# ---- SGPLOT equivalent with ggplot2 ----
# polygon id=boxid x=box_x y=box_y ; text ; series with arrowhead filled at end scale 0.3
p <- ggplot() +
  geom_polygon(data=boxes, aes(x=box_x, y=box_y, group=boxid),
               fill="white", colour="black", linewidth=0.6) +
  geom_path(data=lines, aes(x=line_x, y=line_y, group=lineid),
            colour="black", linewidth=0.6,
            arrow=grid::arrow(type="closed", ends="last",
                              length=grid::unit(0.12, "inches"))) +
  geom_text(data=texts, aes(x=text_x, y=text_y, label=text),
            family="mono", size=3.51, colour="black") +
  coord_cartesian(xlim=c(0, 100), ylim=c(0, 100), expand=FALSE) +
  theme_void() +
  theme(legend.position="none",
        plot.margin=grid::unit(c(0.2,0.2,0.2,0.2), "inches"))

ggsave(file.path(out, "sgplot.png"), plot=p, width=8, height=6, dpi=300, units="in")
