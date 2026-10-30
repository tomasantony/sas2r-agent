```r
# P098 - 3D Waterfall graph (SGF 2019, Matange) translated from SAS to R
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# ---------------- helpers ----------------
round_half_up <- function(x, digits = 0) {
  p <- 10^digits
  sign(x) * floor(abs(x) * p + 0.5 + 1e-9) / p
}

# emulate CALL SYMPUT of a numeric (BEST12. conversion) followed by re-reading as number
best12 <- function(v) {
  if (is.na(v)) return(NA_real_)
  if (v == round(v) && abs(v) < 1e11) return(v)
  neg <- as.integer(v < 0)
  ip <- nchar(format(floor(abs(v)), scientific = FALSE))
  dec <- 12 - neg - ip - 1
  if (dec < 0) dec <- 0
  as.numeric(sprintf(paste0("%.", dec, "f"), round_half_up(v, dec)))
}

# SAS RANUNI: multiplicative congruential generator, one stream per data step
ran_seed <- 3
ranuni <- function() {
  ran_seed <<- (16807 * ran_seed) %% 2147483647
  ran_seed / 2147483647
}

# ---------------- format res. ----------------
res_fmt <- c("0" = "1.0 mg", "1" = "2.0 mg", "2" = "3.0 mg",
             "3" = "4.0 mg", "4" = "3.0 F6 mg", "5" = "4.0 F6 mg")

# ---------------- data tumor ----------------
n <- 20
tumor <- data.frame(i = integer(n), Response = numeric(n), Duration = numeric(n),
                    Dropped = numeric(n), Drug = numeric(n), Code = character(n),
                    codeloc = numeric(n), baseline = numeric(n), stringsAsFactors = FALSE)
for (k in 1:n) {
  Response <- 100 * (ranuni() - 0.7)
  Duration <- 50 * ranuni()
  Dropped <- NA_real_
  if (ranuni() > 0.8) Dropped <- Duration - 5
  Drug <- floor(6 * ranuni())
  Code <- if (ranuni() > 0.7) "FL" else "DLBCL"
  codeloc <- ifelse(Response > 0, 0, Response)
  baseline <- 20 + 180 * ranuni()
  tumor[k, ] <- list(k, Response, Duration, Dropped, Drug, Code, codeloc, baseline)
}
tumor <- tumor[order(-tumor$Response), ]
rownames(tumor) <- NULL
write.csv(tumor, file.path(out, "tumor.csv"), row.names = FALSE, na = "")

# ---------------- attribute map ----------------
wall_cols <- c(Center = "#d0d0d0", Back = "#e0e0e0", Right = "#f0f0f0", Bottom = "#e7e7e7")
resp_cols <- c("1.0 mg" = "white", "2.0 mg" = "#ffffc0", "3.0 mg" = "#f0d0a0",
               "4.0 mg" = "orange", "3.0 F6 mg" = "#ff7f00", "4.0 F6 mg" = "red")

# ---------------- macro ----------------
waterfall_3d <- function(data, xFac = 1, Tilt = 65, Rotate = -55, imagename,
                         Lblx = "X", Lbly = "Y", Lblz = "Z", Title = "") {
  if (xFac > 4) xFac <- 4
  if (xFac < 0.5) xFac <- 0.5
  A <- Tilt; if (Tilt > 80) A <- 80; if (Tilt < 60) A <- 60
  B <- 0
  C <- Rotate; if (Rotate > -15) C <- -15; if (Rotate < -75) C <- -75

  # ---- Wall_Axes ----
  dx <- xFac
  wa <- data.frame(
    id = c("X1-Axis","X3-Axis","X4-Axis","Y2-Axis","Y3-Axis","Y4-Axis","Z1-Axis","Z2-Axis","Z4-Axis"),
    wgroup = c("D","L","D","D","D","L","D","L","D"),
    xw  = c(-dx,-dx,-dx,-dx, dx, dx,-dx, dx, dx),
    yw  = c(-1,-1, 1,-1,-1,-1,-1,-1, 1),
    zw  = c(-1, 1, 1, 1,-1, 1,-1,-1,-1),
    xw2 = c( dx, dx, dx,-dx, dx, dx,-dx, dx, dx),
    yw2 = c(-1,-1, 1, 1, 1, 1,-1,-1, 1),
    zw2 = c(-1, 1, 1, 1,-1, 1, 1, 1, 1),
    xl  = c(0, NA, NA, NA, dx + 0.1, NA, -dx, NA, NA),
    yl  = c(-1, NA, NA, NA, 0, NA, -1.2, NA, NA),
    zl  = c(-1.1, NA, NA, NA, -0.1, NA, 0, NA, NA),
    label = c(1, NA, NA, NA, 2, NA, 3, NA, NA),
    stringsAsFactors = FALSE)
  walls <- data.frame(
    id = rep(c("Back","Right","Bottom","Center"), each = 4), wgroup = "D",
    xw = c(-dx,-dx,-dx,-dx, -dx,dx,dx,-dx, -dx,dx,dx,-dx, -dx,dx,dx,-dx),
    yw = c(-1,1,1,-1, 1,1,1,1, -1,-1,1,1, -1,-1,1,1),
    zw = c(-1,-1,1,1, -1,-1,1,1, -1,-1,-1,-1, 0,0,0,0),
    xw2 = NA_real_, yw2 = NA_real_, zw2 = NA_real_, xl = NA_real_, yl = NA_real_,
    zl = NA_real_, label = NA_real_, stringsAsFactors = FALSE)
  Wall_Axes <- rbind(wa, walls)

  # ---- view matrix ----
  fac <- pi / 180
  Ar <- A * fac; Br <- B * fac; Cr <- C * fac
  r <- 1; t <- 1; f <- 1; nn <- -1
  m <- matrix(c(1/r,0,0,0, 0,1/t,0,0, 0,0,-2/(f-nn),-(f+nn)/(f-nn), 0,0,0,1), 4, 4, byrow = TRUE)
  rx <- matrix(c(1,0,0,0, 0,cos(Ar),-sin(Ar),0, 0,sin(Ar),cos(Ar),0, 0,0,0,1), 4, 4, byrow = TRUE)
  ry <- matrix(c(cos(Br),0,sin(Br),0, 0,1,0,0, -sin(Br),0,cos(Br),0, 0,0,0,1), 4, 4, byrow = TRUE)
  rz <- matrix(c(cos(Cr),-sin(Cr),0,0, sin(Cr),cos(Cr),0,0, 0,0,1,0, 0,0,0,1), 4, 4, byrow = TRUE)
  u <- rz %*% m; v <- ry %*% u; w <- rx %*% v
  proj <- function(x, y, z) w %*% rbind(x, y, z, 1)

  # ---- projected_walls ----
  pw <- Wall_Axes
  pw$lbx <- ifelse(!is.na(pw$label) & pw$label == 1, Lblx, NA)
  pw$lby <- ifelse(!is.na(pw$label) & pw$label == 2, Lbly, NA)
  pw$lbz <- ifelse(!is.na(pw$label) & pw$label == 3, Lblz, NA)
  P <- proj(pw$xw, pw$yw, pw$zw);   pw$xw <- P[1,]; pw$yw <- P[2,]; pw$zw <- P[3,]
  P <- proj(pw$xw2, pw$yw2, pw$zw2); pw$xw2 <- P[1,]; pw$yw2 <- P[2,]; pw$zw2 <- P[3,]
  P <- proj(pw$xl, pw$yl, pw$zl);   pw$xl <- P[1,]; pw$yl <- P[2,]; pw$zl <- P[3,]
  projected_walls <- pw

  # ---- axis label rotation ----
  sfac <- sqrt(xFac)
  rot_of <- function(row) {
    ddx <- row$xw2 - row$xw
    if (abs(ddx) > 0.001) sfac * 180 * atan((row$yw2 - row$yw) / ddx) / 3.1415 else 90
  }
  xaxisRot <- best12(rot_of(pw[pw$id == "X1-Axis", ]))
  yaxisRot <- best12(rot_of(pw[pw$id == "Y3-Axis", ]))

  # ---- min / max ----
  DurMin <- best12(min(1e6, data$Duration, na.rm = TRUE))
  DurMax <- best12(max(-1e6, data$Duration, na.rm = TRUE))
  ResMin <- best12(min(1e6, data$Response, na.rm = TRUE))
  ResMax <- best12(max(-1e6, data$Response, na.rm = TRUE))

  # ---- polygons ----
  N <- 2000
  pg <- data.frame(Drug = rep(NA_real_, N), pid = NA_real_, type = NA_character_,
                   x = NA_real_, y = NA_real_, z = NA_real_, Code = NA_character_,
                   Lid = NA_real_, yTickVal = NA_real_, zTickVal = NA_real_,
                   stringsAsFactors = FALSE)
  k <- 0
  emit <- function(Drug, pid, type, x, y, z, Code, Lid = NA, yTickVal = NA, zTickVal = NA) {
    k <<- k + 1
    pg[k, ] <<- list(Drug, pid, type, x, y, z, Code, Lid, yTickVal, zTickVal)
  }
  pid <- 0
  nr <- nrow(data)
  aResMax <- 1.05 * max(abs(ResMax), abs(ResMin))
  aDurMax <- 1.05 * max(abs(DurMax), abs(DurMin))
  for (ii in 1:nr) {
    i <- ii - 1
    Dur <- data$Duration[ii]; Res <- data$Response[ii]
    Drug <- data$Drug[ii]; Code <- data$Code[ii]; Drp <- data$Dropped[ii]
    # Duration bars
    pid <- pid + 1; z <- 0
    x1 <- dx * (i - 0.5 - 0.4); x2 <- dx * (i - 0.5 + 0.4)
    emit(Drug, pid, "Duration", x1, 0, z, Code)
    emit(Drug, pid, "Duration", x1, Dur, z, Code)
    emit(Drug, pid, "Duration", x2, Dur, z, Code)
    emit(Drug, pid, "Duration", x2, 0, z, Code)
    # Response bars
    pid <- pid + 1; y <- 0
    emit(Drug, pid, "Response", x1, y, 0, Code)
    emit(Drug, pid, "Response", x1, y, Res, Code)
    emit(Drug, pid, "Response", x2, y, Res, Code)
    emit(Drug, pid, "Response", x2, y, 0, Code)
    # Dropped diamonds
    if (!is.na(Drp) && Drp != 0) {
      pid <- pid + 1
      emit(Drug, pid, "Dropped", dx * (i - 0.5 - 0.3), Dur - 0.03 * DurMax, 0, Code)
      emit(Drug, pid, "Dropped", dx * (i - 0.5),       Dur,                  0, Code)
      emit(Drug, pid, "Dropped", dx * (i - 0.5 + 0.3), Dur - 0.03 * DurMax, 0, Code)
      emit(Drug, pid, "Dropped", dx * (i - 0.5),       Dur - 0.06 * DurMax, 0, Code)
    }
    xMax <- dx * i
    # Code location
    emit(Drug, pid, "Code", dx * (i - 0.5), 0, ifelse(Res < 0, Res, 0), Code)

    if (ii == nr) {
      Code <- NA_character_
      pid <- pid + 1
      emit(Drug, pid, "Response", -dx, 0, -aResMax, Code)
      emit(Drug, pid, "Response", -dx, 0,  aResMax, Code)
      pid <- pid + 1
      emit(Drug, pid, "Duration", -dx, 0,       aResMax, Code)
      emit(Drug, pid, "Duration", -dx, aDurMax, aResMax, Code)
      # Y grid
      pid <- NA; Lid <- 1; yy <- 0
      while (yy <= aDurMax) {
        emit(Drug, pid, "Y-Axis", -dx, yy, 0, Code, Lid)
        emit(Drug, pid, "Y-Axis", xMax, yy, 0, Code, Lid)
        Lid <- Lid + 1
        yy <- yy + 10
      }
      # Y tick values
      yy <- 0
      while (yy <= aDurMax) {
        emit(Drug, pid, "Y-Values", xMax, yy, 0, Code, NA, yy)
        yy <- yy + 10
      }
      # Z tick values
      zz <- -10
      while (zz >= -aResMax) {
        emit(Drug, pid, "Z-Values", -dx, 0, zz, Code, NA, NA, zz)
        zz <- zz - 10
      }
      zz <- 0
      while (zz <= aResMax) {
        emit(Drug, pid, "Z-Values", -dx, 0, zz, Code, NA, NA, zz)
        zz <- zz + 10
      }
    }
  }
  polygons <- pg[seq_len(k), ]

  # ---- ranges ----
  xmin <- best12(min(1e10, polygons$x, na.rm = TRUE)); xmax <- best12(max(-1e10, polygons$x, na.rm = TRUE))
  ymin <- best12(min(1e10, polygons$y, na.rm = TRUE)); ymax <- best12(max(-1e10, polygons$y, na.rm = TRUE))
  zmin <- best12(min(1e10, polygons$z, na.rm = TRUE)); zmax <- best12(max(-1e10, polygons$z, na.rm = TRUE))

  # ---- normalized ----
  nm <- polygons
  nm$x <- 2 * xFac * (nm$x - xmin) / (xmax - xmin) - xFac
  nm$y <- 2 * (nm$y - ymin) / (ymax - ymin) - 1
  nm$z <- 2 * (nm$z - zmin) / (zmax - zmin) - 1

  # ---- projected_data ----
  P <- proj(nm$x, nm$y, nm$z)
  pd <- nm[, c("Drug", "pid", "type", "Code", "Lid", "yTickVal", "zTickVal")]
  sel <- function(tp, row) ifelse(nm$type == tp, P[row, ], NA_real_)
  pd$xd <- sel("Duration", 1); pd$yd <- sel("Duration", 2); pd$zd <- sel("Duration", 3)
  pd$xr <- sel("Response", 1); pd$yr <- sel("Response", 2); pd$zr <- sel("Response", 3)
  pd$xc <- sel("Dropped", 1);  pd$yc <- sel("Dropped", 2);  pd$zc <- sel("Dropped", 3)
  pd$xcode <- sel("Code", 1);  pd$ycode <- sel("Code", 2);  pd$zcode <- sel("Code", 3)
  pd$xAy <- sel("Y-Axis", 1);  pd$yAy <- sel("Y-Axis", 2);  pd$zAy <- sel("Y-Axis", 3)
  pd$xYv <- sel("Y-Values", 1); pd$yYv <- sel("Y-Values", 2)
  pd$xZv <- sel("Z-Values", 1); pd$yZv <- sel("Z-Values", 2)
  projected_data <- pd

  # ---- combined (one-to-one merge) ----
  nc <- max(nrow(projected_walls), nrow(projected_data))
  padto <- function(df, n) { if (nrow(df) < n) df[(nrow(df) + 1):n, ] <- NA; rownames(df) <- NULL; df }
  combined <- cbind(padto(projected_walls, nc), padto(projected_data, nc))
  combined$DrugF <- factor(res_fmt[as.character(combined$Drug)], levels = c(names(resp_cols), "Drop"))

  write.csv(combined[, setdiff(names(combined), "DrugF")],
            file.path(out, paste0(tolower(imagename), ".csv")), row.names = FALSE, na = "")

  # ---- plot ----
  pt <- function(s) s / ggplot2::.pt
  g <- ggplot()
  for (wn in c("Back", "Right", "Bottom", "Center")) {
    g <- g + geom_polygon(data = combined[!is.na(combined$id) & combined$id == wn, ],
                          aes(x = xw, y = yw), fill = wall_cols[wn], colour = wall_cols[wn])
  }
  grid <- combined[!is.na(combined$xAy), ]
  g <- g + geom_path(data = grid, aes(x = xAy, y = yAy, group = Lid), colour = "#b0b0b0")
  durd <- combined[!is.na(combined$xd), ]
  g <- g + geom_polygon(data = durd, aes(x = xd, y = yd, group = pid),
                        fill = "#7C95CA", colour = "black", linewidth = 0.2)
  dropd <- combined[!is.na(combined$xc), ]
  if (nrow(dropd) > 0) {
    dropd$fillv <- factor("Drop", levels = levels(combined$DrugF))
    g <- g + geom_polygon(data = dropd, aes(x = xc, y = yc, group = pid, fill = fillv), colour = NA)
  }
  respd <- combined[!is.na(combined$xr), ]
  g <- g + geom_polygon(data = respd, aes(x = xr, y = yr, group = pid, fill = DrugF),
                        colour = "black", linewidth = 0.2)
  vec <- combined[!is.na(combined$xw2), ]
  g <- g + geom_segment(data = vec, aes(x = xw, y = yw, xend = xw2, yend = yw2, linetype = wgroup),
                        colour = "black", linewidth = 0.3, show.legend = FALSE) +
    scale_linetype_manual(values = c(D = "solid", L = "dashed"))
  lx <- combined[!is.na(combined$lbx), ]
  ly <- combined[!is.na(combined$lby), ]
  lz <- combined[!is.na(combined$lbz), ]
  g <- g +
    geom_text(data = lx, aes(x = xl, y = yl, label = lbx), angle = xaxisRot, hjust = 1, vjust = 1, size = pt(5)) +
    geom_text(data = ly, aes(x = xl, y = yl, label = lby), angle = yaxisRot, hjust = 0.5, vjust = 1, size = pt(5)) +
    geom_text(data = lz, aes(x = xl, y = yl, label = lbz), angle = 90, hjust = 0.5, vjust = 0, size = pt(5))
  yv <- combined[!is.na(combined$xYv), ]
  zv <- combined[!is.na(combined$xZv), ]
  g <- g +
    geom_text(data = yv, aes(x = xYv, y = yYv, label = format(yTickVal, trim = TRUE)),
              hjust = 0, vjust = 1, size = pt(4)) +
    geom_text(data = zv, aes(x = xZv, y = yZv, label = format(zTickVal, trim = TRUE)),
              hjust = 1.2, vjust = 0.5, size = pt(4)) +
    geom_point(data = zv, aes(x = xZv, y = yZv), size = 0.3)
  cd <- combined[!is.na(combined$xcode), ]
  g <- g + geom_text(data = cd, aes(x = xcode, y = ycode, label = ifelse(is.na(Code), "", Code)),
                     angle = 90, hjust = 1.1, vjust = 0.5, size = pt(4))
  g <- g + scale_fill_manual(name = "CC-122", values = c(resp_cols, Drop = "gold"),
                             breaks = c(names(resp_cols), "Drop"), drop = FALSE) +
    coord_fixed() +
    labs(title = Title, caption = paste0("X-Rotation=", A, "  Y-Rotation=", B, "  Z-Rotation=", C)) +
    theme_void() +
    theme(plot.title = element_text(size = 12, hjust = 0.5),
          plot.caption = element_text(size = 5, hjust = 0, face = "italic"),
          legend.title = element_text(size = 5), legend.text = element_text(size = 5),
          legend.key.size = unit(5, "pt"),
          legend.background = element_rect(fill = "white", colour = NA),
          plot.background = element_rect(fill = "white", colour = NA))
  if (packageVersion("ggplot2") >= "3.5.0") {
    g <- g + theme(legend.position = "inside", legend.position.inside = c(0.98, 0.02),
                   legend.justification = c(1, 0))
  } else {
    g <- g + theme(legend.position = c(0.98, 0.02), legend.justification = c(1, 0))
  }
  ggsave(file.path(out, paste0(tolower(imagename), ".png")), g, width = 4, height = 4, dpi = 300)
  message("Wrote ", imagename)
  invisible(combined)
}

for (xf in c(1.0, 1.25, 1.5)) {
  nm <- sprintf("Waterfall_Xf_%d", as.integer(round_half_up(xf * 100)))
  waterfall_3d(tumor, xFac = xf, Tilt = 75, Rotate = -45, imagename = nm,
               Lblx = "Subject", Lbly = "Duration of Treatment in Days",
               Lblz = "Tumor Lesion Best Change", Title = "Tumor Response and Duration")
}
message("Done.")
```