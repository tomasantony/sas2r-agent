out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# helper: SAS ranuni stream
sas_ranuni_stream <- function(seed_init, n) {
  m <- 2147483647
  a <- 397204094
  seed <- as.numeric(seed_init)
  vals <- numeric(n)
  for (i in seq_len(n)) {
    seed <- (a * seed) %% m
    vals[i] <- seed / m
  }
  vals
}

# Generate tumor data exactly as SAS ranuni(3) and ranuni(2)
# per iteration: 5 calls to stream 3, 1 call to stream 2
n_obs <- 20
n_calls_3 <- n_obs * 5
n_calls_2 <- n_obs * 1
u3 <- sas_ranuni_stream(3, n_calls_3)
u2 <- sas_ranuni_stream(2, n_calls_2)

# Build tumor unsorted
tumor <- data.frame(
  i = 1:n_obs,
  Response = NA_real_,
  Duration = NA_real_,
  Dropped = NA_real_,
  Drug = NA_integer_,
  Code = NA_character_,
  codeloc = NA_real_,
  baseline = NA_real_,
  stringsAsFactors = FALSE
)

idx3 <- 1
idx2 <- 1
for (i in 1:n_obs) {
  r1 <- u3[idx3]; idx3 <- idx3 + 1
  Response <- 100 * (r1 - 0.7)
  r2 <- u3[idx3]; idx3 <- idx3 + 1
  Duration <- 50 * r2
  r3 <- u3[idx3]; idx3 <- idx3 + 1
  Dropped <- NA_real_
  if (r3 > 0.8) Dropped <- Duration - 5
  r4 <- u3[idx3]; idx3 <- idx3 + 1
  Drug <- floor(6 * r4)
  r5 <- u3[idx3]; idx3 <- idx3 + 1
  Code <- if (r5 > 0.7) "FL" else "DLBCL"
  r6 <- u2[idx2]; idx2 <- idx2 + 1
  baseline <- 20 + 180 * r6
  codeloc <- ifelse(Response > 0, 0, Response)
  tumor$Response[i] <- Response
  tumor$Duration[i] <- Duration
  tumor$Dropped[i] <- Dropped
  tumor$Drug[i] <- Drug
  tumor$Code[i] <- Code
  tumor$codeloc[i] <- codeloc
  tumor$baseline[i] <- baseline
}

# format Drug labels
drug_fmt <- c("1.0 mg","2.0 mg","3.0 mg","4.0 mg","3.0 F6 mg","4.0 F6 mg")
tumor$Drug_label <- drug_fmt[tumor$Drug + 1]

# Sort by descending Response (SAS proc sort)
tumor <- tumor[order(-tumor$Response), , drop=FALSE]
tumor$j <- seq_len(nrow(tumor))

# attrmap colors for Resp
resp_colors <- c(
  "1.0 mg" = "white",
  "2.0 mg" = "#FFFFC0",
  "3.0 mg" = "#F0D0A0",
  "4.0 mg" = "orange",
  "3.0 F6 mg" = "#FF7F00",
  "4.0 F6 mg" = "red"
)
# normalize to hex where needed
resp_colors["white"] <- "#FFFFFF"

# wall colors
wall_fill <- c(
  "Center" = "#D0D0D0",
  "Back"   = "#E0E0E0",
  "Right"  = "#F0F0F0",
  "Bottom" = "#E7E7E7"
)

# Function to build waterfall 3D projection for a given xFac
build_waterfall <- function(tumor_df, xFac=1, Tilt=75, Rotate=-45,
                            Lblx="Subject", Lbly="Duration of Treatment in Days",
                            Lblz="Tumor Lesion Best Change", Title="Tumor Response and Duration") {
  # clamp parameters as SAS macro does
  if (xFac > 4) xFac <- 4
  if (xFac < 0.5) xFac <- 0.5
  A <- Tilt
  if (A > 80) A <- 80
  if (A < 60) A <- 60
  B <- 0
  C <- Rotate
  if (C > -15) C <- -15
  if (C < -75) C <- -75

  # Wall_Axes
  dx <- xFac
  Wall_Axes <- data.frame(
    id = c("X1-Axis","X3-Axis","X4-Axis","Y2-Axis","Y3-Axis","Y4-Axis","Z1-Axis","Z2-Axis","Z4-Axis",
           "Back","Back","Back","Back","Right","Right","Right","Right","Bottom","Bottom","Bottom","Bottom",
           "Center","Center","Center","Center"),
    wgroup = c("D","L","D","D","D","L","D","L","D",
               "D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D"),
    xw = c(-dx,-dx,-dx,-dx, dx, dx,-dx, dx, dx,
           -dx,-dx,-dx,-dx,-dx, dx, dx,-dx,-dx, dx, dx,-dx,-dx, dx, dx,-dx),
    yw = c(-1,-1, 1,-1,-1,-1,-1,-1, 1,
           -1, 1, 1,-1, 1, 1, 1, 1,-1,-1, 1, 1,-1,-1, 1, 1),
    zw = c(-1, 1, 1, 1,-1, 1,-1,-1,-1,
           -1,-1, 1, 1,-1,-1, 1, 1,-1,-1,-1,-1, 0, 0, 0, 0),
    xw2 = c( dx, dx, dx,-dx, dx, dx,-dx, dx, dx, rep(NA,16)),
    yw2 = c(-1,-1, 1, 1, 1, 1,-1,-1, 1, rep(NA,16)),
    zw2 = c(-1, 1, 1, 1,-1, 1, 1, 1, 1, rep(NA,16)),
    xl = c(0, NA, NA, NA, dx+0.1, NA, -dx, NA, NA, rep(NA,16)),
    yl = c(-1, NA, NA, NA, 0, NA, -1.2, NA, NA, rep(NA,16)),
    zl = c(-1.1, NA, NA, NA, -0.1, NA, 0, NA, NA, rep(NA,16)),
    label = c(1, NA, NA, NA, 2, NA, 3, NA, NA, rep(NA,16)),
    stringsAsFactors = FALSE
  )

  # projection matrices
  r <- 1; t <- 1; f <- 1; n <- -1
  pi_val <- pi
  fac <- pi_val/180
  Af <- A*fac; Bf <- B*fac; Cf <- C*fac

  m_mat <- matrix(0,4,4)
  m_mat[1,1] <- 1/r
  m_mat[2,2] <- 1/t
  m_mat[3,3] <- -2/(f-n)
  m_mat[3,4] <- -(f+n)/(f-n)
  m_mat[4,4] <- 1

  rx <- matrix(0,4,4)
  rx[1,1] <- 1
  rx[2,2] <- cos(Af); rx[2,3] <- -sin(Af)
  rx[3,2] <- sin(Af); rx[3,3] <- cos(Af)
  rx[4,4] <- 1

  ry <- matrix(0,4,4)
  ry[1,1] <- cos(Bf); ry[1,3] <- sin(Bf)
  ry[2,2] <- 1
  ry[3,1] <- -sin(Bf); ry[3,3] <- cos(Bf)
  ry[4,4] <- 1

  rz <- matrix(0,4,4)
  rz[1,1] <- cos(Cf); rz[1,2] <- -sin(Cf)
  rz[2,1] <- sin(Cf); rz[2,2] <- cos(Cf)
  rz[3,3] <- 1
  rz[4,4] <- 1

  # w = rx * ry * rz * m
  u_mat <- rz %*% m_mat
  v_mat <- ry %*% u_mat
  w_mat <- rx %*% v_mat

  # project walls
  projected_walls <- Wall_Axes
  projected_walls$lbx <- NA_character_
  projected_walls$lby <- NA_character_
  projected_walls$lbz <- NA_character_
  for (k in seq_len(nrow(projected_walls))) {
    if (!is.na(projected_walls$label[k])) {
      if (projected_walls$label[k]==1) projected_walls$lbx[k] <- Lblx
      if (projected_walls$label[k]==2) projected_walls$lby[k] <- Lbly
      if (projected_walls$label[k]==3) projected_walls$lbz[k] <- Lblz
    }
    # transform walls
    d <- c(projected_walls$xw[k], projected_walls$yw[k], projected_walls$zw[k], 1)
    p <- w_mat %*% d
    projected_walls$xw[k] <- p[1]
    projected_walls$yw[k] <- p[2]
    projected_walls$zw[k] <- p[3]
    # transform axes if not NA
    if (!is.na(projected_walls$xw2[k])) {
      d2 <- c(projected_walls$xw2[k], projected_walls$yw2[k], projected_walls$zw2[k], 1)
      p2 <- w_mat %*% d2
      projected_walls$xw2[k] <- p2[1]
      projected_walls$yw2[k] <- p2[2]
      projected_walls$zw2[k] <- p2[3]
    }
    if (!is.na(projected_walls$xl[k])) {
      d3 <- c(projected_walls$xl[k], projected_walls$yl[k], projected_walls$zl[k], 1)
      p3 <- w_mat %*% d3
      projected_walls$xl[k] <- p3[1]
      projected_walls$yl[k] <- p3[2]
      projected_walls$zl[k] <- p3[3]
    }
  }

  # compute axis rotations
  fac2 <- sqrt(xFac)
  xaxisRot <- 0
  yaxisRot <- 0
  for (k in seq_len(nrow(projected_walls))) {
    if (projected_walls$id[k]=="X1-Axis") {
      dx2 <- projected_walls$xw2[k] - projected_walls$xw[k]
      if (abs(dx2) > 0.001) xaxisRot <- fac2*180*atan((projected_walls$yw2[k]-projected_walls$yw[k])/dx2)/pi
      else xaxisRot <- 90
    }
    if (projected_walls$id[k]=="Y3-Axis") {
      dx2 <- projected_walls$xw2[k] - projected_walls$xw[k]
      if (abs(dx2) > 0.001) yaxisRot <- fac2*180*atan((projected_walls$yw2[k]-projected_walls$yw[k])/dx2)/pi
      else yaxisRot <- 90
    }
  }

  # find min/max for Duration and Response
  DurMin <- min(tumor_df$Duration, na.rm=TRUE)
  DurMax <- max(tumor_df$Duration, na.rm=TRUE)
  ResMin <- min(tumor_df$Response, na.rm=TRUE)
  ResMax <- max(tumor_df$Response, na.rm=TRUE)

  # create polygons
  durfac <- 1.05
  resfac <- 1.05
  aResMax <- resfac * max(abs(ResMax), abs(ResMin))
  aDurMax <- durfac * max(abs(DurMax), abs(DurMin))

  polys <- list()
  pid <- 0
  xMax <- NA
  # helper to append row
  add_row <- function(type, pid, x, y, z, code=NA, Lid=NA, yTickVal=NA, zTickVal=NA, Drug=NA, Drug_label=NA) {
    data.frame(type=type, pid=pid, x=x, y=y, z=z, code=code, Lid=Lid, yTickVal=yTickVal, zTickVal=zTickVal,
               Drug=Drug, Drug_label=Drug_label, stringsAsFactors=FALSE)
  }

  for (idx in seq_len(nrow(tumor_df))) {
    i <- idx - 1  # SAS _n_-1
    Duration <- tumor_df$Duration[idx]
    Response <- tumor_df$Response[idx]
    Dropped <- tumor_df$Dropped[idx]
    Code <- tumor_df$Code[idx]
    Drug <- tumor_df$Drug[idx]
    Drug_label <- tumor_df$Drug_label[idx]

    # Duration bars
    pid <- pid + 1
    z <- 0
    polys[[length(polys)+1]] <- add_row("Duration", pid, dx*(i-0.5-0.4), 0, z, Drug=Drug, Drug_label=Drug_label)
    polys[[length(polys)+1]] <- add_row("Duration", pid, dx*(i-0.5-0.4), Duration, z, Drug=Drug, Drug_label=Drug_label)
    polys[[length(polys)+1]] <- add_row("Duration", pid, dx*(i-0.5+0.4), Duration, z, Drug=Drug, Drug_label=Drug_label)
    polys[[length(polys)+1]] <- add_row("Duration", pid, dx*(i-0.5+0.4), 0, z, Drug=Drug, Drug_label=Drug_label)

    # Response bars
    pid <- pid + 1
    y <- 0
    polys[[length(polys)+1]] <- add_row("Response", pid, dx*(i-0.5-0.4), y, 0, Drug=Drug, Drug_label=Drug_label)
    polys[[length(polys)+1]] <- add_row("Response", pid, dx*(i-0.5-0.4), y, Response, Drug=Drug, Drug_label=Drug_label)
    polys[[length(polys)+1]] <- add_row("Response", pid, dx*(i-0.5+0.4), y, Response, Drug=Drug, Drug_label=Drug_label)
    polys[[length(polys)+1]] <- add_row("Response", pid, dx*(i-0.5+0.4), y, 0, Drug=Drug, Drug_label=Drug_label)

    # Dropped diamond
    if (!is.na(Dropped) && Dropped) {
      pid <- pid + 1
      z <- 0
      polys[[length(polys)+1]] <- add_row("Dropped", pid, dx*(i-0.5-0.3), Duration-0.03*DurMax, z, Drug=Drug, Drug_label=Drug_label)
      polys[[length(polys)+1]] <- add_row("Dropped", pid, dx*(i-0.5), Duration, z, Drug=Drug, Drug_label=Drug_label)
      polys[[length(polys)+1]] <- add_row("Dropped", pid, dx*(i-0.5+0.3), Duration-0.03*DurMax, z, Drug=Drug, Drug_label=Drug_label)
      polys[[length(polys)+1]] <- add_row("Dropped", pid, dx*(i-0.5), Duration-0.06*DurMax, z, Drug=Drug, Drug_label=Drug_label)
    }

    xMax <- dx * i

    # Code location
    zc <- ifelse(Response < 0, Response, 0)
    polys[[length(polys)+1]] <- add_row("Code", NA, dx*(i-0.5), 0, zc, code=Code, Drug=Drug, Drug_label=Drug_label)
  }

  # last observation extra polygons
  # two more polygons to set aspect
  pid <- pid + 1
  polys[[length(polys)+1]] <- add_row("Response", pid, -dx, 0, -aResMax)
  polys[[length(polys)+1]] <- add_row("Response", pid, -dx, 0,  aResMax)
  pid <- pid + 1
  polys[[length(polys)+1]] <- add_row("Duration", pid, -dx, 0, aResMax)
  polys[[length(polys)+1]] <- add_row("Duration", pid, -dx, aDurMax, aResMax)

  # Y-Axis grid
  Lid <- 1
  z <- 0
  y_vals <- seq(0, aDurMax, by=10)
  for (yv in y_vals) {
    polys[[length(polys)+1]] <- add_row("Y-Axis", NA, -dx, yv, z, Lid=Lid)
    polys[[length(polys)+1]] <- add_row("Y-Axis", NA, xMax, yv, z, Lid=Lid)
    Lid <- Lid + 1
  }
  # Y-Values
  for (yv in y_vals) {
    polys[[length(polys)+1]] <- add_row("Y-Values", NA, xMax, yv, 0, yTickVal=yv)
  }
  # Z-Values
  # from -10 to -aResMax by -10
  z_neg <- seq(-10, -aResMax, by=-10)
  for (zv in z_neg) {
    polys[[length(polys)+1]] <- add_row("Z-Values", NA, dx*(-1), 0, zv, zTickVal=zv)
  }
  z_pos <- seq(0, aResMax, by=10)
  for (zv in z_pos) {
    polys[[length(polys)+1]] <- add_row("Z-Values", NA, dx*(-1), 0, zv, zTickVal=zv)
  }

  polygons <- do.call(rbind, polys)

  # compute data ranges
  xmin <- min(polygons$x, na.rm=TRUE)
  xmax <- max(polygons$x, na.rm=TRUE)
  ymin <- min(polygons$y, na.rm=TRUE)
  ymax <- max(polygons$y, na.rm=TRUE)
  zmin <- min(polygons$z, na.rm=TRUE)
  zmax <- max(polygons$z, na.rm=TRUE)

  xrange <- xmax - xmin
  yrange <- ymax - ymin
  zrange <- zmax - zmin

  normalized <- polygons
  normalized$x <- 2*xFac*(polygons$x - xmin)/xrange - xFac
  normalized$y <- 2*(polygons$y - ymin)/yrange - 1
  normalized$z <- 2*(polygons$z - zmin)/zrange - 1

  # project data
  projected_data <- normalized
  # initialize projected columns
  projected_data$xd <- NA_real_; projected_data$yd <- NA_real_; projected_data$zd <- NA_real_
  projected_data$xr <- NA_real_; projected_data$yr <- NA_real_; projected_data$zr <- NA_real_
  projected_data$xc <- NA_real_; projected_data$yc <- NA_real_; projected_data$zc <- NA_real_
  projected_data$xcode <- NA_real_; projected_data$ycode <- NA_real_; projected_data$zcode <- NA_real_
  projected_data$xAy <- NA_real_; projected_data$yAy <- NA_real_; projected_data$zAy <- NA_real_
  projected_data$xYv <- NA_real_; projected_data$yYv <- NA_real_
  projected_data$xZv <- NA_real_; projected_data$yZv <- NA_real_

  for (k in seq_len(nrow(normalized))) {
    d <- c(normalized$x[k], normalized$y[k], normalized$z[k], 1)
    p <- w_mat %*% d
    tp <- normalized$type[k]
    if (tp=="Duration") {
      projected_data$xd[k] <- p[1]; projected_data$yd[k] <- p[2]; projected_data$zd[k] <- p[3]
    } else if (tp=="Response") {
      projected_data$xr[k] <- p[1]; projected_data$yr[k] <- p[2]; projected_data$zr[k] <- p[3]
    } else if (tp=="Dropped") {
      projected_data$xc[k] <- p[1]; projected_data$yc[k] <- p[2]; projected_data$zc[k] <- p[3]
    } else if (tp=="Code") {
      projected_data$xcode[k] <- p[1]; projected_data$ycode[k] <- p[2]; projected_data$zcode[k] <- p[3]
    } else if (tp=="Y-Axis") {
      projected_data$xAy[k] <- p[1]; projected_data$yAy[k] <- p[2]; projected_data$zAy[k] <- p[3]
    } else if (tp=="Y-Values") {
      projected_data$xYv[k] <- p[1]; projected_data$yYv[k] <- p[2]
    } else if (tp=="Z-Values") {
      projected_data$xZv[k] <- p[1]; projected_data$yZv[k] <- p[2]
    }
  }

  list(
    projected_walls=projected_walls,
    polygons=polygons,
    normalized=normalized,
    projected_data=projected_data,
    xaxisRot=xaxisRot, yaxisRot=yaxisRot,
    A=A, B=B, C=C, xFac=xFac,
    DurMin=DurMin, DurMax=DurMax, ResMin=ResMin, ResMax=ResMax,
    aDurMax=aDurMax, aResMax=aResMax,
    Lblx=Lblx, Lbly=Lbly, Lblz=Lblz, Title=Title
  )
}

# Plotting function
plot_waterfall <- function(res, outfile_png) {
  pw <- res$projected_walls
  pd <- res$projected_data

  # walls polygons: Back, Right, Bottom, Center
  walls <- pw[pw$id %in% c("Back","Right","Bottom","Center"), ]
  # need to create polygon per id: order as in Wall_Axes
  # For ggplot, group by id
  # axes lines
  axes <- pw[!is.na(pw$xw2), ]

  # Duration bars
  dur <- pd[pd$type=="Duration" & !is.na(pd$xd), ]
  # Response bars
  resp <- pd[pd$type=="Response" & !is.na(pd$xr), ]
  # Dropped
  drop <- pd[pd$type=="Dropped" & !is.na(pd$xc), ]
  # Y-Axis grid
  yaxis <- pd[pd$type=="Y-Axis" & !is.na(pd$xAy), ]
  # Y tick values
  yvals <- pd[pd$type=="Y-Values" & !is.na(pd$xYv), ]
  # Z tick values
  zvals <- pd[pd$type=="Z-Values" & !is.na(pd$xZv), ]
  # Code labels
  codes <- pd[pd$type=="Code" & !is.na(pd$xcode), ]

  # axis labels positions
  labs <- pw[!is.na(pw$label), ]

  p <- ggplot2::ggplot() +
    # walls
    ggplot2::geom_polygon(data=walls, ggplot2::aes(x=xw, y=yw, group=id, fill=id), color="grey80", alpha=0.9) +
    ggplot2::scale_fill_manual(values=wall_fill, guide="none") +
    # Y grid
    ggplot2::geom_line(data=yaxis, ggplot2::aes(x=xAy, y=yAy, group=Lid), color="#B0B0B0", linewidth=0.3) +
    # Duration bars
    ggplot2::geom_polygon(data=dur, ggplot2::aes(x=xd, y=yd, group=pid), fill="#7FB3D5", color="black", linewidth=0.3, alpha=0.9) +
    # Response bars colored by Drug
    ggplot2::geom_polygon(data=resp, ggplot2::aes(x=xr, y=yr, group=pid, fill=Drug_label), color="black", linewidth=0.3, alpha=0.95) +
    ggplot2::scale_fill_manual(values=resp_colors, name="CC-122", na.value="grey") +
    # Dropped markers
    ggplot2::geom_polygon(data=drop, ggplot2::aes(x=xc, y=yc, group=pid), fill="gold", color="black", linewidth=0.3) +
    # axes lines
    ggplot2::geom_segment(data=axes, ggplot2::aes(x=xw, y=yw, xend=xw2, yend=yw2, linetype=wgroup), color="black", linewidth=0.5) +
    ggplot2::scale_linetype_manual(values=c(D="solid", L="dashed"), guide="none") +
    # Y tick values
    ggplot2::geom_text(data=yvals, ggplot2::aes(x=xYv, y=yYv, label=yTickVal), size=2, hjust=1, vjust=0.5) +
    # Z tick values
    ggplot2::geom_text(data=zvals, ggplot2::aes(x=xZv, y=yZv, label=zTickVal), size=2, hjust=1, vjust=0.5) +
    ggplot2::geom_point(data=zvals, ggplot2::aes(x=xZv, y=yZv), size=0.8) +
    # Code labels
    ggplot2::geom_text(data=codes, ggplot2::aes(x=xcode, y=ycode, label=code), size=2, angle=90, hjust=0, vjust=0.5) +
    # axis labels
    ggplot2::geom_text(data=labs[labs$label==1,], ggplot2::aes(x=xl, y=yl, label=lbx), size=2.5, angle=res$xaxisRot, hjust=0.5, vjust=1) +
    ggplot2::geom_text(data=labs[labs$label==2,], ggplot2::aes(x=xl, y=yl, label=lby), size=2.5, angle=res$yaxisRot, hjust=0.5, vjust=1) +
    ggplot2::geom_text(data=labs[labs$label==3,], ggplot2::aes(x=xl, y=yl, label=lbz), size=2.5, angle=90, hjust=0.5) +
    ggplot2::labs(title=res$Title,
                  subtitle=paste0("X-Rotation=",res$A,"  Y-Rotation=",res$B,"  Z-Rotation=",res$C,"  xFac=",res$xFac)) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title=ggplot2::element_text(size=10, hjust=0.5, face="bold"),
      plot.subtitle=ggplot2::element_text(size=5, hjust=0, face="italic"),
      legend.position=c(0.85,0.15),
      legend.title=ggplot2::element_text(size=5),
      legend.text=ggplot2::element_text(size=5),
      legend.key.size=ggplot2::unit(4,"pt"),
      plot.margin=ggplot2::margin(10,10,10,10)
    ) +
    ggplot2::coord_equal()

  ggplot2::ggsave(outfile_png, plot=p, width=4, height=4, units="in", dpi=300)
  invisible(p)
}

# Generate three figures
configs <- list(
  list(xFac=1.0, suffix="100"),
  list(xFac=1.25, suffix="125"),
  list(xFac=1.5, suffix="150")
)

for (cfg in configs) {
  res <- build_waterfall(tumor, xFac=cfg$xFac, Tilt=75, Rotate=-45,
                         Lblx="Subject", Lbly="Duration of Treatment in Days",
                         Lblz="Tumor Lesion Best Change",
                         Title="Tumor Response and Duration")
  png_file <- file.path(out, paste0("waterfall_xf_", cfg$suffix, ".png"))
  plot_waterfall(res, png_file)

  # CSV of plotted data: combine projected_data and walls for traceability
  # Create a CSV that contains the projected coordinates used for plotting
  csv_file <- file.path(out, paste0("waterfall_xf_", cfg$suffix, ".csv"))
  # Export projected_data with key columns
  export_df <- res$projected_data[, c("type","pid","x","y","z","Drug","Drug_label","code","Lid","yTickVal","zTickVal",
                                      "xd","yd","xr","yr","xc","yc","xAy","yAy","xYv","yYv","xZv","yZv","xcode","ycode")]
  # also add walls for reference as separate rows
  walls_export <- res$projected_walls[, c("id","wgroup","xw","yw","zw","xw2","yw2","zw2","xl","yl","zl","lbx","lby","lbz")]
  # Write main plotted data
  write.csv(export_df, csv_file, row.names=FALSE, na="")

  # Also write walls as separate file if needed (optional)
  # For completeness, also write tumor source
}

# Also write tumor dataset for reference
write.csv(tumor, file.path(out, "tumor.csv"), row.names=FALSE, na="")
