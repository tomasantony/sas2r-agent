```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(ggplot2)
})

# ---------- SAS RANUNI emulation (Fishman-Moore LCG) ----------
# a=397204094, m=2^31-1 ; exact modular multiply via binary method
mod_mult <- function(a, b, m) {
  res <- 0
  a <- a %% m
  b <- floor(b)
  while (b > 0) {
    if ((b %% 2) == 1) res <- (res + a) %% m
    a <- (a * 2) %% m
    b <- floor(b / 2)
  }
  res
}
M_RAN <- 2147483647
A_RAN <- 397204094
seed3 <- 3
seed2 <- 2
ranuni3 <- function() {
  ns <- mod_mult(A_RAN, seed3, M_RAN)
  seed3 <<- ns
  ns / M_RAN
}
ranuni2 <- function() {
  ns <- mod_mult(A_RAN, seed2, M_RAN)
  seed2 <<- ns
  ns / M_RAN
}

# ---------- tumor data (SAS data step) ----------
nobs <- 20
Response <- numeric(nobs)
Duration <- numeric(nobs)
Dropped <- rep(NA_real_, nobs)
Drug <- integer(nobs)
Code <- character(nobs)
codeloc <- numeric(nobs)
baseline <- numeric(nobs)
for (i in 1:nobs) {
  r1 <- ranuni3()
  Response[i] <- 100 * (r1 - 0.7)
  r2 <- ranuni3()
  Duration[i] <- 50 * r2
  r3 <- ranuni3()
  if (r3 > 0.8) Dropped[i] <- Duration[i] - 5 else Dropped[i] <- NA_real_
  r4 <- ranuni3()
  Drug[i] <- floor(6 * r4)
  r5 <- ranuni3()
  Code[i] <- ifelse(r5 > 0.7, "FL", "DLBCL")
  codeloc[i] <- ifelse(Response[i] > 0, 0, Response[i])
  r6 <- ranuni2()
  baseline[i] <- 20 + 180 * r6
}
drug_labels <- c("1.0 mg","2.0 mg","3.0 mg","4.0 mg","3.0 F6 mg","4.0 F6 mg")
DrugLabel <- drug_labels[Drug + 1]
tumor <- data.frame(
  i = 1:nobs,
  Response = Response, Duration = Duration, Dropped = Dropped,
  Drug = Drug, DrugLabel = DrugLabel, Code = Code,
  codeloc = codeloc, baseline = baseline,
  stringsAsFactors = FALSE
)
# sort by descending Response (SAS missing first, none here)
tumor <- tumor[order(-tumor$Response), , drop = FALSE]
rownames(tumor) <- NULL

# write tumor listing/table counterpart (plotted source)
write.csv(tumor, file.path(out, "tumor.csv"), row.names = FALSE, na = "")

# ---------- 3D helpers ----------
build_w <- function(Tilt, Rotate) {
  fac <- pi / 180
  A <- Tilt * fac
  B <- 0 * fac
  C <- Rotate * fac
  m <- matrix(0, 4, 4)
  m[1,1] <- 1; m[2,2] <- 1; m[3,3] <- -1; m[4,4] <- 1
  rx <- matrix(0,4,4)
  rx[1,1]<-1
  rx[2,2]<-cos(A); rx[2,3]<--sin(A)
  rx[3,2]<-sin(A); rx[3,3]<-cos(A)
  rx[4,4]<-1
  ry <- diag(4)
  # B=0 => identity, keep general form
  ry[1,1]<-cos(B); ry[1,3]<-sin(B); ry[2,2]<-1; ry[3,1]<--sin(B); ry[3,3]<-cos(B); ry[4,4]<-1
  rz <- matrix(0,4,4)
  rz[1,1]<-cos(C); rz[1,2]<--sin(C)
  rz[2,1]<-sin(C); rz[2,2]<-cos(C)
  rz[3,3]<-1; rz[4,4]<-1
  u <- rz %*% m
  v <- ry %*% u
  w <- rx %*% v
  w
}
proj_xy <- function(x, y, z, w) {
  # vectorized; NA propagates
  xp <- w[1,1]*x + w[1,2]*y + w[1,3]*z + w[1,4]*1
  yp <- w[2,1]*x + w[2,2]*y + w[2,3]*z + w[2,4]*1
  zp <- w[3,1]*x + w[3,2]*y + w[3,3]*z + w[3,4]*1
  # SAS missing propagation: if any input NA -> NA
  bad <- is.na(x) | is.na(y) | is.na(z)
  xp[bad] <- NA_real_; yp[bad] <- NA_real_; zp[bad] <- NA_real_
  list(x=xp, y=yp, z=zp)
}

make_wall_axes <- function(xFac) {
  dx <- xFac
  df <- data.frame(
    id = c("X1-Axis","X3-Axis","X4-Axis","Y2-Axis","Y3-Axis","Y4-Axis","Z1-Axis","Z2-Axis","Z4-Axis",
           rep("Back",4), rep("Right",4), rep("Bottom",4), rep("Center",4)),
    wgroup = c("D","L","D","D","D","L","D","L","D", rep("D",16)),
    xw = c(-dx,-dx,-dx,-dx, dx, dx,-dx, dx, dx,
           -dx,-dx,-dx,-dx,  -dx, dx, dx,-dx,  -dx, dx, dx,-dx,  -dx, dx, dx,-dx),
    yw = c(-1,-1, 1,-1, -1,-1, -1,-1, 1,
           -1, 1, 1,-1,   1, 1, 1, 1,  -1,-1, 1, 1,  -1,-1, 1, 1),
    zw = c(-1, 1, 1, 1, -1, 1, -1,-1,-1,
           -1,-1, 1, 1,  -1,-1, 1, 1,  -1,-1,-1,-1,   0, 0, 0, 0),
    xw2 = c(dx,dx,dx,-dx,dx,dx,-dx,dx,dx, rep(NA_real_,16)),
    yw2 = c(-1,-1,1,1,1,1,-1,-1,1, rep(NA_real_,16)),
    zw2 = c(-1,1,1,1,-1,1,1,1,1, rep(NA_real_,16)),
    xl = c(0,NA,NA,NA,dx+0.1,NA,-dx,NA,NA, rep(NA_real_,16)),
    yl = c(-1,NA,NA,NA,0,NA,-1.2,NA,NA, rep(NA_real_,16)),
    zl = c(-1.1,NA,NA,NA,-0.1,NA,0,NA,NA, rep(NA_real_,16)),
    label = c(1,NA,NA,NA,2,NA,3,NA,NA, rep(NA_real_,16)),
    stringsAsFactors = FALSE
  )
  df
}

run_waterfall <- function(xFac, Tilt=75, Rotate=-45,
                          Lblx="Subject", Lbly="Duration of Treatment in Days",
                          Lblz="Tumor Lesion Best Change",
                          Title="Tumor Response and Duration") {
  dx <- xFac
  w <- build_w(Tilt, Rotate)
  walls <- make_wall_axes(xFac)
  # project walls
  p1 <- proj_xy(walls$xw, walls$yw, walls$zw, w)
  walls$xw_p <- p1$x; walls$yw_p <- p1$y; walls$zw_p <- p1$z
  p2 <- proj_xy(walls$xw2, walls$yw2, walls$zw2, w)
  walls$xw2_p <- p2$x; walls$yw2_p <- p2$y; walls$zw2_p <- p2$z
  p3 <- proj_xy(walls$xl, walls$yl, walls$zl, w)
  walls$xl_p <- p3$x; walls$yl_p <- p3$y; walls$zl_p <- p3$z
  walls$lbx <- NA_character_; walls$lby <- NA_character_; walls$lbz <- NA_character_
  walls$lbx[walls$label==1 & !is.na(walls$label)] <- Lblx
  walls$lby[walls$label==2 & !is.na(walls$label)] <- Lbly
  walls$lbz[walls$label==3 & !is.na(walls$label)] <- Lblz

  # axis label rotations
  fac2 <- sqrt(xFac)
  xrow <- walls[walls$id=="X1-Axis",][1,]
  dxa <- xrow$xw2_p - xrow$xw_p
  if (!is.na(dxa) && abs(dxa) > 0.001) xaxisRot <- fac2*180*atan((xrow$yw2_p-xrow$yw_p)/dxa)/3.1415 else xaxisRot <- 90
  yrow <- walls[walls$id=="Y3-Axis",][1,]
  dxb <- yrow$xw2_p - yrow$xw_p
  if (!is.na(dxb) && abs(dxb) > 0.001) yaxisRot <- fac2*180*atan((yrow$yw2_p-yrow$yw_p)/dxb)/3.1415 else yaxisRot <- 90

  DurMin <- min(tumor$Duration); DurMax <- max(tumor$Duration)
  ResMin <- min(tumor$Response); ResMax <- max(tumor$Response)
  durfac <- 1.05; resfac <- 1.05
  aResMax <- resfac * max(abs(ResMax), abs(ResMin))
  aDurMax <- durfac * max(abs(DurMax), abs(DurMin))

  # polygons
  pid <- 0
  rows <- list(); k <- 0
  add_row <- function(type, pidv, x, y, z, Drugv, DrugLabv, Codev, Lidv, yTv, zTv) {
    k <<- k + 1
    rows[[k]] <<- data.frame(type=type, pid=pidv, x=x, y=y, z=z,
      Drug=Drugv, DrugLabel=DrugLabv, Code=Codev, Lid=Lidv,
      yTickVal=yTv, zTickVal=zTv, stringsAsFactors=FALSE)
  }
  n <- nrow(tumor)
  xMax <- NA_real_
  for (idx in 1:n) {
    Dur <- tumor$Duration[idx]; Resp <- tumor$Response[idx]
    Drop <- tumor$Dropped[idx]; Dg <- tumor$Drug[idx]; DgL <- tumor$DrugLabel[idx]; Cd <- tumor$Code[idx]
    i <- idx - 1
    # Duration
    pid <- pid + 1; cur <- pid
    xs <- c(dx*(i-0.9), dx*(i-0.9), dx*(i-0.1), dx*(i-0.1))
    ys <- c(0, Dur, Dur, 0)
    for (q in 1:4) add_row("Duration", cur, xs[q], ys[q], 0, Dg, DgL, Cd, NA_real_, NA_real_, NA_real_)
    # Response
    pid <- pid + 1; cur2 <- pid
    zs <- c(0, Resp, Resp, 0)
    for (q in 1:4) add_row("Response", cur2, xs[q], 0, zs[q], Dg, DgL, Cd, NA_real_, NA_real_, NA_real_)
    # Dropped
    isDrop <- !is.na(Drop) && Drop != 0
    if (isDrop) {
      pid <- pid + 1; curd <- pid
      xd <- c(dx*(i-0.8), dx*(i-0.5), dx*(i-0.2), dx*(i-0.5))
      yd <- c(Dur-0.03*DurMax, Dur, Dur-0.03*DurMax, Dur-0.06*DurMax)
      for (q in 1:4) add_row("Dropped", curd, xd[q], yd[q], 0, Dg, DgL, Cd, NA_real_, NA_real_, NA_real_)
    }
    xMax <- dx * i
    zc <- ifelse(Resp < 0, Resp, 0)
    # Code row uses current pid (last)
    code_pid <- pid
    add_row("Code", code_pid, dx*(i-0.5), 0, zc, Dg, DgL, Cd, NA_real_, NA_real_, NA_real_)
  }
  # last extras
  lastDg <- tumor$Drug[n]; lastDgL <- tumor$DrugLabel[n]
  pid <- pid + 1
  add_row("Response", pid, -dx, 0, -aResMax, lastDg, lastDgL, NA_character_, NA_real_, NA_real_, NA_real_)
  add_row("Response", pid, -dx, 0,  aResMax, lastDg, lastDgL, NA_character_, NA_real_, NA_real_, NA_real_)
  pid <- pid + 1
  add_row("Duration", pid, -dx, 0,       aResMax, lastDg, lastDgL, NA_character_, NA_real_, NA_real_, NA_real_)
  add_row("Duration", pid, -dx, aDurMax, aResMax, lastDg, lastDgL, NA_character_, NA_real_, NA_real_, NA_real_)
  # Y-Axis grid
  y_seq <- seq(0, aDurMax, by=10)
  # SAS: do y=0 to aDurMax by 10 -> includes values <= aDurMax
  y_seq <- y_seq[y_seq <= aDurMax + 1e-9]
  Lid <- 1
  for (yv in y_seq) {
    add_row("Y-Axis", NA_real_, -dx,  yv, 0, lastDg, lastDgL, NA_character_, Lid, NA_real_, NA_real_)
    add_row("Y-Axis", NA_real_, xMax, yv, 0, lastDg, lastDgL, NA_character_, Lid, NA_real_, NA_real_)
    Lid <- Lid + 1
  }
  for (yv in y_seq) {
    add_row("Y-Values", NA_real_, xMax, yv, 0, lastDg, lastDgL, NA_character_, NA_real_, yv, NA_real_)
  }
  # Z-Values
  zneg <- numeric(0)
  if (-aResMax <= -10 + 1e-9) zneg <- seq(-10, -aResMax, by=-10)
  zpos <- seq(0, aResMax, by=10)
  zpos <- zpos[zpos <= aResMax + 1e-9]
  for (zv in zneg) add_row("Z-Values", NA_real_, -dx, 0, zv, lastDg, lastDgL, NA_character_, NA_real_, NA_real_, zv)
  for (zv in zpos) add_row("Z-Values", NA_real_, -dx, 0, zv, lastDg, lastDgL, NA_character_, NA_real_, NA_real_, zv)

  polys <- do.call(rbind, rows)
  xmin <- min(polys$x, na.rm=TRUE); xmax <- max(polys$x, na.rm=TRUE)
  ymin <- min(polys$y, na.rm=TRUE); ymax <- max(polys$y, na.rm=TRUE)
  zmin <- min(polys$z, na.rm=TRUE); zmax <- max(polys$z, na.rm=TRUE)
  xrange <- xmax - xmin; yrange <- ymax - ymin; zrange <- zmax - zmin
  polys$xn <- 2*xFac*(polys$x - xmin)/xrange - xFac
  polys$yn <- 2*(polys$y - ymin)/yrange - 1
  polys$zn <- 2*(polys$z - zmin)/zrange - 1

  pp <- proj_xy(polys$xn, polys$yn, polys$zn, w)
  polys$xp <- pp$x; polys$yp <- pp$y; polys$zp <- pp$z

  # split projected data by type into plot columns
  polys$xd <- NA_real_; polys$yd <- NA_real_
  polys$xr <- NA_real_; polys$yr <- NA_real_
  polys$xc <- NA_real_; polys$yc <- NA_real_
  polys$xcode <- NA_real_; polys$ycode <- NA_real_
  polys$xAy <- NA_real_; polys$yAy <- NA_real_
  polys$xYv <- NA_real_; polys$yYv <- NA_real_
  polys$xZv <- NA_real_; polys$yZv <- NA_real_
  polys$xd[polys$type=="Duration"] <- polys$xp[polys$type=="Duration"]
  polys$yd[polys$type=="Duration"] <- polys$yp[polys$type=="Duration"]
  polys$xr[polys$type=="Response"] <- polys$xp[polys$type=="Response"]
  polys$yr[polys$type=="Response"] <- polys$yp[polys$type=="Response"]
  polys$xc[polys$type=="Dropped"] <- polys$xp[polys$type=="Dropped"]
  polys$yc[polys$type=="Dropped"] <- polys$yp[polys$type=="Dropped"]
  polys$xcode[polys$type=="Code"] <- polys$xp[polys$type=="Code"]
  polys$ycode[polys$type=="Code"] <- polys$yp[polys$type=="Code"]
  polys$xAy[polys$type=="Y-Axis"] <- polys$xp[polys$type=="Y-Axis"]
  polys$yAy[polys$type=="Y-Axis"] <- polys$yp[polys$type=="Y-Axis"]
  polys$xYv[polys$type=="Y-Values"] <- polys$xp[polys$type=="Y-Values"]
  polys$yYv[polys$type=="Y-Values"] <- polys$yp[polys$type=="Y-Values"]
  polys$xZv[polys$type=="Z-Values"] <- polys$xp[polys$type=="Z-Values"]
  polys$yZv[polys$type=="Z-Values"] <- polys$yp[polys$type=="Z-Values"]

  # combined one-to-one merge (pad shorter)
  nw <- nrow(walls); np <- nrow(polys)
  N <- max(nw, np)
  pad_df <- function(df, N) {
    if (nrow(df) >= N) return(df[1:N,,drop=FALSE])
    add <- N - nrow(df)
    na_rows <- df[rep(NA_integer_, add),,drop=FALSE]
    # for character keep NA
    rbind(df, na_rows)
  }
  # Use cbind of walls padded and polys padded (SAS merge without BY)
  walls_p <- pad_df(walls, N)
  polys_p <- pad_df(polys, N)
  combined <- cbind(walls_p, polys_p)

  list(walls=walls, polys=polys, combined=combined,
       xaxisRot=xaxisRot, yaxisRot=yaxisRot,
       DurMin=DurMin, DurMax=DurMax, ResMin=ResMin, ResMax=ResMax,
       aDurMax=aDurMax, aResMax=aResMax, w=w,
       xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, zmin=zmin, zmax=zmax)
}

wall_fill <- c(Center="#D0D0D0", Back="#E0E0E0", Right="#F0F0F0", Bottom="#E7E7E7")
resp_fill <- c("1.0 mg"="#FFFFFF","2.0 mg"="#FFFFC0","3.0 mg"="#F0D0A0",
               "4.0 mg"="orange","3.0 F6 mg"="#FF7F00","4.0 F6 mg"="red")

plot_waterfall <- function(res, xFac, Tilt, Rotate, Title, outfile) {
  walls <- res$walls; polys <- res$polys
  xaxisRot <- res$xaxisRot; yaxisRot <- res$yaxisRot
  wall_poly <- walls[walls$id %in% c("Back","Right","Bottom","Center"),]
  axis_seg <- walls[grepl("Axis", walls$id),]
  dur_df <- polys[polys$type=="Duration" & !is.na(polys$xd),]
  resp_df <- polys[polys$type=="Response" & !is.na(polys$xr),]
  # exclude dummy aspect polys at x=-dx? keep them (they set aspect) but they are at edge; keep all
  drop_df <- polys[polys$type=="Dropped" & !is.na(polys$xc),]
  ygrid <- polys[polys$type=="Y-Axis" & !is.na(polys$xAy),]
  ytick <- polys[polys$type=="Y-Values" & !is.na(polys$xYv),]
  ztick <- polys[polys$type=="Z-Values" & !is.na(polys$xZv),]
  codedf <- polys[polys$type=="Code" & !is.na(polys$xcode),]
  lbl_df <- walls[!is.na(walls$label),]

  p <- ggplot() +
    geom_polygon(data=wall_poly, aes(x=xw_p, y=yw_p, group=id, fill=id),
                 color="grey70", alpha=0.9, show.legend=FALSE) +
    scale_fill_manual(values=wall_fill) +
    geom_segment(data=axis_seg, aes(x=xw_p, y=yw_p, xend=xw2_p, yend=yw2_p, linetype=wgroup),
                 color="black", linewidth=0.4, show.legend=FALSE) +
    scale_linetype_manual(values=c(D="solid", L="dashed")) +
    { if (nrow(ygrid)>0) geom_line(data=ygrid, aes(x=xAy, y=yAy, group=Lid), color="#B0B0B0", linewidth=0.3) } +
    { if (nrow(dur_df)>0) geom_polygon(data=dur_df, aes(x=xd, y=yd, group=pid),
                 fill="#5B9BD5", color="black", linewidth=0.25, alpha=0.9) } +
    { if (nrow(resp_df)>0) geom_polygon(data=resp_df, aes(x=xr, y=yr, group=pid, fill=DrugLabel),
                 color="black", linewidth=0.25, alpha=0.95) } +
    scale_fill_manual(values=resp_fill, name="CC-122") +
    { if (nrow(drop_df)>0) geom_polygon(data=drop_df, aes(x=xc, y=yc, group=pid),
                 fill="gold", color="black", linewidth=0.25) } +
    { if (nrow(ytick)>0) geom_text(data=ytick, aes(x=xYv, y=yYv, label=yTickVal), size=2, hjust=1, vjust=1) } +
    { if (nrow(ztick)>0) geom_text(data=ztick, aes(x=xZv, y=yZv, label=zTickVal), size=2, hjust=1) } +
    { if (nrow(ztick)>0) geom_point(data=ztick, aes(x=xZv, y=yZv), size=0.6) } +
    { if (nrow(codedf)>0) geom_text(data=codedf, aes(x=xcode, y=ycode, label=Code), size=2, angle=90, hjust=0) } +
    coord_equal() +
    labs(title=Title,
         caption=paste0("X-Rotation=", Tilt, "  Y-Rotation=0  Z-Rotation=", Rotate)) +
    theme_void() +
    theme(plot.title=element_text(size=12, hjust=0.5),
          plot.caption=element_text(size=5, hjust=0, face="italic"),
          legend.position=c(0.85,0.15), legend.background=element_rect(fill="white", color="black"),
          legend.title=element_text(size=5), legend.text=element_text(size=5),
          legend.key.size=unit(4,"pt"))
  # axis labels (three)
  # extract label positions
  for (k in seq_len(nrow(lbl_df))) {
    lab <- lbl_df$label[k]
    txt <- if (lab==1) "Subject" else if (lab==2) "Duration of Treatment in Days" else "Tumor Lesion Best Change"
    ang <- if (lab==1) xaxisRot else if (lab==2) yaxisRot else 90
    p <- p + annotate("text", x=lbl_df$xl_p[k], y=lbl_df$yl_p[k], label=txt, size=2, angle=ang)
  }
  ggsave(outfile, p, width=4, height=4, dpi=300)
  invisible(p)
}

configs <- list(
  list(xFac=1.0,  suffix="100"),
  list(xFac=1.25, suffix="125"),
  list(xFac=1.5,  suffix="150")
)
for (cfg in configs) {
  xf <- cfg$xFac; suf <- cfg$suffix
  res <- run_waterfall(xFac=xf, Tilt=75, Rotate=-45)
  pngfile <- file.path(out, paste0("waterfall_xf_", suf, ".png"))
  plot_waterfall(res, xf, 75, -45, "Tumor Response and Duration", pngfile)
  # CSV of plotted data
  csvfile <- file.path(out, paste0("waterfall_xf_", suf, ".csv"))
  pd <- res$polys
  pd_out <- data.frame(
    type=pd$type, pid=pd$pid, Drug=pd$Drug, DrugLabel=pd$DrugLabel, Code=pd$Code,
    x=pd$x, y=pd$y, z=pd$z, xn=pd$xn, yn=pd$yn, zn=pd$zn,
    xp=pd$xp, yp=pd$yp, zp=pd$zp, Lid=pd$Lid,
    yTickVal=pd$yTickVal, zTickVal=pd$zTickVal,
    stringsAsFactors=FALSE
  )
  write.csv(pd_out, csvfile, row.names=FALSE, na="")
  # walls CSV supplement appended? keep separate walls in same CSV via combined? also write combined walls
  # write a txt rendering with same numbers
  txtfile <- file.path(out, paste0("waterfall_xf_", suf, ".txt"))
  sink(txtfile)
  cat("Tumor Response and Duration\n")
  cat(paste0("X-Rotation=75  Y-Rotation=0  Z-Rotation=-45  xFac=", xf, "\n\n"))
  cat(sprintf("DurMin=%.4f DurMax=%.4f ResMin=%.4f ResMax=%.4f aDurMax=%.4f aResMax=%.4f\n",
      res$DurMin, res$DurMax, res$ResMin, res$ResMax, res$aDurMax, res$aResMax))
  cat(sprintf("xaxisRot=%.4f yaxisRot=%.4f\n\n", res$xaxisRot, res$yaxisRot))
  cat("Plotted projected data (first 20 rows):\n")
  print(utils::head(as.data.frame(pd_out), 20))
  cat("\nTumor source (sorted by descending Response):\n")
  print(tumor)
  sink()
}
```