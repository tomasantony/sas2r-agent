out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)
suppressPackageStartupMessages(library(ggplot2))

## ---------- SAS RANUNI replication ----------
m_sas <- 2147483647
a_sas <- 397204094
mod_mult <- function(aa, ss, mm) {
  a_hi <- floor(aa / 65536); a_lo <- aa - a_hi * 65536
  s_hi <- floor(ss / 65536); s_lo <- ss - s_hi * 65536
  t1 <- (a_hi * s_hi * 2) %% mm
  t2_mid <- a_hi * s_lo + a_lo * s_hi
  h <- floor(t2_mid / 65536); l <- t2_mid - h * 65536
  t2 <- (h * 2 + l * 65536) %% mm
  t3 <- (a_lo * s_lo) %% mm
  (t1 + t2 + t3) %% mm
}
seed3 <- 3
seed2 <- 2
next_u3 <- function() {
  s_new <- mod_mult(a_sas, seed3, m_sas)
  seed3 <<- s_new
  s_new / m_sas
}
next_u2 <- function() {
  s_new <- mod_mult(a_sas, seed2, m_sas)
  seed2 <<- s_new
  s_new / m_sas
}

n <- 20L
Response <- numeric(n); Duration <- numeric(n)
Dropped <- rep(NA_real_, n); Drug <- integer(n)
Code <- character(n); codeloc <- numeric(n); baseline <- numeric(n)
for (i in 1:n) {
  u1 <- next_u3(); Response[i] <- 100 * (u1 - 0.7)
  u2 <- next_u3(); Duration[i] <- 50 * u2
  u3 <- next_u3(); if (u3 > 0.8) Dropped[i] <- Duration[i] - 5 else Dropped[i] <- NA_real_
  u4 <- next_u3(); Drug[i] <- floor(6 * u4)
  u5 <- next_u3(); Code[i] <- ifelse(u5 > 0.7, "FL", "DLBCL")
  codeloc[i] <- ifelse(Response[i] > 0, 0, Response[i])
  u6 <- next_u2(); baseline[i] <- 20 + 180 * u6
}
tumor <- data.frame(i=1:n, Response=Response, Duration=Duration, Dropped=Dropped,
  Drug=Drug, Code=Code, codeloc=codeloc, baseline=baseline, stringsAsFactors=FALSE)
## sort by descending Response (SAS missing first; no missings here)
tumor <- tumor[order(-tumor$Response), , drop=FALSE]
rownames(tumor) <- NULL

drug_labels <- c("1.0 mg","2.0 mg","3.0 mg","4.0 mg","3.0 F6 mg","4.0 F6 mg")
tumor$DrugLabel <- drug_labels[tumor$Drug + 1L]
Lblx <- "Subject"; Lbly <- "Duration of Treatment in Days"; Lblz <- "Tumor Lesion Best Change"
Title_main <- "Tumor Response and Duration"

get_w <- function(Tilt, Rotate) {
  fac <- pi/180; A <- Tilt*fac; B <- 0; C <- Rotate*fac
  M <- matrix(0,4,4); M[1,1]<-1; M[2,2]<-1; M[3,3]<--1; M[4,4]<-1
  Rx <- diag(4); Rx[2,2]<-cos(A); Rx[2,3]<--sin(A); Rx[3,2]<-sin(A); Rx[3,3]<-cos(A)
  Ry <- diag(4)
  Rz <- diag(4); Rz[1,1]<-cos(C); Rz[1,2]<--sin(C); Rz[2,1]<-sin(C); Rz[2,2]<-cos(C)
  u <- Rz %*% M; v <- Ry %*% u; w <- Rx %*% v
  w
}
proj_pt <- function(x,y,z,w) {
  d <- matrix(c(x,y,z,1),4,1)
  p <- w %*% d
  c(p[1,1], p[2,1], p[3,1])
}

wall_fills <- c(Center="#D0D0D0", Back="#E0E0E0", Right="#F0F0F0", Bottom="#E7E7E7")
drug_fills <- c("1.0 mg"="#FFFFFF","2.0 mg"="#FFFFC0","3.0 mg"="#F0D0A0",
  "4.0 mg"="orange","3.0 F6 mg"="#FF7F00","4.0 F6 mg"="red","Drop"="gold")

run_waterfall <- function(xFac, suffix) {
  Tilt <- 75; Rotate <- -45
  Acl <- Tilt; Bcl <- 0; Ccl <- Rotate
  dx <- xFac
  ## Wall_Axes
  wa <- data.frame(
    id=c('X1-Axis','X3-Axis','X4-Axis','Y2-Axis','Y3-Axis','Y4-Axis','Z1-Axis','Z2-Axis','Z4-Axis',
      rep('Back',4),rep('Right',4),rep('Bottom',4),rep('Center',4)),
    wgroup=c('D','L','D','D','D','L','D','L','D',rep('D',16)),
    xw=c(-dx,-dx,-dx,-dx,dx,dx,-dx,dx,dx,
      -dx,-dx,-dx,-dx, -dx,dx,dx,-dx, -dx,dx,dx,-dx, -dx,dx,dx,-dx),
    yw=c(-1,-1,1,-1,-1,-1,-1,-1,1,
      -1,1,1,-1, 1,1,1,1, -1,-1,1,1, -1,-1,1,1),
    zw=c(-1,1,1,1,-1,1,-1,-1,-1,
      -1,-1,1,1, -1,-1,1,1, -1,-1,-1,-1, 0,0,0,0),
    xw2=c(dx,dx,dx,-dx,dx,dx,-dx,dx,dx,rep(NA_real_,16)),
    yw2=c(-1,-1,1,1,1,1,-1,-1,1,rep(NA_real_,16)),
    zw2=c(-1,1,1,1,-1,1,1,1,1,rep(NA_real_,16)),
    xl=c(0,NA,NA,NA,dx+0.1,NA,-dx,NA,NA,rep(NA_real_,16)),
    yl=c(-1,NA,NA,NA,0,NA,-1.2,NA,NA,rep(NA_real_,16)),
    zl=c(-1.1,NA,NA,NA,-0.1,NA,0,NA,NA,rep(NA_real_,16)),
    label=c(1,NA,NA,NA,2,NA,3,NA,NA,rep(NA_real_,16)),
    stringsAsFactors=FALSE)
  wmat <- get_w(Tilt, Rotate)
  nwa <- nrow(wa)
  wa$xw_p <- NA_real_; wa$yw_p <- NA_real_; wa$zw_p <- NA_real_
  wa$xw2_p <- NA_real_; wa$yw2_p <- NA_real_; wa$zw2_p <- NA_real_
  wa$xl_p <- NA_real_; wa$yl_p <- NA_real_; wa$zl_p <- NA_real_
  wa$lab <- NA_character_
  for (k in 1:nwa) {
    p <- proj_pt(wa$xw[k], wa$yw[k], wa$zw[k], wmat)
    wa$xw_p[k] <- p[1]; wa$yw_p[k] <- p[2]; wa$zw_p[k] <- p[3]
    if (!is.na(wa$xw2[k])) {
      p2 <- proj_pt(wa$xw2[k], wa$yw2[k], wa$zw2[k], wmat)
      wa$xw2_p[k] <- p2[1]; wa$yw2_p[k] <- p2[2]; wa$zw2_p[k] <- p2[3]
    }
    if (!is.na(wa$xl[k])) {
      p3 <- proj_pt(wa$xl[k], wa$yl[k], wa$zl[k], wmat)
      wa$xl_p[k] <- p3[1]; wa$yl_p[k] <- p3[2]; wa$zl_p[k] <- p3[3]
    }
    if (!is.na(wa$label[k])) {
      if (wa$label[k]==1) wa$lab[k] <- Lblx
      if (wa$label[k]==2) wa$lab[k] <- Lbly
      if (wa$label[k]==3) wa$lab[k] <- Lblz
    }
  }
  ## label rotations
  facs <- sqrt(xFac)
  xaxisRot <- 0; yaxisRot <- 0
  r1 <- wa[wa$id=='X1-Axis',,drop=FALSE]
  dx1 <- r1$xw2_p - r1$xw_p
  if (abs(dx1) > 0.001) xaxisRot <- facs*180*atan((r1$yw2_p-r1$yw_p)/dx1)/pi else xaxisRot <- 90
  r2 <- wa[wa$id=='Y3-Axis',,drop=FALSE]
  dx2 <- r2$xw2_p - r2$xw_p
  if (abs(dx2) > 0.001) yaxisRot <- facs*180*atan((r2$yw2_p-r2$yw_p)/dx2)/pi else yaxisRot <- 90
  ## ranges
  DurMin <- min(tumor$Duration); DurMax <- max(tumor$Duration)
  ResMin <- min(tumor$Response); ResMax <- max(tumor$Response)
  aResMax <- 1.05*max(abs(ResMax), abs(ResMin))
  aDurMax <- 1.05*max(abs(DurMax), abs(DurMin))
  ## polygons
  rows <- list(); ri <- 1L; pid <- 0L
  lastDrug <- tumor$Drug[nrow(tumor)]
  for (k in 1:nrow(tumor)) {
    Dur <- tumor$Duration[k]; Resp <- tumor$Response[k]
    Dr <- tumor$Drug[k]; Cd <- tumor$Code[k]
    i0 <- k - 1L
    xs <- c(dx*(i0-0.5-0.4), dx*(i0-0.5-0.4), dx*(i0-0.5+0.4), dx*(i0-0.5+0.4))
    pid <- pid + 1L
    for (j in 1:4) {
      yv <- c(0, Dur, Dur, 0)[j]
      rows[[ri]] <- data.frame(type='Duration', pid=pid, x=xs[j], y=yv, z=0,
        Drug=Dr, Code=Cd, Lid=NA_real_, yTickVal=NA_real_, zTickVal=NA_real_, stringsAsFactors=FALSE); ri<-ri+1L
    }
    pid <- pid + 1L
    for (j in 1:4) {
      zv <- c(0, Resp, Resp, 0)[j]
      rows[[ri]] <- data.frame(type='Response', pid=pid, x=xs[j], y=0, z=zv,
        Drug=Dr, Code=Cd, Lid=NA_real_, yTickVal=NA_real_, zTickVal=NA_real_, stringsAsFactors=FALSE); ri<-ri+1L
    }
    if (!is.na(tumor$Dropped[k])) {
      pid <- pid + 1L
      dxs <- c(dx*(i0-0.5-0.3), dx*(i0-0.5), dx*(i0-0.5+0.3), dx*(i0-0.5))
      dys <- c(Dur-0.03*DurMax, Dur, Dur-0.03*DurMax, Dur-0.06*DurMax)
      for (j in 1:4) {
        rows[[ri]] <- data.frame(type='Dropped', pid=pid, x=dxs[j], y=dys[j], z=0,
          Drug=Dr, Code=Cd, Lid=NA_real_, yTickVal=NA_real_, zTickVal=NA_real_, stringsAsFactors=FALSE); ri<-ri+1L
      }
    }
    zc <- ifelse(Resp < 0, Resp, 0)
    rows[[ri]] <- data.frame(type='Code', pid=pid, x=dx*(i0-0.5), y=0, z=zc,
      Drug=Dr, Code=Cd, Lid=NA_real_, yTickVal=NA_real_, zTickVal=NA_real_, stringsAsFactors=FALSE); ri<-ri+1L
  }
  n_real_pid <- pid
  xMax <- dx*(nrow(tumor)-1L)
  ## aspect polygons
  pid <- pid + 1L
  rows[[ri]] <- data.frame(type='Response', pid=pid, x=-dx, y=0, z=-aResMax, Drug=lastDrug, Code=NA_character_, Lid=NA_real_, yTickVal=NA_real_, zTickVal=NA_real_, stringsAsFactors=FALSE); ri<-ri+1L
  rows[[ri]] <- data.frame(type='Response', pid=pid, x=-dx, y=0, z=aResMax, Drug=lastDrug, Code=NA_character_, Lid=NA_real_, yTickVal=NA_real_, zTickVal=NA_real_, stringsAsFactors=FALSE); ri<-ri+1L
  pid <- pid + 1L
  rows[[ri]] <- data.frame(type='Duration', pid=pid, x=-dx, y=0, z=aResMax, Drug=lastDrug, Code=NA_character_, Lid=NA_real_, yTickVal=NA_real_, zTickVal=NA_real_, stringsAsFactors=FALSE); ri<-ri+1L
  rows[[ri]] <- data.frame(type='Duration', pid=pid, x=-dx, y=aDurMax, z=aResMax, Drug=lastDrug, Code=NA_character_, Lid=NA_real_, yTickVal=NA_real_, zTickVal=NA_real_, stringsAsFactors=FALSE); ri<-ri+1L
  y_seq <- seq(0, floor(aDurMax/10)*10, by=10)
  if (length(y_seq)==0) y_seq <- 0
  Lid <- 1
  for (yv in y_seq) {
    rows[[ri]] <- data.frame(type='Y-Axis', pid=NA_real_, x=-dx, y=yv, z=0, Drug=NA_integer_, Code=NA_character_, Lid=Lid, yTickVal=NA_real_, zTickVal=NA_real_, stringsAsFactors=FALSE); ri<-ri+1L
    rows[[ri]] <- data.frame(type='Y-Axis', pid=NA_real_, x=xMax, y=yv, z=0, Drug=NA_integer_, Code=NA_character_, Lid=Lid, yTickVal=NA_real_, zTickVal=NA_real_, stringsAsFactors=FALSE); ri<-ri+1L
    Lid <- Lid + 1
  }
  for (yv in y_seq) {
    rows[[ri]] <- data.frame(type='Y-Values', pid=NA_real_, x=xMax, y=yv, z=0, Drug=NA_integer_, Code=NA_character_, Lid=NA_real_, yTickVal=yv, zTickVal=NA_real_, stringsAsFactors=FALSE); ri<-ri+1L
  }
  nz1 <- floor(aResMax/10)
  z_seq_neg <- if (nz1>=1) -10*seq_len(nz1) else numeric(0)
  z_seq_pos <- 10*seq(0, nz1, by=1)
  for (zv in z_seq_neg) {
    rows[[ri]] <- data.frame(type='Z-Values', pid=NA_real_, x=-dx, y=0, z=zv, Drug=NA_integer_, Code=NA_character_, Lid=NA_real_, yTickVal=NA_real_, zTickVal=zv, stringsAsFactors=FALSE); ri<-ri+1L
  }
  for (zv in z_seq_pos) {
    rows[[ri]] <- data.frame(type='Z-Values', pid=NA_real_, x=-dx, y=0, z=zv, Drug=NA_integer_, Code=NA_character_, Lid=NA_real_, yTickVal=NA_real_, zTickVal=zv, stringsAsFactors=FALSE); ri<-ri+1L
  }
  polygons <- do.call(rbind, rows)
  xmin <- min(polygons$x); xmax <- max(polygons$x)
  ymin <- min(polygons$y); ymax <- max(polygons$y)
  zmin <- min(polygons$z); zmax <- max(polygons$z)
  xrange <- xmax-xmin; yrange <- ymax-ymin; zrange <- zmax-zmin
  polygons$xn <- 2*xFac*(polygons$x-xmin)/xrange - xFac
  polygons$yn <- 2*(polygons$y-ymin)/yrange - 1
  polygons$zn <- 2*(polygons$z-zmin)/zrange - 1
  polygons$px <- NA_real_; polygons$py <- NA_real_; polygons$pz <- NA_real_
  for (k in 1:nrow(polygons)) {
    p <- proj_pt(polygons$xn[k], polygons$yn[k], polygons$zn[k], wmat)
    polygons$px[k] <- p[1]; polygons$py[k] <- p[2]; polygons$pz[k] <- p[3]
  }
  polygons$DrugLabel <- drug_labels[polygons$Drug+1L]
  polygons$DrugLabel[is.na(polygons$Drug)] <- NA_character_
  ## subsets
  walls_poly <- wa[wa$id %in% c('Back','Right','Bottom','Center'),,drop=FALSE]
  axes_df <- wa[grepl('Axis', wa$id),,drop=FALSE]
  lab_df <- wa[!is.na(wa$lab),,drop=FALSE]
  grid_df <- polygons[polygons$type=='Y-Axis',,drop=FALSE]
  dur_df <- polygons[polygons$type=='Duration' & !is.na(polygons$pid) & polygons$pid<=n_real_pid,,drop=FALSE]
  resp_df <- polygons[polygons$type=='Response' & !is.na(polygons$pid) & polygons$pid<=n_real_pid,,drop=FALSE]
  drop_df <- polygons[polygons$type=='Dropped',,drop=FALSE]
  yval_df <- polygons[polygons$type=='Y-Values',,drop=FALSE]
  zval_df <- polygons[polygons$type=='Z-Values',,drop=FALSE]
  code_df <- polygons[polygons$type=='Code',,drop=FALSE]
  resp_df$fillKey <- resp_df$DrugLabel
  drop_df$fillKey <- "Drop"
  ## plot
  p <- ggplot()
  for (wid in c('Back','Right','Bottom','Center')) {
    sub <- walls_poly[walls_poly$id==wid,,drop=FALSE]
    if (nrow(sub)>0) p <- p + geom_polygon(data=sub, aes(x=xw_p, y=yw_p, group=id), fill=wall_fills[[wid]], colour="grey70", linewidth=0.3)
  }
  if (nrow(grid_df)>0) p <- p + geom_path(data=grid_df, aes(x=px, y=py, group=Lid), colour="#B0B0B0", linewidth=0.3)
  if (nrow(dur_df)>0) p <- p + geom_polygon(data=dur_df, aes(x=px, y=py, group=pid), fill="#7FB3D5", colour="black", linewidth=0.3)
  comb_fill <- rbind(
    if (nrow(resp_df)>0) data.frame(px=resp_df$px, py=resp_df$py, pid=resp_df$pid, fillKey=resp_df$fillKey, stringsAsFactors=FALSE) else NULL,
    if (nrow(drop_df)>0) data.frame(px=drop_df$px, py=drop_df$py, pid=drop_df$pid, fillKey=drop_df$fillKey, stringsAsFactors=FALSE) else NULL)
  if (nrow(resp_df)>0) p <- p + geom_polygon(data=resp_df, aes(x=px, y=py, group=pid, fill=fillKey), colour="black", linewidth=0.3)
  if (nrow(drop_df)>0) p <- p + geom_polygon(data=drop_df, aes(x=px, y=py, group=pid, fill=fillKey), colour="black", linewidth=0.3)
  p <- p + scale_fill_manual(values=drug_fills, name="CC-122")
  if (nrow(axes_df)>0) p <- p + geom_segment(data=axes_df, aes(x=xw_p, y=yw_p, xend=xw2_p, yend=yw2_p, linetype=wgroup), colour="black", linewidth=0.4) +
    scale_linetype_manual(values=c(D="solid", L="dashed"), guide="none")
  if (nrow(yval_df)>0) p <- p + geom_text(data=yval_df, aes(x=px, y=py, label=yTickVal), size=2.5, hjust=1.1, vjust=-0.2)
  if (nrow(zval_df)>0) {
    p <- p + geom_text(data=zval_df, aes(x=px, y=py, label=zTickVal), size=2.5, hjust=1.1)
    p <- p + geom_point(data=zval_df, aes(x=px, y=py), size=0.8)
  }
  if (nrow(code_df)>0) p <- p + geom_text(data=code_df, aes(x=px, y=py, label=Code), size=2.5, angle=90, hjust=0, vjust=0.5)
  if (nrow(lab_df)>0) {
    lab_df$ang <- c(xaxisRot, yaxisRot, 90)[match(lab_df$label, c(1,2,3))]
    p <- p + geom_text(data=lab_df, aes(x=xl_p, y=yl_p, label=lab), size=3, angle=lab_df$ang)
  }
  p <- p + labs(title=Title_main, caption=paste0("X-Rotation=", Acl, "  Y-Rotation=", Bcl, "  Z-Rotation=", Ccl, "   xFac=", xFac)) +
    coord_equal() + theme_void() +
    theme(plot.title=element_text(size=12, hjust=0.5),
      plot.caption=element_text(size=6, hjust=0, face="italic"),
      legend.position=c(0.85,0.2), legend.title=element_text(size=6), legend.text=element_text(size=6),
      legend.key.size=unit(4,"pt"), legend.background=element_rect(fill="white", colour="grey60"))
  pngfile <- file.path(out, paste0("waterfall_xf_", suffix, ".png"))
  ggsave(pngfile, p, width=4, height=4, dpi=300, units="in")
  csvfile <- file.path(out, paste0("waterfall_xf_", suffix, ".csv"))
  outdf <- polygons[, c("type","pid","x","y","z","xn","yn","zn","px","py","pz","Drug","DrugLabel","Code","Lid","yTickVal","zTickVal")]
  write.csv(outdf, csvfile, row.names=FALSE, na="")
  message("wrote ", pngfile, " and ", csvfile)
}

run_waterfall(1.0, "100")
run_waterfall(1.25, "125")
run_waterfall(1.5, "150")
