```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

## ---------- SAS RANUNI replication ----------
sas_modmult <- function(a, b, m) {
  res <- 0
  bb <- b
  aa <- a
  while (aa > 0) {
    if ((aa %% 2) == 1) res <- (res + bb) %% m
    bb <- (bb * 2) %% m
    aa <- floor(aa / 2)
  }
  res
}
sas_next <- function(seed) {
  sas_modmult(397204094, seed, 2147483647)
}

seed3 <- 3
seed2 <- 2
n <- 20
Response <- numeric(n); Duration <- numeric(n); Dropped <- numeric(n)
Drug <- integer(n); Code <- character(n); codeloc <- numeric(n); baseline <- numeric(n)
for (i in 1:n) {
  seed3 <- sas_next(seed3); u1 <- seed3 / 2147483647
  Response[i] <- 100 * (u1 - 0.7)
  seed3 <- sas_next(seed3); u2 <- seed3 / 2147483647
  Duration[i] <- 50 * u2
  seed3 <- sas_next(seed3); u3 <- seed3 / 2147483647
  Dropped[i] <- if (u3 > 0.8) Duration[i] - 5 else NA_real_
  seed3 <- sas_next(seed3); u4 <- seed3 / 2147483647
  Drug[i] <- floor(6 * u4)
  seed3 <- sas_next(seed3); u5 <- seed3 / 2147483647
  Code[i] <- if (u5 > 0.7) "FL" else "DLBCL"
  codeloc[i] <- ifelse(Response[i] > 0, 0, Response[i])
  seed2 <- sas_next(seed2); u6 <- seed2 / 2147483647
  baseline[i] <- 20 + 180 * u6
}
drug_lbl <- c("1.0 mg","2.0 mg","3.0 mg","4.0 mg","3.0 F6 mg","4.0 F6 mg")
tumor <- data.frame(
  Response=Response, Duration=Duration, Dropped=Dropped,
  Drug=Drug, DrugLabel=drug_lbl[Drug+1],
  Code=Code, codeloc=codeloc, baseline=baseline,
  stringsAsFactors=FALSE
)
## sort by descending Response (SAS missing first, but no missings here)
tumor <- tumor[order(-tumor$Response), , drop=FALSE]
rownames(tumor) <- NULL

Lblx <- "Subject"
Lbly <- "Duration of Treatment in Days"
Lblz <- "Tumor Lesion Best Change"
Title <- "Tumor Response and Duration"

build_W <- function(Adeg, Bdeg, Cdeg) {
  fac <- pi/180
  A <- Adeg*fac; B <- Bdeg*fac; C <- Cdeg*fac
  m <- matrix(0,4,4)
  m[1,1] <- 1; m[2,2] <- 1; m[3,3] <- -2/(1-(-1)); m[3,4] <- -(1+(-1))/(1-(-1)); m[4,4] <- 1
  rx <- matrix(0,4,4)
  rx[1,1]<-1
  rx[2,2]<-cos(A); rx[2,3]<--sin(A)
  rx[3,2]<-sin(A); rx[3,3]<-cos(A)
  rx[4,4]<-1
  ry <- matrix(0,4,4)
  ry[1,1]<-cos(B); ry[1,3]<-sin(B)
  ry[2,2]<-1
  ry[3,1]<--sin(B); ry[3,3]<-cos(B)
  ry[4,4]<-1
  rz <- matrix(0,4,4)
  rz[1,1]<-cos(C); rz[1,2]<--sin(C)
  rz[2,1]<-sin(C); rz[2,2]<-cos(C)
  rz[3,3]<-1; rz[4,4]<-1
  W <- rx %*% ry %*% rz %*% m
  W
}
proj_pt <- function(x,y,z,W) {
  if (is.na(x) || is.na(y) || is.na(z)) return(c(NA_real_,NA_real_,NA_real_))
  d <- c(x,y,z,1)
  p <- as.vector(W %*% d)
  c(p[1],p[2],p[3])
}

make_wall_axes <- function(xFac) {
  dx <- xFac
  df <- data.frame(
    id=c("X1-Axis","X3-Axis","X4-Axis","Y2-Axis","Y3-Axis","Y4-Axis","Z1-Axis","Z2-Axis","Z4-Axis",
         rep("Back",4),rep("Right",4),rep("Bottom",4),rep("Center",4)),
    wgroup=c("D","L","D","D","D","L","D","L","D",rep("D",16)),
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
    stringsAsFactors=FALSE
  )
  df
}

process_one <- function(xFac, Tilt=75, Rotate=-45) {
  if (xFac > 4) xFac <- 4
  if (xFac < 0.5) xFac <- 0.5
  A <- Tilt; if (A > 80) A <- 80; if (A < 60) A <- 60
  B <- 0
  C <- Rotate; if (C > -15) C <- -15; if (C < -75) C <- -75
  W <- build_W(A,B,C)
  wall <- make_wall_axes(xFac)
  ## project walls
  nW <- nrow(wall)
  wall$xw_p <- NA_real_; wall$yw_p <- NA_real_; wall$zw_p <- NA_real_
  wall$xw2_p <- NA_real_; wall$yw2_p <- NA_real_; wall$zw2_p <- NA_real_
  wall$xl_p <- NA_real_; wall$yl_p <- NA_real_; wall$zl_p <- NA_real_
  for (k in 1:nW) {
    pp <- proj_pt(wall$xw[k], wall$yw[k], wall$zw[k], W)
    wall$xw_p[k] <- pp[1]; wall$yw_p[k] <- pp[2]; wall$zw_p[k] <- pp[3]
    if (!is.na(wall$xw2[k])) {
      pp2 <- proj_pt(wall$xw2[k], wall$yw2[k], wall$zw2[k], W)
      wall$xw2_p[k] <- pp2[1]; wall$yw2_p[k] <- pp2[2]; wall$zw2_p[k] <- pp2[3]
    }
    if (!is.na(wall$xl[k])) {
      pp3 <- proj_pt(wall$xl[k], wall$yl[k], wall$zl[k], W)
      wall$xl_p[k] <- pp3[1]; wall$yl_p[k] <- pp3[2]; wall$zl_p[k] <- pp3[3]
    }
  }
  wall$lbx <- NA_character_; wall$lby <- NA_character_; wall$lbz <- NA_character_
  wall$lbx[wall$label==1] <- Lblx
  wall$lby[wall$label==2] <- Lbly
  wall$lbz[wall$label==3] <- Lblz
  ## axis rotations
  fac2 <- sqrt(xFac)
  xaxisRot <- 0; yaxisRot <- 0
  r1 <- wall[wall$id=="X1-Axis",][1,]
  dx1 <- r1$xw2_p - r1$xw_p
  if (!is.na(dx1) && abs(dx1) > 0.001) xaxisRot <- fac2*180*atan((r1$yw2_p-r1$yw_p)/dx1)/pi else xaxisRot <- 90
  r2 <- wall[wall$id=="Y3-Axis",][1,]
  dx2 <- r2$xw2_p - r2$xw_p
  if (!is.na(dx2) && abs(dx2) > 0.001) yaxisRot <- fac2*180*atan((r2$yw2_p-r2$yw_p)/dx2)/pi else yaxisRot <- 90

  DurMin <- min(tumor$Duration); DurMax <- max(tumor$Duration)
  ResMin <- min(tumor$Response); ResMax <- max(tumor$Response)
  durfac <- 1.05; resfac <- 1.05
  dx <- xFac
  aResMax <- resfac*max(abs(ResMax),abs(ResMin))
  aDurMax <- durfac*max(abs(DurMax),abs(DurMin))

  ## polygons
  pid <- 0
  prow <- list(); idx <- 1
  addrow <- function(type,pid,x,y,z,code,Lid,yTickVal,zTickVal,Drug,DrugLabel) {
    data.frame(type=type,pid=pid,x=x,y=y,z=z,code=code,Lid=Lid,
               yTickVal=yTickVal,zTickVal=zTickVal,Drug=Drug,DrugLabel=DrugLabel,
               stringsAsFactors=FALSE)
  }
  nn <- nrow(tumor)
  xMax <- NA_real_
  for (k in 1:nn) {
    i <- k-1
    Dur <- tumor$Duration[k]; Res <- tumor$Response[k]
    Dr <- tumor$Drug[k]; Dl <- tumor$DrugLabel[k]; Cd <- tumor$Code[k]
    pid <- pid+1
    x1 <- dx*(i-0.5-0.4); x2 <- dx*(i-0.5+0.4)
    prow[[idx]] <- addrow("Duration",pid,x1,0,0,NA,NA,NA,NA,Dr,Dl); idx<-idx+1
    prow[[idx]] <- addrow("Duration",pid,x1,Dur,0,NA,NA,NA,NA,Dr,Dl); idx<-idx+1
    prow[[idx]] <- addrow("Duration",pid,x2,Dur,0,NA,NA,NA,NA,Dr,Dl); idx<-idx+1
    prow[[idx]] <- addrow("Duration",pid,x2,0,0,NA,NA,NA,NA,Dr,Dl); idx<-idx+1
    pid <- pid+1
    prow[[idx]] <- addrow("Response",pid,x1,0,0,NA,NA,NA,NA,Dr,Dl); idx<-idx+1
    prow[[idx]] <- addrow("Response",pid,x1,0,Res,NA,NA,NA,NA,Dr,Dl); idx<-idx+1
    prow[[idx]] <- addrow("Response",pid,x2,0,Res,NA,NA,NA,NA,Dr,Dl); idx<-idx+1
    prow[[idx]] <- addrow("Response",pid,x2,0,0,NA,NA,NA,NA,Dr,Dl); idx<-idx+1
    if (!is.na(tumor$Dropped[k])) {
      pid <- pid+1
      prow[[idx]] <- addrow("Dropped",pid,dx*(i-0.5-0.3),Dur-0.03*DurMax,0,NA,NA,NA,NA,Dr,Dl); idx<-idx+1
      prow[[idx]] <- addrow("Dropped",pid,dx*(i-0.5),Dur,0,NA,NA,NA,NA,Dr,Dl); idx<-idx+1
      prow[[idx]] <- addrow("Dropped",pid,dx*(i-0.5+0.3),Dur-0.03*DurMax,0,NA,NA,NA,NA,Dr,Dl); idx<-idx+1
      prow[[idx]] <- addrow("Dropped",pid,dx*(i-0.5),Dur-0.06*DurMax,0,NA,NA,NA,NA,Dr,Dl); idx<-idx+1
    }
    xMax <- dx*i
    zc <- ifelse(Res < 0, Res, 0)
    prow[[idx]] <- addrow("Code",pid,dx*(i-0.5),0,zc,Cd,NA,NA,NA,Dr,Dl); idx<-idx+1
  }
  ## last block
  lastDr <- tumor$Drug[nn]; lastDl <- tumor$DrugLabel[nn]
  pid <- pid+1
  prow[[idx]] <- addrow("Response",pid,-dx,0,-aResMax,NA,NA,NA,NA,lastDr,lastDl); idx<-idx+1
  prow[[idx]] <- addrow("Response",pid,-dx,0,aResMax,NA,NA,NA,NA,lastDr,lastDl); idx<-idx+1
  pid <- pid+1
  prow[[idx]] <- addrow("Duration",pid,-dx,0,aResMax,NA,NA,NA,NA,lastDr,lastDl); idx<-idx+1
  prow[[idx]] <- addrow("Duration",pid,-dx,aDurMax,aResMax,NA,NA,NA,NA,lastDr,lastDl); idx<-idx+1
  ## Y-Axis grid
  yseq <- seq(0, aDurMax, by=10)
  Lid <- 1
  for (yv in yseq) {
    prow[[idx]] <- addrow("Y-Axis",NA,-dx,yv,0,NA,Lid,NA,NA,lastDr,lastDl); idx<-idx+1
    prow[[idx]] <- addrow("Y-Axis",NA,xMax,yv,0,NA,Lid,NA,NA,lastDr,lastDl); idx<-idx+1
    Lid <- Lid+1
  }
  for (yv in yseq) {
    prow[[idx]] <- addrow("Y-Values",NA,xMax,yv,0,NA,NA,yv,NA,lastDr,lastDl); idx<-idx+1
  }
  zneg <- numeric(0)
  if ((-aResMax) <= -10) zneg <- seq(-10, -aResMax, by=-10)
  zpos <- seq(0, aResMax, by=10)
  for (zv in zneg) {
    prow[[idx]] <- addrow("Z-Values",NA,-dx,0,zv,NA,NA,NA,zv,lastDr,lastDl); idx<-idx+1
  }
  for (zv in zpos) {
    prow[[idx]] <- addrow("Z-Values",NA,-dx,0,zv,NA,NA,NA,zv,lastDr,lastDl); idx<-idx+1
  }
  polygons <- do.call(rbind, prow)
  xmin <- min(polygons$x); xmax <- max(polygons$x)
  ymin <- min(polygons$y); ymax <- max(polygons$y)
  zmin <- min(polygons$z); zmax <- max(polygons$z)
  xrange <- xmax-xmin; yrange <- ymax-ymin; zrange <- zmax-zmin
  polygons$xn <- 2*xFac*(polygons$x-xmin)/xrange - xFac
  polygons$yn <- 2*(polygons$y-ymin)/yrange - 1
  polygons$zn <- 2*(polygons$z-zmin)/zrange - 1
  ## project data
  polygons$xp <- NA_real_; polygons$yp <- NA_real_; polygons$zp <- NA_real_
  for (r in 1:nrow(polygons)) {
    pp <- proj_pt(polygons$xn[r], polygons$yn[r], polygons$zn[r], W)
    polygons$xp[r] <- pp[1]; polygons$yp[r] <- pp[2]; polygons$zp[r] <- pp[3]
  }
  list(wall=wall, polygons=polygons, W=W, xaxisRot=xaxisRot, yaxisRot=yaxisRot,
       A=A,B=B,C=C, ranges=c(xmin,xmax,ymin,ymax,zmin,zmax),
       DurMin=DurMin,DurMax=DurMax,ResMin=ResMin,ResMax=ResMax,
       aDurMax=aDurMax,aResMax=aResMax,xMax=xMax)
}

library(ggplot2)

plot_waterfall <- function(res, xFac) {
  wall <- res$wall; poly <- res$polygons
  wall_axes <- wall[wall$id %in% c("X1-Axis","X3-Axis","X4-Axis","Y2-Axis","Y3-Axis","Y4-Axis","Z1-Axis","Z2-Axis","Z4-Axis"),]
  wall_quads <- wall[wall$id %in% c("Back","Right","Bottom","Center"),]
  dur <- poly[poly$type=="Duration",]
  rsp <- poly[poly$type=="Response",]
  drp <- poly[poly$type=="Dropped",]
  yax <- poly[poly$type=="Y-Axis",]
  yval <- poly[poly$type=="Y-Values",]
  zval <- poly[poly$type=="Z-Values",]
  cod <- poly[poly$type=="Code",]
  fillmap <- c("1.0 mg"="#FFFFFF","2.0 mg"="#FFFFC0","3.0 mg"="#F0D0A0",
               "4.0 mg"="orange","3.0 F6 mg"="#FF7F00","4.0 F6 mg"="red")
  wallfill <- c("Back"="#E0E0E0","Right"="#F0F0F0","Bottom"="#E7E7E7","Center"="#D0D0D0")
  p <- ggplot()
  p <- p + geom_polygon(data=wall_quads, aes(x=xw_p, y=yw_p, group=id, fill=id),
                        color="grey70", linewidth=0.3, alpha=1)
  p <- p + scale_fill_manual(values=wallfill, guide="none")
  if (nrow(yax)>0) {
    p <- p + geom_line(data=yax, aes(x=xp, y=yp, group=Lid), color="#B0B0B0", linewidth=0.3)
  }
  if (nrow(dur)>0) {
    p <- p + geom_polygon(data=dur, aes(x=xp, y=yp, group=pid),
                          fill="#7EB8DA", color="black", linewidth=0.25)
  }
  if (nrow(rsp)>0) {
    p <- p + geom_polygon(data=rsp, aes(x=xp, y=yp, group=pid, fill=DrugLabel),
                          color="black", linewidth=0.25)
  }
  p <- p + scale_fill_manual(values=fillmap, name="CC-122", limits=names(fillmap))
  if (nrow(drp)>0) {
    p <- p + geom_polygon(data=drp, aes(x=xp, y=yp, group=pid),
                          fill="gold", color="black", linewidth=0.25)
  }
  p <- p + geom_segment(data=wall_axes,
                        aes(x=xw_p, y=yw_p, xend=xw2_p, yend=yw2_p, linetype=wgroup),
                        color="black", linewidth=0.4)
  p <- p + scale_linetype_manual(values=c("D"="solid","L"="dashed"), guide="none")
  labdf <- wall[!is.na(wall$lbx) | !is.na(wall$lby) | !is.na(wall$lbz),]
  for (k in 1:nrow(labdf)) {
    if (!is.na(labdf$lbx[k])) p <- p + annotate("text", x=labdf$xl_p[k], y=labdf$yl_p[k],
                                                label=labdf$lbx[k], size=2.2, angle=res$xaxisRot, hjust=0, vjust=1)
    if (!is.na(labdf$lby[k])) p <- p + annotate("text", x=labdf$xl_p[k], y=labdf$yl_p[k],
                                                label=labdf$lby[k], size=2.2, angle=res$yaxisRot, hjust=0, vjust=0)
    if (!is.na(labdf$lbz[k])) p <- p + annotate("text", x=labdf$xl_p[k], y=labdf$yl_p[k],
                                                label=labdf$lbz[k], size=2.2, angle=90, hjust=0.5, vjust=0)
  }
  if (nrow(yval)>0) p <- p + geom_text(data=yval, aes(x=xp, y=yp, label=yTickVal), size=2, hjust=1, vjust=0.5, nudge_x=-0.02)
  if (nrow(zval)>0) {
    p <- p + geom_text(data=zval, aes(x=xp, y=yp, label=zTickVal), size=2, hjust=1, vjust=0.5, nudge_x=-0.02)
    p <- p + geom_point(data=zval, aes(x=xp, y=yp), size=0.6, shape=16)
  }
  if (nrow(cod)>0) p <- p + geom_text(data=cod, aes(x=xp, y=yp, label=code), size=2, angle=90, hjust=0, vjust=0.5)
  p <- p + ggtitle(Title) +
    labs(caption=paste0("X-Rotation=",res$A,"  Y-Rotation=",res$B,"  Z-Rotation=",res$C,"   xFac=",xFac)) +
    coord_equal() +
    theme_void() +
    theme(plot.title=element_text(size=10, hjust=0.5),
          plot.caption=element_text(size=6, hjust=0, face="italic"),
          legend.position=c(0.85,0.15), legend.background=element_rect(fill="white", color="black"),
          legend.title=element_text(size=6), legend.text=element_text(size=6),
          legend.key.size=unit(4,"pt"))
  p
}

for (xf in c(1.0, 1.25, 1.5)) {
  res <- process_one(xf, Tilt=75, Rotate=-45)
  pp <- plot_waterfall(res, xf)
  suffix <- if (xf==1.0) "100" else if (xf==1.25) "125" else "150"
  pngfile <- file.path(out, paste0("waterfall_xf_", suffix, ".png"))
  ggsave(pngfile, pp, width=4, height=4, dpi=300, units="in")
  csvfile <- file.path(out, paste0("waterfall_xf_", suffix, ".csv"))
  plotdata <- res$polygons[, c("type","pid","x","y","z","xn","yn","zn","xp","yp","zp","code","Lid","yTickVal","zTickVal","Drug","DrugLabel")]
  write.csv(plotdata, csvfile, row.names=FALSE, na="")
}
```