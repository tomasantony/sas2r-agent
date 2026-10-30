```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)

# ---------- SAS RANUNI emulation ----------
mod_mult <- function(a, b, m) {
  # (a*b) %% m without overflow, using binary method
  res <- 0
  a <- a %% m
  while (b > 0) {
    if (b %% 2 == 1) res <- (res + a) %% m
    a <- (a * 2) %% m
    b <- floor(b / 2)
  }
  res
}
sas_ranuni_seq <- function(seed0, n) {
  m <- 2147483647
  a <- 397204094
  s <- seed0
  vals <- numeric(n)
  for (i in seq_len(n)) {
    s <- mod_mult(a, s, m)
    vals[i] <- s / m
  }
  vals
}

# generate streams
u3 <- sas_ranuni_seq(3, 100)  # 5 per obs *20
u2 <- sas_ranuni_seq(2, 20)

# build tumor data exactly as SAS data step
tumor_rows <- list()
idx3 <- 1
idx2 <- 1
for (i in 1:20) {
  u_resp <- u3[idx3]; idx3 <- idx3+1
  Response <- 100*(u_resp - 0.7)
  u_dur <- u3[idx3]; idx3 <- idx3+1
  Duration <- 50*u_dur
  u_drop <- u3[idx3]; idx3 <- idx3+1
  Dropped <- NA_real_
  if (u_drop > 0.8) Dropped <- Duration - 5
  u_drug <- u3[idx3]; idx3 <- idx3+1
  Drug_num <- floor(6*u_drug)
  u_code <- u3[idx3]; idx3 <- idx3+1
  Code <- ifelse(u_code > 0.7, "FL", "DLBCL")
  codeloc <- ifelse(Response > 0, 0, Response)
  u_base <- u2[idx2]; idx2 <- idx2+1
  baseline <- 20 + 180*u_base
  tumor_rows[[i]] <- tibble(
    i=i, Response=Response, Duration=Duration, Dropped=Dropped,
    Drug_num=Drug_num, Code=Code, codeloc=codeloc, baseline=baseline
  )
}
tumor <- bind_rows(tumor_rows)

# format Drug
drug_fmt <- c("1.0 mg","2.0 mg","3.0 mg","4.0 mg","3.0 F6 mg","4.0 F6 mg")
tumor$Drug <- drug_fmt[tumor$Drug_num+1]
tumor$Drug <- factor(tumor$Drug, levels=drug_fmt)

# sort by descending Response (SAS sort)
tumor <- tumor %>% arrange(desc(Response))
tumor$j <- seq_len(nrow(tumor))

# attrmap
attrmap <- tibble(
  ID=c("Walls","Walls","Walls","Walls","Axes","Axes",
       "Resp","Resp","Resp","Resp","Resp","Resp"),
  value=c("Center","Back","Right","Bottom","D","L",
          "1.0 mg","2.0 mg","3.0 mg","4.0 mg","3.0 F6 mg","4.0 F6 mg"),
  fillcolor=c("cxd0d0d0","cxe0e0e0","cxf0f0f0","cxe7e7e7","white","white",
              "white","cxffffc0","cxf0d0a0","orange","cxff7f00","red"),
  linecolor=c("cxd0d0d0","cxe0e0e0","cxf0f0f0","cxe7e7e7","black","black",
              rep(NA,6)),
  linepattern=c("Solid","Solid","Solid","Solid","Solid","ShortDash", rep(NA,6)),
  show=c(rep(NA,6), rep("Attrmap",6))
)
# convert cx colors to hex
cx_to_hex <- function(x) {
  ifelse(substr(x,1,2)=="cx", paste0("#", substr(x,3,8)), x)
}
attrmap$fillcolor_hex <- cx_to_hex(attrmap$fillcolor)
attrmap$linecolor_hex <- cx_to_hex(attrmap$linecolor)

resp_colors <- c(
  "1.0 mg"="#FFFFFF",
  "2.0 mg"="#ffffc0",
  "3.0 mg"="#f0d0a0",
  "4.0 mg"="orange",
  "3.0 F6 mg"="#ff7f00",
  "4.0 F6 mg"="red"
)
wall_colors <- c(
  "Center"="#d0d0d0",
  "Back"="#e0e0e0",
  "Right"="#f0f0f0",
  "Bottom"="#e7e7e7"
)

# helper to build Wall_Axes
build_wall_axes <- function(xFac){
  dx <- xFac
  tibble(
    id=c("X1-Axis","X3-Axis","X4-Axis","Y2-Axis","Y3-Axis","Y4-Axis","Z1-Axis","Z2-Axis","Z4-Axis",
         rep("Back",4), rep("Right",4), rep("Bottom",4), rep("Center",4)),
    wgroup=c("D","L","D","D","D","L","D","L","D", rep("D",16)),
    xw=c(-dx,-dx,-dx,-dx, dx, dx,-dx, dx, dx,
         -dx,-dx,-dx,-dx,
         -dx, dx, dx,-dx,
         -dx, dx, dx,-dx,
         -dx, dx, dx,-dx),
    yw=c(-1,-1,1,-1,-1,-1,-1,-1,1,
         -1,1,1,-1,
         1,1,1,1,
         -1,-1,1,1,
         -1,-1,1,1),
    zw=c(-1,1,1,1,-1,1,-1,-1,-1,
         -1,-1,1,1,
         -1,-1,1,1,
         -1,-1,-1,-1,
         0,0,0,0),
    xw2=c(dx,dx,dx,-dx,dx,dx,-dx,dx,dx, rep(NA_real_,16)),
    yw2=c(-1,-1,1,1,1,1,-1,-1,1, rep(NA_real_,16)),
    zw2=c(-1,1,1,1,-1,1,1,1,1, rep(NA_real_,16)),
    xl=c(0,NA,NA,NA,dx+0.1,NA,-dx,NA,NA, rep(NA_real_,16)),
    yl=c(-1,NA,NA,NA,0,NA,-1.2,NA,NA, rep(NA_real_,16)),
    zl=c(-1.1,NA,NA,NA,-0.1,NA,0,NA,NA, rep(NA_real_,16)),
    label=c(1,NA,NA,NA,2,NA,3,NA,NA, rep(NA_real_,16))
  )
}

build_matrices <- function(Adeg,Bdeg,Cdeg){
  r<-1; t<-1; f<-1; n<- -1
  fac <- pi/180
  A <- Adeg*fac; B <- Bdeg*fac; C <- Cdeg*fac
  m <- matrix(0,4,4)
  m[1,1]<-1/r; m[2,2]<-1/t; m[3,3]<- -2/(f-n); m[3,4]<- -(f+n)/(f-n); m[4,4]<-1
  rx <- matrix(0,4,4)
  rx[1,1]<-1; rx[2,2]<-cos(A); rx[2,3]<- -sin(A); rx[3,2]<-sin(A); rx[3,3]<-cos(A); rx[4,4]<-1
  ry <- matrix(0,4,4)
  ry[1,1]<-cos(B); ry[1,3]<-sin(B); ry[2,2]<-1; ry[3,1]<- -sin(B); ry[3,3]<-cos(B); ry[4,4]<-1
  rz <- matrix(0,4,4)
  rz[1,1]<-cos(C); rz[1,2]<- -sin(C); rz[2,1]<-sin(C); rz[2,2]<-cos(C); rz[3,3]<-1; rz[4,4]<-1
  w <- rx %*% ry %*% rz %*% m
  w
}

project_walls <- function(wall_axes, w, Lblx, Lbly, Lblz){
  df <- wall_axes
  df$lbx <- NA_character_; df$lby <- NA_character_; df$lbz <- NA_character_
  df$lbx[df$label==1] <- Lblx
  df$lby[df$label==2] <- Lbly
  df$lbz[df$label==3] <- Lblz
  # transform
  n <- nrow(df)
  xw_p <- numeric(n); yw_p <- numeric(n); zw_p <- numeric(n)
  xw2_p <- numeric(n); yw2_p <- numeric(n); zw2_p <- numeric(n)
  xl_p <- numeric(n); yl_p <- numeric(n); zl_p <- numeric(n)
  for(i in seq_len(n)){
    d <- c(df$xw[i], df$yw[i], df$zw[i], 1)
    p <- w %*% d
    xw_p[i] <- p[1]; yw_p[i] <- p[2]; zw_p[i] <- p[3]
    if(!is.na(df$xw2[i])){
      d2 <- c(df$xw2[i], df$yw2[i], df$zw2[i], 1)
      p2 <- w %*% d2
      xw2_p[i] <- p2[1]; yw2_p[i] <- p2[2]; zw2_p[i] <- p2[3]
    } else {
      xw2_p[i] <- NA; yw2_p[i] <- NA; zw2_p[i] <- NA
    }
    if(!is.na(df$xl[i])){
      d3 <- c(df$xl[i], df$yl[i], df$zl[i], 1)
      p3 <- w %*% d3
      xl_p[i] <- p3[1]; yl_p[i] <- p3[2]; zl_p[i] <- p3[3]
    } else {
      xl_p[i] <- NA; yl_p[i] <- NA; zl_p[i] <- NA
    }
  }
  df$xw <- xw_p; df$yw <- yw_p; df$zw <- zw_p
  df$xw2 <- xw2_p; df$yw2 <- yw2_p; df$zw2 <- zw2_p
  df$xl <- xl_p; df$yl <- yl_p; df$zl <- zl_p
  df
}

waterfall_one <- function(xFac, Tilt=75, Rotate=-45, Lblx="Subject", Lbly="Duration of Treatment in Days", Lblz="Tumor Lesion Best Change", Title="Tumor Response and Duration"){
  # clamp
  if(xFac>4) xFac<-4
  if(xFac<0.5) xFac<-0.5
  A <- Tilt; if(A>80) A<-80; if(A<60) A<-60
  B <- 0
  C <- Rotate; if(C> -15) C<- -15; if(C< -75) C<- -75

  wall_axes <- build_wall_axes(xFac)
  w <- build_matrices(A,B,C)
  proj_walls <- project_walls(wall_axes, w, Lblx, Lbly, Lblz)

  # axis rotations
  fac <- sqrt(xFac)
  xaxisRot <- 0; yaxisRot <- 0
  # find X1-Axis and Y3-Axis rows
  for(i in seq_len(nrow(proj_walls))){
    if(proj_walls$id[i]=="X1-Axis" && !is.na(proj_walls$xw2[i])){
      dx <- proj_walls$xw2[i]-proj_walls$xw[i]
      if(abs(dx)>0.001) xaxisRot <- fac*180*atan((proj_walls$yw2[i]-proj_walls$yw[i])/dx)/3.1415 else xaxisRot<-90
    }
    if(proj_walls$id[i]=="Y3-Axis" && !is.na(proj_walls$xw2[i])){
      dx <- proj_walls$xw2[i]-proj_walls$xw[i]
      if(abs(dx)>0.001) yaxisRot <- fac*180*atan((proj_walls$yw2[i]-proj_walls$yw[i])/dx)/3.1415 else yaxisRot<-90
    }
  }

  # Dur/Res min max
  DurMin <- min(tumor$Duration, na.rm=TRUE)
  DurMax <- max(tumor$Duration, na.rm=TRUE)
  ResMin <- min(tumor$Response, na.rm=TRUE)
  ResMax <- max(tumor$Response, na.rm=TRUE)

  durfac<-1.05; resfac<-1.05
  dx <- xFac
  aResMax <- resfac*max(abs(ResMax), abs(ResMin))
  aDurMax <- durfac*max(abs(DurMax), abs(DurMin))

  # polygons
  polys <- list()
  pid <- 0
  xMax <- NA
  for(idx in seq_len(nrow(tumor))){
    i <- idx-1  # SAS i = _n_-1
    # Duration bar
    pid <- pid+1
    z <- 0
    type <- "Duration"
    polys[[length(polys)+1]] <- tibble(pid=pid, type=type, x=dx*(i-0.5-0.4), y=0, z=z, Code=NA_character_, Lid=NA_real_, yTickVal=NA_real_, zTickVal=NA_real_, Drug=tumor$Drug[idx])
    polys[[length(polys)+1]] <- tibble(pid=pid, type=type, x=dx*(i-0.5-0.4), y=tumor$Duration[idx], z=z, Code=NA_character_, Lid=NA_real_, yTickVal=NA_real_, zTickVal=NA_real_, Drug=tumor$Drug[idx])
    polys[[length(polys)+1]] <- tibble(pid=pid, type=type, x=dx*(i-0.5+0.4), y=tumor$Duration[idx], z=z, Code=NA_character_, Lid=NA_real_, yTickVal=NA_real_, zTickVal=NA_real_, Drug=tumor$Drug[idx])
    polys[[length(polys)+1]] <- tibble(pid=pid, type=type, x=dx*(i-0.5+0.4), y=0, z=z, Code=NA_character_, Lid=NA_real_, yTickVal=NA_real_, zTickVal=NA_real_, Drug=tumor$Drug[idx])
    # Response bar
    pid <- pid+1
    y <- 0
    type <- "Response"
    polys[[length(polys)+1]] <- tibble(pid=pid, type=type, x=dx*(i-0.5-0.4), y=y, z=0, Code=NA_character_, Lid=NA_real_, yTickVal=NA_real_, zTickVal=NA_real_, Drug=tumor$Drug[idx])
    polys[[length(polys)+1]] <- tibble(pid=pid, type=type, x=dx*(i-0.5-0.4), y=y, z=tumor$Response[idx], Code=NA_character_, Lid=NA_real_, yTickVal=NA_real_, zTickVal=NA_real_, Drug=tumor$Drug[idx])
    polys[[length(polys)+1]] <- tibble(pid=pid, type=type, x=dx*(i-0.5+0.4), y=y, z=tumor$Response[idx], Code=NA_character_, Lid=NA_real_, yTickVal=NA_real_, zTickVal=NA_real_, Drug=tumor$Drug[idx])
    polys[[length(polys)+1]] <- tibble(pid=pid, type=type, x=dx*(i-0.5+0.4), y=y, z=0, Code=NA_character_, Lid=NA_real_, yTickVal=NA_real_, zTickVal=NA_real_, Drug=tumor$Drug[idx])
    # Dropped diamond
    if(!is.na(tumor$Dropped[idx])){
      pid <- pid+1
      type <- "Dropped"
      z <- 0
      polys[[length(polys)+1]] <- tibble(pid=pid, type=type, x=dx*(i-0.5-0.3), y=tumor$Duration[idx]-0.03*DurMax, z=z, Code=NA_character_, Lid=NA_real_, yTickVal=NA_real_, zTickVal=NA_real_, Drug=tumor$Drug[idx])
      polys[[length(polys)+1]] <- tibble(pid=pid, type=type, x=dx*(i-0.5), y=tumor$Duration[idx], z=z, Code=NA_character_, Lid=NA_real_, yTickVal=NA_real_, zTickVal=NA_real_, Drug=tumor$Drug[idx])
      polys[[length(polys)+1]] <- tibble(pid=pid, type=type, x=dx*(i-0.5+0.3), y=tumor$Duration[idx]-0.03*DurMax, z=z, Code=NA_character_, Lid=NA_real_, yTickVal=NA_real_, zTickVal=NA_real_, Drug=tumor$Drug[idx])
      polys[[length(polys)+1]] <- tibble(pid=pid, type=type, x=dx*(i-0.5), y=tumor$Duration[idx]-0.06*DurMax, z=z, Code=NA_character_, Lid=NA_real_, yTickVal=NA_real_, zTickVal=NA_real_, Drug=tumor$Drug[idx])
    }
    xMax <- dx*i
    # Code location
    type <- "Code"
    zc <- ifelse(tumor$Response[idx] < 0, tumor$Response[idx], 0)
    polys[[length(polys)+1]] <- tibble(pid=NA_real_, type=type, x=dx*(i-0.5), y=0, z=zc, Code=tumor$Code[idx], Lid=NA_real_, yTickVal=NA_real_, zTickVal=NA_real_, Drug=tumor$Drug[idx])
    if(idx==nrow(tumor)){
      # two more polygons to set aspect
      pid <- pid+1
      type <- "Response"
      polys[[length(polys)+1]] <- tibble(pid=pid, type=type, x=-dx, y=0, z=-aResMax, Code=NA_character_, Lid=NA_real_, yTickVal=NA_real_, zTickVal=NA_real_, Drug=NA_character_)
      polys[[length(polys)+1]] <- tibble(pid=pid, type=type, x=-dx, y=0, z= aResMax, Code=NA_character_, Lid=NA_real_, yTickVal=NA_real_, zTickVal=NA_real_, Drug=NA_character_)
      pid <- pid+1
      type <- "Duration"
      polys[[length(polys)+1]] <- tibble(pid=pid, type=type, x=-dx, y=0, z= aResMax, Code=NA_character_, Lid=NA_real_, yTickVal=NA_real_, zTickVal=NA_real_, Drug=NA_character_)
      polys[[length(polys)+1]] <- tibble(pid=pid, type=type, x=-dx, y=aDurMax, z= aResMax, Code=NA_character_, Lid=NA_real_, yTickVal=NA_real_, zTickVal=NA_real_, Drug=NA_character_)
      # Y-Axis grid
      Lid <- 1
      type <- "Y-Axis"
      z <- 0
      for(yv in seq(0, aDurMax, by=10)){
        polys[[length(polys)+1]] <- tibble(pid=NA_real_, type=type, x=-dx, y=yv, z=z, Code=NA_character_, Lid=Lid, yTickVal=NA_real_, zTickVal=NA_real_, Drug=NA_character_)
        polys[[length(polys)+1]] <- tibble(pid=NA_real_, type=type, x=xMax, y=yv, z=z, Code=NA_character_, Lid=Lid, yTickVal=NA_real_, zTickVal=NA_real_, Drug=NA_character_)
        Lid <- Lid+1
      }
      # Y-Values
      type <- "Y-Values"
      z <- 0
      for(yv in seq(0, aDurMax, by=10)){
        polys[[length(polys)+1]] <- tibble(pid=NA_real_, type=type, x=xMax, y=yv, z=z, Code=NA_character_, Lid=NA_real_, yTickVal=yv, zTickVal=NA_real_, Drug=NA_character_)
      }
      # Z-Values
      type <- "Z-Values"
      yv <- 0
      xzv <- dx*(-1)
      for(zv in seq(-10, -aResMax, by=-10)){
        polys[[length(polys)+1]] <- tibble(pid=NA_real_, type=type, x=xzv, y=yv, z=zv, Code=NA_character_, Lid=NA_real_, yTickVal=NA_real_, zTickVal=zv, Drug=NA_character_)
      }
      for(zv in seq(0, aResMax, by=10)){
        polys[[length(polys)+1]] <- tibble(pid=NA_real_, type=type, x=xzv, y=yv, z=zv, Code=NA_character_, Lid=NA_real_, yTickVal=NA_real_, zTickVal=zv, Drug=NA_character_)
      }
    }
  }
  polygons <- bind_rows(polys)

  # compute ranges
  xmin <- min(polygons$x, na.rm=TRUE); xmax <- max(polygons$x, na.rm=TRUE)
  ymin <- min(polygons$y, na.rm=TRUE); ymax <- max(polygons$y, na.rm=TRUE)
  zmin <- min(polygons$z, na.rm=TRUE); zmax <- max(polygons$z, na.rm=TRUE)
  xrange <- xmax - xmin; yrange <- ymax - ymin; zrange <- zmax - zmin

  # normalize
  normalized <- polygons %>%
    mutate(
      x = 2*xFac*(x - xmin)/xrange - xFac,
      y = 2*(y - ymin)/yrange -1,
      z = 2*(z - zmin)/zrange -1
    )

  # project data
  proj_list <- list()
  for(k in seq_len(nrow(normalized))){
    d <- c(normalized$x[k], normalized$y[k], normalized$z[k], 1)
    p <- w %*% d
    tp <- normalized$type[k]
    row <- tibble(
      Drug=normalized$Drug[k], pid=normalized$pid[k], type=tp,
      xd=NA_real_, yd=NA_real_, zd=NA_real_,
      xr=NA_real_, yr=NA_real_, zr=NA_real_,
      xc=NA_real_, yc=NA_real_, zc=NA_real_,
      xcode=NA_real_, ycode=NA_real_, zcode=NA_real_,
      xAy=NA_real_, yAy=NA_real_, zAy=NA_real_,
      xYv=NA_real_, yYv=NA_real_, zYv=NA_real_,
      xZv=NA_real_, yZv=NA_real_, zZv=NA_real_,
      Lid=normalized$Lid[k], yTickVal=normalized$yTickVal[k], zTickVal=normalized$zTickVal[k],
      Code=normalized$Code[k],
      orig_x=normalized$x[k], orig_y=normalized$y[k], orig_z=normalized$z[k]
    )
    if(tp=="Duration"){
      row$xd <- p[1]; row$yd <- p[2]; row$zd <- p[3]
    } else if(tp=="Response"){
      row$xr <- p[1]; row$yr <- p[2]; row$zr <- p[3]
    } else if(tp=="Dropped"){
      row$xc <- p[1]; row$yc <- p[2]; row$zc <- p[3]
    } else if(tp=="Code"){
      row$xcode <- p[1]; row$ycode <- p[2]; row$zcode <- p[3]
    } else if(tp=="Y-Axis"){
      row$xAy <- p[1]; row$yAy <- p[2]; row$zAy <- p[3]
    } else if(tp=="Y-Values"){
      row$xYv <- p[1]; row$yYv <- p[2]; row$zYv <- p[3]
    } else if(tp=="Z-Values"){
      row$xZv <- p[1]; row$yZv <- p[2]; row$zZv <- p[3]
    }
    proj_list[[k]] <- row
  }
  projected_data <- bind_rows(proj_list)

  list(
    proj_walls=proj_walls, projected_data=projected_data, polygons=polygons,
    normalized=normalized, xaxisRot=xaxisRot, yaxisRot=yaxisRot,
    A=A, B=B, C=C, xFac=xFac, Title=Title, Lblx=Lblx, Lbly=Lbly, Lblz=Lblz
  )
}

plot_waterfall <- function(res){
  pw <- res$proj_walls
  pd <- res$projected_data

  # walls polygons
  walls_df <- pw %>% filter(id %in% c("Back","Right","Bottom","Center")) %>%
    select(id, xw, yw) %>% rename(x=xw, y=yw)

  # axes lines
  axes_df <- pw %>% filter(grepl("Axis", id)) %>% select(id, wgroup, xw, yw, xw2, yw2)

  # axis labels
  labels_df <- pw %>% filter(!is.na(xl)) %>% select(xl, yl, lbx, lby, lbz) %>%
    mutate(label = coalesce(lbx, lby, lbz)) %>% filter(!is.na(label))

  # grid lines
  grid_df <- pd %>% filter(type=="Y-Axis") %>% select(Lid, xAy, yAy) %>% filter(!is.na(xAy))

  # duration bars
  dur_df <- pd %>% filter(type=="Duration", !is.na(xd))

  # response bars
  resp_df <- pd %>% filter(type=="Response", !is.na(xr))

  # dropped
  drop_df <- pd %>% filter(type=="Dropped", !is.na(xc))

  # Y tick values
  ytick_df <- pd %>% filter(type=="Y-Values", !is.na(xYv))

  # Z tick values
  ztick_df <- pd %>% filter(type=="Z-Values", !is.na(xZv))

  # code labels
  code_df <- pd %>% filter(type=="Code", !is.na(xcode))

  p <- ggplot() +
    # walls
    geom_polygon(data=walls_df, aes(x=x, y=y, group=id, fill=id), color="grey80", alpha=0.9) +
    scale_fill_manual(values=wall_colors, guide="none") +
    # grid
    geom_line(data=grid_df, aes(x=xAy, y=yAy, group=Lid), color="#b0b0b0", linewidth=0.3) +
    # duration bars (light)
    geom_polygon(data=dur_df, aes(x=xd, y=yd, group=pid), fill="grey90", color="black", linewidth=0.3, alpha=0.9) +
    # response bars colored by Drug
    geom_polygon(data=resp_df, aes(x=xr, y=yr, group=pid, fill=Drug), color="black", linewidth=0.3) +
    scale_fill_manual(values=resp_colors, name="CC-122", na.translate=FALSE) +
    # dropped diamonds
    geom_polygon(data=drop_df, aes(x=xc, y=yc, group=pid), fill="gold", color="black", linewidth=0.3) +
    # axes lines
    geom_segment(data=axes_df, aes(x=xw, y=yw, xend=xw2, yend=yw2, linetype=wgroup), color="black", linewidth=0.5) +
    scale_linetype_manual(values=c(D="solid", L="dashed"), guide="none") +
    # axis labels
    geom_text(data=labels_df %>% filter(!is.na(lbx)), aes(x=xl, y=yl, label=label), angle=res$xaxisRot, size=2.5, hjust=0.5, vjust=1) +
    geom_text(data=labels_df %>% filter(!is.na(lby)), aes(x=xl, y=yl, label=label), angle=res$yaxisRot, size=2.5, hjust=0.5, vjust=1) +
    geom_text(data=labels_df %>% filter(!is.na(lbz)), aes(x=xl, y=yl, label=label), angle=90, size=2.5, hjust=0.5) +
    # Y ticks
    geom_text(data=ytick_df, aes(x=xYv, y=yYv, label=yTickVal), size=2, hjust=0, vjust=0.5) +
    # Z ticks
    geom_text(data=ztick_df, aes(x=xZv, y=yZv, label=zTickVal), size=2, hjust=1, vjust=0.5) +
    geom_point(data=ztick_df, aes(x=xZv, y=yZv), size=0.8) +
    # code labels
    geom_text(data=code_df, aes(x=xcode, y=ycode, label=Code), angle=90, size=2, hjust=0, vjust=0.5) +
    labs(title=res$Title,
         caption=paste0("X-Rotation=",res$A,"  Y-Rotation=",res$B,"  Z-Rotation=",res$C)) +
    theme_void() +
    theme(
      plot.title=element_text(size=10, hjust=0.5),
      plot.caption=element_text(size=5, hjust=0, face="italic"),
      legend.position=c(0.85,0.15),
      legend.background=element_rect(fill="white", color="black", linewidth=0.3),
      legend.title=element_text(size=5),
      legend.text=element_text(size=5),
      legend.key.size=unit(4,"pt"),
      aspect.ratio=1
    ) +
    coord_fixed(clip="off")

  p
}

# generate three figures
xfacs <- c(1.0, 1.25, 1.5)
for(xf in xfacs){
  res <- waterfall_one(xFac=xf, Tilt=75, Rotate=-45,
                       Lblx="Subject", Lbly="Duration of Treatment in Days", Lblz="Tumor Lesion Best Change",
                       Title="Tumor Response and Duration")
  plt <- plot_waterfall(res)
  suffix <- sprintf("%03d", as.integer(xf*100))
  # file names expected: waterfall_xf_100.png etc
  png_file <- file.path(out, paste0("waterfall_xf_", suffix, ".png"))
  ggsave(png_file, plot=plt, width=4, height=4, dpi=300, units="in", bg="white")
  # CSV of plotted data
  csv_file <- file.path(out, paste0("waterfall_xf_", suffix, ".csv"))
  # combine projected_data and walls for CSV
  out_df <- res$projected_data %>%
    select(type, pid, Drug, Code, Lid, yTickVal, zTickVal,
           xd, yd, xr, yr, xc, yc, xcode, ycode, xAy, yAy, xYv, yYv, xZv, yZv)
  # also add walls
  walls_out <- res$proj_walls %>% transmute(type=id, pid=NA_real_, Drug=NA_character_, Code=NA_character_, Lid=NA_real_, yTickVal=NA_real_, zTickVal=NA_real_,
                                            xd=xw, yd=yw, xr=xw2, yr=yw2, xc=xl, yc=yl, xcode=NA_real_, ycode=NA_real_, xAy=NA_real_, yAy=NA_real_, xYv=NA_real_, yYv=NA_real_, xZv=NA_real_, yZv=NA_real_)
  combined_csv <- bind_rows(out_df, walls_out)
  write.csv(combined_csv, csv_file, row.names=FALSE, na="")
  # also write tumor sorted for reference
  # ensure PNG exists
  message("Wrote ", png_file, " and ", csv_file)
}

# also write tumor data for listing
write.csv(tumor, file.path(out, "tumor.csv"), row.names=FALSE, na="")

```