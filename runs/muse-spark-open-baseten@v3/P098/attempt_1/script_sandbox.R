out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

library(dplyr)
library(tidyr)
library(ggplot2)

# ---- SAS RANUNI emulation (Fishman-Moore, modulus 2^31-1, multiplier 397204094) ----
mod_mult <- function(a, b, m) {
  # modular multiplication without overflow (a,b < m < 2^31)
  res <- 0
  a <- a %% m
  while (b > 0) {
    if (b %% 2 == 1) res <- (res + a) %% m
    a <- (a * 2) %% m
    b <- floor(b / 2)
  }
  res
}
sas_ranuni_stream <- function(seed0, n) {
  mod <- 2147483647
  mult <- 397204094
  seed <- seed0
  vals <- numeric(n)
  for (i in seq_len(n)) {
    seed <- mod_mult(seed, mult, mod)
    vals[i] <- seed / mod
  }
  vals
}

# ---- Generate tumor data exactly as SAS data step ----
# SAS code:
# do i=1 to 20;
#   Response=100*(ranuni(3)-0.7);
#   Duration=50*ranuni(3);
#   Dropped=.;
#   if ranuni(3) > 0.8 then Dropped=duration-5;
#   Drug=floor(6*ranuni(3));
#   if ranuni(3) > 0.7 then Code='FL'; else Code='DLBCL';
#   codeloc=ifn(response > 0, 0, response);
#   baseline=20+180*(ranuni(2));
#   output;
# end;

n_subj <- 20
u3 <- sas_ranuni_stream(3, n_subj * 5)  # 5 calls per iteration to seed 3
u2 <- sas_ranuni_stream(2, n_subj)      # 1 call per iteration to seed 2

tumor <- tibble(i = 1:n_subj)
# map u3 sequentially: for each i, indices 5*(i-1)+1 .. 5*i
idx <- function(i, k) (i - 1) * 5 + k
tumor <- tumor %>%
  mutate(
    Response = 100 * (u3[idx(i, 1)] - 0.7),
    Duration = 50 * u3[idx(i, 2)],
    Dropped = ifelse(u3[idx(i, 3)] > 0.8, Duration - 5, NA_real_),
    Drug = floor(6 * u3[idx(i, 4)]),
    Code = ifelse(u3[idx(i, 5)] > 0.7, "FL", "DLBCL"),
    codeloc = ifelse(Response > 0, 0, Response),
    baseline = 20 + 180 * u2[i]
  )

# format Drug res. 0='1.0 mg' ... 5='4.0 F6 mg'
drug_labels <- c("1.0 mg","2.0 mg","3.0 mg","4.0 mg","3.0 F6 mg","4.0 F6 mg")
tumor <- tumor %>%
  mutate(DrugLabel = factor(drug_labels[Drug + 1], levels = drug_labels))

# Sort by descending Response (SAS proc sort)
tumor <- tumor %>% arrange(desc(Response))
tumorsorted <- tumor %>% mutate(j = row_number())

# Attribute map for Resp groups
attrmap_resp <- tibble(
  value = drug_labels,
  fillcolor = c("white","#ffffc0","#f0d0a0","orange","#ff7f00","red")
)

# Helper: matrix multiplication (SAS call mult)
mat_mult <- function(A, B) A %*% B

# Function to build Wall_Axes
build_wall_axes <- function(xFac) {
  dx <- xFac
  tibble(
    id = c("X1-Axis","X3-Axis","X4-Axis","Y2-Axis","Y3-Axis","Y4-Axis","Z1-Axis","Z2-Axis","Z4-Axis",
           rep("Back",4), rep("Right",4), rep("Bottom",4), rep("Center",4)),
    wgroup = c("D","L","D","D","D","L","D","L","D", rep("D",16)),
    xw = c(-dx, -dx, -dx, -dx,  dx,  dx, -dx,  dx,  dx,
           -dx, -dx, -dx, -dx,
           -dx,  dx,  dx, -dx,
           -dx,  dx,  dx, -dx,
           -dx,  dx,  dx, -dx),
    yw = c(-1, -1,  1, -1, -1, -1, -1, -1,  1,
           -1,  1,  1, -1,
            1,  1,  1,  1,
           -1, -1,  1,  1,
           -1, -1,  1,  1),
    zw = c(-1,  1,  1,  1, -1,  1, -1, -1, -1,
           -1, -1,  1,  1,
           -1, -1,  1,  1,
           -1, -1, -1, -1,
            0,  0,  0,  0),
    xw2 = c( dx,  dx,  dx, -dx,  dx,  dx, -dx,  dx,  dx, rep(NA_real_,16)),
    yw2 = c(-1, -1,  1,  1,  1,  1, -1, -1,  1, rep(NA_real_,16)),
    zw2 = c(-1,  1,  1,  1, -1,  1,  1,  1,  1, rep(NA_real_,16)),
    xl = c(0, NA, NA, NA, dx+0.1, NA, -dx, NA, NA, rep(NA_real_,16)),
    yl = c(-1, NA, NA, NA, 0, NA, -1.2, NA, NA, rep(NA_real_,16)),
    zl = c(-1.1, NA, NA, NA, -0.1, NA, 0, NA, NA, rep(NA_real_,16)),
    label = c(1, NA, NA, NA, 2, NA, 3, NA, NA, rep(NA_real_,16))
  )
}

# Function to project walls/axes
project_walls <- function(wall_axes, Tilt, Rotate) {
  A <- Tilt; B <- 0; C <- Rotate
  fac <- pi/180
  Ar <- A*fac; Br <- B*fac; Cr <- C*fac
  r <- 1; t <- 1; f <- 1; n <- -1
  m <- matrix(0,4,4)
  m[1,1] <- 1/r; m[2,2] <- 1/t; m[3,3] <- -2/(f-n); m[3,4] <- -(f+n)/(f-n); m[4,4] <- 1
  rx <- matrix(0,4,4)
  rx[1,1]<-1; rx[2,2]<-cos(Ar); rx[2,3]<- -sin(Ar); rx[3,2]<-sin(Ar); rx[3,3]<-cos(Ar); rx[4,4]<-1
  ry <- matrix(0,4,4)
  ry[1,1]<-cos(Br); ry[1,3]<-sin(Br); ry[2,2]<-1; ry[3,1]<- -sin(Br); ry[3,3]<-cos(Br); ry[4,4]<-1
  rz <- matrix(0,4,4)
  rz[1,1]<-cos(Cr); rz[1,2]<- -sin(Cr); rz[2,1]<-sin(Cr); rz[2,2]<-cos(Cr); rz[3,3]<-1; rz[4,4]<-1
  u <- mat_mult(rz, m)
  v <- mat_mult(ry, u)
  w <- mat_mult(rx, v)

  # transform each row
  out <- wall_axes
  # helper to transform point
  transform_point <- function(x,y,z) {
    if (is.na(x) || is.na(y) || is.na(z)) return(c(NA,NA,NA))
    d <- matrix(c(x,y,z,1), nrow=4)
    p <- mat_mult(w, d)
    c(p[1,1], p[2,1], p[3,1])
  }
  nroww <- nrow(out)
  xw_n <- yw_n <- zw_n <- numeric(nroww)
  xw2_n <- yw2_n <- zw2_n <- numeric(nroww)
  xl_n <- yl_n <- zl_n <- numeric(nroww)
  for (k in seq_len(nroww)) {
    pp <- transform_point(out$xw[k], out$yw[k], out$zw[k])
    xw_n[k] <- pp[1]; yw_n[k] <- pp[2]; zw_n[k] <- pp[3]
    pp2 <- transform_point(out$xw2[k], out$yw2[k], out$zw2[k])
    xw2_n[k] <- pp2[1]; yw2_n[k] <- pp2[2]; zw2_n[k] <- pp2[3]
    pp3 <- transform_point(out$xl[k], out$yl[k], out$zl[k])
    xl_n[k] <- pp3[1]; yl_n[k] <- pp3[2]; zl_n[k] <- pp3[3]
  }
  out$xw <- xw_n; out$yw <- yw_n; out$zw <- zw_n
  out$xw2 <- xw2_n; out$yw2 <- yw2_n; out$zw2 <- zw2_n
  out$xl <- xl_n; out$yl <- yl_n; out$zl <- zl_n

  # set axis labels
  out$lbx <- ifelse(!is.na(out$label) & out$label==1, "Subject", NA_character_)
  out$lby <- ifelse(!is.na(out$label) & out$label==2, "Duration of Treatment in Days", NA_character_)
  out$lbz <- ifelse(!is.na(out$label) & out$label==3, "Tumor Lesion Best Change", NA_character_)
  out
}

# Function to build polygons (SAS data polygons)
build_polygons <- function(data_sorted, xFac) {
  durfac <- 1.05; resfac <- 1.05
  dx <- xFac
  DurMin <- min(data_sorted$Duration, na.rm=TRUE)
  DurMax <- max(data_sorted$Duration, na.rm=TRUE)
  ResMin <- min(data_sorted$Response, na.rm=TRUE)
  ResMax <- max(data_sorted$Response, na.rm=TRUE)
  aResMax <- resfac * max(abs(ResMax), abs(ResMin))
  aDurMax <- durfac * max(abs(DurMax), abs(DurMin))

  rows <- list()
  pid <- 0
  n <- nrow(data_sorted)
  for (idx in seq_len(n)) {
    i <- idx - 1  # SAS i = _n_-1
    dur <- data_sorted$Duration[idx]
    resp <- data_sorted$Response[idx]
    dropped <- data_sorted$Dropped[idx]
    code <- data_sorted$Code[idx]
    grp <- data_sorted$DrugLabel[idx]

    # Duration bars in z=0 plane
    pid <- pid + 1
    z <- 0
    # 4 points
    x1 <- dx*(i-0.5-0.4); y1 <- 0
    x2 <- x1; y2 <- dur
    x3 <- dx*(i-0.5+0.4); y3 <- y2
    x4 <- x3; y4 <- 0
    rows[[length(rows)+1]] <- tibble(pid=pid, type="Duration", x=c(x1,x2,x3,x4), y=c(y1,y2,y3,y4), z=z, Code=code, DrugLabel=grp, Lid=NA_real_, yTickVal=NA_real_, zTickVal=NA_real_)
    # Response bars in y=0 plane
    pid <- pid + 1
    y <- 0
    x1r <- dx*(i-0.5-0.4); z1 <- 0
    x2r <- x1r; z2 <- resp
    x3r <- dx*(i-0.5+0.4); z3 <- z2
    x4r <- x3r; z4 <- 0
    rows[[length(rows)+1]] <- tibble(pid=pid, type="Response", x=c(x1r,x2r,x3r,x4r), y=y, z=c(z1,z2,z3,z4), Code=code, DrugLabel=grp, Lid=NA_real_, yTickVal=NA_real_, zTickVal=NA_real_)
    # Dropped diamond
    if (!is.na(dropped) && dropped) {
      pid <- pid + 1
      z <- 0
      xd1 <- dx*(i-0.5-0.3); yd1 <- dur - 0.03*DurMax
      xd2 <- dx*(i-0.5);     yd2 <- dur
      xd3 <- dx*(i-0.5+0.3); yd3 <- dur - 0.03*DurMax
      xd4 <- dx*(i-0.5);     yd4 <- dur - 0.06*DurMax
      rows[[length(rows)+1]] <- tibble(pid=pid, type="Dropped", x=c(xd1,xd2,xd3,xd4), y=c(yd1,yd2,yd3,yd4), z=z, Code=code, DrugLabel=grp, Lid=NA_real_, yTickVal=NA_real_, zTickVal=NA_real_)
    }
    # Code location
    rows[[length(rows)+1]] <- tibble(pid=NA_real_, type="Code", x=dx*(i-0.5), y=0, z=ifelse(resp < 0, resp, 0), Code=code, DrugLabel=grp, Lid=NA_real_, yTickVal=NA_real_, zTickVal=NA_real_)
  }
  xMax <- dx*(n)
  # two more polygons to set aspect (last iteration)
  # Response aspect
  pid <- pid + 1
  rows[[length(rows)+1]] <- tibble(pid=pid, type="Response", x=c(-dx,-dx), y=0, z=c(-aResMax, aResMax), Code=NA_character_, DrugLabel=NA_character_, Lid=NA_real_, yTickVal=NA_real_, zTickVal=NA_real_)
  pid <- pid + 1
  rows[[length(rows)+1]] <- tibble(pid=pid, type="Duration", x=c(-dx,-dx), y=c(0, aDurMax), z=aResMax, Code=NA_character_, DrugLabel=NA_character_, Lid=NA_real_, yTickVal=NA_real_, zTickVal=NA_real_)
  # Y-Axis grid
  Lid <- 1
  for (yy in seq(0, aDurMax, by=10)) {
    rows[[length(rows)+1]] <- tibble(pid=NA_real_, type="Y-Axis", x=c(-dx, xMax), y=yy, z=0, Code=NA_character_, DrugLabel=NA_character_, Lid=Lid, yTickVal=NA_real_, zTickVal=NA_real_)
    Lid <- Lid + 1
  }
  # Y-Values
  for (yy in seq(0, aDurMax, by=10)) {
    rows[[length(rows)+1]] <- tibble(pid=NA_real_, type="Y-Values", x=xMax, y=yy, z=0, Code=NA_character_, DrugLabel=NA_character_, Lid=NA_real_, yTickVal=yy, zTickVal=NA_real_)
  }
  # Z-Values
  for (zz in seq(-10, -aResMax, by=-10)) {
    rows[[length(rows)+1]] <- tibble(pid=NA_real_, type="Z-Values", x=dx*(-1), y=0, z=zz, Code=NA_character_, DrugLabel=NA_character_, Lid=NA_real_, yTickVal=NA_real_, zTickVal=zz)
  }
  for (zz in seq(0, aResMax, by=10)) {
    rows[[length(rows)+1]] <- tibble(pid=NA_real_, type="Z-Values", x=dx*(-1), y=0, z=zz, Code=NA_character_, DrugLabel=NA_character_, Lid=NA_real_, yTickVal=NA_real_, zTickVal=zz)
  }
  polygons <- bind_rows(rows)
  # store aResMax/aDurMax/xMax for later
  attr(polygons, "aResMax") <- aResMax
  attr(polygons, "aDurMax") <- aDurMax
  attr(polygons, "xMax") <- xMax
  polygons
}

# Normalize to -1 to +1
normalize_polygons <- function(polygons, xFac) {
  xmin <- min(polygons$x, na.rm=TRUE); xmax <- max(polygons$x, na.rm=TRUE)
  ymin <- min(polygons$y, na.rm=TRUE); ymax <- max(polygons$y, na.rm=TRUE)
  zmin <- min(polygons$z, na.rm=TRUE); zmax <- max(polygons$z, na.rm=TRUE)
  xrange <- xmax - xmin; yrange <- ymax - ymin; zrange <- zmax - zmin
  polygons %>%
    mutate(
      x = 2*xFac*(x - xmin)/xrange - xFac,
      y = 2*(y - ymin)/yrange - 1,
      z = 2*(z - zmin)/zrange - 1
    ) %>%
    mutate(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, zmin=zmin, zmax=zmax)
}

# Project data
project_data <- function(normalized, Tilt, Rotate) {
  A <- Tilt; B <- 0; C <- Rotate
  fac <- pi/180; Ar <- A*fac; Br <- B*fac; Cr <- C*fac
  r<-1; t<-1; f<-1; n<- -1
  m <- matrix(0,4,4); m[1,1]<-1/r; m[2,2]<-1/t; m[3,3]<- -2/(f-n); m[3,4]<- -(f+n)/(f-n); m[4,4]<-1
  rx <- matrix(0,4,4); rx[1,1]<-1; rx[2,2]<-cos(Ar); rx[2,3]<- -sin(Ar); rx[3,2]<-sin(Ar); rx[3,3]<-cos(Ar); rx[4,4]<-1
  ry <- matrix(0,4,4); ry[1,1]<-cos(Br); ry[1,3]<-sin(Br); ry[2,2]<-1; ry[3,1]<- -sin(Br); ry[3,3]<-cos(Br); ry[4,4]<-1
  rz <- matrix(0,4,4); rz[1,1]<-cos(Cr); rz[1,2]<- -sin(Cr); rz[2,1]<-sin(Cr); rz[2,2]<-cos(Cr); rz[3,3]<-1; rz[4,4]<-1
  u <- mat_mult(rz,m); v <- mat_mult(ry,u); w <- mat_mult(rx,v)
  transform <- function(x,y,z) {
    d <- matrix(c(x,y,z,1), nrow=4)
    p <- mat_mult(w,d)
    c(p[1,1], p[2,1], p[3,1])
  }
  # apply row-wise
  proj <- normalized
  # initialize projected columns
  proj$xd <- NA_real_; proj$yd <- NA_real_; proj$zd <- NA_real_
  proj$xr <- NA_real_; proj$yr <- NA_real_; proj$zr <- NA_real_
  proj$xc <- NA_real_; proj$yc <- NA_real_; proj$zc <- NA_real_
  proj$xcode <- NA_real_; proj$ycode <- NA_real_; proj$zcode <- NA_real_
  proj$xAy <- NA_real_; proj$yAy <- NA_real_; proj$zAy <- NA_real_
  proj$xYv <- NA_real_; proj$yYv <- NA_real_; proj$zYv <- NA_real_
  proj$xZv <- NA_real_; proj$yZv <- NA_real_; proj$zZv <- NA_real_

  for (k in seq_len(nrow(proj))) {
    pp <- transform(proj$x[k], proj$y[k], proj$z[k])
    tp <- proj$type[k]
    if (tp=="Duration") { proj$xd[k]<-pp[1]; proj$yd[k]<-pp[2]; proj$zd[k]<-pp[3] }
    else if (tp=="Response") { proj$xr[k]<-pp[1]; proj$yr[k]<-pp[2]; proj$zr[k]<-pp[3] }
    else if (tp=="Dropped") { proj$xc[k]<-pp[1]; proj$yc[k]<-pp[2]; proj$zc[k]<-pp[3] }
    else if (tp=="Code") { proj$xcode[k]<-pp[1]; proj$ycode[k]<-pp[2]; proj$zcode[k]<-pp[3] }
    else if (tp=="Y-Axis") { proj$xAy[k]<-pp[1]; proj$yAy[k]<-pp[2]; proj$zAy[k]<-pp[3] }
    else if (tp=="Y-Values") { proj$xYv[k]<-pp[1]; proj$yYv[k]<-pp[2]; proj$zYv[k]<-pp[3] }
    else if (tp=="Z-Values") { proj$xZv[k]<-pp[1]; proj$yZv[k]<-pp[2]; proj$zZv[k]<-pp[3] }
  }
  proj
}

# Plotting function for one xFac
plot_waterfall <- function(xFac, Tilt=75, Rotate=-45, title="Tumor Response and Duration") {
  wall_axes <- build_wall_axes(xFac)
  pw <- project_walls(wall_axes, Tilt, Rotate)

  # compute axis label rotations (SAS _null_ step)
  fac <- sqrt(xFac)
  # X1-Axis
  x1 <- pw %>% filter(id=="X1-Axis")
  dx <- x1$xw2 - x1$xw
  if (abs(dx) > 0.001) xaxisRot <- fac*180*atan((x1$yw2 - x1$yw)/dx)/pi else xaxisRot <- 90
  y3 <- pw %>% filter(id=="Y3-Axis")
  dx2 <- y3$xw2 - y3$xw
  if (abs(dx2) > 0.001) yaxisRot <- fac*180*atan((y3$yw2 - y3$yw)/dx2)/pi else yaxisRot <- 90

  polygons <- build_polygons(tumorsorted, xFac)
  normalized <- normalize_polygons(polygons, xFac)
  projected <- project_data(normalized, Tilt, Rotate)

  # Prepare data frames for ggplot
  # Walls: polygon id=id x=xw y=yw
  walls_df <- pw %>% filter(id %in% c("Back","Right","Bottom","Center")) %>%
    select(id, x=xw, y=yw)

  # Duration bars
  dur_df <- projected %>% filter(type=="Duration", !is.na(xd)) %>% select(pid, x=xd, y=yd, DrugLabel)
  # Response bars
  resp_df <- projected %>% filter(type=="Response", !is.na(xr)) %>% select(pid, x=xr, y=yr, DrugLabel)
  # Dropped
  drop_df <- projected %>% filter(type=="Dropped", !is.na(xc)) %>% select(pid, x=xc, y=yc)
  # Y-Axis grid lines
  yaxis_df <- projected %>% filter(type=="Y-Axis", !is.na(xAy)) %>% select(Lid, x=xAy, y=yAy)
  # Axis lines (vectors)
  axes_df <- pw %>% filter(grepl("Axis", id)) %>% select(id, wgroup, x=xw, y=yw, xend=xw2, yend=yw2)
  # Axis labels
  label_df <- pw %>% filter(!is.na(lbx) | !is.na(lby) | !is.na(lbz)) %>%
    mutate(label_text = coalesce(lbx, lby, lbz)) %>% select(x=xl, y=yl, label_text, label)
  # Y tick values
  ytick_df <- projected %>% filter(type=="Y-Values", !is.na(xYv)) %>% select(x=xYv, y=yYv, yTickVal)
  # Z tick values
  ztick_df <- projected %>% filter(type=="Z-Values", !is.na(xZv)) %>% select(x=xZv, y=yZv, zTickVal)
  # Code labels
  code_df <- projected %>% filter(type=="Code", !is.na(xcode)) %>% select(x=xcode, y=ycode, Code)

  # Colors
  resp_colors <- setNames(attrmap_resp$fillcolor, attrmap_resp$value)
  # Ensure white is visible with border
  # Build plot
  p <- ggplot() +
    # walls
    geom_polygon(data=walls_df, aes(x=x, y=y, group=id), fill="grey90", colour="grey80", alpha=0.6) +
    # Y grid lines
    geom_line(data=yaxis_df, aes(x=x, y=y, group=Lid), colour="grey70", linewidth=0.3) +
    # Duration bars (light blue)
    geom_polygon(data=dur_df, aes(x=x, y=y, group=pid), fill="#a0c8f0", colour="black", linewidth=0.2, alpha=0.9) +
    # Response bars colored by Drug
    geom_polygon(data=resp_df, aes(x=x, y=y, group=pid, fill=DrugLabel), colour="black", linewidth=0.2, alpha=0.95) +
    scale_fill_manual(values=resp_colors, name="CC-122", na.translate=FALSE) +
    # Dropped diamonds
    geom_polygon(data=drop_df, aes(x=x, y=y, group=pid), fill="gold", colour="black", linewidth=0.2) +
    # Axis lines
    geom_segment(data=axes_df, aes(x=x, y=y, xend=xend, yend=yend, linetype=wgroup), colour="black", linewidth=0.4, show.legend=FALSE) +
    scale_linetype_manual(values=c(D="solid", L="dashed")) +
    # Y tick values
    geom_text(data=ytick_df, aes(x=x, y=y, label=yTickVal), size=2, hjust=0, vjust=0.5) +
    # Z tick values
    geom_text(data=ztick_df, aes(x=x, y=y, label=zTickVal), size=2, hjust=1, vjust=0.5) +
    geom_point(data=ztick_df, aes(x=x, y=y), size=0.5) +
    # Code labels rotated 90
    geom_text(data=code_df, aes(x=x, y=y, label=Code), size=2, angle=90, hjust=0, vjust=0.5) +
    # Axis labels
    geom_text(data=label_df %>% filter(label==1), aes(x=x, y=y, label=label_text), size=2.5, angle=xaxisRot, hjust=0.5, vjust=1) +
    geom_text(data=label_df %>% filter(label==2), aes(x=x, y=y, label=label_text), size=2.5, angle=yaxisRot, hjust=0.5, vjust=1) +
    geom_text(data=label_df %>% filter(label==3), aes(x=x, y=y, label=label_text), size=2.5, angle=90, hjust=0.5) +
    labs(title=title, subtitle=paste0("X-Rotation=",Tilt,"  Y-Rotation=0  Z-Rotation=",Rotate,"  xFac=",xFac)) +
    theme_void() +
    theme(plot.title=element_text(size=10, hjust=0.5, face="bold"),
          plot.subtitle=element_text(size=5, hjust=0, face="italic"),
          legend.position=c(0.85,0.15), legend.background=element_rect(fill="white", colour="black"),
          legend.title=element_text(size=5), legend.text=element_text(size=5),
          plot.margin=margin(10,10,10,10)) +
    coord_equal()

  list(plot=p, projected=projected, walls=pw, polygons=polygons, xaxisRot=xaxisRot, yaxisRot=yaxisRot)
}

# Generate three figures
for (xf in c(1.0, 1.25, 1.5)) {
  res <- plot_waterfall(xFac=xf, Tilt=75, Rotate=-45, title="Tumor Response and Duration")
  suffix <- sprintf("%03d", as.integer(xf*100))
  # e.g. 100,125,150
  png_name <- paste0("waterfall_xf_", suffix, ".png")
  csv_name <- paste0("waterfall_xf_", suffix, ".csv")
  ggsave(file.path(out, png_name), plot=res$plot, width=4, height=4, units="in", dpi=300)
  # CSV of plotted data: projected coordinates plus original tumor sorted
  # Combine projected data with tumor info for traceability
  write.csv(res$projected, file.path(out, csv_name), row.names=FALSE, na="")
  # Also write tumor sorted for reference
  # Ensure at least one CSV per figure exists (already done)
}

# Also write tumor data as listing/table equivalent
write.csv(tumorsorted, file.path(out, "tumorsorted.csv"), row.names=FALSE, na="")
# Write a text summary
txt <- capture.output({
  cat("Tumor data (sorted by descending Response)\n")
  print(tumorsorted)
})
writeLines(txt, file.path(out, "tumorsorted.txt"))
