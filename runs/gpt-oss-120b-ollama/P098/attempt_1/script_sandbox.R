# WaterFall_3D_Macro translation -------------------------------------------------
# This script reproduces the SAS program P098_WaterFall_3D_Macro.sas.
# It creates the tumor data, builds the 3‑D projection, and generates three PNG
# figures (Waterfall_Xf_100, Waterfall_Xf_125, Waterfall_Xf_150) together with the
# CSV files that contain the plotted data.
# ------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(ggplot2)
library(data.table)

out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")

# ----------------------------------------------------------------------
# Helper functions (matrix operations, SAS‑like rounding)
# ----------------------------------------------------------------------
mat_mult <- function(A, B) {
  # matrix multiplication, works for 4x4 matrices
  A %*% B
}
mat_ident <- function() {
  diag(1, 4, 4)
}
# ----------------------------------------------------------------------
# 1. Create the tumor data (same logic as SAS)
# ----------------------------------------------------------------------
set.seed(3)   # SAS ran ranuni(3) – we approximate with R's runif and seed 3
n <- 20
tumor <- tibble(i = 1:n) %>%
  mutate(
    Response = 100 * (runif(n) - 0.7),
    Duration = 50 * runif(n),
    Dropped = ifelse(runif(n) > 0.8, Duration - 5, NA_real_),
    Drug = floor(6 * runif(n)),
    Code = ifelse(runif(n) > 0.7, "FL", "DLBCL"),
    codeloc = ifelse(Response > 0, 0, Response),
    baseline = 20 + 180 * runif(2)[1]   # SAS used ranuni(2) once per row – we mimic with a single draw
  ) %>%
  select(-i)

# ----------------------------------------------------------------------
# 2. Attribute map (used for fill colours of the response groups)
# ----------------------------------------------------------------------
attrmap <- tribble(
  ~ID,   ~value,      ~fillcolor, ~show,
  "Resp","1.0 mg",    "white",    "Attrmap",
  "Resp","2.0 mg",    "cxffffc0", "Attrmap",
  "Resp","3.0 mg",    "cxf0d0a0", "Attrmap",
  "Resp","4.0 mg",    "orange",   "Attrmap",
  "Resp","3.0 F6 mg", "cxff7f00","Attrmap",
  "Resp","4.0 F6 mg", "red",      "Attrmap"
) %>%
  mutate(fillcolor = str_replace(fillcolor, "^cx", "#"))   # SAS colour syntax -> hex

# ----------------------------------------------------------------------
# 3. Core macro implementation -------------------------------------------
# ----------------------------------------------------------------------
waterfall_macro <- function(data, Duration, Response, Dropped, Group, Code,
                           AttrMap, Lblx = "X", Lbly = "Y", Lblz = "Z",
                           xFac = 1, Tilt = 75, Rotate = -45,
                           Title = NULL, img_name) {
  # --------------------------------------------------------------------
  # Parameter checks (mirroring SAS macro)
  # --------------------------------------------------------------------
  if (missing(data) || missing(Duration) || missing(Response) ||
      missing(Dropped) || missing(Group) || missing(Code))
    stop("Required parameters missing")
  # enforce limits
  xFac <- min(max(xFac, 0.5), 4)
  Tilt <- min(max(Tilt, 60), 80)
  Rotate <- ifelse(Rotate > -15, -15,
                   ifelse(Rotate < -75, -75, Rotate))

  # --------------------------------------------------------------------
  # 3.1 Build Wall_Axes data set (unit space)
  # --------------------------------------------------------------------
  dx <- xFac
  wall_axes <- tibble(
    id = c('X1-Axis','X3-Axis','X4-Axis','Y2-Axis','Y3-Axis','Y4-Axis',
           'Z1-Axis','Z2-Axis','Z4-Axis',
           rep('Back',4), rep('Right',4), rep('Bottom',4),
           rep('Center',4)),
    wgroup = c('D','L','D','D','D','L','D','L','D',
               rep('D',4), rep('D',4), rep('D',4), rep('D',4)),
    xw = c(-dx, -dx, -dx, -dx,  dx,  dx, -dx,  dx,  dx,
           -dx, -dx, -dx, -dx,
           -dx,  dx,  dx, -dx),
    yw = c(-1, -1,  1, -1, -1, -1, -1, -1,  1,
           -1,  1,  1, -1,
           -1, -1,  1,  1),
    zw = c(-1,  1,  1,  1, -1,  1, -1, -1, -1,
           -1, -1,  1,  1,
           -0, -0, -0, -0)
  ) %>%
    mutate(
      xw2 = NA_real_, yw2 = NA_real_, zw2 = NA_real_,
      xl = NA_real_, yl = NA_real_, zl = NA_real_,
      label = NA_real_
    )
  # fill the explicit rows that SAS writes after the CALL MISSING
  wall_axes <- bind_rows(
    wall_axes,
    tibble(
      id = "Back", wgroup = "D", xw = -dx, yw = -1, zw = -1,
      xw2 = NA, yw2 = NA, zw2 = NA, xl = NA, yl = NA, zl = NA, label = NA
    ),
    tibble(
      id = "Back", wgroup = "D", xw = -dx, yw =  1, zw = -1,
      xw2 = NA, yw2 = NA, zw2 = NA, xl = NA, yl = NA, zl = NA, label = NA
    ),
    tibble(
      id = "Back", wgroup = "D", xw = -dx, yw =  1, zw =  1,
      xw2 = NA, yw2 = NA, zw2 = NA, xl = NA, yl = NA, zl = NA, label = NA
    ),
    tibble(
      id = "Back", wgroup = "D", xw = -dx, yw = -1, zw =  1,
      xw2 = NA, yw2 = NA, zw2 = NA, xl = NA, yl = NA, zl = NA, label = NA
    ),
    tibble(
      id = "Right", wgroup = "D", xw = -dx, yw =  1, zw = -1,
      xw2 = NA, yw2 = NA, zw2 = NA, xl = NA, yl = NA, zl = NA, label = NA
    ),
    tibble(
      id = "Right", wgroup = "D", xw =  dx, yw =  1, zw = -1,
      xw2 = NA, yw2 = NA, zw2 = NA, xl = NA, yl = NA, zl = NA, label = NA
    ),
    tibble(
      id = "Right", wgroup = "D", xw =  dx, yw =  1, zw =  1,
      xw2 = NA, yw2 = NA, zw2 = NA, xl = NA, yl = NA, zl = NA, label = NA
    ),
    tibble(
      id = "Right", wgroup = "D", xw = -dx, yw =  1, zw =  1,
      xw2 = NA, yw2 = NA, zw2 = NA, xl = NA, yl = NA, zl = NA, label = NA
    ),
    tibble(
      id = "Bottom", wgroup = "D", xw = -dx, yw = -1, zw = -1,
      xw2 = NA, yw2 = NA, zw2 = NA, xl = NA, yl = NA, zl = NA, label = NA
    ),
    tibble(
      id = "Bottom", wgroup = "D", xw =  dx, yw = -1, zw = -1,
      xw2 = NA, yw2 = NA, zw2 = NA, xl = NA, yl = NA, zl = NA, label = NA
    ),
    tibble(
      id = "Bottom", wgroup = "D", xw =  dx, yw =  1, zw = -1,
      xw2 = NA, yw2 = NA, zw2 = NA, xl = NA, yl = NA, zl = NA, label = NA
    ),
    tibble(
      id = "Bottom", wgroup = "D", xw = -dx, yw =  1, zw = -1,
      xw2 = NA, yw2 = NA, zw2 = NA, xl = NA, yl = NA, zl = NA, label = NA
    ),
    tibble(
      id = "Center", wgroup = "D", xw = -dx, yw = -1, zw = -0,
      xw2 = NA, yw2 = NA, zw2 = NA, xl = NA, yl = NA, zl = NA, label = NA
    ),
    tibble(
      id = "Center", wgroup = "D", xw =  dx, yw = -1, zw = -0,
      xw2 = NA, yw2 = NA, zw2 = NA, xl = NA, yl = NA, zl = NA, label = NA
    ),
    tibble(
      id = "Center", wgroup = "D", xw =  dx, yw =  1, zw = -0,
      xw2 = NA, yw2 = NA, zw2 = NA, xl = NA, yl = NA, zl = NA, label = NA
    ),
    tibble(
      id = "Center", wgroup = "D", xw = -dx, yw =  1, zw = -0,
      xw2 = NA, yw2 = NA, zw2 = NA, xl = NA, yl = NA, zl = NA, label = NA
    )
  ) %>% distinct()

  # --------------------------------------------------------------------
  # 3.2 Projection matrices (same as SAS)
  # --------------------------------------------------------------------
  pi <- base::pi
  fac <- pi / 180
  A <- Tilt * fac
  B <- 0 * fac          # SAS sets &B=0
  C <- Rotate * fac

  # projection matrix
  r <- 1; t <- 1; f <- 1; n <- -1
  m <- matrix(0, 4, 4)
  m[1,1] <- 1/r; m[2,2] <- 1/t; m[3,3] <- -2/(f-n); m[3,4] <- -(f+n)/(f-n); m[4,4] <- 1

  # rotation matrices
  rx <- diag(4)
  rx[2,2] <- cos(A); rx[2,3] <- -sin(A)
  rx[3,2] <- sin(A); rx[3,3] <-  cos(A)

  ry <- diag(4)
  ry[1,1] <- cos(B); ry[1,3] <- sin(B)
  ry[3,1] <- -sin(B); ry[3,3] <- cos(B)

  rz <- diag(4)
  rz[1,1] <- cos(C); rz[1,2] <- -sin(C)
  rz[2,1] <- sin(C); rz[2,2] <-  cos(C)

  # final view matrix
  w_mat <- rx %*% ry %*% rz %*% m   # note order matches SAS (MatMult calls)

  # --------------------------------------------------------------------
  # 3.3 Transform walls and axes
  # --------------------------------------------------------------------
  projected_walls <- wall_axes %>%
    rowwise() %>%
    mutate(
      # transform wall points
      d = list(c(xw, yw, zw, 1)),
      p = list(w_mat %*% d),
      xw = p[[1]][1], yw = p[[1]][2], zw = p[[1]][3],
      # transform secondary points (xw2,yw2,zw2) if not missing
      d2 = list(c(xw2, yw2, zw2, 1)),
      p2 = list(w_mat %*% d2),
      xw2 = p2[[1]][1], yw2 = p2[[1]][2], zw2 = p2[[1]][3],
      # transform label positions
      dl = list(c(xl, yl, zl, 1)),
      pl = list(w_mat %*% dl),
      xl = pl[[1]][1], yl = pl[[1]][2], zl = pl[[1]][3],
      # axis labels
      lbx = ifelse(label == 1, Lblx, NA_character_),
      lby = ifelse(label == 2, Lbly, NA_character_),
      lbz = ifelse(label == 3, Lblz, NA_character_)
    ) %>%
    ungroup() %>%
    select(-d, -p, -d2, -p2, -dl, -pl)

  # --------------------------------------------------------------------
  # 3.4 Compute axis rotation angles (SAS stores them in macro vars)
  # --------------------------------------------------------------------
  fac_sqrt <- sqrt(xFac)
  xaxisRot <- projected_walls %>%
    filter(id == "X1-Axis") %>%
    pull({
      dx <- xw2 - xw
      if (abs(dx) > 0.001) fac_sqrt * 180 * atan((yw2 - yw) / dx) / 3.1415 else 90
    })
  yaxisRot <- projected_walls %>%
    filter(id == "Y3-Axis") %>%
    pull({
      dx <- xw2 - xw
      if (abs(dx) > 0.001) fac_sqrt * 180 * atan((yw2 - yw) / dx) / 3.1415 else 90
    })
  xaxisRot <- ifelse(length(xaxisRot) == 0, 0, xaxisRot)
  yaxisRot <- ifelse(length(yaxisRot) == 0, 0, yaxisRot)

  # --------------------------------------------------------------------
  # 3.5 Determine min/max of Duration and Response in the original data
  # --------------------------------------------------------------------
  DurMin <- min(data[[Duration]], na.rm = TRUE)
  DurMax <- max(data[[Duration]], na.rm = TRUE)
  ResMin <- min(data[[Response]], na.rm = TRUE)
  ResMax <- max(data[[Response]], na.rm = TRUE)

  # --------------------------------------------------------------------
  # 3.6 Build the polygons data set (unscaled unit coordinates)
  # --------------------------------------------------------------------
  durfac <- 1.05
  resfac <- 1.05
  aResMax <- resfac * max(abs(ResMax), abs(ResMin))
  aDurMax <- durfac * max(abs(DurMax), abs(DurMin))

  polygons <- data %>%
    mutate(i = row_number() - 1) %>%   # i = _n_-1 as in SAS
    rowwise() %>%
    do({
      row <- .
      i <- row$i
      dx <- xFac
      # Duration bar (type='Duration')
      dur_bar <- tibble(
        type = "Duration", pid = NA_integer_, x = c(dx*(i-0.5-0.4), dx*(i-0.5), dx*(i-0.5+0.4), dx*(i-0.5)),
        y = c(0, row[[Duration]], row[[Duration]], 0),
        z = 0,
        Lid = NA_integer_, code = NA_character_, yTickVal = NA_real_, zTickVal = NA_real_
      )
      # Response bar (type='Response')
      resp_bar <- tibble(
        type = "Response", pid = NA_integer_, x = c(dx*(i-0.5-0.4), dx*(i-0.5), dx*(i-0.5+0.4), dx*(i-0.5)),
        y = 0,
        z = c(0, row[[Response]], row[[Response]], 0),
        Lid = NA_integer_, code = NA_character_, yTickVal = NA_real_, zTickVal = NA_real_
      )
      # Dropped diamond (if Dropped not missing)
      drop_poly <- if (!is.na(row[[Dropped]])) {
        tibble(
          type = "Dropped", pid = NA_integer_,
          x = c(dx*(i-0.5-0.3), dx*(i-0.5), dx*(i-0.5+0.3), dx*(i-0.5)),
          y = c(row[[Duration]]-0.03*DurMax,
                row[[Duration]],
                row[[Duration]]-0.03*DurMax,
                row[[Duration]]-0.06*DurMax),
          z = 0,
          Lid = NA_integer_, code = NA_character_, yTickVal = NA_real_, zTickVal = NA_real_
        )
      } else tibble()
      # Code label (type='Code')
      code_pt <- tibble(
        type = "Code", pid = NA_integer_,
        x = dx*(i-0.5),
        y = 0,
        z = ifelse(row[[Response]] < 0, row[[Response]], 0),
        Lid = NA_integer_, code = row[[Code]], yTickVal = NA_real_, zTickVal = NA_real_
      )
      bind_rows(dur_bar, resp_bar, drop_poly, code_pt)
    }) %>%
    ungroup() %>%
    mutate(pid = row_number()) %>%   # assign a unique pid per polygon row (approx)
    # add extra polygons for aspect scaling and axis grids (last row only)
    bind_rows(
      # aspect polygons
      tibble(
        type = "Response", pid = NA_integer_, x = c(-dx, -dx), y = 0, z = c(-aResMax, aResMax),
        Lid = NA_integer_, code = NA_character_, yTickVal = NA_real_, zTickVal = NA_real_
      ),
      tibble(
        type = "Duration", pid = NA_integer_, x = c(-dx, -dx), y = 0, z = c(aResMax, aResMax),
        Lid = NA_integer_, code = NA_character_, yTickVal = NA_real_, zTickVal = NA_real_
      ),
      # Y‑axis grid lines
      {
        y_vals <- seq(0, aDurMax, by = 10)
        map_dfr(y_vals, ~ tibble(
          type = "Y-Axis", pid = NA_integer_, x = c(-dx, dx * (max(i)+1)), y = .x, z = 0,
          Lid = NA_integer_, code = NA_character_, yTickVal = NA_real_, zTickVal = NA_real_
        ))
      },
      # Y‑axis tick values
      {
        y_vals <- seq(0, aDurMax, by = 10)
        map_dfr(y_vals, ~ tibble(
          type = "Y-Values", pid = NA_integer_, x = dx * (max(i)+1), y = .x, z = 0,
          Lid = NA_integer_, code = NA_character_, yTickVal = .x, zTickVal = NA_real_
        ))
      },
      # Z‑axis tick values
      {
        z_vals_pos <- seq(0, aResMax, by = 10)
        z_vals_neg <- seq(-10, -aResMax, by = -10)
        bind_rows(
          tibble(type = "Z-Values", pid = NA_integer_, x = -dx, y = 0, z = z_vals_pos,
                Lid = NA_integer_, code = NA_character_, yTickVal = NA_real_, zTickVal = z_vals_pos),
          tibble(type = "Z-Values", pid = NA_integer_, x = -dx, y = 0, z = z_vals_neg,
                Lid = NA_integer_, code = NA_character_, yTickVal = NA_real_, zTickVal = z_vals_neg)
        )
      }
    ) %>%
    mutate(
      !!Group := data[[Group]][(pid %% nrow(data)) + 1]   # map Group back to each polygon row
    )

  # --------------------------------------------------------------------
  # 3.7 Normalise coordinates to -1..+1 range (as SAS does)
  # --------------------------------------------------------------------
  ranges <- polygons %>%
    summarise(
      xmin = min(x, na.rm = TRUE), xmax = max(x, na.rm = TRUE),
      ymin = min(y, na.rm = TRUE), ymax = max(y, na.rm = TRUE),
      zmin = min(z, na.rm = TRUE), zmax = max(z, na.rm = TRUE)
    )
  xrange <- ranges$xmax - ranges$xmin
  yrange <- ranges$ymax - ranges$ymin
  zrange <- ranges$zmax - ranges$zmin

  normalized <- polygons %>%
    mutate(
      x = 2 * xFac * (x - ranges$xmin) / xrange - xFac,
      y = 2 * (y - ranges$ymin) / yrange - 1,
      z = 2 * (z - ranges$zmin) / zrange - 1
    )

  # --------------------------------------------------------------------
  # 3.8 Project the normalised data (same matrix as for walls)
  # --------------------------------------------------------------------
  project_one <- function(df) {
    d <- c(df$x, df$y, df$z, 1)
    p <- w_mat %*% d
    as.list(p[1:3])
  }

  projected_data <- normalized %>%
    rowwise() %>%
    mutate(
      proj = list(project_one(cur_data())),
      xd = ifelse(type == "Duration", proj[[1]], NA_real_),
      yd = ifelse(type == "Duration", proj[[2]], NA_real_),
      zd = ifelse(type == "Duration", proj[[3]], NA_real_),
      xr = ifelse(type == "Response", proj[[1]], NA_real_),
      yr = ifelse(type == "Response", proj[[2]], NA_real_),
      zr = ifelse(type == "Response", proj[[3]], NA_real_),
      xc = ifelse(type == "Dropped", proj[[1]], NA_real_),
      yc = ifelse(type == "Dropped", proj[[2]], NA_real_),
      zc = ifelse(type == "Dropped", proj[[3]], NA_real_),
      xcode = ifelse(type == "Code", proj[[1]], NA_real_),
      ycode = ifelse(type == "Code", proj[[2]], NA_real_),
      zcode = ifelse(type == "Code", proj[[3]], NA_real_),
      xAy = ifelse(type == "Y-Axis", proj[[1]], NA_real_),
      yAy = ifelse(type == "Y-Axis", proj[[2]], NA_real_),
      zAy = ifelse(type == "Y-Axis", proj[[3]], NA_real_),
      xYv = ifelse(type == "Y-Values", proj[[1]], NA_real_),
      yYv = ifelse(type == "Y-Values", proj[[2]], NA_real_),
      zYv = ifelse(type == "Y-Values", proj[[3]], NA_real_),
      xZv = ifelse(type == "Z-Values", proj[[1]], NA_real_),
      yZv = ifelse(type == "Z-Values", proj[[2]], NA_real_),
      zZv = ifelse(type == "Z-Values", proj[[3]], NA_real_)
    ) %>%
    ungroup() %>%
    select(-proj)

  # --------------------------------------------------------------------
  # 3.9 Combine walls and projected data
  # --------------------------------------------------------------------
  combined <- bind_rows(
    projected_walls %>% rename(x = xw, y = yw, z = zw),
    projected_data %>%
      select(
        !!Group, pid, type, x = xd, y = yd, z = zd,
        xr, yr, zr,
        xc, yc, zc,
        xcode, ycode, zcode,
        xAy, yAy, zAy,
        xYv, yYv, zYv,
        xZv, yZv, zZv,
        Lid, yTickVal, zTickVal
      )
  )

  # --------------------------------------------------------------------
  # 4. Write the combined data set (CSV) ---------------------------------
  # --------------------------------------------------------------------
  csv_name <- file.path(out_dir, paste0(tolower(img_name), ".csv"))
  write.csv(combined, csv_name, row.names = FALSE, na = "")

  # --------------------------------------------------------------------
  # 5. Build the plot (ggplot2) -------------------------------------------
  # --------------------------------------------------------------------
  # Helper to get fill colour for response groups
  resp_fill <- setNames(attrmap$fillcolor, attrmap$value)

  # Base plot
  p <- ggplot() +
    # walls (light gray)
    geom_polygon(
      data = combined %>% filter(!is.na(id)),
      aes(x = x, y = y, group = id),
      fill = "lightgray", colour = "lightgray", alpha = 0.5
    ) +
    # Y‑axis grid lines
    geom_path(
      data = combined %>% filter(type == "Y-Axis"),
      aes(x = xAy, y = yAy, group = Lid),
      colour = "#b0b0b0"
    ) +
    # Duration bars
    geom_polygon(
      data = combined %>% filter(type == "Duration"),
      aes(x = xd, y = yd, group = pid),
      fill = "#1f77b4", colour = "black"
    ) +
    # Dropped diamonds
    geom_polygon(
      data = combined %>% filter(type == "Dropped"),
      aes(x = xc, y = yc, group = pid),
      fill = "gold", colour = "black"
    ) +
    # Response bars (coloured by Drug group via attrmap)
    geom_polygon(
      data = combined %>% filter(type == "Response"),
      aes(x = xr, y = yr, group = pid, fill = as.character(!!sym(Group))),
      colour = "black", alpha = 0.8
    ) +
    scale_fill_manual(values = resp_fill, name = "CC-122") +
    # Axes vectors
    geom_segment(
      data = combined %>% filter(!is.na(wgroup)),
      aes(x = x, y = y, xend = xw2, yend = yw2, colour = wgroup),
      arrow = arrow(length = unit(0, "mm"))
    ) +
    # Axis labels
    geom_text(
      data = combined %>% filter(!is.na(lbx)),
      aes(x = xl, y = yl, label = lbx),
      hjust = 0, vjust = 1, angle = xaxisRot, size = 3
    ) +
    geom_text(
      data = combined %>% filter(!is.na(lby)),
      aes(x = xl, y = yl, label = lby),
      hjust = 0.5, vjust = 1, angle = yaxisRot, size = 3
    ) +
    geom_text(
      data = combined %>% filter(!is.na(lbz)),
      aes(x = xl, y = yl, label = lbz),
      hjust = 0, vjust = 0.5, angle = 90, size = 3
    ) +
    # Y‑axis tick values
    geom_text(
      data = combined %>% filter(type == "Y-Values"),
      aes(x = xYv, y = yYv, label = yTickVal),
      hjust = 1, vjust = 0.5, size = 2.5
    ) +
    # Z‑axis tick values
    geom_text(
      data = combined %>% filter(type == "Z-Values"),
      aes(x = xZv, y = yZv, label = zTickVal),
      hjust = 1, vjust = 0.5, size = 2.5
    ) +
    # Code labels
    geom_text(
      data = combined %>% filter(type == "Code"),
      aes(x = xcode, y = ycode, label = code),
      angle = 90, hjust = 1, size = 2.5
    ) +
    labs(title = Title) +
    theme_void() +
    theme(
      plot.title = element_text(size = 12, hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(size = 5),
      legend.text = element_text(size = 5)
    )

  # --------------------------------------------------------------------
  # 6. Save the figure
  # --------------------------------------------------------------------
  png_path <- file.path(out_dir, paste0(tolower(img_name), ".png"))
  ggsave(png_path, plot = p, width = 4, height = 4, dpi = 300, units = "in")
}

# ----------------------------------------------------------------------
# Run the macro three times with the required scale factors
# ----------------------------------------------------------------------
waterfall_macro(
  data = tumor,
  Duration = "Duration",
  Response = "Response",
  Dropped = "Dropped",
  Group = "Drug",
  Code = "Code",
  AttrMap = attrmap,
  Lblx = "Subject",
  Lbly = "Duration of Treatment in Days",
  Lblz = "Tumor Lesion Best Change",
  xFac = 1.0,
  Tilt = 75,
  Rotate = -45,
  Title = "Tumor Response and Duration",
  img_name = "Waterfall_Xf_100"
)

waterfall_macro(
  data = tumor,
  Duration = "Duration",
  Response = "Response",
  Dropped = "Dropped",
  Group = "Drug",
  Code = "Code",
  AttrMap = attrmap,
  Lblx = "Subject",
  Lbly = "Duration of Treatment in Days",
  Lblz = "Tumor Lesion Best Change",
  xFac = 1.25,
  Tilt = 75,
  Rotate = -45,
  Title = "Tumor Response and Duration",
  img_name = "Waterfall_Xf_125"
)

waterfall_macro(
  data = tumor,
  Duration = "Duration",
  Response = "Response",
  Dropped = "Dropped",
  Group = "Drug",
  Code = "Code",
  AttrMap = attrmap,
  Lblx = "Subject",
  Lbly = "Duration of Treatment in Days",
  Lblz = "Tumor Lesion Best Change",
  xFac = 1.5,
  Tilt = 75,
  Rotate = -45,
  Title = "Tumor Response and Duration",
  img_name = "Waterfall_Xf_150"
)

# End of script -------------------------------------------------------------
