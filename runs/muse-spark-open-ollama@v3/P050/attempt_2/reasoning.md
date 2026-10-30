Need to fix sas_put_wd to be vectorized. The error is because is.na(x) with vector length >1. Need to handle vector.

Also need to ensure script writes disp.csv.

We need to provide complete corrected script.

We should fix sas_put_wd to handle vector input.

Implement vectorized version:

sas_put_wd <- function(x, w, d) {
  sapply(seq_along(x), function(i) {
    xi <- x[i]
    if (is.na(xi)) return(strrep(" ", w))
    rx <- round_half_up(xi, d)
    fmt <- sprintf(paste0("%.", d, "f"), rx) if d>0 else sprintf("%.0f", rx)
    if (nchar(fmt, type="bytes") < w) paste0(strrep(" ", w - nchar(fmt, type="bytes")), fmt) else fmt
  })
}

But sapply returns vector.

Also need to ensure round_half_up vectorized? Currently it handles scalar. Make vectorized.

Define round_half_up vectorized:

round_half_up <- function(x, digits=0) {
  mult <- 10^digits
  sign_x <- sign(x)
  abs_x <- abs(x)
  rounded_abs <- floor(abs_x * mult + 0.5 + 1e-9) / mult
  result <- sign_x * rounded_abs
  result[is.na(x)] <- NA_real_
  result
}

Also sas_put_wd should be vectorized.

Also need to ensure mutate works.

Also need to handle other potential issues.

We should output complete script with fixes.

We should keep previous logic but fix functions.

Also ensure out dir handling.

Provide final script.

We must ensure we read adsl correctly.

We should make sas_put_wd vectorized properly.

Implement:

sas_put_wd <- function(x, w, d) {
  vapply(x, function(xi) {
    if (is.na(xi)) return(strrep(" ", w))
    rx <- round_half_up(xi, d)
    # rx is scalar
    if (d == 0) {
      fmt <- sprintf("%.0f", rx)
    } else {
      fmt <- sprintf(paste0("%.", d, "f"), rx)
    }
    if (nchar(fmt, type="bytes") < w) {
      paste0(strrep(" ", w - nchar(fmt, type="bytes")), fmt)
    } else {
      fmt
    }
  }, character(1))
}

But round_half_up now vectorized, but inside we call with scalar xi, okay.

Alternatively make sas_put_wd handle vector via Vectorize.

Simpler: use Vectorize.

We should also ensure that per formatting matches SAS: put(per,5.1) => width 5, 1 decimal. For per values like 100, it would be "100.0" width 5 => "100.0" (5 chars) no pad. For 50, " 50.0" etc.

Our implementation does that.

Also cnt formatting: put(cnt,3.) => width 3, 0 decimals. For cnt=86 => " 86" (width 3). Good.

We need to ensure we produce nper exactly as SAS: put(cnt,3.) || "(" || put(per,5.1) || ")"

Our code does paste0(sas_put_wd(cnt,3,0), "(", sas_put_wd(per,5,1), ")")

That yields e.g., " 86(100.0)"? Wait SAS would have spaces: cnt formatted width 3, per width 5. So example: cnt=86 => " 86", per=100.0 => "100.0", combined => " 86(100.0)". That's correct.

Check SAS example: nper = put(cnt, 3.) || "(" || put(per, 5.1) || ")"; So no extra spaces beyond format widths.

We should ensure we replicate.

Also need to ensure we handle vectorization.

We should provide complete script.

We should also ensure we don't have other errors.

We should keep library loading.

We should ensure we handle adsl path finding robustly.

We should ensure we write disp.csv with correct columns.

We should test mentally.

We should also ensure we handle haven labelled conversion.

We should provide final code block.

We must ensure we don't call install.packages.

We should ensure we use haven::read_xpt.

We should ensure we handle out dir.

We should provide complete corrected script.

We should also ensure we handle the case where adsl_path not found: fallback.

We should keep previous script but fix functions.

We should output one r code block.

