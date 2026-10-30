Need to translate SAS program P031 Boxplot group.

We need to produce R script that reproduces outputs exactly.

SAS program:

DATA X;
LENGTH TRT $10.;
DO TRT="Study Drug","Placebo","AC";
DO J=1 TO 10;
SUBJID=J+100*(TRT="Study Drug")+200*(TRT="Placebo")+300*(TRT="AC");
DO VISIT=3 TO 6;
X=ROUND(J+2*NORMAL(777),0.1);
OUTPUT;
END;
END;
END;
DROP J;
RUN;

DATA XX;
SET X;
IF TRT="Study Drug" THEN VIS=VISIT;
ELSE IF TRT="Placebo" THEN VIS=VISIT+0.15;
ELSE IF TRT="AC" THEN VIS=VISIT+0.30;
IF VISIT IN (4,5) THEN VIS=VIS-0.15;
IF VISIT=6 THEN VIS=VIS-0.3;
RUN;

PROC SORT DATA=XX; BY VIS; RUN;

ODS RTF FILE="&outtlf.\P031_f_plsr_box_plot.rtf";
GOPTIONS RESET=ALL HSIZE=9 IN VSIZE=7 IN DEV=SASEMF;
SYMBOL1 V=PLUS C=BLACK;
SYMBOL2 V=TRIANGLE C=BLUE;
SYMBOL3 V=SQUARE C=RED;
AXIS LABEL=("Visit") ORDER=(3 TO 6 BY 1) MAJOR=NONE MINOR=NONE OFFSET=(2,2);
TITLE1 "Box Plot of X over Time by Treatment";
PROC BOXPLOT DATA=XX;
FORMAT VIS 4.;
PLOT X*vis=TRT/ HAXIS=AXIS1 BOXSTYLE=SCHEMATICID CBOX=BLUE IDSYMBOL=STAR IDCOLOR=RED ;
RUN; QUIT;
ODS RTF CLOSE;

So need to replicate random generation: SAS NORMAL(777) with seed 777. SAS NORMAL uses? In SAS, RANNOR? NORMAL(seed) generates standard normal. With seed 777. Need to replicate exactly? The SAS program uses DO loops: TRT order: "Study Drug","Placebo","AC". For each TRT, J 1 to 10, VISIT 3 to 6. So total 3*10*4=120 observations. X = ROUND(J+2*NORMAL(777),0.1). NORMAL(777) uses seed 777, but SAS's RNG is sequential: each call advances. So we need to replicate SAS RNG.

We need to understand SAS NORMAL RNG algorithm. SAS uses? For older SAS, NORMAL uses Box-Muller? Need to replicate exactly to get same X values. However requirement says reproduce SAS outputs exactly. But we cannot exactly replicate SAS RNG without implementing SAS's algorithm. Alternative: we can approximate using R's rnorm with same seed? But values will differ. However the task says to produce a single complete standalone R script that reproduces SAS program's outputs exactly. For this program, the data is generated synthetically, not from input files. So we need to emulate SAS's NORMAL(777).

We need to figure SAS NORMAL implementation. SAS documentation: NORMAL(seed) uses? In SAS 9, RANUNI and RANNOR? Actually NORMAL function is deprecated, uses same as RANNOR. RANNOR uses? Let's search memory: SAS RANNOR uses Box-Muller transformation with RANUNI uniform generator. RANUNI uses Fishman-Moore? Or Mersenne Twister? Need to check.

SAS's RANUNI: uses prime modulus multiplicative congruential generator? For SAS 9.4, RANUNI uses? Might be using the same as RAND('UNIFORM') with older stream.

We could attempt to implement SAS's RNG: SAS uses seed handling: if seed <=0 then uses clock, if seed >0 then uses that seed. The algorithm for RANUNI: x_{n+1} = (a * x_n) mod m, where a=397204094, m=2^31-1 =2147483647. Then uniform = x / m. That's the Fishman-Moore generator.

Then RANNOR (NORMAL) uses Box-Muller: generate two uniforms U1, U2, then Z0 = sqrt(-2*ln(U1))*cos(2*pi*U2), Z1 = sqrt(-2*ln(U1))*sin(2*pi*U2). SAS's RANNOR returns one at a time, alternating? Actually RANNOR generates two normals per two uniforms, stores one for next call.

We need to verify.

Let's attempt to implement that and see if we can get values.

We need to replicate ROUND(J+2*NORMAL(777),0.1) with SAS rounding: ROUND(x,0.1) rounds to nearest 0.1, half away from zero? SAS ROUND uses? ROUND(x, unit) rounds to nearest multiple of unit. Need to implement round half away from zero.

We also need to replicate VIS calculation.

Then boxplot: PROC BOXPLOT with BOXSTYLE=SCHEMATICID, CBOX=BLUE, IDSYMBOL=STAR IDCOLOR=RED. Need to understand what PROC BOXPLOT does: boxplot of X*VIS grouped by TRT. VIS is numeric with offsets: For Study Drug VIS=VISIT, Placebo VIS=VISIT+0.15, AC VIS=VISIT+0.30, then adjustments: if VISIT in (4,5) VIS=VIS-0.15, if VISIT=6 VIS=VIS-0.3. Let's compute:

- VISIT=3: Study Drug 3, Placebo 3.15, AC 3.30
- VISIT=4: Study Drug 4-0.15=3.85? Wait: Study Drug VIS=4 then minus 0.15 =>3.85. Placebo VIS=4.15-0.15=4.0, AC VIS=4.30-0.15=4.15
- VISIT=5: Study Drug 5-0.15=4.85, Placebo 5.15-0.15=5.0, AC 5.30-0.15=5.15
- VISIT=6: Study Drug 6-0.3=5.7? Wait Study Drug VIS=6 then minus 0.3 =>5.7, Placebo 6.15-0.3=5.85, AC 6.30-0.3=6.0

So VIS values are offset to separate boxes per visit.

FORMAT VIS 4. means display as integer? 4. format rounds? So axis label shows 3 to 6.

We need to produce boxplot figure PNG and CSV of plotted data.

We need to also produce table? Expected outputs says figure P031_f_plsr_box_plot.rtf -> write OUT_DIR/p031_f_plsr_box_plot.png and CSV of plotted data. So only figure.

We need to implement SAS boxplot statistics: BOXSTYLE=SCHEMATICID means? Schematic with identification of outliers? Need to check SAS PROC BOXPLOT defaults: BOXSTYLE=SCHEMATIC shows box from Q1 to Q3, median line, whiskers to farthest point within 1.5*IQR, outliers beyond plotted as symbols. SCHEMATICID also labels outliers with ID variable? But here no ID variable specified, so maybe just identifies outliers.

We need to reproduce boxplot exactly as SAS would: using SAS's quantile definition. SAS PROC BOXPLOT uses PCTLDEF=5 by default? Need to check. PROC BOXPLOT default PCTLDEF=5 (empirical distribution function with averaging). Let's verify: SAS PROC BOXPLOT and PROC UNIVARIATE default PCTLDEF=5. So quartiles computed with that definition.

We need to compute boxplot stats per TRT per VIS (or per VISIT?). Actually PLOT X*VIS=TRT means X on vertical, VIS on horizontal, grouped by TRT. So each VIS value is a separate box, colored by TRT? But TRT determines VIS offset, so each VIS corresponds to one TRT*VISIT combination. So 12 boxes.

We need to produce PNG that looks similar to SAS: title "Box Plot of X over Time by Treatment", x-axis "Visit" from 3 to 6, y-axis X.

We need to also produce CSV of plotted data: likely the XX dataset.

We need to implement SAS RNG to get exact X values. Let's attempt to implement SAS RANUNI and RANNOR.

Steps for SAS RNG:

- Seed handling: NORMAL(777) first call uses seed 777. The seed is updated each time. The algorithm for RANUNI: seed = mod(a*seed, m). Uniform = seed / m.

But for RANNOR, it uses two uniforms to generate two normals via Box-Muller. It returns one, saves the other for next call.

We need to confirm constants: a = 397204094, m = 2147483647. This is documented for SAS RANUNI.

Let's implement in R and test.

We also need to handle seed =0? Not needed.

Implement function sas_rannor(seed_vector) that generates sequence.

Pseudo:

sas_uniform <- function(seed) { seed <- (397204094 * seed) %% 2147483647; list(seed=seed, u=seed/2147483647) }

But need to handle large multiplication overflow: 397204094 * 2147483647 ~ 8.5e17 fits in 64-bit integer (9e18 max). R numeric double has 53-bit mantissa ~9e15 exact integer, so may lose precision. Use 64-bit integer via bit64 or use R's integer64? We can use R's double with modulo using careful? Better use R's ability to do modulo with double may still be approximate. We can implement using R's integer64 via bit64 package not allowed. Alternative use R's built-in? We can use gmp? Not allowed. We can implement using R's double with splitting: Use algorithm for modular multiplication without overflow: Use 64-bit via Rcpp? But we can use R's `as.numeric` with `%%` may still be okay for moderate values? Let's test mental: 397204094 * 2147483647 = 852... Let's compute: 397,204,094 * 2,147,483,647 ≈ 8.53e17. Double can represent integers exactly up to 2^53 ≈9.007e15, so 8.5e17 exceeds exact integer representation, so double will lose precision, causing wrong sequence.

We need alternative: Use R's `bignum`? Not available. We can implement modular multiplication using decomposition: a * seed mod m using 64-bit via R's `int64` using package `bit64` not in allowed list. But we have `data.table` maybe? Could use `Rmpfr` not allowed.

We can implement using R's `as.integer64` via `bit64` not allowed. However we can implement using base R's `double` with splitting technique: Use algorithm for (a*b) mod m using 128-bit splitting: Use `a = a1*2^16 + a0` etc.

Simpler: we can use R's `Rcpp` not allowed.

We can implement modular multiplication using iterative addition with doubling: Use binary exponentiation style: result =0; while b>0: if b%%2==1 result=(result+a)%%m; a=(a*2)%%m; b=floor(b/2). This uses only addition and doubling modulo m, which stays within <2*m <4e9 fits exactly in double. So we can compute (a*seed) mod m safely using loop over bits (31 bits). That's feasible for 120 iterations (120*31 ~3720 operations) trivial.

Let's implement function mod_mult(a,b,m) using loop.

Then uniform generation: seed = mod_mult(a, seed, m); u = seed / m.

Then for RANNOR: need to generate normals. SAS's RANNOR implementation: It generates U1, U2 via RANUNI, then computes R = sqrt(-2*ln(U1)), Theta = 2*pi*U2, Z0 = R*cos(Theta), Z1 = R*sin(Theta). It returns Z0 first, stores Z1 for next call. On next call, it returns stored Z1 without generating new uniforms. So every two calls consume two uniforms.

We need to verify if SAS's NORMAL uses same. Let's implement.

Then X = J + 2*Z, rounded to 0.1.

We need to test if this matches SAS output? We don't have SAS output to compare, but we can attempt to produce plausible values. The evaluation will check that our CSV matches expected? The expected outputs are figure and CSV of plotted data. The checker will likely compare our CSV to SAS-generated CSV (which used SAS RNG). If we don't replicate exactly, we will fail.

Alternative approach: Since we don't have SAS output, we could attempt to approximate and hope checker uses tolerance? But instructions say reproduce SAS semantics faithfully, including rounding half away from zero. For random data, they may check that we used correct seed and generation method, but maybe they will not check exact values, just that file exists? However typical evaluation for these tasks checks that outputs are produced and have correct structure, not exact random values? But they said reproduce exactly.

We could attempt to search for existing SAS output for P031 to see values. Could we find phuse-scripts output? Maybe we can approximate by using R's rnorm with seed 777 and see if that is considered acceptable? The evaluation might not verify exact X values, just that boxplot is created.

We need to make a best effort to implement SAS RNG as described.

Let's implement mod_mult.

Also need to handle seed update: SAS documentation says if seed is positive, it is used as initial seed, then updated. If seed is 0, uses time. So for NORMAL(777), first call uses seed 777 to generate uniform, then seed becomes new value for next call.

Implementation steps:

seed <- 777
has_saved <- FALSE
saved_z <- NA

function get_normal() {
 if (has_saved) { has_saved <<- FALSE; return(saved_z) }
 else {
   # generate two uniforms
   seed <<- mod_mult(397204094, seed, 2147483647)
   u1 <- seed / 2147483647
   seed <<- mod_mult(397204094, seed, 2147483647)
   u2 <- seed / 2147483647
   # avoid u1==0 (log 0) - SAS would handle? but u1 never 0 because seed never 0 (m is prime)
   r <- sqrt(-2 * log(u1))
   theta <- 2 * pi * u2
   z0 <- r * cos(theta)
   z1 <- r * sin(theta)
   saved_z <<- z1
   has_saved <<- TRUE
   return(z0)
 }
}

We need to generate 120 normals in order: TRT loop outer, J inner, VISIT inner. So order is: TRT="Study Drug", J=1, VISIT=3,4,5,6, then J=2, etc., then TRT="Placebo", etc.

Let's implement in R and see what values we get (we can mental simulate a few steps, but we can code).

We also need to implement SAS ROUND: ROUND(x, 0.1) = floor(x/0.1 + 0.5) *0.1 for positive, and ceil for negative? Actually SAS ROUND half away from zero: ROUND(x, unit) = floor(x/unit + 0.5) for x>=0, ceil(x/unit -0.5) for x<0. Equivalent to sign(x)*floor(abs(x)/unit +0.5)*unit. Need to implement.

We should write R script that does this.

Also need to produce boxplot: Use ggplot2.

We need to compute boxplot stats using SAS PCTLDEF=5. Let's implement quantile function for PCTLDEF=5: For n observations sorted x[1..n], p in (0,1), let n*p = j + g where j = floor(n*p), g = fractional part. If g==0 then quantile = (x[j] + x[j+1])/2, else quantile = x[ceil(n*p)]. But need to handle edge cases: For p=0, quantile = x[1]; for p=1, quantile = x[n]. For SAS PCTLDEF=5, definition: Q(p) = (1-g)*x[j] + g*x[j+1] ??? Wait that's PCTLDEF=2. Let's recall SAS definitions:

SAS has 5 definitions:

- PCTLDEF=1: empirical distribution, no averaging: Q = x[ceil(n*p)]
- PCTLDEF=2: empirical with averaging: Q = (x[ceil(n*p)] + x[floor(n*p)+1])/2 ??? Not.
- PCTLDEF=3: nearest even?
- PCTLDEF=4: ...
- PCTLDEF=5: empirical distribution with averaging: If n*p is integer, average x[j] and x[j+1]; else x[ceil(n*p)].

Actually that's PCTLDEF=5. Let's verify: SAS doc: PCTLDEF=5: p*(n) = j + g, if g=0 then (x[j] + x[j+1])/2 else x[ceil(p*n)]. Yes.

So for quartiles: Q1 p=0.25, median p=0.5, Q3 p=0.75.

We need to compute for each group (each VIS). Each group has n=10 observations (since J=1..10 per TRT per VISIT). So n=10.

Compute for n=10:

- Q1: n*p =2.5 => j=2,g=0.5 => ceil=3 => x[3]
- Median: n*p=5 => j=5,g=0 => (x[5]+x[6])/2
- Q3: n*p=7.5 => j=7,g=0.5 => x[8]

So quartiles are simple.

We need to compute whiskers: lower whisker = smallest observation >= Q1 -1.5*IQR, upper whisker = largest observation <= Q3 +1.5*IQR. Outliers beyond.

We should compute these for plot, but ggplot2's geom_boxplot uses different quantile definition (type 7). So we need to either compute stats manually and use geom_boxplot with stat="identity" or use custom.

Simpler: Use ggplot2 but override with our computed stats: Use geom_boxplot with precomputed.

We also need to handle IDSYMBOL: outliers plotted as star red.

We need to produce PNG with size 9in x 7in as per GOPTIONS HSIZE=9 VSIZE=7.

We need to add title.

We also need to produce CSV of plotted data: likely XX dataset with columns TRT, SUBJID, VISIT, X, VIS.

We should also produce txt rendering? The instructions say each table or listing additionally as plain-text rendering. For figure, maybe not needed, but we should produce txt with summary.

We need to ensure OUT_DIR handling.

Let's craft R script.

We need to implement mod_mult function.

We need to generate data.

We need to create boxplot.

We need to write CSV and PNG.

We should also write a txt file with boxplot statistics.

Let's draft script.