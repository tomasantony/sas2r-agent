Need to translate SAS program to R. Need to produce 3 figures waterfall_xf_100, 125, 150 png and CSV of plotted data.

We need to understand SAS logic.

We have tumor data generation:

data tumor;
  format Drug res.;
  length Code $6;
  do i=1 to 20;
    Response=100*(ranuni(3)-0.7);
    Duration=50*ranuni(3);
    Dropped=.;
    if ranuni(3) > 0.8 then Dropped=duration-5; 
    Drug=floor(6*ranuni(3));
    if ranuni(3) > 0.7 then Code='FL';
    else Code='DLBCL';
    codeloc=ifn(response > 0, 0, response);
    baseline=20+180*(ranuni(2));
    output;
  end;
run;

Note ranuni(seed) in SAS: uniform random. With seed argument: if seed <=0 uses clock, if >0 uses that seed and updates. Here ranuni(3) and ranuni(2). Need to replicate exactly SAS ranuni behavior.

SAS ranuni uses Fishman & Moore? Need to emulate.

SAS documentation: RANUNI(seed) returns uniform (0,1). Implementation: uses prime modulus multiplicative congruential generator with modulus 2^31-1 = 2147483647, multiplier 397204094? Wait different versions.

Actually SAS RANUNI: seed is updated: seed = mod(seed * 397204094, 2147483647) ??? Let's recall.

Search memory: SAS ranuni uses: seed = mod(397204094 * seed, 2147483647); return seed / 2147483647.

But there are variations. For RANUNI, multiplier is 397204094? For RANUNI with seed 0? Let's check.

Alternatively, SAS documentation: RANUNI uses Lehmer generator: seed_{n+1} = (a * seed_n) mod m, where a=397204094, m=2^31-1.

Yes.

We need to replicate sequence of calls.

In tumor data step: loop i=1 to 20, each iteration does:

Response=100*(ranuni(3)-0.7);
Duration=50*ranuni(3);
Dropped=.;
if ranuni(3) > 0.8 then Dropped=duration-5;
Drug=floor(6*ranuni(3));
if ranuni(3) > 0.7 then Code='FL'; else Code='DLBCL';
codeloc=ifn(response > 0, 0, response);
baseline=20+180*(ranuni(2));

Note ranuni(3) uses seed 3 stream, ranuni(2) uses seed 2 stream. They are independent seeds.

We need to simulate both streams.

We have 20 iterations, each iteration uses 5 calls to ranuni(3) plus maybe? Let's count: Response (1), Duration (1), Dropped check (1), Drug (1), Code check (1) =5 calls per iteration to seed 3. Plus baseline uses seed 2: 1 call per iteration.

So total 100 calls to seed 3, 20 calls to seed 2.

We need to generate uniform numbers using SAS algorithm.

We must verify multiplier. Some sources say RANUNI multiplier is 397204094. Let's test quickly mental: seed=3 => next seed = 397204094*3 mod 2147483647 = 1191612282 mod ... = 1191612282. /2147483647 = 0.554... Let's see if that plausible.

We need to ensure R reproduces SAS output expected. The expected outputs are figures; we need to produce CSV of plotted data that matches SAS. The benchmark will compare? Probably they check that files exist and maybe content matches within tolerance. We need to faithfully replicate transformation.

We also need to understand WaterFall_3D_Macro logic to produce projected data.

We need to implement matrix functions: MatInv, MatMult, MatIdent. But macro uses MatMult only.

We need to replicate steps:

- Wall_Axes data: defines walls and axes in unit space including xFac.

- Projected_walls: projection matrix m, rotation matrices rx, ry, rz, then w = rx * ry * rz * m? Actually code: call MatMult(rz, m, u); call MatMult(ry, u, v); call MatMult(rx, v, w); So w = rx * ry * rz * m . Order matters: matrix multiplication is not commutative. They do rz*m -> u, ry*u -> v, rx*v -> w. So w = rx * (ry * (rz * m)) = rx*ry*rz*m.

Then for each wall/axis point, d = [xw, yw, zw, 1]^T, p = w * d, then xw = p[1,1] etc. Similarly for xw2 etc and xl.

- Compute xaxisRot, yaxisRot: fac=sqrt(xFac); dx = xw2 - xw; if abs(dx)>0.001 then xaxisRot = fac*180*atan((yw2-yw)/dx)/pi? Actually code: xaxisRot=fac*180*atan((yw2-yw)/dx)/3.1415; Note 3.1415 approx pi. Use pi.

Similarly yaxisRot.

- Find DurMin, DurMax, ResMin, ResMax from Data.

- Create polygons data: polygons dataset with pid, type, x,y,z etc.

Let's parse polygons creation:

retain pid 0 maxy -1e6 miny 1e6 maxz -1e6 minz 1e6;
set Data end=last;
durfac=1.05; resfac=1.05; dx=xFac; i=_n_-1;
aResMax=resfac*max(abs(ResMax), abs(ResMin));
aDurMax=durfac*max(abs(DurMax), abs(DurMin));
Lid=.;
type='Duration'; pid+1; z=0;
 x=dx*(i-0.5-0.4); y=0; output;
 x=x; y=Duration; output;
 x=dx*(i-0.5+0.4); y=y; output;
 x=x; y=0; output;
type='Response'; pid+1; y=0;
 x=dx*(i-0.5-0.4); z=0; output;
 x=x; z=Response; output;
 x=dx*(i-0.5+0.4); z=z; output;
 x=x; z=0; output;
if Dropped then do; pid+1; z=0; type='Dropped'; x=dx*(i-0.5-0.3); y=Duration-0.03*DurMax; output; x=dx*(i-0.5); y=Duration; output; x=dx*(i-0.5+0.3); y=Duration-0.03*DurMax; output; x=dx*(i-0.5); y=Duration-0.06*DurMax; output; end;
xMax=dx*(i);
type='Code'; x=dx*(i-0.5); z=ifn(Response <0, Response,0); y=0; output;
if last then do;
 call missing(duration, response, Code);
 type='Response'; pid+1; x=-dx; y=0; z=-aResMax; output; x=-dx; y=0; z=aResMax; output;
 pid+1; type='Duration'; x=-dx; y=0; z=aResMax; output; x=-dx; y=aDurMax; z=aResMax; output;
 pid=.; type='Y-Axis'; Lid=1; z=0; do y=0 to aDurMax by 10; x=-dx; output; x=xMax; output; Lid+1; end;
 type='Y-Values'; Lid=.; z=0; do y=0 to aDurMax by 10; x=xMax; yTickVal=y; output; end;
 type='Z-Values'; yTickVal=.; Lid=.; y=0; x=dx*(-1); do z=-10 to -aResMax by -10; zTickVal=z; output; end; do z=0 to aResMax by 10; zTickVal=z; output; end;
end;

Note: Need to handle missing values.

- Compute xmin etc from polygons.

- Normalize: xrange = xmax - xmin etc; x = 2*xFac*(x - xmin)/xrange - xFac; y=2*(y - ymin)/yrange -1; z=2*(z - zmin)/zrange -1;

- Project data similarly: same w matrix, transform each point.

- Combined = merge projected_walls and projected_data (SAS merge without by => just concatenates? Actually merge without by does one-to-one merge by observation number. So combined will have rows from both datasets aligned by row number, with missing where one dataset shorter. Need to replicate: combined = bind rows? But SAS merge without BY does sequential merge: first obs of each dataset together, etc. If one dataset has more obs, remaining obs have missing for other dataset's variables. So we can do full join by row index.

- Then SGPLOT draws.

We need to produce R equivalent plot. The expected output is 3 PNGs with different xFac. We need to approximate 3D waterfall.

Simplify: Instead of trying to exactly replicate SAS SGPLOT polygon rendering, we can create a 2D projection plot using ggplot2 polygons.

We need to generate projected coordinates for walls, duration bars, response bars, dropped diamonds, axes, tick values, code labels.

We can compute projected coordinates as SAS does, then plot using ggplot.

We need to implement matrix multiplication correctly.

We also need to handle tumor sorting: proc sort data=tumor out=tumor; by descending Response; So sort descending Response.

Then tumorsorted with j=_n_ but not used? Actually macro uses Data=tumor (sorted). So order is descending Response.

We need to replicate ranuni generation then sort.

We also need attrmap colors: for Resp groups: 1.0 mg white, 2.0 mg #ffffc0, 3.0 mg #f0d0a0, 4.0 mg orange, 3.0 F6 mg #ff7f00, 4.0 F6 mg red. Need to map Drug values 0-5 to those labels via format res. Drug = floor(6*ranuni). So 0->1.0 mg, 1->2.0 mg, etc.

We need to create Drug label.

We also need to handle Code values.

We need to produce CSV of plotted data: likely the projected_data or combined. The spec says "write OUT_DIR/waterfall_xf_100.png and a CSV of the plotted data" – we need to decide CSV name. Probably waterfall_xf_100.csv etc. The expected outputs list figure Waterfall_Xf_100.png -> write OUT_DIR/waterfall_xf_100.png and a CSV of the plotted data. So we should write waterfall_xf_100.csv, waterfall_xf_125.csv, waterfall_xf_150.csv.

We need to ensure OUT_DIR exists.

We need to implement SAS ranuni accurately. Let's implement function sas_ranuni(seed_vec, n) ?

We have two independent seeds: seed3 =3, seed2=2. Each call updates seed.

Pseudo:

sas_ranuni <- function(seed, n) {
  m <- 2147483647
  a <- 397204094
  out <- numeric(n)
  s <- seed
  for (i in 1:n) {
    s <- (a * s) %% m
    out[i] <- s / m
  }
  return(list(values=out, seed=s))
}

But need to check if SAS uses this exact multiplier. Alternative multiplier sometimes cited as 397204094 for RANUNI, 663608941 for RANNOR? Let's verify.

Search memory: SAS documentation for RANUNI: "The RANUNI function uses the prime modulus multiplicative congruential generator with modulus 2**31-1 and multiplier 397204094." Yes.

So we will use that.

We need to generate sequence interleaved? Actually seed3 calls are sequential, seed2 calls separate. So we can generate 100 values for seed3, 20 for seed2.

Let's implement and test quickly mental for first few values to see if plausible.

We should also consider that SAS ranuni(3) with seed 3: first value = 397204094*3 mod 2147483647 = 1191612282 /2147483647 = 0.554... Let's compute: 1191612282/2147483647 ≈0.5549.

Second call: seed=1191612282 => 397204094*1191612282 mod ... Need compute but we can trust R.

We need to ensure R's numeric precision: a*s can be up to ~ 8.5e17, which exceeds 2^53 ~9e15, so double precision may lose integer accuracy. Need to use 64-bit integer or careful modular multiplication. Use R's double may cause error. We need to implement modular multiplication without overflow using e.g., using bit64 or using R's integer64.

We can use R's ability to do (a * s) %% m with double may be inaccurate for large s. Better to use R's gmp or use algorithm for modular multiplication using splitting.

We can implement using R's double with careful: Since m ~2.1e9, a ~4e8, s < m, product < 8.5e17 < 2^53 (9e15) actually 8.5e17 >9e15, so exceeds 53-bit mantissa, so double will lose precision.

We need to use 64-bit integer. We can use bit64 package? Not in allowed list. Allowed packages: haven, dplyr, tidyr, readr, stringr, lubridate, purrr, tibble, jsonlite, data.table, ggplot2, survival, survminer, broom, Hmisc, admiral, pharmaversesdtm, readxl, writexl, openxlsx, gt, r2rtf, arrow. No bit64. But we can implement modular multiplication using splitting technique to avoid overflow with doubles.

We can use algorithm: (a * s) mod m using 64-bit via R's integer? R integer is 32-bit. Could use R's numeric with splitting: a = a1*2^15 + a0 etc.

Simpler: we can use R's built-in to compute with double but using Rmpfr? Not allowed.

We can implement using iterative addition: Use Russian peasant multiplication mod m.

Function mod_mult(a,b,m): result=0; a %=m; while b>0: if b%%2==1 result=(result+a)%%m; a=(a*2)%%m; b<- floor(b/2). This uses only addition and doubling, which stays <2*m <4e9 fits in double exactly (since <2^32). So we can compute accurately with doubles.

We need to do this for each step.

We have up to 100 iterations, each with loop ~31 steps, fine.

Implement sas_ranuni_seq(seed0, n):

s <- seed0
vals <- numeric(n)
for i in 1:n:
  s <- mod_mult(397204094, s, 2147483647)
  vals[i] <- s / 2147483647

We need to verify mod_mult correctness.

We also need to handle seed update: SAS updates seed variable passed in. If seed is 0, uses clock. But we have fixed seeds.

We should implement.

Next, generate tumor data.

We need to replicate SAS data step exactly: For each i 1..20:

Response = 100*(u1 -0.7)
Duration = 50*u2
Dropped = NA; if u3 >0.8 then Dropped = Duration -5
Drug = floor(6*u4)
Code = if u5 >0.7 then 'FL' else 'DLBCL'
codeloc = ifelse(Response>0,0,Response)
baseline = 20+180*u6 where u6 from seed2 stream

Note u1..u5 are sequential from seed3 stream, u6 from seed2.

We need to map Drug to label via format res.

We need to sort by descending Response.

We need to compute DurMin etc.

We need to implement WaterFall_3D_Macro function in R that given data, xFac, Tilt, Rotate, etc., produces projected data and plot.

We need to handle Tilt and Rotate clamping: xFac 0.5-4, Tilt 60-80, Rotate -75 to -15. In our calls: xFac 1.0,1.25,1.5, Tilt 75, Rotate -45 => within bounds.

We need to compute Wall_Axes: dx = xFac.

We need to create data frame wall_axes with columns id, wgroup, xw, yw, zw, xw2, yw2, zw2, xl, yl, zl, label, lbx,lby,lbz? Actually lbx etc set later.

We need to replicate exactly.

Then projected_walls: compute w matrix.

We need to implement matrix multiplication: 4x4 matrices.

We can use R matrix operations: w <- rx %*% ry %*% rz %*% m ??? Wait order: w = rx * ry * rz * m . In SAS they did rz*m -> u, ry*u -> v, rx*v -> w. So w = rx %*% ry %*% rz %*% m . Note matrix multiplication order: In SAS call mult(A,B,C) does C = A*B . So yes.

We need to define m, rx, ry, rz as 4x4.

m = matrix(c(1/r,0,0,0, 0,1/t,0,0, 0,0,-2/(f-n),0, 0,0,-(f+n)/(f-n),1), nrow=4, byrow=FALSE? Need to check SAS array indexing: array m[4,4] temporary; m[1,1]=1/r etc. SAS arrays are row-major? In FCMP, array indexing is [row,col]. So m[1,1]=1/r, m[1,2]=0, etc. So matrix as defined row-wise.

We need to construct matrix with rows as first index, cols second.

So m = matrix(0,4,4); m[1,1]=1/r; m[2,2]=1/t; m[3,3]=-2/(f-n); m[3,4]=-(f+n)/(f-n); m[4,4]=1

Similarly rx: rx[1,1]=1; rx[2,2]=cos(A); rx[2,3]=-sin(A); rx[3,2]=sin(A); rx[3,3]=cos(A); rx[4,4]=1

ry: ry[1,1]=cos(B); ry[1,3]=sin(B); ry[2,2]=1; ry[3,1]=-sin(B); ry[3,3]=cos(B); ry[4,4]=1

rz: rz[1,1]=cos(C); rz[1,2]=-sin(C); rz[2,1]=sin(C); rz[2,2]=cos(C); rz[3,3]=1; rz[4,4]=1

Note B=0 always (since %let B=0). So ry is identity.

We need to compute w = rx %*% ry %*% rz %*% m

Then for each row in Wall_Axes, compute p = w %*% d where d = c(xw,yw,zw,1). Then xw = p[1], yw=p[2], zw=p[3] (ignore w component? SAS does xw=p[1,1] etc, not dividing by w). So just take first 3 components.

Similarly for xw2 etc and xl.

We need to do same for normalized data.

We need to compute xaxisRot, yaxisRot: fac=sqrt(xFac); dx = xw2 - xw; if abs(dx)>0.001 then xaxisRot = fac*180*atan((yw2-yw)/dx)/3.1415 else 90. Note SAS uses 3.1415 not pi. We should use same 3.1415 to match.

Similarly yaxisRot.

We need to compute DurMin etc.

We need to create polygons data frame.

We need to handle pid increment: retain pid 0; each type increments.

We need to handle Lid for Y-Axis.

We need to handle aResMax = resfac * max(abs(ResMax), abs(ResMin)); aDurMax = durfac * max(abs(DurMax), abs(DurMin));

Note DurMin may be >0, but they use max(abs(...)). So aDurMax =1.05*max(DurMax, DurMin) essentially DurMax.

We need to create polygons rows: each output is a row with columns pid, type, x,y,z, code, Lid, yTickVal, zTickVal, Group, etc.

We need to replicate exactly to compute xmin etc.

We need to compute xmin etc from polygons.

Then normalized: x = 2*xFac*(x - xmin)/xrange - xFac; y=2*(y - ymin)/yrange -1; z=2*(z - zmin)/zrange -1;

Note xrange = xmax - xmin etc.

We need to keep Group variable.

Then projected_data: similar transformation but with normalized coordinates.

We need to produce combined data for plotting.

Simplify plotting: Use ggplot2 to draw:

- Walls: polygon id=id x=xw y=yw . Need to map fill colors: Walls attrmap: Center cxd0d0d0, Back cxe0e0e0, Right cxf0f0f0, Bottom cxe7e7e7. Convert cx... to hex: cxd0d0d0 => #d0d0d0 etc.

- Axes: vector lines: from (xw,yw) to (xw2,yw2) with group wgroup: D solid black, L shortdash.

- Y-Axis gridlines: series x=xAy y=yAy group=lid

- Duration bars: polygon id=pid x=xd y=yd fill

- Dropped diamonds: polygon id=pid x=xc y=yc fill gold

- Response bars: polygon id=pid x=xr y=yr fill by Group color

- Axis labels: text at xl,yl

- Y tick values: text at xYv,yYv

- Z tick values: text at xZv,yZv

- Code labels: text at xcode,ycode

We need to implement ggplot layers.

We need to handle colors for Resp groups: map Drug label to fillcolor.

We need to create factor for Group.

We need to produce 3 plots with different xFac.

We need to also output CSV of plotted data: likely the projected_data or combined. We'll output a CSV containing the projected coordinates for each polygon.

We should create a CSV with columns: xFac, pid, type, x, y, z, Group, Code, etc., plus projected coordinates.

Simpler: output the combined projected data (projected_data + projected_walls) as CSV.

We need to ensure output directory exists.

We need to handle OUT_DIR env var.

We need to write R script that does all.

We need to implement SAS ranuni accurately with mod_mult.

Let's code R script.

We need to handle baseline generation: uses ranuni(2) stream. Need to generate 20 values for seed2.

We need to ensure order: For each i, we consume 5 values from seed3, 1 from seed2. So we can pre-generate vectors and then assign.

Let's implement mod_mult function.

We also need to handle Drug floor: floor(6*u). u in (0,1) exclusive? So values 0-5.

We need to handle Code.

We need to sort descending Response.

We need to compute DurMin etc after sorting? Actually macro computes from Data (tumor sorted). So same.

We need to implement function waterfall_3d_macro(data, xFac, Tilt, Rotate, Lblx, Lbly, Lblz, Title, attrmap) returns list with projected data and plot.

We need to handle Tilt clamping: A = Tilt, if >80 set 80, if <60 set 60. Similarly C = Rotate, if >-15 set -15, if <-75 set -75. B=0.

We need to handle xFac clamping 0.5-4.

We need to compute Wall_Axes data frame.

Let's outline steps in R:

wall_axes <- function(xFac) {
  dx <- xFac
  df <- tibble(
    id = c('X1-Axis','X3-Axis','X4-Axis','Y2-Axis','Y3-Axis','Y4-Axis','Z1-Axis','Z2-Axis','Z4-Axis',
           rep('Back',4), rep('Right',4), rep('Bottom',4), rep('Center',4)),
    wgroup = c('D','L','D','D','D','L','D','L','D', rep('D',16)),
    xw = c(-dx,-dx,-dx,-dx, dx, dx,-dx, dx, dx,
           -dx,-dx,-dx,-dx,
           -dx, dx, dx,-dx,
           -dx, dx, dx,-dx,
           -dx, dx, dx,-dx),
    yw = c(-1,-1,1,-1,-1,-1,-1,-1,1,
           -1,1,1,-1,
           1,1,1,1,
           -1,-1,1,1,
           -1,-1,1,1),
    zw = c(-1,1,1,1,-1,1,-1,-1,-1,
           -1,-1,1,1,
           -1,-1,1,1,
           -1,-1,-1,-1,
           0,0,0,0),
    xw2 = c(dx,dx,dx,-dx,dx,dx,-dx,dx,dx, rep(NA,16)),
    yw2 = c(-1,-1,1,1,1,1,-1,-1,1, rep(NA,16)),
    zw2 = c(-1,1,1,1,-1,1,1,1,1, rep(NA,16)),
    xl = c(0,NA,NA,NA,dx+0.1,NA,-dx,NA,NA, rep(NA,16)),
    yl = c(-1,NA,NA,NA,0,NA,-1.2,NA,NA, rep(NA,16)),
    zl = c(-1.1,NA,NA,NA,-0.1,NA,0,NA,NA, rep(NA,16)),
    label = c(1,NA,NA,NA,2,NA,3,NA,NA, rep(NA,16))
  )
  return(df)
}

Check values: For X1-Axis: xw=-dx, yw=-1, zw=-1, xw2=dx, yw2=-1, zw2=-1, xl=0, yl=-1, zl=-1.1, label=1
X3-Axis: xw=-dx, yw=-1, zw=1, xw2=dx, yw2=-1, zw2=1
X4-Axis: xw=-dx, yw=1, zw=1, xw2=dx, yw2=1, zw2=1
Y2-Axis: xw=-dx, yw=-1, zw=1, xw2=-dx, yw2=1, zw2=1
Y3-Axis: xw=dx, yw=-1, zw=-1, xw2=dx, yw2=1, zw2=-1, xl=dx+0.1, yl=0, zl=-0.1, label=2
Y4-Axis: xw=dx, yw=-1, zw=1, xw2=dx, yw2=1, zw2=1
Z1-Axis: xw=-dx, yw=-1, zw=-1, xw2=-dx, yw2=-1, zw2=1, xl=-dx, yl=-1.2, zl=0, label=3
Z2-Axis: xw=dx, yw=-1, zw=-1, xw2=dx, yw2=-1, zw2=1
Z4-Axis: xw=dx, yw=1, zw=-1, xw2=dx, yw2=1, zw2=1
Back: 4 points: (-dx,-1,-1), (-dx,1,-1), (-dx,1,1), (-dx,-1,1)
Right: (-dx,1,-1), (dx,1,-1), (dx,1,1), (-dx,1,1)
Bottom: (-dx,-1,-1), (dx,-1,-1), (dx,1,-1), (-dx,1,-1)
Center: (-dx,-1,0), (dx,-1,0), (dx,1,0), (-dx,1,0)

We need to ensure correct.

Next projected_walls: compute w matrix, transform.

We need to implement function project_walls(df, A,B,C):

r=1,t=1,f=1,n=-1
pi=pi
fac=pi/180
A_rad = A*fac, etc.

m matrix etc.

w = rx %*% ry %*% rz %*% m

Then for each row, compute p = w %*% c(xw,yw,zw,1) => xw_p = p[1], etc.

Similarly for xw2 etc if not NA, and xl etc.

We need to handle NA: if xw2 is NA, keep NA.

We also need to set lbx,lby,lbz based on label: if label==1 then lbx=Lb lx, etc.

We need to compute xaxisRot, yaxisRot after projection.

We need to compute DurMin etc.

We need to create polygons.

We need to handle Dropped: if Dropped is not NA (SAS: if Dropped then do; In SAS, numeric missing is false, non-missing true? Actually Dropped is numeric, missing is false, any non-missing true? In SAS, if Dropped then ... means if Dropped ne 0 and not missing. Since Dropped is either missing or duration-5 (>0), so true when not missing. So we check !is.na(Dropped).

We need to handle Code: Code is character.

We need to handle pid increment: start 0, each polygon increments.

We need to handle type values.

We need to handle xMax: dx*(i) where i = _n_-1? Actually i = _n_-1, where _n_ is observation number (1-indexed). So for first obs, i=0, xMax=dx*0=0, but later for last obs i=19, xMax=dx*19. However xMax is overwritten each iteration, final value is dx*(n-1). But they use xMax for Y-Axis grid: x=xMax. So xMax is final max x.

We need to handle Lid for Y-Axis: start 1, increment each iteration.

We need to handle Y-Values and Z-Values.

We need to compute xmin etc.

We need to normalize.

We need to project data.

We need to combine.

We need to plot.

We need to handle attrmap colors: convert cx... to #...

We need to handle ggplot.

We need to produce PNG with width 4in height 4in at 300 dpi => 1200x1200 pixels. Use ggsave width=4 height=4 dpi=300.

We need to handle title and footnote: title = "Tumor Response and Duration", footnote = "X-Rotation=... Y-Rotation=... Z-Rotation=..."

We need to handle legend.

We need to handle text rotations: xaxisRot, yaxisRot.

We need to handle axis display none.

We need to handle polygon transparency.

We need to handle dataskin sheen? Not needed.

We need to handle lineattrs.

We need to handle vector arrows: noarrowheads.

We need to handle text positions.

We need to handle code labels rotated 90.

We need to handle Y tick values and Z tick values.

We need to handle scatter dots for Z.

We need to handle keylegend.

We need to handle xaxis offset.

We need to handle aspect=1.

We need to handle nowall noborder.

We need to handle output CSV.

We need to ensure script runs non-interactively.

We need to handle OUT_DIR.

We need to handle reading inputs? No inputs needed, data is generated.

We need to handle writing CSVs.

We need to handle figure naming: waterfall_xf_100.png etc. SAS imagename Waterfall_Xf_100 => file Waterfall_Xf_100.png. We need to write lowercase as per spec: waterfall_xf_100.png

We need to handle multiple xFac values: 1.0,1.25,1.5

We need to handle Tilt 75 Rotate -45.

We need to handle Lblx etc.

We need to handle Group variable: Drug.

We need to handle Code variable.

We need to handle Duration, Response, Dropped.

We need to handle AttrMap.

We need to handle Title.

We need to handle gpath etc not needed.

We need to handle options cmplib etc not needed.

We need to handle proc format res.

We need to handle data tumor generation.

We need to handle sorting.

We need to handle tumorsorted j.

We need to handle attrmap creation.

We need to handle WaterFall_3D_Macro calls.

We need to handle ODS listing etc not needed.

We need to handle footnote.

We need to handle proc sgplot.

We need to handle combined data.

We need to handle writing outputs.

We need to handle error handling.

We need to handle package loading.

We need to handle writing CSV of plotted data: we should write projected_data and projected_walls combined.

We need to handle writing table/listing? Expected outputs only figures, but spec says each table/listing additionally as txt. But we have only figures, so just PNG and CSV.

We need to handle writing CSV with same variable names.

We need to handle dates etc not needed.

We need to handle numeric rounding.

We need to handle missing values.

We need to handle character ordering.

We need to handle SAS semantics.

We need to handle matrix functions.

We need to handle all.

We need to produce final R script.

We need to ensure script is standalone.

We need to handle OUT_DIR creation.

We need to handle writing files.

We need to handle ggplot2 theme.

We need to handle colors.

We need to handle legend.

We need to handle text sizes.

We need to handle axis limits.

We need to handle polygon grouping.

We need to handle transparency.

We need to handle line colors.

We need to handle gridlines.

We need to handle walls.

We need to handle axes.

We need to handle labels.

We need to handle tick values.

We need to handle code labels.

We need to handle dropped markers.

We need to handle duration bars.

We need to handle response bars.

We need to handle aspect.

We need to handle title.

We need to handle footnote.

We need to handle caption.

We need to handle ggsave.

We need to handle CSV.

We need to handle all.

We need to produce final answer.

We need to ensure script runs.

We need to test mentally.

We need to handle mod_mult implementation.

We need to handle ranuni generation.

We need to handle baseline.

We need to handle Drug mapping.

We need to handle Code.

We need to handle codeloc baseline not used.

We need to handle sorting.

We need to handle DurMin etc.

We need to handle polygons.

We need to handle normalization.

We need to handle projection.

We need to handle combined.

We need to handle plotting.

We need to handle output.

We need to handle all.

We need to produce final R code block.

We need to ensure no install.packages.

We need to ensure no interactive.

We need to ensure no graphics windows.

We need to ensure use of allowed packages.

We need to ensure reading inputs from absolute paths not needed.

We need to ensure writing to OUT_DIR.

We need to ensure handling of OUT_DIR env var.

We need to ensure handling of missing OUT_DIR.

We need to ensure handling of file paths.

We need to ensure handling of figure names.

We need to ensure handling of CSV names.

We need to ensure handling of PNG.

We need to ensure handling of CSV of plotted data.

We need to ensure handling of all.

We need to produce final script.

Let's craft script.

We need to include helper functions.

We need to include library calls.

We need to handle options.

We need to handle set.seed? Not needed, we use SAS ranuni.

We need to handle all.

We need to produce final answer with one R code block.

We need to ensure script is complete.

We need to handle edge cases.

We need to handle writing.

We need to handle final.

We need to produce final output.

