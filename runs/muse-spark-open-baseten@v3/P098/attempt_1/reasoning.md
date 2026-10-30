Need to translate SAS program to R. Must produce 3 figures waterfall_xf_100.png etc and CSV of plotted data.

We need to understand SAS logic: generates tumor data with ranuni(3) and ranuni(2). Need to replicate SAS RANUNI.

SAS ranuni(seed) is uniform random. With seed argument: if seed <=0 uses clock, if positive uses that seed and updates. The sequence: ranuni(3) uses seed 3 stream. ranuni(2) uses seed 2 stream. Need to replicate exactly to get same data as SAS? But expected outputs are from SAS run; we need to reproduce numbers exactly? However SAS ranuni is specific LCG. We need to implement SAS ranuni to get identical tumor data.

Alternatively we could approximate? But requirement says reproduce outputs exactly. Need to implement SAS ranuni algorithm.

SAS RANUNI uses: seed = mod(seed * 397204094, 2^31-1) ??? Let's recall. SAS documentation: RANUNI uses Fishman and Moore with modulus 2^31-1 = 2147483647, multiplier 397204094? Actually SAS RANUNI multiplier is 397204094? Let's check. Common SAS ranuni: seed_{n+1} = (seed_n * 397204094) mod 2147483647, then ranuni = seed / 2147483647.

But there are different versions. Let's verify: SAS 9.4 RANUNI uses multiplier 397204094. Yes.

We need to simulate sequence for seed 3 and seed 2 interleaved.

In SAS data step: do i=1 to 20;
 Response=100*(ranuni(3)-0.7);
 Duration=50*ranuni(3);
 Dropped=.;
 if ranuni(3) >0.8 then Dropped=duration-5;
 Drug=floor(6*ranuni(3));
 if ranuni(3) >0.7 then Code='FL'; else Code='DLBCL';
 codeloc=ifn(response >0,0,response);
 baseline=20+180*(ranuni(2));
 output;
 end;

Note ranuni(3) calls: 1: Response, 2: Duration, 3: Dropped check, 4: Drug, 5: Code. So 5 calls per iteration to seed 3. Plus 1 call to seed 2 for baseline.

We need to generate 20*5=100 values from seed 3 stream, and 20 values from seed 2 stream.

Implement function sas_ranuni_stream(seed, n): iterative.

Let's implement in R.

We also need to handle sorting by descending Response, then j = _n_.

Then macro WaterFall_3D_Macro does transformations.

We need to reproduce figures: 3D waterfall using projection matrices.

We need to replicate exactly the SAS projection logic to produce PNGs.

Simplify: we can implement the same matrix operations in R.

Steps per xFac value (1.0,1.25,1.5) with Tilt=75, Rotate=-45.

We need to compute Wall_Axes, projected_walls, polygons, normalized, projected_data, combined.

Then plot using ggplot2 polygons.

We need to understand plotting: SAS sgplot with polygon, series, vector, text, scatter.

We can approximate with ggplot2: draw walls as polygons, duration bars, response bars, dropped diamonds, axes lines, grid lines, labels.

We need to produce PNGs that look similar; exact pixel match not required but should be plausible.

Also need CSV of plotted data: "a CSV of the plotted data" – likely the combined projected data.

We should output for each xFac a CSV containing the projected coordinates.

We need to decide what to write: maybe combined dataset.

We should write waterfall_xf_100.csv etc containing the tumor sorted data plus projected coordinates.

Simpler: write the tumor data and the projected_data.

Requirement: each figure as PNG via ggsave plus a CSV of the plotted data.

So for each of three figures, produce PNG and CSV.

We need to implement full pipeline.

Let's code R script.

Steps:

- out <- Sys.getenv("OUT_DIR"); if empty use temp.

- Implement sas_ranuni.

- Generate tumor.

- Create attrmap equivalent for colors.

- Define function waterfall_3d_macro(data, xFac, Tilt, Rotate, ...)

Inside function:

Compute Wall_Axes data frame.

Compute projected_walls using matrix multiplication.

We need MatMult: matrix multiplication. In SAS, call mult(A,B,C) does C = A * B.

We have matrices 4x4.

We need to replicate.

Define helper mat_mult.

Then compute projected_walls.

Then compute DurMin, DurMax, ResMin, ResMax.

Then polygons generation.

Note polygons generation uses pid increment, type, x,y,z.

We need to replicate exactly.

Then compute xmin etc.

Then normalized.

Then projected_data.

Then combined via merge (which in SAS merge without by merges by observation number). So combined = cbind(projected_walls, projected_data) row-wise? Actually SAS merge without BY does one-to-one merge by row number. So we need to align rows: projected_walls has how many rows? Wall_Axes has: 9 axis rows + 4+4+4+4 = 25? Let's count: X1,X3,X4,Y2,Y3,Y4,Z1,Z2,Z4 =9, Back 4, Right 4, Bottom 4, Center 4 =25. So 25 rows.

projected_data has: For each of 20 subjects: Duration polygon 4 rows, Response 4 rows, maybe Dropped 4 rows if dropped, Code 1 row. So per subject 9 or 13 rows. Plus final aspect polygons: 2+2 =4 rows, Y-Axis grid: need to compute aDurMax etc. Let's compute to know rows.

But merge of 25 rows with maybe ~200 rows will result in max rows, with missing values filling.

In SAS, merge without by will interleave: first 25 rows have both, remaining rows have only projected_data.

We need to replicate for plotting: combined dataset contains both wall and data columns.

For ggplot we can just use separate data frames.

Simpler: keep projected_walls and projected_data separate for plotting.

We need to produce plot.

We need to implement plotting logic:

- Walls: polygon id=id x=xw y=yw fill by id

- Y-Axis gridlines: series x=xAy y=yAy group=lid

- Duration Bars: polygon id=pid x=xd y=yd

- Drop Markers: polygon id=pid x=xc y=yc

- Response Bars: polygon id=pid x=xr y=yr group=Drug

- Axis lines: vector from (xw,yw) to (xw2,yw2)

- Axis labels: text at (xl,yl)

- Y tick values: text at (xYv,yYv)

- Z tick values: text at (xZv,yZv)

- Code labels: text at (xcode,ycode)

We can implement with ggplot2.

We need colors for Resp groups: mapping Drug 0-5 to labels 1.0 mg etc and fill colors: white, #ffffc0, #f0d0a0, orange, #ff7f00, red.

We need to map Drug numeric to label via format res.

We should create factor.

We also need to handle dropped diamonds color gold.

We need to produce three PNGs with different xFac.

We also need to write CSVs: let's write projected_data and tumor sorted.

For each xFac, write CSV named waterfall_xf_100.csv etc containing projected_data.

Also need to ensure output directory exists.

Let's craft R script.

Implement sas_ranuni:

sas_ranuni_next <- function(seed) { (seed * 397204094) %% 2147483647 }

But need to handle large multiplication overflow in R double (53 bits). 2147483647*397M ~ 8e17 fits within 2^53 ~9e15? Actually 8e17 >9e15, so double loses precision. Need to use 64-bit integer. Use bit64 or use R's integer? We can use double with modular multiplication using splitting.

We can implement using R's ability with big integer via gmp? Not available. Alternative use R's .C? Simpler use R's double with modulo using 64-bit via Rcpp? But we can implement using algorithm for modular multiplication without overflow: use Russian peasant.

Implement mod_mult(a,b,m): result =0; while b>0: if b%%2==1 result=(result+a)%%m; a=(a*2)%%m; b=floor(b/2). This avoids overflow because a < m <2^31, so a*2 <2^32 fits in double exactly.

Then seed_next = mod_mult(seed, 397204094, 2147483647)

Let's implement.

Test quickly mental.

We also need to generate sequence: initial seed is 3, first call uses updated seed? In SAS, ranuni(seed) where seed is positive, it uses seed to generate next and updates seed variable. The returned value is seed_new / 2^31-1? Or seed_new / 2147483647. Need to confirm.

SAS docs: If seed >0, RANUNI returns seed/(2^31-1) after updating? Actually algorithm: seed = mod(seed* multiplier, modulus); ranuni = seed / modulus.

So first call with seed=3 gives seed1 = (3*397204094) mod 2147483647 = 1191612282 mod ... =1191612282, ranuni=0.5549.

We can test.

We need to generate stream.

Implement function generate_ranuni(seed0, n): seed <- seed0; vals <- numeric(n); for i in 1:n { seed <- mod_mult(seed, 397204094, 2147483647); vals[i] <- seed / 2147483647 }; return(vals)

We need two streams: seed3_vals length 100, seed2_vals length 20.

Then assign per iteration.

Let's code.

Also need to ensure SAS ranuni(3) with constant 3 each call: In SAS data step, ranuni(3) where 3 is constant, does it maintain internal seed? Yes, SAS maintains seed per constant value? Actually ranuni(3) uses 3 as seed first time, then subsequent calls with same constant continue sequence. So our stream approach is correct.

Similarly ranuni(2) separate stream.

Proceed.

Now implement full R script.

