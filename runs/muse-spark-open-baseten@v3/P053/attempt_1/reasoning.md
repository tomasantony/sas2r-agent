Need to translate SAS program P053 to R.

We need to read advsmax.xpt from rawadam path. But libref inlib points to derived adam. However program uses filename source "&rawadam./advsmax.xpt" libname source xport. So read that file.

We need to understand outputs: P053.lst (106 numbers) -> text rendering and CSV, figure sasgraph.png, Target16.pdf (41 numbers). Need to reproduce.

We need to inspect data to understand.

We have environment variables: root etc. But in R we use OUT_DIR.

We need to list files.

We should explore filesystem.