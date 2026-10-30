Need to translate SAS program P002_adae.sas to R.

We have inputs: sdtmnew.AE, sdtmnew.suppae (not used?), adamnew.ADSL, adamnew.ADAE (but we are to recreate ADAE). Actually program reads sdtmnew.AE and adamnew.ADSL, creates adamnew.ADAE.

We need to locate files. Paths given: &root = D:/meta_hackathon/sas2r-agent, &rawsdtm = .../data/raw/cdisc-pilot/sdtm, &rawadam = .../data/raw/cdisc-pilot/adam, &rawsplit etc. But librefs sdtmnew and adamnew point to derived directories: D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/sdtm and adam. However setup.sas loads xpt files into those libs. So actual source data is xpt files in raw directories.

We need to read inputs from absolute paths given in context. The instruction says read inputs from absolute paths given in context (forward slashes). So we need to find where files are.

We have environment variable OUT_DIR. Need to write OUT_DIR/adae.csv

We need to reproduce SAS semantics.

Let's explore filesystem.

We need to list files.