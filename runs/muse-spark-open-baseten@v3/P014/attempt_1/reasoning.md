Need to translate SAS program P014_sdtm_ex.sas to R.

We need to read raw_exposure.csv from project_path/data/raw/raw_exposure.csv

Project path = D:/meta_hackathon/sas2r-agent/data/derived/c2c

But also macro variables: &root. = D:/meta_hackathon/sas2r-agent

We need to handle reading inputs from absolute paths given in context. The SAS librefs: raw -> D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw etc. But program imports from &project_path./data/raw/raw_exposure.csv

We need to locate file. Let's explore filesystem.