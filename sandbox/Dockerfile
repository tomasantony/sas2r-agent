# Sandbox image for running model-generated R: pinned R 4.6 with the packages the prompt allows.
# Containers run with --network none, a read-only mount of the benchmark, and only the attempt folder writable.
FROM rocker/tidyverse:4.6.0

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y --no-install-recommends \
        libxt6 libglpk40 libmagick++-dev libudunits2-dev libgdal-dev libgeos-dev libproj-dev \
    && rm -rf /var/lib/apt/lists/*

# tidyverse image already has dplyr tidyr readr stringr lubridate purrr tibble jsonlite ggplot2 haven readxl
RUN R -q -e 'options(warn = 2); install.packages(c( \
      "data.table", "survival", "survminer", "broom", "Hmisc", "admiral", "pharmaversesdtm", \
      "writexl", "openxlsx", "gt", "r2rtf", "arrow"), \
      repos = "https://p3m.dev/cran/__linux__/noble/latest", Ncpus = 4)'

# unprivileged user (rocker images ship "rstudio" as uid 1000); the attempt folder is mounted at /work
USER rstudio
WORKDIR /work
ENV OUT_DIR=/work/out
ENTRYPOINT ["Rscript", "--vanilla"]
CMD ["script.R"]
