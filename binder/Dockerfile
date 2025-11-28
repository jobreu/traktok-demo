# 1. Base Image: Use the official Rocker Binder image for R 4.5.1
FROM rocker/binder:4.4.2

# -----------------------------------------------------------------------------
# SYSTEM SETUP (Root)
# -----------------------------------------------------------------------------
USER root

# 2. Install System Dependencies
# - chromium & chromium-driver: For 'chromote'
# - libsecret-1-dev: Required by 'cookiemonster' (via keyring)
# - libsodium-dev: Required for encryption/hashing
# - Standard dev libs (libcurl, libssl, libxml2) included for general stability
RUN apt-get update --yes && \
    apt-get install --yes --no-install-recommends \
    chromium \
    chromium-driver \
    libsecret-1-dev \
    libsodium-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    git && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# -----------------------------------------------------------------------------
# R PACKAGE INSTALLATION (Root)
# -----------------------------------------------------------------------------
# Added 'jsonlite' and 'tibble' to the list below.
# Installing from Posit Package Manager (PPM) for fast binary downloads.

RUN R -e "install.packages(c('chromote', 'dplyr', 'readr', 'cookiemonster', 'remotes', 'jsonlite', 'tibble'), repos='https://packagemanager.posit.co/cran/__linux__/jammy/latest')" && \
    R -e "remotes::install_github('JBGruber/traktok')"

# -----------------------------------------------------------------------------
# USER SETUP
# -----------------------------------------------------------------------------
USER ${NB_USER}
