# 1. Base Image: R 4.4.2 (Latest stable)
FROM rocker/binder:4.4.2

# -----------------------------------------------------------------------------
# SYSTEM SETUP (Root)
# -----------------------------------------------------------------------------
USER root

# 2. Install System Dependencies & FIX CHROMIUM
# We add the 'xtradeb/apps' PPA. This provides a native .deb version of Chromium
# that does NOT require Snap, fixing the "snap not installed" error.
RUN apt-get update --yes && \
    apt-get install --yes --no-install-recommends software-properties-common gnupg && \
    add-apt-repository -y ppa:xtradeb/apps && \
    apt-get update --yes && \
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

# 3. Set Chromote Environment Variable
# Tells R explicitly where the browser binary is located
ENV CHROMOTE_CHROME=/usr/bin/chromium

# -----------------------------------------------------------------------------
# R PACKAGE INSTALLATION (Root)
# -----------------------------------------------------------------------------
RUN R -e "install.packages(c('chromote', 'dplyr', 'readr', 'purrr', 'cookiemonster', 'remotes', 'jsonlite', 'tibble'), repos='https://packagemanager.posit.co/cran/__linux__/jammy/latest')" && \
    R -e "remotes::install_github('JBGruber/traktok')"

# -----------------------------------------------------------------------------
# FILE SETUP (The Fix for Missing Files)
# -----------------------------------------------------------------------------
# Because we are using a custom Dockerfile, we must manually copy the repo contents
# to the home directory and set ownership to the notebook user (jovyan).
COPY --chown=${NB_USER}:${NB_USER} . ${HOME}

# -----------------------------------------------------------------------------
# USER SETUP
# -----------------------------------------------------------------------------
USER ${NB_USER}
