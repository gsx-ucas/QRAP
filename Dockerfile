FROM rocker/r-ver:4

apt update && apt upgrade \
    && apt install libfontconfig1-dev libcurl4-openssl-dev libxml2-dev libssl-dev libharfbuzz-dev libglpk40

RUN R -e "install.packages('devtools', repos = 'https://cloud.r-project.org')" \
    && R -e "devtools::install_github('gsx-ucas/QRAP')" 
