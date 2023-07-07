FROM rocker/r-ver:4

RUN apt update && apt upgrade -y \
    && apt install -y --no-install-recommends \
    libssl-dev libfontconfig1-dev \
    libcurl4-openssl-dev libxml2-dev \
    libharfbuzz-dev libfribidi-dev \
    libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
    && R -e "install.packages(c('digest', 'devtools'), repos = 'https://mirrors.tuna.tsinghua.edu.cn/CRAN/')" \
    && R -e "devtools::install_github('gsx-ucas/QRAP', upgrade = 'always', repos = 'https://mirrors.tuna.tsinghua.edu.cn/CRAN/')"

WORKDIR /QRAP
EXPOSE 3838

CMD R -e "QRAP::startQRAP(port = 3838, host = '0.0.0.0')"
