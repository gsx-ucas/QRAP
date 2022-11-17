FROM rocker/r-ver:4

RUN apt update && apt upgrade -y \
    && apt install -y libssl-dev libfontconfig1-dev \
    libcurl4-openssl-dev libxml2-dev \
    libharfbuzz-dev libfribidi-dev \
    libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
    && R -e "install.packages(c('digest', 'devtools'), repos = 'https://mirrors.tuna.tsinghua.edu.cn/CRAN/')" \
    && R -e "devtools::install_github('gsx-ucas/QRAP', upgrade = 'always', repos = 'https://mirrors.tuna.tsinghua.edu.cn/CRAN/')"

WORKDIR /home/QRAP_WD
EXPOSE 3030 3838 3939 

CMD R -e "QRAP::startQRAP(port = 3838)"
