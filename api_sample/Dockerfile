FROM rocker/shiny:4.4.1

# 1. Instalar dependencias del sistema operativo
RUN apt-get update && apt-get install -y \
    # Dependencias geoespaciales (Requeridas por 'sf' / 'epiR')
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libsqlite3-dev \
    libudunits2-dev \
    # Dependencias comunes de red/datos en R
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    # Dependencias de fuentes y renderizado
    libharfbuzz-dev \
    libfribidi-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    # Otras dependencias
    curl \
    gfortran \
    liblapack-dev \
    libblas-dev \
    libicu-dev \
    # Limpiar la caché de apt para aligerar la imagen
    && rm -rf /var/lib/apt/lists/*

# 2. Instalar paquetes de R usando binarios precompilados
# El flag --error asegura que si epiR (o cualquier otro) falla, el build se detenga.
RUN install2.r --error --skipinstalled \
    rlang \
    lifecycle \
    vctrs \
    shiny \
    bslib \
    shinythemes \
    ggplot2 \
    DT \
    shinyjs \
    dplyr \
    rmarkdown \
    knitr \
    kableExtra \
    scales \
    magrittr \
    epiR \
    httr \
    jsonlite \
    stargazer \
    irr \
    psych

# 3. Copiar archivos de la app con los permisos correctos
COPY --chown=shiny:shiny . /srv/shiny-server/

# 4. Configurar permisos de logs y servidor
USER root
RUN mkdir -p /var/log/shiny-server && \
    chown -R shiny:shiny /var/log/shiny-server && \
    chmod -R 755 /var/log/shiny-server && \
    chown -R shiny:shiny /srv/shiny-server/

# 5. Cambiar al usuario sin privilegios para ejecutar la app
USER shiny

# 6. Exponer puerto y definir Healthcheck
EXPOSE 3838

HEALTHCHECK --interval=30s --timeout=10s --start-period=15s --retries=3 \
    CMD curl -f http://localhost:3838/ || exit 1

# 7. Ejecutar Shiny Server
CMD ["/usr/bin/shiny-server"]