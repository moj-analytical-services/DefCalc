FROM quay.io/mojanalytics/rshiny:3.5.1

ENV PATH="/opt/shiny-server/bin:/opt/shiny-server/ext/node/bin:${PATH}"
ENV SHINY_APP=/srv/shiny-server
ENV NODE_ENV=production

WORKDIR /srv/shiny-server

# ENV SHINY_GAID <your google analytics token here>

# Install python3 and essentials 
RUN apt-get update
RUN apt-get install -y python3 python3-pip python3-venv python3-dev
# Make sure reticulate uses the system Python
ENV RETICULATE_PYTHON="/usr/bin/python3"

# use renv for packages
ADD renv.lock renv.lock
RUN R -e "install.packages('renv')"
RUN R -e 'renv::restore()'

# Add Python package requirements and install
COPY requirements.txt .
RUN python3 -m pip install -r requirements.txt

# Add shiny app code
ADD . .

USER shiny
CMD analytics-platform-shiny-server
EXPOSE 9999
