FROM 593291632749.dkr.ecr.eu-west-1.amazonaws.com/rshiny:local

ENV PATH="/opt/shiny-server/bin:/opt/shiny-server/ext/node/bin:${PATH}"
ENV SHINY_APP=/srv/shiny-server
ENV NODE_ENV=production

WORKDIR /srv/shiny-server

# ENV SHINY_GAID <your google analytics token here>

RUN npm i -g ministryofjustice/analytics-platform-shiny-server#v0.0.5

# Install python3 and essentials 
RUN apt-get update -y && \
  apt-get install -y python3 python3-pip python3-venv && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/*

# use renv for packages
ADD renv.lock renv.lock
RUN R -e "install.packages('renv')"
RUN R -e 'renv::restore()'

# Add Python package requirements and install
COPY requirements.txt requirements.txt
RUN pip3 install -r requirements.txt

# Add shiny app code
ADD . .

USER shiny
CMD analytics-platform-shiny-server
EXPOSE 9999
