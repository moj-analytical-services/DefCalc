FROM quay.io/mojanalytics/rshiny:3.5.1 
2 
 
3 ENV PATH="/opt/shiny-server/bin:/opt/shiny-server/ext/node/bin:${PATH}" 
4 ENV SHINY_APP=/srv/shiny-server 
5 ENV NODE_ENV=production 
6 
 
7 WORKDIR /srv/shiny-server 
8 
 
9 # ENV SHINY_GAID <your google analytics token here> 
10 
 
11 # Add environment file individually so that next install command 
12 # can be cached as an image layer separate from application code 
13 ADD environment.yml environment.yml 
14 
 
15 # Install packrat itself then packages from packrat.lock 
16 RUN conda env update --file environment.yml -n base 
17 RUN npm i -g ministryofjustice/analytics-platform-shiny-server#v0.0.3 
18 
 
19 ## ----------------------------------------------------- 
20 ## Uncomment if still using packrat alongside conda 
21 ## Install packrat itself then packages from packrat.lock 
22 # ADD packrat packrat 
23 # RUN R -e "install.packages('packrat'); packrat::restore()" 
24 ## ------------------------------------------------------ 
25 
 
26 # Add shiny app code 
27 ADD . . 
28 
 
29 USER shiny 
30 CMD analytics-platform-shiny-server 
31 EXPOSE 9999 
