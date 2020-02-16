FROM rocker/tidyverse:3.5.1

## Copy requirements.R to container directory /tmp
COPY ./DockerConfig/requirements.R /tmp/requirements.R 
## install required libs on container
RUN Rscript /tmp/requirements.R


# create an R user
ENV USER rstudio


## Copy your working files over. Alternatively, when writing the run command, you can
## set them as volumes if you're happy to write back to your local files
## The $USER defaults to `rstudio` but you can change this at runtime
COPY ./app.R /home/$USER/app.R
## COPY ./get_altmetrics.R /home/$USER/get_altmetrics.R
COPY ./get_metadata.R /home/$USER/get_metadata.R
COPY ./helper.R /home/$USER/helper.R
COPY ./set_up.R /home/$USER/set_up.R
COPY ./data /home/$USER/data
COPY ./www /home/$USER/www



##CMD cd /home/$USER \
 ## && R -e "source('app.R')"