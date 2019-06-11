---
output:
  html_document: default
  pdf_document: default
---
# HIRMEOS-dashboard

This guide will help you deploy a Shiny dashboard that displays book usage metrics. You will need a GitHub account, a Docker account and a ShinyApps.io account.

---

1. You'll need Docker installed. [You can get it here](https://docs.docker.com/install/). A helpful guide to using Docker and R is available [here](https://www.symbolix.com.au/blog-main/r-docker-hello).
2. Next, clone/fork the Github repository.
3. At this stage, the dashboard will require a file named `metrics.csv` to replace to sample data. It should be added to the `data` folder. This `.csv` file should contain at least five named columns:
  * `measure_id` (all measures are listed [here](https://metrics.operas-eu.org/measures); an example is `https://metrics.operas-eu.org/classics-library/sessions/v1`)
  * `timestamp` (as date, formatted as `yyyy-mm-dd`)
  * `work_uri` (as `info:doi:theDOIof/thework.ofinterest`)
  * `country_uri` (as `urn:isso:std:3166:-2:XX` where XX is the 2-letter iso code)
  * `value` (a number, representing number of views/downloads, etc.)
4. From the command line, set your directory to the folder you downloaded from Github. It must contain the `Dockerfile`.
5. Now run `docker build --rm --force-rm -t rstudio/hirmeos .`. Note the full stop. This will build your Docker image, which will contain a stable version of R, RStudio and the tidyverse package. You can check that it worked by running `docker image list`.
6. We want the `data` folder to be available inside the container; that way we can import and export our data easily. To do this, first set `DATA_DIR=${PWD}/data`, then spin up an instance of RStudio by running `docker run -d --rm -p 28787:8787 --name hirmeos-dashboard -e USERID=$UID -e PASSWORD=hirmeos -v $DATA_DIR:/home/rstudio/data rstudio/hirmeos`
7. You can now use RStudio by opening your browser and visiting the address `127.0.0.1:28787`. The username is `rstudio` and the password, as set above, is `hirmeos`. You can set the password to whatever you like.
8. With RStudio now open, there are two optional steps:
 * If your file `metrics.csv` included DOIs but not titles, you can retrieve the titles by running `source(get_metadata.R)` in the console. This will edit your `.csv` and append titles data.
 * In future, if you need altimetric data (like tweets), you will be able to call `source(get_altmetrics.R)` in the console; this will write a new file called `altmetrics.csv`.
 9. Now you're ready to run the app. Still in RStudio (in your browser), open app.R. At the top-right of the script, hit `Run App` (or click the drop-down and select `Run in Window`). You may be prompted that the browser prevented a pop-up. Allow pop-up windows for this address, and check that the app is working normally. If a new page opens but says `Connection refused`, close your browser windows, access `127.0.0.1:28787` in a new window, and try again.
 10. From within the app, you can press `Publish` in the top-right corner. If prompted to download some extra packages to allow this, do so. You will then be guided through the steps to publish the app. Only publish the `app.R` script, the `data` folder and any logos in the `www` folder. Sign into your ShinyApps.io account and follow the prompts to get a token. Once you give RStudio the token, the app will be deployed at your url. Simply visit the url and verify that it works. 

---

And that should be all! To stop the container, go back to the command line and run `docker stop hirmeos-dashboard`. 
