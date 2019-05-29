# HIRMEOS-dashboard
A preliminary proof-of-concept for a dashboard that displays usage metrics with a Shiny app.

For local data to be read in correctly, it should be in a .csv format with at least five named columns:

meausure_id (see https://metrics.operas-eu.org/measures; an example is "https://metrics.operas-eu.org/classics-library/sessions/v1")
timestamp (as date, formatted as yyyy-mm-dd)
work_uri (as "info:doi:theDOIof/thework.ofinterest")
country_uri (as "urn:isso:std:3166:-2:XX" where XX is the 2-letter iso code)
value (a number, representing number of views/downloads, etc)