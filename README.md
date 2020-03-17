# DefCalc
DASD tool, designed to provide a central source for commonly used indices.
Enables users to view, download and upload indices; and inflate, deflate and discount financial values.

### Getting Started
This README file will provide developers with an overview of the project works.

### Prerequisites
Developers **MUST** have access to:
* MOJ-Analytical-Services organisation on Github, and the following repositories:
  * DefCalc; provides the code necessary to run the app
  * airflow-DefCalc; provides the code necessary to auto-scrape the file(s) to update the app's data source
  * airflow-dags; required to allow airflow-DefCalc to run globally using MOJ airflow (rather than localised).
* MOJ Analytical Platform:
  * RStudio
  * Webapp(s):
    * Defcalc
  * Webapp data source(s):
    * alpha-app-defcalc
  * Knowledge of R & Rshiny is strongly recommended
    
### Installing
To link the project to RStudio, it is recommended to clone the repositories using their respective SSH keys.

### Key Files
Within the DefCalc repository, the following are key files:
* ui.R; contains the code for producing the user interface of the app
* server.R; contains the bulk of the code for the app. This includes generating all of the table(s), manipulating the data, download functionality etc. It is set out loosely by 'section' such that each tool has its own scope
* idx.R; takes the data from the AWS S3 bucket (alpha-app-defcalc) and then cleans it, and creates some global functions for use in ui.R & server.R
* www; contains several pages used to generate the guidance displayed within the app for users
* deploy.json; describes the parameters required for access (i.e. everyone, approved users only, DOM1 users...)
* packrat; details all packages/dependencies required for the app to load

Within the airflow-DefCalc repository, the following are key files:
* ds.R; contains the code that scrapes the website(s) specified for the data required to feed into the app, and saves it to the AWS S3 bucket (alpha-app-defcalc)
* iam_policy.json; provides persmissions/access for the airflow to read/write in the AWS S3 bucket (alpha-app-defcalc)
* packrat; details all packages/dependencies required for the app to load

Within the airflow-dags repository, the following are key file(s):
* defcalc_obr_scraper_global.py; contains the code to ensure that the code within airflow-Defcalc is automatically ran at specified time intervals

### Deployment
When ready to deploy new releases, newly published releases should adhere to the following versioning formats: https://semver.org/
