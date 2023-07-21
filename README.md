# DefCalc
Data and Analysis tool, designed to provide a central source for commonly used indices.
Enables users to view, download and upload indices; and inflate, deflate and discount financial values.

### Getting Started
This README file will provide developers with an overview of the project works.

### Prerequisites
Developers **MUST** have access to:
* MOJ-Analytical-Services organisation on Github, and the following repositories:
  * DefCalc; _provides the code necessary to run the app_
 
* MOJ Analytical Platform:
  * RStudio
  * Webapp(s):
    * Defcalc
  * Webapp data source(s):
    * alpha-app-defcalc
    
_Knowledge of R & Rshiny is strongly recommended._
    
### Installing
To link the project to RStudio, it is recommended to clone the repositories using their respective SSH keys.

### Key Files
Within the DefCalc repository, the following are key files:
* ui.R; _contains the code for producing the user interface of the app_
* server.R; _contains the bulk of the code for the app. This includes generating all of the table(s), manipulating the data, download functionality etc. It is set out loosely by 'section' such that each tool has its own scope_
* www; _contains several pages used to generate the guidance displayed within the app for users_
* renv.lock; _details all packages/dependencies required for the app to load_

### Deployment
When ready to deploy new releases, newly published releases should adhere to the following versioning formats: https://semver.org/
