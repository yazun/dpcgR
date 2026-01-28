# dpcgR utility package.

Helps with DB related task:
* Connecting to DB
* Fetching Runs data and visualizing it in R using javscript highcharter package
* Standardizes visualization of Aitoff skymaps, including background data
* Allows to share the skymaps and other plots to Gaia Mattermost.

```R
require(devtools)
install_github("yazun/dpcgR", force= TRUE)
# if needed
reload(pkgload::inst("dpcgR"))

```

