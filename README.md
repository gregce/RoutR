## Public version of the source code
This repo is a public version of the source code for the [RoutR.nyc app](http://routr.nyc). The app is built with R, JavaScript and Shiny. If you got this far, we have to assume you can look after yourself with R and navigating this GitHub repo; we can’t give detailed guidance or help. 

The app itself is contained in the `./shiny` folder and can be run from the root directory with:

```R
shiny::runApp("app.R")
```

Prior to the App's creation, much work was done to clean and integrate the data necessary to make the vizualization. A Redshift database was spun up to help facilitate this process given that the total size of the original database was ~300GB. 

These sql scripts can be found in the `./sql` folder. The massaged and cleaned data ultimatley used can be downloaded directly [here](https://www.dropbox.com/s/f32skfj8qh259af/alltrips.rda?dl=0) as an `.Rdata` file

The app runs on a single node t2 large EC2 instance. Please be gentle :)
