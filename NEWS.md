# burro 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Used colorblind safe palette for `visdat` summary.
* Added `geom_size` as visualization of cross tables
* Fixed error with data dictionary output. There is also a better example of using a data dictionary for `explore_data()`

# burro 0.2.0

* Converted the dashboard from `shinydashboard` to `flexdashboard`
* `skimr` output appearance is improved
* Converted shiny code to shiny modules
* `explore_data()` doesn't crash when an all numeric `data.frame` is input now
