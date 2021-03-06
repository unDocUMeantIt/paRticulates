# paRticulates

Import sensor data on particulate matter as provided by madavi.de, and plot the imported data by time range.
Calculate hourly means and several air quality indices (CITEAIR's [CAQI](https://www.airqualitynow.eu/about_indices_definition.php); EEA's [EAQI](https://www.eea.europa.eu/themes/air/air-quality-index); EPA's [AQI](https://airnow.gov/index.cfm?action=aqibasics.aqi) and NowCast).
Early development, further features might possibly be added...

More information on paRticulates is available on the [project homepage](https://reaktanz.de/?c=hacking&s=paRticulates).

## Installation

### Installation via GitHub

To install the package directly from GitHub, you can use `install_github()` from the [devtools](https://github.com/hadley/devtools) package:

```r
devtools::install_github("unDocUMeantIt/paRticulates") # stable release
devtools::install_github("unDocUMeantIt/paRticulates", ref="develop") # development release
```

## License

paRticulates Copyright (C) 2020 Meik Michalke, released under the
GNU General Public License (GPL) version 3 or (at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.

You should have received a copy of the license with the
source package as the file COPYING or LICENSE.
