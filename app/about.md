#### Purpose
This Shiny app allows to visualize covid19 data that originates from the Johns Hopkins University's (JHU) [Coronavirus resource center](https://coronavirus.jhu.edu/map.html).

#### Data
The data visualized in the app originates from the [Johns Hopkins University github page](https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports).

Populatino data [originates from the World Bank](https://data.worldbank.org/indicator/SP.POP.TOTL). I used the data for 2018 - the most recent data available at the time of writing.

#### Mechanics
* JHU uploads data to the github page every day at approx 00UTC.
* I 'subscribe' to the data through svn, and run a bash script that contains the `svn update` command. This script runs multiple times daily, as to capture any data updates from JHU. For this, I rent a Ubuntu droplet at Digital Ocean.
* The JHU data is post-processed prior to visualization:
	* In some cases, countries are inconsistently named ('Bahamas' and 'Bahamas, The' is one example). I correct this if/when I am aware of this.
	* JHU data is available for within-country regions. I aggregate over these regions; the app is only able to show data at a country level.
	* From 'cumulative' data, I compute daily data.
	* The data is cast in a ['tidy'](https://vita.had.co.nz/papers/tidy-data.pdf) frame and saved to Dropbox as an (R bespoke)RDS file.
	* The app reads the data from Dropbox. 'Relative dates' are compputed on-the-fly.

#### How to
Upon opening, the app shows the cumulative number of confirmed cases in The Netherlands.

Four `variables` may be plotted. One can plot multiple variables in a single plot.
* **Confirmed**: the number of reported covid19 cases, irrespective of whether the patients are recovered or not.
* **Deaths**: the number of reported fatalities believed to be linked to covid19
* **Recovered**: the number of reported cases of recovery
* **Active**: the number of currently carrying covid19 patients.

The data may be shown as cumulative or daily/new cases using the `Data type` dropdown.

The types of `vertical scale` include 'linear' and various log scales. The latter allow for showing a wider range of data in a compact way.

**Relative data**
This option allows to show country numbers relative to that country's population. Note that this is an imperfect measure, as the country may be a lot larger than the affected area and simply dividing by the population of an entire country is not a meaningful measure (e.g. think of Wuhan province in China).

**Relative dates**
In some countries, the covid19 virus spread earlier than in others. In order to compare growth rates, the app allows for visualization on a 'relative scale'. For example, one can choose to show curves for multiple countries where each curve starts when the number of confirmed cases first exceeded (or was equal to) 100. This can be done by:
* Check the `plot using relative dates` option.
* Set the threshold variable to 'Confirmed'
* Set the threshold number to 100.

There is an option to set a criterium in absolute terms as well as in relative terms. The latter criterium refers to the numbers relative to a country's population size. In the threshold box, it is expressed as a fraction - not as a percentage. If you want to set a threshold at 0.01% then you should therefore input this as 0.0001.

#### Credits
This Shiny app was developed (in a private capacity) by [Jan Verkade](mailto:jan.verkade@gmail.com). 

**Tools used**
* [Ubuntu](https://ubuntu.com/) and [GNU bash](https://www.gnu.org/software/bash/)
* [R](https://www.r-project.org/), [RStudio](https://rstudio.com/) and various R libraries including ggplot2, shiny, Rcurl, dplyr, lubridate
* [subversion](https://subversion.apache.org/)
* [ghostwriter](https://wereturtle.github.io/ghostwriter/)