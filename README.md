<h1><center>  Shiny App for Student's Ratings of Psychotherapy </h1></center>
<h2><center> Clinical Behavioral Methods at CLU (PSYC 523/PSYD 745)</h2></center>

Graduate students in clinical psychology watch videos of psychotherapy from a vareity of theoretical orienations within the cognitive-behavioral tradition. Students complete the [Comparative Psychotherapy Process Scale](http://supp.apa.org/psycarticles/supplemental/pst_42_3_340/pst_hilsenroth0110_web_supplement.pdf) by Hilsenroth and rate a series of videos.  Shiny app is hosted by shinapps.io at [https://jdbedics213.shinyapps.io/SP2019-745-Vidoes/](https://jdbedics213.shinyapps.io/SP2019-745-Vidoes/). 

### Using Google Forms

Students complete the CPPS using a Google Form:

<img src="https://raw.githubusercontent.com/jdbedics/shiny-app-psychotherapy-ratings/master/form.png" width="500" height="300" align="middle"  />

### Data Processing

The data from the form are saved as a Google Sheet in a Google Drive and accessed with the [Google Sheets](https://github.com/jennybc/googlesheets) package from Jenny Bryan.


### 1. The Pattern Profile of CPPS Items by Raters with Plotly

![](https://raw.githubusercontent.com/jdbedics/shiny-app-psychotherapy-ratings/master/pattern.png)

### 2. A visualization of Kappas

![](https://github.com/jdbedics/shiny-app-psychotherapy-ratings/blob/master/kappa.png)



