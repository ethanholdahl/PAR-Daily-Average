---
title: "PAR data methods"
author: 
  name: Ethan Holdahl
  affiliation: University of Oregon
  date: "04 March 2020"
output: 
  html_document:
    theme: flatly
    highlight: haddock 
    # code_folding: show
    toc: yes
    toc_depth: 4
    toc_float: yes
    keep_md: true
---





```r
## Load and install the packages that we'll be using today
if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf, tidyverse, hrbrthemes, lwgeom, rnaturalearth, maps, mapdata, spData, tigris, tidycensus, leaflet, tmap, tmaptools)
## My preferred ggplot2 plotting theme (optional)
theme_set(hrbrthemes::theme_ipsum())
```

## Introduction

In this document I will outline my methods used for calculating daily totals of photosynthetically active radiation (PAR) at a given location from incident PAR values. Photosnythetically active radiation is solar radiation in the 400-700 nm spectral range which plants can use for photosynthesis. The data I use is the GLASS PAR product sourced from the Global Land Cover Facility. The GLASS PAR product had 5km and 3 hour spatial and temporal resolution and contained incident radiation values from 2008-2010. The citation for this data is:(insert a click to show button) Shunlin Liang, Xiaotong Zhang. Global Land Surface Products: Photosynthetically Active Radiation Data Collection(2008-2010). Bejing Normal University, 2012. doi:10.6050/glass863.3005.db

## Existing methods

There already exist methods for calculating daily PAR values. Liang et al. (Liang S., Zhang X., Xiao Z., Cheng J., Liu Q., Zhao X. (2014) Incident Photosynthetic Active Radiation. In: Global LAnd Surface Satellite (GLASS) Products. SpringerBriefs in Earth Sciences. Springer, Cham) has published a book containing algorithims for a variety of different GLASS products. Chapter 6 focuses on calculating daily PAR from the dataset we are using. They cite Wang et al. (2010) with the algorithms they use. Given any observed instantaneous PAR $InstPAR(T_{overpass})$ at time $T_{overpass}$ they interpolate to calculate $InstPAR(t)$ at any time of day with:

$$InstPAR(t)=InstPAR(T_{overpass}) \cfrac{\sin(\frac {(t-T_{sunrise)}*\pi}{T_{sunset}-T_{sunrise}})}{\sin(\frac {(T_{overpass}-T_{sunrise)}*\pi}{T_{sunset}-T_{sunrise}})}$$

After obtaining the PAR at any time they can integrate from sunrise to sunset to calculate the PAR for the day. If there is more than one observation in the day then they calculate $InstPAR(t)$ as follows (quoted from liang et al.):

Consider an example of two observations within one day at $T_1$ and $T_2$ respectively. From sunrise to $T_1$, PAR takes on the value of $InstPAR_{T_1}(t)$, and from $T_2$ to sunset, PAR has the value of $InstPAR_{T_2}(t)$. Between $T_1$ and $T_2$, the PAR value is calculated by the weighted average of two sinusoidally interpolated values:
 
$$InstPAR(t) = \cfrac{T_2-t}{T_2-T_1} InstPAR_{T_1}(t)+\cfrac{t-T_1}{T_2-T_1} InstPAR_{T_2}(t)$$

These algorithms work great especially near the equator between sunrise and sunset. However, when applying these algorithms to the dataset I noticed irregularities in the polar regions and in areas and times where there was an observation very close to sunrise or sunset. In the next section I will describe the mechanics of how PAR is transmitted from the sun to the surface of the earth and how that confirms my hesitations when applying these algorithms.

## PAR mechanics

<img src="Sun-Diagram-Solo.PNG" width="60%" style="display: block; margin: auto;" />

<img src="Sun-Diagram-Zoomed.PNG" width="40%" style="display: block; margin: auto;" />

We know that the amount of radiation emmited from the sun, the solar constant, is 1361 watts per square meter on a surface perpendicluar to the rays. Since this is a constant that implies that there is a limit to the amount of radiation a square meter on earth recieves and that it changes throughout the day. This is directly related to the elevation angle of the sun. Looking at the diagrams above it is clear that a ray of width 1 is projected to a length of x on the surface where $x = \frac {1}{\sin(\theta)}$. So, it follows that the amount of radiation that surface recieves is $\frac {1}{x} = \sin(elevation)$. Of course we have an atmosphere, ocassionally cloud cover and other obstacles which limit the fraction of this theoretically maximal amount of radiation that can reach the surface at any elevation.

The other concerning point about having observations near sunrise and sunset is that when radiation passes through the atmosphere it is refracted. Under standard temperature and pressure conditions refraction only adjusts apparent solar elevation by .6 degrees at around 0 degrees elevation and quickly and exponentially approaches 0 degrees of refraction. However, with spatial resolution as fine as we have in this data set that means every day we have observations that pick up positive PAR values before and after our calculated times for sunrise and sunset respectively. If we do not account for this and blindly plug our values into the algorithm provided above then our interpolated PAR values are negative.


## Ratio method

So ideally when we interpolate PAR values based off incident values we want to first calculate the refraction adjusted elevation then calculate the ratio of the maximum possible raditaion that is reacing the surface. Once we have that we can linearlly interpolate that ratio throughout the day and apply it to the theoretical maximum throughout the day based on the refraction adjusted elevation. However, adjusting every solar elevation thoughout the day for refraction is marginally very computationally expensive for almost no change in elevation angle so for practicality I do not adjust for refraction during my interpolation, however I retain the practice for calculating the incident ratio. 

Generally we can estimate the incident PAR for unobserved times by taking a weighted average of the PAR ratios of the nearest observations and multiplying that by the sin of the sun's elevation angle. PAR ratios are calculated as incident value/sin(elevation angle). The weighted average being based on how close the unobserved incident is to the nearest values. So if there exists 2 observations at times $T_1$ and $T_2$ with $R_1$ and $R_2$ as the respective ratios, the estimated incident PAR at time $t$ where $T_1<t<T_2$ is $InstPAR(t) = (R_2 * \frac{t-T_1}{T_2-T_1}+R_1*\frac{T_2-t}{T_2-T_1})*\sin(elevation(t))$. We can estimate PAR between observations by integrating the above equation from $T_1$ to $T_2$ with respect to $t$, and because the calculation is linear we can separate them and calculate the contribution to daily PAR that each observation had separately.

Below I dissect the functional form of $\sin(elevation(t))$ based off NOAA's calculation of solar elevation.

$$\sin(elevation(t))=\sin(90-\arccos(\sin(Lat)*\sin(SunDecline)+\cos(Lat)*\cos(Sundecline)*\cos(t*360+EqOfTime/4+Lon+180))$$
$$=\cos(Arccos(\sin(Lat)*\sin(SunDecline)+\cos(Lat)*\cos(Sundecline)*-\cos(t*360+EqOfTime/4+Lon))$$
$$=\sin(Lat)*\sin(SunDecline)-\cos(Lat)*\cos(Sundecline)*\cos(t*360+EqOfTime/4+Lon)$$
Since SunDecline and EqOfTime are approximately constant during a given day and since Lat and Lon don't change at a location, if we set 

$\sin(Lat)*\sin(SunDecline)=A$,

$\cos(Lat)*\cos(Sundecline)=B$ and 

$EqOfTime/4+Lon=C$ we get 

$\sin(elevation(t))=A-B*\cos(t*360+C)$ with $A,B,C$ as day and location specific constants which makes the integration easy.

Because atmospheric refraction is a function of many variable factors the calculated sunset and sunrise times for a given latitude and longitude is not always the same as the first and last light. This problem reveals itself when we have a PAR observation before sunrise or after sunset. Refraction also becomes an issue when we have an observation just seconds after sunset where the sun may, due to refraction, appear to be up to a degree or so higher in the sky than where we calculate it to be. This can lead to calculated ratios higher than the theoretical maximum or negative ratio calculations. I meet these infrequent occurrances, which are easily identified, with a linear contingent plan. In these cases we ignore solar position and assume a linear relationship between the PAR of the troublesome observation and its neighboring observations. This is the same method used by the authors of the GLASS PAR data product for all observations. However, I believe my methods to be an improvement in cases where the ratio is well behaved (every time the observation occurs outside of sunrise and sunset times including most times near sunrise and sunset) as it is effectively the same estimator when the solar path takes the sun to a 90 degree elevation angle (only when the sun passes directly over a location), and a more accurate estimator as we get closer to the polar regions.

## Interactive plots


## Data

Put but input and output data here.
