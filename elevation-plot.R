library(ggplot2)
##Fix error for days with no positive elevation 
Latitude = -80
Longitude = 0
year = 2008
day = 5
t=.5
Sunrise = 0 #initializing for global assignment
Sunset = 0 #initializing for global assignment

elevation = function(t,Latitude,Longitude,year,day){
  #Converting the year and day to Julian Day and Century. 2454466.5 is
  #12:00 AM of Jan 1 2008. Using mid day as the average 2454467 is Jan 1 2008
  #ifelse(year == 2008, JulianDay = 2454466+day, JulianDay = 2454467+day+(year-2008)*365);
  JulianDay = 2454466+day;
  ##Calculate solar position constants for the day in question. These
  ##functions were obtained from an NOAA Solar Calculations spreadsheet.
  ##Note: the variation seen throughout a given day is so small that we can
  ##treat these as constants. More on this later.
  JulianCentury = (JulianDay-2451545)/36525;
  GeomMeanLongSunDeg = (280.46646+JulianCentury*(36000.76983+JulianCentury*.0003032))%%360;
  GeomMeanAnomSunDeg = 357.52911+JulianCentury*(35999.05029 - 0.0001537*JulianCentury);
  EccentEarthOrbit = 0.016708634-JulianCentury*(0.000042037+0.0000001267*JulianCentury);
  SunEqOfCtr = sin(pi/180*(GeomMeanAnomSunDeg))*(1.914602-JulianCentury*(0.004817+0.000014*JulianCentury))+sin(pi/180*(2*GeomMeanAnomSunDeg))*(0.019993-0.000101*JulianCentury)+sin(pi/180*(3*GeomMeanAnomSunDeg))*0.000289;
  SunTrueLongDeg = GeomMeanLongSunDeg+SunEqOfCtr;
  SunAppLongDeg = SunTrueLongDeg-0.00569-0.00478*sin(pi/180*(125.04-1934.136*JulianCentury));
  MeanObliqEclipticDeg = 23+(26+((21.448-JulianCentury*(46.815+JulianCentury*(0.00059-JulianCentury*0.001813))))/60)/60;
  ObliqCorrDeg = MeanObliqEclipticDeg+0.00256*cos(pi/180*(125.04-1934.136*JulianCentury));
  SunDeclineDeg = 180/pi*(asin(sin(pi/180*(ObliqCorrDeg))*sin(pi/180*(SunAppLongDeg))));
  vary = tan(pi/180*(ObliqCorrDeg/2))*tan(pi/180*(ObliqCorrDeg/2));
  EqOfTimeMinutes = 4*180/pi*(vary*sin(2*pi/180*(GeomMeanLongSunDeg))-2*EccentEarthOrbit*sin(pi/180*(GeomMeanAnomSunDeg))+4*EccentEarthOrbit*vary*sin(pi/180*(GeomMeanAnomSunDeg))*cos(2*pi/180*(GeomMeanLongSunDeg))-0.5*vary*vary*sin(4*pi/180*(GeomMeanLongSunDeg))-1.25*EccentEarthOrbit*EccentEarthOrbit*sin(2*pi/180*(GeomMeanAnomSunDeg)));

  ## Define maximum PAR ratio: Solar Constant*.487(%of radiation that makes up PAR)*scale(100)/Sin(90)
  MaxRatio=.487*1361*100;
  srssangle = 90;
  HASunriseDeg = Re(180/pi*(acos(cos(pi/180*(srssangle))/(cos(pi/180*(Latitude))*cos(pi/180*(SunDeclineDeg)))-tan(pi/180*(Latitude))*tan(pi/180*(SunDeclineDeg)))));
  SunlightDuration = HASunriseDeg/180;

  A = sin(Latitude*pi/180)*sin(SunDeclineDeg*pi/180);
  B = cos(Latitude*pi/180)*cos(SunDeclineDeg*pi/180);

  SolarNoon = (720-4*Longitude-EqOfTimeMinutes+0*60)/1440;
  assign("Sunrise", ((SolarNoon*1440-HASunriseDeg*4)/1440)%%1, envir = .GlobalEnv); #keep sunrise positive and between zero and 1
  assign("Sunset", Sunrise+SunlightDuration, envir = .GlobalEnv); #assign SR/SS to global for use in other functions

  C = EqOfTimeMinutes/4+Longitude;

  #90-(acos(sin(Latitude*pi/180)*sin(SunDeclineDeg*pi/180)+cos(Latitude*pi/180)*cos(SunDeclineDeg*pi/180)*cos((t*360+EqOfTimeMinutes/4+Longitude+180)*pi/180)))*180/pi
  return(90-(acos(A+B*cos((t*360+C+180)*pi/180)))*180/pi)
    
}

elevation(t,Latitude,Longitude,year,day)

t = seq(Sunrise-1/8,Sunset+1/8,1/(8*100))
ggplot(data = NULL, aes(x = t*60*60*24, y = elevation(t,Latitude,Longitude,year,day)))+
  coord_cartesian(ylim = c(-10,90))+
  geom_line()+
  scale_x_time()
            
ggplot(data = NULL, aes(x = 24*60*60*t, y = sin(elevation(t,Latitude,Longitude,year,day)*pi/180)))+
  coord_cartesian(ylim = c(-.1,1))+
  geom_line()+
  scale_x_time()

ggplot(data = NULL, aes(x = t*24*60*60, y = elevation(t,Latitude,Longitude,year,day)/90, col = "elevation"))+
  coord_cartesian(ylim = c(-.1,1))+
  geom_line()+
  geom_line(data = NULL, aes(x = 24*60*60*t, y = sin(elevation(t,Latitude,Longitude,year,day)*pi/180), col = "sin(elevation)"))+
  labs()+
  scale_x_time()





###Creating projections from each observation

#Max PAR: .487*1361/sin(elevation)
library(tidyverse)

time = c(0,1/8,2/8,3/8,4/8,5/8,6/8,7/8)
time = time+floor(Sunrise*8)/8
observations = tibble(time, elevation(time, Latitude, Longitude, year, day))
observations = observations %>%
  select(time, elevation = `elevation(time, Latitude, Longitude, year, day)`)

daylightObservations = observations %>%
  mutate(max_PAR = .487*1361*sin(elevation*pi/180)) %>%
  filter(elevation>0)

daylightObservations = daylightObservations %>%
  mutate(percentMax = max_PAR/max_PAR)
  
##add points to indicate Sunrise and Sunset
timeSS = c(Sunrise, Sunset)
observationsSS = tibble(timeSS, elevation(timeSS, Latitude, Longitude, year, day))
observationsSS = observationsSS %>%
  select(time = timeSS, elevation = `elevation(timeSS, Latitude, Longitude, year, day)`) %>%
  mutate(max_PAR = .487*1361*sin(elevation*pi/180))

ggplot(data = NULL, aes(x = 24*60*60*t, y = sin(elevation(t,Latitude,Longitude,year,day)*pi/180)))+
  #coord_cartesian(ylim = c(-.1,1))+
  geom_line()+
  scale_x_time()+
  geom_point(data = daylightObservations, aes(x = time*24*60*60, y = max_PAR/(.487*1361), color = percentMax))+
  scale_color_viridis_c(option = "C")+
  geom_point(data = observationsSS, aes(x = time*24*60*60,  y = max_PAR/(.487*1361)), color = 2)+
  theme_minimal()
  
#add SR/SS to daylight obs?

###Creating a function that will plot the Wang et al. algorithm for each point

wangInstPAR = function(t, daylightObservations){
  l = dim(daylightObservations)[1]
  if (l == 0){
    return(0)
  }
  wangt = tibble(t) %>%
    mutate(notInDay = t < Sunrise | t > Sunset)
  if (l >= 1){
    wangt = wangt %>%
      mutate(overpass1 = as.numeric(daylightObservations[1,1]),
             PAR1 = as.numeric(daylightObservations[1,3]))
  }
  if (l >= 2){
    wangt = wangt %>%
      mutate(overpass2 = as.numeric(daylightObservations[2,1]),
             PAR2 = as.numeric(daylightObservations[2,3]))
  }
  if (l >= 3){
    wangt = wangt %>%
      mutate(overpass3 = as.numeric(daylightObservations[3,1]),
             PAR3 = as.numeric(daylightObservations[3,3]))
  }
  if (l >= 4){
    wangt = wangt %>%
      mutate(overpass4 = as.numeric(daylightObservations[4,1]),
             PAR4 = as.numeric(daylightObservations[4,3]))
  }
  if (l >= 5){
    wangt = wangt %>%
      mutate(overpass5 = as.numeric(daylightObservations[5,1]),
             PAR5 = as.numeric(daylightObservations[5,3]))
  }
  if (l >= 6){
    wangt = wangt %>%
      mutate(overpass6 = as.numeric(daylightObservations[6,1]),
             PAR6 = as.numeric(daylightObservations[6,3]))
  }
  if (l >= 7){
    wangt = wangt %>%
      mutate(overpass7 = as.numeric(daylightObservations[7,1]),
             PAR7 = as.numeric(daylightObservations[7,3]))
  }
  if (l >= 8){
    wangt = wangt %>%
      mutate(overpass8 = as.numeric(daylightObservations[8,1]),
             PAR8 = as.numeric(daylightObservations[8,3]))
  }
  
  # create index and initialize instPAR column
  wangt = wangt %>%
    mutate(instPAR = round((t-Sunrise+1/8)*800+1))
  
  #identify times before sunrise
  group = wangt %>%
    filter(t<=Sunrise) %>%
    select(instPAR)
  #set instPAR equal to 0.
  wangt$instPAR[group$instPAR] = 0
  
  #identify times after sunset
  group = wangt %>%
    filter(t>= Sunset) %>%
    select(instPAR)
  #set instPAR equal to 0.
  wangt$instPAR[group$instPAR] = 0
  
  if (l >= 1){
    #identify times between sunrise and first observation
    group = wangt %>%
      filter(t>Sunrise & t<=overpass1) %>%
      select(instPAR)
    #calculate instPAR (eq. 6.2)
    wangt$instPAR[group$instPAR] = as_vector(wangt$PAR1*(sin((wangt$t-Sunrise)*pi/(Sunset-Sunrise))/sin((wangt$overpass1-Sunrise)*pi/(Sunset-Sunrise))))[group$instPAR]
  }
  
  if (l >= 2){
    #identify times between first and second observation
    group = wangt %>%
      filter(t>overpass1 & t<=overpass2) %>%
      select(instPAR)
    #calculate instPAR (eq. 6.4)
    wangt$instPAR[group$instPAR] = as_vector((wangt$overpass2-wangt$t)/(wangt$overpass2-wangt$overpass1)*(wangt$PAR1*(sin((wangt$t-Sunrise)*pi/(Sunset-Sunrise))/sin((wangt$overpass1-Sunrise)*pi/(Sunset-Sunrise))))+
                                               (wangt$t-wangt$overpass1)/(wangt$overpass2-wangt$overpass1)*(wangt$PAR2*(sin((wangt$t-Sunrise)*pi/(Sunset-Sunrise))/sin((wangt$overpass2-Sunrise)*pi/(Sunset-Sunrise)))))[group$instPAR]
  }
  
  if (l >= 3){
    #identify times between second and third observation
    group = wangt %>%
      filter(t>overpass2 & t<=overpass3) %>%
      select(instPAR)
    #calculate instPAR (eq. 6.4)
    wangt$instPAR[group$instPAR] = as_vector((wangt$overpass3-wangt$t)/(wangt$overpass3-wangt$overpass2)*(wangt$PAR2*(sin((wangt$t-Sunrise)*pi/(Sunset-Sunrise))/sin((wangt$overpass2-Sunrise)*pi/(Sunset-Sunrise))))+
                                               (wangt$t-wangt$overpass2)/(wangt$overpass3-wangt$overpass2)*(wangt$PAR3*(sin((wangt$t-Sunrise)*pi/(Sunset-Sunrise))/sin((wangt$overpass3-Sunrise)*pi/(Sunset-Sunrise)))))[group$instPAR]
  }
  
  if (l >= 4){
    #identify times between third and forth observation
    group = wangt %>%
      filter(t>overpass3 & t<=overpass4) %>%
      select(instPAR)
    #calculate instPAR (eq. 6.4)
    wangt$instPAR[group$instPAR] = as_vector((wangt$overpass4-wangt$t)/(wangt$overpass4-wangt$overpass3)*(wangt$PAR3*(sin((wangt$t-Sunrise)*pi/(Sunset-Sunrise))/sin((wangt$overpass3-Sunrise)*pi/(Sunset-Sunrise))))+
                                               (wangt$t-wangt$overpass3)/(wangt$overpass4-wangt$overpass3)*(wangt$PAR4*(sin((wangt$t-Sunrise)*pi/(Sunset-Sunrise))/sin((wangt$overpass4-Sunrise)*pi/(Sunset-Sunrise)))))[group$instPAR]
  }
  
  if (l >= 5){
    #identify times between forth and fifth observation
    group = wangt %>%
      filter(t>overpass4 & t<=overpass5) %>%
      select(instPAR)
    #calculate instPAR (eq. 6.4)
    wangt$instPAR[group$instPAR] = as_vector((wangt$overpass5-wangt$t)/(wangt$overpass5-wangt$overpass4)*(wangt$PAR4*(sin((wangt$t-Sunrise)*pi/(Sunset-Sunrise))/sin((wangt$overpass4-Sunrise)*pi/(Sunset-Sunrise))))+
                                               (wangt$t-wangt$overpass4)/(wangt$overpass5-wangt$overpass4)*(wangt$PAR5*(sin((wangt$t-Sunrise)*pi/(Sunset-Sunrise))/sin((wangt$overpass5-Sunrise)*pi/(Sunset-Sunrise)))))[group$instPAR]
  }
  
  if (l >= 6){
    #identify times between fifth and sixth observation
    group = wangt %>%
      filter(t>overpass5 & t<=overpass6) %>%
      select(instPAR)
    #calculate instPAR (eq. 6.4)
    wangt$instPAR[group$instPAR] = as_vector((wangt$overpass6-wangt$t)/(wangt$overpass6-wangt$overpass5)*(wangt$PAR5*(sin((wangt$t-Sunrise)*pi/(Sunset-Sunrise))/sin((wangt$overpass5-Sunrise)*pi/(Sunset-Sunrise))))+
                                               (wangt$t-wangt$overpass5)/(wangt$overpass6-wangt$overpass5)*(wangt$PAR6*(sin((wangt$t-Sunrise)*pi/(Sunset-Sunrise))/sin((wangt$overpass6-Sunrise)*pi/(Sunset-Sunrise)))))[group$instPAR]
  }
  
  if (l >= 7){
    #identify times between sixth and seventh observation
    group = wangt %>%
      filter(t>overpass6 & t<=overpass7) %>%
      select(instPAR)
    #calculate instPAR (eq. 6.4)
    wangt$instPAR[group$instPAR] = as_vector((wangt$overpass7-wangt$t)/(wangt$overpass7-wangt$overpass6)*(wangt$PAR6*(sin((wangt$t-Sunrise)*pi/(Sunset-Sunrise))/sin((wangt$overpass6-Sunrise)*pi/(Sunset-Sunrise))))+
                                               (wangt$t-wangt$overpass6)/(wangt$overpass7-wangt$overpass6)*(wangt$PAR7*(sin((wangt$t-Sunrise)*pi/(Sunset-Sunrise))/sin((wangt$overpass7-Sunrise)*pi/(Sunset-Sunrise)))))[group$instPAR]
  }
  
  if (l >= 8){
    #identify times between seventh and eigth observation
    group = wangt %>%
      filter(t>overpass7 & t<=overpass8) %>%
      select(instPAR)
    #calculate instPAR (eq. 6.4)
    wangt$instPAR[group$instPAR] = as_vector((wangt$overpass8-wangt$t)/(wangt$overpass8-wangt$overpass7)*(wangt$PAR7*(sin((wangt$t-Sunrise)*pi/(Sunset-Sunrise))/sin((wangt$overpass7-Sunrise)*pi/(Sunset-Sunrise))))+
                                               (wangt$t-wangt$overpass7)/(wangt$overpass8-wangt$overpass7)*(wangt$PAR8*(sin((wangt$t-Sunrise)*pi/(Sunset-Sunrise))/sin((wangt$overpass8-Sunrise)*pi/(Sunset-Sunrise)))))[group$instPAR]
  }
  
  #find last observation before Sunset
  if (l == 1){
    #identify times between last observation and Sunset
    group = wangt %>%
      filter(t>overpass1 & t<=Sunset) %>%
      select(instPAR)
    #calculate instPAR (eq. 6.2)
    wangt$instPAR[group$instPAR] = as_vector(wangt$PAR1*(sin((wangt$t-Sunrise)*pi/(Sunset-Sunrise))/sin((wangt$overpass1-Sunrise)*pi/(Sunset-Sunrise))))[group$instPAR]
  } else if (l == 2){
    #identify times between last observation and Sunset
    group = wangt %>%
      filter(t>overpass2 & t<=Sunset) %>%
      select(instPAR)
    #calculate instPAR (eq. 6.2)
    wangt$instPAR[group$instPAR] = as_vector(wangt$PAR2*(sin((wangt$t-Sunrise)*pi/(Sunset-Sunrise))/sin((wangt$overpass2-Sunrise)*pi/(Sunset-Sunrise))))[group$instPAR]
  } else if (l == 3){
    #identify times between last observation and Sunset
    group = wangt %>%
      filter(t>overpass3 & t<=Sunset) %>%
      select(instPAR)
    #calculate instPAR (eq. 6.2)
    wangt$instPAR[group$instPAR] = as_vector(wangt$PAR3*(sin((wangt$t-Sunrise)*pi/(Sunset-Sunrise))/sin((wangt$overpass3-Sunrise)*pi/(Sunset-Sunrise))))[group$instPAR]
  } else if (l == 4){
    #identify times between last observation and Sunset
    group = wangt %>%
      filter(t>overpass4 & t<=Sunset) %>%
      select(instPAR)
    #calculate instPAR (eq. 6.2)
    wangt$instPAR[group$instPAR] = as_vector(wangt$PAR4*(sin((wangt$t-Sunrise)*pi/(Sunset-Sunrise))/sin((wangt$overpass4-Sunrise)*pi/(Sunset-Sunrise))))[group$instPAR]
  } else if (l == 5){
    #identify times between last observation and Sunset
    group = wangt %>%
      filter(t>overpass5 & t<=Sunset) %>%
      select(instPAR)
    #calculate instPAR (eq. 6.2)
    wangt$instPAR[group$instPAR] = as_vector(wangt$PAR5*(sin((wangt$t-Sunrise)*pi/(Sunset-Sunrise))/sin((wangt$overpass5-Sunrise)*pi/(Sunset-Sunrise))))[group$instPAR]
  } else if (l == 6){
    #identify times between last observation and Sunset
    group = wangt %>%
      filter(t>overpass6 & t<=Sunset) %>%
      select(instPAR)
    #calculate instPAR (eq. 6.2)
    wangt$instPAR[group$instPAR] = as_vector(wangt$PAR6*(sin((wangt$t-Sunrise)*pi/(Sunset-Sunrise))/sin((wangt$overpass6-Sunrise)*pi/(Sunset-Sunrise))))[group$instPAR]
  } else if (l == 7){
    #identify times between last observation and Sunset
    group = wangt %>%
      filter(t>overpass7 & t<=Sunset) %>%
      select(instPAR)
    #calculate instPAR (eq. 6.2)
    wangt$instPAR[group$instPAR] = as_vector(wangt$PAR7*(sin((wangt$t-Sunrise)*pi/(Sunset-Sunrise))/sin((wangt$overpass7-Sunrise)*pi/(Sunset-Sunrise))))[group$instPAR]
  } else if (l == 8){
    #identify times between last observation and Sunset
    group = wangt %>%
      filter(t>overpass8 & t<=Sunset) %>%
      select(instPAR)
    #calculate instPAR (eq. 6.2)
    wangt$instPAR[group$instPAR] = as_vector(wangt$PAR8*(sin((wangt$t-Sunrise)*pi/(Sunset-Sunrise))/sin((wangt$overpass8-Sunrise)*pi/(Sunset-Sunrise))))[group$instPAR]
  }
  
  return(wangt$instPAR)
}

wangPercentMax = function(t, daylightObservations){
  wangPercentMaxt = tibble(wangt$instPAR)
  
}

ggplot(data = NULL, aes(x = 24*60*60*t, y = sin(elevation(t,Latitude,Longitude,year,day)*pi/180)))+
  #coord_cartesian(ylim = c(-.1,1))+
  geom_line()+
  scale_x_time()+
  geom_line(data = NULL, aes(x = 24*60*60*t, y = wangInstPAR(t, daylightObservations)/(.487*1361), color = ))+
  geom_point(data = daylightObservations, aes(x = time*24*60*60, y = max_PAR/(.487*1361), color = percentMax))+
  scale_color_viridis_c(option = "C")+
  geom_point(data = observationsSS, aes(x = time*24*60*60,  y = max_PAR/(.487*1361)), color = 2)+
  theme_minimal()


daylightObservations[1,1]

##create sky based on instPAR percentage of max and raw value (black at night, blue-gray during high-low par/high sun, red at sunset)

sky = tibble(t)

# t[101]=Sunrise
#Sunset = length-100

ggplot(data = NULL, aes(x = 24*60*60*t, y = sin(elevation(t,Latitude,Longitude,year,day)*pi/180)))+
  #coord_cartesian(ylim = c(-.1,1))+
  geom_line()+
  geom_vline(xintercept = 1:3, data =  ,aes())+
  scale_x_time()+
  geom_point(data = daylightObservations, aes(x = time*24*60*60, y = max_PAR/(.487*1361), color = percentMax))+
  scale_color_viridis_c(option = "C")+
  geom_point(data = observationsSS, aes(x = time*24*60*60,  y = max_PAR/(.487*1361)), color = 2)+
  theme_minimal()
