library(ggplot2)
Latitude = -70
Longitude = 0
year = 2020
day = 27
t=.5
Sunrise = 0 #initializing for global assignment
Sunset = 0 #initializing for global assignment

elevation = function(t, Latitude, Longitude, year, day){
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
  HAval = cos(pi/180*(srssangle))/(cos(pi/180*(Latitude))*cos(pi/180*(SunDeclineDeg)))-tan(pi/180*(Latitude))*tan(pi/180*(SunDeclineDeg));
  if (HAval <= -1){
    HASunriseDeg = 180
  } else if (HAval >= 1) {
    HASunriseDeg = 0
  } else {
    HASunriseDeg = 180/pi*acos(HAval)
  }
  SunlightDuration = HASunriseDeg/180;
  
  A = sin(Latitude*pi/180)*sin(SunDeclineDeg*pi/180);
  B = cos(Latitude*pi/180)*cos(SunDeclineDeg*pi/180);
  
  SolarNoon = (720-4*Longitude-EqOfTimeMinutes+0*60)/1440;
  Sunrise <<- ((SolarNoon*1440-HASunriseDeg*4)/1440)%%1; #keep sunrise positive and between zero and 1
  Sunset <<- Sunrise+SunlightDuration; #assign SR/SS to global for use in other functions
  
  C = EqOfTimeMinutes/4+Longitude;
  
  return(90-(acos(A+B*cos((t*360+C+180)*pi/180)))*180/pi)
  
}


precision = 100

t = seq(0,2,1/(8*precision))
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

###Filling in observation and elevation data

time_ele_PAR = tibble(t)
time_ele_PAR = time_ele_PAR %>%
  select(time = t) %>%
  mutate(observation = 0==(time*8*precision)%%(precision),
         elevation = elevation(t, Latitude, Longitude, year, day),
         light = elevation>=0,
         maxPAR = light*.487*1361*sin(elevation*pi/180),
         percentMax = observation*light*runif(length(t), min = 0, max = 1),
         PAR = percentMax*maxPAR)

###Seperating observations
observations = time_ele_PAR %>%
  filter(observation==TRUE)

##add vector to indicate Sunrise and Sunset times

SS_time = c(Sunrise, Sunset, Sunrise+1, (Sunset+1)%%2)
SS_time = sort(SS_time)
SS_ele = round(elevation(SS_time, Latitude, Longitude, year, day), digits = 4)

SS = tibble(time = SS_time, elevation = SS_ele) %>%
  mutate(real = elevation == 0,
         PAR = 0)

wang_index = observations %>%
  select(time, elevation, PAR)

wang_index = SS %>%
  select(time, elevation, PAR) %>%
  rbind(wang_index, .)



ggplot(data = NULL, aes(x = 24*60*60*t, y = sin(elevation(t,Latitude,Longitude,year,day)*pi/180))) +
  #coord_cartesian(ylim = c(-.1,1)) +
  geom_line() +
  scale_x_time() +
  geom_point(data = observations, aes(x = time*24*60*60, y = PAR/(.487*1361), color = percentMax)) +
  scale_color_viridis_c(option = "C", limits = c(0,1)) +
  geom_vline(xintercept = SS$time*24*60*60 , color = 8) +
  geom_vline(xintercept = c(Sunrise, Sunset)*24*60*60, color = 2) +
  theme_minimal()

###Creating a function that will plot the Wang et al. algorithm for each point

time_ele_PAR = time_ele_PAR %>%
  mutate(wang_i_obs1 = floor(t*8),
         wang_i_obs2 = ceiling(t*8))


SS = SS %>%
  mutate(slot1 = floor(time*8),
         slot2 = ceiling(time*8))

add_SS = function(SS, time_ele_PAR) {
  for (i in 1:4) {
    a = time_ele_PAR %>%
      filter(observation == FALSE,
             wang_i_obs1 == SS$slot1[i],
             time > SS$time[i]) %>%
      select(time) %>%
      as_vector() %>%
      '*'(800) + 1
    time_ele_PAR$wang_i_obs1[a] = 16 + i
    
    b = time_ele_PAR %>%
      filter(observation == FALSE,
             wang_i_obs2 == SS$slot2[(5-i)],
             time < SS$time[(5-i)]) %>%
      select(time) %>%
      as_vector() %>%
      '*'(800) + 1
    time_ele_PAR$wang_i_obs2[b] = 16 + (5 - i)
  }
  return(time_ele_PAR)
}

time_ele_PAR = add_SS(SS,time_ele_PAR)



time_ele_PAR = time_ele_PAR %>%
  mutate(
    wang_i_PAR1 = wang_index$PAR[time_ele_PAR$wang_i_obs1 + 1],
    wang_i_PAR2 = wang_index$PAR[time_ele_PAR$wang_i_obs2 + 1],
    wang_i_time1 = wang_index$time[time_ele_PAR$wang_i_obs1 + 1],
    wang_i_time2 = wang_index$time[time_ele_PAR$wang_i_obs2 + 1],
    wang_i_sunrise1 = time>((Sunrise+Sunset-1)/2),
    wang_i_sunrise2 = time>((Sunrise+Sunset+1)/2),
    wang_i_sunrise3 = time>((Sunrise+Sunset+3)/2),
    wang_i_dayduration = Sunset-Sunrise
  )

time_ele_PAR = time_ele_PAR %>%
  mutate(wang_i_sunrise = wang_i_sunrise1 + wang_i_sunrise2 + wang_i_sunrise3 + Sunrise - 1) %>%
  select(-wang_i_sunrise1, -wang_i_sunrise2, -wang_i_sunrise3)

time_ele_PAR = time_ele_PAR %>%
  mutate(
    wang_PAR = (wang_i_time2 - time) / (wang_i_time2 - wang_i_time1) * (wang_i_PAR1 * (sin((time - wang_i_sunrise) *
                                                                                             pi / (wang_i_dayduration)
    ) / sin((wang_i_time1 - wang_i_sunrise) * pi / (wang_i_dayduration)
    ))) +
      (time - wang_i_time1) / (wang_i_time2 - wang_i_time1) * (wang_i_PAR2 *
                                                                 (sin((time - wang_i_sunrise) * pi / (Sunset - Sunrise)
                                                                 ) / sin((wang_i_time2 - wang_i_sunrise) * pi / (wang_i_dayduration)
                                                                 )))
  )

i = time_ele_PAR %>%
  filter(observation == TRUE) %>%
  select(time) %>%
  as_vector() %>%
  '*'(800) + 1

time_ele_PAR$wang_PAR[i] = time_ele_PAR$PAR[i] 
rm(i)

time_ele_PAR = time_ele_PAR %>%
  mutate(wang_percent = wang_PAR/maxPAR) 


ggplot(data = time_ele_PAR, aes(x = 24*60*60*time, y = sin(elevation*pi/180))) +
  #coord_cartesian(ylim = c(-.1,1)) +
  geom_line() +
  scale_x_time() +
  geom_line(data = time_ele_PAR, aes(x = 24*60*60*time, y = wang_PAR/(.487*1361), color = wang_percent)) +
  geom_point(data = observations, aes(x = time*24*60*60, y = PAR/(.487*1361), color = percentMax)) +
  scale_color_viridis_c(option = "C", limits = c(0,1)) +
  geom_vline(data = SS, aes(xintercept = time*24*60*60, color = 8)) +
  theme_minimal()


##Create linear PAR ratio




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



time_ele_PAR = time_ele_PAR %>%
  mutate(
    wang_i_PAR1 = wang_index$PAR[time_ele_PAR$wang_i_obs1 + 1],
    wang_i_PAR2 = wang_index$PAR[time_ele_PAR$wang_i_obs2 + 1],
    wang_i_time1 = wang_index$time[time_ele_PAR$wang_i_obs1 + 1],
    wang_i_time2 = wang_index$time[time_ele_PAR$wang_i_obs2 + 1]
  )


a = time_ele_PAR 

(a$wang_i_time2 - a$time) / (a$wang_i_time2 - a$wang_i_time1) * (a$wang_i_PAR1 * (sin((a$time - Sunrise) *
                                                                              pi / (Sunset - Sunrise)
) / sin((a$wang_i_time1 - Sunrise) * pi / (Sunset - Sunrise)
))) +
  (time - a$wang_i_time1) / (a$wang_i_time2 - a$wang_i_time1) * (a$wang_i_PAR2 *
                                                             (sin((a$time - Sunrise) * pi / (Sunset - Sunrise)
                                                             ) / sin((a$wang_i_time2 - Sunrise) * pi / (Sunset - Sunrise)
                                                             )))
