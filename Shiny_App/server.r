
## Load and install the packages
library("tidyverse", "shiny", "stringr")
theme_set(theme_minimal())


# Define server logic
function(input, output) {
  
  url1 <- a("Figshare", href="https://figshare.com/articles/Humanity_s_Fundamental_Environmental_Limits_--_model_input_files/9983369")
  output$results = renderUI({
    tagList("Link to output:", url1)
  })
  
  url2 <- a("GLASS", href="http://www.glass.umd.edu/PAR/")
  output$data = renderUI({
    tagList("Link to inputs:", url2)
  })
  
  url3 <- a("GitHub", href="https://github.com/ethanholdahl/PAR-Daily-Average")
  output$matlab = renderUI({
    tagList("Link to Matlab code:", url3)
  })
  
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
    
    ## Define maximum PAR: Solar Constant*.487(%of radiation that makes up PAR)*scale(100)/Sin(90)
    MaxPAR=.487*1361*100;
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
  
  wang_elevation = function(time, t, Latitude, Longitude, year, day){
    timeEle = sin(elevation(time, Latitude, Longitude, year, day)*pi/180)
    adj = 1
    if (time > (Sunrise+Sunset-1)/2) {
      adj = adj - 1
    }
    if (time > (Sunrise+Sunset+1)/2) {
      adj = adj - 1 
    }
    wangEle = sin((time+adj-Sunrise)*pi/(Sunset-Sunrise))
    ratio = timeEle/wangEle
    wang_ele = sin((t+adj-Sunrise)*pi/(Sunset-Sunrise))*ratio
    return(wang_ele)
  }
  
  add_wang = function(t, Latitude, Longitude, year, day) {
    ###Filling in observation and elevation data
    #Max PAR: .487*1361/sin(elevation)
    
    time_ele_PAR = tibble(t)
    time_ele_PAR = time_ele_PAR %>%
      select(time = t) %>%
      mutate(observation = 0==(time*8*precision)%%(precision),
             elevation = elevation(t, Latitude, Longitude, year, day),
             light = elevation>=0,
             maxPAR = light*.487*1361*sin(elevation*pi/180),
             ratioMax = observation*light*runif(length(t), min = 0, max = 1),
             PAR = ratioMax*maxPAR,
             obs1 = floor(t * 8) + 1,
             obs2 = ceiling(t * 8) + 1)
    
    ###Seperating observations
    observations = time_ele_PAR %>%
      filter(observation==TRUE)
    
    ##add vector to indicate Sunrise and Sunset times
    
    SS_time = c(Sunrise, Sunset, Sunrise+1, (Sunset+1)%%2)
    if(SS_time[2]>SS_time[4]){
      SS_time = c(SS_time[4], SS_time[-4])
      iSR = 2
    } else {
      iSR = 1 
    }
    SS_ele = round(elevation(SS_time, Latitude, Longitude, year, day), digits = 4)
    
    SS = tibble(time = SS_time, elevation = SS_ele) %>%
      mutate(real = elevation == 0,
             PAR = 0)
    
    wang_index = observations %>%
      select(time, elevation, PAR)
    
    wang_index = SS %>%
      select(time, elevation, PAR) %>%
      rbind(wang_index, .)
    
    wang_calculation = time_ele_PAR
    
    SS = SS %>%
      mutate(
        slot1f = ceiling(time * 8 * precision + 1),
        slot1c = ceiling(slot1f / precision) * precision,
        slot2c = floor(SS$time * 8 * precision + 1),
        slot2f = floor(slot2c / precision) * precision + 2
      )
    
    #fill obs 1 forward and obs 2 backwards (so obs 1 is the most recent and obs 2 in the soonest)
    
    for (i in 1:4) {
      wang_calculation$obs1[SS$slot1f[i]:SS$slot1c[i]] = 17 + i
      wang_calculation$obs2[SS$slot2f[5 - i]:SS$slot2c[5 - i]] = 17 + 5 - i
    }
    
    
    wang_calculation = wang_calculation %>%
      mutate(
        wang_i_PAR1 = wang_index$PAR[wang_calculation$obs1],
        wang_i_PAR2 = wang_index$PAR[wang_calculation$obs2],
        wang_i_time1 = wang_index$time[wang_calculation$obs1],
        wang_i_time2 = wang_index$time[wang_calculation$obs2],
        wang_i_sunrise1 = time > ((Sunrise + Sunset - 1) / 2),
        wang_i_sunrise2 = time > ((Sunrise + Sunset + 1) / 2),
        wang_i_sunrise3 = time > ((Sunrise + Sunset + 3) / 2),
        wang_i_dayduration = Sunset - Sunrise
      )
    
    wang_calculation = wang_calculation %>%
      mutate(wang_i_sunrise = wang_i_sunrise1 + wang_i_sunrise2 + wang_i_sunrise3 + Sunrise - 1) %>%
      select(-wang_i_sunrise1,-wang_i_sunrise2,-wang_i_sunrise3)
    
    wang_calculation = wang_calculation %>%
      mutate(
        PAR1 = wang_i_PAR1 * sin((time - wang_i_sunrise) * pi / wang_i_dayduration)
        / sin((wang_i_time1 - wang_i_sunrise) * pi / wang_i_dayduration),
        PAR2 = wang_i_PAR2 * sin((time - wang_i_sunrise) * pi / wang_i_dayduration)
        / sin((wang_i_time2 - wang_i_sunrise) * pi / wang_i_dayduration),
        ratio_1 = (wang_i_time2 - time) / (wang_i_time2 - wang_i_time1),
        ratio_2 = (time - wang_i_time1) / (wang_i_time2 - wang_i_time1),
        wang_PAR = ratio_1*PAR1 + ratio_2*PAR2)
    
    #filter out observations with a SR as the first observation. Then calculate PAR from second observation using 6.2
    
    SR_adjust = wang_calculation %>%
      filter(obs1 == 17 + iSR | obs1 == 19 + iSR) %>%
      mutate(wang_PAR_SR = PAR2)
    
    # set PAR with times where SR is the second observation to 0
    
    SR_adjust_e = wang_calculation %>%
      filter(obs2 == 17 + iSR | obs2 == 19 + iSR) %>%
      mutate(wang_PAR_SR = 0)
    
    #combine SR_adjust tables
    
    SR_adjust = rbind(SR_adjust, SR_adjust_e)
    
    #Insert SR_adjust into wang_calculation
    
    wang_calculation = right_join(SR_adjust, wang_calculation)
    wang_calculation$wang_PAR[!is.na(wang_calculation$wang_PAR_SR)] = wang_calculation$wang_PAR_SR[!is.na(wang_calculation$wang_PAR_SR)]
    
    
    #filter out observations with a SS as the second observation. Then calculate PAR from first observation using 6.2
    
    SS_adjust = wang_calculation %>%
      filter(obs2 == 18 + iSR |
               obs2 == 16 + (iSR + 3) %% 5 + iSR) %>%
      mutate(wang_PAR_SS = PAR1)
    
    # set PAR with times where SS is the first observation to 0
    
    SS_adjust_e = wang_calculation %>%
      filter(obs1 == 18 + iSR |
               obs1 == 16 + (iSR + 3) %% 5 + iSR) %>%
      mutate(wang_PAR_SS = 0)
    
    #combine SR_adjust tables
    
    SS_adjust = rbind(SS_adjust, SS_adjust_e)
    
    #Insert SS_adjust into wang_calculation
    
    wang_calculation = right_join(SS_adjust, wang_calculation)
    wang_calculation$wang_PAR[!is.na(wang_calculation$wang_PAR_SS)] = wang_calculation$wang_PAR_SS[!is.na(wang_calculation$wang_PAR_SS)]
    
    #day of no observations
    
    no_obs_adjust = wang_calculation %>%
      filter(obs1 > 17 & obs2 > 17) %>%
      mutate(wang_PAR_no = NaN)
    
    #Insert no_obs_adjust
    
    wang_calculation = right_join(no_obs_adjust, wang_calculation)
    wang_calculation$wang_PAR[!is.na(wang_calculation$wang_PAR_no)] = wang_calculation$wang_PAR_no[!is.na(wang_calculation$wang_PAR_no)]
    
    #fill days with observations
    
    i = 0:16 * precision + 1
    wang_calculation$wang_PAR[i] = wang_calculation$PAR[i]
    time_ele_PAR = time_ele_PAR %>%
      cbind(.,wang_PAR = wang_calculation$wang_PAR) %>%
      mutate(wang_ratio = wang_PAR/maxPAR)
    return(time_ele_PAR)
  }
  
  add_ratio = function(t, Latitude, Longitude, year, day) {
    
    time_ele_PAR = add_wang(t, Latitude, Longitude, year, day)
    
    ###Seperating observations
    observations = time_ele_PAR %>%
      filter(observation==TRUE)
    
    ##add vector to indicate Sunrise and Sunset times
    
    SS_time = c(Sunrise, Sunset, Sunrise+1, (Sunset+1)%%2)
    if(SS_time[2]>SS_time[4]){
      SS_time = c(SS_time[4], SS_time[-4])
      iSR = 2
    } else {
      iSR = 1 
    }
    SS_ele = round(elevation(SS_time, Latitude, Longitude, year, day), digits = 4)
    
    SS = tibble(time = SS_time, elevation = SS_ele) %>%
      mutate(real = elevation == 0,
             PAR = 0)
    
    #check observations. If both light then take average of ratios. Else take the ratio of the one with light.
    ratio_index = observations %>%
      select(time, elevation, ratioMax)
    
    ratio_calc = time_ele_PAR
    
    ratio_calc = time_ele_PAR %>%
      mutate(
        rat1 = ratio_index$ratioMax[ratio_calc$obs1],
        rat2 = ratio_index$ratioMax[ratio_calc$obs2],
        time1 = ratio_index$time[ratio_calc$obs1],
        time2 = ratio_index$time[ratio_calc$obs2],
        linear_ratio =  rat1*(time2-time)/(time2-time1)+rat2*(time-time1)/(time2-time1)
      )
    
    #identify SR/SS slots
    
    SS = SS %>%
      mutate(
        slot1f = ceiling(time * 8 * precision + 1),
        slot1c = ceiling(slot1f / precision) * precision,
        slot2c = floor(SS$time * 8 * precision + 1),
        slot2f = floor(slot2c / precision) * precision + 2
      )
    
    #fill obs 1 forward and obs 2 backwards (so obs 1 is the most recent and obs 2 in the soonest)
    
    for (i in 1:4) {
      ratio_calc$obs1[SS$slot1f[i]:SS$slot1c[i]] = 17 + i
      ratio_calc$obs2[SS$slot2f[5 - i]:SS$slot2c[5 - i]] = 17 + 5 - i
    }
    
    #filter out observations with a SR as the first observation. Then use ratio from second observation
    
    SR_adjust1 = ratio_calc %>%
      filter(time>5)
    SR_adjust2 = ratio_calc %>%
      filter(time>5)
    
    if(SS_ele[iSR] == 0){
      SR_adjust1 = ratio_calc %>%
        filter(obs1 == 17 + iSR) %>%
        mutate(linear_ratio_SR = rat2)
    }
    
    if(SS_ele[iSR+2] == 0){
      SR_adjust2 = ratio_calc %>%
        filter(obs1 == 19 + iSR) %>%
        mutate(linear_ratio_SR = rat2)
    }
    
    # set PAR with times where SR is the second observation to 0
    
    SR_adjust_e = ratio_calc %>%
      filter(obs2 == 17 + iSR | obs2 == 19 + iSR) %>%
      mutate(linear_ratio_SR = 0)
    
    #combine SR_adjust tables
    
    SR_adjust = rbind(SR_adjust1, SR_adjust2, SR_adjust_e)
    
    #Insert SR_adjust into ratio_calc
    
    ratio_calc = right_join(SR_adjust, ratio_calc)
    ratio_calc$linear_ratio[!is.na(ratio_calc$linear_ratio_SR)] = ratio_calc$linear_ratio_SR[!is.na(ratio_calc$linear_ratio_SR)]
    
    
    #filter out observations with a SS as the second observation. Then use ratio from first observation
    
    SS_adjust1 = ratio_calc %>%
      filter(time>5)
    SS_adjust2 = ratio_calc %>%
      filter(time>5)
    
    if(SS_ele[iSR+1] == 0){
      SS_adjust1 = ratio_calc %>%
        filter(obs2 == 18 + iSR) %>%
        mutate(linear_ratio_SS = rat1)
    }
    
    if(SS_ele[(iSR+3) %% 5 + iSR-1] == 0){
      SS_adjust2 = ratio_calc %>%
        filter(obs2 == 16 + (iSR + 3) %% 5 + iSR) %>%
        mutate(linear_ratio_SS = rat1)
    }
    
    
    
    # set PAR with times where SS is the first observation to 0
    
    SS_adjust_e = ratio_calc %>%
      filter(obs1 == 18 + iSR |
               obs1 == 16 + (iSR + 3) %% 5 + iSR) %>%
      mutate(linear_ratio_SS = 0)
    
    #combine SR_adjust tables
    
    SS_adjust = rbind(SS_adjust1, SS_adjust2, SS_adjust_e)
    
    #Insert SS_adjust into ratio_calc
    
    ratio_calc = right_join(SS_adjust, ratio_calc)
    ratio_calc$linear_ratio[!is.na(ratio_calc$linear_ratio_SS)] = ratio_calc$linear_ratio_SS[!is.na(ratio_calc$linear_ratio_SS)]
    
    #day of no observations
    
    no_obs_adjust = ratio_calc %>%
      filter(obs1 > 17 & obs2 > 17) %>%
      mutate(obs_no = NaN)
    
    #Insert no_obs_adjust
    
    ratio_calc = right_join(no_obs_adjust, ratio_calc)
    ratio_calc$linear_ratio[!is.na(ratio_calc$obs_no)] = ratio_calc$obs_no[!is.na(ratio_calc$obs_no)]
    
    #Fill days with observations
    
    i = 0:16 * precision + 1
    ratio_calc$linear_ratio[i] = ratio_calc$ratioMax[i]
    
    time_ele_PAR = time_ele_PAR %>%
      cbind(., linear_ratio = ratio_calc$linear_ratio) %>%
      mutate(linear_PAR = linear_ratio*maxPAR)
    
    return(time_ele_PAR)
  }
  
  observations = function(t, Latitude, Longitude, year, day) {
    observations = add_ratio(t, Latitude, Longitude, year, day)
    observations = observations %>%
      filter(observation == TRUE)
    return(observations)
  }
  
  precision = 100
  t = round(seq(0, 2, 1/(8*precision)), digits = 5)
  
  time_ele_PAR = reactive({
    add_ratio(t, input$Latitude, input$Longitude, input$year, input$day)
  })
  
  observations = reactive({
    time_ele_PAR() %>%
      filter(observation == TRUE)
  })
  
  output$elevation = renderPlot({
    ggplot(data = NULL,
           aes(
             x = t,
             y = sin(
               elevation(
                 t,
                 input$Latitude,
                 input$Longitude,
                 input$year,
                 input$day
               ) * pi / 180
             ),
             color = sin(
               elevation(
                 t,
                 input$Latitude,
                 input$Longitude,
                 input$year,
                 input$day
               ) * pi / 180
             ),
             size = 2
           )) +
      scale_color_viridis_c(
        option = "C",
        begin = .45,
        rescaler =  function(x,
                             to = NULL,
                             from = NULL) {
          ifelse(
            x < .2,
            scales::rescale(x, from = c(0, .2), to = c(0, .85)),
            scales::rescale(x, from = c(.2, 1), to = c(.85, 1))
          )
        }
      ) +
      coord_cartesian(ylim = c(-1, 1)) +
      geom_point() +
      geom_line(data = NULL, aes(x=t, y = wang_elevation(input$time, t, input$Latitude, input$Longitude, input$year, input$day), size = 1, color = 0))+
      geom_point(data = NULL, aes(x=input$time, y = sin(elevation(input$time, input$Latitude, input$Longitude, input$year, input$day)*pi/180), size = 5, color = 0 )) +
      scale_size(guide = 'none') +
      labs(x = "time", y = "sin(elevation)", color = "sin(ele)", title = "Solar elevation for selected inputs.", 
           caption = str_wrap("The large purple point represents an observation at the specified time
           The purple line shows the elevation under which Wang's algorithm yields a linear relationship in interpolated PAR ratio between observations.", width = 80)) +
      theme(plot.title = element_text(size = 18, hjust = .5), plot.caption = element_text(size = 12, hjust = 1))
  })
  
  output$PAR = renderPlot({
    p = ggplot(data = time_ele_PAR(), aes(x = time, y = sin(elevation*pi/180)*(.487*1361))) +
      geom_line() +
      geom_point(data = observations(), aes(x = time, y = PAR, color = ratioMax)) +
      scale_color_viridis_c(option = "C", limits = c(0,1)) +
      labs(title = "Interpolated PAR values", caption = str_wrap("The black line represents the path of the sun and the theoretical maximum PAR at the given elevation angle (for positive elevations). The points represent observations, and the colorful line represents the interpolated PAR values color coded by the ratio of the interpolated PAR to the maximum possible PAR at that elevation.", width = 80)) +
      scale_y_continuous("PAR", sec.axis = sec_axis(~./(.487*1361), name = "sin(elevation)")) +
      theme(plot.title = element_text(size = 18, hjust = .5), plot.caption = element_text(size = 12, hjust = 1))
    
    if(input$Wang) p = p + geom_line(data = time_ele_PAR(), aes(x = time, y = wang_PAR, color = wang_ratio))
    if(input$ratio) p = p + geom_line(data = time_ele_PAR(), aes(x = time, y = linear_PAR, color = linear_ratio))
    
    p
  })
  
  output$ratio = renderPlot({
    p = ggplot(data = observations(), aes(x = time, y = ratioMax, color = PAR))+
      geom_point() +
      scale_color_viridis_c(option = "C", limits = c(0, .487*1361)) +
      labs(y = "Ratio", title = "Interpolated PAR ratio", 
           caption = str_wrap("The points represent observations. The colorful line represents the ratio of the interpolated PAR to the theoretical maximum PAR at that elevation. The color of the line represents the interpolated PAR values.", width = 80)) +
      theme(plot.title = element_text(size = 18, hjust = .5), plot.caption = element_text(size = 12, hjust = 1))
    
    
    if(input$Wang) p = p + geom_line(data = time_ele_PAR(), aes(x = time, y = wang_ratio, color = wang_PAR))
    if(input$ratio) p = p + geom_line(data = time_ele_PAR(), aes(x = time, y = linear_ratio, color = linear_PAR))
    
    p
  })
  
}
