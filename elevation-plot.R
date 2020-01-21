Latitude = 0
Longitude = 0
year = 2008
day = 1
t=.5


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
  GeomMeanLongSunDeg = 280.46646+JulianCentury*(36000.76983+JulianCentury*.0003032)%%360;
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
  Sunrise = ((SolarNoon*1440-HASunriseDeg*4)/1440)%%1 #keep sunrise positive and between zero and 1
  Sunset = Sunrise+SunlightDuration;

  C = EqOfTimeMinutes/4+Longitude;

  #90-(acos(sin(Latitude*pi/180)*sin(SunDeclineDeg*pi/180)+cos(Latitude*pi/180)*cos(SunDeclineDeg*pi/180)*cos((t*360+EqOfTimeMinutes/4+Longitude+180)*pi/180)))*180/pi
  return(90-(acos(A+B*cos((t*360+C+180)*pi/180)))*180/pi)
}
elevation(.5,0,0,2008,1)

t = seq(0,1,.01)
plot(t,elevation(t,0,0,2008,1),type = "l")
