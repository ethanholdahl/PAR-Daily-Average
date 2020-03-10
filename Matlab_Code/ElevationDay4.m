function [daymatrix] = ElevationDay4(daypar,lon,lat,filename)
integral1= @(T1,T0,V1,V0) ((-1/6).*(T0-T1).*(V0+2.*V1));
integral2= @(T2,T1,V2,V1) ((-1/6).*(T1-T2).*(V2+2.*V1));
integral3= @(SS,T1,V1) (V1.*(SS-T1)./2);
integral4= @(T1,SR,V1) (V1.*(T1-SR)./2);
integral5= @(SS,SR,V1) ((SS-SR).*V1);
integral6= @(T2,T1,R1,A,B,C) ((R1.*(-B.*cosd(C+360.*T1)+B.*cosd(C+360.*T2)+2.*pi.*(T1-T2).*(A.*pi.*(T1-T2)-B.*sind(C+360.*T1))))./(4.*pi^2.*(T2-T1)));
integral7= @(T1,T0,R1,A,B,C) ((-R1.*(-B.*cosd(C+360.*T1)+B.*cosd(C+360.*T0)+2.*pi.*(T1-T0).*(A.*pi.*(T1-T0)-B.*sind(C+360.*T1))))./(4.*pi^2.*(T0-T1)));
integral8= @(T1,SR,R1,A,B,C) (A.*R1.*(T1-SR)+(B.*R1.*(sind(C+360.*SR)-sind(C+360.*T1)))./(2*pi));
integral9= @(SS,T1,R1,A,B,C) (A.*R1.*(SS-T1)-(B.*R1.*(sind(C+360.*SS)-sind(C+360.*T1)))./(2*pi));

%%%Creating an empty matrix to contain the results of the function
daymatrix=NaN(4320,8640);

%%Extracting year and day from the filename of the data
k = strfind(filename,'A20');
year = str2double(filename((k+1):(k+4)));
day = str2double(filename((k+5):(k+7)));

%%Converting the year and day to Julian Day and Century. 2454466.5 is
%%12:00 AM of Jan 1 2008. Using mid day as the average 2454467 is Jan 1 2008
if year == 2008
    JulianDay = 2454466+day;
else  %add 1 here for leap day in 2008
    JulianDay = 2454467+day+(year-2008)*365;
end

%%Calculate solar position constants for the day in question. These
%%functions were obtained from an NOAA Solar Calculations spreadsheet.
%%Note: the variation seen throughout a given day is so small that we can
%%treat these as constants. More on this later.
JulianCentury = (JulianDay-2451545)/36525;
GeomMeanLongSunDeg = mod(280.46646+JulianCentury*(36000.76983+JulianCentury*.0003032),360);
GeomMeanAnomSunDeg = 357.52911+JulianCentury*(35999.05029 - 0.0001537*JulianCentury);
EccentEarthOrbit = 0.016708634-JulianCentury*(0.000042037+0.0000001267*JulianCentury);
SunEqOfCtr = sin(deg2rad(GeomMeanAnomSunDeg))*(1.914602-JulianCentury*(0.004817+0.000014*JulianCentury))+sin(deg2rad(2*GeomMeanAnomSunDeg))*(0.019993-0.000101*JulianCentury)+sin(deg2rad(3*GeomMeanAnomSunDeg))*0.000289;
SunTrueLongDeg = GeomMeanLongSunDeg+SunEqOfCtr;
SunAppLongDeg = SunTrueLongDeg-0.00569-0.00478*sin(deg2rad(125.04-1934.136*JulianCentury));
MeanObliqEclipticDeg = 23+(26+((21.448-JulianCentury*(46.815+JulianCentury*(0.00059-JulianCentury*0.001813))))/60)/60;
ObliqCorrDeg = MeanObliqEclipticDeg+0.00256*cos(deg2rad(125.04-1934.136*JulianCentury));
SunDeclineDeg = rad2deg(asin(sin(deg2rad(ObliqCorrDeg))*sin(deg2rad(SunAppLongDeg))));
vary = tan(deg2rad(ObliqCorrDeg/2))*tan(deg2rad(ObliqCorrDeg/2));
EqOfTimeMinutes = 4*rad2deg(vary*sin(2*deg2rad(GeomMeanLongSunDeg))-2*EccentEarthOrbit*sin(deg2rad(GeomMeanAnomSunDeg))+4*EccentEarthOrbit*vary*sin(deg2rad(GeomMeanAnomSunDeg))*cos(2*deg2rad(GeomMeanLongSunDeg))-0.5*vary*vary*sin(4*deg2rad(GeomMeanLongSunDeg))-1.25*EccentEarthOrbit*EccentEarthOrbit*sin(2*deg2rad(GeomMeanAnomSunDeg)));

%% Define maximum PAR ratio: Solar Constant*.487(%of radiation that makes up PAR)*scale(100)/Sin(90)
MaxRatio=.487*1361*100;
%% Calculating across latitudes
for xx=1:4320
    %If there are no observations in the entire row, continue to next row
    if sum(sum(daypar(xx,:,:))) == -69120
        continue
    end
    %% Assigning constants across longitudes to arrays
    lati = lat(xx,1);
    Temp = 27.8351-.5684*abs(lati)+.2299*lati*sin(2*pi*(day-112)/365); %from temp regression
    %srssangle = 90+(16+34*(283/(273+Temp)))/60; %90+16(radius of
    %sun)+34(average refraction)*283/(273+Temp) is the change that
    %temperature has on refraction angle.
    %HASunriseDeg and SunlightDuration from NOAA Solar Position Spreadsheet
    srssangle = 90; % Removing refraction for simplification. This will bias our estimates down but we are only losing the PAR when the sun has not yet risen (very short time interval and small intensity so the impact is very small)
    HASunriseDeg = real(rad2deg(acos(cos(deg2rad(srssangle))/(cos(deg2rad(lati))*cos(deg2rad(SunDeclineDeg)))-tan(deg2rad(lati))*tan(deg2rad(SunDeclineDeg)))));
    SunlightDuration = HASunriseDeg/180;
    
    %%A and B are latitudinal constants used in calculating the solar
    %%elevation angle.
    A = sind(lati)*sind(SunDeclineDeg);
    B = cosd(lati)*cosd(SunDeclineDeg);
    
    %%Creating arrays for variables for integration
    array1=NaN(8640,8,4);
    array2=NaN(8640,8,4);
    array3=NaN(8640,8,3);
    array4=NaN(8640,8,3);
    array5=NaN(8640,8,3);
    array6=NaN(8640,8,4);
    array7=NaN(8640,8,4);
    array8=NaN(8640,8,4);
    array9=NaN(8640,8,4);
    for yy=1:8640
        %If there are no observations continue to next location in matrix
        %This will leave the PAR value as -1.
        DayO = sum(daypar(xx,yy,:)>-1);
        if DayO == 0
            continue
        end
        
        %%Calculating longitudinal specific values.
        loni = lon(xx,yy);
        SolarNoon = (720-4*loni-EqOfTimeMinutes+0*60)/1440;
        Sunrise = mod((SolarNoon*1440-HASunriseDeg*4)/1440,1); %keep sunrise positive and between zero and 1
        Sunset = Sunrise+SunlightDuration; %ensure Sunset>Sunrise. So Sunset is between 0 and 2.
        %C is a day, lat, and lon specific constant in calculation of elevation
        C = EqOfTimeMinutes/4+loni;
        array6(yy,:,4)=C;
        array7(yy,:,4)=C;
        array8(yy,:,4)=C;
        array9(yy,:,4)=C;
        %% Calculating daily PAR from each observation
        %%% Generally we can estimate the incident PAR for unobserved times
        %%% by taking a weighted average of the PAR ratios of the nearest
        %%% observations and multiplying that by the sin of the sun's
        %%% elevation angle. PAR ratios are calculated as incident
        %%% value/sin(elevation angle). The weighted average being based on
        %%% how close the unobserved incident is to the nearest values. So
        %%% if there exists 2 observations at times T1 and T2 with R1 and R2
        %%% ratios the estimated incident PAR at time t where T1<t<T2 is
        %%%((t-T1)/(T2-T1)*R2+(T2-t)/(T2-T1)*R1)*Sin(elevation(t)).
        %%%
        %%% We can estimate PAR between observations by integrating the
        %%% above equation from T1 to T2 with respect to t, and because the
        %%% Calculation is linear we can separate them and calculate
        %%% the contribution to daily PAR that each observation had
        %%% separately.
        %%%
        %%% Below I dissect the functional form of Sin(elevation(t)).
        %%% Sin(elevation(t))=Sin(90-Arccos(Sin(Lat)*Sin(SunDecline)+Cos(Lat)*Cos(Sundecline)*Cos(t*360+EqOfTime/4+Lon+180)))
        %%%                  =Cos(Arccos(Sin(Lat)*Sin(SunDecline)+Cos(Lat)*Cos(Sundecline)*-Cos(t*360+EqOfTime/4+Lon)))
        %%%                  =Sin(Lat)*Sin(SunDecline)-Cos(Lat)*Cos(Sundecline)*Cos(t*360+EqOfTime/4+Lon)
        %%% Since SunDecline and EqOfTime are approximately constant during
        %%% a given day and since Lat and Lon don't change at a location,
        %%% if we set Sin(Lat)*Sin(SunDecline)=A and
        %%% Cos(Lat)*Cos(Sundecline)=B and EqOfTime/4+Lon=C we get
        %%% Sin(elevation(t))=A-B*Cos(t*360+C) with A,B,C as day and
        %%% location specific constants which makes the integration easy.
        %%%
        %%% Because atmospheric refraction is a function of many variables the calculated sunset and sunrise times for a given
        %%% latitude and longitude is not always the same as the first and
        %%% last light. This problem reveals itself when we have an
        %%% PAR observation before sunrise or after sunset. Or when we have
        %%% an observation just seconds after sunset where the sun may, due
        %%% to refraction, appear to be a degree or so higher in the sky
        %%% than where we calculate it to be. This can lead to calculated
        %%% ratios higher than the theoretical maximum or negative ratio
        %%% calculations. These infrequent occurrances are met with a
        %%% linear contingent plan. In these cases we ignore solar position
        %%% and assume a linear relationship between the PAR of the
        %%% troublesome observation and its neighboring observations. This
        %%% is the same method used by the authors of the GLASS PAR data
        %%% product for all observations. However I believe my methods to
        %%% be an improvement in cases where the ratio is well behaved (any
        %%% time the observation occurs outside of sunrise and sunset
        %%% times) as it is effectively the same estimator when the solar
        %%% path takes the sun to a 90 degree elevation angle and a far
        %%% more accurate estimator as we get closer to the polar regions.
        
        %%%% Creating arrays
        for tt = 1:8
            %only run code for cells with values
            if daypar(xx,yy,tt)>-1
                %set time of observation after sunrise
                if (tt-1)/8<Sunrise
                    T1=(tt-1)/8+1;
                else
                    T1=(tt-1)/8;
                end
                %check if observation occurs outside of daylight hours
                if T1>Sunset
                    %Identify if it is closer to sunrise or sunset.
                    if Sunrise+1-T1<T1-Sunset
                        %The observation is closer to sunrise.
                        %adjust T1 back to before sunrise
                        T1=T1-1;
                        if DayO>1
                            %There exists at least one other observation
                            %that day. We create a linear relationship
                            %between this and the nearest observation after
                            %sunrise and integrate over the daylight hours
                            %with decreasing weight. Here we fill the array
                            %for integral (2) and change the observed value
                            %and observation times to conform to what they
                            %would be at sunrise.
                            
                            %identifying the nearest observation
                            
                            stop = 0;
                            t2 = T1*8+2;
                            while stop == 0
                                if daypar(xx,yy,mod(t2-1,8)+1)>-1
                                    %observation found, set Tn+1
                                    T2=(t2-1)/8;
                                    %check if after sunrise
                                    if T2<Sunrise
                                        %this observation also before
                                        %sunrise. Don't integrate.
                                        break
                                    end
                                    %Set Vn+1
                                    V2=daypar(xx,yy,mod(t2-1,8)+1);
                                    V1=daypar(xx,yy,tt);
                                    %Calculate the new Vn, Set Tn=Sunrise
                                    V1=(V2-V1)/(T2-T1)*(Sunrise-T1)+V1;
                                    T1=Sunrise;
                                    %Input variables into array2
                                    array2(yy,tt,1)=T2;
                                    array2(yy,tt,2)=T1;
                                    array2(yy,tt,3)=V2;
                                    array2(yy,tt,4)=V1;
                                    stop=1;
                                else
                                    t2=t2+1;
                                end
                            end
                        else
                            %There are no other observations during the
                            %day. So there is only 1 observation and it
                            %occurred outside of daylight hours. We simply
                            %integrate over the daylight period with the
                            %value observed here. So we need to fill the
                            %array for integral (5).
                            array5(yy,tt,1)=Sunset;
                            array5(yy,tt,2)=Sunrise;
                            array5(yy,tt,3)=daypar(xx,yy,tt);
                        end
                    else
                        %The observation is closer to sunset.
                        if DayO>1
                            %The observation occurred at a time close to but
                            %after sunset with at least one other
                            %observation that day. We create a linear
                            %relationship between this and the nearest
                            %observation before sunset and integrate over
                            %the daylight hours of this range with
                            %increasing weight. Here we fill the array for
                            %integral (2) and change the observed value and
                            %observation times to conform to what they
                            %would be at sunset.
                            
                            %identifying the nearest observation
                            stop = 0;
                            t0 = T1*8;
                            while stop == 0
                                if daypar(xx,yy,mod(t0-1,8)+1)>-1
                                    %observation found, Set Tn-1
                                    T0=(t0-1)/8;
                                    %check if before sunset
                                    if T0>Sunset
                                        break
                                        %this observation also after
                                        %sunset. Don't integrate
                                    end
                                    %Set Vn-1
                                    V0=daypar(xx,yy,mod(t0-1,8)+1);
                                    V1=daypar(xx,yy,tt);
                                    %Calculate the new Vn, Set Tn=Sunrise
                                    V1=(V1-V0)/(T1-T0)*(Sunset-T1)+V1;
                                    T1=Sunset;
                                    %Input variables into array2
                                    array1(yy,tt,1)=T1;
                                    array1(yy,tt,2)=T0;
                                    array1(yy,tt,3)=V1;
                                    array1(yy,tt,4)=V0;
                                    stop=1;
                                else
                                    t0=t0-1;
                                end
                            end
                        else
                            %There are no other observations during the
                            %day. So there is only 1 observation and it
                            %occurred outside of daylight hours. We simply
                            %integrate over the daylight period with the
                            %value observed here. So we need to fill the
                            %array for integral (5).
                            array5(yy,tt,1)=Sunset;
                            array5(yy,tt,2)=Sunrise;
                            array5(yy,tt,3)=daypar(xx,yy,tt);
                        end
                    end
                else
                    %Observation is inside daylight hours. Check if the PAR
                    %ratio is within acceptable levels.
                    %Calculate refraction adjusted PAR Ratio for the observation
                    Rat1=daypar(xx,yy,tt)/sind(RAE(loni,T1,Temp,A,B,EqOfTimeMinutes));
                    if Rat1>MaxRatio
                        %Ratio exceeds maximum possible level so we don't
                        %use the PAR ratio to calculate PAR for this
                        %observation.
                        %Check if there exists an observation between
                        %it and sunrise.
                        PossObs=floor((T1-Sunrise)*8);
                        for PO=1:PossObs
                            if daypar(xx,yy,mod(tt-PO-1,8)+1)>-1
                                %Earlier observation found. Can now fill
                                %variables for integral (1).
                                array1(yy,tt,1)=T1;
                                array1(yy,tt,2)=T1-PO/8;
                                array1(yy,tt,3)=daypar(xx,yy,tt);
                                array1(yy,tt,4)=daypar(xx,yy,mod(tt-PO-1,8)+1);
                                break
                            end
                            %Checking if we have tested all possible
                            %previous data points. Set PossObs to 0 if
                            %we have to use as an indicator that there
                            %are none.
                            if PO==PossObs
                                PossObs=0;
                            end
                        end
                        if PossObs==0
                            %There are no previous observations so we
                            %can fill in values for integral (4).
                            array4(yy,tt,1)=T1;
                            array4(yy,tt,2)=Sunrise;
                            array4(yy,tt,3)=daypar(xx,yy,tt);
                        end
                        %Check if there exists an observation between
                        %T1 and sunset.
                        PossObs=floor((Sunset-T1)*8);
                        for PO=1:PossObs
                            if daypar(xx,yy,mod(tt+PO-1,8)+1)>-1
                                %Later observation found. Can now fill
                                %variables for integral (2).
                                array2(yy,tt,1)=T1+PO/8;
                                array2(yy,tt,2)=T1;
                                array2(yy,tt,3)=daypar(xx,yy,mod(tt+PO-1,8)+1);
                                array2(yy,tt,4)=daypar(xx,yy,tt);
                                break
                            end
                            %Checking if we have tested all possible
                            %previous data points. Set PossObs to 0 if
                            %we have to use as an indicator that there
                            %are none.
                            if PO==PossObs
                                PossObs=0;
                            end
                        end
                        if PossObs==0
                            %There are no subsequent observations so we
                            %can fill in values for integral (3).
                            array4(yy,tt,1)=Sunset;
                            array4(yy,tt,2)=T1;
                            array4(yy,tt,3)=daypar(xx,yy,tt);
                        end
                    else
                        %Ratio is within reasonable limits.
                        %Now check to see if there is an earlier
                        %observation.
                        PossObs=floor((T1-Sunrise)*8);
                        for PO=1:PossObs
                            if daypar(xx,yy,mod(tt-PO-1,8)+1)>-1
                                %Earlier observation found. Can now fill
                                %variables for integral (7).
                                array7(yy,tt,1)=T1;
                                array7(yy,tt,2)=T1-PO/8;
                                array7(yy,tt,3)=Rat1;
                                break
                            end
                            %Checking if we have tested all possible
                            %previous data points. Set PossObs to 0 if
                            %we have to use as an indicator that there
                            %are none.
                            if PO==PossObs
                                PossObs=0;
                            end
                        end
                        if PossObs==0
                            %There are no previous observations so we
                            %can fill in values for integral (8).
                            array8(yy,tt,1)=T1;
                            array8(yy,tt,2)=Sunrise;
                            array8(yy,tt,3)=Rat1;
                        end
                        %Now check if there exists an observation between
                        %T1 and sunset.
                        PossObs=floor((Sunset-T1)*8);
                        for PO=1:PossObs
                            if daypar(xx,yy,mod(tt+PO-1,8)+1)>-1
                                %Later observation found. Can now fill
                                %variables for integral (6).
                                array6(yy,tt,1)=T1+PO/8;
                                array6(yy,tt,2)=T1;
                                array6(yy,tt,3)=Rat1;
                                break
                            end
                            %Checking if we have tested all possible
                            %previous data points. Set PossObs to 0 if
                            %we have to use as an indicator that there
                            %are none.
                            if PO==PossObs
                                PossObs=0;
                            end
                        end
                        if PossObs==0
                            %There are no subsequent observations so we
                            %can fill in values for integral (9).
                            array9(yy,tt,1)=Sunset;
                            array9(yy,tt,2)=T1;
                            array9(yy,tt,3)=Rat1;
                        end
                    end
                end
            end
        end
    end
    
    %we now have all the arrays defined for integration for the given lat.
    %% Calculate the daily PAR values
    DayPAR = nansum(integral1(array1(:,:,1),array1(:,:,2),array1(:,:,3),array1(:,:,4)),2);
    DayPAR = DayPAR+nansum(integral2(array2(:,:,1),array2(:,:,2),array2(:,:,3),array2(:,:,4)),2);
    DayPAR = DayPAR+nansum(integral3(array3(:,:,1),array3(:,:,2),array3(:,:,3)),2);
    DayPAR = DayPAR+nansum(integral4(array4(:,:,1),array4(:,:,2),array4(:,:,3)),2);
    DayPAR = DayPAR+nansum(integral5(array5(:,:,1),array5(:,:,2),array5(:,:,3)),2);
    DayPAR = DayPAR+nansum(integral6(array6(:,:,1),array6(:,:,2),array6(:,:,3),A,B,array6(:,:,4)),2);
    DayPAR = DayPAR+nansum(integral7(array7(:,:,1),array7(:,:,2),array7(:,:,3),A,B,array7(:,:,4)),2);
    DayPAR = DayPAR+nansum(integral8(array8(:,:,1),array8(:,:,2),array8(:,:,3),A,B,array8(:,:,4)),2);
    DayPAR = DayPAR+nansum(integral9(array9(:,:,1),array9(:,:,2),array9(:,:,3),A,B,array9(:,:,4)),2);
    %% Store value in new matrix
    %Output is in KJ/m^2 (60sec*60min*24hr/1000(K)/100(scale))
    daymatrix(xx,:)=DayPAR*.864;
end
end