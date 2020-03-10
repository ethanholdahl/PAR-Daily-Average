function [RAE] = RAE(lon,time,Temp,A,B,EqOfTime)
hour = pi*((time*1440+EqOfTime+4*lon)/720+1);
zenith = acos(A+B*cos(hour));
elevation = (pi/2-zenith)*180/pi;
RAE = elevation+(1.02*cotd(elevation+10.3/(elevation+5.11)))/60*283/(273+Temp);