load('lon.mat')
load('lat.mat')
GLASSdailyaverage=zeros(4320,8640,125,'int32');

for d = 241:365
    daypar8=zeros(4320,8640,8,3);
    daypar=zeros(4320,8640,3);
    dd=d; 
    if d>120
        dd=d-120;
    end
    if d>240
        dd=d-240;
    end
    for y = 1:3
        if y==1 && d>=60
            c=d+1;
        else
            c=d;
        end
            %Identifying the ordinal file for each day
            if y==1
                x=c;
            elseif y==2
                x=366+c;
            %Day 80 in year 3 was missing
            elseif y==3 && d>80
                x=730+c;
            else
                x=731+c;
            end
            filename=strcat(files(x).folder,'\',files(x).name);
            if y==3 && d==80
                daypar8(:,:,:,3)=NaN;
            else
                for t = 0:7
                    h=t*3;
                    hour=num2str(h);
                    if h<10
                        hour=strcat('0',hour);
                    end
                    time=strcat('GMT_',hour,'00_PAR');
                    file=hdfread(filename,'GLASS04C00','Field',time);
                    daypar8(:,:,(t+1),y)=file;
                end
            end
            daypar(:,:,y)=ElevationDay4(daypar8(:,:,:,y),lon,lat,filename);
    end
    daypar(daypar<1)=NaN;
    daypar=nanmean(daypar,3);
    daypar=round(daypar*1000);
    daypar(isnan(daypar))=-1;
    GLASSdailyaverage(:,:,dd)=int32(daypar);
    %if d==120
        %save firstthird GLASSdailyaverage
        %delete '/Users/holdahea/Dropbox/GLASS PAR/firstthird.mat'
        %clear GLASSdailyaverage
        %GLASSdailyaverage=zeros(4320,8640,120,'int32');
    %end
    %if d==240
        %save secondthird GLASSdailyaverage
        %delete '/Users/holdahea/Dropbox/GLASS PAR/secondthird.mat'
        %clear GLASSdailyaverage
        %GLASSdailyaverage=zeros(4320,8640,125,'int32');
    %end
    %if d==365
        %save thirdthird GLASSdailyaverage
        %delete '/Users/holdahea/Dropbox/GLASS PAR/thirdthird.mat'
        %clear GLASSdailyaverage
    %end
end
