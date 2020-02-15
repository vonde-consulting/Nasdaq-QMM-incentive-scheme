clear all

[~,~,A]=xlsread('D:\Matlab\Tickers2.xlsx','Sheet1');
    ticks=A(:,1); clear A
[~,~,A]=xlsread('D:\Matlab\Tickers2.xlsx','Sheet2');    
    dates=A(:,1); clear A
    
for x=1:length(ticks)
for y=1:length(dates)
   
    tick=ticks{x};
    date=dates{y};
    
%Load Message File    
    
filename=['D:\Data\Data\' tick '_2013-11-04_2013-11-22_10\' tick '_2013-11-' date '_34200000_59400000_message_10.csv'];
fid=fopen(filename);

A=textscan(fid,'%f %f %f %f %f %f %s','Delimiter', ',');

%Load Order Book File

clear filename

filename=['D:\Data\Data\' tick '_2013-11-04_2013-11-22_10\' tick '_2013-11-' date '_34200000_59400000_orderbook_10.csv'];
fid=fopen(filename);

B=textscan(fid,'%f %f %f %f %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d','Delimiter', ',');

fclose('all');

time=A{1};
event=A{2};
order=A{3};
size=A{4};
price=A{5}./10000;
direction=A{6};
mpid=A{7};

askpr=B{1}./10000;
asksz=B{2};
bidpr=B{3}./10000;
bidsz=B{4};

clear A B

%Remove Trades Outside of Opening Hours

start=9.5*60*60; %9:30:00 in seconds after midnight
ending=16*60*60; %4:00:00 in seconds after midnight 

f1=find(time<start,1,'last'); %get rid of observattions before 9:30:00
    f=isempty(f1);
    if f==0
        time=time(f1:end);
        event=event(f1:end);
        order=order(f1:end);
        size=size(f1:end);
        price=price(f1:end);
        direction=direction(f1:end);
        mpid=mpid(f1:end);
        askpr=askpr(f1:end);
        asksz=asksz(f1:end);
        bidpr=bidpr(f1:end);
        bidsz=bidsz(f1:end);        
    end    
    clear f f1 start
f2=find(time>ending,1,'first'); %get rid of observations after 4:00:00
    f=isempty(f2);
    if f==0
        time=time(1:f2);
        event=event(1:f2);
        order=order(1:f2);
        size=size(1:f2);
        price=price(1:f2);
        direction=direction(1:f2);
        mpid=mpid(1:f2);
        askpr=askpr(1:f2);
        asksz=asksz(1:f2);
        bidpr=bidpr(1:f2);
        bidsz=bidsz(1:f2);    
    end   
    clear f f2 ending 
    
%Set up MPID Dummy      
mpid_var=strcmp('null',mpid);
mpid_var=1-mpid_var;

%Set Up Numeric Broker Identifier
mpid_num=mpid;   
mpid_num=strrep(mpid_num, 'ATDF', '01'); mpid_num=strrep(mpid_num, 'BARD', '02'); mpid_num=strrep(mpid_num, 'BOOK', '03'); 
mpid_num=strrep(mpid_num, 'DADA', '04'); mpid_num=strrep(mpid_num, 'FBCO', '05'); mpid_num=strrep(mpid_num, 'FLTU', '06'); 
mpid_num=strrep(mpid_num, 'GSCO', '07'); mpid_num=strrep(mpid_num, 'LEHM', '08'); mpid_num=strrep(mpid_num, 'NITE', '09'); 
mpid_num=strrep(mpid_num, 'RHCO', '10'); mpid_num=strrep(mpid_num, 'SBSH', '11'); mpid_num=strrep(mpid_num, 'TMBR', '12'); 
mpid_num=strrep(mpid_num, 'UBSS', '13'); mpid_num=strrep(mpid_num, 'WCHV', '14'); mpid_num=strrep(mpid_num, 'WEMM', '15'); 
mpid_num=strrep(mpid_num, 'null', '00'); 
    mpid_n=zeros(length(mpid_num),1);
    for i=1:length(mpid_num);
    w=str2double(mpid_num{i});    
    mpid_n(i)=w;
    end
    clear mpid_num i
   
%Remove Negative Bid-Ask Spreads or Spreads=0
spr=askpr-bidpr; g=find(spr<=0);
    time(g)=[]; order(g)=[]; size(g)=[]; price(g)=[]; direction(g)=[]; event(g)=[];
    mpid_var(g)=[]; mpid_n(g)=[]; askpr(g)=[]; asksz(g)=[]; bidpr(g)=[]; bidsz(g)=[]; spr(g)=[]; 
    clear g
    
%Remove Volume=0
g=find(size<=0);
    time(g)=[]; order(g)=[]; size(g)=[]; price(g)=[]; direction(g)=[]; event(g)=[];
    mpid_var(g)=[]; mpid_n(g)=[]; askpr(g)=[]; asksz(g)=[]; bidpr(g)=[]; bidsz(g)=[]; spr(g)=[]; 
    clear g    
    
%Remove Spreads => 2
g=find(2<=spr);
    time(g)=[]; order(g)=[]; size(g)=[]; price(g)=[]; direction(g)=[]; event(g)=[];
    mpid_var(g)=[]; mpid_n(g)=[]; askpr(g)=[]; asksz(g)=[]; bidpr(g)=[]; bidsz(g)=[]; spr(g)=[]; 
    clear g
    
clear year month day hour minute second d

%parse time strings at delimiter
nanoseconds=round((time-floor(time))*10^9);
seconds=floor(time);

M=[seconds nanoseconds event order size price direction mpid_n askpr asksz bidpr bidsz];

filename=['D:\Data\Cleaned_Data\Data3\' tick '_2013-11-' date '.csv'];
dlmwrite(filename,M,'precision',9);

filename=['D:\Data\Cleaned_Data\Data3\' tick '_2013-11-' date '.mat'];
save(filename,'M');

end   
end





