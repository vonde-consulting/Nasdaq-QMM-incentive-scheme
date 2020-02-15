clear all

mypath='\\tsclient\C\Users\Julia\Dropbox\Projects\Trader Anonymity\';

[~,~,A]=xlsread([mypath 'Tickers2.xlsx'],'Sheet1'); 
ticks=A(:,1); clear A
ttype={'MPID_SUB_RATIO','MPID_SUB_BUY_RATIO','MPID_SUB_SELL_RATIO'};
%ttype={'TMBR_SUB_RATIO','TMBR_SUB_BUY_RATIO','TMBR_SUB_SELL_RATIO'};
%mypath='C:/Users/user/Dropbox/Projects/Trader Anonymity/Data/';

%%%%%%%
windows=[120,60,30,10];
numlags=20; %number of lags for autocorrelation figure
numstats=11;
maxlag=50; %maximum lags for ADF tests
%%%%%%%%

%for ww=3 %1:length(windows)

ww=3;

w=windows(ww);
    
%for y=1 %1:4
 
y=1;

if y==1
subset='FullTime';    
elseif y==2
subset='StationaryTime';    
elseif y==3
subset='FullTimeDiff';    
elseif y==4
subset='StationaryTimeDiff';    
end
    
fullstats=nan(length(ttype),numstats);
savestats=nan(length(ticks)*length(ttype),numstats);
savedata=cell(length(ttype),1);

%for j=1:length(ttype)  
    
j=1;
type=ttype{j};

file=[mypath 'Data/' type '_' num2str(w) 'sec.xlsx'];

[~,~,raw]=xlsread(file);

data=cell2mat(raw(2:end,4:end));
%b=cellfun(@ischar,raw(2:end,4:end))
days=cell2mat(raw(2:end,2));
timepoints=cell2mat(raw(2:end,3));

intervals=(9.5*60*60:w:16*60*60)'; intervals=intervals(2:end); intervals=repmat(intervals,length(unique(days)),1);
nintervals=length(unique(intervals));

%q=find(intervals<10.5*60*60|intervals>14*60*60);
q=find(intervals<10.5*60*60);

if y==2||y==4
data(q,:)=[];
days(q)=[];
timepoints(q)=[];
intervals(q)=[];
nintervals=length(unique(intervals));
end

stats=nan(length(ticks),numstats);

for i=1:length(ticks)

v=data(:,i);
if y==3||y==4
v=diff(data(:,i));
end

[~,~,~,~,reg]=adftest(v,'model','TS','lags',0:maxlag);
aic=cell2mat({reg.AIC});
[~,q]=min(aic);
    %if q==maxlag+1
    %    error('Optimal lag is maximum lag.');
    %end
[~,pval_adf,adf,~,~]=adftest(v,'model','AR','lags',q);
[~,pval_pp,pp,~,~]=pptest(v,'model','AR','lags',q);
[~,pval_kpss1,kpss1,~]=kpsstest(v,'trend',false,'lags',q);
[~,pval_kpss2,kpss2,~]=kpsstest(v,'trend',true,'lags',q);
stats(i,:)=[mean(v) median(v) std(v) min(v) max(v) skewness(v) q pval_adf pval_pp pval_kpss1 pval_kpss2];

temp=type(1:4);
hh=figure;
autocorr(v,numlags)
grid off
if j==1 
%title(['Autocorrelation of ' temp ' Submission Intensites: ' ticks{i}]);
title(ticks{i},'FontSize',18);
elseif j==2 
title(['Autocorrelation of ' temp ' Buy Submission Intensites: ' ticks{i}]);
elseif j==3 
title(['Autocorrelation of ' temp ' Sell Submission Intensites: ' ticks{i}]);
end
ti = get(gca,'TightInset');
set(gca,'Position',[ti(1) ti(2) 1-ti(3)-ti(1) 1-ti(4)-ti(2)]);
set(gca,'units','centimeters')
pos = get(gca,'Position');
ti = get(gca, 'TightInset');
set(gcf, 'PaperUnits','centimeters');
set(gcf, 'PaperSize', [pos(3)+ti(1)+ti(3) pos(4)+ti(2)+ti(4)]);
set(gcf, 'PaperPositionMode', 'manual');
set(gcf, 'PaperPosition',[0 0 pos(3)+ti(1)+ti(3) pos(4)+ti(2)+ti(4)]);
saveas(hh,[mypath 'Data\Graphs\Autocorrelation\AutoGraphs_' type '_' ticks{i} '_' num2str(w) 'sec' '_' subset '.pdf']); 
%saveas(hh,[mypath 'Graphs\Autocorrelation\Graphs_' type '_' ticks{i} '_' num2str(w) 'sec' '_' subset '.pdf']); 
%saveas(hh,['C:\Users\user\Dropbox\Projects\Trader Anonymity\Drafts\Draft_20170419\docs\Graphs_' type '_' ticks{i} '_' num2str(w) 'sec' '_' subset '.pdf']); 
end

close all

savestats(1+length(ticks)*(j-1):length(ticks)+length(ticks)*(j-1),:)=stats;

v=data(:);
if y==3||y==4
v=diff(data(:));   
end

[~,~,~,~,reg]=adftest(v,'model','TS','lags',0:maxlag);
aic=cell2mat({reg.AIC});
[~,q]=min(aic);
    %if q==maxlag+1
    %    error('Optimal lag is maximum lag.');
    %end
[~,pval_adf,adf,~,~]=adftest(v,'model','AR','lags',q);
[~,pval_pp,pp,~,~]=pptest(v,'model','AR','lags',q);
[~,pval_kpss1,kpss1,~]=kpsstest(v,'trend',false,'lags',q);
[~,pval_kpss2,kpss2,~]=kpsstest(v,'trend',true,'lags',q);
fullstats(j,:)=[mean(v) median(v) std(v) min(v) max(v) skewness(v) q pval_adf pval_pp pval_kpss1 pval_kpss2];
if y==1||y==2
savedata{j}=data;
elseif y==3||y==4
savedata{j}=diff(data);       
end

%end

temp=type(1:4);
filename=[temp '_SUB_RATIO_SummaryStatistics_' num2str(w) 'sec.xlsx'];
table=cell(3*length(ticks)+3*2+3+2,numstats+1);

table(1,1)={['\multicolumn{' num2str(numstats+1) '}{c}{(A.1) All MPID Submissions}']};
table(2,:)={'' 'Mean' 'Median' 'Std. Dev.' 'Min.' 'Max.' 'Skew' 'Lags' 'ADF' 'PP' 'KPSS1' 'KPSS2'};
table(3:3+length(ticks)-1,1)=ticks';

table(3+length(ticks),1)={['\multicolumn{' num2str(numstats+1) '}{c}{(A.2) Buy-Side MPID Submissions}']};
table(1+3+length(ticks),:)={'' 'Mean' 'Median' 'Std. Dev.' 'Min.' 'Max.' 'Skew' 'Lags' 'ADF' 'PP' 'KPSS1' 'KPSS2'};
table(2+3+length(ticks):2+3+2*length(ticks)-1,1)=ticks';

table(2*(3+length(ticks))-1,1)={['\multicolumn{' num2str(numstats+1) '}{c}{(A.3) Sell-Side MPID Submissions}']};
table(1+2*(3+length(ticks))-1,:)={'' 'Mean' 'Median' 'Std. Dev.' 'Min.' 'Max.' 'Skew' 'Lags' 'ADF' 'PP' 'KPSS1' 'KPSS2'};
table(2+2*(3+length(ticks))-1:2+2*(3+length(ticks))-1+length(ticks)-1,1)=ticks';

table(3*(3+length(ticks))-2,1)={['\multicolumn{' num2str(numstats+1) '}{c}{(A.4) All Firms}']};
table(1+3*(3+length(ticks))-2,:)={'' 'Mean' 'Median' 'Std. Dev.' 'Min.' 'Max.' 'Skew' 'Lags' 'ADF' 'PP' 'KPSS1' 'KPSS2'};
table(2+3*(3+length(ticks))-2,1)={'All MPID Submissions'};
table(3+3*(3+length(ticks))-2,1)={'Buy-Side MPID Submissions'};
table(4+3*(3+length(ticks))-2,1)={'Sell-Side MPID Submissions'};

a=savestats(1:length(ticks),:);

for i=1:length(ticks)
for j=1:numstats

table(i+2,1+j)={num2str(a(i,j),'%9.3f')};

%if j==numstats
%table(i+2,1+j)={['$<' num2str(a(i,j),'%9.2f') '$']};    
%end
      
end
end

a=savestats(1+length(ticks):2*length(ticks),:);

for i=1:length(ticks)
for j=1:numstats

table(length(ticks)+i+4,1+j)={num2str(a(i,j),'%9.3f')};

%if j==numstats
%table(length(ticks)+i+4,1+j)={['$<' num2str(a(i,j),'%9.2f') '$']};    
%end
      
end
end

a=savestats(1+2*length(ticks):3*length(ticks),:);

for i=1:length(ticks)
for j=1:numstats

table(2*length(ticks)+i+6,1+j)={num2str(a(i,j),'%9.3f')};

%if j==numstats
%table(2*length(ticks)+i+6,1+j)={['$<' num2str(a(i,j),'%9.2f') '$']};    
%end
      
end
end

a=fullstats;

for i=1:3
for j=1:numstats

table(3*length(ticks)+i+8,1+j)={num2str(a(i,j),'%9.3f')};

%if j==numstats
%table(3*length(ticks)+i+8,1+j)={['$<' num2str(a(i,j),'%9.2f') '$']};    
%end
      
end
end

xlswrite([mypath filename],table,['Sheet' num2str(y)])

intervals=unique(intervals);

labels={'9:30:30','11:00:00','12:30:00','14:30:00','16:00:00'};

q1=find(intervals==9.5*60*60+30); test1=isempty(q1);
q2=find(intervals==11*60*60); test2=isempty(q2);
q3=find(intervals==12.5*60*60); test3=isempty(q3);
q4=find(intervals==14.5*60*60); test4=isempty(q4);
q5=find(intervals==16*60*60); test5=isempty(q5);

tks=[test1 test2 test3 test4 test5];
labels(tks==1)=[];
q=[q1 q2 q3 q4 q5];

data=savedata{1};
data=mean(data,2);
if y==3||y==4
data=[nan; data];       
end
data=reshape(data,nintervals,length(unique(days)));
data=mean(data,2);

data2=savedata{2};
data2=mean(data2,2);
if y==3||y==4
data2=[nan; data2];       
end
data2=reshape(data2,nintervals,length(unique(days)));
data2=mean(data2,2);

data3=savedata{3};
data3=mean(data3,2);
if y==3||y==4
data3=[nan; data3];       
end
data3=reshape(data3,nintervals,length(unique(days)));
data3=mean(data3,2);

temp=type(1:4);
hh=figure;
plot(data);
ylabel([temp ' Submission Ratio']);
set(gca,'XTick',q,'XTickLabel',labels);
ti = get(gca,'TightInset');
set(gca,'Position',[ti(1) ti(2) 1-ti(3)-ti(1) 1-ti(4)-ti(2)]);
set(gca,'units','centimeters')
pos = get(gca,'Position');
ti = get(gca, 'TightInset');
set(gcf, 'PaperUnits','centimeters');
set(gcf, 'PaperSize', [pos(3)+ti(1)+ti(3) pos(4)+ti(2)+ti(4)]);
set(gcf, 'PaperPositionMode', 'manual');
set(gcf, 'PaperPosition',[0 0 pos(3)+ti(1)+ti(3) pos(4)+ti(2)+ti(4)]);
saveas(hh,[mypath 'Graphs\MPIDSub\' temp 'Submission_Daily_' num2str(w) 'sec' '_' subset '.pdf']); 
%saveas(hh,[mypath 'Graphs\MPIDSub\' temp 'Submission_Daily_' num2str(w) 'sec' '_' subset '.jpg']);
%saveas(hh,['C:\Users\user\Dropbox\Projects\Trader Anonymity\Drafts\Draft_20170419\docs\' temp 'Submission_Daily_' num2str(w) 'sec' '_' subset '.pdf']); 

temp=type(1:4);
hh=figure;
plot([data data2 data3]);
legend('Total','Buy','Sell');
ylabel([temp ' Submission Ratio']);
set(gca,'XTick',q,'XTickLabel',labels);
ti = get(gca,'TightInset');
set(gca,'Position',[ti(1) ti(2) 1-ti(3)-ti(1) 1-ti(4)-ti(2)]);
set(gca,'units','centimeters')
pos = get(gca,'Position');
ti = get(gca, 'TightInset');
set(gcf, 'PaperUnits','centimeters');
set(gcf, 'PaperSize', [pos(3)+ti(1)+ti(3) pos(4)+ti(2)+ti(4)]);
set(gcf, 'PaperPositionMode', 'manual');
set(gcf, 'PaperPosition',[0 0 pos(3)+ti(1)+ti(3) pos(4)+ti(2)+ti(4)]);
saveas(hh,[mypath 'Graphs\MPIDSub\' temp 'Submission_Daily_' num2str(w) 'sec' '_' subset '.pdf']); 
%saveas(hh,[mypath 'Graphs\MPIDSub\' temp 'Submission_Daily_' num2str(w) 'sec' '_' subset '.jpg']);
%saveas(hh,['C:\Users\user\Dropbox\Projects\Trader Anonymity\Drafts\Draft_20170419\docs\' temp 'Submission_Daily_ByType_' num2str(w) 'sec' '_' subset '.pdf']); 

data=savedata{1};
averagedaily=nan(nintervals,length(ticks));
for i=1:length(ticks)
temp=data(:,i);    
if y==3||y==4
temp=[nan; temp];       
end
temp=reshape(temp,nintervals,length(unique(days)));   
temp=mean(temp,2);
averagedaily(:,i)=temp;
end

temp=type(1:4);
hh=figure;
plot(averagedaily);
legend(ticks);
ylabel([temp ' Submission Ratio']);
set(gca,'XTick',q,'XTickLabel',labels);
ti = get(gca,'TightInset');
set(gca,'Position',[ti(1) ti(2) 1-ti(3)-ti(1) 1-ti(4)-ti(2)]);
set(gca,'units','centimeters')
pos = get(gca,'Position');
ti = get(gca, 'TightInset');
set(gcf, 'PaperUnits','centimeters');
set(gcf, 'PaperSize', [pos(3)+ti(1)+ti(3) pos(4)+ti(2)+ti(4)]);
set(gcf, 'PaperPositionMode', 'manual');
set(gcf, 'PaperPosition',[0 0 pos(3)+ti(1)+ti(3) pos(4)+ti(2)+ti(4)]);
saveas(hh,[mypath 'Graphs\MPIDSub\' temp 'Submission_Daily_ByFirm_' num2str(w) 'sec' '_' subset '.pdf']); 
%saveas(hh,['C:\Users\user\Dropbox\Projects\Trader Anonymity\Drafts\Draft_20170419\docs\' temp 'Submission_Daily_ByFirm_' num2str(w) 'sec' '_' subset '.pdf']); 










