clear all

windows={'120';'60';'30';'10'};
alpha=0.10;

type=1; 
if type==1
%[~,~,A]=xlsread('D:\Matlab\Tickers2.xlsx','Sheet1'); 
[~,~,A]=xlsread('C:\Users\user\Dropbox\Projects\Trader Anonymity\Tickers2.xlsx','Sheet1'); 
ticks=A(:,1); clear A
ttype='Alpha';
elseif type==2
ticks={'GOOG';'AAPL';'EBAY';'FB';'MSFT';'YHOO';'INTC';'CSCO'}; %by price   
ttype='Price';
elseif type==3
ticks={'CSCO';'GOOG';'AAPL';'FB';'INTC';'YHOO';'MSFT';'EBAY'}; %by HHI index
ttype='HHI';
end

w=3;

%mypath='C:\Projects\Trader Anonymity\AggregateRegressions\';
mypath='C:\Users\user\Dropbox\Projects\Trader Anonymity\Data\';
%myfolder='Ratio_Vol_Ind_Simpler\'; %'Ratio_Vol_Aggr\'
myfolder='Results\Individual_Ratio\';
graphfolder='Graphs\Individual_Ratio_Coefficients\';

for jj=1:14

%jj=1;
    
if ismember(jj,[1:2,11:15,18:20])==1
depvar='MPID.SUB';
elseif ismember(jj,[3:6,16])==1
depvar='MPID.SUB.BUY';    
elseif ismember(jj,[7:10,17])==1
depvar='MPID.SUB.SELL';    
end    
 
%depvar='MPID.SUB.RATIO';

%filename=[mypath myfolder 'Post_Regression_Ratio_' windows{w} 'sec_' num2str(jj) '.xlsx'];
[~,~,raw]=xlsread([mypath myfolder 'Post_Regression_Ratio_' windows{w} 'sec_' num2str(jj) '.xlsx']);
data=importsumexcelfiles([mypath myfolder 'Post_Regression_Ratio_' windows{w} 'sec_' num2str(jj) '.xlsx']);
%[rownames,data]=importsumfiles([mypath myfolder 'Summary_Regression_Ratio_' windows{w} 'sec_' num2str(jj) '.txt']);
rownames=raw(2:end,1);
colnames=raw(1,2:end);
%data=cell2mat(raw(2:end,2:end));
clear raw
data=data(:,9:12);
%data=data(:,end-4+1:end);

numreg=length(rownames)/length(ticks);

rownamestemp=rownames(1:numreg);
rownamestemp=strrep(rownamestemp,'AAPL.','');rownamestemp=strrep(rownamestemp,'$','.');rownamestemp=strrep(rownamestemp,'y1.','');

indvars=rownamestemp; clear rownamestemp

temp=data(:,1); coeffs_full=reshape(temp,numreg,length(ticks));
temp=data(:,4); pvals_full=reshape(temp,numreg,length(ticks));
temp=data(:,3); tstats_full=reshape(temp,numreg,length(ticks));

col1=find(not(cellfun('isempty',strfind(indvars,'VOL.GRID.SHORT.PRE'))));
col2=find(not(cellfun('isempty',strfind(indvars,'VOL.SD'))));
col3=find(not(cellfun('isempty',strfind(indvars,'VOL.GRID.LONG.PRE'))));
col4=find(not(cellfun('isempty',strfind(indvars,'RELSPR'))));
col5=find(not(cellfun('isempty',strfind(indvars,'RELEFFSPR.EXE.REP'))));
col6=find(not(cellfun('isempty',strfind(indvars,'RELRLZSPR.EXE.REP.PRE'))));
col7=find(not(cellfun('isempty',strfind(indvars,'SUB.ALL.DVOL'))));
col8=find(not(cellfun('isempty',strfind(indvars,'EXE.ALL.DVOL'))));
col9=find(not(cellfun('isempty',strfind(indvars,'DEPTH.TOTAL.DVOL'))));
col10=find(not(cellfun('isempty',strfind(indvars,'SUB.BUY.DVOL'))));
col11=find(not(cellfun('isempty',strfind(indvars,'EXE.BUY.DVOL'))));
col12=find(not(cellfun('isempty',strfind(indvars,'DEPTH.BUY.DVOL'))));
col13=find(not(cellfun('isempty',strfind(indvars,'SUB.SELL.DVOL'))));
col14=find(not(cellfun('isempty',strfind(indvars,'EXE.SELL.DVOL'))));
col15=find(not(cellfun('isempty',strfind(indvars,'DEPTH.SELL.DVOL'))));
col16=find(not(cellfun('isempty',strfind(indvars,'OPEN'))));
col17=find(not(cellfun('isempty',strfind(indvars,'abs(RELDPR.MID):NEG.DUMMY'))));
col18=find(not(cellfun('isempty',strfind(indvars,'HASBROUCK.MAX'))));

col=[col1; col2; col3; col4; col5; col6; col7; col8; col9; col10; col11; col12; col13; col14; col15; col16; col17; col18];

for hh=1:length(col)

gg=col(hh);    
    
%%Plot Coefficients

coefs=coeffs_full(gg,:);
tstats=tstats_full(gg,:);
pvals=pvals_full(gg,:);

temp1=double(pvals<=alpha); temp1(temp1==0)=NaN;      %get index for significance (coloring the graph)
temp2=double(pvals>alpha); temp2(temp2==0)=NaN;   

t=cell(length(tstats),1);                             %get text list of t-statistics
    for j=1:length(tstats);
    t{j}=sprintf('%0.2f',tstats(j));
    t{j}=['(' t{j} ')'];
    end

hh=figure;
hold on
h1=bar((coefs.*temp1),'FaceColor',[0,0,0.75]);
h2=bar((coefs.*temp2),'FaceColor',[0,0,0.75]); 
hpatch=get(h2(1), 'children'); set(hpatch,'FaceAlpha',.3);
hold off
title(depvar)
ylabel([indvars{gg} ' Coefficient Value']);
ylim([min(coefs)-2*std(coefs) max(coefs)+2*std(coefs)])
    if max(coefs)<=0
    dist=repmat(min(coefs)-std(coefs),length(ticks),1);
    text((1:length(pvals))',dist,ticks,'HorizontalAlignment','center'); 
    text((1:length(pvals))',dist-0.3*std(coefs),t,'HorizontalAlignment','center','FontSize',10); 
    else
    dist=repmat(max(coefs)+std(coefs),length(ticks),1);
    text((1:length(pvals))',dist,ticks,'HorizontalAlignment','center'); 
    text((1:length(pvals))',dist-0.3*std(coefs),t,'HorizontalAlignment','center','FontSize',10); 
    end
set(gca,'XTick',1:length(ticks),'XTickLabel','');
ti = get(gca,'TightInset');
set(gca,'Position',[ti(1) ti(2) 1-ti(3)-ti(1) 1-ti(4)-ti(2)]);
set(gca,'units','centimeters')
pos = get(gca,'Position');
ti = get(gca, 'TightInset');
set(gcf, 'PaperUnits','centimeters');
set(gcf, 'PaperSize', [pos(3)+ti(1)+ti(3) pos(4)+ti(2)+ti(4)]);
set(gcf, 'PaperPositionMode', 'manual');
set(gcf, 'PaperPosition',[0 0 pos(3)+ti(1)+ti(3) pos(4)+ti(2)+ti(4)]);
name=indvars{gg};
name=strrep(name,'.','_');name=strrep(name,':','x');
saveas(hh,[mypath graphfolder 'Regression_Ratio_' windows{w} 'sec_' num2str(jj) '_' name '.pdf']); 

end

close all

%%Make Excel File of Errors

filename=[mypath myfolder 'PANELERRORS_STATIONARITY_' num2str(jj) '_' windows{w} 'sec.txt'];
TT=importerrorfiles(filename); TT=TT(1,1:end-1); TT=cell2mat(TT);
errors(jj,:)=TT;

end

numtests=6;
temp=cell(1,numtests*2-1); 
header1=['AAPL',temp,'CSCO',temp,'EBAY',temp,'FB',temp,'GOOG',temp,'INTC',temp,'MSFT',temp,'YHOO',temp];
header2={'ADF','PVAL','PP','PVAL','KPSS.1','PVAL','KPSS.2','PVAL','BOX.TEST','PVAL','T.STAT','PVAL'}; header2=repmat(header2,1,length(ticks)); 
filename=[mypath myfolder 'PANELERRORS_STATIONARITY_BYFIRM_' windows{w} 'sec_.xlsx'];
xlswrite(filename,header1,'Sheet1','B1');
xlswrite(filename,header2,'Sheet1','B2');
xlswrite(filename,errors,'Sheet1','B3');
xlswrite(filename,num2cell((1:25)'),'Sheet1','A3');







