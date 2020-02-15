clear all

windows={'120';'60';'30';'10'};
alpha=0.10;
mpidlags=1;
depvarlags=5;
numtests=6;

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

mypath='C:\Users\user\Dropbox\Projects\Trader Anonymity\Data\';
%myfolder='Results\Individual_Post_Ratio_NoInst\';
myfolder='Results\Individual_Post_Ratio\';
myfolder2=[];
%myfolder2=[num2str(mpidlags) 'MPIDLags_' num2str(depvarlags) 'DepVarLags_ExclAAPL\'];
%myfolder2=[num2str(mpidlags) 'MPIDLags_' num2str(depvarlags) 'DepVarLags\'];
%graphfolder='Graphs\Individual_Post_Ratio_NoInst_Coefficients\';
graphfolder='Graphs\Individual_Post_Ratio_Coefficients\';

ticks=ticks(2:end);

for jj=1:28

%jj=1;

filename=[mypath myfolder myfolder2 'Summary_Post_Regression_Ratio_' windows{w} 'sec_' num2str(jj) '.txt'];
[rownames,T]=importsumfiles(filename); T=cell2mat(T);
numreg=length(rownames)/length(ticks);
%%take last 4 columns of data matrix
n=size(T,2);
%data=T(:,1:4);
data=T(:,1:4);

if ismember(jj,1:3)==1
depvar='RELSPR';    
elseif jj==4
depvar='RELEFFSPR';
elseif jj==5
depvar='RELRLZSPR';
elseif ismember(jj,6:8)==1
depvar='VOL';
elseif jj==9
depvar='VOL';
elseif jj==10
depvar='VOL';
elseif jj==11
depvar='SUB.ALL';
elseif ismember(jj,[12,14])
depvar='SUB.BUY';
elseif ismember (jj,[13,15])
depvar='SUB.SELL';
elseif jj==16
depvar='EXE.ALL';
elseif ismember(jj,[17,19])
depvar='EXE.BUY';
elseif ismember (jj,[18,20])
depvar='EXE.SELL';
elseif jj==21
depvar='DEPTH.TOTAL';
elseif ismember(jj,[22,24])
depvar='DEPTH.BUY';
elseif ismember (jj,[23,25])
depvar='DEPTH.SELL';
elseif ismember (jj,26:28)
depvar='HASBROUCK';
end

rownamestemp=rownames(1:numreg);
rownamestemp=strrep(rownamestemp,'CSCO.','');rownamestemp=strrep(rownamestemp,'$','.');rownamestemp=strrep(rownamestemp,'y1.','');
indvars=rownamestemp; clear rownamestemp

temp=data(:,1); coeffs_full=reshape(temp,numreg,length(ticks));
temp=data(:,4); pvals_full=reshape(temp,numreg,length(ticks));
temp=data(:,3); tstats_full=reshape(temp,numreg,length(ticks));

col=nan(1,1);

if mpidlags>1
for ii=1:mpidlags
col1=find(not(cellfun('isempty',strfind(indvars,['LAG.MPID.SUB.RATIOLag.' num2str(ii)]))));
col2=find(not(cellfun('isempty',strfind(indvars,['LAG.MPID.SUB.BUY.RATIOLag.' num2str(ii)]))));
col3=find(not(cellfun('isempty',strfind(indvars,['LAG.MPID.SUB.SELL.RATIOLag.' num2str(ii)]))));
col=[col; col1; col2; col3];
end
else
col1=find(not(cellfun('isempty',strfind(indvars,'LAG.MPID.SUB.RATIO'))));
col2=find(not(cellfun('isempty',strfind(indvars,'LAG.MPID.SUB.BUY.RATIO'))));
col3=find(not(cellfun('isempty',strfind(indvars,'LAG.MPID.SUB.SELL.RATIO'))));
col=[col; col1; col2; col3];    
end
col=col(2:end);

ind=cell(1,1);
for hh=1:length(col)
temp=indvars{col(hh)}; temp=strrep(temp,'"','');
ind=[ind; temp];
end
ind=ind(2:end);

%%Plot Coefficients
gg=col;
coefs=coeffs_full(gg,:)';
tstats=tstats_full(gg,:)';
pvals=pvals_full(gg,:)';

temp1=double(pvals<=alpha); temp1(temp1==0)=NaN;      %get index for significance (coloring the graph)
temp2=double(pvals>alpha); temp2(temp2==0)=NaN;   

%t=cell(length(tstats),1);                             %get text list of t-statistics
%    for j=1:length(tstats);
%    t{j}=sprintf('%0.2f',tstats(j));
%    t{j}=['(' t{j} ')'];
%    end

t=cell(size(tstats));                             %get text list of t-statistics
    for jjj=1:size(tstats,1);
        for ij=1:size(tstats,2);
    t{jjj,ij}=sprintf('%0.2f',tstats(jjj,ij));
    t{jjj,ij}=['(' t{jjj,ij} ')'];
        end
    end

hh=figure;
hold on
h1=bar((coefs.*temp1));
h2=bar((coefs.*temp2)); 
for ii=1:length(col)
hpatch=get(h2(ii), 'children'); set(hpatch,'FaceAlpha',.3);
end
%h_legend=legend(ind,'Location','northoutside','Orientation','horizontal');
%set(h_legend,'FontSize',8); 
%hpatch=get(h2(1), 'children'); set(hpatch,'FaceAlpha',.3);
title(depvar)
ylabel('MPID.SUB Coefficient Value');
%ylim([min(coefs)-2*std(coefs) max(coefs)+2*std(coefs)])
ylim([min(coefs(:))-2*std(coefs(:)) max(coefs(:))+2*std(coefs(:))])
    if max(coefs(:))<=0
    dist=repmat(min(coefs(:))-0.5*std(coefs(:)),length(ticks),1);
    dist2=min(coefs(:))-0.5*std(coefs(:));
    text((1:length(pvals))',dist,ticks,'HorizontalAlignment','center'); 
    for i=1:length(col)
    XDATA=get(get(h1(i),'Children'),'XData');
    for j=1:length(ticks)
     x=XDATA(1,j)+(XDATA(3,j)-XDATA(1,j))/2;        
     text(x,dist2-0.8*std(coefs(:)),t(j,i),'HorizontalAlignment','center','FontSize',8,'Rotation',90);     
    end
    end       
    else
    dist=repmat(max(coefs(:))+1.6*std(coefs(:)),length(ticks),1);
    dist2=max(coefs(:))+1.6*std(coefs(:));
    text((1:length(pvals))',dist,ticks,'HorizontalAlignment','center'); 
    for i=1:length(col)
    XDATA=get(get(h1(i),'Children'),'XData');
    for j=1:length(ticks)
     x=XDATA(1,j)+(XDATA(3,j)-XDATA(1,j))/2;        
     text(x,dist2-0.8*std(coefs(:)),t(j,i),'HorizontalAlignment','center','FontSize',8,'Rotation',90);     
    end
    end
    end
hold off
set(gca,'XTick',1:length(ticks),'XTickLabel','');
ti = get(gca,'LooseInset');
set(gca,'Position',[ti(1) ti(2) 1-ti(3)-ti(1) 1-ti(4)-ti(2)]);
set(gca,'units','centimeters')
pos = get(gca,'Position');
ti = get(gca, 'LooseInset');
set(gcf, 'PaperUnits','centimeters');
set(gcf, 'PaperSize', [pos(3)+ti(1)+ti(3) pos(4)+ti(2)+ti(4)]);
set(gcf, 'PaperPositionMode', 'manual');
set(gcf, 'PaperPosition',[0 0 pos(3)+ti(1)+ti(3) pos(4)+ti(2)+ti(4)]);
name=ind;
name=strrep(name,'.','_');name=strrep(name,':','x');
saveas(hh,[mypath graphfolder 'Regression_Ratio_' windows{w} 'sec_' num2str(jj) '_MPID_SUB_' num2str(mpidlags) 'MPIDLags_' num2str(depvarlags) 'DepVarLags.pdf']); 

%end

close all

%%Make Excel File of Errors

filename=[mypath myfolder myfolder2 'INDIVIDUALERRORS_STATIONARITY_BYFIRM_' num2str(mpidlags) 'MPIDLags_' num2str(depvarlags) 'DepVarLags_' windows{w} 'sec_' num2str(jj) '.txt'];
TT=importerrorfiles(filename); TT=TT(1,1:end-1); TT=cell2mat(TT);
errors(jj,:)=TT;

end


temp=cell(1,numtests*2-1); 
header1=['AAPL',temp,'CSCO',temp,'EBAY',temp,'FB',temp,'GOOG',temp,'INTC',temp,'MSFT',temp,'YHOO',temp];
header2={'ADF','PVAL','PP','PVAL','KPSS.1','PVAL','KPSS.2','PVAL','BOX.TEST','PVAL','T.STAT','PVAL'}; header2=repmat(header2,1,length(ticks)); 
filename=[mypath myfolder myfolder2 'INDIVIDUALERRORS_STATIONARITY_BYFIRM_' num2str(mpidlags) 'MPIDLags_' num2str(depvarlags) 'DepVarLags_' windows{w} 'sec_.xlsx'];
xlswrite(filename,header1,'Sheet1','B1');
xlswrite(filename,header2,'Sheet1','B2');
xlswrite(filename,errors,'Sheet1','B3');
xlswrite(filename,num2cell((1:25)'),'Sheet1','A3');

