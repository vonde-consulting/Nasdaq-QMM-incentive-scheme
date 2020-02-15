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

%ticks=ticks(2:end);

w=3;

mypath='C:\Users\user\Dropbox\Projects\Trader Anonymity\Data\';
myfolder='Results\Individual_Post_Ratio\';
myfolder2=[num2str(mpidlags) 'MPIDLags_' num2str(depvarlags) 'DepVarLags\'];
%graphfolder='Graphs\Individual_Post_Ratio_NoInst_Coefficients\';
graphfolder='Graphs\Individual_Post_Ratio_Coefficients\';


for jj=1:28

%jj=1;

filename=[mypath myfolder myfolder2 'Summary_Post_Regression_Ratio_' windows{w} 'sec_' num2str(jj) '.txt'];
[rownames,T]=importsumfiles(filename); T=cell2mat(T);
numreg=length(rownames)/length(ticks);
%%take last 4 columns of data matrix
n=size(T,2);
data=T(:,n-4+1:n);

rownamestemp=rownames(1:numreg);
rownamestemp=strrep(rownamestemp,'CSCO.','');rownamestemp=strrep(rownamestemp,'$','.');rownamestemp=strrep(rownamestemp,'y1.','');
indvars=rownamestemp; clear rownamestemp

temp=data(:,1); coeffs_full=reshape(temp,numreg,length(ticks));
temp=data(:,2); stderrs_full=reshape(temp,numreg,length(ticks));
temp=data(:,4); pvals_full=reshape(temp,numreg,length(ticks));
temp=data(:,3); tstats_full=reshape(temp,numreg,length(ticks));

col=nan(1,1);
indvar=[];
if mpidlags>1
for ii=1:mpidlags
col1=find(not(cellfun('isempty',strfind(indvars,['LAG.MPID.SUB.RATIOLag.' num2str(ii)]))));
col2=find(not(cellfun('isempty',strfind(indvars,['LAG.MPID.SUB.BUY.RATIOLag.' num2str(ii)]))));
col3=find(not(cellfun('isempty',strfind(indvars,['LAG.MPID.SUB.SELL.RATIOLag.' num2str(ii)]))));
col=[col; col1; col2; col3];
temp=indvars([col1,col2,col3]); temp=strrep(temp,'LAG',['L' num2str(ii)]); temp=strrep(temp,['.RATIOLag.' num2str(ii)],''); temp=strrep(temp,'"','');
indvar=[indvar temp];
end
else
col1=find(not(cellfun('isempty',strfind(indvars,'LAG.MPID.SUB.RATIO'))));
col2=find(not(cellfun('isempty',strfind(indvars,'LAG.MPID.SUB.BUY.RATIO'))));
col3=find(not(cellfun('isempty',strfind(indvars,'LAG.MPID.SUB.SELL.RATIO'))));
col=[col; col1; col2; col3];  
temp=indvars([col1,col2,col3]); temp=strrep(temp,'LAG','L1'); temp=strrep(temp,'.RATIO',''); temp=strrep(temp,'"','');
indvar=[indvar temp];
end
col=col(2:end);
indvar=indvar';

gg=col;
coefs=coeffs_full(gg,:)';
tstats=tstats_full(gg,:)';
pvals=pvals_full(gg,:)';

%%%%%%make formatted table of coefficients

n=length(indvar);
rownames=cell(n*2,1);
table=cell(n*2,length(ticks));

for i=1:n
rownames{1+n*(i-1)}=indvar{1};
temp1=coefs(:,i);
temp2=pvals(:,i);
for j=1:length(ticks)
table(1+n*(i-1),j)={['$' num2str(temp1(j),'%9.4f') '$']};
table(2+n*(i-1),j)={['$[' num2str(temp2(j),'%9.2f') ']$']};
end    
end

%%%%%get durbin-watson tests, etc.

numtests=6;
nn=numtests*2;

%excelfilename=[mypath myfolder myfolder2 'INDIVIDUALERRORS_STATIONARITY_BYFIRM_' num2str(mpidlags) 'MPIDLags_' num2str(depvarlags) 'DepVarLags_' windows{w} 'sec_.xlsx'];
%[~,~,raw]=xlsread(excelfilename);
excelfilename=[mypath myfolder myfolder2 'INDIVIDUALERRORS_STATIONARITY_BYFIRM_' num2str(mpidlags) 'MPIDLags_' num2str(depvarlags) 'DepVarLags_' windows{w} 'sec_' num2str(jj) '.txt'];
data=importerrorfiles(excelfilename);
data=cell2mat(data(1:end-1));
%data=cell2mat(raw(3:end,2:end));
%data=data(jj,:);

adf=[data(1:nn:end);data(2:nn:end)];
kpss=[data(5:nn:end);data(6:nn:end)];
dw=[data(9:nn:end);data(10:nn:end)];
reg=[data(11:nn:end);data(12:nn:end)];
diags=[adf;kpss;dw;reg];

table2=cell(4*2,length(ticks));

for i=1:4
temp=diags(1+(i-1)*2:2+(i-1)*2,:); 
for j=1:length(ticks)
t1=temp(1,j);         
table2(1+(i-1)*2,j)={['$' num2str(t1,'%9.4f') '$']};
t1=temp(2,j);         
table2(2+(i-1)*2,j)={['$[' num2str(t1,'%9.2f') ']$']};
end
end

filename=[mypath myfolder myfolder2 'INSTR_DIAGNOSTICS_BYFIRM_' num2str(mpidlags) 'MPIDLags_' num2str(depvarlags) 'DepVarLags_' windows{w} 'sec_' num2str(jj) '.txt'];
raw=importdiagfile(filename);
raw=cell2mat(raw(:,2:end));
nn=size(raw,1)/2;
%stats=raw(5:end-1,3:4:end)';
%pvals=raw(5:end-1,4:4:end)';
stats=raw(1:nn-1,3:4:end)';
pvals=raw(1:nn-1,4:4:end)';

t1=cell((size(stats,2)-1)*2,length(ticks));
for i=1:size(stats,2)-1
temp1=stats(:,i);
temp2=pvals(:,i);
for j=1:length(ticks)
t1(1+2*(i-1),j)={['$' num2str(temp1(j),'%9.4f') '$']};
t1(2+2*(i-1),j)={['$[' num2str(temp2(j),'%9.2f') ']$']};
end    
end
t2=cell(2,length(ticks));
temp1=stats(:,end);
temp2=pvals(:,end);
for j=1:length(ticks)
t2(1,j)={['$' num2str(temp1(j),'%9.4f') '$']};
t2(2,j)={['$[' num2str(temp2(j),'%9.2f') ']$']};
end   
table3=[t1;t2];

table=[table; cell(1,length(ticks)); table2; table3];

n=length(indvar);
rowtemp1=cell(length(indvar)*2,1);
rowtemp2=cell(length(indvar)*2,1);
for i=1:n
rowtemp1(1+2*(i-1))=indvar(i);
rowtemp1(2+2*(i-1))={'P.VAL'};
rowtemp2(1+2*(i-1))={['L' num2str(i) '.SUB.NUM']};
rowtemp2(2+2*(i-1))={'P.VAL'};
end

rownames=[rownames;{''};{'ADF'};{'P.VAL'};{'KPSS'};{'P.VAL'};{'DW'};{'P.VAL'};{'TSTAT'};{'P.VAL'};rowtemp1;rowtemp2;{'WU-HAUSMAN'};'P.VAL'];

savefilename=[mypath myfolder myfolder2 'SUMMARY_TABLE_' num2str(mpidlags) 'MPIDLags_' num2str(depvarlags) 'DepVarLags_' windows{w} 'sec.xlsx'];
xlswrite(savefilename,ticks',['Sheet' num2str(jj)],'B1');
xlswrite(savefilename,rownames,['Sheet' num2str(jj)],'A2');
xlswrite(savefilename,table,['Sheet' num2str(jj)],'B2');

clear table rownames

end
































