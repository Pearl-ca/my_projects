%Descargo S&P Futures

%borrar pantalla (tambien se escribe en el editor) y comenzar asi antes de
%todo
%clear; clc; close all;

%Financial Econometrics

%load mydata
load mydata
%Plot prices
pr = AdjClose;
%plot(pr)
%title('SP Futures')
%xlabel Time
%ylabel Prices
%close

%1) Calculate returns and give descriptive statistics.
%Test if there is autocorrelation in the returns. Do we need to model
%the conditional mean?

rt= diff(log(pr))*100 ; %calculate log returns
subplot(2,1,1)
plot(pr)
title('SP Futures')
xlabel Time
ylabel Prices
subplot(2,1,2)
plot(rt)
title('SP 500')
xlabel Time
ylabel Returns
close 

%In the returns plot we can observe that the returns are moving around a
%constant mean, also that there is positive and negative jumps notice that
%after a jump it remains the impact, they have memory.

%We use LB test on returns, to see if there is autocorrelation 
%which may show a need for an ARMA model.

%Test if there is autocorrelation in the returns using Ljung-Box test: 

h_rt=lbqtest(rt,20)

%Ho: There is not autocorrelation in returns, i.e. the first (m=20)
%lags of the ACF of the series are zeros

%h = 0, which means we cannot reject Ho, so there is no autocorrelation in
%returns.  So in lecture it is written that in this case there is no need for an ARMA model.
%As we were trying to forecast, we need to model the conditional mean as we need the residuals, 
%because in the latter question we are asked to model the conditional variance
%so we will use the residuals obtained of the ARMA(p,q) choosen.

%Just for curiosity outliers
%lets take mean +/-5std method for outliers, we find them and keep them.
indp = find(rt > mean(rt) + 5 * std(rt)); %indices of possible positive outliers
indn = find(rt < mean(rt) - 5 * std(rt)); %indices of possible negative outliers
indp; % we keep their indices, por positive and negative outliers
indn;

%I will assume there are not outliers in the model to find the best model
%one. 
%Anyway after finding the model I will run the model removing the outliers, the model
%should fit better.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%2.Plot the autocorrelations and partial autocorrelations of the returns upto lag 10
%and comment which if it is possible to notice which model is likely to ÷t the returns better: AR or MA?

%I draw the ACF and PCF with 10 lags as requested.
%I can see that most of the lags are inside the confidence intervals(considering 20 lags), so there is a low correlation, 
%which explain the result of the LB test, autocorrelation is not
%statistically significant.

%we can have a brief idea of an ARMA model with p,q of at least 1 (for each) are needed,
%because we can see alternation in the spikes and the lags between the ups and downs. 

sacf(rt,10)
close
%autocorr(rt,10)
%close

spacf(rt,10)
close
%parcorr(rt,10)
%close

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%3. (15 pts) Estimate 3 dißerent ARMA speci÷cations (including AR or MA models)
%with dißerent p and q values.
%Make sure that in all these 3 model estimations that you try, the residuals do not have autocorrelation. %Afterwards, choose the best ARMA model.

%ARMA MODELS

%%%%%%%%%%%%%%%%%%%%%

%Let's estimate an ARMA(1,1)

%rt=alpha+beta1*rt_1+eps_t+theta*eps_t-1
%How many parameters => 3 (except the variance of the error term)
C=1; %add an intercept to the model
p=1; %AR order, 1 lag
q=1; %MA order, 1 lag
X=[];%No exogenous variables yet
[parest_ARMA11,LL_ARMA11,err_ARMA11,~,Diag_ARMA11,VCVR_ARMA11]...
                =armaxfilter(rt,C,p,q,X);

%parameters,standard errors, p-values. Are the coefficients significant?
%Intercept is the only not statistically significant (p-value >0.05)

se=sqrt(diag(VCVR_ARMA11));
estres=[parest_ARMA11';se';2*(1-normcdf(abs(parest_ARMA11./se)))']

%To see if the model fitted the data sucessfully
%we use the Ljung box(LB) test in the residuals.
%Ho: a series of residuals exhibits no autocorrelation, i.e. the first (m=20)
%lags of the ACF of the series are zeros

h_ARMA11=lbqtest(err_ARMA11,20)

%h=0, we cannot reject Ho, i.e. there is not enough evidence to say the errors doesn't 
%exhibit no serially autocorrelation. So we assume there is no serial correlation.

%On the ARMA21 and ARMA 22 we will use the same Ho of the LB test to see if
%each one fitted the model succesfully.

%Q3:AIC and (S)BIC values
AIC_ARMA11=Diag_ARMA11.AIC
BIC_ARMA11=Diag_ARMA11.SBIC

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%Let's estimate an ARMA(2,1)

%rt=alpha+beta1*rt_1+beta2*rt_2+eps_t+theta*eps_t-1
%How many parameters==> 4 (except the variance of the error term)
C=1; %add an intercept to the model
p=1:2; %AR order, 2 lag
q=1; %MA order, 1 lag
X=[];%No exogenous variables yet
[parest_ARMA21,LL_ARMA21,err_ARMA21,~,Diag_ARMA21,VCVR_ARMA21]...
                =armaxfilter(rt,C,p,q,X);

se=sqrt(diag(VCVR_ARMA21));
estres=[parest_ARMA21';se';2*(1-normcdf(abs(parest_ARMA21./se)))']
%Intercept and the second autoregressive are not statistically significant (p-value >0.05)

%To see if the model fitted the data sucessfully
%we use the Ljung box(LB) test in the residuals.
%Ho: a series of residuals exhibits no autocorrelation, i.e. the first (m=20)
%lags of the ACF of the series are zeros

h_ARMA21=lbqtest(err_ARMA21,20)

%h=0, i.e. there is not enough evidence to say the errors doesn't exhibit no serially 
%autocorrelation. So we assume there is no serial correlation.

%Q3:AIC and (S)BIC values
AIC_ARMA21=Diag_ARMA21.AIC
BIC_ARMA21=Diag_ARMA21.SBIC

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Let's estimate an ARMA(2,2)

%%rt=alpha+beta1*rt_1+beta2*rt_2+eps_t+theta1*eps_t-1+theta2*eps_t-2
%How many parameters=> 5 (except the variance of the error term)
C=1; %add an intercept to the model
p=1:2; %AR order, 2 lag
q=1:2; %MA order, 2 lag
X=[];%No exogenous variables yet
[parest_ARMA22,LL_ARMA22,err_ARMA22,~,Diag_ARMA22,VCVR_ARMA22]...
                =armaxfilter(rt,C,p,q,X);

se=sqrt(diag(VCVR_ARMA22));
estres=[parest_ARMA22';se';2*(1-normcdf(abs(parest_ARMA22./se)))']

%Intercept, first autoregressive coefficient, and first moving average coefficient 
%are not statistically significant (p-value >0.05)

%To see if the model fitted the data sucessfully
%we use the Ljung box(LB) test in the residuals.
%Ho: a series of residuals exhibits no autocorrelation, i.e. the first (m=20)
%lags of the ACF of the series are zeros

h_ARMA22=lbqtest(err_ARMA22,20)

%h=0, i.e. there is not enough evidence to say the errors doesn't exhibit no serially 
%autocorrelation. So we assume there is no serial correlation.

%Q3:AIC and (S)BIC values
AIC_ARMA22=Diag_ARMA22.AIC
BIC_ARMA22=Diag_ARMA22.SBIC

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%% just for curiosity:

%Let's estimate an ARMA(2,1) but this time considering the outliers, we will replace them.

ind=[indp;indn]; % gathering in one set the indices of all outliers(positives + negatives)

X=zeros(length(rt),length(ind));
for i=1:length(ind);%replace the 0 with 1 when outlier occurs
    X(ind(i),i)=1;
end

%Let's estimate an ARMAX(2,1,) where X is representing the outliers
%rt=alpha + beta1*rt_1 + beta2*rt_2 + Delta*X + eps_t + theta*eps_t-1

%How many parameters==> 4+ 5 (number of outliers)
C=1; %add an intercept to the model
p=1:2; %AR order, 2 lag
q=1; %MA order, 1 lag
[parest_ARMAX21,LL_ARMAX21,err_ARMAX21,~,Diag_ARMAX21,VCVR_ARMAX21]...
                =armaxfilter(rt,C,p,q,X);

%Where the coefficient of intercept and the second order autoregressive 
%are not statistically significant

se=sqrt(diag(VCVR_ARMAX21));
estres=[parest_ARMAX21';se';2*(1-normcdf(abs(parest_ARMAX21./se)))']

h_ARMAX21=lbqtest(err_ARMAX21,20)%errors are not serially correlated

%AIC and (S)BIC values
AIC_ARMAX21=Diag_ARMAX21.AIC
BIC_ARMAX21=Diag_ARMAX21.SBIC

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%After comparing the AIC's an BIC's we conclude that the best model is the:
%ARMA(2,1) which has the lowest AIC, in this case the BIC's values did not 
%follow the same order.


%After comparing AIC of each model ARMA(1,1), ARMA(2,1) and ARMA(2,2) we choose ARMA(2,1)
%AICARMA11: -0.256263552769385
%AICARMA21: -0.257713991673155 - - the one with lowest AIC value
%AICARMA22: -0.256524220951142
%AICARMAX21: -0.404853346832184 this was just for curiosity, we can see the AIC and BIC 
%values are improved.

%I considered like if there were NO outliers in my data. 

%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%

%4.Test if the residuals of the best model in part 3 are conditionally heteroscedastic.

%We look at if there is autocorrelation in squared residuals of the ARMA(2,1)
%to decide if we need a volatility model.

%extract the residuals
res=err_ARMA21;
plot(res.^2);
title('Sqr Residuals')

%test volatility
[h,p]=lbqtest(res.^2,20) %Ljung-Box test
[h,p]=archtest(res,20) %Engle's ARCH test

%%apparent memory in the volatility
%%for both tests h=1, p=0
%%Ho: there is no serial correlation
%%We reject Ho in both tests, therefore we assume there is autocorrelation in the squared
%residuals, so we need to model the conditional variance

%%%%%%%%%%%%%%%%%%%%%

%5.Estimate a GJR-GARCH(1,1) model using the residuals of the best model 
%in part 3 and comment on the coe¢ cients and their signi÷cance.
%Calculate the eßect of positive and negative one unit shock on the volatility and compare.

%Using residuals of ARMA(2,1)
%Estimate a GJR-GARCH(1,1) model, comment on the parameter estimates, in
%particular the asymmetry parameter
%GJR-GARCH(1,1)
p=1;%arch order
o=1;%asymmetry order for asymmetric garch model
q=1;%garch order
errortype='NORMAL'; %distribution, also possible 'STUDENTST'
tarchtype=2;%1:absolute values, 2: squares
init=[0.1;0.2;0.05;0.7];%starting values
[paramgjr, LLgjr, ht_gjr, VCVrobust_gjr]=tarch(res,p,o,q,errortype,tarchtype,init);
se=sqrt(diag(VCVrobust_gjr));%standard errors
pval=2*(1-normcdf(abs(paramgjr)./se));%pvalues
[paramgjr';se';pval']
AICgjr=aicbic(LLgjr,length(paramgjr),length(rt));

%%We can see the coefficients of the parameters:
%intercept: 0.0377 , arch: 0.0419, asymmetric: 0.3675 and garch:0.7491
%and their p-value's to see if there are statistically significant. 
%All of the coefficients are statistically significant (p-value<0.05) except the
%arch coefficient, which we can notice is not statistically significant as
%p-vale=0.5486 > 0.05

%Comment on the effect of the positive and negative shocks on the
%volatility

%positive effect, considering residuals + 1

p=1;%arch order
o=1;%asymmetry order for asymmetric garch model
q=1;%garch order
errortype='NORMAL'; %distribution, also possible 'STUDENTST'
tarchtype=2;%1:absolute values, 2: squares
init=[0.1;0.2;0.05;0.7];%starting values
[paramgjr1, LLgjr1, ht_gjr1, VCVrobust_gjr1]=tarch(res+1,p,o,q,errortype,tarchtype,init);
se=sqrt(diag(VCVrobust_gjr1));%standard errors
pval=2*(1-normcdf(abs(paramgjr1)./se));%pvalues
[paramgjr1';se';pval']
AICgjr1=aicbic(LLgjr1,length(paramgjr1),length(rt));

%negative effect, considering residuals - 1

p=1;%arch order
o=1;%asymmetry order for asymmetric garch model
q=1;%garch order
errortype='NORMAL'; %distribution, also possible 'STUDENTST'
tarchtype=2;%1:absolute values, 2: squares
init=[0.1;0.2;0.05;0.7];%starting values
[paramgjr_1, LLgjr_1, ht_gjr_1, VCVrobust_gjr_1]=tarch(res-1,p,o,q,errortype,tarchtype,init);
se=sqrt(diag(VCVrobust_gjr_1));%standard errors
pval=2*(1-normcdf(abs(paramgjr_1)./se));%pvalues
[paramgjr_1';se';pval']
AICgjr_1=aicbic(LLgjr_1,length(paramgjr_1),length(rt));

%Calculate the effect of positive and negative one unit shock on 
%the volatility and compare:

%I ran three different cases to compare them, in the three I use the residuals of AR(2),

% First case, I use this case to compare versus positive and
%negative effects in volatility.
%I consider as input the residuals, the coefficient of the asymmetric effect is 0.3675 ... (I)

%Second case, consider a one unit positive shock effect.
%I consider as input the (residuals + 1), the coefficient of the asymmetric effect is 0.5563 ... (II)

%Third case, this case consider a one unit negative shock effect.
%I consider as input the (residuals - 1), the coefficient of the asymmetric effect is 0.0936 ...(III)

%Comparing (I) vs (III) and (I) vs (II) we can notice that the magnitud of the negative effect is 
%greater than the magnitud of the positive effect, i.e.

%absolute value((I) - (II))= 0.2739 > 0.1888 = absolute value((I) - (III))

%%%%%%%%
%%aic gjr: 2.768544712590800e+03







