Below we summarise the dataset.

The data is limited to the training dataset.


Data frame:crs$dataset[crs$sample, c(crs$input, crs$risk, crs$target)]	35636 observations and 8 variables    Maximum # NAs:0


                            Levels Storage
Number_of_logins                   integer
Offerid                            integer
Merchant                        23 integer
TNM_Monthly.Income                  double
TNM_Monthly.Spend                   double
TNM_Number.of.Bank.Accounts         double
TNM_Offer.time_stamp                double
R01_Event                           double

+--------+-----------------------------------------------------------------+
|Variable|Levels                                                           |
+--------+-----------------------------------------------------------------+
|Merchant|Capital One,Capital One 360,Chase,Choose Energy,Citi             |
|        |ctg.discovery.intuit.com,Efinancial,Fidelity,FutureAdvisor,InsWeb|
|        |Lending Club,Levanto,Motif,Motif Investing,Personal Capital      |
|        |Progressive,Progrexion,Quicken Loans,Quinstreet,Rate Supermarket |
|        |RateSuperMarket.ca,Scotiabank,Wealthfront                        |
+--------+-----------------------------------------------------------------+

For the simple distribution tables below the 1st and 3rd Qu. 
refer to the first and third quartiles, indicating that 25% 
of the observations have values of that variable which are 
less than or greater than (respectively) the value listed.

 Number_of_logins    Offerid                 Merchant     TNM_Monthly.Income
 Min.   :  1.00   Min.   :35619   Chase          :11309   Min.   :   1.0    
 1st Qu.: 28.00   1st Qu.:44588   Motif Investing: 6625   1st Qu.: 506.0    
 Median : 65.00   Median :44860   Fidelity       : 3313   Median :1003.5    
 Mean   : 98.04   Mean   :44380   Capital One    : 2496   Mean   : 977.2    
 3rd Qu.:129.00   3rd Qu.:45130   FutureAdvisor  : 2239   3rd Qu.:1461.0    
 Max.   :805.00   Max.   :45171   Lending Club   : 2152   Max.   :1928.0    
                                  (Other)        : 7502                     
 TNM_Monthly.Spend TNM_Number.of.Bank.Accounts TNM_Offer.time_stamp
 Min.   :   1.0    Min.   :1.000               Min.   :    1       
 1st Qu.:  34.0    1st Qu.:2.000               1st Qu.: 2912       
 Median : 487.0    Median :2.000               Median : 5898       
 Mean   : 533.7    Mean   :1.909               Mean   : 5919       
 3rd Qu.: 945.0    3rd Qu.:2.000               3rd Qu.: 8970       
 Max.   :1436.0    Max.   :5.000               Max.   :11921       
                                                                   
   R01_Event      
 Min.   :0.00000  
 1st Qu.:0.00000  
 Median :0.00000  
 Mean   :0.06693  
 3rd Qu.:0.00000  
 Max.   :1.00000  
                  
                  
                  
                  
 Below is a description of the dataset.
The data is limited to the training dataset.

crs$dataset[crs$sample, c(crs$input, crs$risk, crs$target)] 

 8  Variables      35636  Observations
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Number_of_logins 
      n missing  unique    Info    Mean     .05     .10     .25     .50     .75     .90     .95 
  35636       0     266       1   98.04       6      11      28      65     129     216     316 

lowest :   1   2   3   4   5, highest: 518 593 700 708 805 
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Offerid 
      n missing  unique    Info    Mean     .05     .10     .25     .50     .75     .90     .95 
  35636       0      40       1   44380   42186   43490   44588   44860   45130   45171   45171 

lowest : 35619 41908 42032 42186 42902, highest: 45130 45131 45144 45153 45171 
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Merchant 
      n missing  unique 
  35636       0      23 

lowest : Capital One        Capital One 360    Chase              Choose Energy      Citi              
highest: Quinstreet         Rate Supermarket   RateSuperMarket.ca Scotiabank         Wealthfront        
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
TNM_Monthly.Income 
      n missing  unique    Info    Mean     .05     .10     .25     .50     .75     .90     .95 
  35636       0    1905       1   977.2    97.0   191.5   506.0  1003.5  1461.0  1727.0  1815.0 

lowest :    1    2    3    4    5, highest: 1923 1924 1925 1927 1928 
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
TNM_Monthly.Spend 
      n missing  unique    Info    Mean     .05     .10     .25     .50     .75     .90     .95 
  35636       0    1419    0.99   533.7       1       1      34     487     945    1233    1346 

lowest :    1    2    3    4    5, highest: 1432 1433 1434 1435 1436 
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
TNM_Number.of.Bank.Accounts 
      n missing  unique    Info    Mean 
  35636       0       5    0.71   1.909 

             1     2    3   4   5
Frequency 8315 23076 3531 605 109
%           23    65   10   2   0
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
TNM_Offer.time_stamp 
      n missing  unique    Info    Mean     .05     .10     .25     .50     .75     .90     .95 
  35636       0   11394       1    5919     506    1098    2912    5898    8970   10766   11330 

lowest :     1     2     3     4     5, highest: 11917 11918 11919 11920 11921 
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
R01_Event 
      n missing  unique    Info     Sum    Mean 
  35636       0       2    0.19    2385 0.06693 
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------                 


Title:
 t Test

Test Results:
  PARAMETER:
    x Observations: 66596
    y Observations: 4676
    mu: 0
  SAMPLE ESTIMATES:
    Mean of x: 44407.1925
    Mean of y: 44046.3807
    Var  of x: 2483859.771
    Var  of y: 4522219.814
  STATISTIC:
                T: 11.3848
    T | Equal Var: 14.7412
  P VALUE:
    Alternative Two-Sided: < 2.2e-16 
    Alternative      Less: 1 
    Alternative   Greater: < 2.2e-16 
    Alternative Two-Sided | Equal Var: < 2.2e-16 
    Alternative      Less | Equal Var: 1 
    Alternative   Greater | Equal Var: < 2.2e-16 
  CONFIDENCE INTERVAL:
    Two-Sided: 298.6809, 422.9428
         Less: -Inf, 412.9508
      Greater: 308.6728, Inf
    Two-Sided | Equal Var: 312.8382, 408.7855
         Less | Equal Var: -Inf, 401.0724
      Greater | Equal Var: 320.5513, Inf
      
      
      
      
      
