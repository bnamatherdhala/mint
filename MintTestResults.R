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
                  
