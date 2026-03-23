* * * Endogenous cost of capital
equitycost(r,t)..
    sum(ivrt(i,v,r,t), rho(r) * (1 - trate(r)) *  omega_remain(i,v,r,t) +  omega_linear(i,v,r,t) * EQI(i,v,r))
    sum(jvrt(i,v,r,t), rho(r) * (1 - trate(r)) * gomega_remain(j,v,r,t) + gomega_linear(j,v,r,t) * EQI(i,v,r))
    sum(tvrt(i,v,r,t), rho(r) * (1 - trate(r)) * tomega_remain(k,v,r,t) + tomega_linear(k,v,r,t) * EQI(i,v,r))




capcost(i,v,r) * sum(tv(t,v), IX(i,r,t)) =e= EQI(i,v,r) + DBI(i,v,r) ;







eq_eqtot2022(r,toptimize(t))$(t.val le 2022)..
    EQTOT(r,t) =e= eq0(r)       - sum(i, sum(tv(t,v), EQI(i,v,r))) + zeta * rho * sum(i, sum(v$(v.val le t.val),  deprtime(i,v,r,t) * EQI(i,v,r)))
                                - sum(j, sum(tv(t,v), EQJ(j,v,r))) + zeta * rho * sum(i, sum(v$(v.val le t.val), gdeprtime(j,v,r,t) * EQJ(j,v,r)))
                                - sum(k, sum(tv(t,v), EQK(k,v,r))) + zeta * rho * sum(i, sum(v$(v.val le t.val), tdeprtime(k,v,r,t) * EQK(k,v,r))) ;    
eq_eqtot(r,toptimize(t))$(t.val ge 2023)..
    EQTOT(r,t) =e= EQTOT(r,t-1) - sum(i, sum(tv(t,v), EQI(i,v,r))) + zeta * rho * sum(i, sum(v$(v.val le t.val),  deprtime(i,v,r,t) * EQI(i,v,r)))
                                - sum(j, sum(tv(t,v), EQJ(j,v,r))) + zeta * rho * sum(i, sum(v$(v.val le t.val), gdeprtime(j,v,r,t) * EQJ(j,v,r)))
                                - sum(k, sum(tv(t,v), EQK(k,v,r))) + zeta * rho * sum(i, sum(v$(v.val le t.val), tdeprtime(k,v,r,t) * EQK(k,v,r))) ;
                                
eq_eqitot(i,r,toptimize(t)..
    EQITOT(i,r,t) = sum(tv(t,v), EQI(i,v,r))) + zeta * rho * sum(i, sum(v$(v.val le t.val),  deprtime(i,v,r,t) * EQI(i,v,r)))
    
eq_eqitot(i,r,toptimize(t)..
    EQITOT(i,r,t) = sum(tv(t,v), EQI(i,v,r))) + zeta * rho * sum(i, sum(v$(v.val le t.val),  deprtime(i,v,r,t) * EQI(i,v,r)))

EQTOT(r,t) =l= eq0(r)

Parameter



eqiold(i,v,r)
eqjold(j,v,r)
eqkold(k,v,r)

eqitotold(i,r,t)
eqjtotold(j,r,t)
eqktotold(k,r,t)

eqishare(i,v,r)
eqjshare(j,v,r)
eqkshare(k,v,r)
;

eqiold(i,v,r) = capcost(i,v,r) * capt(i,v,r,"2022") * eqishare(i,v,r) ;
eqitotold(i,r,t) = sum(tv(t,v), eqiold(i,v,r)) + sum(v$(v.val le t.val), 0.2 * eqiold(i,v,r))

Positive Variable
EQI(i,v,r) 
DBI(i,v,r)

EQITOT(i,r,t)
DBITOT(i,r,t)

EQJ(j,v,r)
DBJ(j,v,r)

EQJTOT(j,r,t)
DBJTOT(j,r,t)

EQK(k,v,r)
DBK(k,v,r)

EQKTOT(k,r,t) 
DBKTOT(k,r,t)

EQTOT(r,t) Total equity used
DBTOT(r,t) Current debt

KAPPAVAR(i,v,r) Mark-up on intense debt usage
GKAPPAVAR(j,v,r) Mark-up on intense debt usage
TKAPPAVAR(k,v,r) Mark-up on intense debt usage
;

*not sure if we need to load all the parameters again or if there is a shortcut
Parameter
lambda(i,v,r,t) share of investment still under depreciation
glambda(j,v,r,t)
tlambda(k,v,r,t)

capital(i,r,t)
gcapital(j,r,t)
tcapital(k,r,t)
capitaltot(r,t)
capitaltot(r,t)

equity(i,r,t)
gequity(j,r,t)
tequity(k,r,t)
equitytot(r,t)

debt(i,r,t)
gdebt(j,r,t)
tdebt(k,r,t)
debttot(r,t)

capitalv(i,v,r)
gcapitalv(j,v,r)
tcapitalv(k,v,r)

capitalv(i,v,r)
gcapitalv(j,v,r)
tcapitalv(k,v,r)
capitalvtot(v,r)
capitalvtot(v,r)

equityv(i,v,r)
gequityv(j,v,r)
tequityv(k,v,r)
equityvtot(v,r)

debtv(i,v,r)
gdebtv(j,v,r)
tdebtv(k,v,r)
debtvtot(v,r)
;

Equation
equitylimit(r,t)

equitytotal(r,t)
equity(i,r,t)
gequity(j,r,t)
tequity(k,r,t)

debttotal(r,t)
debt(i,r,t)
gdebt(j,r,t)
tdebt(k,r,t)

investsum(i,v,r)
ginvestsum(j,v,r)
tinvestsum(k,v,r)

kappaequ(i,v,r)
gkappaequ(j,v,r)
tkappaequ(k,v,r)
;





$gdxin precal\precal_%n%.gdx
$load equitytot_ equity_ gequity_ tequity_ debt_ gdebt_ tdebt
$load lambda, glambda, tlambda, debtv_ gdebtv_ tdebtv
$gdxin

* Total equity limit to avoid massive equity usage
equitylimit(r,toptimize(t))..
    EQTOT(r,t) =l= equitytot(r,"2023") ;

* Equity calculations over all technologies
equitytotal(r,toptimize(t))..
    EQTOT(r,t) =e= sum(i, EQITOT(i,r,t)) + sum(j, EQJTOT(j,r,t)) + sum(k, EQKTOT(k,r,t)) ;

* Technology-specific equity calculation containing age of equity including depreciation (and "free" equity again)
equity(i,r,toptimize(t))..
    EQITOT(i,r,t) =e= sum(ivrt(i,v,r,t),  lambda(i,v,r,t) * EQI(i,v,r)) +  equity(i,r,t) ;
gequity(j,r,toptimize(t))..
    EQJTOT(j,r,t) =e= sum(jvrt(j,v,r,t), glambda(j,v,r,t) * EQJ(j,v,r)) + gequity(j,r,t) ;
tequity(k,r,toptimize(t))..
    EQKTOT(k,r,t) =e= sum(tvrt(k,v,r,t), tlambda(k,v,r,t) * EQK(k,v,r)) + tequity(k,r,t) ;
  
* Technology-specific dent calculation
*debt inprecal: lambda*debtv
debt(i,r,toptimize(t))..
    DBITOT(i,r,t) =e= sum(ivrt(i,v,r,t),  lambda(i,v,r,t) * DBI(i,v,r)) +  debt(i,r,t) ;
gdebt(j,r,toptimize(t))..
    DBJTOT(j,r,t) =e= sum(jvrt(j,v,r,t), glambda(j,v,r,t) * DBJ(j,v,r)) + gdebt(j,r,t) ;
tdebt(k,r,toptimize(t))..
    DBKTOT(k,r,t) =e= sum(tvrt(k,v,r,t), tlambda(k,v,r,t) * DBK(k,v,r)) + tdebt(k,r,t) ;

* Debt calculations over all technologies  
debttotal(r,toptimize(t))..
    DBTOT(r,t) =e= sum(i, DBITOT(i,r,t)) + sum(j, DBJTOT(j,r,t)) + sum(k, DBKTOT(k,r,t)) ;
    
set
$if not  set longrun voptimize(v) /2022,2023,2024,2025,2026,2027,2028,2029,2030,2035,2040,2045,2050/
$if      set longrun voptimize(v) /2023,2030,2035,2040,2045,2050/
;

Parameter
debttot(r,t)
kappa_techno(i,v,r)
kappa_divers(i,v,r)
kappa_newtec(i,v,r)

gkappa_techno(j,v,r)
gkappa_divers(j,v,r)
gkappa_newtec(j,v,r)

tkappa_techno(k,v,r)
tkappa_divers(k,v,r)
tkappa_newtec(k,v,r)

kappa(i,v,r)
gkappa(j,v,r)
tkappa(k,v,r)

;


$gdxin precal\precal_%n%.gdx
$load debttot_ kappa_techno, kappa_divers, kappa_newtec, gkappa_techno, gkappa_divers, gkappa_newtec
$load tkappa_techno, tkappa_divers, tkappa_newtec, kappa, gkappa, tkappa
$gdxin


kappaequ(i,voptimize(v),r)..
    KAPPAVAR(i,v,r)  =e= 0.05 * sum(tv(t,v), DBTOT(r,t)) / debttot(r,"2023") +  kappa_techno(i,v,r) +  kappa_divers(i,v,r)+ kappa_newtec(i,v,r); 
gkappaequ(j,voptimize(v),r)..
    GKAPPAVAR(J,v,r) =e= 0.05 * sum(tv(t,v), DBTOT(r,t)) / debttot(r,"2023") + gkappa_techno(j,v,r) + gkappa_divers(j,v,r) + gkappa_newtec(j,v,r) ;
tkappaequ(k,voptimize(v),r)..
    tKAPPAVAR(K,v,r) =e= 0.05 * sum(tv(t,v), DBTOT(r,t)) / debttot(r,"2023") + tkappa_techno(k,v,r) + tkappa_divers(k,v,r) + tkappa_newtec(k,v,r) ;
   

* Investment sum from a certain "vintage"
investsum(i,voptimize(v),r)..
    EQI(i,v,r) + DBI(i,v,r) =e= sum(tv(t,v), IXINV(i,r,t)) * capcost(i,v,r) ;
ginvestsum(j,voptimize(v),r)..
    EQJ(j,v,r) + DBJ(j,v,r) =e= sum(tv(t,v), IGINV(j,r,t))  * gcapcost(j,v,r)    
$if      set hydrogensimple           + sum(tv(t,v), IGCINV(j,r,t)) * gcapcostc(j,v,r)   
$if      set hydrogensimple           + sum(tv(t,v), IGDINV(j,r,t)) * gcapcostd(j,v,r) 
$if      set hydrogensimple           + sum(tv(t,v), IGRINV(j,r,t)) * gcapcostr(j,v,r)
                                      ;
tinvestsum(k,voptimize(v),r)..
    EQK(k,v,r) + DBK(k,v,r) =e= sum(tmap(k,r,rr), sum(tv(t,v), ITINV(k,r,rr,t)) * tcapcost(k,r,rr)) ;

* todo
* * Calibration
* * Lambda bzw deprtime(i,v,r,t) for linear deprecaition for equity freedom and debt repayment
* * Objective function definition anpassen
* * change v and t and vv matrix into correct values

$if      set onetype    IXINV.FX("pub",i,r,t) = 0 ;
$if      set onetype    IGINV.FX("pub",j,r,t) = 0 ;
$if      set onetype    IGCINV.FX("pub",j,r,t) = 0 ;
$if      set onetype    IGDINV.FX("pub",j,r,t) = 0 ;
$if      set onetype    IGRINV.FX("pub",j,r,t) = 0 ;
$if      set onetype    ITINV.FX("pri",k,r,rr,t) = 0 ;
   
Positive variable
INVCOSTEQUITYDEBT
INVCOSTMIXEDINV
;

Parameter
rrate(r)
trate(r)
irate(r)
;

$gdxin precal\precal_%n%.gdx
$load rrate, trate, irate
$gdxin



Equations
investcost_equitydebt
investcost_mixed
;
*shortcut zu excel 


$ontext
investcost_equitydebt..
*        Surplus is defined in million EUR
         INVCOSTEQUITYDEBT =e=
*        Sum over all time period t
                !! begin period sum
                sum(toptimize(t), round(dfact(t) / nyrs(t),4) * 
*               Sum over all regions r
                !! begin region sum
                sum(r,
*               Sum over investor types
                !! region investor type sum
                sum(
*               INVESTMENT COST
                !! begin investment cost
* * Generation technologies
* Equity cost on total equity (EQITOT is already with lambda)
                        sum(i, EQITOT(i,r,t) * rrater(r) * (1 - trater(r))) +
* Debt cost on newly added investments
                        sum(ivrt(i,v,r,t), (1 -  lambda(i,v,r,t)) * DBI(i,v,r) * (irater(r) +  KAPPAVAR(i,v,r)) +
* Debt cost of old vintage investments                        
                                           (1 -  lambda(i,v,r,t)) * debtv(i,v,r) * (irater(r) +  kappa(i,v,r))) +
* * Storage technologies
* Equity cost on total equity (EQITOT is already with glambda)
                        sum(j, EQJTOT(j,r,t) * rrater(r) * (1 - trater(r))) +
* Debt cost on newly added investments
                        sum(jvrt(j,v,r,t), (1 - glambda(j,v,r,t)) * DBJ(j,v,r) * (irater(r) + GKAPPAVAR(j,v,r)) +
* Debt cost of old vintage investments                        
                                           (1 - glambda(j,v,r,t)) * gdebtv(j,v,r) * (irater(r) + gkappa(j,v,r))) +
* * Transmission technologies                                           
* Equity cost on total equity (EQITOT is already with tlambda)
                        sum(k, EQKTOT(k,r,t) * rrater(r) * (1 - trater(r))) +
* Debt cost on newly added investments
                        sum(tvrt(k,v,r,t), (1 - tlambda(k,v,r,t)) * DBK(k,v,r) * (irater(r) + TKAPPAVAR(k,v,r)) +
* Debt cost of old vintate investments                        
                                           (1 - tlambda(k,v,r,t)) * tdebtv(k,v,r) * (irater(r) + tkappa(k,v,r)))
* Debt zurückzahlen
               !! end investment cost
               !! end investor type sum 
               )
               !! end region sum
               )
               !! end period sum
              ) ;
 
$offText

equation              
investcost_equitydebt..
;

investcost_equitydebt..
*        Surplus is defined in million EUR
         INVCOSTEQUITYDEBT =e=
*        Sum over all time period t
                !! begin period sum
                sum(toptimize(t),
*               Sum over all regions r
                !! begin region sum
                sum(r, 
*               INVESTMENT COST
                !! begin investment cost
                        sum(ivrt(new(i),v,r,t),  sum(tv(tv), EQI(i,v,r)) + lambda(i,v,r,t) * EQI(i,v,r) * rrate(r) * (1 - trate(r)) +  lambda(i,v,r,t) * DBI(i,v,r) * (irater(r) + sum(tv(tt,v),  kappa(i,v,r))) + (1 -  lambda(i,v,r,t)) * DBI(i,v,r)) +
                        sum(jvrt(newj(j),v,r,t), glambda(j,v,r,t) * EQJ(j,v,r) * rrater(r) * (1 - trater(r)) + glambda(j,v,r,t) * DBJ(j,v,r) * (irater(r) + sum(tv(tt,v), gkappa(j,v,r))) + (1 - glambda(j,v,r,t)) * DBJ(j,v,r)) +
                        sum(tvrt(k,v,r,t),       tlambda(k,v,r,t) * EQK(k,v,r) * rrater(r) * (1 - trater(r)) + tlambda(k,v,r,t) * DBK(k,v,r) * (irater(r) + sum(tv(tt,v), tkappa(k,v,r))) + (1 - tlambda(k,v,r,t)) * DBK(k,v,r))  
                      + sum(tv(tt,v), DBTOT(r,t) * 0.1 * DBTOT(r,t) / equitytot(r,"2023")))
                !! end investment cost
               !! end region sum
               )
               !! end period sum
               ) ;


       

cost_equity..
    COSTEQUITY =e=
                !! begin period sum
                sum(toptimize(t),
*               Sum over all regions r
                !! begin region sum
                sum(r, 
*               INVESTMENT COST
                !! begin investment cost
                        sum(ivrt(new(i),v,r,t),  sum(tv(tv), EQI(i,v,r)) + lambda(i,v,r,t) * EQI(i,v,r) * rrate(r) * (1 - trate(r)) +  lambda(i,v,r,t) * DBI(i,v,r) * (irater(r) + sum(tv(tt,v),  kappa(i,v,r))) + (1 -  lambda(i,v,r,t)) * DBI(i,v,r)) +
                        sum(jvrt(newj(j),v,r,t), glambda(j,v,r,t) * EQJ(j,v,r) * rrater(r) * (1 - trater(r)) + glambda(j,v,r,t) * DBJ(j,v,r) * (irater(r) + sum(tv(tt,v), gkappa(j,v,r))) + (1 - glambda(j,v,r,t)) * DBJ(j,v,r)) +
                        sum(tvrt(k,v,r,t),       tlambda(k,v,r,t) * EQK(k,v,r) * rrater(r) * (1 - trater(r)) + tlambda(k,v,r,t) * DBK(k,v,r) * (irater(r) + sum(tv(tt,v), tkappa(k,v,r))) + (1 - tlambda(k,v,r,t)) * DBK(k,v,r))  
                      + sum(tv(tt,v), DBTOT(r,t) * 0.1 * DBTOT(r,t) / equitytot(r,"2023")))
                !! end investment cost
               !! end region sum
               )
               !! end period sum
               ) ;

investcost_equitydebt..
*        Surplus is defined in million EUR
         INVCOSTEQUITYDEBT =e=
*        Sum over all time period t
                !! begin period sum
                sum(toptimize(t),
*               Sum over all regions r
                !! begin region sum
                sum(r, 
*               INVESTMENT COST
                !! begin investment cost
                        sum( sum(ivrt(new(i),v,r,t),   lambda(i,v,r,t) * EQI(i,v,r) * rrater(r) * (1 - trater(r)) +  lambda(i,v,r,t) * DBI(i,v,r) * (irater(r) + sum(tv(tt,v),  kappa(i,v,r))) + (1 -  lambda(i,v,r,t)) * DBI(i,v,r))) +
                        sum( sum(jvrt(newj(j),v,r,t), glambda(j,v,r,t) * EQJ(j,v,r) * rrater(r) * (1 - trater(r)) + glambda(j,v,r,t) * DBJ(j,v,r) * (irater(r) + sum(tv(tt,v), gkappa(j,v,r))) + (1 - glambda(j,v,r,t)) * DBJ(j,v,r))) +
                        sum( sum(tvrt(k,v,r,t),       tlambda(k,v,r,t) * EQK(k,v,r) * rrater(r) * (1 - trater(r)) + tlambda(k,v,r,t) * DBK(k,v,r) * (irater(r) + sum(tv(tt,v), tkappa(k,v,r))) + (1 - tlambda(k,v,r,t)) * DBK(k,v,r)))  
$if      set debtprem + sum( sum(tv(tt,v), DBTOT(r,t) * 0.1 * DBTOT(r,t) / equitytot(r,"2023")))
                !! end investment cost
               !! end region sum
               )
               !! end period sum
               ) ;
$ontext        

investcost_equitydebt..
*        Surplus is defined in million EUR
         INVCOSTEQUITYDEBT =e=
*        Sum over all time period t
                !! begin period sum
                sum(toptimize(t),
*               Sum over all regions r
                !! begin region sum
                sum(r, 
*               INVESTMENT COST
                !! begin investment cost
                        sum( sum(ivrt(new(i),v,r,t),   lambda(i,v,r,t) * EQI(i,v,r) * rrater(r) * (1 - trater(r)) +  lambda(i,v,r,t) * DBI(i,v,r) * (irater(r) + sum(tv(tt,v), 0.01 * DBTOT(r,t) / equitytot(r,"2023") +  kappa(i,r,tt))) + (1 -  lambda(i,v,r,t)) * DBI(i,v,r))) +
                        sum( sum(jvrt(newj(j),v,r,t), glambda(j,v,r,t) * EQJ(j,v,r) * rrater(r) * (1 - trater(r)) + glambda(j,v,r,t) * DBJ(j,v,r) * (irater(r) + sum(tv(tt,v), 0.01 * DBTOT(r,t) / equitytot(r,"2023") + gkappa(j,r,tt))) + (1 - glambda(j,v,r,t)) * DBJ(j,v,r))) +
                        sum( sum(tvrt(k,v,r,t),       tlambda(k,v,r,t) * EQK(k,v,r) * rrater(r) * (1 - trater(r)) + tlambda(k,v,r,t) * DBK(k,v,r) * (irater(r) + sum(tv(tt,v), 0.01 * DBTOT(r,t) / equitytot(r,"2023") + tkappa(k,r,tt))) + (1 - tlambda(k,v,r,t)) * DBK(k,v,r)))  
               !! end investment cost
               !! end region sum
               )
               !! end period sum
               ) ;
$offtext


           
investcost_mixed..
*        Surplus is defined in million EUR
         INVCOSTMIXEDINV =e=
*        Sum over all time period t
                !! begin period sum
                sum(toptimize(t),
*               Sum over all regions r
                !! begin region sum
                sum(r, 
*               INVESTMENT COST
                !! begin investment cost
                        sum(new(i),        sum( IXINV(i,r,t)    * sum(ivrttv(i,v,r,t), (capcost(i,v,r) + deccost(i,v,r)) *  zeta(i,v,r)))) +
                        sum(newj(j),       sum( IGINV(j,r,t)    * sum(jvrttv(j,v,r,t), gcapcost(j,v,r)                   * gzeta(j,v,r)))) +
                        sum(ghyd(j),       sum( IGCINV(j,r,t)   * sum(jvrttv(j,v,r,t), gcapcostc(j,v,r)                  * gzeta(j,v,r)))) +
                        sum(ghyd(j),       sum( IGDINV(j,r,t)   * sum(jvrttv(j,v,r,t), gcapcostd(j,v,r)                  * gzeta(j,v,r)))) +
                        sum(ghyd(j),       sum( IGRINV(j,r,t)   * sum(jvrttv(j,v,r,t), gcapcostr(j,v,r)                  * gzeta(j,v,r)))) +
                        sum(tmap(k,r,rr),  sum( ITINV(k,r,rr,t) * sum(tvrttv(k,v,r,t), tcapcost(k,r,rr)                  * tzeta(k,v,r)))) 
               !! end investment cost
               !! end region sum
               )
               !! end period sum
               ) ;
               
$exit

    