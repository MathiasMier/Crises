* * * Investor type formulation

set
pub(inv) /pub/
pri(inv) /pri/
;

Parameter
invbudget(inv,r,t)
;

invbudget(pub(inv),r,t) = 10000 * daref(r,t) / daref("Germany",t)  ;

Positive Variable
IXINV(inv,i,r,t)
IGINV(inv,j,r,t)
IGCINV(inv,j,r,t)
IGDINV(inv,j,r,t)
IGRINV(inv,j,r,t)
ITINV(inv,k,r,r,t)
;

Equations
ix_investinv(i,r,t)
ig_investinv(j,r,t)
igc_investinv(j,r,t)
igd_investinv(j,r,t)
igr_investinv(j,r,t)
it_investinv(k,r,r,t)
investbudget(inv,r,t)
;


ix_investinv(new(i),r,toptimize(t))..
    IX(i,r,t) =e= sum(inv, IXINV(inv,i,r,t)) ;

ig_investinv(newj(j),r,toptimize(t))..
    IG(j,r,t) =e= sum(inv, IGINV(inv,j,r,t)) ;

igc_investinv(ghyd(j),r,toptimize(t))..
    IGC(j,r,t) =e= sum(inv, IGCINV(inv,j,r,t)) ;
    
igd_investinv(ghyd(j),r,toptimize(t))..
    IGD(j,r,t) =e= sum(inv, IGDINV(inv,j,r,t)) ;
    
igr_investinv(ghyd(j),r,toptimize(t))..
    IGR(j,r,t) =e= sum(inv, IGRINV(inv,j,r,t)) ;
    
it_investinv(k,r,rr,t)..
    IT(k,r,rr,t) =e= sum(inv, ITINV(inv,k,r,rr,t)) ;
    
investbudget(pub(inv),r,toptimize(t))..
      sum(new(i),  sum(tv(t,v),  capcost(i,v,r)) * IXINV(inv,i,r,t))
    + sum(newj(j), sum(tv(t,v), gcapcost(j,v,r)) * IGINV(inv,j,r,t))
    + sum(ghyd(j), sum(tv(t,v), gcapcostc(j,v,r)) * IGCINV(inv,j,r,t))   
    + sum(ghyd(j), sum(tv(t,v), gcapcostd(j,v,r)) * IGDINV(inv,j,r,t))
    + sum(ghyd(j), sum(tv(t,v), gcapcostr(j,v,r)) * IGRINV(inv,j,r,t))
    + sum(tmap(k,r,rr), tcapcost(k,r,rr) * ITINV(inv,k,r,rr,t))
    =l= invbudget(inv,r,t) ;

Positive Variable
EQI(inv,i,v,r)
DBI(inv,i,v,r)

EQITOT(inv,i,r,t)
DBITOT(inv,i,r,t)

EQJ(inv,j,v,r)
DBJ(inv,j,v,r)

EQJTOT(inv,j,r,t)
DBJTOT(inv,j,r,t)

EQK(inv,k,v,r)
DBK(inv,k,v,r)

EQKTOT(inv,k,r,t)
DBKTOT(inv,k,r,t)

EQTOT(inv,r,t)
DBTOT(inv,r,t)

KAPPAVAR(inv,i,v,r)
GKAPPAVAR(inv,j,v,r)
TKAPPAVAR(inv,k,v,r)
;

*not sure if we need to load all the parameters again or if there is a shortcut
Parameter
lambda(i,v,r,t) share of investment still under depreciation
glambda(j,v,r,t)
tlambda(k,v,r,t)

capital_inv(inv,i,r,t)
gcapital_inv(inv,j,r,t)
tcapital_inv(inv,k,r,t)
capitaltot_inv(inv,r,t)
capitaltot(r,t)

equity_inv(inv,i,r,t)
gequity_inv(inv,j,r,t)
tequity_inv(inv,k,r,t)
equitytot_inv(inv,r,t)

debt_inv(inv,i,r,t)
gdebt_inv(inv,j,r,t)
tdebt_inv(inv,k,r,t)
debttot_inv(inv,r,t)

capitalv(i,v,r)
gcapitalv(j,v,r)
tcapitalv(k,v,r)

capitalv_inv(inv,i,v,r)
gcapitalv_inv(inv,j,v,r)
tcapitalv_inv(inv,k,v,r)
capitalvtot_inv(inv,v,r)
capitalvtot(v,r)

equityv_inv(inv,i,v,r)
gequityv_inv(inv,j,v,r)
tequityv_inv(inv,k,v,r)
equityvtot_inv(inv,v,r)

debtv_inv(inv,i,v,r)
gdebtv_inv(inv,j,v,r)
tdebtv_inv(inv,k,v,r)
debtvtot_inv(inv,v,r)
;

Equation
equitylimit(inv,r,t)

equitytotal(inv,r,t)
equity(inv,i,r,t)
gequity(inv,j,r,t)
tequity(inv,k,r,t)

debttotal(inv,r,t)
debt(inv,i,r,t)
gdebt(inv,j,r,t)
tdebt(inv,k,r,t)

investsum(inv,i,v,r)
ginvestsum(inv,j,v,r)
tinvestsum(inv,k,v,r)

kappaequ(inv,i,v,r)
gkappaequ(inv,j,v,r)
tkappaequ(inv,k,v,r)
;





$gdxin precal\precal_%n%.gdx
$load equitytot_inv, equity_inv, gequity_inv, tequity_inv, debt_inv, gdebt_inv, tdebt_inv
$load lambda, glambda, tlambda, debtv_inv, gdebtv_inv, tdebtv_inv
$gdxin

* Total equity limit to avoid massive equity usage
equitylimit(inv,r,toptimize(t))..
    EQTOT(inv,r,t) =l= equitytot_inv(inv,r,"2023") ;

* Equity calculations over all technologies
equitytotal(inv,r,toptimize(t))..
    EQTOT(inv,r,t) =e= sum(i, EQITOT(inv,i,r,t)) + sum(j, EQJTOT(inv,j,r,t)) + sum(k, EQKTOT(inv,k,r,t)) ;

* Technology-specific equity calculation containing age of equity including depreciation (and "free" equity again)
equity(inv,i,r,toptimize(t))..
    EQITOT(inv,i,r,t) =e= sum(ivrt(i,v,r,t),  lambda(i,v,r,t) * EQI(inv,i,v,r)) +  equity_inv(inv,i,r,t) ;
gequity(inv,j,r,toptimize(t))..
    EQJTOT(inv,j,r,t) =e= sum(jvrt(j,v,r,t), glambda(j,v,r,t) * EQJ(inv,j,v,r)) + gequity_inv(inv,j,r,t) ;
tequity(inv,k,r,toptimize(t))..
    EQKTOT(inv,k,r,t) =e= sum(tvrt(k,v,r,t), tlambda(k,v,r,t) * EQK(inv,k,v,r)) + tequity_inv(inv,k,r,t) ;
  
* Technology-specific dent calculation
*debt_inv inprecal: lambda*debtv_inv
debt(inv,i,r,toptimize(t))..
    DBITOT(inv,i,r,t) =e= sum(ivrt(i,v,r,t),  lambda(i,v,r,t) * DBI(inv,i,v,r)) +  debt_inv(inv,i,r,t) ;
gdebt(inv,j,r,toptimize(t))..
    DBJTOT(inv,j,r,t) =e= sum(jvrt(j,v,r,t), glambda(j,v,r,t) * DBJ(inv,j,v,r)) + gdebt_inv(inv,j,r,t) ;
tdebt(inv,k,r,toptimize(t))..
    DBKTOT(inv,k,r,t) =e= sum(tvrt(k,v,r,t), tlambda(k,v,r,t) * DBK(inv,k,v,r)) + tdebt_inv(inv,k,r,t) ;

* Debt calculations over all technologies  
debttotal(inv,r,toptimize(t))..
    DBTOT(inv,r,t) =e= sum(i, DBITOT(inv,i,r,t)) + sum(j, DBJTOT(inv,j,r,t)) + sum(k, DBKTOT(inv,k,r,t)) ;
    
set
$if not  set longrun voptimize(v) /2022,2023,2024,2025,2026,2027,2028,2029,2030,2035,2040,2045,2050/
$if      set longrun voptimize(v) /2023,2030,2035,2040,2045,2050/
;

Parameter
debttot_inv(inv,r,t)
kappa_techno(inv,i,v,r)
kappa_divers(inv,i,v,r)
kappa_newtec(inv,i,v,r)

gkappa_techno(inv,j,v,r)
gkappa_divers(inv,j,v,r)
gkappa_newtec(inv,j,v,r)

tkappa_techno(inv,k,v,r)
tkappa_divers(inv,k,v,r)
tkappa_newtec(inv,k,v,r)

kappa(inv,i,v,r)
gkappa(inv,j,v,r)
tkappa(inv,k,v,r)

;


$gdxin precal\precal_%n%.gdx
$load debttot_inv, kappa_techno, kappa_divers, kappa_newtec, gkappa_techno, gkappa_divers, gkappa_newtec
$load tkappa_techno, tkappa_divers, tkappa_newtec, kappa, gkappa, tkappa
$gdxin


kappaequ(inv,i,voptimize(v),r)..
    KAPPAVAR(inv,i,v,r)  =e= 0.05 * sum(tv(t,v), DBTOT(inv,r,t)) / debttot_inv(inv,r,"2023") +  kappa_techno(inv,i,v,r) +  kappa_divers(inv,i,v,r)+ kappa_newtec(inv,i,v,r); 
gkappaequ(inv,j,voptimize(v),r)..
    GKAPPAVAR(inv,J,v,r) =e= 0.05 * sum(tv(t,v), DBTOT(inv,r,t)) / debttot_inv(inv,r,"2023") + gkappa_techno(inv,j,v,r) + gkappa_divers(inv,j,v,r) + gkappa_newtec(inv,j,v,r) ;
tkappaequ(inv,k,voptimize(v),r)..
    tKAPPAVAR(inv,K,v,r) =e= 0.05 * sum(tv(t,v), DBTOT(inv,r,t)) / debttot_inv(inv,r,"2023") + tkappa_techno(inv,k,v,r) + tkappa_divers(inv,k,v,r) + tkappa_newtec(inv,k,v,r) ;
   

* Investment sum from a certain "vintage"
investsum(inv,i,voptimize(v),r)..
    EQI(inv,i,v,r) + DBI(inv,i,v,r) =e= sum(tv(t,v), IXINV(inv,i,r,t)) * capcost(i,v,r) ;
ginvestsum(inv,j,voptimize(v),r)..
    EQJ(inv,j,v,r) + DBJ(inv,j,v,r) =e= sum(tv(t,v), IGINV(inv,j,r,t))  * gcapcost(j,v,r)    
$if      set hydrogensimple           + sum(tv(t,v), IGCINV(inv,j,r,t)) * gcapcostc(j,v,r)   
$if      set hydrogensimple           + sum(tv(t,v), IGDINV(inv,j,r,t)) * gcapcostd(j,v,r) 
$if      set hydrogensimple           + sum(tv(t,v), IGRINV(inv,j,r,t)) * gcapcostr(j,v,r)
                                      ;
tinvestsum(inv,k,voptimize(v),r)..
    EQK(inv,k,v,r) + DBK(inv,k,v,r) =e= sum(tmap(k,r,rr), sum(tv(t,v), ITINV(inv,k,r,rr,t)) * tcapcost(k,r,rr)) ;

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
rrate_invr(inv,r)
trate_invr(inv,r)
irate_invr(inv,r)
;

$gdxin precal\precal_%n%.gdx
$load rrate_invr, trate_invr, irate_invr
$gdxin



Equations
investcost_equitydebt_inv
investcost_mixed_inv
;
*shortcut zu excel 


$ontext
investcost_equitydebt_inv..
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
                sum(inv,
*               INVESTMENT COST
                !! begin investment cost
* * Generation technologies
* Equity cost on total equity (EQITOT is already with lambda)
                        sum(i, EQITOT(inv,i,r,t) * rrate_invr(inv,r) * (1 - trate_invr(inv,r))) +
* Debt cost on newly added investments
                        sum(ivrt(i,v,r,t), (1 -  lambda(i,v,r,t)) * DBI(inv,i,v,r) * (irate_invr(inv,r) +  KAPPAVAR(inv,i,v,r)) +
* Debt cost of old vintage investments                        
                                           (1 -  lambda(i,v,r,t)) * debtv_inv(inv,i,v,r) * (irate_invr(inv,r) +  kappa(inv,i,v,r))) +
* * Storage technologies
* Equity cost on total equity (EQITOT is already with glambda)
                        sum(j, EQJTOT(inv,j,r,t) * rrate_invr(inv,r) * (1 - trate_invr(inv,r))) +
* Debt cost on newly added investments
                        sum(jvrt(j,v,r,t), (1 - glambda(j,v,r,t)) * DBJ(inv,j,v,r) * (irate_invr(inv,r) + GKAPPAVAR(inv,j,v,r)) +
* Debt cost of old vintage investments                        
                                           (1 - glambda(j,v,r,t)) * gdebtv_inv(inv,j,v,r) * (irate_invr(inv,r) + gkappa(inv,j,v,r))) +
* * Transmission technologies                                           
* Equity cost on total equity (EQITOT is already with tlambda)
                        sum(k, EQKTOT(inv,k,r,t) * rrate_invr(inv,r) * (1 - trate_invr(inv,r))) +
* Debt cost on newly added investments
                        sum(tvrt(k,v,r,t), (1 - tlambda(k,v,r,t)) * DBK(inv,k,v,r) * (irate_invr(inv,r) + TKAPPAVAR(inv,k,v,r)) +
* Debt cost of old vintate investments                        
                                           (1 - tlambda(k,v,r,t)) * tdebtv_inv(inv,k,v,r) * (irate_invr(inv,r) + tkappa(inv,k,v,r)))
* Debt zurückzahlen
               !! end investment cost
               !! end investor type sum 
               )
               !! end region sum
               )
               !! end period sum
               ) ;
$offText             

investcost_equitydebt_inv..
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
                        sum(inv, sum(ivrt(new(i),v,r,t),   lambda(i,v,r,t) * EQI(inv,i,v,r) * rrate_invr(inv,r) * (1 - trate_invr(inv,r)) +  lambda(i,v,r,t) * DBI(inv,i,v,r) * (irate_invr(inv,r) + sum(tv(tt,v),  kappa(inv,i,v,r))) + (1 -  lambda(i,v,r,t)) * DBI(inv,i,v,r))) +
                        sum(inv, sum(jvrt(newj(j),v,r,t), glambda(j,v,r,t) * EQJ(inv,j,v,r) * rrate_invr(inv,r) * (1 - trate_invr(inv,r)) + glambda(j,v,r,t) * DBJ(inv,j,v,r) * (irate_invr(inv,r) + sum(tv(tt,v), gkappa(inv,j,v,r))) + (1 - glambda(j,v,r,t)) * DBJ(inv,j,v,r))) +
                        sum(inv, sum(tvrt(k,v,r,t),       tlambda(k,v,r,t) * EQK(inv,k,v,r) * rrate_invr(inv,r) * (1 - trate_invr(inv,r)) + tlambda(k,v,r,t) * DBK(inv,k,v,r) * (irate_invr(inv,r) + sum(tv(tt,v), tkappa(inv,k,v,r))) + (1 - tlambda(k,v,r,t)) * DBK(inv,k,v,r)))  
$if      set debtprem + sum(inv, sum(tv(tt,v), DBTOT(inv,r,t) * 0.1 * DBTOT(inv,r,t) / equitytot_inv(inv,r,"2023")))
                !! end investment cost
               !! end region sum
               )
               !! end period sum
               ) ;
$ontext        

investcost_equitydebt_inv..
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
                        sum(inv, sum(ivrt(new(i),v,r,t),   lambda(i,v,r,t) * EQI(inv,i,v,r) * rrate_invr(inv,r) * (1 - trate_invr(inv,r)) +  lambda(i,v,r,t) * DBI(inv,i,v,r) * (irate_invr(inv,r) + sum(tv(tt,v), 0.01 * DBTOT(inv,r,t) / equitytot_inv(inv,r,"2023") +  kappa(inv,i,r,tt))) + (1 -  lambda(i,v,r,t)) * DBI(inv,i,v,r))) +
                        sum(inv, sum(jvrt(newj(j),v,r,t), glambda(j,v,r,t) * EQJ(inv,j,v,r) * rrate_invr(inv,r) * (1 - trate_invr(inv,r)) + glambda(j,v,r,t) * DBJ(inv,j,v,r) * (irate_invr(inv,r) + sum(tv(tt,v), 0.01 * DBTOT(inv,r,t) / equitytot_inv(inv,r,"2023") + gkappa(inv,j,r,tt))) + (1 - glambda(j,v,r,t)) * DBJ(inv,j,v,r))) +
                        sum(inv, sum(tvrt(k,v,r,t),       tlambda(k,v,r,t) * EQK(inv,k,v,r) * rrate_invr(inv,r) * (1 - trate_invr(inv,r)) + tlambda(k,v,r,t) * DBK(inv,k,v,r) * (irate_invr(inv,r) + sum(tv(tt,v), 0.01 * DBTOT(inv,r,t) / equitytot_inv(inv,r,"2023") + tkappa(inv,k,r,tt))) + (1 - tlambda(k,v,r,t)) * DBK(inv,k,v,r)))  
               !! end investment cost
               !! end region sum
               )
               !! end period sum
               ) ;
$offtext


           
investcost_mixed_inv..
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
                        sum(new(i),        sum(inv, IXINV(inv,i,r,t)    * sum(ivrttv(i,v,r,t), (capcost(i,v,r) + deccost(i,v,r)) *  zeta(inv,i,v,r)))) +
                        sum(newj(j),       sum(inv, IGINV(inv,j,r,t)    * sum(jvrttv(j,v,r,t), gcapcost(j,v,r)                   * gzeta(inv,j,v,r)))) +
                        sum(ghyd(j),       sum(inv, IGCINV(inv,j,r,t)   * sum(jvrttv(j,v,r,t), gcapcostc(j,v,r)                  * gzeta(inv,j,v,r)))) +
                        sum(ghyd(j),       sum(inv, IGDINV(inv,j,r,t)   * sum(jvrttv(j,v,r,t), gcapcostd(j,v,r)                  * gzeta(inv,j,v,r)))) +
                        sum(ghyd(j),       sum(inv, IGRINV(inv,j,r,t)   * sum(jvrttv(j,v,r,t), gcapcostr(j,v,r)                  * gzeta(inv,j,v,r)))) +
                        sum(tmap(k,r,rr),  sum(inv, ITINV(inv,k,r,rr,t) * sum(tvrttv(k,v,r,t), tcapcost(k,r,rr)                  * tzeta(inv,k,v,r)))) 
               !! end investment cost
               !! end region sum
               )
               !! end period sum
               ) ;
               
$exit

    