* Cause dollar statements to appear in lst file
$ondollar
* Set EOL comment indicator to the default of !!
$oneolcom

* * * FLH LBS
set
i_flh(i)
ir_flh(i,r)
flhls
;

$gdxin precal\precal_%n%.gdx
$load i_flh, ir_flh, flhls
$gdxin

* * Regional
parameter
bflh(i,r) learning rate (%)
flhstock(i,r,v) knowledge stock (million ?)
flhrdbudget(i,r,v) RD budget (million ?)
flhindex(i,r,v) Average FLH (index 2022 = 1)
flh(i,r,v) Average FLH (h per a)
vrsc_nor(s,i,v,r)
flh_check(i,v,r)
;

$gdxin precal\precal_%n%.gdx
$load bflh,flhstock,flhrdbudget,flhindex,flh,vrsc_nor,flh_check
$gdxin

parameter
flhstockFIRST(i,r)
flhstockSTART(i,r)
flhstockLAST(i,r)
flhstockUP(i,r,flhls)
flhstockLO(i,r,flhls)
flhaccumFIRST(i,r)
flhaccumSTART(i,r)
flhaccumLAST(i,r)
flhaccumUP(i,r,flhls)
flhaccumLO(i,r,flhls)
flhindexFIRST(i,r)
flhindexSTART(i,r)
flhindexLAST(i,r)
flhindexUP(i,r,flhls)
flhindexLO(i,r,flhls)
flhindexSLOPE(i,r,flhls)
;

$gdxin precal\precal_%n%.gdx
$load flhstockFIRST
$load flhstockSTART
$load flhstockLAST
$load flhstockUP
$load flhstockLO
$load flhaccumFIRST
$load flhaccumSTART
$load flhaccumLAST
$load flhaccumUP
$load flhaccumLO
$load flhindexFIRST
$load flhindexSTART
$load flhindexLAST
$load flhindexUP
$load flhindexLO
$load flhindexSLOPE
$gdxin


positive variable
FLHTRY(i,r,t) knowledge stock to increase FLH (million ?)
IFLH(i,r,t) investments into knowledge stock to increase FLH (million ?)
FLHREGLS(i,r,t,flhls) line segment specific knowledge stock
XCFLH(i,v,r,t,flhls) XCS per line segment
;

$if      set fixrdinvest IFLH.FX(i,r,t) = sum(v$(v.val eq t.val), flhrdbudget(i,r,v)) ;

binary variable
RHOFLH(i,r,t,flhls)
;

equation
acc_flh2020(i,r,t)
acc_flh(i,r,t)
rhoflh_mixedip(i,r,t)
flhlsLO_mixedip(i,r,t,flhls)
flhlsUP_mixedip(i,r,t,flhls)
acc_flhls_mixedip(i,r,t)
flh_helper1(i,v,r,t,flhls)
flh_helper2(i,v,r,t,flhls)
flh_helper3(i,v,r,t,flhls)
flh_helper4(i,v,r,t,flhls)
capacity_flholdv(s,i,v,r,t)
capacity_flh(s,i,v,r,t)
;

* Knowledge stock equations
acc_flh2020(i,r,t)$(sameas(t,"2022") and toptimize(t) and ir_flh(i,r))..
FLHTRY(i,r,t) =e= flhstockSTART(i,r) ;
acc_flh(i,r,t)$(t.val ge 2023 and toptimize(t) and ir_flh(i,r))..
FLHTRY(i,r,t) =e= FLHTRY(i,r,t - 1) * round(delta_k_pa**nyrs(t),4) + IFLH(i,r,t) * nyrs(t) ;

* RHO equations
rhoflh_mixedip(i,r,t)$(ir_flh(i,r) and not sameas(t,"2050") and toptimize(t))..
                 sum(flhls, RHOFLH(i,r,t,flhls)) =e= 1 ;                
flhlsLO_mixedip(i,r,t,flhls)$(ir_flh(i,r) and not sameas(t,"2050") and toptimize(t))..
                 FLHREGLS(i,r,t,flhls)              =g= flhstockLO(i,r,flhls) * RHOFLH(i,r,t,flhls) ;
flhlsUP_mixedip(i,r,t,flhls)$(ir_flh(i,r) and not sameas(t,"2050") and toptimize(t))..
                 FLHREGLS(i,r,t,flhls)              =l= flhstockUP(i,r,flhls) * RHOFLH(i,r,t,flhls) ;
acc_flhls_mixedip(i,r,t)$(ir_flh(i,r) and not sameas(t,"2050") and toptimize(t))..
                 FLHTRY(i,r,t)                   =e= sum(flhls, FLHREGLS(i,r,t,flhls)) ;

* Helper equations
flh_helper1(ivrt(i,v,r,t),flhls)$(t.val ge 2023 and toptimize(t) and ir_flh(i,r) and v.val le t.val and v.val ge 2023)..
                XCFLH(i,v,r,t,flhls) =l= irnwlimUP_quantiles(i,r,"q90") * flhindexSLOPE(i,r,flhls) * sum(tv(tt,v), RHOFLH(i,r,tt-1,flhls)) ;
flh_helper2(ivrt(i,v,r,t),flhls)$(t.val ge 2023 and toptimize(t) and ir_flh(i,r) and v.val le t.val and v.val ge 2023)..
                XCFLH(i,v,r,t,flhls) =l= XC(i,v,r,t) * flhindexSLOPE(i,r,flhls) ;
flh_helper3(ivrt(i,v,r,t),flhls)$(t.val ge 2023 and toptimize(t) and ir_flh(i,r) and v.val le t.val and v.val ge 2023)..
                XCFLH(i,v,r,t,flhls) =g= XC(i,v,r,t) * flhindexSLOPE(i,r,flhls) - irnwlimUP_quantiles(i,r,"q90") * flhindexSLOPE(i,r,flhls)* (1 - sum(tv(tt,v), RHOFLH(i,r,tt-1,flhls))) ;
flh_helper4(ivrt(i,v,r,t),flhls)$(t.val ge 2023 and toptimize(t) and ir_flh(i,r) and v.val le t.val and v.val ge 2023)..
                XCFLH(i,v,r,t,flhls) =g= 0 ;

* Capacity equations
capacity_flholdv(s,ivrt(i,v,r,t))$(toptimize(t) and ir_flh(i,r) and v.val le 2022)..
                X(s,i,v,r,t) =l=  XC(i,v,r,t) * vrsc(s,i,v,r) ;             
capacity_flh(s,ivrt(i,v,r,t))$(toptimize(t) and ir_flh(i,r) and v.val ge 2023)..
                X(s,i,v,r,t) =l=  sum(flhls, XCFLH(i,v,r,t,flhls)) * vrsc_nor(s,i,v,r) ;
                
* * European
parameter
bflheur(i) learning rate (%)
flheurindex_reg(i,r,v) regional correction (0 ... X)
flheurstock(i,v) knowledge stock (million ?)
flheurrdbudget(i,v) RD budget (million ?)
flheurindex(i,v) Average FLH (index 2022 = 1)
flheur(i,v) Average FLH (h per a)
vrsceur_nor(s,i,v,r)
flheur_check(i,v,r)
;

$gdxin precal\precal_%n%.gdx
$load bflheur,flheurstock,flheurindex_reg,flheurrdbudget,flheurindex,flheur, vrsceur_nor, flheur_check
$gdxin

parameter
flheurstockFIRST(i)
flheurstockSTART(i)
flheurstockLAST(i)
flheurstockUP(i,flhls)
flheurstockLO(i,flhls)
flheuraccumFIRST(i)
flheuraccumSTART(i)
flheuraccumLAST(i)
flheuraccumUP(i,flhls)
flheuraccumLO(i,flhls)
flheurindexFIRST(i)
flheurindexSTART(i)
flheurindexLAST(i)
flheurindexUP(i,flhls)
flheurindexLO(i,flhls)
flheurindexSLOPE(i,flhls)
;


$gdxin precal\precal_%n%.gdx
$load flheurstockFIRST
$load flheurstockSTART
$load flheurstockLAST
$load flheurstockUP
$load flheurstockLO
$load flheuraccumFIRST
$load flheuraccumSTART
$load flheuraccumLAST
$load flheuraccumUP
$load flheuraccumLO
$load flheurindexFIRST
$load flheurindexSTART
$load flheurindexLAST
$load flheurindexUP
$load flheurindexLO
$load flheurindexSLOPE
$gdxin

positive variable
FLHTRYEUR(i,t) knowledge stock to increase FLH (million ?)
IFLHEUR(i,t) investments into knowledge stock to increase FLH (million ?)
FLHEURLS(i,t,flhls) line segment specific knowledge stock
XCFLHEUR(i,v,r,t,flhls) XCS per line segment
;

$if      set fixrdinvesteur IFLHEUR.FX(i,t) = sum(v$(v.val eq t.val), flheurrdbudget(i,v)) ;

binary variable
RHOFLHEUR(i,t,flhls)
;

equation
acc_flheur2020(i,t)
acc_flheur(i,t)
rhoflheur_mixedip(i,t)
flheurlsLO_mixedip(i,t,flhls)
flheurlsUP_mixedip(i,t,flhls)
acc_flheurls_mixedip(i,t)
flheur_helper1(i,v,r,t,flhls)
flheur_helper2(i,v,r,t,flhls)
flheur_helper3(i,v,r,t,flhls)
flheur_helper4(i,v,r,t,flhls)
capacity_flheuroldv(s,i,v,r,t)
capacity_flheur(s,i,v,r,t)
;

* Knowledge stock equations
acc_flheur2020(i,t)$(sameas(t,"2022") and toptimize(t) and i_flh(i))..
FLHTRYEUR(i,t) =e= flheurstockSTART(i) ;
acc_flheur(i,t)$(t.val ge 2023 and toptimize(t) and i_flh(i))..
FLHTRYEUR(i,t) =e= FLHTRYEUR(i,t-1) * round(delta_k_pa**nyrs(t),4) + IFLHEUR(i,t) * nyrs(t) ;

* RHO equations
rhoflheur_mixedip(i,t)$(i_flh(i) and not sameas(t,"2050") and toptimize(t))..
                 sum(flhls, RHOFLHEUR(i,t,flhls)) =e= 1 ;                
flheurlsLO_mixedip(i,t,flhls)$(i_flh(i) and not sameas(t,"2050") and toptimize(t))..
                 FLHEURLS(i,t,flhls)              =g= flheurstockLO(i,flhls) * RHOFLHEUR(i,t,flhls) ;
flheurlsUP_mixedip(i,t,flhls)$(i_flh(i) and not sameas(t,"2050") and toptimize(t))..
                 FLHEURLS(i,t,flhls)              =l= flheurstockUP(i,flhls) * RHOFLHEUR(i,t,flhls) ;
acc_flheurls_mixedip(i,t)$(i_flh(i) and not sameas(t,"2050") and toptimize(t))..
                 FLHTRYEUR(i,t)                   =e= sum(flhls, FLHEURLS(i,t,flhls)) ;
                 
* Helper equations
flheur_helper1(ivrt(i,v,r,t),flhls)$(t.val ge 2023 and toptimize(t) and i_flh(i) and v.val le t.val and v.val ge 2023)..
                XCFLHEUR(i,v,r,t,flhls) =l= irnwlimUP_quantiles(i,r,"q90") * flheurindexSLOPE(i,flhls) * sum(tv(tt,v), RHOFLHEUR(i,tt-1,flhls)) ;
flheur_helper2(ivrt(i,v,r,t),flhls)$(t.val ge 2023 and toptimize(t) and i_flh(i) and v.val le t.val and v.val ge 2023)..
                XCFLHEUR(i,v,r,t,flhls) =l= XC(i,v,r,t) * flheurindexSLOPE(i,flhls) ;
flheur_helper3(ivrt(i,v,r,t),flhls)$(t.val ge 2023 and toptimize(t) and i_flh(i) and v.val le t.val and v.val ge 2023)..
                XCFLHEUR(i,v,r,t,flhls) =g= XC(i,v,r,t) * flheurindexSLOPE(i,flhls) - irnwlimUP_quantiles(i,r,"q90") * flheurindexSLOPE(i,flhls) * (1 - sum(tv(tt,v), RHOFLHEUR(i,tt-1,flhls))) ;
flheur_helper4(ivrt(i,v,r,t),flhls)$(t.val ge 2023 and toptimize(t) and i_flh(i) and v.val le t.val and v.val ge 2023)..
                XCFLHEUR(i,v,r,t,flhls) =g= 0 ;

* Capacity equations
capacity_flheuroldv(s,ivrt(i,v,r,t))$(toptimize(t) and i_flh(i) and v.val le 2022)..
                X(s,i,v,r,t) =l=  XC(i,v,r,t) * vrsc(s,i,v,r) ;             
capacity_flheur(s,ivrt(i,v,r,t))$(toptimize(t) and i_flh(i) and v.val ge 2023)..
                X(s,i,v,r,t) =l=  sum(flhls, XCFLHEUR(i,v,r,t,flhls)) * vrsceur_nor(s,i,v,r) * flheurindex_reg(i,r,v) ;      

equation
eq_flh_rdbudget_irt(i,r,t) 
eq_flh_rdbudget_rt(r,t)
eq_flh_rdbudget_it(i,t)
eq_flh_rdbudget_ir(i,r)
eq_flh_rdbudget_t(t)
eq_flh_rdbudget_r(r)
eq_flh_rdbudget_i(i)
eq_flh_rdbudget
eq_flheur_rdbudget_it(i,t)
eq_flheur_rdbudget_i(i)
eq_flheur_rdbudget_i(i)
eq_flheur_rdbudget_t(t)
eq_flheur_rdbudget
;

parameter
flhrdbudget_int(i,r,v)
flheurrdbudget_int(i,v)
;

flhrdbudget_int(i,r,v) = flhrdbudget(i,r,v) ;
flheurrdbudget_int(i,v) = flheurrdbudget(i,v) ;

$if      set halfbudget      flhrdbudget(i,r,v)$(v.val ge 2023 and v.val le 2045) = 0.5 * flhrdbudget_int(i,r,v) ;
$if      set halfbudget      flheurrdbudget(i,v)$(v.val ge 2023 and v.val le 2045) = 0.5 * flheurrdbudget_int(i,v) ;

$if      set threeqbudget    flhrdbudget(i,r,v)$(v.val ge 2023 and v.val le 2045) = 0.75 * flhrdbudget_int(i,r,v) ;
$if      set threeqbudget    flheurrdbudget(i,v)$(v.val ge 2023 and v.val le 2045) = 0.75 * flheurrdbudget_int(i,v) ;

$if      set onefiftybudget  flhrdbudget(i,r,v)$(v.val ge 2023 and v.val le 2045) = 1.5 * flhrdbudget_int(i,r,v) ;
$if      set onefiftybudget  flheurrdbudget(i,v)$(v.val ge 2023 and v.val le 2045) = 1.5 * flheurrdbudget_int(i,v) ;

$if      set doublebudget    flhrdbudget(i,r,v)$(v.val ge 2023 and v.val le 2045) = 2 * flhrdbudget_int(i,r,v) ;
$if      set doublebudget    flheurrdbudget(i,v)$(v.val ge 2023 and v.val le 2045) = 2 * flheurrdbudget_int(i,v) ;

eq_flh_rdbudget_irt(ir_flh(i,r),toptimize(t))..
$if not  set fixbudget        IFLH(i,r,t) =l= sum(tv(t,v), flhrdbudget(i,r,v)) ;
$if      set fixbudget        IFLH(i,r,t) =e= sum(tv(t,v), flhrdbudget(i,r,v)) ;
eq_flh_rdbudget_rt(r,toptimize(t))..
$if not  set fixbudget        sum(i_flh(i), IFLH(i,r,t)) =l= sum(i, sum(tv(t,v), flhrdbudget(i,r,v))) ;
$if      set fixbudget        sum(i_flh(i), IFLH(i,r,t)) =e= sum(i, sum(tv(t,v), flhrdbudget(i,r,v))) ;
eq_flh_rdbudget_it(i_flh(i),toptimize(t))..
$if not  set fixbudget        sum(r, IFLH(i,r,t)) =l= sum(r, sum(tv(t,v), flhrdbudget(i,r,v))) ;
$if      set fixbudget        sum(r, IFLH(i,r,t)) =e= sum(r, sum(tv(t,v), flhrdbudget(i,r,v))) ;
eq_flh_rdbudget_ir(ir_flh(i,r))..
$if not  set fixbudget        sum(toptimize(t), nyrs(t) * IFLH(i,r,t)) =l= sum(toptimize(t), nyrs(t) * sum(tv(t,v), flhrdbudget(i,r,v))) ;
$if      set fixbudget        sum(toptimize(t), nyrs(t) * IFLH(i,r,t)) =e= sum(toptimize(t), nyrs(t) * sum(tv(t,v), flhrdbudget(i,r,v))) ;
eq_flh_rdbudget_t(toptimize(t))..
$if not  set fixbudget        sum((ir_flh(i,r)), IFLH(i,r,t)) =l= sum((ir_flh(i,r)), sum(tv(t,v), flhrdbudget(i,r,v))) ;
$if      set fixbudget        sum((ir_flh(i,r)), IFLH(i,r,t)) =e= sum((ir_flh(i,r)), sum(tv(t,v), flhrdbudget(i,r,v))) ;
eq_flh_rdbudget_r(r)..
$if not  set fixbudget        sum((i_flh(i),toptimize(t)), nyrs(t) * IFLH(i,r,t) * nyrs(t)) =l= sum((i_flh(i),toptimize(t)), nyrs(t) * sum(tv(t,v), flhrdbudget(i,r,v))) ;
$if      set fixbudget        sum((i_flh(i),toptimize(t)), nyrs(t) * IFLH(i,r,t) * nyrs(t)) =e= sum((i_flh(i),toptimize(t)), nyrs(t) * sum(tv(t,v), flhrdbudget(i,r,v))) ;
eq_flh_rdbudget_i(i_flh(i))..
$if not  set fixbudget        sum((r,toptimize(t)), nyrs(t) * IFLH(i,r,t)) =l= sum((r,toptimize(t)), nyrs(t) * sum(tv(t,v), flhrdbudget(i,r,v))) ;
$if      set fixbudget        sum((r,toptimize(t)), nyrs(t) * IFLH(i,r,t)) =e= sum((r,toptimize(t)), nyrs(t) * sum(tv(t,v), flhrdbudget(i,r,v))) ;
eq_flh_rdbudget..
$if not  set fixbudget        sum((ir_flh(i,r),toptimize(t)), nyrs(t) * IFLH(i,r,t)) =l= sum((ir_flh(i,r),toptimize(t)), nyrs(t) * sum(tv(t,v), flhrdbudget(i,r,v))) ;
$if      set fixbudget        sum((ir_flh(i,r),toptimize(t)), nyrs(t) * IFLH(i,r,t)) =e= sum((ir_flh(i,r),toptimize(t)), nyrs(t) * sum(tv(t,v), flhrdbudget(i,r,v))) ;
eq_flheur_rdbudget_it(i_flh(i),toptimize(t))..
$if not  set fixbudget        IFLHEUR(i,t) =l= sum(tv(t,v), flheurrdbudget(i,v)) ;
$if      set fixbudget        IFLHEUR(i,t) =e= sum(tv(t,v), flheurrdbudget(i,v)) ;
eq_flheur_rdbudget_i(i_flh(i))..
$if not  set fixbudget        sum(toptimize(t), nyrs(t) * IFLHEUR(i,t)) =l= sum(toptimize(t), nyrs(t) * sum(tv(t,v), flheurrdbudget(i,v))) ;
$if      set fixbudget        sum(toptimize(t), nyrs(t) * IFLHEUR(i,t)) =e= sum(toptimize(t), nyrs(t) * sum(tv(t,v), flheurrdbudget(i,v))) ;
eq_flheur_rdbudget_t(toptimize(t))..
$if not  set fixbudget        sum(i_flh(i), IFLHEUR(i,t)) =l= sum(i_flh(i), sum(tv(t,v), flheurrdbudget(i,v))) ;
$if      set fixbudget        sum(i_flh(i), IFLHEUR(i,t)) =e= sum(i_flh(i), sum(tv(t,v), flheurrdbudget(i,v))) ;
eq_flheur_rdbudget..
$if not  set fixbudget        sum((i_flh(i),toptimize(t)), nyrs(t) * IFLHEUR(i,t)) =l= sum((i_flh(i),toptimize(t)), nyrs(t) * sum(tv(t,v), flheurrdbudget(i,v))) ;
$if      set fixbudget        sum((i_flh(i),toptimize(t)), nyrs(t) * IFLHEUR(i,t)) =e= sum((i_flh(i),toptimize(t)), nyrs(t) * sum(tv(t,v), flheurrdbudget(i,v))) ;

$if      set flh              IFLH.FX(i_flh(i),r,"2022") = flhrdbudget(i,r,"2022") ;
$if      set flheur           IFLHEUR.FX(i_flh(i),"2022") = flheurrdbudget(i,"2022") ;
$if      set flh              IFLH.FX(i_flh(i),r,"2050") = flhrdbudget(i,r,"2050") ;
$if      set flheur           IFLHEUR.FX(i_flh(i),"2050") = flheurrdbudget(i,"2050") ;

parameter
deprtimeeur(i,v,t)      deprtime for European learning metric
endeffecteur(i,v,t)     endeffect for European learning metric
kendeffect(i,r,t)       endeffect for lbs
kendeffecteur(i,t)      endeffecteur for lbs
rshare(inv,i)
rzeta(inv,i,v)
;

$gdxin precal\precal_%n%.gdx
$load deprtimeeur
$load endeffecteur
$load kendeffect
$load kendeffecteur
$load rshare
$load rzeta
$gdxin

$ontext
* * * Spillover
parameter
lag time period lag for LBD spillover
klag time period lag for LBS spillover
spill(r,r) LBD spillover between region pair (1 = perfect spillover -> European LBD)
kspill(r,r) LBS spillover between region pair (1 = perfect spillover --> European LBS)
spilllag(r,r,lag) LBD spillover between region pair (1 = perfect spillover -> European LBD) in dependency of lag
kspilllag(r,r,klag) LBS spillover between region pair (1 = perfect spillover -> European LBS) in dependency of lag
;
$offtext
;
$offtext