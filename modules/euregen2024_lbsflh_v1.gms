* Cause dollar statements to appear in lst file
$ondollar
* Set EOL comment indicator to the default of !!
$oneolcom

* * * Learning sets
set
ki_del(i)         Technologies to exclude from the model
ki_lea(i)         Technologies to learn
kr_lea(r)         Regions to learn
kir_lea(i,r)      Technology region pair under learning
kls               Number of line segments
;

$gdxin precal\precal_%n%.gdx
$load ki_del, ki_lea, kr_lea, kls, kir_lea
$gdxin

parameter
kcapcost(i,v,r)
kls_weight(kls)          Weight to determine breakpoints
delta_k_pa               Annual depreciation factor of the experience stock
delta_k                  Periodical depreciation factor of the experience stock
;

$gdxin precal\precal_%n%.gdx
$load kls_weight,delta_k_pa, delta_k, kcapcost
$gdxin

* * * European Learning-by-lbs
parameter
b_keur(i)                Learning rate from capacity (Q) expansion by technology and region
keurFIRST(i)             First capacity stock unit (GW)
keurSTART(i)             Initial capacity stock (GW)
keurLAST(i)              Maximum capacity stock (GW)
keurlsLO(i,kls)          Lower kink points of capacity stock (GW)
keurlsUP(i,kls)          Upper kink points of capacity stock (GW)
kcapcosteur0(i)          Cost of the first unit installed (EUR per kW)
kcapcosteurFIRST(i)      Cost of the first unit installed (EUR per kW)
kcapcosteurSTART(i)      Cost of the first unit to install in 2020
kcapcosteurLAST(i)       Cost of the last possible unit installed (EUR per kW)
kcapcosteurLO(i,kls)     Cost of the units at the breakpoints
kcapcosteurUP(i,kls)     Cost of the units at the breakpoints
kcapcosteurAV(i,kls)     Cost of the units at the breakpoints
kacc_capexeurFIRST(i)    First unit capacity stock accumulated cost (million)
kacc_capexeurSTART(i)    Initial capacity stock accumulated cost (million)
kacc_capexeurLAST(i)     Maximum capacity stock accumulated cost (million)
kacc_capexeurLO(i,kls)   Lower kink points of capacity stock accumulated cost (million)
kacc_capexeurUP(i,kls)   Upper kink points of capacity stock accumulated cost (million)
kslopeeur_lin(i)         Constant slope of linear approximated function (EUR per kW)
kslopeeur_mip(i,kls)     Slope of linear approximated function (EUR per kW)
ktest_slopeeur(i,kls,*)  Difference to average (EUR per kW)
ktest_slopeeur2(i,kls,*) Average cost per line segment (EUR per kW)
rd_budgeteur(i,t)        RD budget (million EUR)
kcapcosteur(i,v)
;

$gdxin precal\precal_%n%.gdx
$load kcapcosteur, b_keur,keurFIRST,keurSTART,keurLAST,keurlsLO,keurlsUP,kcapcosteur0,kcapcosteurSTART,kcapcosteurLAST,kcapcosteurLO,kcapcosteurUP,kcapcosteurAV,kacc_capexeurFIRST,kacc_capexeurSTART,kacc_capexeurLAST,kacc_capexeurLO,kacc_capexeurUP,kslopeeur_lin,kslopeeur_mip,ktest_slopeeur,ktest_slopeeur2,rd_budgeteur
$gdxin

positive variable
KTRYEUR(i,t)                Stock of accumulated R&D spendings (million)
KEURLS(i,t,kls)             Stock of accumulated R&D spendings on line segment kls  (million)
IKEUR(i,t)                  R&D investments (million)
KCAPCOSTEUR_MIP(i,t)        Per unit capacity cost of learning technologies (EUR per kW)
KCAPEXEUR_MIP(i,t)          Capacity cost of learning technologies (million)
IXEURLS(i,t,kls)            Variable that replaces the nonlinear product IX*RHO as part of a linearisation strategy
;

$if      set kfixrdinvest IKEUR.FX(i,t) = rd_budgeteur(i,t) ;

binary variable
KRHOEUR(i,t,kls)                   Binary variable that reflects piecewise linear segment of learning curve (for European metric)
;

equation
acc_keur_continuous2020(i,t)     Accumulation of 2015 R&D (lbs)
acc_keur_continuous(i,t)         Accumulation of R&D (lbs)
rhoeur_mixedip_k(i,t)            Equation that enforces one rho = 1 in Europe
capexeur_mixedip_k(i,t)          Equation that describes evolution of CAPEX in Europe
lbseur_helper1(i,t,kls)    Equation that helps to linearise the nonlinear product IX*RHO
lbseur_helper2(i,t,kls)    Equation that helps to linearise the nonlinear product IX*RHO
lbseur_helper3(i,t,kls)    Equation that helps to linearise the nonlinear product IX*RHO
lbseur_helper4(i,t,kls)    Equation that helps to linearise the nonlinear product IX*RHO
capcosteur_mixedip_k(i,t)        Equation that describes the evolution of capacity cost
keurlsLO_mixedip(i,t,kls)        Equation that enforces the lower bound of linear segment in Europe
keurlsUP_mixedip(i,t,kls)        Equation that enforces the upper bound of linear segment in Europe
acc_keurls_mixedip(i,t)          Equation that describes evolution of KQLS in Europe
;

* * * European Learning-by-Searching
* Accumulation of knowledge stock
acc_keur_continuous2020(i,t)$(t.val le 2022 and toptimize(t) and ki_lea(i))..
                 KTRYEUR(i,t)       =e=    sum(r, k_exo(i,r,"2022")) ;
acc_keur_continuous(i,t)$(t.val ge 2023 and toptimize(t) and ki_lea(i) and not sameas(t,"2050"))..
                 KTRYEUR(i,t)       =e=    KTRYEUR(i,t-1) * round(delta_k_pa**nyrs(t),4) + IKEUR(i,t) * nyrs(t) ;
* * * Assume nonlinear unit cost (leads to NLP)
* Comment/ToDo JA: Add NLP implementation for learning by lbs

* * * Assume MIP approximation of NLP problem
* Capacity cost follow from the investment on a line segment and the slope of this line segment
capexeur_mixedip_k(i,t)$(t.val ge 2023 and toptimize(t) and ki_lea(i))..
                 KCAPEXEUR_MIP(i,t)        =e=  sum(kls, IXEURLS(i,t,kls) * kslopeeur_mip(i,kls)) ;
* Additional constraints in order to linearise IX*RHO, which is replaced by a new helper variable IXLS above
* RHO of t-1 to realise the time lag of one period until the cost degressions materialise
lbseur_helper1(i,t,kls)$(t.val ge 2023 and toptimize(t) and ki_lea(i))..
                 IXEURLS(i,t,kls)          =l=     2000 * KRHOEUR(i,t-1,kls) ;
lbseur_helper2(i,t,kls)$(t.val ge 2023 and toptimize(t) and ki_lea(i))..
                 IXEURLS(i,t,kls)          =l=     sum(r, IX(i,r,t)) ;
lbseur_helper3(i,t,kls)$(t.val ge 2023 and toptimize(t) and ki_lea(i))..
                 IXEURLS(i,t,kls)          =g=     sum(r, IX(i,r,t))  - 2000 * (1 - KRHOEUR(i,t-1,kls)) ;
lbseur_helper4(i,t,kls)$(t.val ge 2023 and toptimize(t) and ki_lea(i))..
                 IXEURLS(i,t,kls)          =g=     0 ;
* Capacity cost follow from the linear approximation of the learning curve (Learning by lbs) (just placeholder equation, maybe delete later)
capcosteur_mixedip_k(i,t)$(ki_lea(i) and t.val ge 2023 and toptimize(t))..
                 KCAPCOSTEUR_MIP(i,t)             =e= sum(kls, KRHOEUR(i,t-1,kls) * kslopeeur_mip(i,kls) ) * 1e-3 ;
* This equation enforces that always just one line segment is active
rhoeur_mixedip_k(i,t)$(ki_lea(i) and not sameas(t,"2050") and toptimize(t))..
                 sum(kls, KRHOEUR(i,t,kls))       =e= 1 ;
* Capacity stock per line segment is greater than the lower bound of that line segment (Learning by lbs)
keurlsLO_mixedip(i,t,kls)$(ki_lea(i) and not sameas(t,"2050") and toptimize(t))..
                 KEURLS(i,t,kls)                  =g= keurlsLO(i,kls) * KRHOEUR(i,t,kls) ;
* Capacity stock per line segment is lower than the upper bound of that line segment (Learning by lbs)
keurlsUP_mixedip(i,t,kls)$(ki_lea(i) and not sameas(t,"2050") and toptimize(t))..
                 KEURLS(i,t,kls)                  =l= keurlsUP(i,kls) * KRHOEUR(i,t,kls) ;
* Capacity stock per "active" (rho = 1) line segment is equal to capacity stock (Learning by lbs)
acc_keurls_mixedip(i,t)$(ki_lea(i) and not sameas(t,"2050") and toptimize(t))..
                 KTRYEUR(i,t)                     =e= sum(kls, KEURLS(i,t,kls)) ;
                 
* * * R&D budget allocation
equation
eq_lbs_rdbudget_irt(i,r,t) 
eq_lbs_rdbudget_rt(r,t)
eq_lbs_rdbudget_it(i,t)
eq_lbs_rdbudget_ir(i,r)
eq_lbs_rdbudget_t(t)
eq_lbs_rdbudget_r(r)
eq_lbs_rdbudget_i(i)
eq_lbs_rdbudget
eq_lbseur_rdbudget_it(i,t)
eq_lbseur_rdbudget_i(i)
eq_lbseur_rdbudget_t(t)
eq_lbseur_rdbudget
;

parameter
lbsrdbudget(i,r,v)
lbseurrdbudget(i,v)
;

$gdxin precal\precal_%n%.gdx
$load lbsrdbudget, lbseurrdbudget
$gdxin

eq_lbs_rdbudget_irt(kir_lea(i,r),toptimize(t))..
$if not  set fixbudget        IK(i,r,t) =l= sum(tv(t,v), lbsrdbudget(i,r,v)) ;
$if      set fixbudget        IK(i,r,t) =e= sum(tv(t,v), lbsrdbudget(i,r,v)) ;
eq_lbs_rdbudget_rt(r,t)$(toptimize(t))..
$if not  set fixbudget        sum(ki_lea(i), IK(i,r,t)) =l= sum(i, sum(tv(t,v), lbsrdbudget(i,r,v))) ;
$if      set fixbudget        sum(ki_lea(i), IK(i,r,t)) =e= sum(i, sum(tv(t,v), lbsrdbudget(i,r,v))) ;
eq_lbs_rdbudget_it(ki_lea(i),toptimize(t))..
$if not  set fixbudget        sum(r, IK(i,r,t)) =l= sum(r, sum(tv(t,v), lbsrdbudget(i,r,v))) ;
$if      set fixbudget        sum(r, IK(i,r,t)) =e= sum(r, sum(tv(t,v), lbsrdbudget(i,r,v))) ;
eq_lbs_rdbudget_ir(kir_lea(i,r))..
$if not  set fixbudget        sum(toptimize(t), nyrs(t) * IK(i,r,t)) =l= sum(toptimize(t), nyrs(t) * sum(tv(t,v), lbsrdbudget(i,r,v))) ;
$if      set fixbudget        sum(toptimize(t), nyrs(t) * IK(i,r,t)) =e= sum(toptimize(t), nyrs(t) * sum(tv(t,v), lbsrdbudget(i,r,v))) ;
eq_lbs_rdbudget_t(toptimize(t))..
$if not  set fixbudget        sum((kir_lea(i,r)), IK(i,r,t)) =l= sum((kir_lea(i,r)), sum(tv(t,v), lbsrdbudget(i,r,v))) ;
$if      set fixbudget        sum((kir_lea(i,r)), IK(i,r,t)) =e= sum((kir_lea(i,r)), sum(tv(t,v), lbsrdbudget(i,r,v))) ;
eq_lbs_rdbudget_r(r)..
$if not  set fixbudget        sum((ki_lea(i),toptimize(t)), nyrs(t) * IK(i,r,t) * nyrs(t)) =l= sum((ki_lea(i),toptimize(t)), nyrs(t) * sum(tv(t,v), lbsrdbudget(i,r,v))) ;
$if      set fixbudget        sum((ki_lea(i),toptimize(t)), nyrs(t) * IK(i,r,t) * nyrs(t)) =e= sum((ki_lea(i),toptimize(t)), nyrs(t) * sum(tv(t,v), lbsrdbudget(i,r,v))) ;
eq_lbs_rdbudget_i(ki_lea(i))..
$if not  set fixbudget        sum((r,toptimize(t)), nyrs(t) * IK(i,r,t)) =l= sum((r,toptimize(t)), nyrs(t) * sum(tv(t,v), lbsrdbudget(i,r,v))) ;
$if      set fixbudget        sum((r,toptimize(t)), nyrs(t) * IK(i,r,t)) =e= sum((r,toptimize(t)), nyrs(t) * sum(tv(t,v), lbsrdbudget(i,r,v))) ;
eq_lbs_rdbudget..
$if not  set fixbudget        sum((kir_lea(i,r),toptimize(t)), nyrs(t) * IK(i,r,t)) =l= sum((kir_lea(i,r),toptimize(t)), nyrs(t) * sum(tv(t,v), lbsrdbudget(i,r,v))) ;
$if      set fixbudget        sum((kir_lea(i,r),toptimize(t)), nyrs(t) * IK(i,r,t)) =e= sum((kir_lea(i,r),toptimize(t)), nyrs(t) * sum(tv(t,v), lbsrdbudget(i,r,v))) ;
eq_lbseur_rdbudget_it(ki_lea(i),toptimize(t))..
$if not  set fixbudget        IKEUR(i,t) =l= sum(tv(t,v), lbseurrdbudget(i,v)) ;
$if      set fixbudget        IKEUR(i,t) =e= sum(tv(t,v), lbseurrdbudget(i,v)) ;
eq_lbseur_rdbudget_i(ki_lea(i))..
$if not  set fixbudget        sum(toptimize(t), nyrs(t) * IKEUR(i,t)) =l= sum(toptimize(t), nyrs(t) * sum(tv(t,v), lbseurrdbudget(i,v))) ;
$if      set fixbudget        sum(toptimize(t), nyrs(t) * IKEUR(i,t)) =e= sum(toptimize(t), nyrs(t) * sum(tv(t,v), lbseurrdbudget(i,v))) ;
eq_lbseur_rdbudget_t(toptimize(t))..
$if not  set fixbudget        sum(ki_lea(i), IKEUR(i,t)) =l= sum(ki_lea(i), sum(tv(t,v), lbseurrdbudget(i,v))) ;
$if      set fixbudget        sum(ki_lea(i), IKEUR(i,t)) =e= sum(ki_lea(i), sum(tv(t,v), lbseurrdbudget(i,v))) ;
eq_lbseur_rdbudget..
$if not  set fixbudget        sum((ki_lea(i),toptimize(t)), nyrs(t) * IKEUR(i,t)) =l= sum((ki_lea(i),toptimize(t)), nyrs(t) * sum(tv(t,v), lbseurrdbudget(i,v))) ;
$if      set fixbudget        sum((ki_lea(i),toptimize(t)), nyrs(t) * IKEUR(i,t)) =e= sum((ki_lea(i),toptimize(t)), nyrs(t) * sum(tv(t,v), lbseurrdbudget(i,v))) ;

$if      set lbs              IK.FX(ki_lea(i),r,"2022") = lbsrdbudget(i,r,"2022") ;
$if      set lbseur           IKEUR.FX(ki_lea(i),"2022") = lbseurrdbudget(i,"2022") ;
$if      set lbs              IK.FX(ki_lea(i),r,"2050") = lbsrdbudget(i,r,"2050") ;
$if      set lbseur           IKEUR.FX(ki_lea(i),"2050") = lbseurrdbudget(i,"2050") ;

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
$offtext of lag
;
$offtext