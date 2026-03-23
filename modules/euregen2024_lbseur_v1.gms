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
eq_lbseur_rdbudget_it(i,t)
eq_lbseur_rdbudget_i(i)
eq_lbseur_rdbudget_t(t)
eq_lbseur_rdbudget
;

parameter
lbseurrdbudget(i,v)
;

$gdxin precal\precal_%n%.gdx
$load lbseurrdbudget
$gdxin

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

IKEUR.FX(ki_lea(i),"2022") = lbseurrdbudget(i,"2022") ;
IKEUR.FX(ki_lea(i),"2050") = lbseurrdbudget(i,"2050") ;

parameter
kendeffect(i,r,t)       endeffect for lbs
kendeffecteur(i,t)      endeffecteur for lbs
rshare(inv,i)
rzeta(inv,i,v)
;

$gdxin precal\precal_%n%.gdx
$load kendeffect
$load kendeffecteur
$load rshare
$load rzeta
$gdxin