* Cause dollar statements to appear in lst file
$ondollar
* Set EOL comment indicator to the default of !!
$oneolcom

* * * R&D: Learning-by-lbs
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

* * * Regional Learning-by-lbs
parameter
b_k(i,r)                 Learning rate from capacity (Q) expansion by technology and region
kFIRST(i,r)              First capacity stock unit (GW)
kSTART(i,r)              Initial capacity stock (GW)
kLAST(i,r)               Maximum capacity stock (GW)
klsLO(i,r,kls)           Lower kink points of capacity stock (GW)
klsUP(i,r,kls)           Upper kink points of capacity stock (GW)
kcapcost0(i,r)           Cost of the first unit installed (EUR per kW)
kcapcostFIRST(i,r)       Cost of the first unit installed (EUR per kW)
kcapcostSTART(i,r)       Cost of the first unit to install in 2020
kcapcostLAST(i,r)        Cost of the last possible unit installed (EUR per kW)
kcapcostLO(i,r,kls)      Cost of the units at the breakpoints
kcapcostUP(i,r,kls)      Cost of the units at the breakpoints
kcapcostAV(i,r,kls)      Cost of the units at the breakpoints
kacc_capexFIRST(i,r)     First unit capacity stock accumulated cost (million)
kacc_capexSTART(i,r)     Initial capacity stock accumulated cost (million)
kacc_capexLAST(i,r)      Maximum capacity stock accumulated cost (million)
kacc_capexLO(i,r,kls)    Lower kink points of capacity stock accumulated cost (million)
kacc_capexUP(i,r,kls)    Upper kink points of capacity stock accumulated cost (million)
kslope_lin(i,r)          onstant slope of linear approximated function (EUR per kW)
kslope_mip(i,r,kls)      Slope of linear approximated function (EUR per kW)
ktest_slope(i,r,kls,*)   Difference to average (EUR per kW)
ktest_slope2(i,r,kls,*)  Average cost per line segment (EUR per kW)
rd_budget(i,r,t)         RD budget (million EUR)
kspillover(i,r,r)        Spillover from r to r
k_exo(i,r,v)
;

$gdxin precal\precal_%n%.gdx
$load b_k,kFIRST,kSTART,kLAST,klsLO,klsUP,kcapcost0,kcapcostFIRST,kcapcostSTART,kcapcostLAST,kcapcostLO,kcapcostUP,kcapcostAV,kacc_capexFIRST,kacc_capexSTART,kacc_capexLAST,kacc_capexLO,kacc_capexUP,kslope_lin,kslope_mip,ktest_slope,ktest_slope2,rd_budget
$load k_exo
$gdxin

positive variable
KTRY(i,r,t)                 Stock of accumulated R&D spendings (million)
KQLS(i,r,t,kls)             Stock of accumulated R&D spendings on line segment kls  (million)
IK(i,r,t)                   R&D investments (million)
KCAPCOST_MIP(i,r,t)         Per unit capacity cost of learning technologies (1000 EUR per kW)
KCAPEX_MIP(i,v,r)           Capacity cost of learning technologies (billion EUR)
IXLS(i,r,t,kls)             Variable that replaces the nonlinear product IX*RHO as part of a linearisation strategy
;

$if      set kfixrdinvest IK.FX(i,r,t) = rd_budget(i,r,t) ;

binary variable
KRHO(i,r,t,kls)                    Binary variable that reflects piecewise linear segment of learning curve
;

equation
* Regional Learning-by-Searching
acc_k_continuous2020(i,r,t)      Accumulation of R&D (lbs)
acc_k_continuous(i,r,t)          Accumulation of R&D (lbs)
rho_mixedip_k(i,r,t)             Equation that enforces one rho = 1
capex_mixedip_k(i,v,r)           Equation that describes evolution of CAPEX
capex_mixedip_k2020(i,v,r)
lbs_helper1(i,r,t,kls)     Equation that helps to linearise the nonlinear product IX*RHO
lbs_helper2(i,r,t,kls)     Equation that helps to linearise the nonlinear product IX*RHO
lbs_helper3(i,r,t,kls)     Equation that helps to linearise the nonlinear product IX*RHO
lbs_helper4(i,r,t,kls)     Equation that helps to linearise the nonlinear product IX*RHO
capcost_mixedip_k(i,r,t)         Equation that describes the evolution of capacity cost
klsLO_mixedip(i,r,t,kls)         Equation that enforces the lower bound of linear segment
klsUP_mixedip(i,r,t,kls)         Equation that enforces the upper bound of linear segment
acc_kls_mixedip(i,r,t)           Equation that describes evolution of KQLS
;

parameter
tspillover(i,i)
kspillover_int(i,r,r)
;

$gdxin precal\precal_%n%.gdx
$load tspillover, kspillover
$gdxin

* * * Global learning-by-doing
* We use the European metric but "add" an exogenous assumption about rest-of-the-world (ROW) capacity

* * * Regional Learning-by-Searching
* Accumulation of knowledge stock
acc_k_continuous2020(i,r,t)$(t.val le 2022 and toptimize(t) and kir_lea(i,r))..
                 KTRY(i,r,t)       =e=    k_exo(i,r,"2022")             ;
                 
acc_k_continuous(i,r,t)$(t.val ge 2023 and toptimize(t) and kir_lea(i,r) and not sameas(t,"2050"))..
                 KTRY(i,r,t)       =e=    KTRY(i,r,t-1) * round(delta_k_pa**nyrs(t),4) + IK(i,r,t) * nyrs(t)
$if      set spillover                  + sum(rr, IK(i,rr,t) * kspillover(i,r,rr))
$if      set techspillover              + sum(ii, IK(ii,r,t) * tspillover(i,ii))    
                 ;
* * * Assume nonlinear unit cost (leads to NLP)
* Comment/ToDo JA: Add NLP implementation for learning by lbs

* * * Assume MIP approximation of NLP problem
* Capacity cost follow from the investment on a line segment and the slope of this line segment
capex_mixedip_k2020(i,v,r)$(sameas(v,"2022") and kir_lea(i,r))..
                 KCAPEX_MIP(i,v,r)        =e=  sum(tv(t,v), IX(i,r,t)) * capcost(i,v,r) ;
capex_mixedip_k(i,v,r)$(v.val ge 2023 and kir_lea(i,r))..
                 KCAPEX_MIP(i,v,r)        =e=  sum(tv(t,v), sum(kls, IXLS(i,r,t,kls) * kslope_mip(i,r,kls))) ;
* Additional constraints in order to linearise IX*RHO, which is replaced by a new helper variable IXLS above
* RHO of t-1 to realise the time lag of one period until the cost degressions materialise
lbs_helper1(i,r,t,kls)$(t.val ge 2023 and toptimize(t) and kir_lea(i,r))..
                 IXLS(i,r,t,kls)          =l=     200 * KRHO(i,r,t-1,kls) ;
lbs_helper2(i,r,t,kls)$(t.val ge 2023 and toptimize(t) and kir_lea(i,r))..
                 IXLS(i,r,t,kls)          =l=     IX(i,r,t) ;
lbs_helper3(i,r,t,kls)$(t.val ge 2023 and toptimize(t) and kir_lea(i,r))..
                 IXLS(i,r,t,kls)          =g=     IX(i,r,t) - 200 * (1 - KRHO(i,r,t-1,kls)) ;
lbs_helper4(i,r,t,kls)$(t.val ge 2023 and toptimize(t) and kir_lea(i,r))..
                 IXLS(i,r,t,kls)          =g=     0 ;
* Capacity cost follow from the linear approximation of the learning curve (Learning by lbs) (just placeholder equation, maybe delete later)
capcost_mixedip_k(i,r,t)$(kir_lea(i,r) and t.val ge 2023 and toptimize(t))..
                 KCAPCOST_MIP(i,r,t)             =e= sum(kls, KRHO(i,r,t-1,kls) * kslope_mip(i,r,kls) ) ;
* This equation enforces that always just one line segment is active
rho_mixedip_k(i,r,t)$(kir_lea(i,r) and not sameas(t,"2050") and toptimize(t))..
                 sum(kls, KRHO(i,r,t,kls))       =e= 1 ;
* Capacity stock per line segment is greater than the lower bound of that line segment (Learning by lbs)
klsLO_mixedip(i,r,t,kls)$(kir_lea(i,r) and not sameas(t,"2050") and toptimize(t))..
                 KQLS(i,r,t,kls)                 =g= klsLO(i,r,kls) * KRHO(i,r,t,kls) ;
* Capacity stock per line segment is lower than the upper bound of that line segment (Learning by lbs)
klsUP_mixedip(i,r,t,kls)$(kir_lea(i,r) and not sameas(t,"2050") and toptimize(t))..
                 KQLS(i,r,t,kls)                 =l= klsUP(i,r,kls) * KRHO(i,r,t,kls) ;
* Capacity stock per "active" (rho = 1) line segment is equal to capacity stock (Learning by lbs)
acc_kls_mixedip(i,r,t)$(kir_lea(i,r) and not sameas(t,"2050") and toptimize(t))..
                 KTRY(i,r,t)                     =e= sum(kls, KQLS(i,r,t,kls)) ;
                 
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
;

parameter
lbsrdbudget(i,r,v)
;

$gdxin precal\precal_%n%.gdx
$load lbsrdbudget
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

IK.FX(kir_lea(i,r),"2022") = lbsrdbudget(i,r,"2022") ;
IK.FX(kir_lea(i,r),"2050") = lbsrdbudget(i,r,"2050") ;,"2050") ;