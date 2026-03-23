* Cause dollar statements to appear in lst file
$ondollar
* Set EOL comment indicator to the default of !!
$oneolcom

* * * LBD: Learning-by-doing
set
i_del(i)         Technologies to exclude from the model
i_lea(i)         Technologies to learn
r_lea(r)         Regions to learn
ir_lea(i,r)      Technology region pair under learning
ls               Number of line segments
dpr              Depreciation method (of experience stock)
;

$gdxin precal\precal_%n%.gdx
$load i_del, i_lea, r_lea, ir_lea, ls, dpr
$gdxin

parameter
ls_weight(ls)            Weight to determine breakpoints
delta_q_pa               Annual depreciation factor of the experience stock
delta_q                  Periodical depreciation factor of the experience stock
;

$gdxin precal\precal_%n%.gdx
$load ls_weight,delta_q_pa, delta_q
$gdxin

* * * European Learning-by-doing
parameter
b_qeur(i)                 Learning rate from capacity (Q) expansion by technology and region
qeurFIRST(i)              First capacity stock unit (GW)
qeurSTART(i)              Initial capacity stock (GW)
qeurLAST(i)               Maximum capacity stock (GW)
qeurlsLO(i,ls)            Lower kink points of capacity stock (GW)
qeurlsUP(i,ls)            Upper kink points of capacity stock (GW)
capcosteur0(i)            Cost of the first unit installed (EUR per kW)
acc_capexeurFIRST(i)      First unit capacity stock accumulated cost (million)
acc_capexeurSTART(i)      Initial capacity stock accumulated cost (million)
acc_capexeurLAST(i)       Maximum capacity stock accumulated cost (million)
acc_capexeurLO(i,ls)      Lower kink points of capacity stock accumulated cost (million)
acc_capexeurUP(i,ls)      Upper kink points of capacity stock accumulated cost (million)
slopeeur_mip(i,ls)        Slope of linear approximated function (EUR per kW)
;


$gdxin precal\precal_%n%.gdx
$load b_qeur 
$load qeurFIRST
$load qeurSTART 
$load qeurLAST 
$load qeurlsLO 
$load qeurlsUP 
$load capcosteur0 
$load acc_capexeurFIRST 
$load acc_capexeurSTART 
$load acc_capexeurLAST 
$load acc_capexeurLO 
$load acc_capexeurUP 
$load slopeeur_mip 
$gdxin
 
positive variable
CAPEXEUR_CON(i,v)           Capacity cost of learning technologies (million)
CAPEXEUR_NLP(i,v)           Capacity cost of learning technologies (million)
CAPEXEUR_NLP_LEG(i,v)       Capacity cost of learning technologies (million)
CAPEXEUR_MIP(i,v)           Capacity cost of learning technologies (trillion)
QTRYEUR(i,t)                Stock of accumulated capacity (TW)
QTRYEUR_LEG(i,t)            Stock of accumulated legacy capacity (from previous periods) (TW)
QEURLS(i,t,ls)              Stock of capacity on line segment ls (TW)
QEURLS_LEG(i,t,ls)          Stock of legacy capacity (from previous periods) on line segment ls (TW)
ACC_CAPEXEUR_NLP(i,t)       Accumulated capacity cost of learning technologies (million)
ACC_CAPEXEUR_NLP_LEG(i,t)   Accumulated legacy capacity cost of learning technologies (million)
ACC_CAPEXEUR_MIP(i,t)       Accumulated capacity cost of learning technologies (trillion)
ACC_CAPEXEUR_MIP_LEG(i,t)   Accumulated legacy capacity cost of learning technologies (trillion)
;

binary variable
RHOEUR(i,t,ls)                   Binary variable that reflects piecewise linear segment of learning curve (for European metric)
RHOEUR_LEG(i,t,ls)               Binary variable that reflects piecewise linear segment of learning curve (for legacy capacity)
;

Equation
* European Learning-by-doing
acc_qeur_recall2020(i,t)         Accumulation of capacity in 2015 in Europe (doing)
acc_qeur_leg_recall2020(i,t)     Accumulation of legacy capacity in 2015 in Europe (doing)
acc_qeur_recall(i,t)             Accumulation of capacity in Europe (doing)
acc_qeur_leg_recall(i,t)         Accumulation of legacy capacity in Europe (doing)
acc_qeur_continuous2020(i,t)     Accumulation of capacity in 2015 in Europe (doing)
acc_qeur_leg_continuous2020(i,t) Accumulation of legacy capacity in 2015 in Europe (doing)
acc_qeur_continuous(i,t)         Accumulation of capacity in Europe (doing)
acc_qeur_leg_continuous(i,t)     Accumulation of legacy capacity in Europe (doing)
acc_qeur_discrete2020(i,t)       Accumulation of capacity in 2015 in Europe (doing)
acc_qeur_leg_discrete2020(i,t)   Accumulation of legacy capacity in 2015 in Europe (doing)
acc_qeur_discrete(i,t)           Accumulation of capacity in Europe (doing)
acc_qeur_leg_discrete(i,t)       Accumulation of legacy capacity in Europe (doing)
rhoeur_mixedip(i,t)              Equation that enforces one rho = 1 in Europe
rhoeur_leg_mixedip(i,t)          Equation that enforces one rho = 1 in Europe (for legacy capacity)
capexeur_constant(i,v)           Equation that describes evolution of CAPEX in Europe
capexeur_nonlinear(i,v)          Equation that describes evolution of CAPEX in Europe
capexeur_mixedip(i,v)            Equation that describes evolution of CAPEX in Europe (trillion EUR)
acc_capexeur_nonlinear(i,t)      Equation that describes evolution of accumulated CAPEX in Europe
acc_capexeur_leg_nonlinear(i,t)  Equation that describes evolution of accumulated legacy CAPEX in Europe
acc_capexeur_mixedip(i,t)        Equation that describes evolution of accumulated CAPEX in Europe (trillion EUR)
acc_capexeur_leg_mixedip(i,t)    Equation that describes evolution of accumulated legacy CAPEX in Europe (trillion EUR)
qeurlsLO_mixedip(i,t,ls)         Equation that enforces the lower bound of linear segment in Europe
qeurlsLO_leg_mixedip(i,t,ls)     Equation that enforces the lower bound of linear segment in Europe (for legacy capacity)
qeurlsUP_mixedip(i,t,ls)         Equation that enforces the upper bound of linear segment in Europe
qeurlsUP_leg_mixedip(i,t,ls)     Equation that enforces the upper bound of linear segment in Europe (for legacy capacity)
acc_qeurls_mixedip(i,t)          Equation that describes evolution of QLS in Europe
acc_qeurls_leg_mixedip(i,t)      Equation that describes evolution of legacy QLS in Europe
* Try a monotonicity constraint to speed up problem (for perfect recall only)
acc_qeur_mono(i,t)               Does not allow to reduce learning capacity stock (for perfect recall)
acc_qeur_leg_mono(i,t)           Does not allow to reduce learning capacity stock (for perfect recall)
acc_qeur_max(i,t)                Limits learning capacity stock to max value
acc_qeur_leg_max(i,t)            Limits learning capacity stock to max value
rhoeur_mixedip_mono(i,t)         Does not allow to move backwards for rho (for perfect recall)
rhoeur_leg_mixedip_mono(i,t)     Does not allow to move backwards for rho (for perfect recall)
qeurls_mixedip_mono(i,t)         Does not allow to move backwards for rho (for perfect recall)
qeurls_leg_mixedip_mono(i,t)     Does not allow to move backwards for rho (for perfect recall)
;
* * Accumulated capacity stock (legacy capacity concept is used to account for the fact that some capacity depreciates between period t and t-1 and to thus become cost right)
* with perfect recall
acc_qeur_recall2020(i,t)$(t.val le 2022 and toptimize(t) and i_lea(i))..
                 QTRYEUR(i,t)        =e= qeurSTART(i) * 1e-3 ;
acc_qeur_leg_recall2020(i,t)$(t.val le 2022 and toptimize(t) and i_lea(i))..
                 QTRYEUR_LEG(i,t)    =e= QTRYEUR(i,t) ;
acc_qeur_recall(i,t)$(t.val ge 2023 and toptimize(t) and i_lea(i))..
                 QTRYEUR(i,t)        =e= qeurSTART(i) * 1e-3 + sum(tt$(tt.val le t.val), sum(r, IX(i,r,tt))) * 1e-3  ;
acc_qeur_leg_recall(i,t)$(t.val ge 2023 and toptimize(t) and i_lea(i))..
                 QTRYEUR_LEG(i,t)    =e= QTRYEUR(i,t-1) ;   

* with continuous depreciation
acc_qeur_continuous2020(i,t)$(t.val le 2022 and toptimize(t) and i_lea(i))..
                 QTRYEUR(i,t)        =e= sum(oldv(v), round(delta_q_pa**(t.val - v.val),4) * sum(r, capt(i,v,r,"2022"))) * 1e-3 ;
acc_qeur_leg_continuous2020(i,t)$(t.val le 2022 and toptimize(t) and i_lea(i))..
                 QTRYEUR_LEG(i,t)    =e= QTRYEUR(i,t);
acc_qeur_continuous(i,t)$(t.val ge 2023 and toptimize(t) and i_lea(i))..
                 QTRYEUR(i,t)        =e= QTRYEUR(i,t-1) * round(delta_q_pa**nyrs(t),4) + sum(r, IX(i,r,t)) * 1e-3 ;
acc_qeur_leg_continuous(i,t)$(t.val ge 2023 and toptimize(t) and i_lea(i))..
                 QTRYEUR_LEG(i,t)    =e= QTRYEUR(i,t-1) * round(delta_q_pa**nyrs(t),4) ;

* with discrete depreciation
acc_qeur_discrete2020(i,t)$(t.val le 2022 and toptimize(t) and i_lea(i))..
                 QTRYEUR(i,t)        =e= sum(oldv, sum(r, deprtime(i,oldv,r,t) * capt(i,oldv,r,"2022"))) * 1e-3 ;
acc_qeur_leg_discrete2020(i,t)$(t.val le 2022 and toptimize(t) and i_lea(i))..
                 QTRYEUR_LEG(i,t)    =e= QTRYEUR(i,t);
acc_qeur_discrete(i,t)$(t.val ge 2023 and toptimize(t) and i_lea(i))..
                 QTRYEUR(i,t)        =e= sum(oldv, sum(r, deprtime(i,oldv,r,t) * capt(i,oldv,r,"2022"))) * 1e-3 + sum(newv(v)$(v.val < t.val), sum(r, deprtime(i,v,r,t) * sum(tv(tt,v), IX(i,r,tt)))) * 1e-3 + sum(r, IX(i,r,t)) * 1e-3 ;
acc_qeur_leg_discrete(i,t)$(t.val ge 2023 and toptimize(t) and i_lea(i))..
                 QTRYEUR_LEG(i,t)    =e= sum(oldv, sum(r, deprtime(i,oldv,r,t) * capt(i,oldv,r,"2022"))) * 1e-3 + sum(newv(v)$(v.val < t.val), sum(r, deprtime(i,v,r,t) * sum(tv(tt,v), IX(i,r,tt)))) * 1e-3 ;

* Try a monotonicity constraint to speed up problem (for perfect recall only)                 
acc_qeur_mono(i,t)$(t.val ge 2023 and toptimize(t) and i_lea(i))..
                 QTRYEUR(i,t)       =g= QTRYEUR(i,t-1) ;
acc_qeur_leg_mono(i,t)$(t.val ge 2023 and toptimize(t) and i_lea(i))..
                 QTRYEUR_LEG(i,t)   =g= QTRYEUR_LEG(i,t-1) ;
* Maximum values
acc_qeur_max(i,t)$(t.val ge 2023 and toptimize(t) and i_lea(i))..
                 QTRYEUR(i,t)       =l= qeurLAST(i) * 1e-3 ;
acc_qeur_leg_max(i,t)$(t.val ge 2023 and toptimize(t) and i_lea(i))..
                 QTRYEUR_LEG(i,t)   =l= qeurLAST(i) * 1e-3 ;   

* * Unit cost
* Constant
capexeur_constant(i,newv(v))$(i_lea(i))..
                 CAPEXEUR_CON(i,v)        =e= sum(r, capcost(i,v,r) * sum(tv(t,v), IX(i,r,t))) ;
* Nonlinear
capexeur_nonlinear(i,newv(v))$(i_lea(i))..
$if      set leg CAPEXEUR_NLP(i,v)        =e= sum(tv(t,v), ACC_capexeur_NLP(i,t) - ACC_capexeur_NLP_LEG(i,t)) ;
$if not  set leg CAPEXEUR_NLP(i,v)        =e= sum(tv(t,v), ACC_capexeur_NLP(i,t) - ACC_capexeur_NLP(i,t-1)) ;
* MIP
capexeur_mixedip(i,newv(v))$(i_lea(i))..
$if      set leg CAPEXEUR_MIP(i,v)        =e= sum(tv(t,v), ACC_capexeur_MIP(i,t) - ACC_capexeur_MIP_LEG(i,t)) ;
$if not  set leg CAPEXEUR_MIP(i,v)        =e= sum(tv(t,v), ACC_capexeur_MIP(i,t) - ACC_capexeur_MIP(i,t-1)) ;

* * Accumulated cost (Constant unit cost does not demand for accumulation equations)
* Nonlinear
acc_capexeur_nonlinear(i,t)$(i_lea(i) and toptimize(t))..
                 ACC_capexeur_NLP(i,t)     =e= capcosteur0(i) / (1 + b_qeur(i)) * (QTRYEUR(i,t)    *1e+9)**(1 + b_qeur(i)) * 1e-6 ;
acc_capexeur_leg_nonlinear(i,t)$(i_lea(i) and toptimize(t))..
                 ACC_capexeur_NLP_LEG(i,t) =e= capcosteur0(i) / (1 + b_qeur(i)) * (QTRYEUR_LEG(i,t)*1e+9)**(1 + b_qeur(i)) * 1e-6 ;
* MIP
acc_capexeur_mixedip(i,t)$(i_lea(i) and toptimize(t))..
                 ACC_capexeur_MIP(i,t)     =e= sum(ls, RHOEUR(i,t,ls)     * acc_capexeurLO(i,ls) + slopeeur_mip(i,ls) * (QEURLS(i,t,ls)  * 1e+3     - qeurlsLO(i,ls) * RHOEUR(i,t,ls))) * 1e-6 ;
acc_capexeur_leg_mixedip(i,t)$(i_lea(i) and toptimize(t))..
                 ACC_capexeur_MIP_LEG(i,t) =e= sum(ls, RHOEUR_LEG(i,t,ls) * acc_capexeurLO(i,ls) + slopeeur_mip(i,ls) * (QEURLS_LEG(i,t,ls) * 1e+3  - qeurlsLO(i,ls) * RHOEUR_LEG(i,t,ls))) * 1e-6 ;

* * Mixed integer variable rho and related equations
* This equation enforces that always just one line segment is active
rhoeur_mixedip(i,t)$(i_lea(i) and toptimize(t))..
                 sum(ls, RHOEUR(i,t,ls))     =e= 1 ;
rhoeur_leg_mixedip(i,t)$(i_lea(i) and toptimize(t))..
                 sum(ls, RHOEUR_LEG(i,t,ls)) =e= 1 ;
                 
* Capacity stock per line segment is greater than the lower bound of that line segment (Learning by doing)
qeurlsLO_mixedip(i,t,ls)$(i_lea(i) and toptimize(t))..
                 QEURLS(i,t,ls)           =g= qeurlsLO(i,ls) * RHOEUR(i,t,ls) * 1e-3 ;
qeurlsLO_leg_mixedip(i,t,ls)$(i_lea(i) and toptimize(t))..
                 QEURLS_LEG(i,t,ls)       =g= qeurlsLO(i,ls) * RHOEUR_LEG(i,t,ls) * 1e-3 ;

* Capacity stock per line segment is lower than the upper bound of that line segment (Learning by doing)
qeurlsUP_mixedip(i,t,ls)$(i_lea(i) and toptimize(t))..
                 QEURLS(i,t,ls)           =l= qeurlsUP(i,ls) * RHOEUR(i,t,ls) * 1e-3 ;
qeurlsUP_leg_mixedip(i,t,ls)$(i_lea(i) and toptimize(t))..
                 QEURLS_LEG(i,t,ls)       =l= qeurlsUP(i,ls) * RHOEUR_LEG(i,t,ls) * 1e-3  ;

* Capacity stock per "active" (rho = 1) line segment is equal to capacity stock  (Learning by doing)
acc_qeurls_mixedip(i,t)$(i_lea(i) and toptimize(t))..
                 QTRYEUR(i,t)             =e= sum(ls, QEURLS(i,t,ls)) ;
acc_qeurls_leg_mixedip(i,t)$(i_lea(i) and toptimize(t))..
                 QTRYEUR_LEG(i,t)         =e= sum(ls, QEURLS_LEG(i,t,ls)) ;
                 
* Try a monotonicity constraint to speed up problem (for perfect recall only)
rhoeur_mixedip_mono(i,t)$(t.val ge 2023 and toptimize(t) and i_lea(i))..              
                 sum(ls, RHOEUR(i,t,ls)) =g= sum(ls, RHOEUR(i,t-1,ls)) ;
rhoeur_leg_mixedip_mono(i,t)$(t.val ge 2023 and toptimize(t) and i_lea(i))..              
                 sum(ls, RHOEUR_LEG(i,t,ls)) =g= sum(ls, RHOEUR_LEG(i,t-1,ls)) ; 
                 
* Try a monotonicity constraint to speed up problem (for perfect recall only)
qeurls_mixedip_mono(i,t)$(t.val ge 2023 and toptimize(t) and i_lea(i))..              
                 sum(ls,QEURLS(i,t,ls)) =g= sum(ls, QEURLS(i,t-1,ls)) ;
qeurls_leg_mixedip_mono(i,t)$(t.val ge 2023 and toptimize(t) and i_lea(i))..              
                 sum(ls, QEURLS_LEG(i,t,ls)) =g= sum(ls, QEURLS_LEG(i,t-1,ls)) ;  
                 * Monotonicity (for perfect recall)

parameter
deprtimeeur(i,v,t)      deprtime for European learning metric
endeffecteur(i,v,t)     endeffect for European learning metric
rshare(inv,i)
rzeta(inv,i,v)
;

$gdxin precal\precal_%n%.gdx
$load deprtimeeur
$load endeffecteur
$load rshare
$load rzeta
$gdxin
$gdxin