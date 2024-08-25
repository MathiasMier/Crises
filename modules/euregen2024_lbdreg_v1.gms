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

* * * Regional Learning-by-doing (LBD)
parameter
b_q(i,r)                 Learning rate from capacity (Q) expansion by technology and region
qFIRST(i,r)              First capacity stock unit (GW)
qSTART(i,r)              Initial capacity stock (GW)
qLAST(i,r)               Maximum capacity stock (GW)
qlsLO(i,r,ls)            Lower kink points of capacity stock (GW)
qlsUP(i,r,ls)            Upper kink points of capacity stock (GW)
capcost0(i,r)            Cost of the first unit installed (EUR per kW)
acc_capexFIRST(i,r)      First unit capacity stock accumulated cost (million)
acc_capexSTART(i,r)      Initial capacity stock accumulated cost (million)
acc_capexLAST(i,r)       Maximum capacity stock accumulated cost (million)
acc_capexLO(i,r,ls)      Lower kink points of capacity stock accumulated cost (million)
acc_capexUP(i,r,ls)      Upper kink points of capacity stock accumulated cost (million)
slope_mip(i,r,ls)        Slope of linear approximated function (EUR per kW)
;

$gdxin precal\precal_%n%.gdx
$load b_q 
$load qFIRST
$load qSTART 
$load qLAST 
$load qlsLO 
$load qlsUP 
$load capcost0 
$load acc_capexFIRST 
$load acc_capexSTART 
$load acc_capexLAST 
$load acc_capexLO 
$load acc_capexUP 
$load slope_mip 
$gdxin

positive variable
CAPEX_CON(i,v,r)            Capacity cost of learning technologies (million)
CAPEX_NLP(i,v,r)            Capacity cost of learning technologies (million)
CAPEX_NLP_LEG(i,v,r)        Capacity cost of learning technologies (million)
CAPEX_MIP(i,v,r)            Capacity cost of learning technologies (trillion)
QTRY(i,r,t)                 Stock of accumulated capacity (TW)
QTRY_LEG(i,r,t)             Stock of accumulated legacy capacity (from previous periods) (TW)
QLS(i,r,t,ls)               Stock of capacity on line segment ls (TW)
QLS_LEG(i,r,t,ls)           Stock of legacy capacity (from previous periods) on line segment ls (TW)
ACC_CAPEX_NLP(i,r,t)        Accumulated capacity cost of learning technologies (million)
ACC_CAPEX_NLP_LEG(i,r,t)    Accumulated legacy capacity cost of learning technologies (million)
ACC_CAPEX_MIP(i,r,t)        Accumulated capacity cost of learning technologies (trillion)
ACC_CAPEX_MIP_LEG(i,r,t)    Accumulated legacy capacity cost of learning technologies (trillion)
;

binary variable
RHO(i,r,t,ls)                    Binary variable that reflects piecewise linear segment of learning curve
RHO_LEG(i,r,t,ls)                Binary variable that reflects piecewise linear segment of learning curve (for legacy capacity)
;

Equation
acc_q_recall2020(i,r,t)          Accumulation of capacity in 2015 (doing)
acc_q_leg_recall2020(i,r,t)      Accumulation of legacy capacity in 2015 (doing)
acc_q_recall(i,r,t)              Accumulation of capacity (doing)
acc_q_leg_recall(i,r,t)          Accumulation of legacy capacity (doing)
acc_q_continuous2020(i,r,t)      Accumulation of capacity in 2015 (doing)
acc_q_leg_continuous2020(i,r,t)  Accumulation of legacy capacity in 2015 (doing)
acc_q_continuous(i,r,t)          Accumulation of capacity (doing)
acc_q_leg_continuous(i,r,t)      Accumulation of legacy capacity (doing)
acc_q_discrete2020(i,r,t)        Accumulation of capacity in 2015 (doing)
acc_q_leg_discrete2020(i,r,t)    Accumulation of legacy capacity in 2015 (doing)
acc_q_discrete(i,r,t)            Accumulation of capacity (doing)
acc_q_leg_discrete(i,r,t)        Accumulation of legacy capacity (doing)
rho_mixedip(i,r,t)               Equation that enforces one rho = 1
rho_leg_mixedip(i,r,t)           Equation that enforces one rho = 1 (for legacy capacity)
capex_constant(i,v,r)            Equation that describes evolution of CAPEX
capex_nonlinear(i,v,r)           Equation that describes evolution of CAPEX
capex_mixedip(i,v,r)             Equation that describes evolution of CAPEX (trillion EUR)
acc_capex_nonlinear(i,r,t)       Equation that describes evolution of accumulated CAPEX
acc_capex_leg_nonlinear(i,r,t)   Equation that describes evolution of accumulated legacy CAPEX
acc_capex_mixedip(i,r,t)         Equation that describes evolution of accumulated CAPEX (trillion EUR)
acc_capex_leg_mixedip(i,r,t)     Equation that describes evolution of accumulated legacy CAPEX (trillion EUR)
qlsLO_mixedip(i,r,t,ls)          Equation that enforces the lower bound of linear segment
qlsLO_leg_mixedip(i,r,t,ls)      Equation that enforces the lower bound of linear segment (for legacy capacity)
qlsUP_mixedip(i,r,t,ls)          Equation that enforces the upper bound of linear segment
qlsUP_leg_mixedip(i,r,t,ls)      Equation that enforces the upper bound of linear segment (for legacy capacity)
acc_qls_mixedip(i,r,t)           Equation that describes evolution of QLS
acc_qls_leg_mixedip(i,r,t)       Equation that describes evolution of legacy QLS
* Try a monotonicity constraint to speed up problem (for perfect recall only)
acc_q_mono(i,r,t)                Does not allow to reduce learning capacity stock (for perfect recall)
acc_q_leg_mono(i,r,t)            Does not allow to reduce learning capacity stock (for perfect recall)
acc_q_max(i,r,t)                 Limits learning capacity stock to max value
acc_q_leg_max(i,r,t)             Limits learning capacity stock to max value
rho_mixedip_mono(i,r,t,ls)       Does not allow to move backwards for rho (for perfect recall)
rho_leg_mixedip_mono(i,r,t,ls)   Does not allow to move backwards for rho (for perfect recall)
qls_mixedip_mono(i,r,t)          Does not allow to move backwards for rho (for perfect recall)
qls_leg_mixedip_mono(i,r,t)      Does not allow to move backwards for rho (for perfect recall)
;

* * * Regional Learning-by-doing
* * Accumulated capacity stock (legacy capacity concept is used to account for the fact that some capacity depreciates between period t and t-1 and to thus become cost right)
* with perfect recall
acc_q_recall2020(i,r,t)$(t.val le 2022 and toptimize(t) and i_lea(i) and r_lea(r))..
                 QTRY(i,r,t)        =e= qSTART(i,r) * 1e-3 ;
acc_q_leg_recall2020(i,r,t)$(t.val le 2022 and toptimize(t) and i_lea(i) and r_lea(r))..
                 QTRY_LEG(i,r,t)    =e= QTRY(i,r,t) ;
acc_q_recall(i,r,t)$(t.val ge 2023 and toptimize(t) and i_lea(i) and r_lea(r))..
                 QTRY(i,r,t)        =e= qSTART(i,r) * 1e-3 + sum(tt$(tt.val le t.val), IX(i,r,tt)) * 1e-3 ;
acc_q_leg_recall(i,r,t)$(t.val ge 2023 and toptimize(t) and i_lea(i) and r_lea(r))..
                 QTRY_LEG(i,r,t)    =e= QTRY(i,r,t-1) ;

* with continuous depreciation
acc_q_continuous2020(i,r,t)$(t.val le 2022 and toptimize(t) and i_lea(i) and r_lea(r))..
                 QTRY(i,r,t)        =e= sum(oldv(v), round(delta_q_pa**(t.val - v.val),4) * capt(i,v,r,"2022")) * 1e-3  ;
acc_q_leg_continuous2020(i,r,t)$(t.val le 2022 and toptimize(t) and i_lea(i) and r_lea(r))..
                 QTRY_LEG(i,r,t)    =e= QTRY(i,r,t);
acc_q_continuous(i,r,t)$(t.val ge 2023 and toptimize(t) and i_lea(i) and r_lea(r))..
                 QTRY(i,r,t)        =e= QTRY(i,r,t-1) * round(delta_q_pa**nyrs(t),4) + IX(i,r,t) * 1e-3 ;                
acc_q_leg_continuous(i,r,t)$(t.val ge 2023 and toptimize(t) and i_lea(i) and r_lea(r))..
                 QTRY_LEG(i,r,t)    =e= QTRY(i,r,t-1) * round(delta_q_pa**nyrs(t),4) ;

* with discrete depreciation
acc_q_discrete2020(i,r,t)$(t.val le 2022 and toptimize(t) and i_lea(i) and r_lea(r))..
                 QTRY(i,r,t)        =e= sum(oldv, deprtime(i,oldv,r,t) * capt(i,oldv,r,"2022")) * 1e-3 ;
acc_q_leg_discrete2020(i,r,t)$(t.val le 2022 and toptimize(t) and i_lea(i) and r_lea(r))..
                 QTRY_LEG(i,r,t)    =e= QTRY(i,r,t);
acc_q_discrete(i,r,t)$(t.val ge 2023 and toptimize(t) and i_lea(i) and r_lea(r))..
                 QTRY(i,r,t)        =e= sum(oldv, deprtime(i,oldv,r,t) * capt(i,oldv,r,"2022")) * 1e-3 + sum(newv(v)$(v.val < t.val), deprtime(i,v,r,t) * sum(tv(tt,v), IX.L(i,r,tt))) * 1e-3 + IX.L(i,r,t) * 1e-3 ;
acc_q_leg_discrete(i,r,t)$(t.val ge 2023 and toptimize(t) and i_lea(i) and r_lea(r))..
                 QTRY_LEG(i,r,t)    =e= sum(oldv, deprtime(i,oldv,r,t) * capt(i,oldv,r,"2022")) * 1e-3 + sum(newv(v)$(v.val < t.val), deprtime(i,v,r,t) * sum(tv(tt,v), IX.L(i,r,tt))) * 1e-3 ;
  
* Try a monotonicity constraint to speed up problem (for perfect recall only)                 
acc_q_mono(i,r,t)$(t.val ge 2023 and toptimize(t) and i_lea(i) and r_lea(r))..
                 QTRY(i,r,t)       =g= QTRY(i,r,t-1) ;
acc_q_leg_mono(i,r,t)$(t.val ge 2023 and toptimize(t) and i_lea(i) and r_lea(r))..
                 QTRY_LEG(i,r,t)   =g= QTRY_LEG(i,r,t-1) ;
* Maximum values
acc_q_max(i,r,t)$(t.val ge 2023 and toptimize(t) and i_lea(i) and r_lea(r))..
                 QTRY(i,r,t)       =l= qLAST(i,r) * 1e-3 ;
acc_q_leg_max(i,r,t)$(t.val ge 2023 and toptimize(t) and i_lea(i) and r_lea(r))..
                 QTRY_LEG(i,r,t)   =l= qLAST(i,r) * 1e-3 ;            

* * Unit cost
* Constant
capex_constant(i,newv(v),r)$(i_lea(i) and r_lea(r))..
                 CAPEX_CON(i,v,r)        =e= capcost(i,v,r) * sum(tv(t,v), IX(i,r,t)) ;
* Nonlinear
capex_nonlinear(i,newv(v),r)$(i_lea(i) and r_lea(r))..
$if      set leg CAPEX_NLP(i,v,r)        =e= sum(tv(t,v), ACC_CAPEX_NLP(i,r,t) - ACC_CAPEX_NLP_LEG(i,r,t)) ;
$if not  set leg CAPEX_NLP(i,v,r)        =e= sum(tv(t,v), ACC_CAPEX_NLP(i,r,t) - ACC_CAPEX_NLP(i,r,t-1)) ;

* MIP
capex_mixedip(i,newv(v),r)$(i_lea(i) and r_lea(r))..
$if      set leg CAPEX_MIP(i,v,r)        =e= sum(tv(t,v), ACC_CAPEX_MIP(i,r,t) - ACC_CAPEX_MIP_LEG(i,r,t)) ;
$if not  set leg CAPEX_MIP(i,v,r)        =e= sum(tv(t,v), ACC_CAPEX_MIP(i,r,t) - ACC_CAPEX_MIP(i,r,t-1)) ;

* * Accumulated cost (Constant unit cost does not demand for accumulation equations)
* Nonlinear
acc_capex_nonlinear(i,r,t)$(i_lea(i) and r_lea(r)and toptimize(t))..
                 ACC_CAPEX_NLP(i,r,t)     =e= capcost0(i,r) / (1 + b_q(i,r)) * (QTRY(i,r,t)    *1e+9)**(1 + b_q(i,r)) * 1e-6 ;
acc_capex_leg_nonlinear(i,r,t)$(i_lea(i) and r_lea(r)and toptimize(t))..
                 ACC_CAPEX_NLP_LEG(i,r,t) =e= capcost0(i,r) / (1 + b_q(i,r)) * (QTRY_LEG(i,r,t)*1e+9)**(1 + b_q(i,r)) * 1e-6 ;
* MIP
acc_capex_mixedip(i,r,t)$(i_lea(i) and r_lea(r)and toptimize(t))..
                 ACC_CAPEX_MIP(i,r,t)     =e= sum(ls, RHO(i,r,t,ls)     * acc_capexLO(i,r,ls) + slope_mip(i,r,ls) * (QLS(i,r,t,ls) * 1e+3     - qlsLO(i,r,ls) * RHO(i,r,t,ls))) * 1e-6 ;
acc_capex_leg_mixedip(i,r,t)$(i_lea(i) and r_lea(r)and toptimize(t))..
                 ACC_CAPEX_MIP_LEG(i,r,t) =e= sum(ls, RHO_LEG(i,r,t,ls) * acc_capexLO(i,r,ls) + slope_mip(i,r,ls) * (QLS_LEG(i,r,t,ls) * 1e+3 - qlsLO(i,r,ls) * RHO_LEG(i,r,t,ls))) * 1e-6 ;

* * Mixed integer variable rho and related equations
* This equation enforces that always just one line segment is active
rho_mixedip(i,r,t)$(i_lea(i) and r_lea(r)and toptimize(t))..
                 sum(ls, RHO(i,r,t,ls))     =e= 1 ;
rho_leg_mixedip(i,r,t)$(i_lea(i) and r_lea(r)and toptimize(t))..
                 sum(ls, RHO_LEG(i,r,t,ls)) =e= 1 ;

* Capacity stock per line segment is greater than the lower bound of that line segment (Learning by doing)
qlsLO_mixedip(i,r,t,ls)$(i_lea(i) and r_lea(r)and toptimize(t))..
                 QLS(i,r,t,ls)           =g= qlsLO(i,r,ls) * RHO(i,r,t,ls) * 1e-3 ;
qlsLO_leg_mixedip(i,r,t,ls)$(i_lea(i) and r_lea(r)and toptimize(t))..
                 QLS_LEG(i,r,t,ls)       =g= qlsLO(i,r,ls) * RHO_LEG(i,r,t,ls)* 1e-3  ;

* Capacity stock per line segment is lower than the upper bound of that line segment (Learning by doing)
qlsUP_mixedip(i,r,t,ls)$(i_lea(i) and r_lea(r)and toptimize(t))..
                 QLS(i,r,t,ls)           =l= qlsUP(i,r,ls) * RHO(i,r,t,ls) * 1e-3 ;
qlsUP_leg_mixedip(i,r,t,ls)$(i_lea(i) and r_lea(r)and toptimize(t))..
                 QLS_LEG(i,r,t,ls)       =l= qlsUP(i,r,ls) * RHO_LEG(i,r,t,ls) * 1e-3 ;

* Capacity stock per "active" (rho = 1) line segment is equal to capacity stock  (Learning by doing)
acc_qls_mixedip(i,r,t)$(i_lea(i) and r_lea(r)and toptimize(t))..
                 QTRY(i,r,t)             =e= sum(ls, QLS(i,r,t,ls)) ;
acc_qls_leg_mixedip(i,r,t)$(i_lea(i) and r_lea(r)and toptimize(t))..
                 QTRY_LEG(i,r,t)         =e= sum(ls, QLS_LEG(i,r,t,ls)) ;
                                  
* Try a monotonicity constraint to speed up problem (for perfect recall only)
rho_mixedip_mono(i,r,t,ls)$(t.val ge 2023 and toptimize(t) and i_lea(i) and r_lea(r))..              
                 RHO(i,r,t,ls) =g= RHO(i,r,t-1,ls) ;
rho_leg_mixedip_mono(i,r,t,ls)$(t.val ge 2023 and toptimize(t) and i_lea(i) and r_lea(r))..              
                 RHO_LEG(i,r,t,ls) =g= RHO_LEG(i,r,t-1,ls) ;
                 
* Try a monotonicity constraint to speed up problem (for perfect recall only)
qls_mixedip_mono(i,r,t)$(t.val ge 2023 and toptimize(t) and i_lea(i) and r_lea(r))..              
                 sum(ls, QLS(i,r,t,ls)) =g= sum(ls, QLS(i,r,t-1,ls)) ;
qls_leg_mixedip_mono(i,r,t)$(t.val ge 2023 and toptimize(t) and i_lea(i) and r_lea(r))..              
                 sum(ls, QLS_LEG(i,r,t,ls)) =g= sum(ls, QLS_LEG(i,r,t-1,ls)) ;  
 ;  
