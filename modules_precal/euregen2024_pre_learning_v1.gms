* * * LBD: Learning-by-doing
set
i_del(i)         Technologies to exclude from the model
i_lea(i)         Technologies to learn
r_lea(r)         Regions to learn
ir_lea(i,r)      Technology region pair under learning
ls               Number of line segments
dpr              Depreciation method (of experience stock)
;

$gdxin database\setpar_%n%.gdx
$load i_del, i_lea, r_lea, ls, dpr
$gdxin

ir_lea(i,r) = YES$(i_lea(i) and r_lea(r)) ;

$if      set learning capcost(i,v,r)$(i_lea(i)) = capcost_int(i,v,r) ;
$if      set learning capcost(i_del(i),v,r) = 0 ;
$if      set learning lifetime(i_del(i),v,r,t) = NO ;
$if      set learning deprtime(i_del(i),v,r,t) = NO ;
$if      set learning capcost(i_del(i),v,r)    = 0 ;
$if      set learning vrsc(s,i_del(i),v,r)       = 0 ;
$if      set learning invlimLO(i_del(i),r,t)   = 0 ;
$if      set learning cap(i_del(i),v,r)        = 0 ;

parameter
ls_weight(ls)            Weight to determine breakpoints
delta_q_pa               Annual depreciation factor of the experience stock
delta_q                  Periodical depreciation factor of the experience stock
;

$gdxin database\setpar_%n%.gdx
$load ls_weight,delta_q_pa, delta_q
$gdxin

* * * Regional Learning-by-doing (LBD)
parameter
* Interim parameter
b_q_int(dpr,i,r) 
qFIRST_int(dpr,i,r)
qSTART_int(dpr,i,r) 
qLAST_int(dpr,i,r) 
qlsLO_int(dpr,i,r,ls) 
qlsUP_int(dpr,i,r,ls) 
capcost0_int(dpr,i,r) 
acc_capexFIRST_int(dpr,i,r) 
acc_capexSTART_int(dpr,i,r) 
acc_capexLAST_int(dpr,i,r) 
acc_capexLO_int(dpr,i,r,ls) 
acc_capexUP_int(dpr,i,r,ls) 
slope_mip_int(dpr,i,r,ls) 
* Used parameter
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


$gdxin database\setpar_%n%.gdx
$load   b_q_int  =   b_q 
$load   qFIRST_int   =   qFIRST
$load   qSTART_int   =   qSTART 
$load   qLAST_int    =   qLAST 
$load   qlsLO_int    =   qlsLO 
$load   qlsUP_int    =   qlsUP 
$load   capcost0_int     =   capcost0 
$load   acc_capexFIRST_int   =   acc_capexFIRST 
$load   acc_capexSTART_int   =   acc_capexSTART 
$load   acc_capexLAST_int    =   acc_capexLAST 
$load   acc_capexLO_int  =   acc_capexLO 
$load   acc_capexUP_int  =   acc_capexUP 
$load   slope_mip_int    =   slope_mip 
$gdxin

b_q(i,r)   =   b_q_int("rec",i,r)     ;
qFIRST(i,r)    =   qFIRST_int("rec",i,r)  ;
qSTART(i,r)    =   qSTART_int("rec",i,r)  ;
qLAST(i,r)     =   qLAST_int("rec",i,r)   ;
qlsLO(i,r,ls)  =   qlsLO_int("rec",i,r,ls)    ;
qlsUP(i,r,ls)  =   qlsUP_int("rec",i,r,ls)    ;
capcost0(i,r)  =   capcost0_int("rec",i,r)    ;
acc_capexFIRST(i,r)    =   acc_capexFIRST_int("rec",i,r)  ;
acc_capexSTART(i,r)    =   acc_capexSTART_int("rec",i,r)  ;
acc_capexLAST(i,r)     =   acc_capexLAST_int("rec",i,r)   ;
acc_capexLO(i,r,ls)    =   acc_capexLO_int("rec",i,r,ls)  ;
acc_capexUP(i,r,ls)    =   acc_capexUP_int("rec",i,r,ls)  ;
slope_mip(i,r,ls)  =   slope_mip_int("rec",i,r,ls)    ;

$if      set continuous  b_q(i,r)   =   b_q_int("rec",i,r)     ;
$if      set continuous  qFIRST(i,r)    =   qFIRST_int("rec",i,r)  ;
$if      set continuous  qSTART(i,r)    =   qSTART_int("rec",i,r)  ;
$if      set continuous  qLAST(i,r)     =   qLAST_int("rec",i,r)   ;
$if      set continuous  qlsLO(i,r,ls)  =   qlsLO_int("rec",i,r,ls)    ;
$if      set continuous  qlsUP(i,r,ls)  =   qlsUP_int("rec",i,r,ls)    ;
$if      set continuous  capcost0(i,r)  =   capcost0_int("rec",i,r)    ;
$if      set continuous  acc_capexFIRST(i,r)    =   acc_capexFIRST_int("rec",i,r)  ;
$if      set continuous  acc_capexSTART(i,r)    =   acc_capexSTART_int("rec",i,r)  ;
$if      set continuous  acc_capexLAST(i,r)     =   acc_capexLAST_int("rec",i,r)   ;
$if      set continuous  acc_capexLO(i,r,ls)    =   acc_capexLO_int("rec",i,r,ls)  ;
$if      set continuous  acc_capexUP(i,r,ls)    =   acc_capexUP_int("rec",i,r,ls)  ;
$if      set continuous  slope_mip(i,r,ls)  =   slope_mip_int("rec",i,r,ls)    ;

$if      set discrete      b_q(i,r)   =   b_q_int("rec",i,r)     ;
$if      set discrete      qFIRST(i,r)    =   qFIRST_int("rec",i,r)  ;
$if      set discrete      qSTART(i,r)    =   qSTART_int("rec",i,r)  ;
$if      set discrete      qLAST(i,r)     =   qLAST_int("rec",i,r)   ;
$if      set discrete      qlsLO(i,r,ls)  =   qlsLO_int("rec",i,r,ls)    ;
$if      set discrete      qlsUP(i,r,ls)  =   qlsUP_int("rec",i,r,ls)    ;
$if      set discrete      capcost0(i,r)  =   capcost0_int("rec",i,r)    ;
$if      set discrete      acc_capexFIRST(i,r)    =   acc_capexFIRST_int("rec",i,r)  ;
$if      set discrete      acc_capexSTART(i,r)    =   acc_capexSTART_int("rec",i,r)  ;
$if      set discrete      acc_capexLAST(i,r)     =   acc_capexLAST_int("rec",i,r)   ;
$if      set discrete      acc_capexLO(i,r,ls)    =   acc_capexLO_int("rec",i,r,ls)  ;
$if      set discrete      acc_capexUP(i,r,ls)    =   acc_capexUP_int("rec",i,r,ls)  ;
$if      set discrete      slope_mip(i,r,ls)  =   slope_mip_int("rec",i,r,ls)    ;

* * * European Learning-by-doing
parameter
* Interim parameter
b_qeur_int(dpr,i) 
qeurFIRST_int(dpr,i)
qeurSTART_int(dpr,i) 
qeurLAST_int(dpr,i) 
qeurlsLO_int(dpr,i,ls) 
qeurlsUP_int(dpr,i,ls) 
capcosteur0_int(dpr,i) 
acc_capexeurFIRST_int(dpr,i) 
acc_capexeurSTART_int(dpr,i) 
acc_capexeurLAST_int(dpr,i) 
acc_capexeurLO_int(dpr,i,ls) 
acc_capexeurUP_int(dpr,i,ls) 
slopeeur_mip_int(dpr,i,ls) 
* Used parameter
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

$gdxin database\setpar_%n%.gdx
$load   b_qeur_int  =   b_qeur 
$load   qeurFIRST_int   =   qeurFIRST
$load   qeurSTART_int   =   qeurSTART 
$load   qeurLAST_int    =   qeurLAST 
$load   qeurlsLO_int    =   qeurlsLO 
$load   qeurlsUP_int    =   qeurlsUP 
$load   capcosteur0_int     =   capcosteur0 
$load   acc_capexeurFIRST_int   =   acc_capexeurFIRST 
$load   acc_capexeurSTART_int   =   acc_capexeurSTART 
$load   acc_capexeurLAST_int    =   acc_capexeurLAST 
$load   acc_capexeurLO_int  =   acc_capexeurLO 
$load   acc_capexeurUP_int  =   acc_capexeurUP 
$load   slopeeur_mip_int    =   slopeeur_mip 
$gdxin

b_qeur(i)   =   b_qeur_int("rec",i)     ;
qeurFIRST(i)    =   qeurFIRST_int("rec",i)  ;
qeurSTART(i)    =   qeurSTART_int("rec",i)  ;
qeurLAST(i)     =   qeurLAST_int("rec",i)   ;
qeurlsLO(i,ls)  =   qeurlsLO_int("rec",i,ls)    ;
qeurlsUP(i,ls)  =   qeurlsUP_int("rec",i,ls)    ;
capcosteur0(i)  =   capcosteur0_int("rec",i)    ;
acc_capexeurFIRST(i)    =   acc_capexeurFIRST_int("rec",i)  ;
acc_capexeurSTART(i)    =   acc_capexeurSTART_int("rec",i)  ;
acc_capexeurLAST(i)     =   acc_capexeurLAST_int("rec",i)   ;
acc_capexeurLO(i,ls)    =   acc_capexeurLO_int("rec",i,ls)  ;
acc_capexeurUP(i,ls)    =   acc_capexeurUP_int("rec",i,ls)  ;
slopeeur_mip(i,ls)  =   slopeeur_mip_int("rec",i,ls)    ;

$if      set continuous  b_qeur(i)   =   b_qeur_int("rec",i)     ;
$if      set continuous  qeurFIRST(i)    =   qeurFIRST_int("rec",i)  ;
$if      set continuous  qeurSTART(i)    =   qeurSTART_int("rec",i)  ;
$if      set continuous  qeurLAST(i)     =   qeurLAST_int("rec",i)   ;
$if      set continuous  qeurlsLO(i,ls)  =   qeurlsLO_int("rec",i,ls)    ;
$if      set continuous  qeurlsUP(i,ls)  =   qeurlsUP_int("rec",i,ls)    ;
$if      set continuous  capcosteur0(i)  =   capcosteur0_int("rec",i)    ;
$if      set continuous  acc_capexeurFIRST(i)    =   acc_capexeurFIRST_int("rec",i)  ;
$if      set continuous  acc_capexeurSTART(i)    =   acc_capexeurSTART_int("rec",i)  ;
$if      set continuous  acc_capexeurLAST(i)     =   acc_capexeurLAST_int("rec",i)   ;
$if      set continuous  acc_capexeurLO(i,ls)    =   acc_capexeurLO_int("rec",i,ls)  ;
$if      set continuous  acc_capexeurUP(i,ls)    =   acc_capexeurUP_int("rec",i,ls)  ;
$if      set continuous  slopeeur_mip(i,ls)  =   slopeeur_mip_int("rec",i,ls)    ;

$if      set discrete    b_qeur(i)   =   b_qeur_int("rec",i)     ;
$if      set discrete    qeurFIRST(i)    =   qeurFIRST_int("rec",i)  ;
$if      set discrete    qeurSTART(i)    =   qeurSTART_int("rec",i)  ;
$if      set discrete    qeurLAST(i)     =   qeurLAST_int("rec",i)   ;
$if      set discrete    qeurlsLO(i,ls)  =   qeurlsLO_int("rec",i,ls)    ;
$if      set discrete    qeurlsUP(i,ls)  =   qeurlsUP_int("rec",i,ls)    ;
$if      set discrete    capcosteur0(i)  =   capcosteur0_int("rec",i)    ;
$if      set discrete    acc_capexeurFIRST(i)    =   acc_capexeurFIRST_int("rec",i)  ;
$if      set discrete    acc_capexeurSTART(i)    =   acc_capexeurSTART_int("rec",i)  ;
$if      set discrete    acc_capexeurLAST(i)     =   acc_capexeurLAST_int("rec",i)   ;
$if      set discrete    acc_capexeurLO(i,ls)    =   acc_capexeurLO_int("rec",i,ls)  ;
$if      set discrete    acc_capexeurUP(i,ls)    =   acc_capexeurUP_int("rec",i,ls)  ;
$if      set discrete    slopeeur_mip(i,ls)  =   slopeeur_mip_int("rec",i,ls)    ;
  
* * * R&D: Learning-by-lbs
set
ki_del(i)         Technologies to exclude from the model
ki_lea(i)         Technologies to learn
kr_lea(r)         Regions to learn
kir_lea(i,r)      Technology region pair under learning
kls               Number of line segments
;

$gdxin database\setpar_%n%.gdx
$load ki_del, ki_lea, kr_lea, kls
$gdxin


parameter
kcapcost(i,v,r)
kls_weight(kls)          Weight to determine breakpoints
delta_k_pa               Annual depreciation factor of the experience stock
delta_k                  Periodical depreciation factor of the experience stock
;

$gdxin database\setpar_%n%.gdx
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

$gdxin database\setpar_%n%.gdx
$load b_k,kFIRST,kSTART,kLAST,klsLO,klsUP,kcapcost0,kcapcostFIRST,kcapcostSTART,kcapcostLAST,kcapcostLO,kcapcostUP,kcapcostAV,kacc_capexFIRST,kacc_capexSTART,kacc_capexLAST,kacc_capexLO,kacc_capexUP,kslope_lin,kslope_mip,ktest_slope,ktest_slope2,rd_budget
$load kspillover
$load k_exo
$gdxin

kir_lea(i,r) = YES$(kcapcost0(i,r) > 0) ;

$if      set lbs    capcost(i,v,r)$(ki_lea(i)) = kcapcost(i,v,r) ;
$if      set lbseur capcost(i,v,r)$(kir_lea(i,r)) = kcapcost(i,v,r) ;
$if      set lbsbenchmark   capcost(i,v,r)$(kir_lea(i,r)) = kcapcost(i,v,r) ;

parameter
tspillover(i,i)
;

tspillover("WindOn_q90","WindOff_q90") = 0 ;
tspillover("WindOff_q90","WindOn_q90") = 0 ;

$if      set fulltechspillover   tspillover("WindOn_q90","WindOff_q90") = 1 ;
$if      set fulltechspillover   tspillover("WindOff_q90","WindOn_q90") = 1 ;
$if      set halftechspillover   tspillover("WindOn_q90","WindOff_q90") = 0.5 ;
$if      set halftechspillover   tspillover("WindOff_q90","WindOn_q90") = 0.5 ;
$if      set quartechspillover   tspillover("WindOn_q90","WindOff_q90") = 0.25 ;
$if      set quartechspillover   tspillover("WindOff_q90","WindOn_q90") = 0.25 ;

parameter
kspillover_int(i,r,r)
;

kspillover_int(i,r,rr) = kspillover(i,r,rr) ;
$if      set spill2  kspillover(i,r,rr) = 2  * kspillover_int(i,r,rr) ;
$if      set spill5  kspillover(i,r,rr) = 5  * kspillover_int(i,r,rr) ;

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

$gdxin database\setpar_%n%.gdx
$load kcapcosteur, b_keur,keurFIRST,keurSTART,keurLAST,keurlsLO,keurlsUP,kcapcosteur0,kcapcosteurSTART,kcapcosteurLAST,kcapcosteurLO,kcapcosteurUP,kcapcosteurAV,kacc_capexeurFIRST,kacc_capexeurSTART,kacc_capexeurLAST,kacc_capexeurLO,kacc_capexeurUP,kslopeeur_lin,kslopeeur_mip,ktest_slopeeur,ktest_slopeeur2,rd_budgeteur
$gdxin

$if      set lbsnobenchmark capcost(i,v,r)$(kir_lea(i,r)) = kcapcosteur(i,v) ;

                 
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
lbsrdbudget_int(i,r,v)
lbseurrdbudget_int(i,v)
;

lbsrdbudget_int(i,r,v) = sum(tv(t,v), rd_budget(i,r,t)) ;
lbseurrdbudget_int(i,v) = sum(tv(t,v), rd_budgeteur(i,t)) ;

lbsrdbudget(i,r,v) = 1 * lbsrdbudget_int(i,r,v) ;
lbseurrdbudget(i,v) = 1 * lbseurrdbudget_int(i,v) ;

$if      set halfbudget      lbsrdbudget(i,r,v) = 0.5 * lbsrdbudget_int(i,r,v) ;
$if      set halfbudget      lbseurrdbudget(i,v) = 0.5 * lbseurrdbudget_int(i,v) ;

$if      set threeqbudget    lbsrdbudget(i,r,v) = 0.75 * lbsrdbudget_int(i,r,v) ;
$if      set threeqbudget    lbseurrdbudget(i,v) = 0.75 * lbseurrdbudget_int(i,v) ;

$if      set onefiftybudget  lbsrdbudget(i,r,v) = 1.5 * lbsrdbudget_int(i,r,v) ;
$if      set onefiftybudget  lbseurrdbudget(i,v) = 1.5 * lbseurrdbudget_int(i,v) ;

$if      set doublebudget    lbsrdbudget(i,r,v) = 2 * lbsrdbudget_int(i,r,v) ;
$if      set doublebudget    lbseurrdbudget(i,v) = 2 * lbseurrdbudget_int(i,v) ;

* * * FLH LBS
set
i_flh(i)
ir_flh(i,r)
flhls
;

$gdxin database\setpar_%n%.gdx
$load i_flh, ir_flh, flhls
$gdxin

* * Regional
parameter
bflh(i,r) learning rate (%)
flhstock(i,r,v) knowledge stock (million ?)
flhrdbudget(i,r,v) RD budget (million ?)
flhindex(i,r,v) Average FLH (index 2022 = 1)
flh(i,r,v) Average FLH (h per a)
;

$gdxin database\setpar_%n%.gdx
$load bflh,flhstock,flhrdbudget,flhindex,flh
$gdxin

parameter
$if not  set days   vrsc_nor(s,i,v,r)
$if      set days   vrsc_nor_d(sd,hd,i,v,r)
flh_check(i,v,r)
;

$if not  set days   vrsc_nor(s,i,v,r)$(ir_flh(i,r) and v.val ge 2023 and sum(ss, hours(ss) * vrsc(ss,i,v,r)) > 0) = vrsc(s,i,v,r) * sum(ss, hours(ss) * vrsc(ss,i,"2022",r)) / sum(ss, hours(ss) * vrsc(ss,i,v,r)) ;
$if      set days   vrsc_nor_d(sd,hd,i,v,r)$(ir_flh(i,r) and v.val ge 2023 and sum((sdd,hdd), days(sdd) * vrsc_d(sdd,hdd,i,v,r)) > 0) = vrsc_d(sd,hd,i,v,r) * sum((sdd,hdd), days(sdd) * vrsc_d(sdd,hdd,i,"2022",r)) / sum((sdd,hdd), days(sdd) * vrsc_d(sdd,hdd,i,v,r)) ;
$if not  set days   flh_check(i,v,r) = sum(s, hours(s) * vrsc_nor(s,i,v,r)) ;
$if      set days   flh_check(i,v,r) = sum((sd,hd), days(sd) * vrsc_nor_d(sd,hd,i,v,r)) ;

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

$gdxin database\setpar_%n%.gdx
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
               
* * European
parameter
bflheur(i) learning rate (%)
flheurindex_reg(i,r,v) regional correction (0 ... X)
flheurstock(i,v) knowledge stock (million ?)
flheurrdbudget(i,v) RD budget (million ?)
flheurindex(i,v) Average FLH (index 2022 = 1)
flheur(i,v) Average FLH (h per a)
;

$gdxin database\setpar_%n%.gdx
$load bflheur,flheurstock,flheurindex_reg,flheurrdbudget,flheurindex,flheur
$gdxin

parameter
$if not  set days   vrsceur_nor(s,i,v,r)
flheur_check(i,v,r)
;

$if not  set days   vrsceur_nor(s,i,v,r)$(ir_flh(i,r) and v.val ge 2023 and sum(ss, hours(ss) * vrsc(ss,i,v,r)) > 0) = vrsc(s,i,v,r) * flheur(i,"2022") / sum(ss, hours(ss) * vrsc(ss,i,v,r)) ;
$if not  set days   flheur_check(i,v,r) = sum(s, hours(s) * vrsceur_nor(s,i,v,r)) ;

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

$gdxin database\setpar_%n%.gdx
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

set
ivt(i,v,t)              Active vintage-capacity blocks aggregated to European metric
ivttv(i,v,t)
notir_lea(i,r)
notkir_lea(i,r)
;

* ETC modules
ivt(i,v,t) = YES$(ivrt(i,v,"Germany",t) and i_lea(i)) ;
ivttv(ivt(i,v,t))$(tv(t,v)) = YES ;
notir_lea(i,r)$(not ir_lea(i,r)) = YES ;
notkir_lea(i,r)$(not kir_lea(i,r)) = YES ;

parameter
deprtimeeur(i,v,t)      deprtime for European learning metric
endeffecteur(i,v,t)     endeffect for European learning metric
kendeffect(i,r,t)       endeffect for lbs
kendeffecteur(i,t)      endeffecteur for lbs
;
deprtimeeur(i,v,t)$(sum(r, 1$deprtime(i,v,r,t)) > 0) = sum(r, deprtime(i,v,r,t)) / sum(r, 1$deprtime(i,v,r,t)) ;
endeffecteur(i,v,t)$(sum(r, 1$endeffect(i,v,r,t)) > 0) = sum(r, endeffect(i,v,r,t)) / sum(r, 1$endeffect(i,v,r,t)) ;
kendeffect(i,r,t)$(kir_lea(i,r)) = sum(tv(t,v), endeffect(i,v,r,t)) ;
kendeffecteur(i,t)$(ki_lea(i) and sum(r, 1$kendeffect(i,r,t)) > 0) = sum(r, kendeffect(i,r,t)) / sum(r, 1$kendeffect(i,r,t)) ;

parameter
rshare(inv,i)
rzeta(inv,i,v)
;

rzeta(inv,i,v) = sum(r, share(inv,i,r) * zeta(inv,i,v,r) * daref(r,"2022")) / sum(r, daref(r,"2022")) ;
rshare(inv,i)  = sum(r, share(inv,i,r) * daref(r,"2022")) / sum(r, daref(r,"2022")) ;
