parameter
capt_static(i,v,r,t)
gcapt_static(j,v,r,t)
gcaptnv_static(j,r,t)
tcapt_static(k,r,r,t)
co2eleeu_static(t)
co2eleuk_static(t)
co2prreg_static(r,t)
;

$gdxin static\%l%_%s%.gdx
$load capt_static, gcapt_static, gcaptnv_static, tcapt_static, co2eleeu_static, co2eleuk_static, co2prreg_static=co2prreg
$gdxin

equations
exlife_static(i,v,r,t)
gexlife_static(j,v,r,t)
gexlifenv_static(j,r,t)
tinvestexi_static(k,r,rr,t)
euets_static(t)
ukets_static(t)
;

* * * * * Static constraints
exlife_static(ivrt(i,v,r,t))..
    XC(i,v,r,t) =e= capt_static(i,v,r,t) ;
gexlife_static(jvrt(j,v,r,t))..
    GC(j,v,r,t) =e= gcapt_static(j,v,r,t) ;
gexlifenv_static(j,r,t)..
    GCNV(j,r,t) =e= gcaptnv_static(j,r,t) ;
tinvestexi_static(tmapopt(k,r,rr,t))..
    TC(k,r,rr,t) =e= tcapt_static(k,r,rr,t) ;
euets_static(t)..
    ECEU(t) =e= co2eleeu_static(t) ;
ukets_static(t)..
    ECUK(t) =e= co2eleuk_static(t) ;
    
* * * Dispatch cost
parameter
$if not  set days   discost_static(s,i,v,r,t)         Dispatch cost (EUR per MWh el)
$if      set days   discost_static_d(sd,i,v,r,t)      Dispatch cost (EUR per MWh el)
;

$if not  set days   discost_static(s,ivrt(i,v,r,t)) =
$if      set days   discost_static_d(sd,ivrt(i,v,r,t)) = 
*        Variable O&M costs
                           vomcost(i,v,r) 
*        Fuel costs (including region-specific price delta) including regional adder relative                    
                         + round(sum(xfueli(fuel,i), pfuel(fuel,r,t) / effrate(i,v,r)), 8)
* Determine true average load of each vintages and calibrate for ramping cost here
* Determine true average load of each vintages and calibrate for ramping cost here
$if      set ramcost     * (1 + effloss(i,v,r) / 0.5 )
*        Regional adder absolute
*                         + (pfadd(fuel,r,t)$xfueli(fuel,i))$effrate(i,v,r)
*        CO2 price (includes benefits from negative emissions)
$if      set co2price    + emit(i,v,r) * co2prreg_static(r,t) 
*        CCS costs (from capturing)
$if      set ccs         + round(co2captured(i,v,r) * ccscost(r,t), 8)
;

