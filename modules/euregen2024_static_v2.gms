parameter
capt_static(i,v,r,t)
gcapt_static(j,v,r,t)
gccapt_static(j,v,r,t)
gdcapt_static(j,v,r,t)
grcapt_static(j,v,r,t)
gcaptnv_static(j,r,t)
tcapt_static(k,r,r,t)
co2eleeu_static(t)
co2eleuk_static(t)
co2prreg_static(r,t)
;

$if      set fromsame   $gdxin static\%sn%_%l%_%s%.gdx
$if      set from6d     $gdxin static\6d_%l%_%s%.gdx
$if      set from15d    $gdxin static\15d_%l%_%s%.gdx
$if      set from31s    $gdxin static\31s_%l%_%s%.gdx
$if      set from119s   $gdxin static\119s_%l%_%s%.gdx
$load capt_static, gcapt_static, gccapt_static, gdcapt_static, grcapt_static, gcaptnv_static, tcapt_static, co2eleeu_static, co2eleuk_static, co2prreg_static=co2prreg
$gdxin

equations
exlife_static(i,v,r,t)
gexlife_static(j,v,r,t)
gcexlife_static(j,v,r,t)
gdexlife_static(j,v,r,t)
grexlife_static(j,v,r,t)
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
gcexlife_static(jvrt(j,v,r,t))..
    GCC(j,v,r,t) =e= gccapt_static(j,v,r,t) ;
gdexlife_static(jvrt(j,v,r,t))..
    GCD(j,v,r,t) =e= gdcapt_static(j,v,r,t) ;   
grexlife_static(jvrt(j,v,r,t))..
    GCR(j,v,r,t) =e= grcapt_static(j,v,r,t) ;
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

