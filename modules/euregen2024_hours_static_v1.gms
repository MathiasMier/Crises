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
    
set
d /1*365/
comm /Gas,Coal,Oil,Uranium,CO2/
maphd(h,d)
;

*$onecho >temp\gdxxrw.rsp
*set=maphd      rng=maphd!a2                rdim=2 cdim=0 values=nodata

*$offecho
*$call 'gdxxrw i=database\data\static_data.xlsx o=database\data_static\set_static.gdx trace=3 log=database\temp\set_static.log @temp\gdxxrw.rsp';

$gdxin database\data_static\set_static
$load maphd
$gdxin

parameter
static2022(d,comm)
static2023(d,comm)
co2p_static(s,t)
co2p_static_ave(t)
pfuel_static(s,fuel,r,t)
;

*$onecho >temp\gdxxrw.rsp
*par=static2022           rng=2022!a1                rdim=1 cdim=1
*par=static2023           rng=2023!a1                rdim=1 cdim=1
*$offecho

*$call 'gdxxrw i=database\data\static_data.xlsx o=database\data_static\par_static.gdx trace=3 log=database\temp\par_static.log @temp\gdxxrw.rsp';

$gdxin database\data_static\par_static
$load static2022,static2023
$gdxin

co2p_static(s,"2022") = sum(srep(s,h), sum(maphd(h,d), static2022(d,"CO2"))) ;
*co2p_static(s,"2023") = sum(srep(s,h), sum(maphd(h,d), static2022(d,"CO2"))) ;

pfuel_static(s,"Gas",r,"2022") = sum(srep(s,h), sum(maphd(h,d), static2022(d,"Gas"))) ;
*pfuel_static(s,"Gas",r,"2023") = sum(srep(s,h), sum(maphd(h,d), static2023(d,"Gas"))) ;

pfuel_static(s,"Coal",r,"2022") = sum(srep(s,h), sum(maphd(h,d), static2022(d,"Coal"))) ;
*pfuel_static(s,"Coal",r,"2023") = sum(srep(s,h), sum(maphd(h,d), static2023(d,"Coal"))) ;

pfuel_static(s,"Oil",r,"2022") = sum(srep(s,h), sum(maphd(h,d), static2022(d,"Oil"))) ;
*pfuel_static(s,"Oil",r,"2023") = sum(srep(s,h), sum(maphd(h,d), static2023(d,"Oil"))) ;

pfuel_static(s,"Uranium",r,"2022") = sum(srep(s,h), sum(maphd(h,d), static2022(d,"Uranium"))) ;
*pfuel_static(s,"Uranium",r,"2023") = sum(srep(s,h), sum(maphd(h,d), static2023(d,"Uranium"))) ;

co2p_static_ave("2022") = 81.48 ;
*co2p_static_ave("2023") = 89.38 ;

* * * Dispatch cost
parameter
discost_static(s,i,v,r,t)
;

discost_static(s,ivrt(i,v,r,t)) = 
*        Variable O&M costs
                           vomcost(i,v,r)
*        Fuel costs (including region-specific price delta) including regional adder relative                    
$if not  set baup        + round(sum(xfueli(fuel,i), pfuel_static(s,fuel,r,t) / effrate(i,v,r)), 8)
$if      set baup        + round(sum(xfueli(fuel,i), pfuel(fuel,r,t) / effrate(i,v,r)), 8)
* Determine true average load of each vintages and calibrate for ramping cost here
* Determine true average load of each vintages and calibrate for ramping cost here
$if      set ramcost     * (1 + effloss(i,v,r) / 0.5 )
*        Regional adder absolute
*                         + (pfadd(fuel,r,t)$xfueli(fuel,i))$effrate(i,v,r)
*        CO2 price (includes benefits from negative emissions)
$if not  set baup        $if      set co2price    + emit(i,v,r) * co2p_static(s,t) * co2prreg_static(r,t) / co2p_static_ave(t)
$if      set baup        $if      set co2price    + emit(i,v,r) * co2prreg_static(r,t) 
*        CCS costs (from capturing)
$if      set ccs         + round(co2captured(i,v,r) * ccscost(r,t), 8)
;
