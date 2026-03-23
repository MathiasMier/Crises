parameter
capt_static(i,v,r,t)
gcapt_static(j,v,r,t)
gccapt_static(j,v,r,t)
gdcapt_static(j,v,r,t)
grcapt_static(j,v,r,t)
gcaptnv_static(j,r,t)
capt_static_int(i,v,r,t)
gcapt_static_int(j,v,r,t)
gcaptnv_static_int(j,r,t)
tcapt_static(k,r,r,t)
co2eleeu_static(t)
co2eleuk_static(t)
co2prreg_static(r,t)
co2prrefreg_static(r,t)
;

$if      set fromsame   $if      set fromref    $gdxin static\%sn%_newcap_high_str_frn_hyd_loa.gdx
$if      set fromsame   $if not  set fromref    $gdxin static\%sn%_%l%_%s%.gdx

$if      set from6d     $if      set fromref    $gdxin static\6d_%l%_newcap_high_str_frn_hyd_loa.gdx
$if      set from6d     $if not  set fromref    $gdxin static\6d_%l%_%s%.gdx

$if      set from15d    $if      set fromref    $gdxin static\15d_%l%_newcap_high_str_frn_hyd_loa.gdx
$if      set from15d    $if not  set fromref    $gdxin static\15d_%l%_%s%.gdx

$if      set from31s    $if      set fromref    $gdxin static\31s_%l%_newcap_high_str_frn_hyd_loa.gdx
$if      set from31s    $if not  set fromref    $gdxin static\31s_%l%_%s%.gdx

$if      set from119s   $if      set fromref    $gdxin static\119s_%l%_newcap_high_str_frn_hyd_loa.gdx
$if      set from119s   $if not  set fromref    $gdxin static\119s_%l%_%s%.gdx


$load capt_static, gcapt_static, gccapt_static, gdcapt_static, grcapt_static, gcaptnv_static, tcapt_static, co2eleeu_static, co2eleuk_static, co2prreg_static=co2prreg
$gdxin

*capt_static_int(i,v,r,t) = capt_static(i,v,r,t) ;
*gcapt_static_int(j,v,r,t) = gcapt_static(j,v,r,t) ;
*gcaptnv_static_int(j,r,t) = gcaptnv_static(j,r,t) ;

*capt_static_int(i,v,r,t) = capt(i,v,r,t) ;
*gcapt_static_int(j,v,r,t) = gcapt(j,v,r,t) ;
*gcaptnv_static_int(j,r,t) = sum(v, gcapt(j,v,r,t)) ;

$if      set co2priceref    $if      set fromsame   $gdxin static\%sn%_%l%_newcap_high_str_frn_hyd_loa.gdx
$if      set co2priceref    $if      set from6d     $gdxin static\6d_%l%_newcap_high_str_frn_hyd_loa.gdx
$if      set co2priceref    $if      set from15d    $gdxin static\15d_%l%_newcap_high_str_frn_hyd_loa.gdx
$if      set co2priceref    $if      set from31s    $gdxin static\31s_%l%_newcap_high_str_frn_hyd_loa.gdx
$if      set co2priceref    $if      set from119s   $gdxin static\119s_%l%_newcap_high_str_frn_hyd_loa.gdx
$if      set co2priceref    $load co2prrefreg_static=co2prreg
$if      set co2priceref    $gdxin

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

* Ensure that static capacity in calibration years is at least calibration capacity
*capt_static(i,v,r,topt2023(t)) = max(capt_static(i,v,r,t),capt(i,v,r,t)) ;
*gcapt_static(j,v,r,topt2023(t)) = max(gcapt_static(j,v,r,t),gcapt(j,v,r,t)) ;
*gcaptnv_static(j,r,topt2023(t)) = max(gcaptnv_static(j,r,t),sum(v, gcapt(j,v,r,t))) ;

* * * * * Static constraints
exlife_static(ivrt(i,v,r,t))..
    XC(i,v,r,t) =l= capt(i,v,r,t) * lifetime(i,v,r,t) ;
gexlife_static(jvrt(j,v,r,t))..
    GC(j,v,r,t) =l= gcapt(j,v,r,t) * glifetime(j,v,r,t) ;
    
gcexlife_static(jvrt(j,v,r,t))..
    GCC(j,v,r,t) =e= gccapt_static(j,v,r,t) ;
gdexlife_static(jvrt(j,v,r,t))..
    GCD(j,v,r,t) =e= gdcapt_static(j,v,r,t) ;   
grexlife_static(jvrt(j,v,r,t))..
    GCR(j,v,r,t) =e= grcapt_static(j,v,r,t) ;

gexlifenv_static(nvj(j),r,t)..
    GCNV(j,r,t) =l= sum(jvrt(j,oldv(v),r,t), gcapt(j,v,r,t) * glifetime(j,v,r,t)) ;
    
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
co2price_model(s,r,t)
co2price_model_ave(r,t)
;

*$onecho >temp\gdxxrw.rsp
*par=static2022           rng=2022!a1                rdim=1 cdim=1
*par=static2023           rng=2023!a1                rdim=1 cdim=1
*$offecho

*$call 'gdxxrw i=database\data\static_data.xlsx o=database\data_static\par_static.gdx trace=3 log=database\temp\par_static.log @temp\gdxxrw.rsp';

$gdxin database\data_static\par_static
$load static2022,static2023
$gdxin

set
tonly2023(t)
;

tonly2023(t)$(sameas(t,"2023")) = yes ;

co2p_static(s,t2022(t)) = sum(srep(s,h), sum(maphd(h,d), static2022(d,"CO2"))) ;
co2p_static(s,tonly2023(t)) = sum(srep(s,h), sum(maphd(h,d), static2022(d,"CO2"))) ;

pfuel_static(s,"Bioenergy",r,t2022(t)) = pfuel("Bioenergy",r,t);
pfuel_static(s,"Bioenergy",r,tonly2023(t)) = pfuel("Bioenergy",r,t) ;

pfuel_static(s,"Gas",r,t2022(t)) = sum(srep(s,h), sum(maphd(h,d), static2022(d,"Gas"))) ;
pfuel_static(s,"Gas",r,tonly2023(t)) = sum(srep(s,h), sum(maphd(h,d), static2023(d,"Gas"))) ;

pfuel_static(s,"Coal",r,t2022(t)) = sum(srep(s,h), sum(maphd(h,d), static2022(d,"Coal"))) ;
pfuel_static(s,"Coal",r,tonly2023(t)) = sum(srep(s,h), sum(maphd(h,d), static2023(d,"Coal"))) ;

pfuel_static(s,"Oil",r,t2022(t)) = sum(srep(s,h), sum(maphd(h,d), static2022(d,"Oil"))) ;
pfuel_static(s,"Oil",r,tonly2023(t)) = sum(srep(s,h), sum(maphd(h,d), static2023(d,"Oil"))) ;

pfuel_static(s,"Uranium",r,t2022(t)) = sum(srep(s,h), sum(maphd(h,d), static2022(d,"Uranium"))) ;
pfuel_static(s,"Uranium",r,tonly2023(t)) = sum(srep(s,h), sum(maphd(h,d), static2023(d,"Uranium"))) ;

co2p_static_ave(t2022(t)) = 81.48 ;
co2p_static_ave(tonly2023(t)) = 89.38 ;

$if not  set co2priceref    co2prrefreg_static(r,t2022(t)) = 81.48 ;
$if not  set co2priceref    co2prrefreg_static(r,tonly2023(t)) = 89.38 ;

co2price_model(s,reu(r),t) = co2p_static(s,t) * co2prreg_static(r,t) / co2prrefreg_static(r,t) ;
co2price_model(s,rbr(r),t) = co2prreg_static(r,t) ;
co2price_model_ave(r,t) = sum(s, co2price_model(s,r,t)) / 8760 ;

* * * Dispatch cost
parameter
discost_hours(s,i,v,r,t)
;

discost_hours(s,ivrt(i,v,r,topt2023(t))) = 
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
$if      set co2price    + emit(i,v,r) * co2price_model(s,r,t)
*        CCS costs (from capturing)
$if      set ccs         + round(co2captured(i,v,r) * ccscost(r,t), 8)
$if      set nochp       + 60 ;
;

discost_hours(s,ivrt(i,v,r,topt2024(t))) = 
                           discost(i,v,r,t)
$if      set co2price    + emit(i,v,r) * co2prreg_static(r,t)
$if      set nochp       + 60 ;
;
