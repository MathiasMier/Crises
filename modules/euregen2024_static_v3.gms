pparameter
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
co2prrefreg_static(r,t)
;

$if      set fromsame   $gdxin static\%sn%_%l%_%s%.gdx
$if      set from6d     $gdxin static\6d_%l%_%s%.gdx
$if      set from15d    $gdxin static\15d_%l%_%s%.gdx
$if      set from31s    $gdxin static\31s_%l%_%s%.gdx
$if      set from119s   $gdxin static\119s_%l%_%s%.gdx
$load capt_static, gcapt_static, gccapt_static, gdcapt_static, grcapt_static, gcaptnv_static, tcapt_static, co2eleeu_static, co2eleuk_static, co2prreg_static=co2prreg
$gdxin

$if      set co2priceref    $if      set fromsame   $gdxin static\%sn%_%l%_high_str_frn_hyd_loa.gdx
$if      set co2priceref    $if      set from6d     $gdxin static\6d_%l%_high_str_frn_hyd_loa.gdx
$if      set co2priceref    $if      set from15d    $gdxin static\15d_%l%_high_str_frn_hyd_loa.gdx
$if      set co2priceref    $if      set from31s    $gdxin static\31s_%l%_high_str_frn_hyd_loa.gdx
$if      set co2priceref    $if      set from119s   $gdxin static\119s_%l%_high_str_frn_hyd_loa.gdx
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

parameter
co2p_static_ave(t)
;

co2p_static_ave(t2022(t)) = 81.48 ;
co2p_static_ave(t2023(t)) = 89.38 ;

$if not  set co2priceref    co2prrefreg_static(r,t2022(t)) = 81.48 ;
$if not  set co2priceref    co2prrefreg_static(r,t2023(t)) = 89.38 ;

* * * Dispatch cost
parameter
discost_static(i,v,r,t)  
;

discost_static(ivrt(i,v,r,topt2023(t))) = 
                           discost(i,v,r,t)
$if      set co2price    + emit(i,v,r) * co2p_static_ave(t) * co2prreg_static(r,t) / co2prrefreg_static(r,t)
;

discost_static(ivrt(i,v,r,topt2024(t))) = 
                           discost(i,v,r,t)
$if      set co2price    + emit(i,v,r) * co2prreg_static(r,t)
;
