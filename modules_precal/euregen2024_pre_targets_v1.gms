* * * Targets equations
* French nuclear
parameter
frnuc_gen_share(r,t)
;

frnuc_gen_share(r,t) = 0 ;
$if      set frnuc10     frnuc_gen_share("France",t) = 0.1 ;
$if      set frnuc20     frnuc_gen_share("France",t) = 0.2 ;
$if      set frnuc30     frnuc_gen_share("France",t) = 0.3 ;
$if      set frnuc40     frnuc_gen_share("France",t) = 0.4 ;
$if      set frnuc50     frnuc_gen_share("France",t) = 0.5 ;
$if      set frnuc60     frnuc_gen_share("France",t) = 0.6 ;
$if      set frnuc70     frnuc_gen_share("France",t) = 0.7 ;
$if      set frnuc80     frnuc_gen_share("France",t) = 0.8 ;
$if      set frnuc90     frnuc_gen_share("France",t) = 0.9 ;
$if      set frnuc100    frnuc_gen_share("France",t) = 1.0 ;

* Renewable targets
set
techtarget
maptechtarget(techtarget,i)
mapsuperirnw(techtarget,superirnw)
;

*$onecho >temp\gdxxrw.rsp
*set=techtarget     rng=techtarget!a1    rdim=1 cdim=0 values=nodata
*set=maptechtarget  rng=maptechtarget!a1 rdim=2 cdim=0 values=nodata
*set=mapsuperirnw   rng=mapsuperirnw!a1  rdim=2 cdim=0 values=nodata
*$offecho

*$call 'gdxxrw i=restarget\restarget_%hor%.xlsx o=restarget\set_restarget_%hor%.gdx trace=3 log=temp\set_restarget_%hor%.log @temp\gdxxrw.rsp';

$gdxin restarget\set_restarget_%hor%
$load techtarget
$load maptechtarget
$load mapsuperirnw
$gdxIn

parameter
sha_gen_constant(r,t)
sha_gen_extra(r,t)
sha_con_constant(r,t)
sha_con_extra(r,t)
sha_gen_constant_int(r,t)
sha_gen_extra_int(r,t)
sha_con_constant_int(r,t)
sha_con_extra_int(r,t)
tot_con_constant(r,t)
tot_con_extra(r,t)
cap_constant(r,techtarget,t)
cap_constant_int(r,techtarget,t)
cap_extra(r,techtarget,t)
cap_extra_int(r,techtarget,t)
eng_constant(r,techtarget,t)
eng_constant_int(r,techtarget,t)
eng_extra(r,techtarget,t)
eng_extra_int(r,techtarget,t)
cap_constant_diff(r,techtarget)
cap_extra_diff(r,techtarget)
;

*$onecho >temp\gdxxrw.rsp
*par=sha_gen_constant     rng=sha_gen_constant!a1    rdim=1 cdim=1
*par=sha_gen_extra        rng=sha_gen_extra!a1       rdim=1 cdim=1
*par=sha_con_constant     rng=sha_con_constant!a1    rdim=1 cdim=1
*par=sha_con_extra        rng=sha_con_extra!a1       rdim=1 cdim=1
*par=cap_constant         rng=cap_constant!a1        rdim=2 cdim=1
*par=cap_extra            rng=cap_extra!a1           rdim=2 cdim=1
*par=eng_constant         rng=eng_constant!a1        rdim=2 cdim=1
*par=eng_extra            rng=eng_extra!a1           rdim=2 cdim=1
*$offecho

*$call 'gdxxrw i=restarget\restarget_%hor%.xlsx o=restarget\par_restarget_%hor%.gdx trace=3 log=temp\par_restarget_%hor%.log @temp\gdxxrw.rsp';

$gdxin restarget\par_restarget_%hor%
$load sha_gen_constant_int=sha_gen_constant
$load sha_gen_extra_int=sha_gen_extra
$load sha_con_constant_int=sha_con_constant
$load sha_con_extra_int=sha_con_extra
$load cap_constant_int=cap_constant
$load cap_extra_int=cap_extra
$load eng_constant_int=eng_constant
$load eng_extra_int=eng_extra
$gdxin

sha_gen_constant(r,t) = round(sha_gen_constant_int(r,t),4) ;
sha_gen_extra(r,t) = round(sha_gen_extra_int(r,t),4) ;
sha_con_constant(r,t) = round(sha_con_constant_int(r,t),4) ;
sha_con_extra(r,t)= round(sha_con_extra_int(r,t),4) ;

cap_constant(r,techtarget,t) = round(cap_constant_int(r,techtarget,t),4) ;
cap_extra(r,techtarget,t) = round(cap_extra_int(r,techtarget,t),4) ;
eng_constant(r,techtarget,t) = round(eng_constant_int(r,techtarget,t),4) ;
eng_extra(r,techtarget,t)= round(eng_extra_int(r,techtarget,t),4) ;

* Translate energy targets into capacity ones when merging irnw
$if      set mergeirnw   cap_constant_int(r,techtarget,t)$(t.val le 2023 and sameas(techtarget,"WindOn") and irnwflh_exi_h("WindOn_exi",r,t) > 0) = max(cap_constant(r,techtarget,t), eng_constant(r,techtarget,t) * 1e+3 / irnwflh_exi_h("WindOn_exi",r,t)) ;
$if      set mergeirnw   cap_constant_int(r,techtarget,t)$(t.val le 2023 and sameas(techtarget,"WindOff") and irnwflh_exi_h("WindOff_exi",r,t) > 0) = max(cap_constant(r,techtarget,t), eng_constant(r,techtarget,t) * 1e+3 / irnwflh_exi_h("WindOff_exi",r,t)) ;
$if      set mergeirnw   cap_constant_int(r,techtarget,t)$(t.val le 2023 and sameas(techtarget,"SolarPV") and irnwflh_exi_h("OpenPV_exi",r,t) > 0) = max(cap_constant(r,techtarget,t), eng_constant(r,techtarget,t) * 1e+3 / irnwflh_exi_h("OpenPV_exi",r,t)) ;
$if      set mergeirnw   cap_constant_int(r,techtarget,t)$(t.val ge 2024 and sameas(techtarget,"WindOn") and sum(newv(v)$(v.val le t.val), irnwflh_h("WindOn_q90",v,r)) > 0) = max(cap_constant(r,techtarget,t), eng_constant(r,techtarget,t) * 1e+3 / sum(newv(v)$(v.val le t.val), irnwflh_h("WindOn_q90",v,r)) * sum(newv(v)$(v.val le t.val), 1$irnwflh_h("WindOn_q90",v,r))) ;
$if      set mergeirnw   cap_constant_int(r,techtarget,t)$(t.val ge 2024 and sameas(techtarget,"WindOff") and sum(newv(v)$(v.val le t.val), irnwflh_h("WindOff_q90",v,r)) > 0) = max(cap_constant(r,techtarget,t), eng_constant(r,techtarget,t) * 1e+3 / sum(newv(v)$(v.val le t.val), irnwflh_h("WindOff_q90",v,r)) * sum(newv(v)$(v.val le t.val), 1$irnwflh_h("WindOff_q90",v,r))) ;
$if      set mergeirnw   cap_constant_int(r,techtarget,t)$(t.val ge 2024 and sameas(techtarget,"SolarPV") and sum(newv(v)$(v.val le t.val), irnwflh_h("OpenPV_q90",v,r)) > 0) = max(cap_constant(r,techtarget,t), eng_constant(r,techtarget,t) * 1e+3 / sum(newv(v)$(v.val le t.val), irnwflh_h("OpenPV_q90",v,r)) * sum(newv(v)$(v.val le t.val), 1$irnwflh_h("OpenPV_q90",v,r))) ;

$if      set mergeirnw   cap_extra_int(r,techtarget,t)$(t.val le 2023 and sameas(techtarget,"WindOn") and irnwflh_exi_h("WindOn_exi",r,t) > 0) = max(cap_extra(r,techtarget,t), eng_extra(r,techtarget,t) * 1e+3 / irnwflh_exi_h("WindOn_exi",r,t)) ;
$if      set mergeirnw   cap_extra_int(r,techtarget,t)$(t.val le 2023 and sameas(techtarget,"WindOff") and irnwflh_exi_h("WindOff_exi",r,t) > 0) = max(cap_extra(r,techtarget,t), eng_extra(r,techtarget,t) * 1e+3 / irnwflh_exi_h("WindOff_exi",r,t)) ;
$if      set mergeirnw   cap_extra_int(r,techtarget,t)$(t.val le 2023 and sameas(techtarget,"SolarPV") and irnwflh_exi_h("OpenPV_exi",r,t) > 0) = max(cap_extra(r,techtarget,t), eng_extra(r,techtarget,t) * 1e+3 / irnwflh_exi_h("OpenPV_exi",r,t)) ;
$if      set mergeirnw   cap_extra_int(r,techtarget,t)$(t.val ge 2024 and sameas(techtarget,"WindOn") and sum(newv(v)$(v.val le t.val), irnwflh_h("WindOn_q90",v,r)) > 0) = max(cap_extra(r,techtarget,t), eng_extra(r,techtarget,t) * 1e+3 / sum(newv(v)$(v.val le t.val), irnwflh_h("WindOn_q90",v,r)) * sum(newv(v)$(v.val le t.val), 1$irnwflh_h("WindOn_q90",v,r))) ;
$if      set mergeirnw   cap_extra_int(r,techtarget,t)$(t.val ge 2024 and sameas(techtarget,"WindOff") and sum(newv(v)$(v.val le t.val), irnwflh_h("WindOff_q90",v,r)) > 0) = max(cap_extra(r,techtarget,t), eng_extra(r,techtarget,t) * 1e+3 / sum(newv(v)$(v.val le t.val), irnwflh_h("WindOff_q90",v,r)) * sum(newv(v)$(v.val le t.val), 1$irnwflh_h("WindOff_q90",v,r))) ;
$if      set mergeirnw   cap_extra_int(r,techtarget,t)$(t.val ge 2024 and sameas(techtarget,"SolarPV") and sum(newv(v)$(v.val le t.val), irnwflh_h("OpenPV_q90",v,r)) > 0) = max(cap_extra(r,techtarget,t), eng_extra(r,techtarget,t) * 1e+3 / sum(newv(v)$(v.val le t.val), irnwflh_h("OpenPV_q90",v,r)) * sum(newv(v)$(v.val le t.val), 1$irnwflh_h("OpenPV_q90",v,r))) ;

* Delay will not get compensated for
cap_constant_diff(r,techtarget) = max(0,cap_constant_int(r,techtarget,"2023") - sum(maptechtarget(techtarget,i), sum(oldv(v), capt(i,v,r,"2023")))) ;
cap_extra_diff(r,techtarget) = max(0,cap_extra_int(r,techtarget,"2023") - sum(maptechtarget(techtarget,i), sum(oldv(v), capt(i,v,r,"2023")))) ;

$if      set frnuctarget   $if      set constanttargets        sha_gen_constant("France",t)$(t.val ge 2035 and sha_gen_constant("France",t) + frnuc_gen_share("France",t) ge 1) = 1 - frnuc_gen_share("France",t) ;
$if      set frnuctarget   $if      set extratargets           sha_gen_extra("France",t)$(t.val ge 2035 and sha_gen_extra("France",t) + frnuc_gen_share("France",t) ge 1) = 1 - frnuc_gen_share("France",t) ;

tot_con_constant(r,t)$(sha_con_constant(r,t) > 0) = round(sha_con_constant(r,t) * daref(r,t), 4) ;
tot_con_extra(r,t)$(sha_con_extra(r,t) > 0)       = round(sha_con_extra(r,t) * daref(r,t), 4) ;

* Set generation targets equal to consumption targets (plus distribution losses) as approximation to simplify model
$if      set gentargetsdemand    tot_con_constant(r,t)$(sha_gen_constant(r,t) > 0) = round(sha_gen_constant(r,t) * daref(r,t) * (1 + lossave(r,t)),4) ;
$if      set gentargetsdemand    tot_con_extra(r,t)$(sha_gen_extra(r,t) > 0)       = round(sha_gen_extra(r,t) * daref(r,t) * (1 + lossave(r,t)),4) ;

* Correct capacity targets in line with resource potential (important for the simpmip metric with reduced potentials)
cap_constant(r,techtarget,t)$(t.val ge 2024 and cap_constant_int(r,techtarget,t) > 0 and (sameas(techtarget,"SolarPV") or sameas(techtarget,"WindOn") or sameas(techtarget,"WindOff")))
                  = min(round(cap_constant_int(r,techtarget,t)-cap_constant_diff(r,techtarget),4), sum(mapsuperirnw(techtarget,superirnw), sum(superirnw_mapq(i,quantiles,superirnw)$(not sameas(i,"qexi")), irnwlimUP_quantiles(i,r,quantiles)))) ;
cap_extra(r,techtarget,t)$(t.val ge 2024 and cap_extra_int(r,techtarget,t) > 0 and (sameas(techtarget,"SolarPV") or sameas(techtarget,"WindOn") or sameas(techtarget,"WindOff")))
                  = min(round(cap_extra_int(r,techtarget,t)-cap_extra_diff(r,techtarget),4), sum(mapsuperirnw(techtarget,superirnw), sum(superirnw_mapq(i,quantiles,superirnw)$(not sameas(i,"qexi")), irnwlimUP_quantiles(i,r,quantiles)))) ;

parameter
cap_constantann(r,techtarget,t)
cap_extraann(r,techtarget,t)
;

cap_constantann(r,techtarget,t)$(t.val ge 2024) = max(0,cap_constant(r,techtarget,t) - cap_constant(r,techtarget,t-1)) ;
cap_extraann(r,techtarget,t) = max(0,cap_extra(r,techtarget,t) - cap_extra(r,techtarget,t-1)) ;
                  
$if      set hydroexp  invlimUP("Hydro",r,t)$(toptimize(t))      = cap_extra(r,"Water",t) - cap_extra(r,"Water",t-1) ;