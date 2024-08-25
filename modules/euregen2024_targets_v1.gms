* Renewable targets
set
techtarget
maptechtarget(techtarget,i)
;

$gdxin precal\precal_%n%.gdx
$load techtarget
$load maptechtarget
$gdxIn

parameter
sha_gen_constant(r,t)
sha_gen_extra(r,t)
sha_con_constant(r,t)
sha_con_extra(r,t)
tot_con_constant(r,t)
tot_con_extra(r,t)
cap_constant(r,techtarget,t)
cap_extra(r,techtarget,t)
eng_constant(r,techtarget,t)
eng_extra(r,techtarget,t)
cap_constantann(r,techtarget,t)
cap_extraann(r,techtarget,t)
;

$gdxin precal\precal_%n%.gdx
$load sha_gen_constant
$load sha_gen_extra
$load sha_con_constant
$load sha_con_extra
$load tot_con_constant
$load tot_con_extra
$load cap_constant
$load cap_extra
$load eng_constant
$load eng_extra
$load cap_constantann
$load cap_extraann
$gdxin

equation
resmarket_gen(r,t) equation that ensures renewable generation SHARE target compared to total generation
resmarket_con(r,t) equation that ensures renewable generation SHARE target compared to total demand
capmarket(techtarget,r,t) equation that ensures renewable cap target (GW)
engmarket(techtarget,r,t) equation that ensures renewable energy target (TWh)
;

resmarket_gen(r,t)$(t.val ge 2024 and toptimize(t)
$if      set constanttargets        and sha_gen_constant(r,t)
$if      set extratargets           and sha_gen_extra(r,t)
*$if not  set resmarket   $if      set euetsbreak      and t.val ge 2031              
    )..
    sum(ivrt(rnw(i),v,r,t)$(not irnw(i)), XTWH(i,v,r,t)) + XTWHIRNW(r,t) *
$if not  set days                   sum(s, hours(s) *         GDNV(s,"Reservoir",r,t))  * 1e-3 =g=
$if      set days                   sum(sd, days(s) * sum(hd, GDNV(s,"Reservoir",r,t))) * 1e-3 =g=
$if      set constanttargets        sha_gen_constant(r,t) * (sum(ivrt(i,v,r,t), XTWH(i,v,r,t)) + XTWHIRNW(r,t)) +
$if      set extratargets           sha_gen_extra(r,t) * (sum(ivrt(i,v,r,t), XTWH(i,v,r,t)) + XTWHIRNW(r,t)) +
        0 ;
 
resmarket_con(r,t)$(t.val ge 2024 and toptimize(t)
$if      set constanttargets        and tot_con_constant(r,t)
$if      set extratargets           and tot_con_extra(r,t)
*$if not  set resmarket   $if      set euetsbreak      and t.val ge 2031
    )..
    sum(ivrt(rnw(i),v,r,t)$(not irnw(i)), XTWH(i,v,r,t)) + XTWHIRNW(r,t) + 
$if not  set days                   sum(s, hours(s) *         GDNV(s,"Reservoir",r,t))  * 1e-3 =g=
$if      set days                   sum(sd, days(s) * sum(hd, GDNV(s,"Reservoir",r,t))) * 1e-3 =g=
$if      set constanttargets        tot_con_constant(r,t) +
$if      set extratargets           tot_con_extra(r,t) +
        0 ;

capmarket(techtarget,r,t)$(toptimize(t) and not sameas(techtarget,"Water")
$if not  set uselimits             and t.val ge 2024
$if      set uselimits             and t.val ge 2026   
$if      set constanttargets        and cap_constant(r,techtarget,t)
$if      set extratargets           and cap_extra(r,techtarget,t)
*$if not  set capmarket   $if      set euetsbreak      and t.val ge 2031
    )..
*    sum(maptechtarget(techtarget,i), sum(ivrt(i,oldv(v),r,t), capt(i,v,r,"2023"))) + sum(maptechtarget(techtarget,new(i)), sum(tt$(tt.val le t.val and tt.val ge 2024), IX(i,r,tt))) =g=
    sum(maptechtarget(techtarget,new(i)), IX(i,r,t)) =g=
*$if      set constanttargets        cap_constant(r,techtarget,t) +
*$if      set extratargets           cap_extra(r,techtarget,t) +
$if      set constanttargets        cap_constantann(r,techtarget,t) +
$if      set extratargets           cap_extraann(r,techtarget,t) +
        0 ;
        
engmarket(techtarget,r,t)$(toptimize(t) and not sameas(techtarget,"Water")
$if      set mergeirnw                 and sameas(techtarget,"Biomass") 
$if not  set uselimits             and t.val ge 2024
$if      set uselimits             and t.val ge 2026   
$if      set constanttargets        and eng_constant(r,techtarget,t)
$if      set extratargets           and eng_extra(r,techtarget,t)
*$if not  set engpmarket   $if      set euetsbreak      and t.val ge 2031
    )..
    sum(maptechtarget(techtarget,i), sum(ivrt(i,v,r,t), XTWH(i,v,r,t))) =g=
$if      set constanttargets        eng_constant(r,techtarget,t) +
$if      set extratargets           eng_extra(r,techtarget,t) +
        0 ; 0 ;