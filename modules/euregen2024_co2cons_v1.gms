* * * EU ETS MSR version of the model (this condition is deactivated if not "euetsmsr=yes")
PARAMETER
co2ele_int(t)
co2ind_int(t)
co2avi_int(t)
co2shi_int(t)
co2out_int(t)
co2can_int(t)
co2indfix_int(t)
co2indorgfix_int(t)
co2indshare_int(t)
co2ele_org_int(t)
co2ind_org_int(t)
co2add_int(t)
co2allocated_int(t)
co2auctioned_int(t)
tnac_int(t)
tnacuse_int(t)
tnacres_int(t)
tnacresuse_int(t)
msr_int(t)
msrin_int(t)
co2eleuk_int(t)
;

* * * * * Carbon markets
* We have four model versions: (1) simple cap w/o shortrun, (2) simple cap with banking w/o shortrun, (3) EU ETS MSR iterative w/o shortrun, and (4) EU ETS MSR MIP w/o shotrun
* * * Loadings from simulation model (simple cap uses the respective MIP module)
$onecho >temp\gdxxrw.rsp
par=co2ele_in           rng=co2ele_in!a2             rdim=1 cdim=0
par=co2ind_in           rng=co2ind_in!a2             rdim=1 cdim=0
par=co2can_in           rng=co2can_in!a2             rdim=1 cdim=0
par=co2indfix_in        rng=co2indfix_in!a2          rdim=1 cdim=0
par=co2indshare_in      rng=co2indshare_in!a2        rdim=1 cdim=0
par=co2ele_org          rng=co2ele_org!a2            rdim=1 cdim=0
par=co2ind_org          rng=co2ind_org!a2            rdim=1 cdim=0
par=co2add_in           rng=co2add_in!a2             rdim=1 cdim=0 
par=co2allocated_in     rng=co2allocated_in!a2       rdim=1 cdim=0
par=co2auctioned_in     rng=co2auctioned_in!a2       rdim=1 cdim=0
par=msr_in              rng=msr_in!a2                rdim=1 cdim=0
par=msrin_in            rng=msrin_in!a2              rdim=1 cdim=0
par=tnac_in             rng=tnac_in!a2               rdim=1 cdim=0
par=tnacuse_in          rng=tnacuse_in!a2            rdim=1 cdim=0
par=co2eleuk_in         rng=co2eleuk_in!a2           rdim=1 cdim=0
par=co2avi_in           rng=co2avi_in!a2             rdim=1 cdim=0
par=co2shi_in           rng=co2shi_in!a2             rdim=1 cdim=0
par=co2out_in           rng=co2out_in!a2             rdim=1 cdim=0
par=co2indorgfix_in     rng=co2indorgfix_in!a2       rdim=1 cdim=0
par=tnacres_in          rng=tnacres_in!a2            rdim=1 cdim=0
par=tnacresuse_in       rng=tnacresuse_in!a2         rdim=1 cdim=0
$offecho
* * Indformula routine
* Define iterative loading loop (iter 0 always loads from the "base" files)

$call 'gdxxrw i=euetsmsr2023\%p%\%s%.xlsx o=euetsmsr2023\%p%\%s%.gdx trace=3 log=temp\%p%_%s%.log @temp\gdxxrw.rsp';
$gdxin          euetsmsr2023\%p%\%s%

* * Final load
$load co2ele_int=co2ele_in
$load co2ind_int=co2ind_in
$load co2avi_int=co2avi_in
$load co2shi_int=co2shi_in
$load co2out_int=co2out_in
$load co2indfix_int=co2indfix_in
$load co2indorgfix_int=co2indorgfix_in
$load co2indshare_int=co2indshare_in
$load co2ind_org_int=co2ind_org
$load co2ele_org_int=co2ele_org
$load co2add_int=co2add_in
$load co2can_int=co2can_in
$load co2allocated_int=co2allocated_in
$load co2auctioned_int=co2auctioned_in
$load tnac_int=tnac_in
$load tnacuse_int=tnacuse_in
$load tnacres_int=tnacres_in
$load tnacresuse_int=tnacresuse_in
$load msr_int=msr_in
$load msrin_int=msrin_in
$load co2eleuk_int=co2eleuk_in
$gdxin

PARAMETER
co2ele_in(t)
co2ind_in(t)
co2avi_in(t)
co2shi_in(t)
co2out_in(t)
co2can_in(t)
co2indfix_in(t)
co2indorgfix_in(t)
co2ele_org(t)
co2ind_org(t)
co2add_in(t)
co2allocated_in(t)
co2auctioned_in(t)
tnac_in(t)
tnacuse_in(t)
msr_in(t)
msrin_in(t)
msrstart_in
tnacstart_in
co2eleuk_in(t)
co2elecind(t)
;

co2ele_in(t) = round(co2ele_int(t), 4) ;
co2ind_in(t) = round(co2ind_int(t), 4) ;
co2avi_in(t) = round(co2avi_int(t), 4) ;
co2shi_in(t) = round(co2shi_int(t), 4) ;
co2out_in(t) = round(co2out_int(t), 4) ;
co2indfix_in(t) = round(co2indfix_int(t), 4) ;
co2indorgfix_in(t) = round(co2indorgfix_int(t), 4) ;
co2elecind(t) = round(co2indshare_int(t), 4) ;
co2ele_org(t) = round(co2ele_org_int(t), 4) ;
co2ind_org(t) = round(co2ind_org_int(t), 4) ;
co2can_in(t) = round(co2can_int(t), 4) ;
co2add_in(t) = round(co2add_int(t), 4) ;
co2allocated_in(t) = round(co2allocated_int(t), 4) ;
co2auctioned_in(t) = round(co2auctioned_int(t), 4) ;
tnac_in(t) =
$if not  set hedgeeua    round(tnac_int(t), 4) ;
$if      set hedgeeua    round(tnacres_int(t), 4) ;
tnacuse_in(t) = 
$if not  set hedgeeua    round(tnacuse_int(t), 4) ;
$if      set hedgeeua    round(tnacresuse_int(t), 4) ;
msr_in(t) = round(msr_int(t), 4) ;
msrin_in(t) = round(msrin_int(t), 4) ;
msrstart_in = 
$if      set co2mips     round(msr_int("2021"), 4) ;
$if not  set co2mips     round(msr_int("2021"), 4) ;
tnacstart_in =
$if      set co2mips     round(tnac_int("2021"), 4) ;
$if not  set co2mips     round(tnac_int("2021"), 4) ;
co2eleuk_in(t) = round(co2eleuk_int(t), 4) ;

* * * Simple carbon market without MSR dynamics
PARAMETER
co2sup(t)
;

co2sup(t) = co2add_in(t) + co2allocated_in(t) + co2auctioned_in(t) - msrin_in(t) ;
 
POSITIVE VARIABLE
TNAC(t)             Cumulative banked allowances (Mt)
TNACUK(t)           Cumulative banked allowances (Mt)
;

VARIABLE
TNACUSE(t)          Allowance usage from bank in EU27 (Mt)
TNACUKUSE(t)        Allowance usage from bank in UK (Mt)
TNACR(r,t)
TNACUSER(r,t)
;

* Determine some general starting/ending variables and variable ranges
TNAC.FX("2020") = tnac_in("2020") ;
TNAC.FX("2021") = tnac_in("2021") ;
TNAC.UP(t)$(t.val ge 2035) = 833 ;
$if      set capfast     TNAC.FX(t)$(t.val ge 2040) = 0 ; 
$if      set noets2050   TNAC.FX("2045") = 0 ;
TNAC.FX("2050") = 0 ;

TNACUSE.FX("2020") = tnacuse_in("2020") ;
TNACUSE.FX("2021") = tnacuse_in("2021") ;
$if      set capfast     TNACUSE.FX(t)$(t.val ge 2045) = 0 ;
$if      set noets2050   TNACUSE.FX("2050") = 0 ;

TNACUK.FX(t)$(t.val le 2021) = 0 ;
TNACUKUSE.FX(t)$(t.val le 2021) = 0 ;

TNACUK.FX(t)$(t.val ge 2050) = 0 ;
$if      set noets2050   TNACUK.FX(t)$(t.val ge 2045) = 0 ;
$if      set noets2050   TNACUKUSE.FX(t)$(t.val ge 2050) = 0 ;

$if not  set banking     TNAC.FX(t)     = tnac_in(t) ;
$if not  set banking     TNACUSE.FX(t)  = tnacuse_in(t) ;
$if not  set banking     TNACUK.FX(t)     = 0 ;
$if not  set banking     TNACUKUSE.FX(t)  = 0 ;

TNACR.FX(r,t)$(t.val le 2030) = 0 ;
TNACUSER.FX(r,t)$(t.val le 2030) = 0 ;
TNACR.FX(r,t)$(t.val ge 2045) = 0 ;
TNACUSER.FX(r,t)$(t.val ge 2050) = 0 ;
TNACUK.FX(t)$(t.val ge 2045) = 0 ;       
TNACUKUSE.FX("2050") = 0 ;

equations
ukets(t)                Cap market for British CO2 emissions (Mt)
uktnac(t)               Total number of allowances in circulation (UK) (Mt)
;

ukets(toptimize(t))..                                              ECUK(t) =l= co2eleuk_in(t) + TNACUKUSE(t) ;
uktnac(toptimize(t))$(
$if not  set noets2050   t.val le 2050 
$if      set noets2050   t.val le 2045 
        )..
        TNACUK(t) =e= 0 - sum(tt$(tt.val le t.val and tt.val ge 2022), TNACUKUSE(tt) * nyrs(tt)) ;
 
TNACUK.FX(t)$(t.val ge 2045) = 0 ;       
TNACUKUSE.FX("2050") = 0 ;

equation
eqs_co2real2022(t)
eqs_co2mono(t)
;

parameter
co2diff(t)
;

co2diff(t)$(t.val ge 2021 and toptimize(t) and (co2ele_in(t) - co2ele_in(t-1) >= 0)) = co2ele_in(t) - co2ele_in(t-1) ;
co2diff(t)$(t.val ge 2021 and toptimize(t) and (co2ele_in(t) - co2ele_in(t-1) < 0)) = 0 ;

eqs_co2real2022(tcri(t))..
                        ECEU(t) =l= co2ele_in(t) ;      
                     
eqs_co2mono(toptimize(t))..
    ECEU(t) =l= ECEU(t-1) ;