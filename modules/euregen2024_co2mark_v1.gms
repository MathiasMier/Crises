
EQUATION
co2market(t)            Cap market for CO2 emissions (regional) (Mt)
co2market_r(r,t)        Cap market for CO2 emissions (regional) (Mt)
euets(t)                Cap market for CO2 emissions (system) (Mt)
eutnac(t)               Total number of allowances in circulation (system) (Mt)
tnac_r(r,t)
;

parameter
trashare(r,t)
indshare(r,t)
regshare(r,t)
co2elec_break(t)
;

trashare(r,t)= 1 ;
*= round(daref_sec(r,t,"tra") / sum(rr$(not sameas(rr,"Britain")), daref_sec(rr,t,"tra")),4) ;
indshare(r,t) = 1 ;
* = round(daref_sec(r,t,"ind") / sum(rr$(not sameas(rr,"Britain")), daref_sec(rr,t,"ind")),4) ;
regshare(r,t) = round(daref(r,t) / sum(rr$(not sameas(rr,"Britain")), daref(rr,t)),4) ;
co2cap_r(r,t) = round(regshare(r,t) * co2sup(t),4) ;

$if set fixets2030 $gdxin euetsmsr\co2mark_%pm%\co2out_basesce.gdx
$if set fixets2030 $load co2elec_break=co2elec_out
$if set fixets2030 $gdxin

$if set fixets2030 ECEU.FX(t)$(t.val ge 2024 and t.val le 2030) = co2elec_break(t) ;


co2market(t)$(toptimize(t))..             EC(t)                                                          =e= co2ele_in(t) + co2eleuk_in(t) ;
co2market_r(r,t)$(toptimize(t)
$if      set euetsbreak  and t.val ge 2031
          )..                             EC_r(r,t) + co2ind_in(t)  * indshare(r,t) + (co2avi_in(t) + co2shi_in(t)) * trashare(r,t) =l= co2cap_r(r,t) + TNACUSER(r,t) ;
          
tnac_r(r,toptimize(t))$(t.val ge 2031 and t.val le 2045
        )..
        TNACR(r,t) =e= TNAC("2030") * regshare(r,t) - sum(tt$(tt.val le t.val and tt.val ge 2031), TNACUSER(r,tt) * nyrs(tt)) ;

euets(t)$(toptimize(t))..                                            ECEU(t) * (1 + co2elecind(t)) + co2indorgfix_in(t) + co2can_in(t) + co2avi_in(t) + co2shi_in(t)
                                                                                                        =e= co2add_in(t) + co2allocated_in(t) + (co2auctioned_in(t) - msrin_in(t)) + TNACUSE(t) ;
                                     
eutnac(toptimize(t))$(
$if not  set noets2050   t.val le 2050 
$if      set noets2050   t.val le 2045
$if      set euetsbreak  t.val le 2030
        )..
        TNAC(t) =e= tnacstart_in - sum(tt$(tt.val le t.val and tt.val ge 2022), TNACUSE(tt) * nyrs(tt)) ;