parameter
euets_rpt(t,*)
;

euets_rpt(t,"CO2-ele") = ECEU.L(t) ;
euets_rpt(t,"CO2-ind") = ECEU.L(t) * co2elecind(t) + co2indorgfix_in(t) ;
euets_rpt(t,"CO2-shi") = co2shi_in(t) ;
euets_rpt(t,"CO2-avi") = co2avi_in(t) ;
euets_rpt(t,"CO2-tot") = ECEU.L(t) * (1 + co2elecind(t)) + co2indorgfix_in(t) + co2shi_in(t) + co2avi_in(t) ;
euets_rpt(t,"CO2-sup") = co2add_in(t) + co2allocated_in(t) + co2auctioned_in(t) ;
euets_rpt(t,"CO2-sup-true") = co2add_in(t) + co2allocated_in(t) + co2auctioned_in(t)
$if set co2mips             - MSRIN.L(t) + MSROUT.L(t)
$if not  set co2mips         - msrin_in(t)
                            ;
euets_rpt(t,"CO2-pri") = co2preu(t) ;
euets_rpt(t,"TNAC")    = TNAC.L(t) ;
euets_rpt(t,"TNACUSE") = TNACUSE.L(t) ;
$if set co2mips euets_rpt(t,"MSR")     = MSR.L(t) ;
$if set co2mips euets_rpt(t,"MSR-IN")  = MSRIN.L(t) - MSROUT.L(t) ;
*euets_rpt(t,"MSR-OUT") = MSROUT.L(t) ;
$if set co2mips euets_rpt(t,"CANCEL")  = CANCEL.L(t) ;
$if set co2mips euets_rpt(t,"CANCEL-ACC")  = sum(tt$(tt.val le t.val), CANCEL.L(tt)) ; 

$if set co2mips euets_rpt(t,"MSR-OUT-MAX")  = MSROUT_MAXL(t) ;
$if set co2mips euets_rpt(t,"MSR-OUT-INT")  = MSROUT_INT.L(t) ;
$if set co2mips euets_rpt(t,"MSR-FULL")     = MSRFULL.L(t) ;
$if set co2mips euets_rpt(t,"MSR-EMPTY")    = MSREMPTY.L(t) ;

$if set co2mips euets_rpt(t,"LOW")     = LOW.L(t) ;
$if set co2mips euets_rpt(t,"LOWMID")    = LOWMID.L(t) ;
$if set co2mips euets_rpt(t,"MID")     = MID.L(t) ;
$if set co2mips euets_rpt(t,"MIDHIGH")    = UPPMID.L(t) ;
$if set co2mips euets_rpt(t,"HIGH")     = UPP.L(t) ;
$if set co2mips euets_rpt(t,"CANCEL-INT")    = CANCEL_INT.L(t) ;
$if set co2mips euets_rpt(t,"CANCEL-YES")     = CANCELYES.L(t) ;
$if set co2mips euets_rpt(t,"CANCEL-NO")    = CANCELNO.L(t) ;

Emissions_total_rpt(t,"CO2ind-EUETS") = co2ind_in(t) + eps ;
Emissions_total_rpt(t,"CO2-EUETS") = sum(reu(r), co2emit(r,t)) + co2ind_in(t) + eps ;