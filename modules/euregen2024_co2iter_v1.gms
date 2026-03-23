* * * Iterative "shortrun" EU ETS MSR version of the model (this module is deactived if not "co2iter=yes")
EQUATION
it_euets(t)
it_tnac(t)
;

it_euets(t)$(toptimize(t))..                                            ECEU(t) * (1 + co2elecind(t)) + co2indorgfix_in(t) + co2can_in(t) + co2avi_in(t) + co2shi_in(t)
                                                                                                        =e= co2add_in(t) + co2allocated_in(t) + (co2auctioned_in(t) - msrin_in(t)) + TNACUSE(t)
$if      set hedgeeua                                                                                        - co2out_in(t)
                                                                           ;
                                                                        
it_tnac(t)$(toptimize(t)  and t.val le 2045 and t.val ge 2022)..        TNAC(t) =e= tnacstart_in - sum(tt$(tt.val le t.val and tt.val ge 2022), TNACUSE(tt) * nyrs(tt)) ;
