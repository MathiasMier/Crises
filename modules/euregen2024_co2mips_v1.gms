* * * MIP EU ETS MSR version of the model
BINARY VARIABLE
UPP(t)                  TNAC is above 833 Mio
MID(t)                  TNAC is between 400 and 800 Mio
UPPMID(t)               TNAC is between 833 and 1096.0526 Mio
LOWMID(t)               TNAC is between 200 and 400 Mio
LOW(t)                  TNAC is below 200 Mio
MSRFULL(t)              MSR is above nyrs(t) * 100 or 200
MSREMPTY(t)             MSR is empty after outtake (below nyrs(t) * 100 or 200)
CANCELNO(t)             MSR after fillings is below true auctioning (no canceling happens)
CANCELYES(t)            MSR after fillings is above true auctioning (canceling happens)
;

POSITIVE VARIABLE
MSR(t)
CANCEL(t)
MSROUT(t)
MSROUT_INT(t)
MSROUT_MAX(t)
MSRIN(t)
;

VARIABLE
CANCEL_INT(t)
;

* * * Reduced shortrun EU ETS MSR version of the model
EQUATION
eqs_euets(t)
eqs_msr(t)
eqs_msrin2023(t)
eqs_msrin_old(t)
eqs_msrin_new(t)
eqs_msrout2023(t)
eqs_msrout_old(t)
eqs_msrout_new(t)
eqs_msrout_max(t)
eqs_msrout_fin(t)
eqs_msrout_bin(t)
eqs_msroutup(t)
eqs_msroutlo(t)
eqs_msrinout(t)
eqs_tnac(t)
eqs_tnac_bin(t)
eqs_tnacup_old(t)
eqs_tnaclo_old(t)
eqs_tnacup_new(t)
eqs_tnaclo_new(t)
eqs_cancel2023(t)
eqs_cancel_old(t)
eqs_cancel_new(t)
eqs_cancel2045(t)
eqs_cancel_fin(t)
eqs_cancel_bin(t)
eqs_cancelup_old(t)             
eqs_cancello_old(t)
eqs_cancelup_new(t)             
eqs_cancello_new(t)
;

eqs_euets(t)$(toptimize(t))..                                                 ECEU(t) * (1 + co2elecind(t)) + co2indorgfix_in(t) + co2can_in(t) + co2avi_in(t) + co2shi_in(t)
                                                                                                        =e= co2add_in(t) + co2allocated_in(t) + (co2auctioned_in(t) - MSRIN(t) + MSROUT(t)) + TNACUSE(t)
$if      set hedgeeua                                                                                        - co2out_in(t)
                                                                            ;
eqs_msr(t)$(t.val ge 2022 and toptimize(t)
$if not  set noets2050   and t.val le 2050 
$if      set noets2050   and t.val le 2045 
        )..                                                                 MSR(t)                      =e= msrstart_in  + sum(tt$(tt.val le t.val and tt.val ge 2022), (MSRIN(tt) - MSROUT(tt) - CANCEL(tt)) * nyrs(tt)) ;                                                                            
eqs_msrin2023(t)$(t.val ge 2023 and t.val le 2023 and toptimize(t))..         MSRIN(t)                    =e= UPP(t-1) * 0.24 * TNAC(t-1) + UPPMID(t-1) * (TNAC(t-1) - 833) * 200.16 ;
eqs_msrin_old(t)$(t.val ge 2024 and toptimize(t)   
$if not  set noets2050   and t.val le 2050 
$if      set noets2050   and t.val le 2045 
        )..                                                                 MSRIN(t)                    =e= UPP(t-1) * 0.12 * TNAC(t-1) + UPPMID(t-1) * (TNAC(t-1) - 833) * 100.08 ;
eqs_msrin_new(t)$(t.val ge 2024 and toptimize(t)   
$if not  set noets2050   and t.val le 2050 
$if      set noets2050   and t.val le 2045 
        )..                                                                 MSRIN(t)                    =e= UPP(t-1) * 0.24 * TNAC(t-1) + UPPMID(t-1) * (TNAC(t-1) - 833) ;
eqs_msrout2023(t)$(t.val ge 2023 and t.val le 2023 and toptimize(t))..        MSROUT_INT(t)               =e= LOW(t-1) * 200 + LOWMID(t-1) * 200 * (400 - TNAC(t-1)) ;
eqs_msrout_old(t)$(t.val ge 2024 and toptimize(t)
$if not  set noets2050   and t.val le 2050 
$if      set noets2050   and t.val le 2045 
        )..                                                                 MSROUT_INT(t)               =e= LOW(t-1) * 100 + LOWMID(t-1) * 100 * (400 - TNAC(t-1)) ;
eqs_msrout_new(t)$(t.val ge 2024 and toptimize(t)
$if not  set noets2050   and t.val le 2050 
$if      set noets2050   and t.val le 2045 
        )..                                                                 MSROUT_INT(t)               =e= LOW(t-1) * 200 + LOWMID(t-1) * 200 * (400 - TNAC(t-1)) ;
eqs_msrout_max(t)$(t.val ge 2023 and toptimize(t)
$if not  set noets2050   and t.val le 2050 
$if      set noets2050   and t.val le 2045 
        )..                                                                 MSROUT_MAX(t)               =e= (LOW(t-1) + LOWMID(t-1)) * MSR(t-1) / nyrs(t) ;
eqs_msrout_bin(t)$(t.val ge 2023 and toptimize(t)
$if not  set noets2050   and t.val le 2050 
$if      set noets2050   and t.val le 2045 
        )..                                                                 MSRFULL(t) + MSREMPTY(t)    =e= 1 ;
eqs_msrout_fin(t)$(t.val ge 2023 and toptimize(t)
$if not  set noets2050   and t.val le 2050 
$if      set noets2050   and t.val le 2045 
        )..                                                                 MSROUT(t)                   =e= MSRFULL(t-1) * MSROUT_INT(t) + MSREMPTY(t-1) * MSROUT_MAX(t) ;
eqs_msroutup(t)$(t.val ge 2023 and toptimize(t)
$if not  set noets2050   and t.val le 2050 
$if      set noets2050   and t.val le 2045 
        )..                                                                 MSR(t-1)                    =g=                                           MSRFULL(t-1) * MSROUT_INT(t) * nyrs(t)  ;             
eqs_msroutlo(t)$(t.val ge 2023 and toptimize(t)
$if not  set noets2050   and t.val le 2050 
$if      set noets2050   and t.val le 2045 
        )..                                                                 MSR(t-1)                    =l= MSREMPTY(t-1) * MSROUT_INT(t) * nyrs(t) + MSRFULL(t-1) * 5000 ;

eqs_msrinout(t)$(t.val ge 2023 and toptimize(t)
$if not  set noets2050   and t.val le 2050 
$if      set noets2050   and t.val le 2045 
        )..                                                                 MSRIN(t) * (LOW(t) + LOWMID(t))                  =e= MSROUT(t) * (UPP(t) + UPPMID(t)) ;

eqs_tnac(t)$(t.val ge 2022 and toptimize(t)
$if not  set noets2050   and t.val le 2050 
$if      set noets2050   and t.val le 2045 
        )..                                                                 TNAC(t)                     =e= tnacstart_in - sum(tt$(tt.val le t.val and tt.val ge 2022), TNACUSE(tt) * nyrs(tt)) ;
eqs_tnac_bin(t)$(t.val ge 2022 and toptimize(t)
$if not  set noets2050   and t.val le 2045 
$if      set noets2050   and t.val le 2040 
        )..                                                                 LOW(t) + LOWMID(t) + UPP(t) + UPPMID(t) + MID(t) =e= 1 ;
eqs_tnacup_old(t)$(t.val ge 2022 and toptimize(t)
$if not  set noets2050   and t.val le 2045 
$if      set noets2050   and t.val le 2040 
        )..                                                                 TNAC(t)                     =g=                LOWMID(t) * 399 + MID(t) * 400 + UPPMID(t) *  833 + UPP(t) *  834 ;
eqs_tnaclo_old(t)$(t.val ge 2022 and toptimize(t)
$if not  set noets2050   and t.val le 2045 
$if      set noets2050   and t.val le 2040 
        )..                                                                 TNAC(t)                     =l= LOW(t) * 399 + LOWMID(t) * 400 + MID(t) * 833 + UPPMID(t) *  834 + UPP(t) * 2000 ;
eqs_tnacup_new(t)$(t.val ge 2022 and toptimize(t)
$if not  set noets2050   and t.val le 2045 
$if      set noets2050   and t.val le 2040 
        )..                                                                 TNAC(t)                     =g=                LOWMID(t) * 399 + MID(t) * 400 + UPPMID(t) *  833 + UPP(t) * 1096.0526 ;
eqs_tnaclo_new(t)$(t.val ge 2022 and toptimize(t)
$if not  set noets2050   and t.val le 2045 
$if      set noets2050   and t.val le 2040 
        )..                                                                 TNAC(t)                     =l= LOW(t) * 399 + LOWMID(t) * 400 + MID(t) * 833 + UPPMID(t) * 1096.0526 + UPP(t) * 2000 ;
eqs_cancel2023(t)$(t.val ge 2023 and t.val le 2023 and toptimize(t))..        CANCEL_INT(t)               =e= MSR(t-1) + MSRIN(t) - MSROUT(t) - (co2auctioned_in(t-1) - MSRIN(t-1) + MSROUT(t-1)) ;
eqs_cancel_old(t)$(t.val ge 2024 and toptimize(t)
$if not  set noets2050   and t.val le 2045 
$if      set noets2050   and t.val le 2040 
        )..                                                                 CANCEL_INT(t)               =e= MSR(t-1) + MSRIN(t) - MSROUT(t) - (co2auctioned_in(t-1) - MSRIN(t-1) + MSROUT(t-1)) ;
eqs_cancel_new(t)$(t.val ge 2024 and toptimize(t)
$if not  set noets2050   and t.val le 2045 
$if      set noets2050   and t.val le 2040 
        )..                                                                 CANCEL_INT(t)               =e= MSR(t-1) + MSRIN(t) - MSROUT(t) - 400 ;
eqs_cancel2045(t)$(toptimize(t)
$if not  set noets2050   and sameas(t,"2050")
$if      set noets2050   and sameas(t,"2050")
        )..                                                                 CANCEL(t)                   =e= MSR(t-1) + MSRIN(t) - MSROUT(t) - 0 ;

eqs_cancel_fin(t)$(t.val ge 2023 and toptimize(t)
$if not  set noets2050   and t.val le 2045 
$if      set noets2050   and t.val le 2040 
        )..                                                                 CANCEL(t)                   =e= CANCEL_INT(t) * CANCELYES(t) ;
eqs_cancel_bin(t)$(t.val ge 2023 and toptimize(t)
$if not  set noets2050   and t.val le 2045 
$if      set noets2050   and t.val le 2040 
        )..                                                                 CANCELNO(t) + CANCELYES(t)  =e= 1 ;
eqs_cancelup_old(t)$(t.val ge 2023 and toptimize(t)
$if not  set noets2050   and t.val le 2045 
$if      set noets2050   and t.val le 2040 
        )..                                                                 MSR(t-1) + MSRIN(t) - MSROUT(t) =g=                                                                   CANCELYES(t) * (co2auctioned_in(t-1) - MSRIN(t-1) + MSROUT(t-1)) ;             
eqs_cancello_old(t)$(t.val ge 2023 and toptimize(t)
$if not  set noets2050   and t.val le 2045 
$if      set noets2050   and t.val le 2040 
        )..                                                                 MSR(t-1) + MSRIN(t) - MSROUT(t) =l= CANCELNO(t) * (co2auctioned_in(t-1) - MSRIN(t-1) + MSROUT(t-1)) + CANCELYES(t) * 5000 ;
eqs_cancelup_new(t)$(t.val ge 2023 and toptimize(t)
$if not  set noets2050   and t.val le 2045 
$if      set noets2050   and t.val le 2040 
        )..                                                                 MSR(t-1) + MSRIN(t) - MSROUT(t) =g=                     CANCELYES(t) * 400 ;             
eqs_cancello_new(t)$(t.val ge 2023 and toptimize(t)
$if not  set noets2050   and t.val le 2045 
$if      set noets2050   and t.val le 2040 
        )..                                                                 MSR(t-1) + MSRIN(t) - MSROUT(t) =l= CANCELNO(t) * 400 + CANCELYES(t) * 5000 ;


Parameter
cancel_in(t)
;

cancel_in(t)$(t.val ge 2023 and t.val le 2023) = msr_in(t-1) + msrin_in(t) -  (co2auctioned_in(t-1) - msrin_in(t-1)) ;
$if      set co2mips  $if      set euetsold    cancel_in(t)$(t.val ge 2024 and t.val le 2045 and (msr_in(t-1) + msrin_in(t) - (co2auctioned_in(t-1) - msrin_in(t-1)) > 0)) = msr_in(t-1) + msrin_in(t) - (co2auctioned_in(t-1) - msrin_in(t-1)) ;
$if      set co2mips  $if      set euetsmsrin  cancel_in(t)$(t.val ge 2024 and t.val le 2045 and (msr_in(t-1) + msrin_in(t) - (co2auctioned_in(t-1) - msrin_in(t-1)) > 0)) = msr_in(t-1) + msrin_in(t) - (co2auctioned_in(t-1) - msrin_in(t-1)) ;
$if      set co2mips  $if      set euetscancel cancel_in(t)$(t.val ge 2024 and t.val le 2045 and (msr_in(t-1) + msrin_in(t) - 400) > 0) = msr_in(t-1) + msrin_in(t) - 400 ;
$if      set co2mips  $if      set euetsnew    cancel_in(t)$(t.val ge 2024 and t.val le 2045 and (msr_in(t-1) + msrin_in(t) - 400) > 0) = msr_in(t-1) + msrin_in(t) - 400 ;
$if      set co2mips                           cancel_in(t)$(sameas(t,"2045") and (msr_in(t-1) + msrin_in(t) - 0 > 0))                  = msr_in(t-1) + msrin_in(t) - 0 ;

* Define priors
$if      set co2mips  ECEU.L(t) = co2ele_in(t) ;
$if      set co2mips  TNACUSE.L(t)                 = tnacuse_in(t) ;
$if      set co2mips  TNAC.L(t)                    = tnac_in(t) ;
$if      set co2mips  CANCEL.L(t)                  = cancel_in(t) ;
$if      set co2mips  CANCEL_INT.L(t)              = cancel_in(t) ;
$if      set co2mips  MSRIN.L(t)$(msr_in(t) >= 0 and tnac_in(t) > 833)  =   msrin_in(t) ;
$if      set co2mips  MSRIN.L(t)$(msr_in(t) < 0)  = 0 ;
$if      set co2mips  MSROUT_INT.L(t)$(msr_in(t) >= 0)  = 0 ;
$if      set co2mips  MSROUT_INT.L(t)$(msr_in(t) < 0 and tnac_in(t) < 400)  = - msrin_in(t) ;
$if      set co2mips  MSROUT_MAX.L(t)$(tnac_in(t) < 400) = msr_in(t-1) / nyrs(t) ;
$if      set co2mips  MSROUT.L(t)$(msr_in(t) >= 0)  = 0 ;
$if      set co2mips  MSROUT.L(t)$(msr_in(t) < 0)  = - msrin_in(t) ;
$if      set co2mips  MSROUT.L(t)$(msr_in(t) >= 0)  = 0 ;
$if      set co2mips  MSROUT.L(t)$(msr_in(t) < 0)  = - msrin_in(t) ;
$if      set co2mips  LOW.L(t)$(tnac_in(t) <  360) = 1 ;
$if      set co2mips  LOW.L(t)$(tnac_in(t) >= 360) = 0 ;
$if      set co2mips  LOWMID.L(t)$(tnac_in(t) <  400 and tnac_in(t) >= 360) = 1 ;
$if      set co2mips  LOWMID.L(t)$(tnac_in(t) >= 400 or  tnac_in(t) < 360) = 0 ;
$if      set co2mips  MID.L(t)$(tnac_in(t) <= 833 and tnac_in(t) >= 400) = 1 ;
$if      set co2mips  MID.L(t)$(tnac_in(t) <  400 or  tnac_in(t) >  833) = 0 ;
$if      set co2mips  UPPMID.L(t)$(tnac_in(t) >  833 and tnac_in(t) <= 1096.0526) = 1 ;
$if      set co2mips  UPPMID.L(t)$(tnac_in(t) <= 833 or  tnac_in(t) >  1096.0526) = 0 ;
$if      set co2mips  UPP.L(t)$(tnac_in(t) >  1096.0526) = 1 ;
$if      set co2mips  UPP.L(t)$(tnac_in(t) <= 1096.0526) = 0 ;

$if      set co2mips  parameter
$if      set co2mips  msrout_in(t)
$if      set co2mips  ;

$if      set co2mips  msrout_in(t)$(msr_in(t) < 0 and tnac_in(t) < 400) = msrin_in(t) ;

$if      set co2mips  MSRFULL.L(t)$(msr_in(t-1)  >=  msrout_in(t) * nyrs(t)) = 1 ;
$if      set co2mips  MSRFULL.L(t)$(msr_in(t-1)    < msrout_in(t) * nyrs(t)) = 0 ;
$if      set co2mips  MSREMPTY.L(t)$(msr_in(t-1) >= msrout_in(t) * nyrs(t)) = 0 ;
$if      set co2mips  MSREMPTY.L(t)$(msr_in(t-1)   < msrout_in(t) * nyrs(t)) = 1 ;

$if      set co2mips  CANCELNO.L(t)$(t.val ge 2023  and t.val le 2023 and msr_in(t-1) + msrin_in(t) < co2auctioned_in(t-1) - msrin_in(t-1)) = 1 ;
$if      set co2mips  CANCELNO.L(t)$(t.val ge 2023  and t.val le 2023 and msr_in(t-1) + msrin_in(t) > co2auctioned_in(t-1) - msrin_in(t-1)) = 0 ;
$if      set co2mips  CANCELYES.L(t)$(t.val ge 2023 and t.val le 2023 and msr_in(t-1) + msrin_in(t) < co2auctioned_in(t-1) - msrin_in(t-1)) = 0 ;
$if      set co2mips  CANCELYES.L(t)$(t.val ge 2023 and t.val le 2023 and msr_in(t-1) + msrin_in(t) > co2auctioned_in(t-1) - msrin_in(t-1)) = 1 ;

$if not  set noets2050 $if      set co2mips  CANCELNO.L(t)$(t.val ge 2024  and t.val le 2050 and msr_in(t-1) + msrin_in(t) < co2auctioned_in(t-1) - msrin_in(t-1)) = 1 ;
$if not  set noets2050 $if      set co2mips  CANCELNO.L(t)$(t.val ge 2024  and t.val le 2050 and msr_in(t-1) + msrin_in(t) > co2auctioned_in(t-1) - msrin_in(t-1)) = 0 ;
$if not  set noets2050 $if      set co2mips  CANCELYES.L(t)$(t.val ge 2024 and t.val le 2050 and msr_in(t-1) + msrin_in(t) < co2auctioned_in(t-1) - msrin_in(t-1)) = 0 ;
$if not  set noets2050 $if      set co2mips  CANCELYES.L(t)$(t.val ge 2024 and t.val le 2050 and msr_in(t-1) + msrin_in(t) > co2auctioned_in(t-1) - msrin_in(t-1)) = 1 ;

$if not  set noets2050 $if      set co2mips  CANCELNO.L(t)$(t.val ge 2023  and t.val le 2050 and msr_in(t-1) + msrin_in(t) < co2auctioned_in(t-1) - msrin_in(t-1)) = 1 ;
$if not  set noets2050 $if      set co2mips  CANCELNO.L(t)$(t.val ge 2023  and t.val le 2050 and msr_in(t-1) + msrin_in(t) > co2auctioned_in(t-1) - msrin_in(t-1)) = 0 ;
$if not  set noets2050 $if      set co2mips  CANCELYES.L(t)$(t.val ge 2023 and t.val le 2050 and msr_in(t-1) + msrin_in(t) < co2auctioned_in(t-1) - msrin_in(t-1)) = 0 ;
$if not  set noets2050 $if      set co2mips  CANCELYES.L(t)$(t.val ge 2023 and t.val le 2050 and msr_in(t-1) + msrin_in(t) > co2auctioned_in(t-1) - msrin_in(t-1)) = 1 ;

$if      set noets2050 $if      set co2mips  CANCELNO.L(t)$(t.val ge 2024  and t.val le 2045 and msr_in(t-1) + msrin_in(t) < co2auctioned_in(t-1) - msrin_in(t-1)) = 1 ;
$if      set noets2050 $if      set co2mips  CANCELNO.L(t)$(t.val ge 2024  and t.val le 2045 and msr_in(t-1) + msrin_in(t) > co2auctioned_in(t-1) - msrin_in(t-1)) = 0 ;
$if      set noets2050 $if      set co2mips  CANCELYES.L(t)$(t.val ge 2024 and t.val le 2045 and msr_in(t-1) + msrin_in(t) < co2auctioned_in(t-1) - msrin_in(t-1)) = 0 ;
$if      set noets2050 $if      set co2mips  CANCELYES.L(t)$(t.val ge 2024 and t.val le 2045 and msr_in(t-1) + msrin_in(t) > co2auctioned_in(t-1) - msrin_in(t-1)) = 1 ;

$if      set noets2050 $if      set co2mips  CANCELNO.L(t)$(t.val ge 2023  and t.val le 2045 and msr_in(t-1) + msrin_in(t) < co2auctioned_in(t-1) - msrin_in(t-1)) = 1 ;
$if      set noets2050 $if      set co2mips  CANCELNO.L(t)$(t.val ge 2023  and t.val le 2045 and msr_in(t-1) + msrin_in(t) > co2auctioned_in(t-1) - msrin_in(t-1)) = 0 ;
$if      set noets2050 $if      set co2mips  CANCELYES.L(t)$(t.val ge 2023 and t.val le 2045 and msr_in(t-1) + msrin_in(t) < co2auctioned_in(t-1) - msrin_in(t-1)) = 0 ;
$if      set noets2050 $if      set co2mips  CANCELYES.L(t)$(t.val ge 2023 and t.val le 2045 and msr_in(t-1) + msrin_in(t) > co2auctioned_in(t-1) - msrin_in(t-1)) = 1 ;

* Fix variables
$if      set co2mips  MSRIN.FX("2020") = msrin_in("2020") ;
$if      set co2mips  MSRIN.FX("2021") = msrin_in("2021") ;
$if      set co2mips  MSRIN.FX("2022") = msrin_in("2022") ;
$if      set co2mips  MSRIN.FX("2050") = 0 ;
$if      set co2mips  MSR.FX("2020") = msr_in("2020") ;
$if      set co2mips  MSR.FX("2021") = msr_in("2021") ;
$if      set co2mips  MSR.FX("2050") = 0 ;
$if      set co2mips  MSROUT.FX("2020") = 0 ;
$if      set co2mips  MSROUT.FX("2021") = 0 ;
$if      set co2mips  MSROUT.FX("2022") = 0 ;
$if      set co2mips  MSROUT.FX("2050") = 0 ;
$if      set co2mips  LOW.FX("2020") = 0 ;
$if      set co2mips  LOW.FX("2021") = 0 ;
$if      set co2mips  LOWMID.FX("2020") = 0 ;
$if      set co2mips  LOWMID.FX("2021") = 0 ;
$if      set co2mips  MID.FX("2020") = 0 ;
$if      set co2mips  MID.FX("2021") = 0 ;
$if      set co2mips  UPPMID.FX("2020") = 0 ;
$if      set co2mips  UPPMID.FX("2021") = 0 ;
$if      set co2mips  UPP.FX("2020") = 1 ;
$if      set co2mips  UPP.FX("2021") = 1 ;
$if      set co2mips  CANCEL.FX("2020") = 0 ;
$if      set co2mips  CANCEL.FX("2021") = 0 ;
$if      set co2mips  CANCEL.FX("2022") = 0 ;
*$if      set co2mips  CANCEL.FX("2050") = 0 ;

$if      set co2mips  CANCELYES.FX("2022") = 0 ;
$if      set co2mips  CANCELNO.FX("2022") = 1 ;
*$if      set co2mips  CANCELYES.FX("2050") = 0 ;
*$if      set co2mips  CANCELNO.FX("2050") = 1 ;

$if      set co2mips  MSRFULL.FX("2022") = 1 ;
$if      set co2mips  MSREMPTY.FX("2022") = 0 ;
*$if      set co2mips  MSRFULL.FX("2045") = 0 ;
*$if      set co2mips  MSREMPTY.FX("2045") = 1 ;
*$if      set co2mips  MSRFULL.FX("2050") = 0 ;
*$if      set co2mips  MSREMPTY.FX("2050") = 1 ;
;