* * * Ukraine Russian war investment lag module (should work for both shortrun and not)
parameter
ixfx(i,r,t)
itfx(k,r,r,t)
igfx(j,r,t)

ixfx_plus(i,r,t)
ixfx_plus2(i,r,t)
;

$if not  set fromd  $gdxin limits\%l%_newcap_bauprice.gdx
$if      set fromd  $gdxin limits_days\%l%_newcap_bauprice.gdx
$load ixfx, itfx, igfx
$gdxin

ixfx_plus(irnw_nohydro(i),r,topt2024_2030(t)) = ixfx(i,r,t-1) ;
ixfx_plus2(irnw_nohydro(i),r,topt2024_2030(t)) = ixfx(i,r,t-2) ;

ixfx_plus(open(i),r,"2024")  = ixfx("OpenPV_exi",r,"2023")/5 + ixfx(i,r,"2023") ;
ixfx_plus2(open(i),r,"2024") = ixfx("OpenPV_exi",r,"2022")/5 + ixfx(i,r,"2022") ;
ixfx_plus2(open(i),r,"2025") = ixfx("OpenPV_exi",r,"2023")/5 + ixfx(i,r,"2023") ;

ixfx_plus(roof(i),r,"2024")  = ixfx("RoofPV_exi",r,"2023")/5 + ixfx(i,r,"2023") ;
ixfx_plus2(roof(i),r,"2024") = ixfx("RoofPV_exi",r,"2022")/5 + ixfx(i,r,"2022") ;
ixfx_plus2(roof(i),r,"2025") = ixfx("RoofPV_exi",r,"2023")/5 + ixfx(i,r,"2023") ;

ixfx_plus(WindOn(i),r,"2024")  = ixfx("WindOn_exi",r,"2023")/5 + ixfx(i,r,"2023") ;
ixfx_plus2(WindOn(i),r,"2024") = ixfx("WindOn_exi",r,"2022")/5 + ixfx(i,r,"2022") ;
ixfx_plus2(WindOn(i),r,"2025") = ixfx("WindOn_exi",r,"2023")/5 + ixfx(i,r,"2023") ;

ixfx_plus(WindOff(i),r,"2024")  = ixfx("WindOff_exi",r,"2023")/5 + ixfx(i,r,"2023") ;
ixfx_plus2(WindOff(i),r,"2024") = ixfx("WindOff_exi",r,"2022")/5 + ixfx(i,r,"2022") ;
ixfx_plus2(WindOff(i),r,"2025") = ixfx("WindOff_exi",r,"2023")/5 + ixfx(i,r,"2023") ;

* Investment limits depending on (not) shortrun modeling
IX.UP(conv(i),r,topt2024_2030(t))    = round(ixfx(i,r,t),4) ;
IX.UP(nuc(i),r,topt2024_2030(t))     = round(ixfx(i,r,t),4) ;

set
tlag_irnw(t) /2024,2025,2026,2027/
;

IX.UP(sol(i),r,tlag_irnw(t))     = round(max(ixfx_plus2(i,r,t),ixfx_plus(i,r,t),ixfx(i,r,t)) + 5 * daref(r,"2022") / daref("Germany","2022"),4) ;
IX.UP(windon(i),r,tlag_irnw(t))  = round(max(ixfx_plus2(i,r,t),ixfx_plus(i,r,t),ixfx(i,r,t)) + 3 * daref(r,"2022") / daref("Germany","2022"),4) ;
IX.UP(windoff(i),r,tlag_irnw(t)) = round(max(ixfx_plus2(i,r,t),ixfx_plus(i,r,t),ixfx(i,r,t)) + 1 * daref(r,"2022") / daref("Germany","2022"),4) ;

IT.UP(k,r,rr,topt2030(t))         = itfx(k,r,rr,t) ;
IG.UP("Storage_ST",r,t)$(t.val le 2025 and t.val ge 2024) = igfx("Storage_ST",r,t) ;
IG.UP("Storage_LT",r,t)$(t.val le 2025 and t.val ge 2024) = igfx("Storage_LT",r,t) ;

set
invset /windon,windoff,solarpv/
tinvset(t) /2025,2026,2027,2028,2029/
;

parameter
invlimit(invset,r,t)
;

invlimit("windon",r,"2024") = round(max(sum(windon(i),capt(i,"2023",r,"2023")),sum(windon(i),capt(i,"2022",r,"2022"))) + 3 * daref(r,"2022") / daref("Germany","2022"),4) ;
invlimit("windoff",r,"2024") = round(max(sum(windoff(i),capt(i,"2023",r,"2023")),sum(windoff(i),capt(i,"2022",r,"2022"))) + 1 * daref(r,"2022") / daref("Germany","2022"),4) ;
invlimit("solarpv",r,"2024") = round(max(sum(sol(i),capt(i,"2023",r,"2023")),sum(sol(i),capt(i,"2022",r,"2022"))) + 5 * daref(r,"2022") / daref("Germany","2022"),4) ;
invlimit(invset,r,"2030") = invlimit(invset,r,"2024") * 2 ;
invlimit(invset,r,tinvset(t)) = round(invlimit(invset,r,"2024") + (invlimit(invset,r,"2030") - invlimit(invset,r,"2024")) * (t.val - 2024) / (2030 - 2024),4) ;

Equations
investlimit_windon(r,t)
investlimit_windoff(r,t)
investlimit_sol(r,t)
;

investlimit_windon(r,topt2024_2030(t))..
    sum(windon(i), IX(i,r,t)) =l= invlimit("windon",r,t) ;

investlimit_windoff(r,topt2024_2030(t))..
    sum(windoff(i), IX(i,r,t)) =l= invlimit("windoff",r,t) ;
    
investlimit_sol(r,topt2024_2030(t))..
    sum(sol(i), IX(i,r,t)) =l= invlimit("solarpv",r,t) ;