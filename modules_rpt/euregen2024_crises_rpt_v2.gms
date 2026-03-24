Set
crises_set /nuc,irnw,rnw,bio,bionochp,biochp,chp,wind,sol,hydro,gas,coal,lign,oil,gasnochp,coalnochp,lignnochp,oilnochp,gaschp,coalchp,lignchp,oilchp,dirty,dirtynochp,dirtychp,dirtynolig,import,export,resv,pump,impexp,importfr,exportfr,impexpfr,hydrodiff/
;

parameter
afxc(i,v,r,t)

gen_crises(crises_set,r,t)
cap_crises(crises_set,r,t)
afcap_crises(crises_set,r,t)

sharecap_crises(t,crises_set,r)
shareafcap_crises(t,crises_set,r)
sharegen_crises(t,crises_set,r)

market_crises(crises_set,r,t)

gen_hydr_nocrises(r,t)
gen_resv_nocrises(r,t)
gen_pump_nocrises(r,t)
gen_hydr_crises(r,t)
gen_resv_crises(r,t)
gen_pump_crises(r,t)

dif_hydro(r,t)
;

$if not  set days   afxc(i,oldv(v),r,t) = sum(s, hours(s) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (vrsc(s,i,v,r)-1)$vrsc(s,i,v,r)) * (1 + (vrsc_exi(s,i,r,t)-1)$vrsc_exi(s,i,r,t))) / 8760 ;
$if      set days   afxc(i,oldv(v),r,t) = sum((sd,hd), days(sd) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (vrsc_d(sd,hd,i,v,r)-1)$vrsc_d(sd,hd,i,v,r)) * (1 + (vrsc_exi_d(sd,hd,i,r,t)-1)$vrsc_exi_d(sd,hd,i,r,t))) / 8760 ;

$gdxin precal\precal_days_28R_v3030xx_3d_shortrun_windref_average_lifetime2030_hydro66_chp_minmax_high_frn_loa
$load gen_hydr_nocrises=gen_hydr
$load gen_resv_nocrises=gen_resv
$load gen_pump_nocrises=gen_pump
$gdxin

$gdxin precal\precal_days_28R_v3030xx_3d_shortrun_windref_average_lifetime2030_hydro66_chp_minmax_high_frn_hyd_loa
$load gen_hydr_crises=gen_hydr
$load gen_resv_crises=gen_resv
$load gen_pump_crises=gen_pump
$gdxin

dif_hydro(r,t) = gen_hydr_nocrises(r,t) + gen_resv_nocrises(r,t) + gen_pump_nocrises(r,t)  
              - (gen_hydr_crises(r,t) + gen_resv_crises(r,t) + gen_pump_crises(r,t)) ;
              
cap_crises(crises_set,r,t) = eps ;
afcap_crises(crises_set,r,t) = eps ;
gen_crises(crises_set,r,t) = eps ;

cap_crises("nuc",r,t) = sum(ivrt(nuc(i),v,r,t), XC.L(i,v,r,t)) + eps ;
cap_crises("irnw",r,t) = sum(ivrt(irnw(i),v,r,t), XC.L(i,v,r,t)) + eps ;
cap_crises("rnw",r,t) = sum(ivrt(rnw(i),v,r,t), XC.L(i,v,r,t)) + eps ;
cap_crises("bio",r,t) = sum(ivrt(bio(i),v,r,t), XC.L(i,v,r,t)) + eps ;
cap_crises("chp",r,t) = sum(ivrt(chp(i),v,r,t), XC.L(i,v,r,t)) + eps ;
cap_crises("wind",r,t) = sum(ivrt(wind(i),v,r,t), XC.L(i,v,r,t)) + eps ;
cap_crises("sol",r,t) = sum(ivrt(sol(i),v,r,t), XC.L(i,v,r,t)) + eps ;
cap_crises("hydro",r,t) = sum(ivrt(hyd(i),v,r,t), XC.L(i,v,r,t)) + eps ;
cap_crises("gas",r,t) = sum(ivrt(gas(i),v,r,t), XC.L(i,v,r,t)) + eps ;
cap_crises("coal",r,t) = sum(ivrt(coa(i),v,r,t), XC.L(i,v,r,t)) + eps ;
cap_crises("lign",r,t) = sum(ivrt(lig(i),v,r,t), XC.L(i,v,r,t)) + eps ;
cap_crises("oil",r,t) = sum(ivrt(oil(i),v,r,t), XC.L(i,v,r,t)) + eps ;
cap_crises("gasnochp",r,t) = sum(ivrt_nochp(gas(i),v,r,t), XC.L(i,v,r,t)) + eps ;
cap_crises("coalnochp",r,t) = sum(ivrt_nochp(coa(i),v,r,t), XC.L(i,v,r,t)) + eps ;
cap_crises("lignnochp",r,t) = sum(ivrt_nochp(lig(i),v,r,t), XC.L(i,v,r,t)) + eps ;
cap_crises("oilnochp",r,t) = sum(ivrt_nochp(oil(i),v,r,t), XC.L(i,v,r,t)) + eps ;
cap_crises("bionochp",r,t) = sum(ivrt_nochp(bio(i),v,r,t), XC.L(i,v,r,t)) + eps ;
cap_crises("gaschp",r,t) = sum(ivrt_chp(gas(i),v,r,t), XC.L(i,v,r,t)) + eps ;
cap_crises("coalchp",r,t) = sum(ivrt_chp(coa(i),v,r,t), XC.L(i,v,r,t)) + eps ;
cap_crises("lignchp",r,t) = sum(ivrt_chp(lig(i),v,r,t), XC.L(i,v,r,t)) + eps ;
cap_crises("oilchp",r,t) = sum(ivrt_chp(oil(i),v,r,t), XC.L(i,v,r,t)) + eps ;
cap_crises("biochp",r,t) = sum(ivrt_chp(bio(i),v,r,t), XC.L(i,v,r,t)) + eps ;
cap_crises("dirty",r,t) = cap_crises("gas",r,t) + cap_crises("coal",r,t) + cap_crises("lign",r,t) + cap_crises("oil",r,t)  + eps ;
cap_crises("dirtychp",r,t) = cap_crises("chp",r,t) - cap_crises("biochp",r,t) + eps ;
cap_crises("dirtynochp",r,t) = cap_crises("dirty",r,t) - cap_crises("dirtychp",r,t) + eps ;
cap_crises("dirtynolig",r,t) = cap_crises("dirty",r,t) - cap_crises("lign",r,t) + eps ;

cap_crises("import",r,t) = ntc_r(r,t) + eps ;
cap_crises("export",r,t) = ntc_r(r,t) + eps ;
cap_crises("resv",r,t) = sum(jres(j), GCNV.L(j,r,t)) + eps ;
cap_crises("pump",r,t) = sum(jpump(j), GCNV.L(j,r,t)) + eps ;

afcap_crises("nuc",r,t) = sum(ivrt(nuc(i),v,r,t), afxc(i,v,r,t)) + eps ;
afcap_crises("irnw",r,t) = sum(ivrt(irnw(i),v,r,t), afxc(i,v,r,t)) + eps ;
afcap_crises("rnw",r,t) = sum(ivrt(rnw(i),v,r,t), afxc(i,v,r,t)) + eps ;
afcap_crises("bio",r,t) = sum(ivrt(bio(i),v,r,t), afxc(i,v,r,t)) + eps ;
afcap_crises("chp",r,t) = sum(ivrt(chp(i),v,r,t), afxc(i,v,r,t)) + eps ;
afcap_crises("wind",r,t) = sum(ivrt(wind(i),v,r,t), afxc(i,v,r,t)) + eps ;
afcap_crises("sol",r,t) = sum(ivrt(sol(i),v,r,t), afxc(i,v,r,t)) + eps ;
afcap_crises("hydro",r,t) = sum(ivrt(hyd(i),v,r,t), afxc(i,v,r,t)) + eps ;
afcap_crises("gas",r,t) = sum(ivrt(gas(i),v,r,t), afxc(i,v,r,t)) + eps ;
afcap_crises("coal",r,t) = sum(ivrt(coa(i),v,r,t), afxc(i,v,r,t)) + eps ;
afcap_crises("lign",r,t) = sum(ivrt(lig(i),v,r,t), afxc(i,v,r,t)) + eps ;
afcap_crises("oil",r,t) = sum(ivrt(oil(i),v,r,t), afxc(i,v,r,t)) + eps ;
afcap_crises("gasnochp",r,t) = sum(ivrt_nochp(gas(i),v,r,t), afxc(i,v,r,t)) + eps ;
afcap_crises("coalnochp",r,t) = sum(ivrt_nochp(coa(i),v,r,t), afxc(i,v,r,t)) + eps ;
afcap_crises("lignnochp",r,t) = sum(ivrt_nochp(lig(i),v,r,t), afxc(i,v,r,t)) + eps ;
afcap_crises("oilnochp",r,t) = sum(ivrt_nochp(oil(i),v,r,t), afxc(i,v,r,t)) + eps ;
afcap_crises("bionochp",r,t) = sum(ivrt_nochp(bio(i),v,r,t), afxc(i,v,r,t)) + eps ;
afcap_crises("gaschp",r,t) = sum(ivrt_chp(gas(i),v,r,t), afxc(i,v,r,t)) + eps ;
afcap_crises("coalchp",r,t) = sum(ivrt_chp(coa(i),v,r,t), afxc(i,v,r,t)) + eps ;
afcap_crises("lignchp",r,t) = sum(ivrt_chp(lig(i),v,r,t), afxc(i,v,r,t)) + eps ;
afcap_crises("oilchp",r,t) = sum(ivrt_chp(oil(i),v,r,t), afxc(i,v,r,t)) + eps ;
afcap_crises("biochp",r,t) = sum(ivrt_chp(bio(i),v,r,t), afxc(i,v,r,t)) + eps ;
afcap_crises("dirty",r,t) = afcap_crises("gas",r,t) + afcap_crises("coal",r,t) + afcap_crises("lign",r,t) + afcap_crises("oil",r,t)  + eps ;
afcap_crises("dirtychp",r,t) = afcap_crises("chp",r,t) - afcap_crises("biochp",r,t) + eps ;
afcap_crises("dirtynochp",r,t) = afcap_crises("dirty",r,t) - afcap_crises("dirtychp",r,t) + eps ;
afcap_crises("dirtynolig",r,t) = afcap_crises("dirty",r,t) - afcap_crises("lign",r,t) + eps ;

afcap_crises("import",r,t) = ntc_r(r,t) + eps ;
afcap_crises("export",r,t) = ntc_r(r,t) + eps ;
afcap_crises("resv",r,t) = sum(jres(j), GCNV.L(j,r,t)) + eps ;
afcap_crises("pump",r,t) = sum(jpump(j), GCNV.L(j,r,t)) + eps ;

gen_crises("nuc",r,t) = sum(ivrt(nuc(i),v,r,t), XTWHL(i,v,r,t)) + eps ;
gen_crises("irnw",r,t) = sum(ivrt(irnw(i),v,r,t), XTWHL(i,v,r,t)) + eps ;
gen_crises("rnw",r,t) = sum(ivrt(rnw(i),v,r,t), XTWHL(i,v,r,t)) + eps ;
gen_crises("bio",r,t) = sum(ivrt(bio(i),v,r,t), XTWHL(i,v,r,t)) + eps ;
gen_crises("chp",r,t) = sum(ivrt(chp(i),v,r,t), XTWHL(i,v,r,t)) + eps ;
gen_crises("wind",r,t) = sum(ivrt(wind(i),v,r,t), XTWHL(i,v,r,t)) + eps ;
gen_crises("sol",r,t) = sum(ivrt(sol(i),v,r,t), XTWHL(i,v,r,t)) + eps ;
gen_crises("hydro",r,t) = sum(ivrt(hyd(i),v,r,t), XTWHL(i,v,r,t)) + eps ;
gen_crises("gas",r,t) = sum(ivrt(gas(i),v,r,t), XTWHL(i,v,r,t)) + eps ;
gen_crises("coal",r,t) = sum(ivrt(coa(i),v,r,t), XTWHL(i,v,r,t)) + eps ;
gen_crises("lign",r,t) = sum(ivrt(lig(i),v,r,t), XTWHL(i,v,r,t)) + eps ;
gen_crises("oil",r,t) = sum(ivrt(oil(i),v,r,t), XTWHL(i,v,r,t)) + eps ;
gen_crises("gasnochp",r,t) = sum(ivrt_nochp(gas(i),v,r,t), XTWHL(i,v,r,t)) + eps ;
gen_crises("coalnochp",r,t) = sum(ivrt_nochp(coa(i),v,r,t), XTWHL(i,v,r,t)) + eps ;
gen_crises("lignnochp",r,t) = sum(ivrt_nochp(lig(i),v,r,t), XTWHL(i,v,r,t)) + eps ;
gen_crises("oilnochp",r,t) = sum(ivrt_nochp(oil(i),v,r,t), XTWHL(i,v,r,t)) + eps ;
gen_crises("bionochp",r,t) = sum(ivrt_nochp(bio(i),v,r,t), XTWHL(i,v,r,t)) + eps ;
gen_crises("gaschp",r,t) = sum(ivrt_chp(gas(i),v,r,t), XTWHL(i,v,r,t)) + eps ;
gen_crises("coalchp",r,t) = sum(ivrt_chp(coa(i),v,r,t), XTWHL(i,v,r,t)) + eps ;
gen_crises("lignchp",r,t) = sum(ivrt_chp(lig(i),v,r,t), XTWHL(i,v,r,t)) + eps ;
gen_crises("oilchp",r,t) = sum(ivrt_chp(oil(i),v,r,t), XTWHL(i,v,r,t)) + eps ;
gen_crises("biochp",r,t) = sum(ivrt_chp(bio(i),v,r,t), XTWHL(i,v,r,t)) + eps ;
gen_crises("dirty",r,t) = gen_crises("gas",r,t) + gen_crises("coal",r,t) + gen_crises("lign",r,t) + gen_crises("oil",r,t)  + eps ;
gen_crises("dirtychp",r,t) = gen_crises("chp",r,t) - gen_crises("biochp",r,t) + eps ;
gen_crises("dirtynochp",r,t) = gen_crises("dirty",r,t) - gen_crises("dirtychp",r,t) + eps ;
gen_crises("dirtynolig",r,t) = gen_crises("dirty",r,t) - gen_crises("lign",r,t) + eps ;

gen_crises("import",r,t) = import_r(r,t) + eps ;
gen_crises("export",r,t) = export_r(r,t) + eps ;

gen_crises("resv",r,t) = gmarket(r,"Reservoir",t) + eps ;
gen_crises("pump",r,t) = gmarket(r,"Pumpstorage",t) + eps ;

gen_crises("impexp",r,t) = import_r(r,t) + export_r(r,t) + eps ;

gen_crises("importfr",r,t) = import_rr(r,"France",t) + eps ;
gen_crises("exportfr",r,t) = export_rr(r,"France",t) + eps ;
gen_crises("impexpfr",r,t) = import_rr(r,"France",t) + export_rr(r,"France",t) + eps ;

gen_crises("hydrodiff",r,t) = dif_hydro(r,t) + eps ;

parameter
supply(r,t)
;

supply(r,t) = sum(ivrt(i,v,r,t), XTWHL(i,v,r,t)) + gen_crises("resv",r,t) +  gen_crises("pump",r,t) + gen_crises("import",r,t) ;

$if not  set days   sharecap_crises(t,crises_set,r)$(loadmax(r,t) > 0) = cap_crises(crises_set,r,t) / loadmax(r,t) + eps ;
$if not  set days   shareafcap_crises(t,crises_set,r)$(loadmax(r,t) > 0) = afcap_crises(crises_set,r,t) / loadmax(r,t) + eps ;
$if      set days   sharecap_crises(t,crises_set,r)$(loadmax_d(r,t) > 0) = cap_crises(crises_set,r,t) / loadmax_d(r,t) + eps ;
$if      set days   shareafcap_crises(t,crises_set,r)$(loadmax_d(r,t) > 0) = afcap_crises(crises_set,r,t) / loadmax_d(r,t) + eps ;
sharegen_crises(t,crises_set,r)$(supply(r,t) > 0) = gen_crises(crises_set,r,t) / supply(r,t) + eps ;

market_crises("nuc",r,t)$(sum(nuc(i), genera(t,r,i)) > 0) = sum(nuc(i), profit(t,r,i)) * 1e-3 / sum(nuc(i), genera(t,r,i)) + eps ;
market_crises("irnw",r,t)$(sum(irnw(i), genera(t,r,i)) > 0) = sum(irnw(i), profit(t,r,i)) * 1e-3 / sum(irnw(i), genera(t,r,i)) + eps ;
market_crises("rnw",r,t)$(sum(rnw(i), genera(t,r,i)) > 0) = sum(rnw(i), profit(t,r,i)) * 1e-3 / sum(rnw(i), genera(t,r,i)) + eps ;
market_crises("bio",r,t)$(sum(bio(i), genera(t,r,i)) > 0) = sum(bio(i), profit(t,r,i)) * 1e-3 / sum(bio(i), genera(t,r,i)) + eps ;
market_crises("chp",r,t)$(sum(chp(i), genera(t,r,i)) > 0) = sum(chp(i), profit(t,r,i)) * 1e-3 / sum(chp(i), genera(t,r,i)) + eps ;
market_crises("wind",r,t)$(sum(wind(i), genera(t,r,i)) > 0) = sum(wind(i), profit(t,r,i)) * 1e-3 / sum(wind(i), genera(t,r,i)) + eps ;
market_crises("sol",r,t)$(sum(sol(i), genera(t,r,i)) > 0) = sum(sol(i), profit(t,r,i)) * 1e-3 / sum(sol(i), genera(t,r,i)) + eps ;
market_crises("hydro",r,t)$(sum(hyd(i), genera(t,r,i)) > 0) = sum(hyd(i), profit(t,r,i)) * 1e-3 / sum(hyd(i), genera(t,r,i)) + eps ;
market_crises("gas",r,t)$(sum(gas(i), genera(t,r,i)) > 0) = sum(gas(i), profit(t,r,i)) * 1e-3 / sum(gas(i), genera(t,r,i)) + eps ;
market_crises("coal",r,t)$(sum(coa(i), genera(t,r,i)) > 0) = sum(coa(i), profit(t,r,i)) * 1e-3 / sum(coa(i), genera(t,r,i)) + eps ;
market_crises("lign",r,t)$(sum(lig(i), genera(t,r,i)) > 0) = sum(lig(i), profit(t,r,i)) * 1e-3 / sum(lig(i), genera(t,r,i)) + eps ;
market_crises("oil",r,t)$(sum(oil(i), genera(t,r,i)) > 0) = sum(oil(i), profit(t,r,i)) * 1e-3 / sum(oil(i), genera(t,r,i)) + eps ;
market_crises("gasnochp",r,t)$(sum(gas(i)$nochp(i), genera(t,r,i)) > 0) = sum(gas(i)$nochp(i), profit(t,r,i)) * 1e-3 / sum(gas(i)$nochp(i), genera(t,r,i)) + eps ;
market_crises("coalnochp",r,t)$(sum(coa(i)$nochp(i), genera(t,r,i)) > 0) = sum(coa(i)$nochp(i), profit(t,r,i)) * 1e-3 / sum(coa(i)$nochp(i), genera(t,r,i)) + eps ;
market_crises("lignnochp",r,t)$(sum(lig(i)$nochp(i), genera(t,r,i)) > 0) = sum(lig(i)$nochp(i), profit(t,r,i)) * 1e-3 / sum(lig(i)$nochp(i), genera(t,r,i)) + eps ;
market_crises("oilnochp",r,t)$(sum(oil(i)$nochp(i), genera(t,r,i)) > 0) = sum(oil(i)$nochp(i), profit(t,r,i)) * 1e-3 / sum(oil(i)$nochp(i), genera(t,r,i)) + eps ;
market_crises("bionochp",r,t)$(sum(bio(i)$nochp(i), genera(t,r,i)) > 0) = sum(bio(i)$nochp(i), profit(t,r,i)) * 1e-3 / sum(bio(i)$nochp(i), genera(t,r,i)) + eps ;
market_crises("gaschp",r,t)$(sum(gas(i)$chp(i), genera(t,r,i)) > 0) = sum(gas(i)$chp(i), profit(t,r,i)) * 1e-3 / sum(gas(i)$chp(i), genera(t,r,i)) + eps ;
market_crises("coalchp",r,t)$(sum(coa(i)$chp(i), genera(t,r,i)) > 0) = sum(coa(i)$chp(i), profit(t,r,i)) * 1e-3 / sum(coa(i)$chp(i), genera(t,r,i)) + eps ;
market_crises("lignchp",r,t)$(sum(lig(i)$chp(i), genera(t,r,i)) > 0) = sum(lig(i)$chp(i), profit(t,r,i)) * 1e-3 / sum(lig(i)$chp(i), genera(t,r,i)) + eps ;
market_crises("oilchp",r,t)$(sum(oil(i)$chp(i), genera(t,r,i)) > 0) = sum(oil(i)$chp(i), profit(t,r,i)) * 1e-3 / sum(oil(i)$chp(i), genera(t,r,i)) + eps ;
market_crises("biochp",r,t)$(sum(bio(i)$chp(i), genera(t,r,i)) > 0) = sum(bio(i)$chp(i), profit(t,r,i)) * 1e-3 / sum(bio(i)$chp(i), genera(t,r,i)) + eps ;
market_crises("dirty",r,t)$(sum(i$(coa(i) or lig(i) or oil(i) or gas(i)), genera(t,r,i)) > 0) = sum(i$(coa(i) or lig(i) or oil(i) or gas(i)), profit(t,r,i)) * 1e-3 / sum(i$(coa(i) or lig(i) or oil(i) or gas(i)), genera(t,r,i)) + eps ;
market_crises("dirtychp",r,t)$(sum(i$(coa(i) or lig(i) or oil(i) or gas(i) and chp(i)), genera(t,r,i)) > 0) = sum(i$(coa(i) or lig(i) or oil(i) or gas(i) and chp(i)), profit(t,r,i)) * 1e-3 / sum(i$(coa(i) or lig(i) or oil(i) or gas(i) and chp(i)), genera(t,r,i)) + eps ;
market_crises("dirtynochp",r,t)$(sum(i$(coa(i) or lig(i) or oil(i) or gas(i) and nochp(i)), genera(t,r,i)) > 0) = sum(i$(coa(i) or lig(i) or oil(i) or gas(i) and nochp(i)), profit(t,r,i)) * 1e-3 / sum(i$(coa(i) or lig(i) or oil(i) or gas(i) and nochp(i)), genera(t,r,i)) + eps ;
market_crises("dirtynolig",r,t)$(sum(i$(coa(i) or oil(i) or gas(i)), genera(t,r,i)) > 0) = sum(i$(coa(i) or oil(i) or gas(i)), profit(t,r,i)) * 1e-3 / sum(i$(coa(i) or oil(i) or gas(i)), genera(t,r,i)) + eps ;


*set rank /1*8760/ ;

*set
*bins klassen /0*10*8760/ 
*;

set
bins dd /0*300/
;

Parameter
bin_index(bins)
;


$offorder
loop(bins,
    bin_index(bins) = ord(bins) ;
    ) ;
$onorder

parameter
bin_lower(bins)
bin_upper(bins)
;

loop(bins,
    bin_lower(bins) = (bin_index(bins)-1) * 10 ;
    bin_upper(bins) = bin_index(bins) * 10 ;
    ) ;

parameter
freq_int(bins,r,t)
freq(bins,r,t)
density(bins,r,t)
;

freq(bins,r,t) = 0 ;

*loop(t, loop(r, loop(s, loop(bins, if(price(s,r,t) >= (card(bins)-ord(bins)) * 50 and price(s,r,t) < (card(bins)-ord(bins)+1) * 50,
*loop(t, loop(r, loop(s, loop(bins, if(price(s,r,t) >= bin_lower(bins) and price(s,r,t) < bin_upper(bins),
*    freq(bins,r,t) = freq(bins,r,t) + 1 ;
*) ) ) ) ) ;

*loop(t, loop(r, loop(s, loop(bins, if(price(s,r,t) >= (card(bins)-ord(bins)) * 50 and price(s,r,t) < (card(bins)-ord(bins)+1) * 50,
$if not  set days   loop(t, loop(r, loop(bins,
$if not  set days       freq(bins,r,t) = sum(s$(price(s,r,t) >= bin_lower(bins) and price(s,r,t) < bin_upper(bins)), 1) ;
$if not  set days   ) ) ) ;

$if not  set days   density(bins,r,t) = freq(bins,r,t) / card(s) + eps ;

$if      set days   loop(t, loop(r, loop(bins,
$if      set days       freq(bins,r,t) = sum((sd,hd)$(price_d(sd,hd,r,t) >= bin_lower(bins) and price_d(sd,hd,r,t) < bin_upper(bins)), 1) ;
$if      set days   ) ) ) ;

$if      set days   density(bins,r,t) = freq(bins,r,t) / (24 * card(sd)) + eps ;

parameter
discostco2_tyrpt(tyrpt,r,t)
discostco2_nochp_tyrpt(tyrpt,r,t)
discostco2_chp_tyrpt(tyrpt,r,t)
discostco2_gas(r,t)
discostco2_gasnochp(r,t)
discostco2_gaschp(r,t)
;

discostco2_tyrpt(tyrpt,r,t)$(sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(ivrt(i,v,r,t), XTWHL(i,v,r,t)))) > 0) = sum(xtyperpt(tyrpt,type), sum(idef(i,type),  sum(ivrt(i,v,r,t), discostco2(i,v,r,t) * XTWHL(i,v,r,t)))) / sum(xtyperpt(tyrpt,type), sum(idef(i,type),  sum(ivrt(i,v,r,t), XTWHL(i,v,r,t)))) + eps ;
discostco2_nochp_tyrpt(tyrpt,r,t)$(sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(ivrt(nochp(i),v,r,t), XTWHL(i,v,r,t)))) > 0) = sum(xtyperpt(tyrpt,type), sum(idef(i,type),  sum(ivrt(nochp(i),v,r,t), discostco2(i,v,r,t) * XTWHL(i,v,r,t)))) / sum(xtyperpt(tyrpt,type), sum(idef(i,type),  sum(ivrt(nochp(i),v,r,t), XTWHL(i,v,r,t)))) + eps ;
discostco2_chp_tyrpt(tyrpt,r,t)$(sum(xtyperpt(tyrpt,type), sum(idef(i,type), sum(ivrt(chp(i),v,r,t), XTWHL(i,v,r,t)))) > 0) = sum(xtyperpt(tyrpt,type), sum(idef(i,type),  sum(ivrt(chp(i),v,r,t), discostco2(i,v,r,t) * XTWHL(i,v,r,t)))) / sum(xtyperpt(tyrpt,type), sum(idef(i,type),  sum(ivrt(chp(i),v,r,t), XTWHL(i,v,r,t)))) + eps ;

discostco2_gas(r,t)$(sum(idef(i,type), sum(ivrt(gas(i),v,r,t), XTWHL(i,v,r,t))) > 0) = sum(idef(i,type),  sum(ivrt(gas(i),v,r,t), discostco2(i,v,r,t) * XTWHL(i,v,r,t))) / sum(idef(i,type),  sum(ivrt(gas(i),v,r,t), XTWHL(i,v,r,t))) + eps ;
discostco2_gasnochp(r,t)$(sum(idef(nochp(i),type), sum(ivrt(gas(i),v,r,t), XTWHL(i,v,r,t))) > 0) = sum(idef(nochp(i),type),  sum(ivrt(gas(i),v,r,t), discostco2(i,v,r,t) * XTWHL(i,v,r,t))) / sum(idef(nochp(i),type),  sum(ivrt(gas(i),v,r,t), XTWHL(i,v,r,t))) + eps ;
discostco2_gaschp(r,t)$(sum(idef(chp(i),type), sum(ivrt(gas(i),v,r,t), XTWHL(i,v,r,t))) > 0) = sum(idef(chp(i),type),  sum(ivrt(gas(i),v,r,t), discostco2(i,v,r,t) * XTWHL(i,v,r,t))) / sum(idef(chp(i),type),  sum(ivrt(gas(i),v,r,t), XTWHL(i,v,r,t))) + eps ;


$ontext

set
marginal(s,i,v,r,t)
marginalirnw(s,r,t)
marginalcount()
;

marginal(s,ivrt(i,v,r,t))$(XL(s,i,v,r,t) < (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * XC.L(i,v,r,t) and XL(s,i,v,r,t) > 0 and not irnw(i)) = YES ;
$if      set minmax marginal(s,ivrt(nuc(i),v,r,t)) = NO ;
$if      set minmax marginal(s,ivrt_nochp(bio(i),v,r,t)) = NO ;
$if      set minmax marginal(s,ivrt(nuc(i),v,r,t))$(XL(s,i,v,r,t) < afmax(s,i,v,r,t) * XC.L(i,v,r,t)) = YES ;
$if      set minmax marginal(s,ivrt_nochp(bio(i),v,r,t))$(XL(s,i,v,r,t) < afmax(s,i,v,r,t) * XC.L(i,v,r,t)) = YES ;
marginalirnw(s,r,t)$(XIRNW.L(s,r,t) < sum(ivrt(irnw_nohydro(i),newv(v),r,t), XC.L(i,v,r,t) * vrsc(s,i,v,r) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)))
                                           + sum(ivrt(irnw(i),oldv(v),r,t), XC.L(i,v,r,t) * vrsc_exi(s,i,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t))) and XIRNW.L(s,r,t) > 0) = YES ;





$ontext


Parameter
marginalshare(s,i,r,t)
productionshare(s,i,v,r,t)
productionshareirnw(s,r,t)
;

marginalshare(s,i,v,r,t) = sum(ivrt(i,v,r,t), XL(s,i,v,r,t)) / (sum(ii, sum(ivrt(ii,v,r,t), XL(s,ii,v,r,t))) + XIRNW.L(s,r,t)) ;
productionshare(s,i,v,,t) = XL(s,i,v,r,t) / XC.L(i,v,r,t) ;
productionshareirnw(s,r,t) = XIRNW.L(s,r,t) / (sum(ivrt(irnw_nohydro(i),newv(v),r,t), XC.L(i,v,r,t) * vrsc(s,i,v,r) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)))
                                           + sum(ivrt(irnw(i),oldv(v),r,t), XC.L(i,v,r,t) * vrsc_exi(s,i,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)))) ;
                                           
$offtext