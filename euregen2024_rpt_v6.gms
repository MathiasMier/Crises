* * * Allocate not vintage-specific variable into vintage-specific variables
Parameter
XL(s,i,v,r,t)
XTWHL(i,v,r,t)
*shareirnw(i,v,r,t)
shareirnw_s(s,i,v,r,t)
;

alias(irnw,irnww) ;
alias(ivrt,ivrtt) ;
alias(jvrt,jvrtt) ;
alias(newv,newvv) ;
alias(oldv,oldvv) ;

XL(s,ivrt(i,v,r,t)) = X.L(s,i,v,r,t) ;
XTWHL(ivrt(i,v,r,t)) = XTWH.L(i,v,r,t) ;

*$if set mergeirnw   shareirnw(ivrt(irnw(i),newv(v),r,t))$(sum(s, hours(s) * (sum(ivrtt(irnww(ii),newvv(vv),r,t), vrsc(s,ii,vv,r) * XC.L(ii,vv,r,t)) + sum(ivrtt(irnww(ii),oldvv(vv),r,t), vrsc_exi(s,ii,r,t) * XC.L(ii,vv,r,t)))) > 0) = sum(s, hours(s) * vrsc(s,i,v,r) * XC.L(i,v,r,t))
*$if set mergeirnw       / sum(s, hours(s) * (sum(ivrtt(irnww(ii),newvv(vv),r,t), vrsc(s,ii,vv,r) * XC.L(ii,vv,r,t)) + sum(ivrtt(irnww(ii),oldvv(vv),r,t), vrsc_exi(s,ii,r,t) * XC.L(ii,vv,r,t)))) ; 
*$if set mergeirnw   shareirnw(ivrt(irnw(i),oldv(v),r,t))$(sum(s, hours(s) * (sum(ivrtt(irnww(ii),newvv(vv),r,t), vrsc(s,ii,vv,r) * XC.L(ii,vv,r,t)) + sum(ivrtt(irnww(ii),oldvv(vv),r,t), vrsc_exi(s,ii,r,t) * XC.L(ii,vv,r,t)))) > 0) = sum(s, hours(s) * vrsc_exi(s,i,r,t) * XC.L(i,v,r,t))
*$if set mergeirnw       / sum(s, hours(s) * (sum(ivrtt(irnww(ii),newvv(vv),r,t), vrsc(s,ii,vv,r) * XC.L(ii,vv,r,t)) + sum(ivrtt(irnww(ii),oldvv(vv),r,t), vrsc_exi(s,ii,r,t) * XC.L(ii,vv,r,t)))) ; 
$if set mergeirnw   shareirnw_s(s,ivrt(irnw(i),newv(v),r,t))$((sum(ivrtt(irnww(ii),newvv(vv),r,t), vrsc(s,ii,vv,r) * XC.L(ii,vv,r,t)) + sum(ivrtt(irnww(ii),oldvv(vv),r,t), vrsc_exi(s,ii,r,t) * XC.L(ii,vv,r,t))) > 0) = vrsc(s,i,v,r) * XC.L(i,v,r,t)
$if set mergeirnw       / (sum(ivrtt(irnww(ii),newvv(vv),r,t), vrsc(s,ii,vv,r) * XC.L(ii,vv,r,t)) + sum(ivrtt(irnww(ii),oldvv(vv),r,t), vrsc_exi(s,ii,r,t) * XC.L(ii,vv,r,t))) ; 
$if set mergeirnw   shareirnw_s(s,ivrt(irnw(i),oldv(v),r,t))$((sum(ivrtt(irnww(ii),newvv(vv),r,t), vrsc(s,ii,vv,r) * XC.L(ii,vv,r,t)) + sum(ivrtt(irnww(ii),oldvv(vv),r,t), vrsc_exi(s,ii,r,t) * XC.L(ii,vv,r,t))) > 0) = vrsc_exi(s,i,r,t) * XC.L(i,v,r,t)
$if set mergeirnw       / (sum(ivrtt(irnww(ii),newvv(vv),r,t), vrsc(s,ii,vv,r) * XC.L(ii,vv,r,t)) + sum(ivrtt(irnww(ii),oldvv(vv),r,t), vrsc_exi(s,ii,r,t) * XC.L(ii,vv,r,t))) ; 

$if set mergeirnw   XL(s,ivrt(irnw(i),v,r,t)) = XIRNW.L(s,r,t) * shareirnw_s(s,i,v,r,t) ;
$if set mergeirnw   XTWHL(ivrt(irnw(i),v,r,t)) = sum(s, hours(s) * XL(s,i,v,r,t)) * 1e-3 ;
*$if set mergeirnw   XTWHL(ivrt(irnw(i),v,r,t)) = XTWHIRNW.L(r,t) * shareirnw(i,v,r,t) ;
*$if set mergeirnw   XL(s,ivrt(irnw(i),v,r,t)) = XIRNW.L(s,r,t) * shareirnw(i,v,r,t) ;

parameter
IGL(j,r,t)                              Interim capacity for "no storage" run
GCL(j,v,r,t)                            Interim capacity for "no storage" run
GBL(s,j,v,r,t)                          Interim capacity for "no storage" run
GL(s,j,v,r,t)                           Interim capacity for "no storage" run
GDL(s,j,v,r,t)                          Interim capacity for "no storage" run
;

* Interim calculations
IGL(j,r,t)      = eps ;
GCL(j,v,r,t)    = eps ;
GBL(s,j,v,r,t)  = eps ;
GL(s,j,v,r,t)   = eps ;
GDL(s,j,v,r,t)  = eps ;

$if not  set storage  IG.L(j,r,t) = eps ;
$if not  set storage  GC.L(j,v,r,t) = eps ;
$if not  set storage  GB.L(s,j,v,r,t) = eps ;
$if not  set storage  G.L(s,j,v,r,t) = eps ;
$if not  set storage  GD.L(s,j,v,r,t) = eps ;

$if not  set storage  GCNV.L(j,r,t) = eps ;
$if not  set storage  GBNV.L(s,j,r,t) = eps ;
$if not  set storage  GNV.L(s,j,r,t) = eps ;
$if not  set storage  GDNV.L(s,j,r,t) = eps ;

$if      set storage                               IGL(j,r,t)                   = IG.L(j,r,t)   ;
$if      set storage $if not  set storagebalnv     GCL(jvrt(j,v,r,t))           = GC.L(j,v,r,t) ;
$if      set storage $if      set storagebalnv     GCL(jvrt(nonvj(j),v,r,t))    = GC.L(j,v,r,t) ;
$if      set storage $if      set storagebalnv     $if    set hydrogensimple    GCL(jvrt(ghyd(j),v,r,t))    = GCC.L(j,v,r,t) ;
$if      set storage $if      set storagebalnv     GCL(jvrt(nvj(j),v,r,t))$(sum(vv, gcapt(j,vv,r,"2023")) > 0)      = GCNV.L(j,r,t) * gcapt(j,v,r,"2023") / sum(vv, gcapt(j,vv,r,"2023")) ;
$if      set storage $if not  set storagebalnv     GBL(s,jvrt(j,v,r,t))  = GB.L(s,j,v,r,t) ;
$if      set storage $if not  set storagebalnv     GL(s,jvrt(j,v,r,t))   = G.L(s,j,v,r,t)  ;
$if      set storage $if not  set storagebalnv     GDL(s,jvrt(j,v,r,t))  = GD.L(s,j,v,r,t) ;
$if      set storage $if      set storagebalnv     GL(s,jvrt(nonvj(j),v,r,t))  = G.L(s,j,v,r,t)  ;
$if      set storage $if      set storagebalnv     GDL(s,jvrt(nonvj(j),v,r,t)) = GD.L(s,j,v,r,t) ;
$if      set storage $if      set storagebalnv     GBL(s,jvrt(nonvj(j),v,r,t))$(sum(vv, ghours(j,vv,r) * GCL(j,vv,r,t)) > 0) = GBNV.L(s,j,r,t) * ghours(j,v,r) * GCL(j,v,r,t) / sum(vv, ghours(j,vv,r) * GCL(j,vv,r,t)) ;
$if      set storage $if      set storagebalnv     $if    set hydrogensimple    GBL(s,jvrt(ghyd(j),v,r,t))$(sum(vv, GCR.L(j,vv,r,t)) > 0) = GBNV.L(s,j,r,t) * GCL(j,v,r,t) / sum(vv, GCR.L(j,vv,r,t)) ;
$if      set storage $if      set storagebalnv     GBL(s,jvrt(nvj(j),v,r,t))$(sum(vv, ghours(j,vv,r) * GCL(j,vv,r,t)) > 0) = GBNV.L(s,j,r,t) * ghours(j,v,r) * gcapt(j,v,r,"2023") / sum(vv, ghours(j,vv,r) * gcapt(j,vv,r,"2023")) ;
$if      set storage $if      set storagebalnv     GL(s,jvrt(nvj(j),v,r,t))$(sum(vv, GCL(j,vv,r,t)) > 0) = GNV.L(s,j,r,t) * GCL(j,v,r,t) / sum(vv, GCL(j,vv,r,t)) ;
$if      set storage $if      set storagebalnv     GDL(s,jvrt(nvj(j),v,r,t))$(sum(vv, GCL(j,vv,r,t)) > 0) = GDNV.L(s,j,r,t) * GCL(j,v,r,t) / sum(vv, GCL(j,vv,r,t)) ;

* * * Define reporting type for simplification
set
tyrpt                    Reporting technology types
                         /Bioenergy, Coal, Gas-CCGT, Gas-OCGT, Gas-ST, Geothermal, Hydro, Lignite, Nuclear,
                         OilOther, Solar, WindOff, WindOn, Bio-CCS, Coal-CCS, Gas-CCS, Lignite-CCS/
xtyperpt(tyrpt,type)     Map between technology types
                         /Bioenergy.biow, Coal.hdcl, Gas-CCGT.ngcc, Gas-OCGT.nggt, Gas-ST.ngst, Geothermal.geot,
                         Hydro.Hydro, Lignite.lign, Nuclear.nuc, OilOther.ptsg, Solar.slpv,
                         WindOff.wind-os, WindOn.wind, Bio-CCS.becs, Coal-CCS.hdcs, Gas-CCS.ngcs, Lignite-CCS.lics/

rrpt                     Reporting regions
                         /North, South, Central, East/

xrrpt(rrpt,r)            Map rrpt r
$if      set spatial28                        /North.(Britain, Ireland, Norway, Sweden, Finland, Denmark),
$if      set spatial28                         South.(Portugal, Spain, France, Italy),
$if      set spatial28                         Central.(Belgium, Luxembourg, Netherlands, Austria, Switzerland, Germany)
$if      set spatial28                         East.(Czech,Poland,Slovakia,Estonia,Lithuania,Latvia,Croatia,Hungary,Slovenia,Bulgaria,Greece,Romania)/

$if      set spatialveryhigh                       /North.(Britain, Ireland, Norway, Sweden, Finland, Denmark),
$if      set spatialveryhigh                        South.(Portugal, Spain, France, Italy),
$if      set spatialveryhigh                        Central.(Belux, Netherlands, Austria, Switzerland, Germany)
$if      set spatialveryhigh                        East.(Czeslovak,Poland,EE-NE,EE-SW,Bulgaria,Greece,Romania)/

$if      set spatialhigh                           /North.(Britain, Norway, Sweden, Finland, Denmark),
$if      set spatialhigh                            South.(Iberia, France, Italy),
$if      set spatialhigh                            Central.(Belux, Netherlands, Alpine, Germany)
$if      set spatialhigh                            East.(Czeslovak,Poland,EE-NE,EE-SW,EE-SE)/

$if      set spatialmid                            /North.(Britain, Scanda),
$if      set spatialmid                             South.(Iberia, France, Italy),
$if      set spatialmid                             Central.(Benelux, Alpine, Germany)
$if      set spatialmid                             East.(EE-NW,EE-NE,EE-SW,EE-SE)/

$if      set spatiallow                            /North.(Britain, Scanda),
$if      set spatiallow                             South.(Iberia, France, Italy),
$if      set spatiallow                             Central.(Central)
$if      set spatiallow                             East.(East)/

$if      set spatialverylow                        /North.(North),
$if      set spatialverylow                         South.(South),
$if      set spatialverylow                         Central.(Central)
$if      set spatialverylow                         East.(East)/

xrrpt_(rrpt,r)           Alias map rrpt r
;

alias(r,rr) ;
alias(rrpt,rrpt_) ;
xrrpt_(rrpt,r)$xrrpt(rrpt,r) = YES ;


parameter
load_res(s,r,t)
;

$if set hydrogensimple load_res(s,r,t) = load(s,r,t) - load_sec(s,r,t,"hyd") ;
$if set hydrogensimple daref(r,t) = sum(s, hours(s) * load_res(s,r,t)) * 1e-3 ;

* * * Emissions reporting
parameters
co2emit(r,t)                     CO2 emissions (MtCO2)
co2emit_fuel(r,t,fuel)           CO2 emissions by fuel (MtCO2)
co2preu(t)                       CO2 emissions price (EUR per tCO2) in EU
co2pruk(t)                       CO2 emissions price (EUR per tCO2) in UK
co2prreg(r,t)                    Regional CO2 emissions price (EUR per tCO2)
co2prwei(t)                      Emission-weighted CO2 price (EUR per tCO2)
co2capt(r,t)                     CO2 emissions captued (MtCO2)
Emissions_rpt(t,r,*)             Emissions reporting vector (MtC2)
Emissions_ByFuel_rpt(t,r,*)      Emissions by fuel reporting vector (MtCO2)
Emissions_total_ByFuel_rpt(t,*)  Total emissions by fuel reporting vector (MtCO2)
Emissions_GER_ByFuel_rpt(t,*)    Total emissions by fuel reporting vector (MtCO2)
Emissions_total_rpt(t,*)         Total emissions reporting vector (MtC2)
Emissions_GER_rpt(t,*)           Total emissions reporting vector (MtC2)
Emissions_FR_rpt(t,*)           Total emissions reporting vector (MtC2)
gasuse(r,t)
gasuseeu(t)

;

gasuse(r,t)             = sum(ivrt(gas(i),v,r,t), XTWHL(i,v,r,t) / effrate(i,v,r)) ;
gasuseeu(t)             = sum(r, gasuse(r,t)) ;
co2emit(r,t)            = sum(ivrt(i,v,r,t), emit(i,v,r) * XTWHL(i,v,r,t)) ;
co2emit_fuel(r,t,fuel)  = sum(xfueli(fuel,i), sum(ivrt(i,v,r,t), emit(i,v,r) * XTWHL(i,v,r,t))) ;
co2capt(r,t)            = sum(ivrt(i,v,r,t), co2captured(i,v,r) * XTWHL(i,v,r,t)) ;

co2preu(t)         = 0
$if      set scc                            + scc(t)
$if      set co2price                       + co2p(t)
$if      set co2marktotal                   + abs(co2market.M(t)) / dfact(t)
$if      set co2mark                        + abs(euets.M(t)) / dfact(t)
$if      set co2iter                        + abs(it_euets.M(t)) / dfact(t)
$if      set co2mips                        + abs(eqs_euets.M(t)) / dfact(t)
$if      set static                         + co2prreg_static("Germany",t)
                ;
                
co2pruk(t) = 0 
$if      set co2mark                        + abs(ukets.M(t)) / dfact(t)
$if      set co2iter                        + abs(ukets.M(t)) / dfact(t)
$if      set co2mips                        + abs(ukets.M(t)) / dfact(t)
$if      set static                         + co2prreg_static("Britain",t)
                ;
   
co2prreg(r,t)$(not sameas(r,"Britain")) = co2preu(t) ;
co2prreg("Britain",t) = co2pruk(t) ;

$if      set euetsbreak  co2prreg(r,t)$(not sameas(r,"Britain") and t.val ge 2031) = abs(co2market_r.M(r,t)) / dfact(t) ;
$if      set euetsbreak  co2prreg(r,t)$(not sameas(r,"Britain") and t.val le 2030) = co2preu(t) ;

co2prwei(t)$(sum(r, co2emit(r,t)) ne 0) = sum(r, co2emit(r,t) * co2prreg(r,t)) / sum(r, co2emit(r,t)) ;


* CO2 emissions is the dummy for the python algorithm but could be filled by assumptions about remaining EU ETS emissions (for example)
Emissions_rpt(t,r,"CO2-emissions")                      = eps ;
Emissions_rpt(t,r,"CO2-emissions-elec")                 = co2emit(r,t) + eps ;
Emissions_rpt(t,r,"CO2-price")                          = co2prreg(r,t) + eps ;
Emissions_rpt(t,r,"CO2-captured")                       = co2capt(r,t) + eps ;

Emissions_total_rpt(t,"Total CO2-emissions-elec")       = sum(r, co2emit(r,t)) + eps ;
Emissions_total_rpt(t,"Total CO2-captured")             = sum(r, co2capt(r,t)) + eps ;
Emissions_total_rpt(t,"CO2 price")                      = co2prwei(t) + eps ;

Emissions_total_rpt(t,"Total CO2-emissions-elec-eu")    = sum(r$(not sameas(r,"Britain")), co2emit(r,t)) + eps ;
Emissions_total_rpt(t,"Total CO2-captured-eu")          = sum(r$(not sameas(r,"Britain")), co2capt(r,t)) + eps ;
Emissions_total_rpt(t,"CO2 price-eu")                   = co2preu(t) + eps ;

Emissions_total_rpt(t,"Total CO2-emissions-elec-uk")    = sum(r$(sameas(r,"Britain")), co2emit(r,t)) + eps ;
Emissions_total_rpt(t,"Total CO2-captured-uk")          = sum(r$(sameas(r,"Britain")), co2capt(r,t)) + eps ;
Emissions_total_rpt(t,"CO2 price-uk")                   = co2pruk(t) + eps ;

Emissions_GER_rpt(t,"Total CO2-emissions-elec")         = sum(r$(sameas(r,"Germany")), co2emit(r,t)) + eps ;
Emissions_GER_rpt(t,"Total CO2-captured")               = sum(r$(sameas(r,"Germany")), co2capt(r,t)) + eps ;
Emissions_GER_rpt(t,"CO2 price")                        = co2prreg("Germany",t) + eps ;

Emissions_GER_rpt(t,"Total CO2-emissions-elec-eu")    = sum(r$(not sameas(r,"Britain")), co2emit(r,t)) + eps ;
Emissions_GER_rpt(t,"Total CO2-captured-eu")          = sum(r$(not sameas(r,"Britain")), co2capt(r,t)) + eps ;
Emissions_GER_rpt(t,"CO2 price-eu")                   = co2preu(t) + eps ;

Emissions_GER_rpt(t,"Total CO2-emissions-elec-uk")    = sum(r$(sameas(r,"Britain")), co2emit(r,t)) + eps ;
Emissions_GER_rpt(t,"Total CO2-captured-uk")          = sum(r$(sameas(r,"Britain")), co2capt(r,t)) + eps ;
Emissions_GER_rpt(t,"CO2 price-uk")                   = co2pruk(t) + eps ;

Emissions_FR_rpt(t,"Total CO2-emissions-elec")          = sum(r$(sameas(r,"France")), co2emit(r,t)) + eps ;
Emissions_FR_rpt(t,"Total CO2-captured")                = sum(r$(sameas(r,"France")), co2capt(r,t)) + eps ;
Emissions_FR_rpt(t,"CO2 price")                         = co2prreg("France",t) + eps ;

Emissions_FR_rpt(t,"Total CO2-emissions-elec-eu")    = sum(r$(not sameas(r,"Britain")), co2emit(r,t)) + eps ;
Emissions_FR_rpt(t,"Total CO2-captured-eu")          = sum(r$(not sameas(r,"Britain")), co2capt(r,t)) + eps ;
Emissions_FR_rpt(t,"CO2 price-eu")                   = co2preu(t) + eps ;

Emissions_FR_rpt(t,"Total CO2-emissions-elec-uk")    = sum(r$(sameas(r,"Britain")), co2emit(r,t)) + eps ;
Emissions_FR_rpt(t,"Total CO2-captured-uk")          = sum(r$(sameas(r,"Britain")), co2capt(r,t)) + eps ;
Emissions_FR_rpt(t,"CO2 price-uk")                   = co2pruk(t) + eps ;

Emissions_ByFuel_rpt(t,r,"CO2-emissions-bioenergy")     = co2emit_fuel(r,t,"Bioenergy") + eps ;
Emissions_ByFuel_rpt(t,r,"CO2-emissions-coal")          = co2emit_fuel(r,t,"Coal") + eps ;
Emissions_ByFuel_rpt(t,r,"CO2-emissions-gas")           = co2emit_fuel(r,t,"Gas") + eps ;
Emissions_ByFuel_rpt(t,r,"CO2-emissions-lignite")       = co2emit_fuel(r,t,"Lignite") + eps ;
Emissions_ByFuel_rpt(t,r,"CO2-emissions-oil/other")     = co2emit_fuel(r,t,"Oil") + eps ;

Emissions_total_ByFuel_rpt(t,"CO2-emissions-bioenergy") = sum(r, co2emit_fuel(r,t,"Bioenergy")) + eps ;
Emissions_total_ByFuel_rpt(t,"CO2-emissions-coal")      = sum(r, co2emit_fuel(r,t,"Coal")) + eps ;
Emissions_total_ByFuel_rpt(t,"CO2-emissions-gas")       = sum(r, co2emit_fuel(r,t,"Gas")) + eps ;
Emissions_total_ByFuel_rpt(t,"CO2-emissions-lignite")   = sum(r, co2emit_fuel(r,t,"Lignite")) + eps ;
Emissions_total_ByFuel_rpt(t,"CO2-emissions-oil/other") = sum(r, co2emit_fuel(r,t,"Oil")) + eps ;

Emissions_total_ByFuel_rpt(t,"CO2-emissions-bioenergy-eu") = sum(r$(not sameas(r,"Britain")), co2emit_fuel(r,t,"Bioenergy")) + eps ;
Emissions_total_ByFuel_rpt(t,"CO2-emissions-coal-eu")      = sum(r$(not sameas(r,"Britain")), co2emit_fuel(r,t,"Coal")) + eps ;
Emissions_total_ByFuel_rpt(t,"CO2-emissions-gas-eu")       = sum(r$(not sameas(r,"Britain")), co2emit_fuel(r,t,"Gas")) + eps ;
Emissions_total_ByFuel_rpt(t,"CO2-emissions-lignite-eu")   = sum(r$(not sameas(r,"Britain")), co2emit_fuel(r,t,"Lignite")) + eps ;
Emissions_total_ByFuel_rpt(t,"CO2-emissions-oil/other-eu") = sum(r$(not sameas(r,"Britain")), co2emit_fuel(r,t,"Oil")) + eps ;

Emissions_total_ByFuel_rpt(t,"CO2-emissions-bioenergy-uk") = sum(r$(sameas(r,"Britain")), co2emit_fuel(r,t,"Bioenergy")) + eps ;
Emissions_total_ByFuel_rpt(t,"CO2-emissions-coal-uk")      = sum(r$(sameas(r,"Britain")), co2emit_fuel(r,t,"Coal")) + eps ;
Emissions_total_ByFuel_rpt(t,"CO2-emissions-gas-uk")       = sum(r$(sameas(r,"Britain")), co2emit_fuel(r,t,"Gas")) + eps ;
Emissions_total_ByFuel_rpt(t,"CO2-emissions-lignite-uk")   = sum(r$(sameas(r,"Britain")), co2emit_fuel(r,t,"Lignite")) + eps ;
Emissions_total_ByFuel_rpt(t,"CO2-emissions-oil/other-uk") = sum(r$(sameas(r,"Britain")), co2emit_fuel(r,t,"Oil")) + eps ;

Emissions_GER_ByFuel_rpt(t,"CO2-emissions-bioenergy") =  sum(r$(sameas(r,"Germany")), co2emit_fuel(r,t,"Bioenergy")) + eps ;
Emissions_GER_ByFuel_rpt(t,"CO2-emissions-coal") =       sum(r$(sameas(r,"Germany")), co2emit_fuel(r,t,"Coal")) + eps;
Emissions_GER_ByFuel_rpt(t,"CO2-emissions-gas") =        sum(r$(sameas(r,"Germany")), co2emit_fuel(r,t,"Gas")) + eps ;
Emissions_GER_ByFuel_rpt(t,"CO2-emissions-lignite") =    sum(r$(sameas(r,"Germany")), co2emit_fuel(r,t,"Lignite")) + eps ;
Emissions_GER_ByFuel_rpt(t,"CO2-emissions-oil/other") =  sum(r$(sameas(r,"Germany")), co2emit_fuel(r,t,"Oil")) + eps ;

* * * Electricity reporting
set
sea                    Seasons of the year /w, s, m, strm, stra/
ssea(s,sea)            Map between segment and season
;

* Generate map between segments and seasons
ssea(s,"w")$(sm(s,"1") or sm(s,"2") or sm(s,"12")) = YES ;
ssea(s,"s")$(sm(s,"6") or sm(s,"7") or sm(s,"8"))  = YES ;
ssea(s,"m")$(not ssea(s,"s") and not ssea(s,"w"))  = YES ;
ssea(s,"strm")$(sm(s,"1") or sm(s,"2") or sm(s,"3"))                = YES ;
ssea(s,"stra")$(sm(s,"1") or sm(s,"2") or sm(s,"3") or sm(s,"4"))   = YES ;

parameter
price(s,r,t)                     Electricity price (EUR per MWh)
Electricity1_rpt(t,r,*)           Electricity reporting vector (EUR per MWh and TWh)
Electricity1_total_rpt(t,*)       Electricity reporting vector (EUR per MWh and TWh)
Electricity1_GER_rpt(t,*)         Electricity reporting vector (EUR per MWh and TWh)
Electricity2_rpt(t,r,*)           Electricity reporting vector (EUR per MWh and TWh)
Electricity2_total_rpt(t,*)       Electricity reporting vector (EUR per MWh and TWh)
Electricity2_GER_rpt(t,*)         Electricity reporting vector (EUR per MWh and TWh)
Electricity3_rpt(t,r,*)           Electricity reporting vector (EUR per MWh and TWh)
Electricity3_total_rpt(t,*)       Electricity reporting vector (EUR per MWh and TWh)
Electricity3_GER_rpt(t,*)         Electricity reporting vector (EUR per MWh and TWh)
discostco2(i,v,r,t)
;

discostco2(i,v,r,t) = discost(i,v,r,t) + emit(i,v,r) * co2prreg(r,t) ;

price(s,r,t)     = (demand.M(s,r,t)
*$if      set rsa + demand_rsa.M(s,r,t)
                 ) / dfact(t) ;

* Compile different prices
$if not  set elastic Electricity1_rpt(t,r,"price-avg")$(sum(s,             load_res(s,r,t) * hours(s)) > 0)                = sum(s,             price(s,r,t) * load_res(s,r,t) * hours(s)) / sum(s,             load_res(s,r,t) * hours(s)) + eps ;
$if not  set elastic Electricity1_rpt(t,r,"price-avg-winter")$(sum(s$ssea(s,"w"), load_res(s,r,t) * hours(s)) > 0)         = sum(s$ssea(s,"w"), price(s,r,t) * load_res(s,r,t) * hours(s)) / sum(s$ssea(s,"w"), load_res(s,r,t) * hours(s)) + eps ;
$if not  set elastic Electricity1_rpt(t,r,"price-avg-summer")$(sum(s$ssea(s,"s"), load_res(s,r,t) * hours(s)) > 0)         = sum(s$ssea(s,"s"), price(s,r,t) * load_res(s,r,t) * hours(s)) / sum(s$ssea(s,"s"), load_res(s,r,t) * hours(s)) + eps ;
$if not  set elastic Electricity1_rpt(t,r,"price-avg-midseason")$(sum(s$ssea(s,"m"), load_res(s,r,t) * hours(s)) > 0)      = sum(s$ssea(s,"m"), price(s,r,t) * load_res(s,r,t) * hours(s)) / sum(s$ssea(s,"m"), load_res(s,r,t) * hours(s)) + eps ;
$if not  set elastic Electricity1_rpt(t,r,"price-avg-janmar")$(sum(s$ssea(s,"strm"), load_res(s,r,t) * hours(s)) > 0)      = sum(s$ssea(s,"strm"), price(s,r,t) * load_res(s,r,t) * hours(s)) / sum(s$ssea(s,"strm"), load_res(s,r,t) * hours(s)) + eps ;
$if not  set elastic Electricity1_rpt(t,r,"price-avg-janapr")$(sum(s$ssea(s,"stra"), load_res(s,r,t) * hours(s)) > 0)      = sum(s$ssea(s,"stra"), price(s,r,t) * load_res(s,r,t) * hours(s)) / sum(s$ssea(s,"stra"), load_res(s,r,t) * hours(s)) + eps ;
$if not  set elastic Electricity1_rpt(t,r,"price-max")                = smax(s, price(s,r,t)) + eps ;
$if not  set elastic Electricity1_rpt(t,r,"price-min")                = smin(s, price(s,r,t)) + eps ;

$if      set elastic Electricity1_rpt(t,r,"price-avg")                = sum(s,             price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s)) / sum(s,             sum(bse, DS.L(bse,s,r,t)) * hours(s)) + eps ;
$if      set elastic Electricity1_rpt(t,r,"price-avg-winter")         = sum(s$ssea(s,"w"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s)) / sum(s$ssea(s,"w"), sum(bse, DS.L(bse,s,r,t)) * hours(s)) + eps ;
$if      set elastic Electricity1_rpt(t,r,"price-avg-summer")         = sum(s$ssea(s,"s"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s)) / sum(s$ssea(s,"s"), sum(bse, DS.L(bse,s,r,t)) * hours(s)) + eps ;
$if      set elastic Electricity1_rpt(t,r,"price-avg-midseason")      = sum(s$ssea(s,"m"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s)) / sum(s$ssea(s,"m"), sum(bse, DS.L(bse,s,r,t)) * hours(s)) + eps ;
$if      set elastic Electricity1_rpt(t,r,"price-avg-janmar")         = sum(s$ssea(s,"strm"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s)) / sum(s$ssea(s,"strm"), sum(bse, DS.L(bse,s,r,t)) * hours(s)) + eps ;
$if      set elastic Electricity1_rpt(t,r,"price-avg-janapr")         = sum(s$ssea(s,"stra"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s)) / sum(s$ssea(s,"stra"), sum(bse, DS.L(bse,s,r,t)) * hours(s)) + eps ;
$if      set elastic Electricity1_rpt(t,r,"price-max")                = smax(s, price(s,r,t)) + eps ;
$if      set elastic Electricity1_rpt(t,r,"price-min")                = smin(s, price(s,r,t)) + eps ;

$if not  set elastic Electricity1_rpt(t,r,"elec-demand")              = sum(s, load_res(s,r,t) * hours(s)) * 1e-3 + eps ;
$if not  set elastic Electricity1_rpt(t,r,"elec-demand-org")          = sum(s, load_res(s,r,t) * hours(s)) * 1e-3 + eps ;         
$if      set elastic Electricity1_rpt(t,r,"elec-demand")              = sum(s, hours(s) * sum(bse, DS.L(bse,s,r,t) - BSELAS.L(bse,s,r,t))) * 1e-3 + eps ;
$if      set elastic Electricity1_rpt(t,r,"elec-demand-org")          = sum(s, load_res(s,r,t) * hours(s)) * 1e-3 + eps ;

$if not  set elastic Electricity1_rpt(t,r,"elec-demand-ave")          = sum(s, load_res(s,r,t) * hours(s)) / 8760 + eps ;
$if not  set elastic Electricity1_rpt(t,r,"elec-demand-ave-org")      = sum(s, load_res(s,r,t) * hours(s)) / 8760 + eps ;
$if      set elastic Electricity1_rpt(t,r,"elec-demand-ave")          = sum(s, hours(s) * sum(bse, DS.L(bse,s,r,t) - BSELAS.L(bse,s,r,t))) / 8760 + eps ;
$if      set elastic Electricity1_rpt(t,r,"elec-demand-ave-org")      = sum(s, load_res(s,r,t) * hours(s)) / 8760 + eps ;

Electricity2_rpt(t,r,"price-nuc")$(sum(nuc(i), sum(s, sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(nuc(i), sum(s, price(s,r,t) * sum(v, XL(s,i,v,r,t)) * hours(s)))
                                                / sum(nuc(i), sum(s,                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity2_rpt(t,r,"price-sol")$(sum(sol(i), sum(s, sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(sol(i), sum(s, price(s,r,t) * sum(v, XL(s,i,v,r,t)) * hours(s)))
                                                / sum(sol(i), sum(s,                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity2_rpt(t,r,"price-wind")$(sum(wind(i), sum(s, sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(wind(i), sum(s, price(s,r,t) * sum(v, XL(s,i,v,r,t)) * hours(s)))
                                                / sum(wind(i), sum(s,                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity2_rpt(t,r,"price-windon")$(sum(windon(i), sum(s, sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(windon(i), sum(s, price(s,r,t) * sum(v, XL(s,i,v,r,t)) * hours(s)))
                                                / sum(windon(i), sum(s,                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity2_rpt(t,r,"price-windoff")$(sum(windoff(i), sum(s, sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(windoff(i), sum(s, price(s,r,t) * sum(v, XL(s,i,v,r,t)) * hours(s)))
                                                / sum(windoff(i), sum(s,                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity2_rpt(t,r,"price-bio")$(sum(bio(i), sum(s, sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(bio(i), sum(s, price(s,r,t) * sum(v, XL(s,i,v,r,t)) * hours(s)))
                                                / sum(bio(i), sum(s,                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity2_rpt(t,r,"price-gas")$(sum(gas(i), sum(s, sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(gas(i), sum(s, price(s,r,t) * sum(v, XL(s,i,v,r,t)) * hours(s)))
                                                / sum(gas(i), sum(s,                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity2_rpt(t,r,"price-coa")$(sum(i$sameas(i,"Coal"), sum(s, sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(i$sameas(i,"Coal"), sum(s, price(s,r,t) * sum(v, XL(s,i,v,r,t)) * hours(s)))
                                                / sum(i$sameas(i,"Coal"), sum(s,                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity2_rpt(t,r,"price-lig")$(sum(i$sameas(i,"Lignite"), sum(s, sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(i$sameas(i,"Lignite"), sum(s, price(s,r,t) * sum(v, XL(s,i,v,r,t)) * hours(s)))
                                                / sum(i$sameas(i,"Lignite"), sum(s,                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity2_rpt(t,r,"price-ccs")$(sum(ccs(i), sum(s, sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(ccs(i), sum(s, price(s,r,t) * sum(v, XL(s,i,v,r,t)) * hours(s)))
                                                / sum(ccs(i), sum(s,                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;

Electricity3_rpt(t,r,"cost-nuc")$(sum(nuc(i), sum(s, sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(nuc(i), sum(s, sum(v, discostco2(i,v,r,t) * XL(s,i,v,r,t)) * hours(s)) + sum(v, XC.L(i,v,r,t) * 1e+3 *  fomcost(i,v,r)))
                                                / sum(nuc(i), sum(s,                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity3_rpt(t,r,"cost-sol")$(sum(sol(i), sum(s, sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(sol(i), sum(s, sum(v, discostco2(i,v,r,t) * XL(s,i,v,r,t)) * hours(s)) + sum(v, XC.L(i,v,r,t) * 1e+3 *  fomcost(i,v,r)))
                                                / sum(sol(i), sum(s,                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity3_rpt(t,r,"cost-wind")$(sum(wind(i), sum(s, sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(wind(i), sum(s, sum(v, discostco2(i,v,r,t) * XL(s,i,v,r,t)) * hours(s)) + sum(v, XC.L(i,v,r,t) * 1e+3 *  fomcost(i,v,r)))
                                                / sum(wind(i), sum(s,                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity3_rpt(t,r,"cost-windon")$(sum(windon(i), sum(s, sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(windon(i), sum(s, sum(v, discostco2(i,v,r,t) * XL(s,i,v,r,t)) * hours(s)) + sum(v, XC.L(i,v,r,t) * 1e+3 *  fomcost(i,v,r)))
                                                / sum(windon(i), sum(s,                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity3_rpt(t,r,"cost-windoff")$(sum(windoff(i), sum(s, sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(windoff(i), sum(s, sum(v, discostco2(i,v,r,t) * XL(s,i,v,r,t)) * hours(s)) + sum(v, XC.L(i,v,r,t) * 1e+3 *  fomcost(i,v,r)))
                                                / sum(windoff(i), sum(s,                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity3_rpt(t,r,"cost-bio")$(sum(bio(i), sum(s, sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(bio(i), sum(s, sum(v, discostco2(i,v,r,t) * XL(s,i,v,r,t)) * hours(s)) + sum(v, XC.L(i,v,r,t) * 1e+3 *  fomcost(i,v,r)))
                                                / sum(bio(i), sum(s,                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity3_rpt(t,r,"cost-gas")$(sum(gas(i), sum(s, sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(gas(i), sum(s, sum(v, discostco2(i,v,r,t) * XL(s,i,v,r,t)) * hours(s)) + sum(v, XC.L(i,v,r,t) * 1e+3 *  fomcost(i,v,r)))
                                                / sum(gas(i), sum(s,                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity3_rpt(t,r,"cost-coa")$(sum(i$sameas(i,"Coal"), sum(s, sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(i$sameas(i,"Coal"), sum(s, sum(v, discostco2(i,v,r,t) * XL(s,i,v,r,t)) * hours(s)) + sum(v, XC.L(i,v,r,t) * 1e+3 *  fomcost(i,v,r)))
                                                / sum(i$sameas(i,"Coal"), sum(s,                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity3_rpt(t,r,"cost-lig")$(sum(i$sameas(i,"Lignite"), sum(s, sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(i$sameas(i,"Lignite"), sum(s, sum(v, discostco2(i,v,r,t) * XL(s,i,v,r,t)) * hours(s)) + sum(v, XC.L(i,v,r,t) * 1e+3 *  fomcost(i,v,r)))
                                                / sum(i$sameas(i,"Lignite"), sum(s,                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity3_rpt(t,r,"cost-ccs")$(sum(ccs(i), sum(s, sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(ccs(i), sum(s, sum(v, discostco2(i,v,r,t) * XL(s,i,v,r,t)) * hours(s)) + sum(v, XC.L(i,v,r,t) * 1e+3 *  fomcost(i,v,r)))
                                                / sum(ccs(i), sum(s,                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
                                                

$if set profits     $include    modules\euregen2024_profits_rpt_v1                                

Electricity1_total_rpt(t,"price-avg")            = sum(r, sum(s,             price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r, sum(s,             load_res(s,r,t) * hours(s))) + eps ;
Electricity1_total_rpt(t,"price-avg-winter")     = sum(r, sum(s$ssea(s,"w"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r, sum(s$ssea(s,"w"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_total_rpt(t,"price-avg-summer")     = sum(r, sum(s$ssea(s,"s"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r, sum(s$ssea(s,"s"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_total_rpt(t,"price-avg-midseason")  = sum(r, sum(s$ssea(s,"m"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r, sum(s$ssea(s,"m"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_total_rpt(t,"price-avg-janmar")     = sum(r, sum(s$ssea(s,"strm"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r, sum(s$ssea(s,"strm"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_total_rpt(t,"price-avg-janapr")     = sum(r, sum(s$ssea(s,"stra"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r, sum(s$ssea(s,"stra"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_total_rpt(t,"price-max")            = smax((s,r), price(s,r,t)) + eps ;
Electricity1_total_rpt(t,"price-min")            = smin((s,r), price(s,r,t)) + eps ;
Electricity1_total_rpt(t,"elec-demand")          = sum((s,r), load_res(s,r,t) * hours(s)) * 1e-3  + eps ;
Electricity1_total_rpt(t,"elec-demand-ave")      = sum((s,r), load_res(s,r,t) * hours(s))/8760  + eps ;

$if      set elastic                   Electricity1_total_rpt(t,"price-avg")           = sum(r, sum(s,             price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r, sum(s,             sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic                   Electricity1_total_rpt(t,"price-avg-winter")    = sum(r, sum(s$ssea(s,"w"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r, sum(s$ssea(s,"w"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic                   Electricity1_total_rpt(t,"price-avg-summer")     = sum(r, sum(s$ssea(s,"s"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r, sum(s$ssea(s,"s"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic                   Electricity1_total_rpt(t,"price-avg-midseason")  = sum(r, sum(s$ssea(s,"m"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r, sum(s$ssea(s,"m"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic                   Electricity1_total_rpt(t,"price-avg-janmar")     = sum(r, sum(s$ssea(s,"strm"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r, sum(s$ssea(s,"strm"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic                   Electricity1_total_rpt(t,"price-avg-janapr")     = sum(r, sum(s$ssea(s,"stra"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r, sum(s$ssea(s,"stra"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic                   Electricity1_total_rpt(t,"price-max")            = smax((s,r), price(s,r,t)) + eps ;
$if      set elastic                   Electricity1_total_rpt(t,"price-min")            = smin((s,r), price(s,r,t)) + eps ;
$if      set elastic                   Electricity1_total_rpt(t,"elec-demand")          = sum((s,r), sum(bse, DS.L(bse,s,r,t)) * hours(s)) * 1e-3  + eps ;
$if      set elastic                   Electricity1_total_rpt(t,"elec-demand-ave")      = sum((s,r), sum(bse, DS.L(bse,s,r,t)) * hours(s))/8760  + eps ;

Electricity1_total_rpt(t,"elec-demand-org")      = sum((s,r), load_res(s,r,t) * hours(s)) * 1e-3  + eps ;
Electricity1_total_rpt(t,"elec-demand-ave-org")  = sum((s,r), load_res(s,r,t) * hours(s))/8760  + eps ;


Electricity2_total_rpt("2020","price-nuc") = eps ;
Electricity3_total_rpt("2020","cost-nuc") = eps ;
Electricity2_total_rpt("2021","price-nuc") = eps ;
Electricity3_total_rpt("2021","cost-nuc") = eps ;

Electricity2_total_rpt(t,"price-nuc")$(sum(nuc(i), sum((s,r), sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(nuc(i), sum((s,r), price(s,r,t) * sum(v, XL(s,i,v,r,t)) * hours(s)))
                                                / sum(nuc(i), sum((s,r),                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity2_total_rpt(t,"price-sol")$(sum(sol(i), sum((s,r), sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(sol(i), sum((s,r), price(s,r,t) * sum(v, XL(s,i,v,r,t)) * hours(s)))
                                                / sum(sol(i), sum((s,r),                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity2_total_rpt(t,"price-wind")$(sum(wind(i), sum((s,r), sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(wind(i), sum((s,r), price(s,r,t) * sum(v, XL(s,i,v,r,t)) * hours(s)))
                                                / sum(wind(i), sum((s,r),                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity2_total_rpt(t,"price-windon")$(sum(windon(i), sum((s,r), sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(windon(i), sum((s,r), price(s,r,t) * sum(v, XL(s,i,v,r,t)) * hours(s)))
                                                / sum(windon(i), sum((s,r),                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity2_total_rpt(t,"price-windoff")$(sum(windoff(i), sum((s,r), sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(windoff(i), sum((s,r), price(s,r,t) * sum(v, XL(s,i,v,r,t)) * hours(s)))
                                                / sum(windoff(i), sum((s,r),                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity2_total_rpt(t,"price-bio")$(sum(bio(i), sum((s,r), sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(bio(i), sum((s,r), price(s,r,t) * sum(v, XL(s,i,v,r,t)) * hours(s)))
                                                / sum(bio(i), sum((s,r),                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;                                                
Electricity2_total_rpt(t,"price-gas")$(sum(gas(i), sum((s,r), sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(gas(i), sum((s,r), price(s,r,t) * sum(v, XL(s,i,v,r,t)) * hours(s)))
                                                / sum(gas(i), sum((s,r),                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity2_total_rpt(t,"price-coa")$(sum(i$sameas(i,"Coal"), sum((s,r), sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(i$sameas(i,"Coal"), sum((s,r), price(s,r,t) * sum(v, XL(s,i,v,r,t)) * hours(s)))
                                                / sum(i$sameas(i,"Coal"), sum((s,r),                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity2_total_rpt(t,"price-lig")$(sum(i$sameas(i,"Lignite"), sum((s,r), sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(i$sameas(i,"Lignite"), sum((s,r), price(s,r,t) * sum(v, XL(s,i,v,r,t)) * hours(s)))
                                                / sum(i$sameas(i,"Lignite"), sum((s,r),                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity2_total_rpt(t,"price-ccs")$(sum(ccs(i), sum((s,r), sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(ccs(i), sum((s,r), price(s,r,t) * sum(v, XL(s,i,v,r,t)) * hours(s)))
                                                / sum(ccs(i), sum((s,r),                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
                                                
                                                
Electricity3_total_rpt(t,"cost-nuc")$(sum(nuc(i), sum((s,r), sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(nuc(i), sum((s,r), sum(v, discostco2(i,v,r,t) * XL(s,i,v,r,t)) * hours(s)) + sum((v,r), XC.L(i,v,r,t) * 1e+3 *  fomcost(i,v,r)))
                                                / sum(nuc(i), sum((s,r),                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity3_total_rpt(t,"cost-sol")$(sum(sol(i), sum((s,r), sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(sol(i), sum((s,r), sum(v, discostco2(i,v,r,t) * XL(s,i,v,r,t)) * hours(s)) + sum((v,r), XC.L(i,v,r,t) * 1e+3 *  fomcost(i,v,r)))
                                                / sum(sol(i), sum((s,r),                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity3_total_rpt(t,"cost-wind")$(sum(wind(i), sum((s,r), sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(wind(i), sum((s,r), sum(v, discostco2(i,v,r,t) * XL(s,i,v,r,t)) * hours(s)) + sum((v,r), XC.L(i,v,r,t) * 1e+3 *  fomcost(i,v,r)))
                                                / sum(wind(i), sum((s,r),                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity3_total_rpt(t,"cost-windon")$(sum(windon(i), sum((s,r), sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(windon(i), sum((s,r), sum(v, discostco2(i,v,r,t) * XL(s,i,v,r,t)) * hours(s)) + sum((v,r), XC.L(i,v,r,t) * 1e+3 *  fomcost(i,v,r)))
                                                / sum(windon(i), sum((s,r),                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity3_total_rpt(t,"cost-windoff")$(sum(windoff(i), sum((s,r), sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(windoff(i), sum((s,r), sum(v, discostco2(i,v,r,t) * XL(s,i,v,r,t)) * hours(s)) + sum((v,r), XC.L(i,v,r,t) * 1e+3 *  fomcost(i,v,r)))
                                                / sum(windoff(i), sum((s,r),                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity3_total_rpt(t,"cost-bio")$(sum(bio(i), sum((s,r), sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(bio(i), sum((s,r), sum(v, discostco2(i,v,r,t) * XL(s,i,v,r,t)) * hours(s)) + sum((v,r), XC.L(i,v,r,t) * 1e+3 *  fomcost(i,v,r)))
                                                / sum(bio(i), sum((s,r),                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;                                                
Electricity3_total_rpt(t,"cost-gas")$(sum(gas(i), sum((s,r), sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(gas(i), sum((s,r), sum(v, discostco2(i,v,r,t) * XL(s,i,v,r,t)) * hours(s)) + sum((v,r), XC.L(i,v,r,t) * 1e+3 *  fomcost(i,v,r)))
                                                / sum(gas(i), sum((s,r),                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity3_total_rpt(t,"cost-coa")$(sum(i$sameas(i,"Coal"), sum((s,r), sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(i$sameas(i,"Coal"), sum((s,r), sum(v, discostco2(i,v,r,t) * XL(s,i,v,r,t)) * hours(s)) + sum((v,r), XC.L(i,v,r,t) * 1e+3 *  fomcost(i,v,r)))
                                                / sum(i$sameas(i,"Coal"), sum((s,r),                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity3_total_rpt(t,"cost-lig")$(sum(i$sameas(i,"Lignite"), sum((s,r), sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(i$sameas(i,"Lignite"), sum((s,r), sum(v, discostco2(i,v,r,t) * XL(s,i,v,r,t)) * hours(s)) + sum((v,r), XC.L(i,v,r,t) * 1e+3 *  fomcost(i,v,r)))
                                                / sum(i$sameas(i,"Lignite"), sum((s,r),                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity3_total_rpt(t,"cost-lig")$(sum(i$sameas(i,"Lignite"), sum((s,r), sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(i$sameas(i,"Lignite"), sum((s,r), sum(v, discostco2(i,v,r,t) * XL(s,i,v,r,t)) * hours(s)) + sum((v,r), XC.L(i,v,r,t) * 1e+3 *  fomcost(i,v,r)))
                                                / sum(i$sameas(i,"Lignite"), sum((s,r),                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
Electricity3_total_rpt(t,"cost-ccs")$(sum(ccs(i), sum((s,r), sum(v, XL(s,i,v,r,t)) * hours(s))) > 0)
                                                = sum(ccs(i), sum((s,r), sum(v, discostco2(i,v,r,t) * XL(s,i,v,r,t)) * hours(s)) + sum((v,r), XC.L(i,v,r,t) * 1e+3 *  fomcost(i,v,r)))
                                                / sum(ccs(i), sum((s,r),                sum(v, XL(s,i,v,r,t)) * hours(s))) + eps ;
                                        

Electricity1_GER_rpt(t,"price-avg")              = sum(r$sameas(r,"Germany"), sum(s,             price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Germany"), sum(s,             load_res(s,r,t) * hours(s))) + eps ;
Electricity1_GER_rpt(t,"price-avg-winter")       = sum(r$sameas(r,"Germany"), sum(s$ssea(s,"w"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Germany"), sum(s$ssea(s,"w"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_GER_rpt(t,"price-avg-summer")       = sum(r$sameas(r,"Germany"), sum(s$ssea(s,"s"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Germany"), sum(s$ssea(s,"s"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_GER_rpt(t,"price-avg-midseason")    = sum(r$sameas(r,"Germany"), sum(s$ssea(s,"m"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Germany"), sum(s$ssea(s,"m"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_GER_rpt(t,"price-avg-janmar")       = sum(r$sameas(r,"Germany"), sum(s$ssea(s,"strm"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Germany"), sum(s$ssea(s,"strm"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_GER_rpt(t,"price-avg-janapr")       = sum(r$sameas(r,"Germany"), sum(s$ssea(s,"stra"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Germany"), sum(s$ssea(s,"stra"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_GER_rpt(t,"price-max")              = smax((s,r)$sameas(r,"Germany"), price(s,r,t)) + eps ;
Electricity1_GER_rpt(t,"price-min")              = smin((s,r)$sameas(r,"Germany"), price(s,r,t)) + eps ;
Electricity1_GER_rpt(t,"elec-demand")            = sum((s,r)$sameas(r,"Germany"), load_res(s,r,t) * hours(s)) * 1e-3  + eps ;

Electricity2_GER_rpt(t,"price-nuc")              = Electricity2_rpt(t,"Germany","price-nuc") + eps ;
Electricity2_GER_rpt(t,"price-sol")              = Electricity2_rpt(t,"Germany","price-sol") + eps ;
Electricity2_GER_rpt(t,"price-wind")             = Electricity2_rpt(t,"Germany","price-wind") + eps ;
Electricity2_GER_rpt(t,"price-windon")           = Electricity2_rpt(t,"Germany","price-windon") + eps ;
Electricity2_GER_rpt(t,"price-windoff")          = Electricity2_rpt(t,"Germany","price-windoff") + eps ;
Electricity2_GER_rpt(t,"price-bio")              = Electricity2_rpt(t,"Germany","price-bio") + eps ;
Electricity2_GER_rpt(t,"price-gas")              = Electricity2_rpt(t,"Germany","price-gas") + eps ;
Electricity2_GER_rpt(t,"price-coa")              = Electricity2_rpt(t,"Germany","price-coa") + eps ;
Electricity2_GER_rpt(t,"price-lig")              = Electricity2_rpt(t,"Germany","price-lig") + eps ;
Electricity2_GER_rpt(t,"price-ccs")              = Electricity2_rpt(t,"Germany","price-ccs") + eps ;

Electricity3_GER_rpt(t,"cost-nuc")              = Electricity3_rpt(t,"Germany","cost-nuc") + eps ;
Electricity3_GER_rpt(t,"cost-sol")              = Electricity3_rpt(t,"Germany","cost-sol") + eps ;
Electricity3_GER_rpt(t,"cost-wind")             = Electricity3_rpt(t,"Germany","cost-wind") + eps ;
Electricity3_GER_rpt(t,"cost-windon")           = Electricity3_rpt(t,"Germany","cost-windon") + eps ;
Electricity3_GER_rpt(t,"cost-windoff")          = Electricity3_rpt(t,"Germany","cost-windoff") + eps ;
Electricity3_GER_rpt(t,"cost-bio")              = Electricity3_rpt(t,"Germany","cost-bio") + eps ;
Electricity3_GER_rpt(t,"cost-gas")              = Electricity3_rpt(t,"Germany","cost-gas") + eps ;
Electricity3_GER_rpt(t,"cost-coa")              = Electricity3_rpt(t,"Germany","cost-coa") + eps ;
Electricity3_GER_rpt(t,"cost-lig")              = Electricity3_rpt(t,"Germany","cost-lig") + eps ;
Electricity3_GER_rpt(t,"cost-ccs")              = Electricity3_rpt(t,"Germany","cost-ccs") + eps ;

parameter
Electricity1_FR_rpt(t,*)
Electricity2_FR_rpt(t,*)
Electricity3_FR_rpt(t,*)
;

Electricity1_FR_rpt(t,"price-avg")              = sum(r$sameas(r,"France"), sum(s,             price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"France"), sum(s,             load_res(s,r,t) * hours(s))) + eps ;
Electricity1_FR_rpt(t,"price-avg-winter")       = sum(r$sameas(r,"France"), sum(s$ssea(s,"w"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"France"), sum(s$ssea(s,"w"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_FR_rpt(t,"price-avg-summer")       = sum(r$sameas(r,"France"), sum(s$ssea(s,"s"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"France"), sum(s$ssea(s,"s"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_FR_rpt(t,"price-avg-midseason")    = sum(r$sameas(r,"France"), sum(s$ssea(s,"m"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"France"), sum(s$ssea(s,"m"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_FR_rpt(t,"price-avg-janmar")       = sum(r$sameas(r,"France"), sum(s$ssea(s,"strm"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"France"), sum(s$ssea(s,"strm"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_FR_rpt(t,"price-avg-janapr")       = sum(r$sameas(r,"France"), sum(s$ssea(s,"stra"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"France"), sum(s$ssea(s,"stra"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_FR_rpt(t,"price-max")              = smax((s,r)$sameas(r,"France"), price(s,r,t)) + eps ;
Electricity1_FR_rpt(t,"price-min")              = smin((s,r)$sameas(r,"France"), price(s,r,t)) + eps ;
Electricity1_FR_rpt(t,"elec-demand")            = sum((s,r)$sameas(r,"France"), load_res(s,r,t) * hours(s)) * 1e-3  + eps ;

Electricity2_FR_rpt(t,"price-nuc")              = Electricity2_rpt(t,"France","price-nuc") + eps ;
Electricity2_FR_rpt(t,"price-sol")              = Electricity2_rpt(t,"France","price-sol") + eps ;
Electricity2_FR_rpt(t,"price-wind")             = Electricity2_rpt(t,"France","price-wind") + eps ;
Electricity2_FR_rpt(t,"price-windon")           = Electricity2_rpt(t,"France","price-windon") + eps ;
Electricity2_FR_rpt(t,"price-windoff")          = Electricity2_rpt(t,"France","price-windoff") + eps ;
Electricity2_FR_rpt(t,"price-bio")              = Electricity2_rpt(t,"France","price-bio") + eps ;
Electricity2_FR_rpt(t,"price-gas")              = Electricity2_rpt(t,"France","price-gas") + eps ;
Electricity2_FR_rpt(t,"price-coa")              = Electricity2_rpt(t,"France","price-coa") + eps ;
Electricity2_FR_rpt(t,"price-lig")              = Electricity2_rpt(t,"France","price-lig") + eps ;
Electricity2_FR_rpt(t,"price-ccs")              = Electricity2_rpt(t,"France","price-ccs") + eps ;

Electricity3_FR_rpt(t,"cost-nuc")              = Electricity3_rpt(t,"France","cost-nuc") + eps ;
Electricity3_FR_rpt(t,"cost-sol")              = Electricity3_rpt(t,"France","cost-sol") + eps ;
Electricity3_FR_rpt(t,"cost-wind")             = Electricity3_rpt(t,"France","cost-wind") + eps ;
Electricity3_FR_rpt(t,"cost-windon")           = Electricity3_rpt(t,"France","cost-windon") + eps ;
Electricity3_FR_rpt(t,"cost-windoff")          = Electricity3_rpt(t,"France","cost-windoff") + eps ;
Electricity3_FR_rpt(t,"cost-bio")              = Electricity3_rpt(t,"France","cost-bio") + eps ;
Electricity3_FR_rpt(t,"cost-gas")              = Electricity3_rpt(t,"France","cost-gas") + eps ;
Electricity3_FR_rpt(t,"cost-coa")              = Electricity3_rpt(t,"France","cost-coa") + eps ;
Electricity3_FR_rpt(t,"cost-lig")              = Electricity3_rpt(t,"France","cost-lig") + eps ;
Electricity3_FR_rpt(t,"cost-ccs")              = Electricity3_rpt(t,"France","cost-ccs") + eps ;

$ontext
parameter
Electricity1_UK_rpt(t,*)
Electricity2_UK_rpt(t,*)
Electricity3_UK_rpt(t,*)
;

Electricity1_UK_rpt(t,"price-avg")              = sum(r$sameas(r,"Britain"), sum(s,             price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Britain"), sum(s,             load_res(s,r,t) * hours(s))) + eps ;
Electricity1_UK_rpt(t,"price-avg-winter")       = sum(r$sameas(r,"Britain"), sum(s$ssea(s,"w"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Britain"), sum(s$ssea(s,"w"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_UK_rpt(t,"price-avg-summer")       = sum(r$sameas(r,"Britain"), sum(s$ssea(s,"s"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Britain"), sum(s$ssea(s,"s"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_UK_rpt(t,"price-avg-midseason")    = sum(r$sameas(r,"Britain"), sum(s$ssea(s,"m"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Britain"), sum(s$ssea(s,"m"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_UK_rpt(t,"price-avg-janmar")       = sum(r$sameas(r,"Britain"), sum(s$ssea(s,"strm"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Britain"), sum(s$ssea(s,"strm"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_UK_rpt(t,"price-avg-janapr")       = sum(r$sameas(r,"Britain"), sum(s$ssea(s,"stra"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Britain"), sum(s$ssea(s,"stra"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_UK_rpt(t,"price-max")              = smax((s,r)$sameas(r,"Britain"), price(s,r,t)) + eps ;
Electricity1_UK_rpt(t,"price-min")              = smin((s,r)$sameas(r,"Britain"), price(s,r,t)) + eps ;
Electricity1_UK_rpt(t,"elec-demand")            = sum((s,r)$sameas(r,"Britain"), load_res(s,r,t) * hours(s)) * 1e-3  + eps ;

Electricity2_UK_rpt(t,"price-nuc")              = Electricity2_rpt(t,"Britain","price-nuc") + eps ;
Electricity2_UK_rpt(t,"price-sol")              = Electricity2_rpt(t,"Britain","price-sol") + eps ;
Electricity2_UK_rpt(t,"price-wind")             = Electricity2_rpt(t,"Britain","price-wind") + eps ;
Electricity2_UK_rpt(t,"price-windon")           = Electricity2_rpt(t,"Britain","price-windon") + eps ;
Electricity2_UK_rpt(t,"price-windoff")          = Electricity2_rpt(t,"Britain","price-windoff") + eps ;
Electricity2_UK_rpt(t,"price-bio")              = Electricity2_rpt(t,"Britain","price-bio") + eps ;
Electricity2_UK_rpt(t,"price-gas")              = Electricity2_rpt(t,"Britain","price-gas") + eps ;
Electricity2_UK_rpt(t,"price-coa")              = Electricity2_rpt(t,"Britain","price-coa") + eps ;
Electricity2_UK_rpt(t,"price-lig")              = Electricity2_rpt(t,"Britain","price-lig") + eps ;
Electricity2_UK_rpt(t,"price-ccs")              = Electricity2_rpt(t,"Britain","price-ccs") + eps ;

Electricity3_UK_rpt(t,"cost-nuc")              = Electricity3_rpt(t,"Britain","cost-nuc") + eps ;
Electricity3_UK_rpt(t,"cost-sol")              = Electricity3_rpt(t,"Britain","cost-sol") + eps ;
Electricity3_UK_rpt(t,"cost-wind")             = Electricity3_rpt(t,"Britain","cost-wind") + eps ;
Electricity3_UK_rpt(t,"cost-windon")           = Electricity3_rpt(t,"Britain","cost-windon") + eps ;
Electricity3_UK_rpt(t,"cost-windoff")          = Electricity3_rpt(t,"Britain","cost-windoff") + eps ;
Electricity3_UK_rpt(t,"cost-bio")              = Electricity3_rpt(t,"Britain","cost-bio") + eps ;
Electricity3_UK_rpt(t,"cost-gas")              = Electricity3_rpt(t,"Britain","cost-gas") + eps ;
Electricity3_UK_rpt(t,"cost-coa")              = Electricity3_rpt(t,"Britain","cost-coa") + eps ;
Electricity3_UK_rpt(t,"cost-lig")              = Electricity3_rpt(t,"Britain","cost-lig") + eps ;
Electricity3_UK_rpt(t,"cost-ccs")              = Electricity3_rpt(t,"Britain","cost-ccs") + eps ;

parameter
Electricity1_DK_rpt(t,*)
Electricity2_DK_rpt(t,*)
Electricity3_DK_rpt(t,*)
;

Electricity1_DK_rpt(t,"price-avg")              = sum(r$sameas(r,"Denmark"), sum(s,             price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Denmark"), sum(s,             load_res(s,r,t) * hours(s))) + eps ;
Electricity1_DK_rpt(t,"price-avg-winter")       = sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"w"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"w"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_DK_rpt(t,"price-avg-summer")       = sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"s"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"s"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_DK_rpt(t,"price-avg-midseason")    = sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"m"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"m"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_DK_rpt(t,"price-avg-janmar")       = sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"strm"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"strm"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_DK_rpt(t,"price-avg-janapr")       = sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"stra"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"stra"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_DK_rpt(t,"price-max")              = smax((s,r)$sameas(r,"Denmark"), price(s,r,t)) + eps ;
Electricity1_DK_rpt(t,"price-min")              = smin((s,r)$sameas(r,"Denmark"), price(s,r,t)) + eps ;
Electricity1_DK_rpt(t,"elec-demand")            = sum((s,r)$sameas(r,"Denmark"), load_res(s,r,t) * hours(s)) * 1e-3  + eps ;

Electricity2_DK_rpt(t,"price-nuc")              = Electricity2_rpt(t,"Denmark","price-nuc") + eps ;
Electricity2_DK_rpt(t,"price-sol")              = Electricity2_rpt(t,"Denmark","price-sol") + eps ;
Electricity2_DK_rpt(t,"price-wind")             = Electricity2_rpt(t,"Denmark","price-wind") + eps ;
Electricity2_DK_rpt(t,"price-windon")           = Electricity2_rpt(t,"Denmark","price-windon") + eps ;
Electricity2_DK_rpt(t,"price-windoff")          = Electricity2_rpt(t,"Denmark","price-windoff") + eps ;
Electricity2_DK_rpt(t,"price-bio")              = Electricity2_rpt(t,"Denmark","price-bio") + eps ;
Electricity2_DK_rpt(t,"price-gas")              = Electricity2_rpt(t,"Denmark","price-gas") + eps ;
Electricity2_DK_rpt(t,"price-coa")              = Electricity2_rpt(t,"Denmark","price-coa") + eps ;
Electricity2_DK_rpt(t,"price-lig")              = Electricity2_rpt(t,"Denmark","price-lig") + eps ;
Electricity2_DK_rpt(t,"price-ccs")              = Electricity2_rpt(t,"Denmark","price-ccs") + eps ;

Electricity3_DK_rpt(t,"cost-nuc")              = Electricity3_rpt(t,"Denmark","cost-nuc") + eps ;
Electricity3_DK_rpt(t,"cost-sol")              = Electricity3_rpt(t,"Denmark","cost-sol") + eps ;
Electricity3_DK_rpt(t,"cost-wind")             = Electricity3_rpt(t,"Denmark","cost-wind") + eps ;
Electricity3_DK_rpt(t,"cost-windon")           = Electricity3_rpt(t,"Denmark","cost-windon") + eps ;
Electricity3_DK_rpt(t,"cost-windoff")          = Electricity3_rpt(t,"Denmark","cost-windoff") + eps ;
Electricity3_DK_rpt(t,"cost-bio")              = Electricity3_rpt(t,"Denmark","cost-bio") + eps ;
Electricity3_DK_rpt(t,"cost-gas")              = Electricity3_rpt(t,"Denmark","cost-gas") + eps ;
Electricity3_DK_rpt(t,"cost-coa")              = Electricity3_rpt(t,"Denmark","cost-coa") + eps ;
Electricity3_DK_rpt(t,"cost-lig")              = Electricity3_rpt(t,"Denmark","cost-lig") + eps ;
Electricity3_DK_rpt(t,"cost-ccs")              = Electricity3_rpt(t,"Denmark","cost-ccs") + eps ;

parameter
Electricity1_NO_rpt(t,*)
Electricity2_NO_rpt(t,*)
Electricity3_NO_rpt(t,*)
;

Electricity1_NO_rpt(t,"price-avg")              = sum(r$sameas(r,"Norway"), sum(s,             price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Norway"), sum(s,             load_res(s,r,t) * hours(s))) + eps ;
Electricity1_NO_rpt(t,"price-avg-winter")       = sum(r$sameas(r,"Norway"), sum(s$ssea(s,"w"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Norway"), sum(s$ssea(s,"w"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_NO_rpt(t,"price-avg-summer")       = sum(r$sameas(r,"Norway"), sum(s$ssea(s,"s"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Norway"), sum(s$ssea(s,"s"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_NO_rpt(t,"price-avg-midseason")    = sum(r$sameas(r,"Norway"), sum(s$ssea(s,"m"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Norway"), sum(s$ssea(s,"m"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_NO_rpt(t,"price-avg-janmar")       = sum(r$sameas(r,"Norway"), sum(s$ssea(s,"strm"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Norway"), sum(s$ssea(s,"strm"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_NO_rpt(t,"price-avg-janapr")       = sum(r$sameas(r,"Norway"), sum(s$ssea(s,"stra"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Norway"), sum(s$ssea(s,"stra"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_NO_rpt(t,"price-max")              = smax((s,r)$sameas(r,"Norway"), price(s,r,t)) + eps ;
Electricity1_NO_rpt(t,"price-min")              = smin((s,r)$sameas(r,"Norway"), price(s,r,t)) + eps ;
Electricity1_NO_rpt(t,"elec-demand")            = sum((s,r)$sameas(r,"Norway"), load_res(s,r,t) * hours(s)) * 1e-3  + eps ;

Electricity2_NO_rpt(t,"price-nuc")              = Electricity2_rpt(t,"Norway","price-nuc") + eps ;
Electricity2_NO_rpt(t,"price-sol")              = Electricity2_rpt(t,"Norway","price-sol") + eps ;
Electricity2_NO_rpt(t,"price-wind")             = Electricity2_rpt(t,"Norway","price-wind") + eps ;
Electricity2_NO_rpt(t,"price-windon")           = Electricity2_rpt(t,"Norway","price-windon") + eps ;
Electricity2_NO_rpt(t,"price-windoff")          = Electricity2_rpt(t,"Norway","price-windoff") + eps ;
Electricity2_NO_rpt(t,"price-bio")              = Electricity2_rpt(t,"Norway","price-bio") + eps ;
Electricity2_NO_rpt(t,"price-gas")              = Electricity2_rpt(t,"Norway","price-gas") + eps ;
Electricity2_NO_rpt(t,"price-coa")              = Electricity2_rpt(t,"Norway","price-coa") + eps ;
Electricity2_NO_rpt(t,"price-lig")              = Electricity2_rpt(t,"Norway","price-lig") + eps ;
Electricity2_NO_rpt(t,"price-ccs")              = Electricity2_rpt(t,"Norway","price-ccs") + eps ;

Electricity3_NO_rpt(t,"cost-nuc")              = Electricity3_rpt(t,"Norway","cost-nuc") + eps ;
Electricity3_NO_rpt(t,"cost-sol")              = Electricity3_rpt(t,"Norway","cost-sol") + eps ;
Electricity3_NO_rpt(t,"cost-wind")             = Electricity3_rpt(t,"Norway","cost-wind") + eps ;
Electricity3_NO_rpt(t,"cost-windon")           = Electricity3_rpt(t,"Norway","cost-windon") + eps ;
Electricity3_NO_rpt(t,"cost-windoff")          = Electricity3_rpt(t,"Norway","cost-windoff") + eps ;
Electricity3_NO_rpt(t,"cost-bio")              = Electricity3_rpt(t,"Norway","cost-bio") + eps ;
Electricity3_NO_rpt(t,"cost-gas")              = Electricity3_rpt(t,"Norway","cost-gas") + eps ;
Electricity3_NO_rpt(t,"cost-coa")              = Electricity3_rpt(t,"Norway","cost-coa") + eps ;
Electricity3_NO_rpt(t,"cost-lig")              = Electricity3_rpt(t,"Norway","cost-lig") + eps ;
Electricity3_NO_rpt(t,"cost-ccs")              = Electricity3_rpt(t,"Norway","cost-ccs") + eps ;

parameter
Electricity1_PL_rpt(t,*)
Electricity2_PL_rpt(t,*)
Electricity3_PL_rpt(t,*)
;

Electricity1_PL_rpt(t,"price-avg")              = sum(r$sameas(r,"Poland"), sum(s,             price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Poland"), sum(s,             load_res(s,r,t) * hours(s))) + eps ;
Electricity1_PL_rpt(t,"price-avg-winter")       = sum(r$sameas(r,"Poland"), sum(s$ssea(s,"w"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Poland"), sum(s$ssea(s,"w"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_PL_rpt(t,"price-avg-summer")       = sum(r$sameas(r,"Poland"), sum(s$ssea(s,"s"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Poland"), sum(s$ssea(s,"s"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_PL_rpt(t,"price-avg-midseason")    = sum(r$sameas(r,"Poland"), sum(s$ssea(s,"m"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Poland"), sum(s$ssea(s,"m"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_PL_rpt(t,"price-avg-janmar")       = sum(r$sameas(r,"Poland"), sum(s$ssea(s,"strm"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Poland"), sum(s$ssea(s,"strm"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_PL_rpt(t,"price-avg-janapr")       = sum(r$sameas(r,"Poland"), sum(s$ssea(s,"stra"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Poland"), sum(s$ssea(s,"stra"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_PL_rpt(t,"price-max")              = smax((s,r)$sameas(r,"Poland"), price(s,r,t)) + eps ;
Electricity1_PL_rpt(t,"price-min")              = smin((s,r)$sameas(r,"Poland"), price(s,r,t)) + eps ;
Electricity1_PL_rpt(t,"elec-demand")            = sum((s,r)$sameas(r,"Poland"), load_res(s,r,t) * hours(s)) * 1e-3  + eps ;

Electricity2_PL_rpt(t,"price-nuc")              = Electricity2_rpt(t,"Poland","price-nuc") + eps ;
Electricity2_PL_rpt(t,"price-sol")              = Electricity2_rpt(t,"Poland","price-sol") + eps ;
Electricity2_PL_rpt(t,"price-wind")             = Electricity2_rpt(t,"Poland","price-wind") + eps ;
Electricity2_PL_rpt(t,"price-windon")           = Electricity2_rpt(t,"Poland","price-windon") + eps ;
Electricity2_PL_rpt(t,"price-windoff")          = Electricity2_rpt(t,"Poland","price-windoff") + eps ;
Electricity2_PL_rpt(t,"price-bio")              = Electricity2_rpt(t,"Poland","price-bio") + eps ;
Electricity2_PL_rpt(t,"price-gas")              = Electricity2_rpt(t,"Poland","price-gas") + eps ;
Electricity2_PL_rpt(t,"price-coa")              = Electricity2_rpt(t,"Poland","price-coa") + eps ;
Electricity2_PL_rpt(t,"price-lig")              = Electricity2_rpt(t,"Poland","price-lig") + eps ;
Electricity2_PL_rpt(t,"price-ccs")              = Electricity2_rpt(t,"Poland","price-ccs") + eps ;

Electricity3_PL_rpt(t,"cost-nuc")              = Electricity3_rpt(t,"Poland","cost-nuc") + eps ;
Electricity3_PL_rpt(t,"cost-sol")              = Electricity3_rpt(t,"Poland","cost-sol") + eps ;
Electricity3_PL_rpt(t,"cost-wind")             = Electricity3_rpt(t,"Poland","cost-wind") + eps ;
Electricity3_PL_rpt(t,"cost-windon")           = Electricity3_rpt(t,"Poland","cost-windon") + eps ;
Electricity3_PL_rpt(t,"cost-windoff")          = Electricity3_rpt(t,"Poland","cost-windoff") + eps ;
Electricity3_PL_rpt(t,"cost-bio")              = Electricity3_rpt(t,"Poland","cost-bio") + eps ;
Electricity3_PL_rpt(t,"cost-gas")              = Electricity3_rpt(t,"Poland","cost-gas") + eps ;
Electricity3_PL_rpt(t,"cost-coa")              = Electricity3_rpt(t,"Poland","cost-coa") + eps ;
Electricity3_PL_rpt(t,"cost-lig")              = Electricity3_rpt(t,"Poland","cost-lig") + eps ;
Electricity3_PL_rpt(t,"cost-ccs")              = Electricity3_rpt(t,"Poland","cost-ccs") + eps ;

parameter
Electricity1_ES_rpt(t,*)
Electricity2_ES_rpt(t,*)
Electricity3_ES_rpt(t,*)
;

Electricity1_ES_rpt(t,"price-avg")              = sum(r$sameas(r,"Spain"), sum(s,             price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Spain"), sum(s,             load_res(s,r,t) * hours(s))) + eps ;
Electricity1_ES_rpt(t,"price-avg-winter")       = sum(r$sameas(r,"Spain"), sum(s$ssea(s,"w"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Spain"), sum(s$ssea(s,"w"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_ES_rpt(t,"price-avg-summer")       = sum(r$sameas(r,"Spain"), sum(s$ssea(s,"s"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Spain"), sum(s$ssea(s,"s"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_ES_rpt(t,"price-avg-midseason")    = sum(r$sameas(r,"Spain"), sum(s$ssea(s,"m"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Spain"), sum(s$ssea(s,"m"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_ES_rpt(t,"price-avg-janmar")       = sum(r$sameas(r,"Spain"), sum(s$ssea(s,"strm"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Spain"), sum(s$ssea(s,"strm"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_ES_rpt(t,"price-avg-janapr")       = sum(r$sameas(r,"Spain"), sum(s$ssea(s,"stra"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Spain"), sum(s$ssea(s,"stra"), load_res(s,r,t) * hours(s))) + eps ;
Electricity1_ES_rpt(t,"price-max")              = smax((s,r)$sameas(r,"Spain"), price(s,r,t)) + eps ;
Electricity1_ES_rpt(t,"price-min")              = smin((s,r)$sameas(r,"Spain"), price(s,r,t)) + eps ;
Electricity1_ES_rpt(t,"elec-demand")            = sum((s,r)$sameas(r,"Spain"), load_res(s,r,t) * hours(s)) * 1e-3  + eps ;

Electricity2_ES_rpt(t,"price-nuc")              = Electricity2_rpt(t,"Spain","price-nuc") + eps ;
Electricity2_ES_rpt(t,"price-sol")              = Electricity2_rpt(t,"Spain","price-sol") + eps ;
Electricity2_ES_rpt(t,"price-wind")             = Electricity2_rpt(t,"Spain","price-wind") + eps ;
Electricity2_ES_rpt(t,"price-windon")           = Electricity2_rpt(t,"Spain","price-windon") + eps ;
Electricity2_ES_rpt(t,"price-windoff")          = Electricity2_rpt(t,"Spain","price-windoff") + eps ;
Electricity2_ES_rpt(t,"price-bio")              = Electricity2_rpt(t,"Spain","price-bio") + eps ;
Electricity2_ES_rpt(t,"price-gas")              = Electricity2_rpt(t,"Spain","price-gas") + eps ;
Electricity2_ES_rpt(t,"price-coa")              = Electricity2_rpt(t,"Spain","price-coa") + eps ;
Electricity2_ES_rpt(t,"price-lig")              = Electricity2_rpt(t,"Spain","price-lig") + eps ;
Electricity2_ES_rpt(t,"price-ccs")              = Electricity2_rpt(t,"Spain","price-ccs") + eps ;

Electricity3_ES_rpt(t,"cost-nuc")              = Electricity3_rpt(t,"Spain","cost-nuc") + eps ;
Electricity3_ES_rpt(t,"cost-sol")              = Electricity3_rpt(t,"Spain","cost-sol") + eps ;
Electricity3_ES_rpt(t,"cost-wind")             = Electricity3_rpt(t,"Spain","cost-wind") + eps ;
Electricity3_ES_rpt(t,"cost-windon")           = Electricity3_rpt(t,"Spain","cost-windon") + eps ;
Electricity3_ES_rpt(t,"cost-windoff")          = Electricity3_rpt(t,"Spain","cost-windoff") + eps ;
Electricity3_ES_rpt(t,"cost-bio")              = Electricity3_rpt(t,"Spain","cost-bio") + eps ;
Electricity3_ES_rpt(t,"cost-gas")              = Electricity3_rpt(t,"Spain","cost-gas") + eps ;
Electricity3_ES_rpt(t,"cost-coa")              = Electricity3_rpt(t,"Spain","cost-coa") + eps ;
Electricity3_ES_rpt(t,"cost-lig")              = Electricity3_rpt(t,"Spain","cost-lig") + eps ;
Electricity3_ES_rpt(t,"cost-ccs")              = Electricity3_rpt(t,"Spain","cost-ccs") + eps ;
$offtext

* * * Newdemand
$if      set newdemand         Electricity1_GER_rpt(t,"price-avg")              = sum(r$sameas(r,"Germany"), sum(s,             price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Germany"), sum(s,             load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_GER_rpt(t,"price-avg-winter")       = sum(r$sameas(r,"Germany"), sum(s$ssea(s,"w"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Germany"), sum(s$ssea(s,"w"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_GER_rpt(t,"price-avg-summer")       = sum(r$sameas(r,"Germany"), sum(s$ssea(s,"s"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Germany"), sum(s$ssea(s,"s"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_GER_rpt(t,"price-avg-midseason")    = sum(r$sameas(r,"Germany"), sum(s$ssea(s,"m"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Germany"), sum(s$ssea(s,"m"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_GER_rpt(t,"price-avg-janmar")       = sum(r$sameas(r,"Germany"), sum(s$ssea(s,"strm"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Germany"), sum(s$ssea(s,"strm"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_GER_rpt(t,"price-avg-janapr")       = sum(r$sameas(r,"Germany"), sum(s$ssea(s,"stra"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Germany"), sum(s$ssea(s,"stra"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_GER_rpt(t,"price-max")              = smax((s,r)$sameas(r,"Germany"), price(s,r,t)) + eps ;
$if      set newdemand         Electricity1_GER_rpt(t,"price-min")              = smin((s,r)$sameas(r,"Germany"), price(s,r,t)) + eps ;
$if      set newdemand         Electricity1_GER_rpt(t,"elec-demand")            = sum((s,r)$sameas(r,"Germany"), load_res(s,r,t) * hours(s)) * 1e-3  + eps ;

$if      set newdemand         Electricity1_FR_rpt(t,"price-avg")              = sum(r$sameas(r,"France"), sum(s,             price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"France"), sum(s,             load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_FR_rpt(t,"price-avg-winter")       = sum(r$sameas(r,"France"), sum(s$ssea(s,"w"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"France"), sum(s$ssea(s,"w"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_FR_rpt(t,"price-avg-summer")       = sum(r$sameas(r,"France"), sum(s$ssea(s,"s"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"France"), sum(s$ssea(s,"s"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_FR_rpt(t,"price-avg-midseason")    = sum(r$sameas(r,"France"), sum(s$ssea(s,"m"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"France"), sum(s$ssea(s,"m"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_FR_rpt(t,"price-avg-janmar")       = sum(r$sameas(r,"France"), sum(s$ssea(s,"strm"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"France"), sum(s$ssea(s,"strm"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_FR_rpt(t,"price-avg-janapr")       = sum(r$sameas(r,"France"), sum(s$ssea(s,"stra"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"France"), sum(s$ssea(s,"stra"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_FR_rpt(t,"price-max")              = smax((s,r)$sameas(r,"France"), price(s,r,t)) + eps ;
$if      set newdemand         Electricity1_FR_rpt(t,"price-min")              = smin((s,r)$sameas(r,"France"), price(s,r,t)) + eps ;
$if      set newdemand         Electricity1_FR_rpt(t,"elec-demand")            = sum((s,r)$sameas(r,"France"), load_res(s,r,t) * hours(s)) * 1e-3  + eps ;

$if      set newdemand         Electricity1_UK_rpt(t,"price-avg")              = sum(r$sameas(r,"Britain"), sum(s,             price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Britain"), sum(s,             load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_UK_rpt(t,"price-avg-winter")       = sum(r$sameas(r,"Britain"), sum(s$ssea(s,"w"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Britain"), sum(s$ssea(s,"w"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_UK_rpt(t,"price-avg-summer")       = sum(r$sameas(r,"Britain"), sum(s$ssea(s,"s"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Britain"), sum(s$ssea(s,"s"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_UK_rpt(t,"price-avg-midseason")    = sum(r$sameas(r,"Britain"), sum(s$ssea(s,"m"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Britain"), sum(s$ssea(s,"m"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_UK_rpt(t,"price-avg-janmar")       = sum(r$sameas(r,"Britain"), sum(s$ssea(s,"strm"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Britain"), sum(s$ssea(s,"strm"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_UK_rpt(t,"price-avg-janapr")       = sum(r$sameas(r,"Britain"), sum(s$ssea(s,"stra"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Britain"), sum(s$ssea(s,"stra"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_UK_rpt(t,"price-max")              = smax((s,r)$sameas(r,"Britain"), price(s,r,t)) + eps ;
$if      set newdemand         Electricity1_UK_rpt(t,"price-min")              = smin((s,r)$sameas(r,"Britain"), price(s,r,t)) + eps ;
$if      set newdemand         Electricity1_UK_rpt(t,"elec-demand")            = sum((s,r)$sameas(r,"Britain"), load_res(s,r,t) * hours(s)) * 1e-3  + eps ;

$if      set newdemand         Electricity1_DK_rpt(t,"price-avg")              = sum(r$sameas(r,"Denmark"), sum(s,             price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Denmark"), sum(s,             load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_DK_rpt(t,"price-avg-winter")       = sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"w"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"w"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_DK_rpt(t,"price-avg-summer")       = sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"s"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"s"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_DK_rpt(t,"price-avg-midseason")    = sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"m"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"m"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_DK_rpt(t,"price-avg-janmar")       = sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"strm"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"strm"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_DK_rpt(t,"price-avg-janapr")       = sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"stra"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"stra"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_DK_rpt(t,"price-max")              = smax((s,r)$sameas(r,"Denmark"), price(s,r,t)) + eps ;
$if      set newdemand         Electricity1_DK_rpt(t,"price-min")              = smin((s,r)$sameas(r,"Denmark"), price(s,r,t)) + eps ;
$if      set newdemand         Electricity1_DK_rpt(t,"elec-demand")            = sum((s,r)$sameas(r,"Denmark"), load_res(s,r,t) * hours(s)) * 1e-3  + eps ;

$if      set newdemand         Electricity1_NO_rpt(t,"price-avg")              = sum(r$sameas(r,"Norway"), sum(s,             price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Norway"), sum(s,             load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_NO_rpt(t,"price-avg-winter")       = sum(r$sameas(r,"Norway"), sum(s$ssea(s,"w"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Norway"), sum(s$ssea(s,"w"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_NO_rpt(t,"price-avg-summer")       = sum(r$sameas(r,"Norway"), sum(s$ssea(s,"s"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Norway"), sum(s$ssea(s,"s"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_NO_rpt(t,"price-avg-midseason")    = sum(r$sameas(r,"Norway"), sum(s$ssea(s,"m"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Norway"), sum(s$ssea(s,"m"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_NO_rpt(t,"price-avg-janmar")       = sum(r$sameas(r,"Norway"), sum(s$ssea(s,"strm"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Norway"), sum(s$ssea(s,"strm"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_NO_rpt(t,"price-avg-janapr")       = sum(r$sameas(r,"Norway"), sum(s$ssea(s,"stra"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Norway"), sum(s$ssea(s,"stra"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_NO_rpt(t,"price-max")              = smax((s,r)$sameas(r,"Norway"), price(s,r,t)) + eps ;
$if      set newdemand         Electricity1_NO_rpt(t,"price-min")              = smin((s,r)$sameas(r,"Norway"), price(s,r,t)) + eps ;
$if      set newdemand         Electricity1_NO_rpt(t,"elec-demand")            = sum((s,r)$sameas(r,"Norway"), load_res(s,r,t) * hours(s)) * 1e-3  + eps ;

$if      set newdemand         Electricity1_PL_rpt(t,"price-avg")              = sum(r$sameas(r,"Poland"), sum(s,             price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Poland"), sum(s,             load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_PL_rpt(t,"price-avg-winter")       = sum(r$sameas(r,"Poland"), sum(s$ssea(s,"w"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Poland"), sum(s$ssea(s,"w"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_PL_rpt(t,"price-avg-summer")       = sum(r$sameas(r,"Poland"), sum(s$ssea(s,"s"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Poland"), sum(s$ssea(s,"s"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_PL_rpt(t,"price-avg-midseason")    = sum(r$sameas(r,"Poland"), sum(s$ssea(s,"m"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Poland"), sum(s$ssea(s,"m"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_PL_rpt(t,"price-avg-janmar")       = sum(r$sameas(r,"Poland"), sum(s$ssea(s,"strm"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Poland"), sum(s$ssea(s,"strm"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_PL_rpt(t,"price-avg-janapr")       = sum(r$sameas(r,"Poland"), sum(s$ssea(s,"stra"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Poland"), sum(s$ssea(s,"stra"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_PL_rpt(t,"price-max")              = smax((s,r)$sameas(r,"Poland"), price(s,r,t)) + eps ;
$if      set newdemand         Electricity1_PL_rpt(t,"price-min")              = smin((s,r)$sameas(r,"Poland"), price(s,r,t)) + eps ;
$if      set newdemand         Electricity1_PL_rpt(t,"elec-demand")            = sum((s,r)$sameas(r,"Poland"), load_res(s,r,t) * hours(s)) * 1e-3  + eps ;

$if      set newdemand         Electricity1_ES_rpt(t,"price-avg")              = sum(r$sameas(r,"Spain"), sum(s,             price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Spain"), sum(s,             load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_ES_rpt(t,"price-avg-winter")       = sum(r$sameas(r,"Spain"), sum(s$ssea(s,"w"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Spain"), sum(s$ssea(s,"w"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_ES_rpt(t,"price-avg-summer")       = sum(r$sameas(r,"Spain"), sum(s$ssea(s,"s"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Spain"), sum(s$ssea(s,"s"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_ES_rpt(t,"price-avg-midseason")    = sum(r$sameas(r,"Spain"), sum(s$ssea(s,"m"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Spain"), sum(s$ssea(s,"m"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_ES_rpt(t,"price-avg-janmar")       = sum(r$sameas(r,"Spain"), sum(s$ssea(s,"strm"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Spain"), sum(s$ssea(s,"strm"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_ES_rpt(t,"price-avg-janapr")       = sum(r$sameas(r,"Spain"), sum(s$ssea(s,"stra"), price(s,r,t) * load_res(s,r,t) * hours(s))) / sum(r$sameas(r,"Spain"), sum(s$ssea(s,"stra"), load_res(s,r,t) * hours(s))) + eps ;
$if      set newdemand         Electricity1_ES_rpt(t,"price-max")              = smax((s,r)$sameas(r,"Spain"), price(s,r,t)) + eps ;
$if      set newdemand         Electricity1_ES_rpt(t,"price-min")              = smin((s,r)$sameas(r,"Spain"), price(s,r,t)) + eps ;
$if      set newdemand         Electricity1_ES_rpt(t,"elec-demand")            = sum((s,r)$sameas(r,"Spain"), load_res(s,r,t) * hours(s)) * 1e-3  + eps ;

* * * Elastic
$if      set elastic Electricity1_GER_rpt(t,"price-avg")              = sum(r$sameas(r,"Germany"), sum(s,             price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Germany"), sum(s,             sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_GER_rpt(t,"price-avg-winter")       = sum(r$sameas(r,"Germany"), sum(s$ssea(s,"w"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Germany"), sum(s$ssea(s,"w"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_GER_rpt(t,"price-avg-summer")       = sum(r$sameas(r,"Germany"), sum(s$ssea(s,"s"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Germany"), sum(s$ssea(s,"s"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_GER_rpt(t,"price-avg-midseason")    = sum(r$sameas(r,"Germany"), sum(s$ssea(s,"m"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Germany"), sum(s$ssea(s,"m"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_GER_rpt(t,"price-avg-janmar")       = sum(r$sameas(r,"Germany"), sum(s$ssea(s,"strm"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Germany"), sum(s$ssea(s,"strm"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_GER_rpt(t,"price-avg-janapr")       = sum(r$sameas(r,"Germany"), sum(s$ssea(s,"stra"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Germany"), sum(s$ssea(s,"stra"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_GER_rpt(t,"price-max")              = smax((s,r)$sameas(r,"Germany"), price(s,r,t)) + eps ;
$if      set elastic Electricity1_GER_rpt(t,"price-min")              = smin((s,r)$sameas(r,"Germany"), price(s,r,t)) + eps ;
$if      set elastic Electricity1_GER_rpt(t,"elec-demand")            = sum((s,r)$sameas(r,"Germany"), sum(bse, DS.L(bse,s,r,t)) * hours(s)) * 1e-3  + eps ;

$if      set elastic Electricity1_FR_rpt(t,"price-avg")              = sum(r$sameas(r,"France"), sum(s,             price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"France"), sum(s,             sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_FR_rpt(t,"price-avg-winter")       = sum(r$sameas(r,"France"), sum(s$ssea(s,"w"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"France"), sum(s$ssea(s,"w"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_FR_rpt(t,"price-avg-summer")       = sum(r$sameas(r,"France"), sum(s$ssea(s,"s"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"France"), sum(s$ssea(s,"s"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_FR_rpt(t,"price-avg-midseason")    = sum(r$sameas(r,"France"), sum(s$ssea(s,"m"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"France"), sum(s$ssea(s,"m"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_FR_rpt(t,"price-avg-janmar")       = sum(r$sameas(r,"France"), sum(s$ssea(s,"strm"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"France"), sum(s$ssea(s,"strm"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_FR_rpt(t,"price-avg-janapr")       = sum(r$sameas(r,"France"), sum(s$ssea(s,"stra"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"France"), sum(s$ssea(s,"stra"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_FR_rpt(t,"price-max")              = smax((s,r)$sameas(r,"France"), price(s,r,t)) + eps ;
$if      set elastic Electricity1_FR_rpt(t,"price-min")              = smin((s,r)$sameas(r,"France"), price(s,r,t)) + eps ;
$if      set elastic Electricity1_FR_rpt(t,"elec-demand")            = sum((s,r)$sameas(r,"France"), sum(bse, DS.L(bse,s,r,t)) * hours(s)) * 1e-3  + eps ;

$if      set elastic Electricity1_UK_rpt(t,"price-avg")              = sum(r$sameas(r,"Britain"), sum(s,             price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Britain"), sum(s,             sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_UK_rpt(t,"price-avg-winter")       = sum(r$sameas(r,"Britain"), sum(s$ssea(s,"w"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Britain"), sum(s$ssea(s,"w"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_UK_rpt(t,"price-avg-summer")       = sum(r$sameas(r,"Britain"), sum(s$ssea(s,"s"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Britain"), sum(s$ssea(s,"s"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_UK_rpt(t,"price-avg-midseason")    = sum(r$sameas(r,"Britain"), sum(s$ssea(s,"m"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Britain"), sum(s$ssea(s,"m"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_UK_rpt(t,"price-avg-janmar")       = sum(r$sameas(r,"Britain"), sum(s$ssea(s,"strm"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Britain"), sum(s$ssea(s,"strm"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_UK_rpt(t,"price-avg-janapr")       = sum(r$sameas(r,"Britain"), sum(s$ssea(s,"stra"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Britain"), sum(s$ssea(s,"stra"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_UK_rpt(t,"price-max")              = smax((s,r)$sameas(r,"Britain"), price(s,r,t)) + eps ;
$if      set elastic Electricity1_UK_rpt(t,"price-min")              = smin((s,r)$sameas(r,"Britain"), price(s,r,t)) + eps ;
$if      set elastic Electricity1_UK_rpt(t,"elec-demand")            = sum((s,r)$sameas(r,"Britain"), sum(bse, DS.L(bse,s,r,t)) * hours(s)) * 1e-3  + eps ;

$if      set elastic Electricity1_DK_rpt(t,"price-avg")              = sum(r$sameas(r,"Denmark"), sum(s,             price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Denmark"), sum(s,             sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_DK_rpt(t,"price-avg-winter")       = sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"w"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"w"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_DK_rpt(t,"price-avg-summer")       = sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"s"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"s"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_DK_rpt(t,"price-avg-midseason")    = sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"m"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"m"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_DK_rpt(t,"price-avg-janmar")       = sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"strm"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"strm"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_DK_rpt(t,"price-avg-janapr")       = sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"stra"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Denmark"), sum(s$ssea(s,"stra"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_DK_rpt(t,"price-max")              = smax((s,r)$sameas(r,"Denmark"), price(s,r,t)) + eps ;
$if      set elastic Electricity1_DK_rpt(t,"price-min")              = smin((s,r)$sameas(r,"Denmark"), price(s,r,t)) + eps ;
$if      set elastic Electricity1_DK_rpt(t,"elec-demand")            = sum((s,r)$sameas(r,"Denmark"), sum(bse, DS.L(bse,s,r,t)) * hours(s)) * 1e-3  + eps ;

$if      set elastic Electricity1_NO_rpt(t,"price-avg")              = sum(r$sameas(r,"Norway"), sum(s,             price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Norway"), sum(s,             sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_NO_rpt(t,"price-avg-winter")       = sum(r$sameas(r,"Norway"), sum(s$ssea(s,"w"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Norway"), sum(s$ssea(s,"w"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_NO_rpt(t,"price-avg-summer")       = sum(r$sameas(r,"Norway"), sum(s$ssea(s,"s"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Norway"), sum(s$ssea(s,"s"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_NO_rpt(t,"price-avg-midseason")    = sum(r$sameas(r,"Norway"), sum(s$ssea(s,"m"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Norway"), sum(s$ssea(s,"m"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_NO_rpt(t,"price-avg-janmar")       = sum(r$sameas(r,"Norway"), sum(s$ssea(s,"strm"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Norway"), sum(s$ssea(s,"strm"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_NO_rpt(t,"price-avg-janapr")       = sum(r$sameas(r,"Norway"), sum(s$ssea(s,"stra"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Norway"), sum(s$ssea(s,"stra"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_NO_rpt(t,"price-max")              = smax((s,r)$sameas(r,"Norway"), price(s,r,t)) + eps ;
$if      set elastic Electricity1_NO_rpt(t,"price-min")              = smin((s,r)$sameas(r,"Norway"), price(s,r,t)) + eps ;
$if      set elastic Electricity1_NO_rpt(t,"elec-demand")            = sum((s,r)$sameas(r,"Norway"), sum(bse, DS.L(bse,s,r,t)) * hours(s)) * 1e-3  + eps ;

$if      set elastic Electricity1_PL_rpt(t,"price-avg")              = sum(r$sameas(r,"Poland"), sum(s,             price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Poland"), sum(s,             sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_PL_rpt(t,"price-avg-winter")       = sum(r$sameas(r,"Poland"), sum(s$ssea(s,"w"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Poland"), sum(s$ssea(s,"w"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_PL_rpt(t,"price-avg-summer")       = sum(r$sameas(r,"Poland"), sum(s$ssea(s,"s"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Poland"), sum(s$ssea(s,"s"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_PL_rpt(t,"price-avg-midseason")    = sum(r$sameas(r,"Poland"), sum(s$ssea(s,"m"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Poland"), sum(s$ssea(s,"m"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_PL_rpt(t,"price-avg-janmar")       = sum(r$sameas(r,"Poland"), sum(s$ssea(s,"strm"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Poland"), sum(s$ssea(s,"strm"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_PL_rpt(t,"price-avg-janapr")       = sum(r$sameas(r,"Poland"), sum(s$ssea(s,"stra"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Poland"), sum(s$ssea(s,"stra"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_PL_rpt(t,"price-max")              = smax((s,r)$sameas(r,"Poland"), price(s,r,t)) + eps ;
$if      set elastic Electricity1_PL_rpt(t,"price-min")              = smin((s,r)$sameas(r,"Poland"), price(s,r,t)) + eps ;
$if      set elastic Electricity1_PL_rpt(t,"elec-demand")            = sum((s,r)$sameas(r,"Poland"), sum(bse, DS.L(bse,s,r,t)) * hours(s)) * 1e-3  + eps ;

$if      set elastic Electricity1_ES_rpt(t,"price-avg")              = sum(r$sameas(r,"Spain"), sum(s,             price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Spain"), sum(s,             sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_ES_rpt(t,"price-avg-winter")       = sum(r$sameas(r,"Spain"), sum(s$ssea(s,"w"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Spain"), sum(s$ssea(s,"w"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_ES_rpt(t,"price-avg-summer")       = sum(r$sameas(r,"Spain"), sum(s$ssea(s,"s"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Spain"), sum(s$ssea(s,"s"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_ES_rpt(t,"price-avg-midseason")    = sum(r$sameas(r,"Spain"), sum(s$ssea(s,"m"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Spain"), sum(s$ssea(s,"m"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_ES_rpt(t,"price-avg-janmar")       = sum(r$sameas(r,"Spain"), sum(s$ssea(s,"strm"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Spain"), sum(s$ssea(s,"strm"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_ES_rpt(t,"price-avg-janapr")       = sum(r$sameas(r,"Spain"), sum(s$ssea(s,"stra"), price(s,r,t) * sum(bse, DS.L(bse,s,r,t)) * hours(s))) / sum(r$sameas(r,"Spain"), sum(s$ssea(s,"stra"), sum(bse, DS.L(bse,s,r,t)) * hours(s))) + eps ;
$if      set elastic Electricity1_ES_rpt(t,"price-max")              = smax((s,r)$sameas(r,"Spain"), price(s,r,t)) + eps ;
$if      set elastic Electricity1_ES_rpt(t,"price-min")              = smin((s,r)$sameas(r,"Spain"), price(s,r,t)) + eps ;
$if      set elastic Electricity1_ES_rpt(t,"elec-demand")            = sum((s,r)$sameas(r,"Spain"), sum(bse, DS.L(bse,s,r,t)) * hours(s)) * 1e-3  + eps ;

* * * Electricity generation reporting
Parameters
gen(i,r,t)                                       Generation by technology (TWh)
gen_type(type,r,t)                               Generation by type (TWh)
gen_xtype(tyrpt,r,t)                             Generation by xtype (TWh)
ElectricityGeneration_rpt(t,r,i)                 Electricity generation reporting vector (TWh)
ElectricityGeneration_total_rpt(t,i)             Electricity generation reporting vector (TWh)
ElectricityGeneration_type_rpt(t,r,type)         Electricity generation reporting vector (TWh)
ElectricityGeneration_total_type_rpt(t,type)     Electricity generation reporting vector (TWh)
ElectricityGeneration_xtype_rpt(t,r,tyrpt)       Electricity generation reporting vector (TWh)
ElectricityGeneration_total_xtype_rpt(t,tyrpt)   Electricity generation reporting vector (TWh)
ElectricityGeneration_GER_xtype_rpt(t,tyrpt)     Electricity generation reporting vector (TWh)
ElectricityGeneration_FR_xtype_rpt(t,tyrpt)     Electricity generation reporting vector (TWh)
;

gen(i,r,t)               = sum(ivrt(i,v,r,t), XTWHL(i,v,r,t)) ;
gen_type(type,r,t)       = sum(idef(i,type), gen(i,r,t));
gen_xtype(tyrpt,r,t)     = sum(xtyperpt(tyrpt,type), gen_type(type,r,t)) ;

ElectricityGeneration_rpt(t,r,i)                 = gen(i,r,t) ;
ElectricityGeneration_total_rpt(t,i)             = sum(r, gen(i,r,t)) ;
ElectricityGeneration_type_rpt(t,r,type)         = gen_type(type,r,t) ;
ElectricityGeneration_total_type_rpt(t,type)     = sum(r, gen_type(type,r,t)) ;
ElectricityGeneration_xtype_rpt(t,r,tyrpt)       = gen_xtype(tyrpt,r,t) ;
ElectricityGeneration_total_xtype_rpt(t,tyrpt)   = sum(r, gen_xtype(tyrpt,r,t)) + eps ;
ElectricityGeneration_GER_xtype_rpt(t,tyrpt)     = sum(r$sameas(r,"Germany"), gen_xtype(tyrpt,r,t)) + eps ;
ElectricityGeneration_FR_xtype_rpt(t,tyrpt)      = sum(r$sameas(r,"France"), gen_xtype(tyrpt,r,t)) + eps ;

* * * Intalled capacities reporting
Parameters
installed(i,r,t)                                 Installed capacity by region (GW)
installed_type(type,r,t)                         Installed capacity by region and type (GW)
InstalledCapacities_rpt(t,r,i)                   Installed capacities reporting vector (GW)
InstalledCapacities_total_rpt(t,i)               Installed capacities reporting vector (GW)
InstalledCapacities_type_rpt(t,r,type)           Installed capacities reporting vector (GW)
InstalledCapacities_total_type_rpt(t,type)       Installed capacities reporting vector (GW)
InstalledCapacities_xtype_rpt(t,r,tyrpt)         Installed capacities reporting vector (GW)
InstalledCapacities_total_xtype_rpt(t,tyrpt)     Installed capacities reporting vector (GW)
InstalledCapacities_GER_xtype_rpt(t,tyrpt)       Installed capacities reporting vector (GW)
InstalledCapacities_FR_xtype_rpt(t,tyrpt)        Installed capacities reporting vector (GW)
;

installed(i,r,t)         = sum(ivrt(i,v,r,t), XC.L(i,v,r,t)) ;
installed_type(type,r,t) = sum(idef(i,type), installed(i,r,t)) ;

InstalledCapacities_rpt(t,r,i)                   = installed(i,r,t) ;
InstalledCapacities_total_rpt(t,i)                = sum(r, installed(i,r,t)) ;
InstalledCapacities_type_rpt(t,r,type)           = installed_type(type,r,t) ;
InstalledCapacities_total_type_rpt(t,type)       = sum(r, installed_type(type,r,t)) ;
InstalledCapacities_xtype_rpt(t,r,tyrpt)         = sum(xtyperpt(tyrpt,type), installed_type(type,r,t)) ;
InstalledCapacities_total_xtype_rpt(t,tyrpt)     = sum(r, sum(xtyperpt(tyrpt,type), installed_type(type,r,t))) + eps ;
InstalledCapacities_GER_xtype_rpt(t,tyrpt)       = sum(r$sameas(r,"Germany"), sum(xtyperpt(tyrpt,type), installed_type(type,r,t))) + eps ;
InstalledCapacities_FR_xtype_rpt(t,tyrpt)        = sum(r$sameas(r,"France"), sum(xtyperpt(tyrpt,type), installed_type(type,r,t))) + eps ;

* * * Added capacities reporting
Parameters
added(i,r,t)                                     Added capacity by region (GW)
added_type(type,r,t)                             Added capacity by region and type (GW)
AddedCapacities_rpt(t,r,i)                       Added capacities reporting vector (GW)
AddedCapacities_total_rpt(t,i)                   Added capacities reporting vector (GW)
AddedCapacities_type_rpt(t,r,type)               Added capacities reporting vector (GW)
AddedCapacities_total_type_rpt(t,type)           Added capacities reporting vector (GW)
AddedCapacities_xtype_rpt(t,r,tyrpt)             Added capacities reporting vector (GW)
AddedCapacities_total_xtype_rpt(t,tyrpt)         Added capacities reporting vector (GW)
AddedCapacities_GER_xtype_rpt(t,tyrpt)           Added capacities reporting vector (GW)
AddedCapacities_FR_xtype_rpt(t,tyrpt)            Added capacities reporting vector (GW)
;

added(i,r,t)         = IX.L(i,r,t) ;
added_type(type,r,t) = sum(idef(i,type), added(i,r,t)) ;

AddedCapacities_rpt(t,r,i)                      = added(i,r,t) ;
AddedCapacities_total_rpt(t,i)                  = sum(r, added(i,r,t)) ;
AddedCapacities_type_rpt(t,r,type)              = added_type(type,r,t) ;
AddedCapacities_total_type_rpt(t,type)          = sum(r, added_type(type,r,t)) ;
AddedCapacities_xtype_rpt(t,r,tyrpt)            = sum(xtyperpt(tyrpt,type), added_type(type,r,t)) ;
AddedCapacities_total_xtype_rpt(t,tyrpt)        = sum(r, sum(xtyperpt(tyrpt,type), added_type(type,r,t))) + eps ;
AddedCapacities_GER_xtype_rpt(t,tyrpt)          = sum(r$sameas(r,"Germany"), sum(xtyperpt(tyrpt,type), added_type(type,r,t))) + eps ;
AddedCapacities_FR_xtype_rpt(t,tyrpt)           = sum(r$sameas(r,"France"), sum(xtyperpt(tyrpt,type), added_type(type,r,t))) + eps ;

parameters
AccAddedCapacities_xtype_rpt(t,r,tyrpt)
AccAddedCapacities_total_xtype_rpt(t,tyrpt)
AccAddedCapacities_GER_xtype_rpt(t,tyrpt)
AccAddedCapacities_FR_xtype_rpt(t,tyrpt)
;

AccAddedCapacities_xtype_rpt(t,r,tyrpt)         = sum(tt$(tt.val le t.val), AddedCapacities_xtype_rpt(tt,r,tyrpt)) + eps ;
AccAddedCapacities_total_xtype_rpt(t,tyrpt)     = sum(tt$(tt.val le t.val), AddedCapacities_total_xtype_rpt(tt,tyrpt)) + eps ;
AccAddedCapacities_GER_xtype_rpt(t,tyrpt)       = sum(tt$(tt.val le t.val), AddedCapacities_GER_xtype_rpt(tt,tyrpt)) + eps ;
AccAddedCapacities_FR_xtype_rpt(t,tyrpt)        = sum(tt$(tt.val le t.val), AddedCapacities_FR_xtype_rpt(tt,tyrpt)) + eps ;

* * * Retired capacities reporting
parameters
retired(i,r,t)                                   Retired capacity by region (GW)
retired_type(type,r,t)                           Retired capacity by region and type (GW)
RetiredCapacities_rpt(t,r,i)                       Retired capacities reporting vector (GW)
RetiredCapacities_total_rpt(t,i)                 Retired capacities reporting vector (GW)
RetiredCapacities_type_rpt(t,r,type)               Retired capacities reporting vector (GW)
RetiredCapacities_total_type_rpt(t,type)         Retired capacities reporting vector (GW)
RetiredCapacities_xtype_rpt(t,r,tyrpt)             Retired capacities reporting vector (GW)
RetiredCapacities_total_xtype_rpt(t,tyrpt)       Retired capacities reporting vector (GW)
;

retired(i,r,"2020")            = sum(ivrt(i,v,r,"2020"), XC.L(i,v,r,"2020") - capt(i,v,r,"2023")) ;
retired(i,r,t)$(t.val > 2022)  = sum(ivrt(i,v,r,t)$(t.val > v.val), XC.L(i,v,r,t-1) - XC.L(i,v,r,t)) ;
retired_type(type,r,t)         = sum(idef(i,type), retired(i,r,t)) ;

RetiredCapacities_rpt(t,r,i)               = retired(i,r,t) ;
RetiredCapacities_total_rpt(t,i)           = sum(r, retired(i,r,t)) ;
RetiredCapacities_type_rpt(t,r,type)       = retired_type(type,r,t) ;
RetiredCapacities_total_type_rpt(t,type)   = sum(r, retired_type(type,r,t)) ;
RetiredCapacities_xtype_rpt(t,r,tyrpt)     = sum(xtyperpt(tyrpt,type), retired_type(type,r,t)) ;
RetiredCapacities_total_xtype_rpt(t,tyrpt) = sum(r, sum(xtyperpt(tyrpt,type), retired_type(type,r,t))) + eps ;

* * * RetiredEarly capacities reporting
parameters
retiredearly(i,r,t)                                      RetiredEarly capacity by region (GW)
retiredearly_type(type,r,t)                              RetiredEarly capacity by region and type (GW)
RetiredEarlyCapacities_rpt(t,r,i)                        RetiredEarly capacities reporting vector (GW)
RetiredEarlyCapacities_total_rpt(t,i)                  RetiredEarly capacities reporting vector (GW)
RetiredEarlyCapacities_type_rpt(t,r,type)                RetiredEarly capacities reporting vector (GW)
RetiredEarlyCapacities_total_type_rpt(t,type)          RetiredEarly capacities reporting vector (GW)
RetiredEarlyCapacities_xtype_rpt(t,r,tyrpt)              RetiredEarly capacities reporting vector (GW)
RetiredEarlyCapacities_total_xtype_rpt(t,tyrpt)        RetiredEarly capacities reporting vector (GW)
;

retiredearly(i,r,"2020")            = sum(ivrt(i,v,r,"2020"), XC.L(i,v,r,"2020") - capt(i,v,r,"2023")) ;
retiredearly(i,r,t)$(t.val > 2022)  = sum(ivrt(i,v,r,t)$(t.val > v.val), (XC.L(i,v,r,t-1) - XC.L(i,v,r,t)) * lifetime(i,v,r,t)) ;
retiredearly_type(type,r,t)         = sum(idef(i,type), retiredearly(i,r,t)) ;

RetiredEarlyCapacities_rpt(t,r,i)                        = retiredearly(i,r,t) ;
RetiredEarlyCapacities_total_rpt(t,i)                  = sum(r, retiredearly(i,r,t)) ;
RetiredEarlyCapacities_type_rpt(t,r,type)                = retiredearly_type(type,r,t) ;
RetiredEarlyCapacities_total_type_rpt(t,type)          = sum(r, retiredearly_type(type,r,t)) ;
RetiredEarlyCapacities_xtype_rpt(t,r,tyrpt)              = sum(xtyperpt(tyrpt,type), retiredearly_type(type,r,t)) ;
RetiredEarlyCapacities_total_xtype_rpt(t,tyrpt)        = sum(r, sum(xtyperpt(tyrpt,type), retiredearly_type(type,r,t))) + eps ;


* * * Cost caluclations
Parameters
inves_zeta(r,t)
inves_zeta_nodisc(r,t)
inves_normal(r,t)           Annual investment cost (EUR billions)
inves_annui(r,t)            Annual investment cost (EUR billions)
inves_ccost(r,t)            Annual investment cost (EUR billions)
inves_normal_nodisc(r,t)    Annual investment cost (EUR billions)
inves_annui_nodisc(r,t)     Annual investment cost (EUR billions)
inves_ccost_nodisc(r,t)     Annual investment cost (EUR billions)
fixed(r,t)                  Annual fixed cost (EUR billions)
varia(r,t)                  Annual dispatch cost(EUR billions)
lostl(r,t)                  Lost load 
carbt(r,t)                  Carbon tax (or EU ETS) revenue
airpt(r,t)                  Air pollution tax revenue
;

parameter
zeta_invir_rpt(inv,i,v,r)
zeta_invjr_rpt(inv,j,v,r)
zeta_invkr_rpt(inv,k,v,r)
zeta_invir_nodisc_rpt(inv,i,v,r)
zeta_invjr_nodisc_rpt(inv,j,v,r)
zeta_invkr_nodisc_rpt(inv,k,v,r)
;

$gdxin precal\precal_%n%.gdx
$load zeta_invir_rpt
$load zeta_invjr_rpt
$load zeta_invkr_rpt
$load zeta_invir_nodisc_rpt
$load zeta_invjr_nodisc_rpt
$load zeta_invkr_nodisc_rpt
$gdxin 


inves_zeta(r,t) =      1e-3 * sum(new(i),              sum(inv,  share(inv,i,r) * IX.L(i,r,t)    * sum(tv(t,v)$ivrt(i,v,r,t),  (capcost(i,v,r) + deccost(i,v,r)) * zeta_invir_rpt(inv,i,v,r)))) 
$if      set storage + 1e-3 * sum(newj(j),             sum(inv, gshare(inv,j,r) * IGL(j,r,t)    * sum(tv(t,v)$jvrt(j,v,r,t),  gcapcost(j,v,r)                   * zeta_invjr_rpt(inv,j,v,r)))) 
$if      set trans   + 1e-3 * sum((rr,k)$tmap(k,r,rr), sum(inv, tshare(inv,k,r) * IT.L(k,r,rr,t) * sum(tv(t,v)$tvrt(k,v,r,t),  tcapcost(k,r,rr)                  * zeta_invkr_rpt(inv,k,v,r))))
;

inves_zeta_nodisc(r,t) =   1e-3 * sum(new(i),              sum(inv,  share(inv,i,r) * IX.L(i,r,t)    * sum(tv(t,v)$ivrt(i,v,r,t),  (capcost(i,v,r) + deccost(i,v,r)) * zeta_invir_nodisc_rpt(inv,i,v,r)))) 
$if      set storage     + 1e-3 * sum(newj(j),             sum(inv, gshare(inv,j,r) * IGL(j,r,t)    * sum(tv(t,v)$jvrt(j,v,r,t),  gcapcost(j,v,r)                   * zeta_invjr_nodisc_rpt(inv,j,v,r)))) 
$if      set trans       + 1e-3 * sum((rr,k)$tmap(k,r,rr), sum(inv, tshare(inv,k,r) * IT.L(k,r,rr,t) * sum(tv(t,v)$tvrt(k,v,r,t),  tcapcost(k,r,rr)                  * zeta_invkr_nodisc_rpt(inv,k,v,r))))
;

inves_normal(r,t) =    1e-3 * sum(new,                                        IX.L(new,r,t)             * sum(tv(t,v)$ivrt(new,v,r,t),         capcost(new,v,r)           *  modeldepr(new,v,r,t) ))        * dfact(t) / nyrs(t)
$if      set storage + 1e-3 * sum(newj,                                       IGL(newj,r,t)            * sum(tv(t,v)$jvrt(newj,v,r,t),       gcapcost(newj,v,r)          * gmodeldepr(newj,v,r,t) ))       * dfact(t) / nyrs(t)
$if      set trans   + 1e-3 * sum((rr,k)$tmap(k,r,rr), IT.L(k,r,rr,t) * sum(tv(t,v)$tvrt(k,v,r,t), tcapcost(k,r,rr) * tmodeldepr(k,v,r,t) )) * dfact(t) / nyrs(t)
;

inves_normal_nodisc(r,t) =    1e-3 * sum(new,                                        IX.L(new,r,t)             * sum(tv(t,v)$ivrt(new,v,r,t),         capcost(new,v,r)           *  modeldepr_nodisc(new,v,r,t) ))
$if      set storage        + 1e-3 * sum(newj,                                       IGL(newj,r,t)            * sum(tv(t,v)$jvrt(newj,v,r,t),       gcapcost(newj,v,r)          * gmodeldepr_nodisc(newj,v,r,t) ))
$if      set trans          + 1e-3 * sum((rr,k)$tmap(k,r,rr), IT.L(k,r,rr,t) * sum(tv(t,v)$tvrt(k,v,r,t), tcapcost(k,r,rr) * tmodeldepr_nodisc(k,v,r,t) ))
;

inves_annui(r,t) =     1e-3 * sum(new,                                        sum((tt,v)$((tt.val le t.val) and tv(tt,v) and ivrt(new,v,r,tt)),        IX.L(new,r,tt)            *  capcost(new,v,r)           *  deprtime(new,v,r,tt)        *  annuity(new,v)          * dfact(t) ))
$if      set storage + 1e-3 * sum(newj,                                       sum((tt,v)$((tt.val le t.val) and tv(tt,v) and jvrt(newj,v,r,tt)),       IGL(newj,r,tt)           * gcapcost(newj,v,r)          * gdeprtime(newj,v,r,tt)       * gannuity(newj,v)       * dfact(t) ))
$if      set trans   + 1e-3 * sum((rr,k)$tmap(k,r,rr), sum((tt,v)$((tt.val le t.val) and tv(tt,v) and tvrt(k,v,r,tt)), IT.L(k,r,rr,t) * tcapcost(k,r,rr) * tdeprtime(k,v,r,tt) * tannuity(k) * dfact(t) ))
;

inves_annui_nodisc(r,t) =     1e-3 * sum(new,                                        sum((tt,v)$((tt.val le t.val) and tv(tt,v) and ivrt(new,v,r,tt)),        IX.L(new,r,tt)            *  capcost(new,v,r)           *  deprtime(new,v,r,tt)        *  annuity(new,v)          * nyrs(t) ))
$if      set storage        + 1e-3 * sum(newj,                                       sum((tt,v)$((tt.val le t.val) and tv(tt,v) and jvrt(newj,v,r,tt)),       IGL(newj,r,tt)           * gcapcost(newj,v,r)          * gdeprtime(newj,v,r,tt)       * gannuity(newj,v)       * nyrs(t) ))
$if      set trans          + 1e-3 * sum((rr,k)$tmap(k,r,rr), sum((tt,v)$((tt.val le t.val) and tv(tt,v) and tvrt(k,v,r,tt)), IT.L(k,r,rr,t) * tcapcost(k,r,rr) * tdeprtime(k,v,r,tt) * tannuity(k) * nyrs(t) ))
;

inves_ccost(r,t) =     1e-3 * sum(new,                                        sum((tt,v)$((tt.val le t.val) and tv(tt,v) and ivrt(new,v,r,tt)),        IX.L(new,r,tt)            *  capcost(new,v,r)           *  deprtime(new,v,r,tt)        * drate * dfact(t) ))
$if      set storage + 1e-3 * sum(newj,                                       sum((tt,v)$((tt.val le t.val) and tv(tt,v) and jvrt(newj,v,r,tt)),       IGL(newj,r,tt)           * gcapcost(newj,v,r)          * gdeprtime(newj,v,r,tt)       * drate * dfact(t) ))
$if      set trans   + 1e-3 * sum((rr,k)$tmap(k,r,rr), sum((tt,v)$((tt.val le t.val) and tv(tt,v) and tvrt(k,v,r,tt)), IT.L(k,r,rr,t) * tcapcost(k,r,rr) * tdeprtime(k,v,r,tt) * drate * dfact(t) ))
;

inves_ccost_nodisc(r,t) =     1e-3 * sum(new,                                        sum((tt,v)$((tt.val le t.val) and tv(tt,v) and ivrt(new,v,r,tt)),        IX.L(new,r,tt)            *  capcost(new,v,r)           *  deprtime(new,v,r,tt)        * drate * nyrs(t) ))
$if      set storage        + 1e-3 * sum(newj,                                       sum((tt,v)$((tt.val le t.val) and tv(tt,v) and jvrt(newj,v,r,tt)),       IGL(newj,r,tt)           * gcapcost(newj,v,r)          * gdeprtime(newj,v,r,tt)       * drate * nyrs(t) ))
$if      set trans          + 1e-3 * sum((rr,k)$tmap(k,r,rr), sum((tt,v)$((tt.val le t.val) and tv(tt,v) and tvrt(k,v,r,tt)), IT.L(k,r,rr,t) * tcapcost(k,r,rr) * tdeprtime(k,v,r,tt) * drate * nyrs(t) ))
;

fixed(r,t) =           1e-3 * sum(ivrt(i,v,r,t), XC.L(i,v,r,t) *  fomcost(i,v,r))
$if      set storage + 1e-3 * sum(jvrt(j,v,r,t), GCL(j,v,r,t) * gfomcost(j,v,r))
$if      set trans   + 1e-3 * sum((rr,k)$tcap(k,r,rr), (tcap(k,r,rr) + TC.L(k,r,rr,t)) * tfomcost(k,r,rr) )
;

varia(r,t) =           1e-3 * sum(ivrt(i,v,r,t), XTWHL(i,v,r,t) * discost(i,v,r,t))
$if      set storage + 1e-6 * sum(jvrt(j,v,r,t), sum(s, (GL(s,j,v,r,t) + GDL(s,j,v,r,t)) * hours(s)) * gvomcost(j,v,r))
$if      set trans   + 1e-6 * sum((rr,k)$tcap(k,r,rr), sum(s, E.L(s,k,r,rr,t)) * tvomcost(k,r,rr))
;

lostl(r,t) = 1e-6 * voll(r,t) * sum(s, BS.L(s,r,t) * hours(s)) ;
carbt(r,t) =
$if      set scc         1e-6 * dfact_scc(t)  / dfact(t) * sum(ivrt(i,v,r,t), scc_emit(i,v,r,t)  * sum(s, XL(s,i,v,r,t) * hours(s))) ;
$if not  set scc         1e-6 * sum(ivrt(i,v,r,t), co2prreg(r,t) * emit(i,v,r) * sum(s, XL(s,i,v,r,t) * hours(s))) ;
$if      set socialcost airpt(r,t) = 1e-6 * dfact_scap(t) / dfact(t) * sum(ivrt(i,v,r,t), scap_emit(i,v,r,t) * sum(s, XL(s,i,v,r,t) * hours(s))) ;

parameters
EmissionsCost_rpt(t,*)
EmissionsCost_Accumulated_rpt(t,*)
;

EmissionsCost_rpt(t,"CO2 emissions (annual)")            = sum(r, co2emit(r,t)) ;
EmissionsCost_rpt(t,"CO2 emissions captured (annual)")   = sum(r, co2capt(r,t)) + eps ;
EmissionsCost_rpt(t,"CO2 price")                         = co2prwei(t) ;
EmissionsCost_rpt(t,"CO2 emissions abated (annual)")     = sum(r, co2emit(r,"2020") - co2emit(r,t)) ;
EmissionsCost_rpt(t,"Cost (annual)")                     = sum(r, inves_annui_nodisc(r,t) / nyrs(t) + fixed(r,t) + varia(r,t)) ;
EmissionsCost_rpt(t,"Investment cost (annual)")          = sum(r, inves_annui_nodisc(r,t) / nyrs(t)) ;
EmissionsCost_rpt(t,"Fixed cost (annual)")               = sum(r, fixed(r,t)) ;
EmissionsCost_rpt(t,"Variable cost (annual)")            = sum(r, varia(r,t)) ;

EmissionsCost_Accumulated_rpt(t,"CO2 emissions")            = sum(tt$(tt.val le t.val), nyrs(tt) * sum(r, co2emit(r,tt))) ;
EmissionsCost_Accumulated_rpt(t,"CO2 emissions captured")   = sum(tt$(tt.val le t.val), nyrs(tt) * sum(r, co2capt(r,tt))) + eps ;
EmissionsCost_Accumulated_rpt(t,"CO2 price")                = co2prwei(t) ;
EmissionsCost_Accumulated_rpt(t,"CO2 emissions abated")     = sum(tt$(tt.val le t.val), nyrs(tt) * sum(r, co2emit(r,"2020") - co2emit(r,tt))) ;
EmissionsCost_Accumulated_rpt(t,"Cost")                     = sum(tt$(tt.val le t.val), nyrs(tt) * sum(r, inves_annui_nodisc(r,tt) / nyrs(tt) + fixed(r,tt) + varia(r,tt))) ;
EmissionsCost_Accumulated_rpt(t,"Investment cost")          = sum(tt$(tt.val le t.val), nyrs(tt) * sum(r, inves_annui_nodisc(r,tt) / nyrs(tt))) ;
EmissionsCost_Accumulated_rpt(t,"Fixed cost")               = sum(tt$(tt.val le t.val), nyrs(tt) * sum(r, fixed(r,tt))) ;
EmissionsCost_Accumulated_rpt(t,"Variable cost")            = sum(tt$(tt.val le t.val), nyrs(tt) * sum(r, varia(r,tt))) ;
*$offtext

* * * Curtailment reporting (of intermittent renewable energies)
parameters
curtailment(i,r,t)                       Curtailment by technology (TWh)
curtailment_type(type,r,t)               Curtailment by type (TWh)
Curtailment_rpt(t,r,i)                   Curtailment reporting vector (TWh)
Curtailment_total_rpt(t,i)               Curtailment reporting vector (TWh)
Curtailment_type_rpt(t,r,type)           Curtailment reporting vector (TWh)
Curtailment_total_type_rpt(t,type)       Curtailment reporting vector (TWh)
Curtailment_xtype_rpt(t,r,tyrpt)         Curtailment reporting vector (TWh)
Curtailment_total_xtype_rpt(t,tyrpt)     Curtailment reporting vector (TWh)
;

curtailment(i,r,t)$sum((s,v), vrsc(s,i,v,r))   = 1e-3 * sum((s,ivrt(i,v,r,t)), XC.L(i,v,r,t) * hours(s) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (vrsc(s,i,v,r)-1)$vrsc(s,i,v,r))) - gen(i,r,t) ;
curtailment_type(type,r,t)               = sum(idef(i,type), curtailment(i,r,t)) ;

Curtailment_rpt(t,r,i)                   = curtailment(i,r,t) ;
Curtailment_type_rpt(t,r,type)           = curtailment_type(type,r,t) ;
Curtailment_xtype_rpt(t,r,tyrpt)         = sum(xtyperpt(tyrpt,type), curtailment_type(type,r,t)) ;
Curtailment_total_rpt(t,i)               = sum(r, curtailment(i,r,t)) ;
Curtailment_total_type_rpt(t,type)       = sum(r, curtailment_type(type,r,t)) ;
Curtailment_total_xtype_rpt(t,tyrpt)     = sum(r, sum(xtyperpt(tyrpt,type), curtailment_type(type,r,t))) + eps ;

* * * Full-load hours generation
Parameters
flhgen(i,r,t)                               Full-load hours by technology (h)
flh_type(type,r,t)                       Full load hours by type (h)
FlhGeneration_rpt(t,r,i)                 Full-load hours generation reporting vector (h)
FlhGeneration_type_rpt(t,r,type)         Full-load hours generation reporting vector (h)
FlhGeneration_xtype_rpt(t,r,tyrpt)       Full-load hours generation reporting vector (h)
FlhGeneration_total_rpt(t,i)             Full-load hours generation reporting vector (h)
FlhGeneration_total_type_rpt(t,type)     Full-load hours generation reporting vector (h)
FlhGeneration_total_xtype_rpt(t,tyrpt)   Full-load hours generation reporting vector (h)
;

flhgen(i,r,t)$(installed(i,r,t) > 0.1)                                                      = 1e+3 * gen(i,r,t)             / installed(i,r,t) ;
flh_type(type,r,t)$(installed_type(type,r,t) > 0.1)                                      = 1e+3 * gen_type(type,r,t)     / installed_type(type,r,t) ;

FlhGeneration_rpt(t,r,i)                 = flhgen(i,r,t) ;
FlhGeneration_type_rpt(t,r,type)         = flh_type(type,r,t) ;
FlhGeneration_xtype_rpt(t,r,tyrpt)$(sum(xtyperpt(tyrpt,type), installed_type(type,r,t)) > 0.1)                   = 1e+3 * sum(xtyperpt(tyrpt,type), gen_type(type,r,t)) / sum(xtyperpt(tyrpt,type), installed_type(type,r,t)) ;
FlhGeneration_total_rpt(t,i)$(sum(r, installed(i,r,t)) > 0.1)                                                    = 1e+3 * sum(r, gen(i,r,t)) / sum(r, installed(i,r,t)) ;
FlhGeneration_total_type_rpt(t,type)$(sum(r, installed_type(type,r,t)) > 0.1)                                    = 1e+3 * sum(r, gen_type(type,r,t)) / sum(r, installed_type(type,r,t)) ;
FlhGeneration_total_xtype_rpt(t,tyrpt)$(sum(r, sum(xtyperpt(tyrpt,type), installed_type(type,r,t))) > 0.1)       = 1e+3 * sum(r, sum(xtyperpt(tyrpt,type), gen_type(type,r,t))) / sum(r, sum(xtyperpt(tyrpt,type), installed_type(type,r,t))) + eps ;

* * * Transfers reporting
parameters
transfers(s,k,r,r,t)          Transfers by technology (TWh)
imports(s,k,r,r,t)            Transfers by technology (TWh)
transfers_r(s,k,r,t)          Transfers by technology in one region (TWh)
imports_r(s,k,r,t)          Transfers by technology in one region (TWh)
transfers_ann(k,r,r,t)        Transfers by technology annual (TWh)
imports_ann(k,r,r,t)        Transfers by technology annual (TWh)
transfers_ann_r(k,r,t)        Transfers by technology annual in one region (TWh)
imports_ann_r(k,r,t)        Transfers by technology annual in one region (TWh)
domexport(s,r,r,t)                       Transfers (TWh)
domexport_r(s,r,t)                       Transfers in one region (TWh)
domimport(s,r,r,t)                       Transfers (TWh)
domimport_r(s,r,t)                       Transfers in one region (TWh)
domexport_ann(r,r,t)                     Transfers annual (TWh)
domexport_ann_r(r,t)                     Transfers annual in one region (TWh)
domimport_ann(r,r,t)     
domimport_ann_r(r,t)
Transfers_rpt(t,r,r)                     Transfers reporting vector (TWh)
Imports_rpt(t,r,r)
Transfers_r_rpt(t,r)                     Transfers reporting vector (TWh)
Imports_r_rpt(t,r)                     Transfers reporting vector (TWh)
Transfers_total_rpt(t)                   Transfers reporting vector (TWh)
Imports_total_rpt(t)
;

$if not  set trans      transfers(s,k,r,rr,t) = eps ;
$if      set trans      transfers(s,k,r,rr,t) = E.L(s,k,r,rr,t) * 1e-3 ;

transfers_r(s,k,r,t)    = sum(rr, transfers(s,k,r,rr,t)) ;
transfers_ann(k,r,rr,t) = sum(s,  transfers(s,k,r,rr,t) * hours(s)) ;
transfers_ann_r(k,r,t)  = sum(rr, transfers_ann(k,r,rr,t)) ;

domexport(s,r,rr,t)       = sum(k, transfers(s,k,r,rr,t)) ;
domexport_r(s,r,t)        = sum(k, transfers_r(s,k,r,t)) ;
domexport_ann(r,rr,t)     = sum(k, transfers_ann(k,r,rr,t)) ;
domexport_ann_r(r,t)      = sum(k, transfers_ann_r(k,r,t)) ;

Transfers_rpt(t,r,rr)     = domexport_ann(r,rr,t) ;
Transfers_r_rpt(t,r)      = domexport_ann_r(r,t) ;
Transfers_total_rpt(t)    = sum(r, domexport_ann_r(r,t)) + eps ;

$if not  set trans      imports(s,k,r,rr,t) = eps ;
$if      set trans      imports(s,k,r,rr,t) = E.L(s,k,rr,r,t) * 1e-3 ;

imports_r(s,k,r,t)    = sum(rr, imports(s,k,r,rr,t)) ;
imports_ann(k,r,rr,t) = sum(s,  imports(s,k,r,rr,t) * hours(s)) ;
imports_ann_r(k,r,t)  = sum(rr, imports_ann(k,r,rr,t)) ;

domimport(s,r,rr,t)       = sum(k, imports(s,k,r,rr,t)) ;
domimport_r(s,r,t)        = sum(k, imports_r(s,k,r,t)) ;
domimport_ann(r,rr,t)     = sum(k, imports_ann(k,r,rr,t)) ;
domimport_ann_r(r,t)      = sum(k, imports_ann_r(k,r,t)) ;

Imports_rpt(t,r,rr)     = domimport_ann(r,rr,t) ;
Imports_r_rpt(t,r)      = domimport_ann_r(r,t) ;
Imports_total_rpt(t)    = sum(r, domimport_ann_r(r,t)) + eps ;

* * * NTC reporting
parameters
ntc(k,r,r,t)                  NTC (GW)
ntc_r(k,r,t)                  NTC (GW)
ntc_rr(r,r,t)                            NTC (GW)
ntc_rrr(r,t)                             NTC (GW)
NTC_rpt(t,r,r)                           NTC (GW)
NTC_r_rpt(r,t)                           NTC (GW)
NTC_total_rpt(t)                         NTC (GW)
NTCTransfers_rpt(t,r,*)                  NTC (GW)
NTCTransfers_total_rpt(t,*)              NTC (GW)
NTCTransfers_GER_rpt(t,*)              NTC (GW)
NTCTransfers_FR_rpt(t,*)              NTC (GW)
;

$if not  set trans      ntc(k,r,rr,t) = eps ;
$if      set trans      ntc(k,r,rr,t) = TC.L(k,r,rr,t) ;

ntc_r(k,r,t)  = sum(rr, ntc(k,r,rr,t)) ;
ntc_rr(r,rr,t)           = sum(k, ntc(k,r,rr,t)) ;
ntc_rrr(r,t)             = sum((rr,k), ntc(k,r,rr,t)) ;

NTC_rpt(t,r,rr)          = ntc_rr(r,rr,t) ;
NTC_r_rpt(r,t)           = ntc_rrr(r,t) ;
NTC_total_rpt(t)         = sum(r, ntc_rrr(r,t)) + eps ;

NTCTransfers_rpt(t,r,"NTC (GW)")            = NTC_r_rpt(r,t) + eps ;
NTCTransfers_rpt(t,r,"Transfer (TWh)")      = domexport_ann_r(r,t) + eps ;
NTCTransfers_rpt(t,r,"Imports (TWh)")       = domimport_ann_r(r,t) + eps ;

NTCTransfers_total_rpt(t,"NTC (GW)")        = NTC_total_rpt(t) + eps ;
NTCTransfers_total_rpt(t,"Transfer (TWh)")  = Transfers_total_rpt(t) + eps ;
NTCTransfers_total_rpt(t,"Imports (TWh)")   = Imports_total_rpt(t) + eps ;

NTCTransfers_GER_rpt(t,"NTC (GW)")        = sum(r$(sameas(r,"Germany")), NTCTransfers_rpt(t,r,"NTC (GW)")) ;
NTCTransfers_GER_rpt(t,"Transfer (TWh)")  = sum(r$(sameas(r,"Germany")), NTCTransfers_rpt(t,r,"Transfer (TWh)")) ;
NTCTransfers_GER_rpt(t,"Imports (TWh)")   = sum(r$(sameas(r,"Germany")), NTCTransfers_rpt(t,r,"Imports (TWh)")) ;

NTCTransfers_FR_rpt(t,"NTC (GW)")        = sum(r$(sameas(r,"France")), NTCTransfers_rpt(t,r,"NTC (GW)")) ;
NTCTransfers_FR_rpt(t,"Transfer (TWh)")  = sum(r$(sameas(r,"France")), NTCTransfers_rpt(t,r,"Transfer (TWh)")) ;
NTCTransfers_FR_rpt(t,"Imports (TWh)")   = sum(r$(sameas(r,"France")), NTCTransfers_rpt(t,r,"Imports (TWh)")) ;

* * * Regional reporting
parameter
NTC_rrpt(t,rrpt,rrpt)                    NTC (GW) and transfers (TWh) between 4 reporting regions without within region values
NTC_r_rrpt(rrpt,t)                       NTC (GW) and transfers (TWh) between 4 reporting regions without within region values
NTC_total_rrpt(t)                        NTC (GW) and transfers (TWh) between 4 reporting regions without within region values
Transfers_rrpt(t,rrpt,rrpt)              NTC (GW) and transfers (TWh) between 4 reporting regions without within region values
Transfers_r_rrpt(rrpt,t)                 NTC (GW) and transfers (TWh) between 4 reporting regions without within region values
Transfers_total_rrpt(t)                  NTC (GW) and transfers (TWh) between 4 reporting regions without within region values
NTCTransfers_total_rrpt(t,*)             NTC (GW) and transfers (TWh) between 4 reporting regions without within region values
;

NTC_rrpt(t,rrpt,rrpt_)$(not sameas(rrpt,rrpt_)) = sum(xrrpt_(rrpt_,rr), sum(xrrpt(rrpt,r), ntc_rr(r,rr,t))) ;
NTC_r_rrpt(rrpt,t)     = sum(rrpt_, NTC_rrpt(t,rrpt,rrpt_)) ;
NTC_total_rrpt(t)      = sum(rrpt,  NTC_r_rrpt(rrpt,t)) ;

Transfers_rrpt(t,rrpt,rrpt_)$(not sameas(rrpt,rrpt_)) = sum(xrrpt_(rrpt_,rr), sum(xrrpt(rrpt,r), Transfers_rpt(t,r,rr))) ;
Transfers_r_rrpt(rrpt,t)     = sum(rrpt_,                               Transfers_rrpt(t,rrpt,rrpt_)) ;
Transfers_total_rrpt(t)      = sum(rrpt,                                Transfers_r_rrpt(rrpt,t) ) ;

NTCTransfers_total_rrpt(t,"NTC (GW)") = NTC_total_rrpt(t) ;
NTCTransfers_total_rrpt(t,"Transfer (TWh)") = Transfers_total_rrpt(t) ;

* * * Regional reporting
parameter
NTC_rrrpt(t,rrpt,rrpt)                   NTC (GW) and transfers (TWh) between 4 reporting regions with within region values
NTC_r_rrrpt(rrpt,t)                      NTC (GW) and transfers (TWh) between 4 reporting regions with within region values
NTC_total_rrrpt(t)                       NTC (GW) and transfers (TWh) between 4 reporting regions with within region values
Transfers_rrrpt(t,rrpt,rrpt)             NTC (GW) and transfers (TWh) between 4 reporting regions with within region values
Transfers_r_rrrpt(rrpt,t)                NTC (GW) and transfers (TWh) between 4 reporting regions with within region values
Transfers_total_rrrpt(t)                 NTC (GW) and transfers (TWh) between 4 reporting regions with within region values
NTCTransfers_total_rrrpt(t,*)            NTC (GW) and transfers (TWh) between 4 reporting regions with within region values
;

NTC_rrrpt(t,rrpt,rrpt_) = sum(xrrpt_(rrpt_,rr), sum(xrrpt(rrpt,r), ntc_rr(r,rr,t))) ;
NTC_r_rrrpt(rrpt,t)     = sum(rrpt_, NTC_rrpt(t,rrpt,rrpt_)) ;
NTC_total_rrrpt(t)      = sum(rrpt,  NTC_r_rrpt(rrpt,t)) ;

Transfers_rrrpt(t,rrpt,rrpt_) = sum(xrrpt_(rrpt_,rr), sum(xrrpt(rrpt,r), Transfers_rpt(t,r,rr))) ;
Transfers_r_rrrpt(rrpt,t)     = sum(rrpt_,                               Transfers_rrpt(t,rrpt,rrpt_)) ;
Transfers_total_rrrpt(t)      = sum(rrpt,                                Transfers_r_rrpt(rrpt,t) ) ;

NTCTransfers_total_rrrpt(t,"NTC (GW)") = NTC_total_rrpt(t) ;
NTCTransfers_total_rrrpt(t,"Transfer (TWh)") = Transfers_total_rrpt(t) ;

* * * NTC rents reporting
parameter
rent(k,r,r,t) NTC rent by technology (billion EUR)
rent_r(k,r,t) NTC rent by technology in one region (billion EUR)
rent_rr(r,r,t)           NTC rent (billion EUR)
rent_rrr(r,t)            NTC rent in one region (billion EUR)
Rent_rpt(t,r,r)          NTC rent reporting vector (billion EUR)
Rent_r_rpt(r,t)          NTC rent reporting vector (billion EUR)
Rent_total_rpt(t)        NTC rent reporting vector (billion EUR)
;

$if not  set trans      rent(k,r,rr,t) = eps ;
$if      set trans      rent(k,r,rr,t) = sum(s, E.L(s,k,r,rr,t) * hours(s) * (price(s,rr,t) - price(s,r,t))) * 1e-6 ;

rent_r(k,r,t)  = sum(rr, rent(k,r,rr,t)) ;
rent_rr(r,rr,t)           = sum(k, rent(k,r,rr,t)) ;
rent_rrr(r,t)             = sum((rr,k), rent(k,r,rr,t)) ;

Rent_rpt(t,r,rr)          = rent_rr(r,rr,t) ;
Rent_r_rpt(r,t)           = rent_rrr(r,t) ;
Rent_total_rpt(t)         = sum(r, rent_rrr(r,t)) + eps ;

* * * Full-load hours transmission reporting
Parameters
flh_transmission(r,r,t)      Full load hours per line (h)
flh_transmission_r(r,t)      Full load hours per region (h)
flh_transmission_total(t)    Full load hours of the system (h)
FlhTransmission_rpt(t,r,r)   Transmission flh reporting vector (h)
FlhTransmission_r_rpt(r,t)   Transmission flh reporting vector (h)
FlhTransmission_total_rpt(t) Transmission flh reporting vector (h)
;

flh_transmission(r,rr,t)$(ntc_rr(r,rr,t) > 0)    = domexport_ann(r,rr,t) / ntc_rr(r,rr,t) * 1e+3 ;
flh_transmission_r(r,t)$ntc_rrr(r,t)             = domexport_ann_r(r,t) / ntc_rrr(r,t) * 1e+3 ;
flh_transmission_total(t)$sum(r, ntc_rrr(r,t))   = sum(r, domexport_ann_r(r,t)) / sum(r, ntc_rrr(r,t)) * 1e+3 ;

FlhTransmission_rpt(t,r,rr)      = flh_transmission(r,rr,t) ;
FlhTransmission_r_rpt(r,t)       = flh_transmission_r(r,t) ;
FlhTransmission_total_rpt(t)     = flh_transmission_total(t) + eps ;

* * * Storage reporting
Parameter
ginstalled(r,j,t)                        Installed storage discharge capacity by region (GW)
ginvestment(r,j,t)                       Investments in storage charge capacity by region (GW)
gretirement(r,j,t)                       Retirement of storage charge capacity by region (GW)
ginstalledc(r,j,t)                       Installed storage charge capacity by region (GW)
ginstalledr(r,j,t)                       Installed storage capacity by region (TWh)
ginvestmentr(r,j,t)                      Investments in storage capacity by region (TWh)
gretirementr(r,j,t)                      Retirement of storage capacity in GWh by region (TWh)
gavcharge(r,j,t)                         Average state of charging (% of reservoir capacity)
gflh(r,j,t)                              Storage FLH cycles (from reservoir capacity)
gstored(r,j,t)                           Accumulated stored energy (TWh)
gcharge(r,j,t)
greserv(r,j,t)
gdcharge(r,j,t)
gmarket(r,j,t)

Storage(t,j,r,*)                     Storage reporting vector - all
Storage_total(t,j,*)                 Storage reporting vector - all in total

Storage_GenerationCapacity_rpt(t,r,j)      Storage reporting vector - Generation capacity (GW)
Storage_ReservoirCapacity_rpt(t,r,j)       Storage reporting vector - Reservoir capacity (TWh)
Storage_StoredEnergy_rpt(t,r,j)            Storage reporting vector - Stored energy (TWh)
Storage_FromMarket_rpt(t,r,j)              
Storage_ToMarket_rpt(t,r,j) 
Storage_ChargeCapacity_rpt(t,r,j)

Storage_total_GenerationCapacity_rpt(t,j)      Storage reporting vector - Generation capacity (GW)
Storage_total_ReservoirCapacity_rpt(t,j)       Storage reporting vector - Reservoir capacity (TWh)
Storage_total_StoredEnergy_rpt(t,j)            Storage reporting vector - Stored energy (TWh)
Storage_total_FromMarket_rpt(t,j)              
Storage_total_ToMarket_rpt(t,j)
Storage_total_ChargeCapacity_rpt(t,j)
;

* Variable calculations
ginstalled(r,j,t)        = sum(jvrt(j,v,r,t), GCL(j,v,r,t)) ;
$if      set hydrogensimple ginstalled(r,ghyd(j),t) = sum(jvrt(j,v,r,t), GCD.L(j,v,r,t)) ;
$if      set storagebalnv   ginstalled(r,nvj(j),t) = GCNV.L(j,r,t) ;

$if not  set hydrogensimple ginstalledc(r,j,t)               = sum(jvrt(j,v,r,t), GCL(j,v,r,t)) ;
$if      set hydrogensimple ginstalledc(r,j,t)$(not ghyd(j)) = sum(jvrt(j,v,r,t), GCL(j,v,r,t)) ;
$if      set storagebalnv   ginstalledc(r,nvj(j),t)          = GCNV.L(j,r,t) ;
$if      set hydrogensimple ginstalledc(r,ghyd(j),t)         = sum(jvrt(j,v,r,t), GCC.L(j,v,r,t)) ;

ginvestment(r,j,t)       = IGL(j,r,t) ;

gretirement(r,j,t)       = eps ;
gretirement(r,j,t)$(t.val > 2023)       = sum(jvrt(j,v,r,t)$(gcapt(j,v,r,"2023")), GCL(j,v,r,t-1) - GCL(j,v,r,t) ) ;

ginstalledr(r,j,t)       = sum(jvrt(j,v,r,t), GCL(j,v,r,t) * ghours(j,v,r)) * 1e-3 ;
$if      set storagebalnv   ginstalledr(r,nvj(j),t)       = GCNV.L(j,r,t) * ghoursnv(j,r,t) * 1e-3 ;
$if      set hydrogensimple ginstalledr(r,ghyd(j),t) = sum(jvrt(j,v,r,t), GCR.L(j,v,r,t)) * 1e-3 ;

ginvestmentr(r,j,t)      = sum(jvrt(j,v,r,t)$(t.val eq v.val), IGL(j,r,t) * ghours(j,v,r)) * 1e-3 ;

gretirementr(r,j,t)      = eps ;
gretirementr(r,j,t)$(t.val > 2023)      = sum(jvrt(j,v,r,t)$(gcapt(j,v,r,"2023")), (GCL(j,v,r,t-1) - GCL(j,v,r,t) ) * ghours(j,v,r)) * 1e-3 ;

gavcharge(r,j,t)$(ginstalledr(r,j,t) > 0.001)  = sum((s,jvrt(j,v,r,t)), GBL(s,j,v,r,t) * 1e-3 * hours(s) ) / sum(s, hours(s) * sum(jvrt(j,v,r,t), GCL(j,v,r,t) * ghours(j,v,r))) ;
$if      set hydrogensimple gavcharge(r,ghyd(j),t)$(ginstalledr(r,j,t) > 0.001)  = sum((s,jvrt(j,v,r,t)), GBL(s,j,v,r,t) * 1e-3 * hours(s) ) / sum(s, hours(s) * sum(jvrt(j,v,r,t), GCR.L(j,v,r,t))) ;

gstored(r,j,t)           = sum((s,jvrt(j,v,r,t)), GL(s,j,v,r,t) * hours(s) ) * 1e-3 ;
$if      set storagebalnv   gstored(r,nvj(j),t)      = sum((s), GNV.L(s,j,r,t) * hours(s) ) * 1e-3 ;
gcharge(r,j,t)           = sum((s,jvrt(j,v,r,t)), GL(s,j,v,r,t) * hours(s) ) * 1e-3 ;
$if      set storagebalnv   gcharge(r,nvj(j),t)      = sum((s), GNV.L(s,j,r,t) * hours(s) ) * 1e-3 ;
greserv(r,j,t)           = sum((s,jvrt(j,v,r,t)), GL(s,j,v,r,t) * chrgpen(j,v,r) * hours(s) ) * 1e-3 ;
$if      set storagebalnv   greserv(r,nvj(j),t)      = sum((s), GNV.L(s,j,r,t) * chrgpennv(j,r) * hours(s) ) * 1e-3 ;
gdcharge(r,j,t)          = sum((s,jvrt(j,v,r,t)), GDL(s,j,v,r,t) * hours(s) ) * 1e-3 ;
$if      set storagebalnv   gdcharge(r,nvj(j),t)     = sum((s), GDNV.L(s,j,r,t) * hours(s) ) * 1e-3 ;
gmarket(r,j,t)           = sum((s,jvrt(j,v,r,t)), GDL(s,j,v,r,t) * dchrgpen(j,v,r) * hours(s) ) * 1e-3 ;
$if      set storagebalnv   gmarket(r,nvj(j),t)      = sum((s), GDNV.L(s,j,r,t) * dchrgpennv(j,r) * hours(s) ) * 1e-3 ;
gflh(r,j,t)$(ginstalledc(r,j,t) > 0.001) = gstored(r,j,t) / ginstalledc(r,j,t) ;

* Reporting calculations
Storage(t,j,r,"inst-cap-p")                 = ginstalled(r,j,t) ;
Storage(t,j,r,"new-inst-cap-p")             = ginvestment(r,j,t) ;
Storage(t,j,r,"retired-cap-p")              = gretirement(r,j,t) ;
Storage(t,j,r,"inst-cap-c")                 = ginstalledr(r,j,t) ;
Storage(t,j,r,"new-inst-cap-c")             = ginvestmentr(r,j,t) ;
Storage(t,j,r,"retired-cap-c")              = gretirementr(r,j,t) ;
Storage(t,j,r,"avg-state-of-charge")        = gavcharge(r,j,t) ;
Storage(t,j,r,"full-load cycles")           = gflh(r,j,t) ;
Storage(t,j,r,"stored-energy")              = gstored(r,j,t) ;
Storage(t,j,r,"taken from market")          = gcharge(r,j,t) ;
Storage(t,j,r,"into reservoir")             = greserv(r,j,t) ;
Storage(t,j,r,"taken from reservoir")       = gdcharge(r,j,t) ;
Storage(t,j,r,"provided to market")         = gmarket(r,j,t) ;
Storage(t,j,r,"total storage losses")       = gcharge(r,j,t) - gmarket(r,j,t) ;

Storage(t,j,r,"inst-cap-charge")            = ginstalledc(r,j,t) ;

Storage_total(t,j,"inst-cap-p")             = sum(r, ginstalled(r,j,t)) ;
Storage_total(t,j,"new-inst-cap-p")         = sum(r, ginvestment(r,j,t)) ;
Storage_total(t,j,"retired-cap-p")          = sum(r, gretirement(r,j,t)) ;
Storage_total(t,j,"inst-cap-c")             = sum(r, ginstalledr(r,j,t)) ;
Storage_total(t,j,"new-inst-cap-c")         = sum(r, ginvestmentr(r,j,t)) ;
Storage_total(t,j,"retired-cap-c")          = sum(r, gretirementr(r,j,t)) ;
Storage_total(t,j,"avg-state-of-charge")    = sum(r, gavcharge(r,j,t)) / sum(r, 1) ;
Storage_total(t,j,"full-load cycles")       = sum(r, gflh(r,j,t)) ;
Storage_total(t,j,"stored-energy")          = sum(r, gstored(r,j,t)) ;
Storage_total(t,j,"taken from market")      = sum(r, gcharge(r,j,t)) ;
Storage_total(t,j,"into reservoir")         = sum(r, greserv(r,j,t)) ;
Storage_total(t,j,"taken from reservoir")   = sum(r, gdcharge(r,j,t)) ;
Storage_total(t,j,"provided to market")     = sum(r, gmarket(r,j,t)) ;
Storage_total(t,j,"total storage losses")   = sum(r, gcharge(r,j,t)) - sum(r, gmarket(r,j,t)) ;
Storage_total(t,j,"inst-cap-charge")        = sum(r, ginstalledc(r,j,t)) ;

Storage_GenerationCapacity_rpt(t,r,j)                  = Storage(t,j,r,"inst-cap-p") + eps ;
Storage_ReservoirCapacity_rpt(t,r,j)                   = Storage(t,j,r,"inst-cap-c") + eps ;
Storage_StoredEnergy_rpt(t,r,j)                        = Storage(t,j,r,"stored-energy") + eps ;
Storage_FromMarket_rpt(t,r,j)                          = Storage(t,j,r,"taken from market") + eps ;
Storage_ToMarket_rpt(t,r,j)                            = Storage(t,j,r,"provided to market") + eps ;
Storage_ChargeCapacity_rpt(t,r,j)                      = Storage(t,j,r,"inst-cap-charge") + eps ;

Storage_total_GenerationCapacity_rpt(t,j)              = Storage_total(t,j,"inst-cap-p") + eps ;
Storage_total_ReservoirCapacity_rpt(t,j)               = Storage_total(t,j,"inst-cap-c") + eps ;
Storage_total_StoredEnergy_rpt(t,j)                    = Storage_total(t,j,"stored-energy") + eps ;
Storage_total_FromMarket_rpt(t,j)                      = Storage_total(t,j,"taken from market") + eps ;
Storage_total_ToMarket_rpt(t,j)                        = Storage_total(t,j,"provided to market") + eps ;
Storage_total_ChargeCapacity_rpt(t,j)                  = Storage_total(t,j,"inst-cap-charge") + eps ;

Set
storagerpt /Pump-GenCap,Res-GenCap,Hyd-GenCap,Bat-GenCap,
            Pump-ResCap,Res-ResCap,Hyd-ResCap,Bat-ResCap,
            Pump-FromMarket,Res-FromMarket,Hyd-FromMarket,Bat-FromMarket,
            Pump-ToMarket,Res-ToMarket,Hyd-ToMarket,Bat-ToMarket,
            GenCap,ResCap,FromMarket,ToMarket,Hyd-ChaCap,Hyd-Use,Hyd-Import,Hyd-Price,Hyd-Impprice/
;

parameter
Storage_rpt(t,r,storagerpt)
Storage_total_rpt(t,storagerpt)
Storage_GER_rpt(t,storagerpt)
Storage_FR_rpt(t,storagerpt) 
;

Storage_rpt(t,r,"Pump-GenCap")          = Storage_GenerationCapacity_rpt(t,r,"Pumpstorage") ;
Storage_rpt(t,r,"Res-GenCap")           = Storage_GenerationCapacity_rpt(t,r,"Reservoir") ;
Storage_rpt(t,r,"Hyd-GenCap")           = Storage_GenerationCapacity_rpt(t,r,"Storage_LT") ;
Storage_rpt(t,r,"Bat-GenCap")           = Storage_GenerationCapacity_rpt(t,r,"Storage_ST") ;

Storage_rpt(t,r,"Pump-ResCap")          = Storage_ReservoirCapacity_rpt(t,r,"Pumpstorage") ;
Storage_rpt(t,r,"Res-ResCap")           = Storage_ReservoirCapacity_rpt(t,r,"Reservoir") ;
Storage_rpt(t,r,"Hyd-ResCap")           = Storage_ReservoirCapacity_rpt(t,r,"Storage_LT") ;
Storage_rpt(t,r,"Bat-ResCap")           = Storage_ReservoirCapacity_rpt(t,r,"Storage_ST") ;

Storage_rpt(t,r,"Pump-FromMarket")      = Storage_FromMarket_rpt(t,r,"Pumpstorage") ;
Storage_rpt(t,r,"Res-FromMarket")       = Storage_FromMarket_rpt(t,r,"Reservoir") ;
Storage_rpt(t,r,"Hyd-FromMarket")       = Storage_FromMarket_rpt(t,r,"Storage_LT") ;
Storage_rpt(t,r,"Bat-FromMarket")       = Storage_FromMarket_rpt(t,r,"Storage_ST") ;

Storage_rpt(t,r,"Pump-ToMarket")        = Storage_ToMarket_rpt(t,r,"Pumpstorage") ;
Storage_rpt(t,r,"Res-ToMarket")         = Storage_ToMarket_rpt(t,r,"Reservoir") ;
Storage_rpt(t,r,"Hyd-ToMarket")         = Storage_ToMarket_rpt(t,r,"Storage_LT") ;
Storage_rpt(t,r,"Bat-ToMarket")         = Storage_ToMarket_rpt(t,r,"Storage_ST") ;

Storage_rpt(t,r,"GenCap")               = sum(j, Storage_GenerationCapacity_rpt(t,r,j)) ;
Storage_rpt(t,r,"ResCap")               = sum(j, Storage_ReservoirCapacity_rpt(t,r,j)) ;
Storage_rpt(t,r,"FromMarket")           = sum(j, Storage_FromMarket_rpt(t,r,j)) ;
Storage_rpt(t,r,"ToMarket")             = sum(j, Storage_ToMarket_rpt(t,r,j)) ;

Storage_rpt(t,r,"Hyd-ChaCap")           = Storage_ChargeCapacity_rpt(t,r,"Storage_LT") ;
$if      set hydrogensimple     Storage_rpt(t,r,"Hyd-Use")              = sum(ghyd(j), HRES.L(j,r,t)) + eps ;
$if      set hydrogensimple     Storage_rpt(t,r,"Hyd-Price")            = sum(ghyd(j), demand_hydrogen.M(r,t)) / dfact(t) + eps ;
$if      set hydrogenimport     Storage_rpt(t,r,"Hyd-Import")           = sum(ghyd(j), HIMP.L(j,r,t)) + eps ;
$if      set hydrogenimport     Storage_rpt(t,r,"Hyd-Impprice")         = himport(r,t) + eps ;

Storage_total_rpt(t,storagerpt)         = sum(r, Storage_rpt(t,r,storagerpt)) ;

Storage_total_rpt(t,"Hyd-Price")$(sum(r, hydtwh(r,t)) > 0)  = sum(r, Storage_rpt(t,r,"Hyd-Price") * hydtwh(r,t)) / sum(r, hydtwh(r,t)) ;
Storage_total_rpt(t,"Hyd-Price")$(sum(r, hydtwh(r,t)) = 0)  = eps ;
Storage_total_rpt(t,"Hyd-Impprice")$(sum(r, hydtwh(r,t)) > 0)  = sum(r, himport(r,t) * hydtwh(r,t)) / sum(r, hydtwh(r,t)) ;
Storage_total_rpt(t,"Hyd-Impprice")$(sum(r, hydtwh(r,t)) = 0)  = eps ;

Storage_GER_rpt(t,storagerpt)           = sum(r$(sameas(r,"Germany")), Storage_rpt(t,r,storagerpt)) ;
Storage_FR_rpt(t,storagerpt)            = sum(r$(sameas(r,"France")), Storage_rpt(t,r,storagerpt)) ;

parameter
Storage_endbalance(j,v,r,t)
Storage_endbalancenv(j,r,t) 
;

$if      set storagecal      $if not  set storagebalnv    Storage_endbalance(j,v,r,t)$((ghours(j,v,r) * GCL(j,v,r,t)) > 0) = sum(ss$(ss.val eq number), GBL(ss,j,v,r,t)) * 1e+3  / (ghours(j,v,r) * GCL(j,v,r,t)) ;
$if      set storagecal      $if      set storagebalnv    Storage_endbalancenv(j,r,t)$(sum(jvrt(j,v,r,t), ghours(j,v,r) * GCL(j,v,r,t)) > 0) = sum(ss$(ss.val eq number), GBNV.L(ss,j,r,t)) * 1e+3  / sum(jvrt(j,v,r,t), ghours(j,v,r) * GCL(j,v,r,t)) ;
                         
* * * Hourly prices reporting
Parameters
Price_s_rpt(t,s,r)         Price in segments (EUR per MWh)
Price_h_rpt(t,h,r)         Price in hours (EUR per MWh)
;

*Price_s_rpt(t,s,r)  = price(s,r,t) ;
*Price_h_rpt(t,h,r)  = sum(s$hmaps(h,s), price(s,r,t)) ;

Parameter
scc_t(t)
;

scc_t("2020") =  50 ;
scc_t("2050") = 250 ;
scc_t(t) $(t.val ge 2021 and t.val le 2049) = scc_t("2020") + (scc_t("2050") - scc_t("2020")) * (t.val - 2020) / (2050 - 2020) ;

$if set socialcost  $include    modules\euregen2024_socialcost_rpt_v1
$if set socialcost  $include    modules\euregen2024_systemcost_rpt_v1

parameter
noweffect_rpt(t,*,*)
realeffect_rpt(t,*,*)
effect_rpt(t,*)
price_rpt(t,r)
;

price_rpt(t,r) = Electricity1_rpt(t,r,"price-avg") + eps ;

noweffect_rpt("2022","EUR","Gasuse") = gasuseeu("2022") + eps ;
noweffect_rpt("2022","EUR","Price") = Electricity1_total_rpt("2022","price-avg") + eps ;
noweffect_rpt("2022","EUR","Coal") = ElectricityGeneration_total_xtype_rpt("2022","Coal") + eps ;
noweffect_rpt("2022","EUR","Lignite") = ElectricityGeneration_total_xtype_rpt("2022","Lignite") + eps ;
noweffect_rpt("2022","EUR","Gas") = ElectricityGeneration_total_xtype_rpt("2022","Gas-CCGT") + ElectricityGeneration_total_xtype_rpt("2022","Gas-OCGT") + ElectricityGeneration_total_xtype_rpt("2022","Gas-ST")+ eps ;
noweffect_rpt("2022","EUR","Nuclear") = ElectricityGeneration_total_xtype_rpt("2022","Nuclear") + eps ;
noweffect_rpt("2022","EUR","Hydro") = ElectricityGeneration_total_xtype_rpt("2022","Hydro") + Storage_total_rpt("2022","Res-ToMarket") + eps ;
noweffect_rpt("2022","EUR","Wind") = ElectricityGeneration_total_xtype_rpt("2022","WindOn") + ElectricityGeneration_total_xtype_rpt("2022","WindOff") + eps ;
noweffect_rpt("2022","EUR","Solar") = ElectricityGeneration_total_xtype_rpt("2022","Solar") + eps ;
noweffect_rpt("2022","EUR","Imports") = Imports_total_rpt("2022") + eps ;
noweffect_rpt("2022","EUR","Exports") = Transfers_total_rpt("2022") + eps ;
noweffect_rpt("2022","EUR","CO2 price") = Emissions_total_rpt("2022","CO2 price")  + eps ;
noweffect_rpt("2022","EUR","CO2") = Emissions_total_rpt("2022","Total CO2-emissions-elec")  + eps ;
noweffect_rpt("2022","EUR","Biomass") = ElectricityGeneration_total_xtype_rpt("2022","Bioenergy") + eps ;
noweffect_rpt("2022","EUR","Oil") = ElectricityGeneration_total_xtype_rpt("2022","OilOther") + eps ;

noweffect_rpt("2022","GER","Gasuse") = gasuse("Germany","2022") + eps ;
noweffect_rpt("2022","GER","Price") = Electricity1_rpt("2022","Germany","price-avg") + eps ;
noweffect_rpt("2022","GER","Coal") = ElectricityGeneration_xtype_rpt("2022","Germany","Coal") + eps ;
noweffect_rpt("2022","GER","Lignite") = ElectricityGeneration_xtype_rpt("2022","Germany","Lignite") + eps ;
noweffect_rpt("2022","GER","Gas") = ElectricityGeneration_xtype_rpt("2022","Germany","Gas-CCGT") + ElectricityGeneration_xtype_rpt("2022","Germany","Gas-OCGT") + ElectricityGeneration_xtype_rpt("2022","Germany","Gas-ST")+ eps ;
noweffect_rpt("2022","GER","Nuclear") = ElectricityGeneration_xtype_rpt("2022","Germany","Nuclear") + eps ;
noweffect_rpt("2022","GER","Hydro") = ElectricityGeneration_xtype_rpt("2022","Germany","Hydro") + Storage_GER_rpt("2022","Res-ToMarket") + eps ;
noweffect_rpt("2022","GER","Wind") = ElectricityGeneration_xtype_rpt("2022","Germany","WindOn") + ElectricityGeneration_xtype_rpt("2022","Germany","WindOff") + eps ;
noweffect_rpt("2022","GER","Solar") = ElectricityGeneration_xtype_rpt("2022","Germany","Solar") + eps ;
noweffect_rpt("2022","GER","Imports") = domimport_ann_r("Germany","2022") + eps ;
noweffect_rpt("2022","GER","Exports") = domexport_ann_r("Germany","2022") + eps ;
noweffect_rpt("2022","GER","CO2 price") = Emissions_total_rpt("2022","CO2 price") + eps ;
noweffect_rpt("2022","GER","CO2") = Emissions_rpt("2022","Germany","CO2-emissions-elec")  + eps ;
noweffect_rpt("2022","GER","Biomass") = ElectricityGeneration_xtype_rpt("2022","Germany","Bioenergy") + eps ;
noweffect_rpt("2022","GER","Oil") = ElectricityGeneration_xtype_rpt("2022","Germany","OilOther") + eps ;

noweffect_rpt("2022","FR","Gasuse") = gasuse("France","2022") + eps ;
noweffect_rpt("2022","FR","Price") = Electricity1_rpt("2022","France","price-avg") + eps ;
noweffect_rpt("2022","FR","Coal") = ElectricityGeneration_xtype_rpt("2022","France","Coal") + eps ;
noweffect_rpt("2022","FR","Lignite") = ElectricityGeneration_xtype_rpt("2022","France","Lignite") + eps ;
noweffect_rpt("2022","FR","Gas") = ElectricityGeneration_xtype_rpt("2022","France","Gas-CCGT") + ElectricityGeneration_xtype_rpt("2022","France","Gas-OCGT") + ElectricityGeneration_xtype_rpt("2022","France","Gas-ST")+ eps ;
noweffect_rpt("2022","FR","Nuclear") = ElectricityGeneration_xtype_rpt("2022","France","Nuclear") + eps ;
noweffect_rpt("2022","FR","Hydro") = ElectricityGeneration_xtype_rpt("2022","France","Hydro") + Storage_FR_rpt("2022","Res-ToMarket") + eps ;
noweffect_rpt("2022","FR","Wind") = ElectricityGeneration_xtype_rpt("2022","France","WindOn") + ElectricityGeneration_xtype_rpt("2022","France","WindOff") + eps ;
noweffect_rpt("2022","FR","Solar") = ElectricityGeneration_xtype_rpt("2022","France","Solar") + eps ;
noweffect_rpt("2022","FR","Imports") = domimport_ann_r("France","2022") + eps ;
noweffect_rpt("2022","FR","Exports") = domexport_ann_r("France","2022") + eps ;
noweffect_rpt("2022","FR","CO2 price") = Emissions_total_rpt("2022","CO2 price")  + eps ;
noweffect_rpt("2022","FR","CO2") = Emissions_rpt("2022","France","CO2-emissions-elec")  + eps ;
noweffect_rpt("2022","FR","Biomass") = ElectricityGeneration_xtype_rpt("2022","France","Bioenergy") + eps ;
noweffect_rpt("2022","FR","Oil") = ElectricityGeneration_xtype_rpt("2022","France" ,"OilOther") + eps ;

realeffect_rpt("2022","EUR","Gasuse") = eps ;
realeffect_rpt("2022","EUR","Price") = eps ;
realeffect_rpt("2022","EUR","Coal") = sum(r, gen_min2_chp_coal(r,"2022") + gen_min2_nochp_coal(r,"2022")) + eps ;
realeffect_rpt("2022","EUR","Lignite") = sum(r, gen_min2_chp_lign(r,"2022") + gen_min2_nochp_lign(r,"2022")) + eps ;
realeffect_rpt("2022","EUR","Gas") = sum(r, gen_min2_chp_ngas(r,"2022") + gen_min2_nochp_ngas(r,"2022")) + eps ;
realeffect_rpt("2022","EUR","Nuclear") = sum(r, gen_min2_nucl(r,"2022")) + eps ;
realeffect_rpt("2022","EUR","Hydro") = sum(r, gen_min2_hydr(r,"2022") + gen_resv(r,"2022")) + eps ;
realeffect_rpt("2022","EUR","Wind") = sum(r, gen_min2_wind(r,"2022")) + eps ;
realeffect_rpt("2022","EUR","Solar") = sum(r, gen_min2_sola(r,"2022")) + eps ;
realeffect_rpt("2022","EUR","Imports") = sum(r, imp(r,"2022")) + eps ;
realeffect_rpt("2022","EUR","Exports") = sum(r, expo(r,"2022")) + eps ;
realeffect_rpt("2022","EUR","CO2 price") = 81.48 + eps ;
$if not set co2price    realeffect_rpt("2022","EUR","CO2") = co2ele_org("2022") + co2eleuk_in("2022") + eps ;
realeffect_rpt("2022","EUR","Biomass") = sum(r, gen_min2_chp_biom(r,"2022") + gen_min2_nochp_biom(r,"2022")) + eps ;
realeffect_rpt("2022","EUR","oil") = sum(r, gen_min2_chp_oil(r,"2022") + gen_min2_nochp_oil(r,"2022")) + eps ;

realeffect_rpt("2022","GER","Gasuse") = eps ;
realeffect_rpt("2022","GER","Price") = 235.31 + eps ;
realeffect_rpt("2022","GER","Coal") = sum(rge(r), gen_min2_chp_coal(r,"2022") + gen_min2_nochp_coal(r,"2022")) + eps ;
realeffect_rpt("2022","GER","Lignite") = sum(rge(r), gen_min2_chp_lign(r,"2022") + gen_min2_nochp_lign(r,"2022")) + eps ;
realeffect_rpt("2022","GER","Gas") = sum(rge(r), gen_min2_chp_ngas(r,"2022") + gen_min2_nochp_ngas(r,"2022")) + eps ;
realeffect_rpt("2022","GER","Nuclear") = sum(rge(r), gen_min2_nucl(r,"2022")) + eps ;
realeffect_rpt("2022","GER","Hydro") = sum(rge(r), gen_min2_hydr(r,"2022") + gen_resv(r,"2022")) + eps ;
realeffect_rpt("2022","GER","Wind") = sum(rge(r), gen_min2_wind(r,"2022")) + eps ;
realeffect_rpt("2022","GER","Solar") = sum(rge(r), gen_min2_sola(r,"2022")) + eps ;
realeffect_rpt("2022","GER","Imports") = sum(rge(r), imp(r,"2022")) + eps ;
realeffect_rpt("2022","GER","Exports") = sum(rge(r), expo(r,"2022")) + eps ;
realeffect_rpt("2022","GER","CO2 price") = eps ;
realeffect_rpt("2022","GER","CO2") = eps ;
realeffect_rpt("2022","GER","Biomass") = sum(rge(r), gen_min2_chp_biom(r,"2022") + gen_min2_nochp_biom(r,"2022")) + eps ;
realeffect_rpt("2022","GER","oil") = sum(rge(r), gen_min2_chp_oil(r,"2022") + gen_min2_nochp_oil(r,"2022")) + eps ;

realeffect_rpt("2022","FR","Gasuse") = eps ;
realeffect_rpt("2022","FR","Price") = 365.52 + eps ;
realeffect_rpt("2022","FR","Coal") = sum(rfr(r), gen_min2_chp_coal(r,"2022") + gen_min2_nochp_coal(r,"2022")) + eps ;
realeffect_rpt("2022","FR","Lignite") = sum(rfr(r), gen_min2_chp_lign(r,"2022") + gen_min2_nochp_lign(r,"2022")) + eps ;
realeffect_rpt("2022","FR","Gas") = sum(rfr(r), gen_min2_chp_ngas(r,"2022") + gen_min2_nochp_ngas(r,"2022")) + eps ;
realeffect_rpt("2022","FR","Nuclear") = sum(rfr(r), gen_min2_nucl(r,"2022")) + eps ;
realeffect_rpt("2022","FR","Hydro") = sum(rfr(r), gen_min2_hydr(r,"2022") + gen_resv(r,"2022")) + eps ;
realeffect_rpt("2022","FR","Wind") = sum(rfr(r), gen_min2_wind(r,"2022")) + eps ;
realeffect_rpt("2022","FR","Solar") = sum(rfr(r), gen_min2_sola(r,"2022")) + eps ;
realeffect_rpt("2022","FR","Imports") = sum(rfr(r), imp(r,"2022")) + eps ;
realeffect_rpt("2022","FR","Exports") = sum(rfr(r), expo(r,"2022")) + eps ;
realeffect_rpt("2022","FR","CO2 price") = eps ;
realeffect_rpt("2022","FR","CO2") = eps ;
realeffect_rpt("2022","FR","Biomass") = sum(rfr(r), gen_min2_chp_biom(r,"2022") + gen_min2_nochp_biom(r,"2022")) + eps ;
realeffect_rpt("2022","FR","oil") = sum(rfr(r), gen_min2_chp_oil(r,"2022") + gen_min2_nochp_oil(r,"2022")) + eps ;

noweffect_rpt("2022","EUR","Gasuse") = gasuseeu("2022") + eps ;
noweffect_rpt("2022","EUR","Price") = Electricity1_total_rpt("2022","price-avg") + eps ;
noweffect_rpt("2022","EUR","Coal") = ElectricityGeneration_total_xtype_rpt("2022","Coal") + eps ;
noweffect_rpt("2022","EUR","Lignite") = ElectricityGeneration_total_xtype_rpt("2022","Lignite") + eps ;
noweffect_rpt("2022","EUR","Gas") = ElectricityGeneration_total_xtype_rpt("2022","Gas-CCGT") + ElectricityGeneration_total_xtype_rpt("2022","Gas-OCGT") + ElectricityGeneration_total_xtype_rpt("2022","Gas-ST")+ eps ;
noweffect_rpt("2022","EUR","Nuclear") = ElectricityGeneration_total_xtype_rpt("2022","Nuclear") + eps ;
noweffect_rpt("2022","EUR","Hydro") = ElectricityGeneration_total_xtype_rpt("2022","Hydro") + Storage_total_rpt("2022","Res-ToMarket") + eps ;
noweffect_rpt("2022","EUR","Wind") = ElectricityGeneration_total_xtype_rpt("2022","WindOn") + ElectricityGeneration_total_xtype_rpt("2022","WindOff") + eps ;
noweffect_rpt("2022","EUR","Solar") = ElectricityGeneration_total_xtype_rpt("2022","Solar") + eps ;
noweffect_rpt("2022","EUR","Imports") = Imports_total_rpt("2022") + eps ;
noweffect_rpt("2022","EUR","Exports") = Transfers_total_rpt("2022") + eps ;
noweffect_rpt("2022","EUR","CO2 price") = Emissions_total_rpt("2022","CO2 price")  + eps ;
noweffect_rpt("2022","EUR","CO2") = Emissions_total_rpt("2022","Total CO2-emissions-elec")  + eps ;
noweffect_rpt("2022","EUR","Biomass") = ElectricityGeneration_total_xtype_rpt("2022","Bioenergy") + eps ;
noweffect_rpt("2022","EUR","Oil") = ElectricityGeneration_total_xtype_rpt("2022","OilOther") + eps ;

noweffect_rpt("2022","GER","Gasuse") = gasuse("Germany","2022") + eps ;
noweffect_rpt("2022","GER","Price") = Electricity1_rpt("2022","Germany","price-avg") + eps ;
noweffect_rpt("2022","GER","Coal") = ElectricityGeneration_xtype_rpt("2022","Germany","Coal") + eps ;
noweffect_rpt("2022","GER","Lignite") = ElectricityGeneration_xtype_rpt("2022","Germany","Lignite") + eps ;
noweffect_rpt("2022","GER","Gas") = ElectricityGeneration_xtype_rpt("2022","Germany","Gas-CCGT") + ElectricityGeneration_xtype_rpt("2022","Germany","Gas-OCGT") + ElectricityGeneration_xtype_rpt("2022","Germany","Gas-ST")+ eps ;
noweffect_rpt("2022","GER","Nuclear") = ElectricityGeneration_xtype_rpt("2022","Germany","Nuclear") + eps ;
noweffect_rpt("2022","GER","Hydro") = ElectricityGeneration_xtype_rpt("2022","Germany","Hydro") + Storage_GER_rpt("2022","Res-ToMarket") + eps ;
noweffect_rpt("2022","GER","Wind") = ElectricityGeneration_xtype_rpt("2022","Germany","WindOn") + ElectricityGeneration_xtype_rpt("2022","Germany","WindOff") + eps ;
noweffect_rpt("2022","GER","Solar") = ElectricityGeneration_xtype_rpt("2022","Germany","Solar") + eps ;
noweffect_rpt("2022","GER","Imports") = domimport_ann_r("Germany","2022") + eps ;
noweffect_rpt("2022","GER","Exports") = domexport_ann_r("Germany","2022") + eps ;
noweffect_rpt("2022","GER","CO2 price") = Emissions_total_rpt("2022","CO2 price") + eps ;
noweffect_rpt("2022","GER","CO2") = Emissions_rpt("2022","Germany","CO2-emissions-elec")  + eps ;
noweffect_rpt("2022","GER","Biomass") = ElectricityGeneration_xtype_rpt("2022","Germany","Bioenergy") + eps ;
noweffect_rpt("2022","GER","Oil") = ElectricityGeneration_xtype_rpt("2022","Germany","OilOther") + eps ;

noweffect_rpt("2022","FR","Gasuse") = gasuse("France","2022") + eps ;
noweffect_rpt("2022","FR","Price") = Electricity1_rpt("2022","France","price-avg") + eps ;
noweffect_rpt("2022","FR","Coal") = ElectricityGeneration_xtype_rpt("2022","France","Coal") + eps ;
noweffect_rpt("2022","FR","Lignite") = ElectricityGeneration_xtype_rpt("2022","France","Lignite") + eps ;
noweffect_rpt("2022","FR","Gas") = ElectricityGeneration_xtype_rpt("2022","France","Gas-CCGT") + ElectricityGeneration_xtype_rpt("2022","France","Gas-OCGT") + ElectricityGeneration_xtype_rpt("2022","France","Gas-ST")+ eps ;
noweffect_rpt("2022","FR","Nuclear") = ElectricityGeneration_xtype_rpt("2022","France","Nuclear") + eps ;
noweffect_rpt("2022","FR","Hydro") = ElectricityGeneration_xtype_rpt("2022","France","Hydro") + Storage_FR_rpt("2022","Res-ToMarket") + eps ;
noweffect_rpt("2022","FR","Wind") = ElectricityGeneration_xtype_rpt("2022","France","WindOn") + ElectricityGeneration_xtype_rpt("2022","France","WindOff") + eps ;
noweffect_rpt("2022","FR","Solar") = ElectricityGeneration_xtype_rpt("2022","France","Solar") + eps ;
noweffect_rpt("2022","FR","Imports") = domimport_ann_r("France","2022") + eps ;
noweffect_rpt("2022","FR","Exports") = domexport_ann_r("France","2022") + eps ;
noweffect_rpt("2022","FR","CO2 price") = Emissions_total_rpt("2022","CO2 price")  + eps ;
noweffect_rpt("2022","FR","CO2") = Emissions_rpt("2022","France","CO2-emissions-elec")  + eps ;
noweffect_rpt("2022","FR","Biomass") = ElectricityGeneration_xtype_rpt("2022","France","Bioenergy") + eps ;
noweffect_rpt("2022","FR","Oil") = ElectricityGeneration_xtype_rpt("2022","France" ,"OilOther") + eps ;

realeffect_rpt("2023","EUR","Gasuse") = eps ;
realeffect_rpt("2023","EUR","Price") = eps ;
realeffect_rpt("2023","EUR","Coal") = sum(r, gen_min2_chp_coal(r,"2023") + gen_min2_nochp_coal(r,"2023")) + eps ;
realeffect_rpt("2023","EUR","Lignite") = sum(r, gen_min2_chp_lign(r,"2023") + gen_min2_nochp_lign(r,"2023")) + eps ;
realeffect_rpt("2023","EUR","Gas") = sum(r, gen_min2_chp_ngas(r,"2023") + gen_min2_nochp_ngas(r,"2023")) + eps ;
realeffect_rpt("2023","EUR","Nuclear") = sum(r, gen_min2_nucl(r,"2023")) + eps ;
realeffect_rpt("2023","EUR","Hydro") = sum(r, gen_min2_hydr(r,"2023") + gen_resv(r,"2023")) + eps ;
realeffect_rpt("2023","EUR","Wind") = sum(r, gen_min2_wind(r,"2023")) + eps ;
realeffect_rpt("2023","EUR","Solar") = sum(r, gen_min2_sola(r,"2023")) + eps ;
realeffect_rpt("2023","EUR","Imports") = sum(r, imp(r,"2023")) + eps ;
realeffect_rpt("2023","EUR","Exports") = sum(r, expo(r,"2023")) + eps ;
realeffect_rpt("2023","EUR","CO2 price") = 81.48 + eps ;
$if not set co2price    realeffect_rpt("2023","EUR","CO2") = co2ele_org("2023") + co2eleuk_in("2023") + eps ;
realeffect_rpt("2023","EUR","Biomass") = sum(r, gen_min2_chp_biom(r,"2023") + gen_min2_nochp_biom(r,"2023")) + eps ;
realeffect_rpt("2023","EUR","oil") = sum(r, gen_min2_chp_oil(r,"2023") + gen_min2_nochp_oil(r,"2023")) + eps ;

realeffect_rpt("2023","GER","Gasuse") = eps ;
realeffect_rpt("2023","GER","Price") = 235.31 + eps ;
realeffect_rpt("2023","GER","Coal") = sum(rge(r), gen_min2_chp_coal(r,"2023") + gen_min2_nochp_coal(r,"2023")) + eps ;
realeffect_rpt("2023","GER","Lignite") = sum(rge(r), gen_min2_chp_lign(r,"2023") + gen_min2_nochp_lign(r,"2023")) + eps ;
realeffect_rpt("2023","GER","Gas") = sum(rge(r), gen_min2_chp_ngas(r,"2023") + gen_min2_nochp_ngas(r,"2023")) + eps ;
realeffect_rpt("2023","GER","Nuclear") = sum(rge(r), gen_min2_nucl(r,"2023")) + eps ;
realeffect_rpt("2023","GER","Hydro") = sum(rge(r), gen_min2_hydr(r,"2023") + gen_resv(r,"2023")) + eps ;
realeffect_rpt("2023","GER","Wind") = sum(rge(r), gen_min2_wind(r,"2023")) + eps ;
realeffect_rpt("2023","GER","Solar") = sum(rge(r), gen_min2_sola(r,"2023")) + eps ;
realeffect_rpt("2023","GER","Imports") = sum(rge(r), imp(r,"2023")) + eps ;
realeffect_rpt("2023","GER","Exports") = sum(rge(r), expo(r,"2023")) + eps ;
realeffect_rpt("2023","GER","CO2 price") = eps ;
realeffect_rpt("2023","GER","CO2") = eps ;
realeffect_rpt("2023","GER","Biomass") = sum(rge(r), gen_min2_chp_biom(r,"2023") + gen_min2_nochp_biom(r,"2023")) + eps ;
realeffect_rpt("2023","GER","oil") = sum(rge(r), gen_min2_chp_oil(r,"2023") + gen_min2_nochp_oil(r,"2023")) + eps ;

realeffect_rpt("2023","FR","Gasuse") = eps ;
realeffect_rpt("2023","FR","Price") = 365.52 + eps ;
realeffect_rpt("2023","FR","Coal") = sum(rfr(r), gen_min2_chp_coal(r,"2023") + gen_min2_nochp_coal(r,"2023")) + eps ;
realeffect_rpt("2023","FR","Lignite") = sum(rfr(r), gen_min2_chp_lign(r,"2023") + gen_min2_nochp_lign(r,"2023")) + eps ;
realeffect_rpt("2023","FR","Gas") = sum(rfr(r), gen_min2_chp_ngas(r,"2023") + gen_min2_nochp_ngas(r,"2023")) + eps ;
realeffect_rpt("2023","FR","Nuclear") = sum(rfr(r), gen_min2_nucl(r,"2023")) + eps ;
realeffect_rpt("2023","FR","Hydro") = sum(rfr(r), gen_min2_hydr(r,"2023") + gen_resv(r,"2023")) + eps ;
realeffect_rpt("2023","FR","Wind") = sum(rfr(r), gen_min2_wind(r,"2023")) + eps ;
realeffect_rpt("2023","FR","Solar") = sum(rfr(r), gen_min2_sola(r,"2023")) + eps ;
realeffect_rpt("2023","FR","Imports") = sum(rfr(r), imp(r,"2023")) + eps ;
realeffect_rpt("2023","FR","Exports") = sum(rfr(r), expo(r,"2023")) + eps ;
realeffect_rpt("2023","FR","CO2 price") = eps ;
realeffect_rpt("2023","FR","CO2") = eps ;
realeffect_rpt("2023","FR","Biomass") = sum(rfr(r), gen_min2_chp_biom(r,"2023") + gen_min2_nochp_biom(r,"2023")) + eps ;
realeffect_rpt("2023","FR","oil") = sum(rfr(r), gen_min2_chp_oil(r,"2023") + gen_min2_nochp_oil(r,"2023")) + eps ;

noweffect_rpt("2022","EUR","Pump") = Storage_total_rpt("2022","Pump-ToMarket") + eps ;
noweffect_rpt("2023","EUR","Pump") = Storage_total_rpt("2023","Pump-ToMarket") + eps ;
realeffect_rpt("2022","EUR","Pump") = sum(r, gen_pump(r,"2022")) + eps ;
realeffect_rpt("2023","EUR","Pump") = sum(r, gen_pump(r,"2023")) + eps ;

noweffect_rpt("2022","GER","Pump") = Storage_GER_rpt("2022","Pump-ToMarket") + eps ;
noweffect_rpt("2023","GER","Pump") = Storage_GER_rpt("2023","Pump-ToMarket") + eps ;
realeffect_rpt("2022","GER","Pump") = sum(rge(r), gen_pump(r,"2022")) + eps ;
realeffect_rpt("2023","GER","Pump") = sum(rge(r), gen_pump(r,"2023")) + eps ;

noweffect_rpt("2022","FR","Pump") = Storage_FR_rpt("2022","Pump-ToMarket") + eps ;
noweffect_rpt("2023","FR","Pump") = Storage_FR_rpt("2023","Pump-ToMarket") + eps ;
realeffect_rpt("2022","FR","Pump") = sum(rfr(r), gen_pump(r,"2022")) + eps ;
realeffect_rpt("2023","FR","Pump") = sum(rfr(r), gen_pump(r,"2023")) + eps ;


effect_rpt(t,"Price") = Electricity1_total_rpt(t,"price-avg") + eps ;
effect_rpt(t,"GER-Price") = Electricity1_rpt(t,"Germany","price-avg") + eps ;
effect_rpt(t,"FR-Price") = Electricity1_rpt(t,"France","price-avg") + eps ;
effect_rpt(t,"CO2") = Emissions_total_rpt(t,"Total CO2-emissions-elec") + eps ;
effect_rpt(t,"GER-CO2") = Emissions_rpt(t,"Germany","CO2-emissions-elec")  + eps ;
effect_rpt(t,"FR-CO2") = Emissions_rpt(t,"France","CO2-emissions-elec")  + eps ;
effect_rpt(t,"CO2 price") = Emissions_total_rpt(t,"CO2 price") + eps ;
effect_rpt(t,"GER-CO2 price") = Emissions_rpt(t,"Germany","CO2-price")  + eps ;
effect_rpt(t,"FR-CO2 price") = Emissions_rpt(t,"France","CO2-price")  + eps ;
effect_rpt(t,"Gasuse") = gasuseeu(t) + eps ;
effect_rpt(t,"GER-Gasuse") = gasuse("Germany",t) + eps ;
effect_rpt(t,"FR-Gasuse") = gasuse("France",t) + eps ;

tv("2020","2020") = YES ;

$if set costcal    $include    modules\euregen2024_cost_rpt_v1

parameter
lostload(r,t) TWh
lostload_total(t) TWh
;

lostload(r,t) = sum(s, hours(s) * BS.L(s,r,t)) * 1e-3
$if set elastic + sum(bse, sum(s, hours(s) * BSELAS.L(bse,s,r,t))) * 1e-3
                + eps ;

lostload_total(t) = sum(r, lostload(r,t)) + eps ;

$if set learning  $include  modules\euregen2024_lea_rpt_v1

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

* * * Set exogenous investment schock variables when running Ukrain Russian war scenarios
parameters
ixfx(i,r,t)
itfx(k,r,r,t)
igfx(j,r,t)
igfx(j,r,t)
;

ixfx(i,r,topt2030(t))     = IX.L(i,r,t) ;
itfx(k,r,rr,topt2030(t))  = IT.L(k,r,rr,t) ;
igfx(j,r,topt2030(t))     = IGL(j,r,t) ;

$if      set writelimits     execute_unload 'limits\%l%_%s%.gdx', ixfx, itfx, igfx ;

parameter
capt_static(i,v,r,t)
gcapt_static(j,v,r,t)
gcaptnv_static(j,r,t)
tcapt_static(k,r,r,t)
co2eleeu_static(t)
co2eleuk_static(t)
;

capt_static(i,v,r,t) = XC.L(i,v,r,t) ;
gcapt_static(j,v,r,t) = GC.L(j,v,r,t) ;
gcaptnv_static(j,r,t) = GCNV.L(j,r,t) ;
tcapt_static(k,r,rr,t) = TC.L(k,r,rr,t) ;
co2eleeu_static(t) = ECEU.L(t) ;
co2eleuk_static(t) = ECUK.L(t) ;

$if      set staticcap       execute_unload 'static\%l%_%s%.gdx', capt_static,gcapt_static,gcaptnv_static,tcapt_static,co2eleeu_static,co2eleuk_static,co2prreg;

* * * Set exogonous electriicty co2 emissions into the iterative file
parameter
co2elec_out(t) 
co2ind_out(t)
co2elecuk_out(t)
;

co2elec_out(t) = co2ele_in(t) + eps ;
co2elec_out(t)$(ECEU.L(t) > 0) = ECEU.L(t) + eps ;
co2elec_out(t)$(ECEU.L(t) = 0) = ECEU.L(t) + eps ;
co2elec_out(t)$(ECEU.L(t) < 0) = ECEU.L(t) + eps ;
co2ind_out(t) = co2ind_in(t) + eps ;
co2ind_out(t)$(ECEU.L(t) > 0) = ECEU.L(t) * co2elecind(t) + co2indorgfix_in(t)  + eps ;
co2ind_out(t)$(ECEU.L(t) < 0) = ECEU.L(t) * co2elecind(t) + co2indorgfix_in(t)  + eps ;

co2elecuk_out(t) = co2eleuk_in(t) + eps ;
co2elecuk_out(t)$(ECUK.L(t) > 0) = ECUK.L(t) + eps ;
co2elecuk_out(t)$(ECUK.L(t) < 0) = ECUK.L(t) + eps ; 

Emissions_total_rpt(t,"CO2elec-EUETS") = co2elec_out(t) ;
Emissions_total_rpt(t,"CO2ind-EUETS") = co2ind_out(t) ;
Emissions_total_rpt(t,"CO2-EUETS") = co2elec_out(t) + co2ind_out(t) ;
Emissions_total_rpt(t,"CO2elec-UKETS") = co2elecuk_out(t) ;

$if      set co2iter     execute_unload   'euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx',             co2elec_out, co2ind_out ;
$if      set co2iter     execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\%s%.xlsx           	                         par=co2elec_out rng=co2elec_out!a1'
$if      set iter31s     execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%_31s\%s%.xlsx                                   par=co2elec_out rng=co2elec_out!a1'
$if      set iter119s    execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%_119s\%s%.xlsx                                  par=co2elec_out rng=co2elec_out!a1'
* Newcap bauprice without load
*$if      set newiter     execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\newcap_bauprice.xlsx                           par=co2elec_out rng=co2elec_out!a1'
* Newcap bauprice with load
*$if      set newiter     execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\newcap_bauprice_loa.xlsx                       par=co2elec_out rng=co2elec_out!a1'
$if      set newiter     execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\newcap_bauprice_frn_loa.xlsx                   par=co2elec_out rng=co2elec_out!a1'
$if      set newiter     execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\newcap_bauprice_hyd_loa.xlsx                   par=co2elec_out rng=co2elec_out!a1'
$if      set newiter     execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\newcap_bauprice_frn_hyd_loa.xlsx               par=co2elec_out rng=co2elec_out!a1'
* Newcap reco with load
$if      set newiter     execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\newcap_reco_loa.xlsx                           par=co2elec_out rng=co2elec_out!a1'
$if      set newiter     execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\newcap_reco_frn_loa.xlsx                       par=co2elec_out rng=co2elec_out!a1'
$if      set newiter     execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\newcap_reco_hyd_loa.xlsx                       par=co2elec_out rng=co2elec_out!a1'
$if      set newiter     execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\newcap_reco_frn_hyd_loa.xlsx                   par=co2elec_out rng=co2elec_out!a1'
$if      set newiter     execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\newcap_reco_str_frn_hyd_loa.xlsx               par=co2elec_out rng=co2elec_out!a1'
* Newcap high with load
$if      set newiter     execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\newcap_high_loa.xlsx                           par=co2elec_out rng=co2elec_out!a1'
$if      set newiter     execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\newcap_high_frn_loa.xlsx                       par=co2elec_out rng=co2elec_out!a1'
$if      set newiter     execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\newcap_high_hyd_loa.xlsx                       par=co2elec_out rng=co2elec_out!a1'
$if      set newiter     execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\newcap_high_frn_hyd_loa.xlsx                   par=co2elec_out rng=co2elec_out!a1'
$if      set newiter     execute          'gdxxrw.exe euetsmsr2023\co2iter_%pm%\co2out_%s%.gdx    o=euetsmsr2023\co2iter_%pm%\newcap_high_str_frn_hyd_loa.xlsx               par=co2elec_out rng=co2elec_out!a1'

$if not  set elastic   $if not set days $if not set hours   execute_unload'elastic\%s%_priceref.gdx' price, Electricity1_rpt ;

execute_unload'report\%e%_rpt.gdx',                              Emissions_total_rpt,
$if      set details                                             Emissions_rpt, Emissions_GER_rpt, Emissions_FR_rpt,
                                                                 Electricity1_total_rpt,
$if      set details                                             Electricity1_rpt,
$if      set details                                             Electricity1_GER_rpt,
$if      set details                                             Electricity1_FR_rpt,
                                                                 Electricity2_total_rpt,
$if      set details                                             Electricity2_rpt,
$if      set details                                             Electricity2_GER_rpt,
$if      set details                                             Electricity2_FR_rpt,
                                                                 Electricity3_total_rpt,
$if      set details                                             Electricity3_rpt,
$if      set details                                             Electricity3_GER_rpt,
$if      set details                                             Electricity3_FR_rpt,

*$if      set details                                            Electricity1_UK_rpt,
*$if      set details                                            Electricity2_UK_rpt,
*$if      set details                                            Electricity3_UK_rpt,
*$if      set details                                            Electricity1_DK_rpt,
*$if      set details                                            Electricity2_DK_rpt,
*$if      set details                                            Electricity3_DK_rpt,
*$if      set details                                            Electricity1_NO_rpt,
*$if      set details                                            Electricity2_NO_rpt,
*$if      set details                                            Electricity3_NO_rpt,
*$if      set details                                            Electricity1_PL_rpt,
*$if      set details                                            Electricity2_PL_rpt,
*$if      set details                                            Electricity3_PL_rpt,
*$if      set details                                            Electricity1_ES_rpt,
*$if      set details                                            Electricity2_ES_rpt,
*$if      set details                                            Electricity3_ES_rpt,
                                                                 ElectricityGeneration_total_xtype_rpt,
$if      set details                                             ElectricityGeneration_xtype_rpt,
$if      set details                                             ElectricityGeneration_GER_xtype_rpt,
$if      set details                                             ElectricityGeneration_FR_xtype_rpt,
                                                                 InstalledCapacities_total_xtype_rpt,
$if      set details                                             InstalledCapacities_xtype_rpt,
$if      set details                                             InstalledCapacities_GER_xtype_rpt,
$if      set details                                             InstalledCapacities_FR_xtype_rpt,
                                                                 AddedCapacities_total_xtype_rpt,
$if      set details                                             AddedCapacities_xtype_rpt, AddedCapacities_GER_xtype_rpt,AddedCapacities_FR_xtype_rpt,
                                                                 AccAddedCapacities_total_xtype_rpt,
$if      set details                                             AccAddedCapacities_xtype_rpt, AccAddedCapacities_GER_xtype_rpt,AccAddedCapacities_FR_xtype_rpt,
$if      set costcal                                             capcost_rrpt,fomcost_rrpt,vomcost_rrpt,effrate_rrpt,emit_rrpt,irnwlim_rrpt,irnwflh_windon_rrpt,irnwflh_windoff_rrpt,irnwflh_openpv_rrpt,irnwflh_roofpv_rrpt,
$if      set costcal                                             pfuel_rrpt,pco2_rrpt,price_rrpt,pricetot_rrpt,irnwflh_windon90_rrpt,irnwflh_windoff90_rrpt,irnwflh_openpv90_rrpt,irnwflh_roofpv90_rrpt,irnwflh_rrpt,irnwflh90_rrpt
$if      set costcal                                             gcapcost_rrpt,gfomcost_rrpt,gvomcost_rrpt,geffrate_rrpt,tcapcost_rrpt,tfomcost_rrpt,tvomcost_rrpt,teffrate_rrpt,daref,daref_ind,daref_res,daref_com,daref_tra,daref_oth,daref_hyd
$if      set learningcal                                         capirnw_q,capirnw,capirnw_max,limirnw_q,limirnw,limirnw_max,shairnw_q,shairnw,shairnw_max,
$if      set flhcal                                              flhreport, flhpoteni, investflh_irt, investflh_it, investflh_rt, investflh_ir, investflh_i, investflh_r, investflh_t, investflh, investflheur_it, investflheur_i, investflheur_t, investflheur,
$if      set flhcal                                              stock, accspendings, flheur_ls_rpt, flheur_cost_rpt, flheur_stock_rpt, flheur_check_rpt
                                                                 Storage_total_rpt,
$if      set details                                             Storage_rpt, Storage_GER_rpt, Storage_FR_rpt,
                                                                 NTCTransfers_total_rpt
$if      set details                                             ,NTCTransfers_rpt, NTCTransfers_GER_rpt,NTCTransfers_FR_rpt,
$if      set details                                             gasuseeu, noweffect_rpt, realeffect_rpt, effect_rpt,price_rpt,gflh,
$if      set systemcost                                          SystemCost_rpt, SystemCost_GER_rpt, SystemCost_FR_rpt, TotalSystemCost_rpt, AggreSystemCost_rpt, TotalAggreSystemCost_rpt, Interval_rpt, TotalInterval_rpt,
$if      set systemcost                                          ihk_rpt, ihk_GER_rpt, 
$if      set details                                             lostload, lostload_total, euets_rpt
                                                                 ;
                                                                
$if      set excelsimple             execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Emissions_total_rpt rng=Emissions!a1'
$if      set excelsimple             execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Emissions_GER_rpt rng=Emissions_GER!a1'
$if      set excelsimple             execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Emissions_FR_rpt rng=Emissions_FR!a1'

$if      set excelsimple             execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Electricity1_total_rpt rng=Electricity1!a1'
*$if      set excelsimple             execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Electricity2_total_rpt rng=Electricity2!a1'
*$if      set excelsimple             execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Electricity3_total_rpt rng=Electricity3!a1'

$if      set excelsimple             execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Electricity1_GER_rpt rng=Electricity1_GER!a1'
*$if      set excelsimple             execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Electricity2_GER_rpt rng=Electricity2_GER!a1'
*$if      set excelsimple             execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Electricity3_GER_rpt rng=Electricity3_GER!a1'

$if      set excelsimple             execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Electricity1_FR_rpt rng=Electricity1_FR!a1'
*$if      set excelsimple             execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Electricity2_FR_rpt rng=Electricity2_FR!a1'
*$if      set excelsimple             execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Electricity3_FR_rpt rng=Electricity3_FR!a1'

$if      set flhcal                  execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=stock rng=stock!a1 par=accspendings rng=accspendings!a1 par=flheur_ls_rpt rng=flheur_ls_rpt!a1 par=flheur_cost_rpt rng=flheur_cost_rpt!a1 par=flheur_check_rpt rng=flheur_check_rpt!a1 par=flheur_stock_rpt rng=flheur_stock_rpt!a1'                                    
$if      set flhcal                  execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Electricity1_GER_rpt rng=Electricity1_GER!a1'
$if      set flhcal                  execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Electricity2_GER_rpt rng=Electricity2_GER!a1'
$if      set flhcal                  execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Electricity3_GER_rpt rng=Electricity3_GER!a1'

$if      set flhcal                  execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Electricity1_FR_rpt rng=Electricity1_FR!a1'
$if      set flhcal                  execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Electricity2_FR_rpt rng=Electricity2_FR!a1'
$if      set flhcal                  execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Electricity3_FR_rpt rng=Electricity3_FR!a1'

$if      set flhcal                  execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Electricity1_UK_rpt rng=Electricity1_UK!a1'
$if      set flhcal                  execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Electricity2_UK_rpt rng=Electricity2_UK!a1'
$if      set flhcal                  execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Electricity3_UK_rpt rng=Electricity3_UK!a1'

$if      set flhcal                  execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Electricity1_DK_rpt rng=Electricity1_DK!a1'
$if      set flhcal                  execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Electricity2_DK_rpt rng=Electricity2_DK!a1'
$if      set flhcal                  execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Electricity3_DK_rpt rng=Electricity3_DK!a1'

$if      set flhcal                  execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Electricity1_NO_rpt rng=Electricity1_NO!a1'
$if      set flhcal                  execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Electricity2_NO_rpt rng=Electricity2_NO!a1'
$if      set flhcal                  execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Electricity3_NO_rpt rng=Electricity3_NO!a1'

$if      set flhcal                  execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Electricity1_PL_rpt rng=Electricity1_PL!a1'
$if      set flhcal                  execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Electricity2_PL_rpt rng=Electricity2_PL!a1'
$if      set flhcal                  execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Electricity3_PL_rpt rng=Electricity3_PL!a1'

$if      set flhcal                  execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Electricity1_ES_rpt rng=Electricity1_ES!a1'
$if      set flhcal                  execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Electricity2_ES_rpt rng=Electricity2_ES!a1'
$if      set flhcal                  execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Electricity3_ES_rpt rng=Electricity3_ES!a1'

$if      set excelsimple             execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=ElectricityGeneration_total_xtype_rpt rng=Generation!a1'
$if      set excelsimple             execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=ElectricityGeneration_GER_xtype_rpt rng=Generation_GER!a1'
$if      set excelsimple             execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=ElectricityGeneration_FR_xtype_rpt rng=Generation_FR!a1'
$if      set excelsimple             execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=AddedCapacities_total_xtype_rpt rng=Added!a1'
$if      set excelsimple             execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=AddedCapacities_GER_xtype_rpt rng=Added_GER!a1'
$if      set excelsimple             execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=AddedCapacities_FR_xtype_rpt rng=Added_FR!a1'
$if      set excelsimple             execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=AccAddedCapacities_total_xtype_rpt rng=AccAdded!a1'
$if      set excelsimple             execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=AccAddedCapacities_GER_xtype_rpt rng=AccAdded_GER!a1'
$if      set excelsimple             execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=AccAddedCapacities_FR_xtype_rpt rng=AccAdded_FR!a1'
$if      set excelsimple             execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=InstalledCapacities_total_xtype_rpt rng=Installed!a1'
$if      set excelsimple             execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=InstalledCapacities_GER_xtype_rpt rng=Installed_GER!a1'
$if      set excelsimple             execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=InstalledCapacities_FR_xtype_rpt rng=Installed_FR!a1'
$if      set excelsimple             execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Storage_total_rpt rng=Storage!a1'
$if      set excelsimple             execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Storage_GER_rpt rng=Storage_GER!a1'
$if      set excelsimple             execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=Storage_FR_rpt rng=Storage_FR!a1'
$if      set excelsimple             execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=NTCTransfers_total_rpt rng=NTCTransfers!a1'
$if      set excelsimple             execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=NTCTransfers_GER_rpt rng=NTCTransfers_GER!a1'
$if      set excelsimple             execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=NTCTransfers_FR_rpt rng=NTCTransfers_FR!a1'
$if      set excelsimple             execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=noweffect_rpt rng=noweffect!a1 par=realeffect_rpt rng=realeffect!a1 par=effect_rpt rng=effect!a1 par=price_rpt rng=price!a1'
$if      set systemcost              execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=ihk_rpt rng=ihk!a1'
$if      set systemcost              execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=ihk_GER_rpt rng=ihk_GER!a1'

$if      set costcal                 execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=capcost_rrpt rng=capcost!a1 par=irnwlim_rrpt         rng=irnwlim!a1          par=pfuel_rrpt rng=pfuel!a1 par=daref_ind rng=daref_ind!a1'
$if      set costcal                 execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=fomcost_rrpt rng=fomcost!a1 par=irnwflh_windon_rrpt  rng=irnwflh_windon!a1   par=pco2_rrpt rng=pco2!a1 par=daref_res rng=daref_res!a1'
$if      set costcal                 execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=vomcost_rrpt rng=vomcost!a1 par=irnwflh_windoff_rrpt rng=irnwflh_windoff!a1  par=daref rng=daref!a1 par=daref_com rng=daref_com!a1'
$if      set costcal                 execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=effrate_rrpt rng=effrate!a1 par=irnwflh_openpv_rrpt  rng=irnwflh_openpv!a1   par=price_rrpt rng=price!a1 par=daref_tra rng=daref_tra!a1'
$if      set costcal                 execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=emit_rrpt    rng=emit!a1    par=irnwflh_roofpv_rrpt  rng=irnwflh_roofpv!a1   par=pricetot_rrpt rng=pricetot!a1 par=daref_oth rng=daref_oth!a1'

$if      set costcal                 execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=irnwflh_windon90_rrpt  rng=irnwflh_windon90!a1   par=irnwflh_rrpt  rng=irnwflh!a1 par=daref_hyd rng=daref_hyd!a1'
$if      set costcal                 execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=irnwflh_windoff90_rrpt rng=irnwflh_windoff90!a1  par=irnwflh90_rrpt  rng=irnwflh90!a1'
$if      set costcal                 execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=irnwflh_openpv90_rrpt  rng=irnwflh_openpv90!a1'
$if      set costcal                 execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=irnwflh_roofpv90_rrpt  rng=irnwflh_roofpv90!a1'

$if      set costcal                 execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=gcapcost_rrpt rng=gcapcost!a1 par=gfomcost_rrpt      rng=gfomcost!a1         par=gvomcost_rrpt      rng=gvomcost!a1         par=geffrate_rrpt rng=geffrate!a1'
$if      set costcal                 execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=tcapcost_rrpt rng=tcapcost!a1 par=tfomcost_rrpt      rng=tfomcost!a1         par=tvomcost_rrpt      rng=tvomcost!a1         par=teffrate_rrpt rng=teffrate!a1'
$if      set systemcost              execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=TotalSystemCost_rpt rng=SystemCost!a1'
$if      set systemcost              execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=SystemCost_GER_rpt rng=SystemCost_GER!a1'
$if      set systemcost              execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=SystemCost_FR_rpt rng=SystemCost_FR!a1'
$if      set systemcost              execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=TotalAggreSystemCost_rpt rng=AgreeCost_total!a1 par=TotalInterval_rpt rng=TotalInterval!a1'
$if      set excelsimple             execute 'gdxxrw.exe report\%e%_rpt.gdx o=excel\%e%.xlsx par=euets_rpt rng=euets!a1'