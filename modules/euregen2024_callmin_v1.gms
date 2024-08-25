* * * Calibration of past years and old vintage generation
parameter
imp(r,t)
expo(r,t)
;

$gdxin precal\precal_%n%.gdx
$load imp,expo
$gdxin

parameter
taf_expo(r,t)
taf_impo(r,t)
;

$gdxin precal\precal_%n%.gdx
$load taf_expo,taf_impo
$gdxIn

parameter
gen_pump(r,t)
gen_resv(r,t)
* Other
gen_min2_nucl(r,t)
gen_min2_sola(r,t)
gen_min2_wind(r,t)
gen_min2_hydr(r,t)
* CHP/NOCHP
gen_min2_ngas(r,t)
gen_min2_biom(r,t)
gen_min2_lign(r,t)
gen_min2_oil(r,t)
gen_min2_coal(r,t)
* CHP
gen_min2_chp_ngas(r,t)
gen_min2_chp_biom(r,t)
gen_min2_chp_lign(r,t)
gen_min2_chp_oil(r,t)
gen_min2_chp_coal(r,t)
* NOCHP
gen_min2_nochp_ngas(r,t)
gen_min2_nochp_biom(r,t)
gen_min2_nochp_lign(r,t)
gen_min2_nochp_oil(r,t)
gen_min2_nochp_coal(r,t)
;

$gdxin precal\precal_%n%.gdx
$load gen_pump,gen_resv
$load gen_min2_nucl,gen_min2_hydr,gen_min2_sola,gen_min2_wind
$load gen_min2_ngas
$load gen_min2_biom
$load gen_min2_lign
$load gen_min2_oil
$load gen_min2_coal
$load gen_min2_chp_ngas
$load gen_min2_chp_biom
$load gen_min2_chp_lign
$load gen_min2_chp_oil
$load gen_min2_chp_coal
$load gen_min2_nochp_ngas
$load gen_min2_nochp_biom
$load gen_min2_nochp_lign
$load gen_min2_nochp_oil
$load gen_min2_nochp_coal
$gdxin

$if set oldcap_baup Parameter
$if set oldcap_baup gen_min2_nochp_biom_int(r,t)
$if set oldcap_baup gen_min2_nochp_lign_int(r,t)
$if set oldcap_baup gen_min2_nochp_oil_int(r,t)
$if set oldcap_baup gen_min2_nochp_coal_int(r,t)
$if set oldcap_baup ;

$if set oldcap_baup gen_min2_nochp_coal_int(r,"2023") = max(gen_min2_nochp_coal(r,"2023"),gen_min2_nochp_coal(r,"2022")) ;
$if set oldcap_baup gen_min2_nochp_lign_int(r,"2023") = max(gen_min2_nochp_lign(r,"2023"),gen_min2_nochp_lign(r,"2022")) ;
$if set oldcap_baup gen_min2_nochp_oil_int(r,"2023") = max(gen_min2_nochp_oil(r,"2023"),gen_min2_nochp_oil(r,"2022")) ;
$if set oldcap_baup gen_min2_nochp_biom_int(r,"2023") = max(gen_min2_nochp_biom(r,"2023"),gen_min2_nochp_biom(r,"2022")) ;

$if set oldcap_baup gen_min2_nochp_coal(r,"2023") = gen_min2_nochp_coal_int(r,"2023") ;
$if set oldcap_baup gen_min2_nochp_lign(r,"2023") = gen_min2_nochp_lign_int(r,"2023") ;
$if set oldcap_baup gen_min2_nochp_oil(r,"2023") = gen_min2_nochp_oil_int(r,"2023") ;
$if set oldcap_baup gen_min2_nochp_biom(r,"2023") = gen_min2_nochp_biom_int(r,"2023") ;