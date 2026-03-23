* * * Calibration of past years and old vintage generation
set
genset /nucl,sola,wind,hydr,pump,resv,ngas,biom,lign,oil,coal,ngas_chp,biom_chp,lign_chp,oil_chp,coal_chp,ngas_nochp,biom_nochp,lign_nochp,oil_nochp,coal_nochp,ngas_tog,biom_tog,
        lign_tog,oil_tog,coal_tog,sum,sum_dif,sum_norchp,sum_chp_nochp,sum_chp,sum_nochp,daref,daref_loss/
nor(genset) /nucl,sola,wind,hydr,pump,resv,ngas,biom,lign,oil,coal/
norchp(genset) /ngas,biom,lign,oil,coal/
genset_chp(genset) /ngas_chp,biom_chp,lign_chp,oil_chp,coal_chp/
genset_nochp(genset) /ngas_nochp,biom_nochp,lign_nochp,oil_nochp,coal_nochp/
res(genset) /nucl,sola,wind,hydr,pump,resv/
;

parameter
imp(r,t)
expo(r,t)
* Other
gen_nucl(r,t)
gen_sola(r,t)
gen_wind(r,t)
gen_hydr(r,t)
gen_pump(r,t)
gen_resv(r,t)
* CHP/NOCHP
gen_ngas(r,t)
gen_biom(r,t)
gen_lign(r,t)
gen_oil(r,t)
gen_coal(r,t)
* CHP
gen_chp_ngas(r,t)
gen_chp_biom(r,t)
gen_chp_lign(r,t)
gen_chp_oil(r,t)
gen_chp_coal(r,t)
* NOCHP
gen_nochp_ngas(r,t)
gen_nochp_biom(r,t)
gen_nochp_lign(r,t)
gen_nochp_oil(r,t)
gen_nochp_coal(r,t)
* Other
cap_nucl(r,t)
cap_sola(r,t)
cap_wind(r,t)
cap_hydr(r,t)
cap_pump(r,t)
cap_resv(r,t)
* CHP/NOCHP
cap_ngas(r,t)
cap_biom(r,t)
cap_lign(r,t)
cap_oil(r,t)
cap_coal(r,t)
* CHP
cap_chp_ngas(r,t)
cap_chp_biom(r,t)
cap_chp_lign(r,t)
cap_chp_oil(r,t)
cap_chp_coal(r,t)
* NOCHP
cap_nochp_ngas(r,t)
cap_nochp_biom(r,t)
cap_nochp_lign(r,t)
cap_nochp_oil(r,t)
cap_nochp_coal(r,t)
;

$gdxin database\setpar_%n%.gdx
$load gen_nucl
$load gen_ngas
$load gen_biom
$load gen_lign
$load gen_oil
$load gen_coal
$load gen_wind
$load gen_sola
$load gen_hydr
$load gen_pump
$load gen_resv
$load imp,expo
$load gen_chp_ngas,gen_chp_biom,gen_chp_lign,gen_chp_oil,gen_chp_coal
$load gen_nochp_ngas,gen_nochp_biom,gen_nochp_lign,gen_nochp_oil,gen_nochp_coal
$gdxin

Set
set_chp /TWh,GW,effrate,Twhheat,Gwheat,effrateheat,TWhth,effratetot,Coal,Oil,Lignite,Gasd,hd,Bioenergy/
;

Parameter
chpinfo2021(r,set_chp)
;

$gdxin database\data_chp\chp_shortrun_out.gdx
$load chpinfo2021
$gdxin


parameter
gen_new(genset,r,t)
gen_new_all(genset,t)
;

gen_coal("Czech",t)$(t.val le 2023 and sum(v, capt("Coa_CHP",v,"Czech",t)) > 0) = chpinfo2021("Czech","Coal") * sum(v, capt("Coa_CHP",v,"Czech",t) * effrate("Coa_CHP",v,"Czech")) / sum(v, capt("Coa_CHP",v,"Czech",t)) ;
gen_coal("Romania",t)$(t.val le 2023 and sum(v, capt("Coa_CHP",v,"Romania",t)) > 0) = chpinfo2021("Romania","Coal") * sum(v, capt("Coa_CHP",v,"Romania",t) * effrate("Coa_CHP",v,"Romania")) / sum(v, capt("Coa_CHP",v,"Romania",t)) ;
gen_coal("Hungary",t)$(t.val le 2023 and sum(v, capt("Coa_CHP",v,"Hungary",t)) > 0) = chpinfo2021("Hungary","Coal") * sum(v, capt("Coa_CHP",v,"Hungary",t) * effrate("Coa_CHP",v,"Hungary")) / sum(v, capt("Coa_CHP",v,"Hungary",t)) ;
gen_coal("Bulgaria",t)$(t.val le 2023 and sum(v, capt("Coa_CHP",v,"Bulgaria",t)) > 0) = chpinfo2021("Bulgaria","Coal") * sum(v, capt("Coa_CHP",v,"Bulgaria",t) * effrate("Coa_CHP",v,"Bulgaria")) / sum(v, capt("Coa_CHP",v,"Bulgaria",t)) ;

gen_chp_coal("Czech",t)$(t.val le 2023 and sum(v, capt("Coa_CHP",v,"Czech",t)) > 0) = chpinfo2021("Czech","Coal") * sum(v, capt("Coa_CHP",v,"Czech",t) * effrate("Coa_CHP",v,"Czech")) / sum(v, capt("Coa_CHP",v,"Czech",t)) ;
gen_chp_coal("Romania",t)$(t.val le 2023 and sum(v, capt("Coa_CHP",v,"Romania",t)) > 0) = chpinfo2021("Romania","Coal") * sum(v, capt("Coa_CHP",v,"Romania",t) * effrate("Coa_CHP",v,"Romania")) / sum(v, capt("Coa_CHP",v,"Romania",t)) ;
gen_chp_coal("Hungary",t)$(t.val le 2023 and sum(v, capt("Coa_CHP",v,"Hungary",t)) > 0) = chpinfo2021("Hungary","Coal") * sum(v, capt("Coa_CHP",v,"Hungary",t) * effrate("Coa_CHP",v,"Hungary")) / sum(v, capt("Coa_CHP",v,"Hungary",t)) ;
gen_chp_coal("Bulgaria",t)$(t.val le 2023 and sum(v, capt("Coa_CHP",v,"Bulgaria",t)) > 0) = chpinfo2021("Bulgaria","Coal") * sum(v, capt("Coa_CHP",v,"Bulgaria",t) * effrate("Coa_CHP",v,"Bulgaria")) / sum(v, capt("Coa_CHP",v,"Bulgaria",t)) ;

gen_new("Nucl",r,t) = gen_nucl(r,t) ;
gen_new("ngas",r,t) = gen_ngas(r,t) ;
gen_new("biom",r,t) = gen_biom(r,t) ;
gen_new("Lign",r,t) = gen_lign(r,t) ;
gen_new("Oil",r,t) = gen_oil(r,t) ;
gen_new("Coal",r,t) = gen_coal(r,t) ;
gen_new("Wind",r,t) = gen_wind(r,t) ;
gen_new("Sola",r,t) = gen_sola(r,t) ;
gen_new("Hydr",r,t) = gen_hydr(r,t) ;
gen_new("Pump",r,t) = gen_pump(r,t) ;
gen_new("Resv",r,t) = gen_resv(r,t) ;

gen_new("ngas_chp",r,t) = gen_chp_ngas(r,t) ;
gen_new("Biom_chp",r,t) = gen_chp_biom(r,t) ;
gen_new("Lign_chp",r,t) = gen_chp_lign(r,t) ;
gen_new("Oil_chp",r,t) = gen_chp_oil(r,t) ;
gen_new("Coal_chp",r,t) = gen_chp_coal(r,t) ;

gen_new("ngas_nochp",r,t) = gen_nochp_ngas(r,t) ;
gen_new("Biom_nochp",r,t) = gen_nochp_biom(r,t) ;
gen_new("Lign_nochp",r,t) = gen_nochp_lign(r,t) ;
gen_new("Oil_nochp",r,t) = gen_nochp_oil(r,t) ;
gen_new("Coal_nochp",r,t) = gen_nochp_coal(r,t) ;

gen_new("ngas_tog",r,t) = gen_new("ngas_nochp",r,t) + gen_new("ngas_chp",r,t) ;
gen_new("Biom_tog",r,t) = gen_new("biom_nochp",r,t) + gen_new("biom_chp",r,t) ;
gen_new("Lign_tog",r,t) = gen_new("lign_nochp",r,t) + gen_new("lign_chp",r,t) ;
gen_new("Oil_tog",r,t) = gen_new("oil_nochp",r,t) + gen_new("oil_chp",r,t) ;
gen_new("Coal_tog",r,t) = gen_new("coal_nochp",r,t) + gen_new("coal_chp",r,t) ;

gen_new("sum_chp",r,t) = sum(genset_chp(genset), gen_new(genset,r,t)) ;

gen_new("ngas_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_ngas(r,t),gen_chp_ngas(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Biom_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_biom(r,t),gen_chp_biom(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Lign_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_lign(r,t),gen_chp_lign(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Oil_chp",r,t)$gen_new("sum_chp",r,t)  = min(gen_oil(r,t),gen_chp_oil(r,t)   * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Coal_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_coal(r,t),gen_chp_coal(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;

gen_new("sum_chp",r,t) = sum(genset_chp(genset), gen_new(genset,r,t)) ;

gen_new("ngas_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_ngas(r,t),gen_chp_ngas(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Biom_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_biom(r,t),gen_chp_biom(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Lign_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_lign(r,t),gen_chp_lign(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Oil_chp",r,t)$gen_new("sum_chp",r,t)  = min(gen_oil(r,t),gen_chp_oil(r,t)   * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Coal_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_coal(r,t),gen_chp_coal(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;

gen_new("sum_chp",r,t) = sum(genset_chp(genset), gen_new(genset,r,t)) ;

gen_new("ngas_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_ngas(r,t),gen_chp_ngas(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Biom_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_biom(r,t),gen_chp_biom(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Lign_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_lign(r,t),gen_chp_lign(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Oil_chp",r,t)$gen_new("sum_chp",r,t)  = min(gen_oil(r,t),gen_chp_oil(r,t)   * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Coal_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_coal(r,t),gen_chp_coal(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;

gen_new("sum_chp",r,t) = sum(genset_chp(genset), gen_new(genset,r,t)) ;

gen_new("ngas_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_ngas(r,t),gen_chp_ngas(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Biom_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_biom(r,t),gen_chp_biom(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Lign_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_lign(r,t),gen_chp_lign(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Oil_chp",r,t)$gen_new("sum_chp",r,t)  = min(gen_oil(r,t),gen_chp_oil(r,t)   * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Coal_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_coal(r,t),gen_chp_coal(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;

gen_new("sum_chp",r,t) = sum(genset_chp(genset), gen_new(genset,r,t)) ;

gen_new("ngas_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_ngas(r,t),gen_chp_ngas(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Biom_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_biom(r,t),gen_chp_biom(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Lign_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_lign(r,t),gen_chp_lign(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Oil_chp",r,t)$gen_new("sum_chp",r,t)  = min(gen_oil(r,t),gen_chp_oil(r,t)   * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Coal_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_coal(r,t),gen_chp_coal(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;

gen_new("sum_chp",r,t) = sum(genset_chp(genset), gen_new(genset,r,t)) ;

gen_new("ngas_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_ngas(r,t),gen_chp_ngas(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Biom_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_biom(r,t),gen_chp_biom(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Lign_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_lign(r,t),gen_chp_lign(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Oil_chp",r,t)$gen_new("sum_chp",r,t)  = min(gen_oil(r,t),gen_chp_oil(r,t)   * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Coal_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_coal(r,t),gen_chp_coal(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;

gen_new("sum_chp",r,t) = sum(genset_chp(genset), gen_new(genset,r,t)) ;

gen_new("ngas_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_ngas(r,t),gen_chp_ngas(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Biom_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_biom(r,t),gen_chp_biom(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Lign_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_lign(r,t),gen_chp_lign(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Oil_chp",r,t)$gen_new("sum_chp",r,t)  = min(gen_oil(r,t),gen_chp_oil(r,t)   * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Coal_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_coal(r,t),gen_chp_coal(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;

gen_new("sum_chp",r,t) = sum(genset_chp(genset), gen_new(genset,r,t)) ;

gen_new("ngas_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_ngas(r,t),gen_chp_ngas(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Biom_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_biom(r,t),gen_chp_biom(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Lign_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_lign(r,t),gen_chp_lign(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Oil_chp",r,t)$gen_new("sum_chp",r,t)  = min(gen_oil(r,t),gen_chp_oil(r,t)   * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Coal_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_coal(r,t),gen_chp_coal(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;

gen_new("sum_chp",r,t) = sum(genset_chp(genset), gen_new(genset,r,t)) ;

gen_new("ngas_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_ngas(r,t),gen_chp_ngas(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Biom_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_biom(r,t),gen_chp_biom(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Lign_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_lign(r,t),gen_chp_lign(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Oil_chp",r,t)$gen_new("sum_chp",r,t)  = min(gen_oil(r,t),gen_chp_oil(r,t)   * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Coal_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_coal(r,t),gen_chp_coal(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;

gen_new("sum_chp",r,t) = sum(genset_chp(genset), gen_new(genset,r,t)) ;

gen_new("ngas_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_ngas(r,t),gen_chp_ngas(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Biom_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_biom(r,t),gen_chp_biom(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Lign_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_lign(r,t),gen_chp_lign(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Oil_chp",r,t)$gen_new("sum_chp",r,t)  = min(gen_oil(r,t),gen_chp_oil(r,t)   * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;
gen_new("Coal_chp",r,t)$gen_new("sum_chp",r,t) = min(gen_coal(r,t),gen_chp_coal(r,t) * chpinfo2021(r,"TWh") / gen_new("sum_chp",r,t)) ;

gen_new("ngas_nochp",r,t) = gen_new("ngas",r,t) - gen_new("ngas_chp",r,t) ;
gen_new("Biom_nochp",r,t) = gen_new("biom",r,t) - gen_new("biom_chp",r,t) ;
gen_new("Lign_nochp",r,t) = gen_new("lign",r,t) - gen_new("lign_chp",r,t) ;
gen_new("Oil_nochp",r,t) = gen_new("oil",r,t) - gen_new("oil_chp",r,t) ;
gen_new("Coal_nochp",r,t) = gen_new("Coal",r,t) - gen_new("Coal_chp",r,t) ;

gen_new("ngas_tog",r,t) = gen_new("ngas_nochp",r,t) + gen_new("ngas_chp",r,t) ;
gen_new("Biom_tog",r,t) = gen_new("biom_nochp",r,t) + gen_new("biom_chp",r,t) ;
gen_new("Lign_tog",r,t) = gen_new("lign_nochp",r,t) + gen_new("lign_chp",r,t) ;
gen_new("Oil_tog",r,t) = gen_new("oil_nochp",r,t) + gen_new("oil_chp",r,t) ;
gen_new("Coal_tog",r,t) = gen_new("coal_nochp",r,t) + gen_new("coal_chp",r,t) ;

gen_new("sum",r,t) = sum(nor(genset), gen_new(genset,r,t)) ;
gen_new("sum_dif",r,t) = sum(genset_chp(genset), gen_new(genset,r,t)) + sum(genset_nochp(genset), gen_new(genset,r,t)) + sum(res(genset), gen_new(genset,r,t)) ;
gen_new("sum_norchp",r,t) = sum(norchp(genset), gen_new(genset,r,t)) ;
gen_new("sum_chp_nochp",r,t) = sum(genset_chp(genset), gen_new(genset,r,t)) + sum(genset_nochp(genset), gen_new(genset,r,t)) ;
gen_new("sum_chp",r,t) = sum(genset_chp(genset), gen_new(genset,r,t)) ;
gen_new("sum_nochp",r,t) = sum(genset_nochp(genset), gen_new(genset,r,t)) ;

gen_new_all(genset,t) = sum(r, gen_new(genset,r,t)) ;

gen_nucl(r,t) = gen_new("nucl",r,t) ;
gen_sola(r,t) = gen_new("sola",r,t) ;
gen_wind(r,t) = gen_new("wind",r,t) ;
gen_hydr(r,t) = gen_new("hydr",r,t) ;
gen_pump(r,t) = gen_new("pump",r,t) ;
gen_resv(r,t) = gen_new("resv",r,t) ;
* CHP/NOCHP
gen_ngas(r,t) = gen_new("ngas",r,t) ;
gen_biom(r,t) = gen_new("biom",r,t) ;
gen_lign(r,t) = gen_new("lign",r,t) ;
gen_oil(r,t) = gen_new("oil",r,t) ;
gen_coal(r,t) = gen_new("coal",r,t) ;
* CHP
gen_chp_ngas(r,t) = gen_new("ngas_chp",r,t) ;
gen_chp_biom(r,t) = gen_new("biom_chp",r,t) ;
gen_chp_lign(r,t) = gen_new("lign_chp",r,t) ;
gen_chp_oil(r,t) = gen_new("oil_chp",r,t) ;
gen_chp_coal(r,t) = gen_new("coal_chp",r,t) ;
* NOCHP
gen_nochp_ngas(r,t) = gen_new("ngas_nochp",r,t) ;
gen_nochp_biom(r,t) = gen_new("biom_nochp",r,t) ;
gen_nochp_lign(r,t) = gen_new("lign_nochp",r,t) ;
gen_nochp_oil(r,t) = gen_new("oil_nochp",r,t) ;
gen_nochp_coal(r,t) = gen_new("coal_nochp",r,t) ;

$if      set loadnormal     gen_nochp_ngas(r,"2023") = max(gen_new("ngas_nochp",r,"2022"),gen_new("ngas_nochp",r,"2023")) ;
$if      set loadnormal     gen_nochp_biom(r,"2023") = max(gen_new("biom_nochp",r,"2022"),gen_new("biom_nochp",r,"2023")) ;
$if      set loadnormal     gen_nochp_lign(r,"2023") = max(gen_new("lign_nochp",r,"2022"),gen_new("lign_nochp",r,"2023")) ;
$if      set loadnormal     gen_nochp_oil(r,"2023") = max(gen_new("oil_nochp",r,"2022"),gen_new("oil_nochp",r,"2023")) ;
$if      set loadnormal     gen_nochp_coal(r,"2023") = max(gen_new("coal_nochp",r,"2022"),gen_new("coal_nochp",r,"2023")) ;

* * Calculation of 2024 to 2050 generation
* Other technologies
gen_nucl(r,t)$(t.val ge 2024) = gen_nucl(r,"2023") ;
gen_nucl("Germany",t)$(t.val ge 2024) = 0 ;
gen_nucl(r,t)$(t.val ge 2024 and sameas(r,"France")) = 393.134 ;
gen_hydr(r,t)$(t.val ge 2024) = gen_hydr(r,"2020")/2 + gen_hydr(r,"2021")/2 ;
gen_resv(r,t)$(t.val ge 2024) = gen_resv(r,"2020")/2 + gen_resv(r,"2021")/2 ;
gen_pump(r,t)$(t.val ge 2024) = gen_pump(r,"2020")/2 + gen_pump(r,"2021")/2 ;
gen_wind(r,t)$(t.val ge 2024) = gen_wind(r,"2023") ;
gen_sola(r,t)$(t.val ge 2024) = gen_sola(r,"2023") ;
* CHP/NOCHP
gen_coal(r,t)$(t.val ge 2024) = gen_coal(r,"2023") ;
gen_lign(r,t)$(t.val ge 2024) = gen_lign(r,"2023") ;
gen_biom(r,t)$(t.val ge 2024) = gen_biom(r,"2023") ;
gen_ngas(r,t)$(t.val ge 2024) = gen_ngas(r,"2023") ;
gen_oil(r,t)$(t.val ge 2024) = gen_oil(r,"2023") ;
* CHP
gen_chp_coal(r,t)$(t.val ge 2024) = gen_chp_coal(r,"2023") ;
gen_chp_lign(r,t)$(t.val ge 2024) = gen_chp_lign(r,"2023") ;
gen_chp_biom(r,t)$(t.val ge 2024) = gen_chp_biom(r,"2023") ;
gen_chp_ngas(r,t)$(t.val ge 2024) = gen_chp_ngas(r,"2023") ;
gen_chp_oil(r,t)$(t.val ge 2024) = gen_chp_oil(r,"2023") ;
* NOCHP
gen_nochp_coal(r,t)$(t.val ge 2024) = gen_nochp_coal(r,"2023") ;
gen_nochp_lign(r,t)$(t.val ge 2024) = gen_nochp_lign(r,"2023") ;
gen_nochp_biom(r,t)$(t.val ge 2024) = gen_nochp_biom(r,"2023") ;
gen_nochp_ngas(r,t)$(t.val ge 2024) = gen_nochp_ngas(r,"2023") ;
gen_nochp_oil(r,t)$(t.val ge 2024) = gen_nochp_oil(r,"2023") ;

* Correct for hydro and nuclear crises years
$if      set frnucnormal gen_nucl(r,t)$(t.val ge 2022 and t.val le 2023 and sameas(r,"France")) = 393.134 ;
$if      set frnucperman gen_nucl(r,t)$(t.val ge 2024                   and sameas(r,"France")) = gen_nucl(r,"2023") ;
$if      set hydronormal gen_hydr(r,t)$(t.val ge 2022 and t.val le 2023                       ) = gen_hydr(r,"2020")/2 + gen_hydr(r,"2021")/2 ;
$if      set hydroperman gen_hydr(r,t)$(t.val ge 2024                                         ) = gen_hydr(r,"2023") ;
$if      set hydronormal gen_resv(r,t)$(t.val ge 2022 and t.val le 2023                       ) = gen_resv(r,"2020")/2 + gen_resv(r,"2021")/2 ;
$if      set hydroperman gen_resv(r,t)$(t.val ge 2024                                         ) = gen_resv(r,"2023") ;
$if      set hydronormal gen_pump(r,t)$(t.val ge 2022 and t.val le 2023                       ) = gen_pump(r,"2020")/2 + gen_pump(r,"2021")/2 ;
$if      set hydroperman gen_pump(r,t)$(t.val ge 2024                                         ) = gen_pump(r,"2023") ;

* Correct for German nuclear exit

$if      set gernucnormal    capt("Nuclear",oldv(v),"Germany","2023") = 0 ;
$if      set gernucnormal    gen_nucl(r,t)$(t.val ge 2023 and t.val le 2029 and sameas(r,"Germany")) = 0 ;
$if      set extension       gen_nucl(r,t)$(t.val ge 2023 and t.val le 2029 and sameas(r,"Germany")) = round(gen_nucl(r,"2020")/4 + gen_nucl(r,"2021")/4,4) ;
$if      set strext          gen_nucl(r,t)$(t.val ge 2023 and t.val le 2023 and sameas(r,"Germany")) = round(gen_nucl(r,"2023") + (gen_nucl(r,"2020")/4 + gen_nucl(r,"2021")/4)*3/12,4) ;
$if      set strext          gen_nucl(r,t)$(t.val ge 2024 and t.val le 2030 and sameas(r,"Germany")) = round(gen_nucl(r,"2020")/4 + gen_nucl(r,"2021")/4,4) ;

cap_nucl(r,t) = sum(ivrt(nuc(i),v,r,t), capt(i,v,r,t)) ;
cap_sola(r,t) = sum(ivrt(sol(i),v,r,t), capt(i,v,r,t)) ;
cap_wind(r,t) = sum(ivrt(wind(i),v,r,t), capt(i,v,r,t)) ;
cap_hydr(r,t) = sum(ivrt(hyd(i),v,r,t), capt(i,v,r,t)) ;
* CHP/NOCHP
cap_ngas(r,t) = sum(ivrt(gas(i),v,r,t), capt(i,v,r,t)) ;
cap_biom(r,t) = sum(ivrt(bio(i),v,r,t), capt(i,v,r,t)) ;
cap_lign(r,t) = sum(ivrt(lig(i),v,r,t), capt(i,v,r,t)) ;
cap_oil(r,t)  = sum(ivrt(oil(i),v,r,t), capt(i,v,r,t)) ;
cap_coal(r,t) = sum(ivrt(coa(i),v,r,t), capt(i,v,r,t)) ;
* CHP
cap_chp_ngas(r,t) = sum(ivrt_chp(gas(i),v,r,t), capt(i,v,r,t)) ;
cap_chp_biom(r,t) = sum(ivrt_chp(bio(i),v,r,t), capt(i,v,r,t)) ;
cap_chp_lign(r,t) = sum(ivrt_chp(lig(i),v,r,t), capt(i,v,r,t)) ;
cap_chp_oil(r,t)  = sum(ivrt_chp(oil(i),v,r,t), capt(i,v,r,t)) ;
cap_chp_coal(r,t) = sum(ivrt_chp(coa(i),v,r,t), capt(i,v,r,t)) ;
* NOCHP
cap_nochp_ngas(r,t) = sum(ivrt_nochp(gas(i),v,r,t), capt(i,v,r,t)) ;
cap_nochp_biom(r,t) = sum(ivrt_nochp(bio(i),v,r,t), capt(i,v,r,t)) ;
cap_nochp_lign(r,t) = sum(ivrt_nochp(lig(i),v,r,t), capt(i,v,r,t)) ;
cap_nochp_oil(r,t)  = sum(ivrt_nochp(oil(i),v,r,t), capt(i,v,r,t)) ;
cap_nochp_coal(r,t) = sum(ivrt_nochp(coa(i),v,r,t), capt(i,v,r,t)) ;

parameter
capt_add(i,v,r,t)
;

capt_add(ivrt(nuc(i),"2020",r,tcri(t)))$(cap_nucl(r,t) = 0 and gen_nucl(r,t) > 0) = gen_nucl(r,t) * 1e+3 / 8760 ;
capt_add(ivrt(sol(i),"2020",r,tcri(t)))$(cap_sola(r,t) = 0 and gen_sola(r,t) > 0) = gen_sola(r,t) * 1e+3 / 8760 ;
capt_add(ivrt(wind(i),"2020",r,tcri(t)))$(cap_wind(r,t) = 0 and gen_wind(r,t) > 0) = gen_wind(r,t) * 1e+3 / 8760 ;
capt_add(ivrt(hyd(i),"2020",r,tcri(t)))$(cap_hydr(r,t) = 0 and gen_hydr(r,t) > 0) = gen_hydr(r,t) * 1e+3 / 8760 ;

capt_add(ivrt(gas(i),"2020",r,tcri(t)))$(cap_ngas(r,t) = 0 and gen_ngas(r,t) > 0) = gen_ngas(r,t) * 1e+3 / 8760 ;
capt_add(ivrt(bio(i),"2020",r,tcri(t)))$(cap_biom(r,t) = 0 and gen_biom(r,t) > 0) = gen_biom(r,t) * 1e+3 / 8760 ;
capt_add(ivrt(lig(i),"2020",r,tcri(t)))$(cap_lign(r,t) = 0 and gen_lign(r,t) > 0) = gen_lign(r,t) * 1e+3 / 8760 ;
capt_add(ivrt(oil(i),"2020",r,tcri(t)))$(cap_oil(r,t) = 0 and gen_oil(r,t) > 0) = gen_oil(r,t) * 1e+3 / 8760 ;
capt_add(ivrt(coa(i),"2020",r,tcri(t)))$(cap_coal(r,t) = 0 and gen_coal(r,t) > 0) = gen_coal(r,t) * 1e+3 / 8760 ;

capt_add(ivrt_chp(gas(i),"2020",r,tcri(t)))$(cap_ngas(r,t) = 0 and gen_chp_ngas(r,t) > 0) = gen_chp_ngas(r,t) * 1e+3 / 8760 ;
capt_add(ivrt_chp(bio(i),"2020",r,tcri(t)))$(cap_biom(r,t) = 0 and gen_chp_biom(r,t) > 0) = gen_chp_biom(r,t) * 1e+3 / 8760 ;
capt_add(ivrt_chp(lig(i),"2020",r,tcri(t)))$(cap_lign(r,t) = 0 and gen_chp_lign(r,t) > 0) = gen_chp_lign(r,t) * 1e+3 / 8760 ;
capt_add(ivrt_chp(oil(i),"2020",r,tcri(t)))$(cap_oil(r,t) = 0 and gen_chp_oil(r,t) > 0) = gen_chp_oil(r,t) * 1e+3 / 8760 ;
capt_add(ivrt_chp(coa(i),"2020",r,tcri(t)))$(cap_coal(r,t) = 0 and gen_chp_coal(r,t) > 0) = gen_chp_coal(r,t) * 1e+3 / 8760 ;

capt_add(ivrt_nochp(gas(i),"2020",r,tcri(t)))$(cap_ngas(r,t) = 0 and gen_nochp_ngas(r,t) > 0) = gen_nochp_ngas(r,t) * 1e+3 / 8760 ;
capt_add(ivrt_nochp(bio(i),"2020",r,tcri(t)))$(cap_biom(r,t) = 0 and gen_nochp_biom(r,t) > 0) = gen_nochp_biom(r,t) * 1e+3 / 8760 ;
capt_add(ivrt_nochp(lig(i),"2020",r,tcri(t)))$(cap_lign(r,t) = 0 and gen_nochp_lign(r,t) > 0) = gen_nochp_lign(r,t) * 1e+3 / 8760 ;
capt_add(ivrt_nochp(oil(i),"2020",r,tcri(t)))$(cap_oil(r,t) = 0 and gen_nochp_oil(r,t) > 0) = gen_nochp_oil(r,t) * 1e+3 / 8760 ;
capt_add(ivrt_nochp(coa(i),"2020",r,tcri(t)))$(cap_coal(r,t) = 0 and gen_nochp_coal(r,t) > 0) = gen_nochp_coal(r,t) * 1e+3 / 8760 ;