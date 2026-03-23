* * * Allocate not vintage-specific variable into vintage-specific variables

* * * Learning calibration
parameter
capirnw_q(quantiles,superirnw,r,t)
capirnw(superirnw,r,t)
capirnw_max(superirnw,r)
limirnw_q(quantiles,superirnw,r,t)
limirnw(superirnw,r,t)
limirnw_max(superirnw,r)
shairnw_q(quantiles,superirnw,r,t)
shairnw(superirnw,r,t)
shairnw_max(superirnw,r)
;

capirnw_q(quantiles,superirnw,r,t) = sum(superirnw_mapq(i,quantiles,superirnw), sum(v, XC.L(i,v,r,t))) ;
capirnw(superirnw,r,t) = sum(quantiles, capirnw_q(quantiles,superirnw,r,t)) ;
capirnw_max(superirnw,r) = smax(t, capirnw(superirnw,r,t)) ;

limirnw_q(quantiles,superirnw,r,t) = sum(superirnw_mapq(i,quantiles,superirnw), irnwlimUP_quantiles(i,r,quantiles)) ;
limirnw(superirnw,r,t) = sum(quantiles, limirnw_q(quantiles,superirnw,r,t)) ;
limirnw_max(superirnw,r) = smax(t, limirnw(superirnw,r,t)) ;

shairnw_q(quantiles,superirnw,r,t)$(limirnw_q(quantiles,superirnw,r,t) > 0) = capirnw_q(quantiles,superirnw,r,t) / limirnw_q(quantiles,superirnw,r,t) ;
shairnw(superirnw,r,t)$(limirnw(superirnw,r,t) > 0) = capirnw(superirnw,r,t) / limirnw(superirnw,r,t) ;
shairnw_max(superirnw,r)$(limirnw_max(superirnw,r) > 0) = smax(t, shairnw(superirnw,r,t)) ;



Parameter
flhlearni(i,v,r,t)
flhpoteni(i,v,r)
flhreport(*,i,v,r,t)
irnwflh_check(i,v,r)
;

irnwflh_check(i,newv(v),r) = sum(s, hours(s) * vrsc(s,i,v,r)) ;
irnwflh_check(i,oldv(v),r) = sum(tv(t,v), sum(s, hours(s) * vrsc_exi(s,i,r,t))) ;


flhlearni(ivrt(i_flh(i),v,r,t))$(XC.L(i,v,r,t) > 0) = sum(s, hours(s) * XL(s,i,v,r,t)) / XC.L(i,v,r,t) ;

flhpoteni(i_flh(i),v,r)$(capt(i,v,r,"2023") > 0 or sum(tv(t,v), IX.L(i,r,t)) > 0 and v.val le 2022) = sum(s, hours(s) * vrsc(s,i,v,r)) ;     
$if      set flh     flhpoteni(i_flh(i),v,r)$(capt(i,v,r,"2023") > 0 or sum(tv(t,v), IX.L(i,r,t)) > 0 and v.val ge 2023) = sum(s, hours(s) * vrsc_nor(s,i,v,r)) * sum(flhls, flhindexSLOPE(i,r,flhls) * sum(tv(tt,v), RHOFLH.L(i,r,tt-1,flhls))) ;     
$if      set flheur  flhpoteni(i_flh(i),v,r)$(capt(i,v,r,"2023") > 0 or sum(tv(t,v), IX.L(i,r,t)) > 0 and v.val ge 2023) = sum(s, hours(s) * vrsceur_nor(s,i,v,r)) * sum(flhls, flheurindexSLOPE(i,flhls) * sum(tv(tt,v), RHOFLHEUR.L(i,tt-1,flhls))) * flheurindex_reg(i,r,v) ;

flhreport("Max",ivrt(i_flh(i),v,r,t))$(capt(i,v,r,"2023") > 0 or sum(tv(t,v), IX.L(i,r,t)) > 0) = irnwflh_check(i,v,r) ;
flhreport("Real",ivrt(i_flh(i),v,r,t))$(capt(i,v,r,"2023") > 0 or sum(tv(t,v), IX.L(i,r,t)) > 0) = flhlearni(i,v,r,t) ;
flhreport("Pote",ivrt(i_flh(i),v,r,t))$(capt(i,v,r,"2023") > 0 or sum(tv(t,v), IX.L(i,r,t)) > 0) = flhpoteni(i,v,r) ;
flhreport("Diff-Real",ivrt(i_flh(i),v,r,t))$(flhlearni(i,v,r,t) > 0 and flhpoteni(i,v,r) > 0) = flhpoteni(i,v,r) - flhlearni(i,v,r,t) ;    
flhreport("Diff-Pote",ivrt(i_flh(i),v,r,t))$(irnwflh_check(i,v,r) > 0 and flhpoteni(i,v,r) > 0) = irnwflh_check(i,v,r) - flhpoteni(i,v,r) ;

Parameter
investflh_irt(i,r,t)
investflh_ir(i,r)      
investflh_it(i,t)      
investflh_rt(r,t)   
investflh_i(i)         
investflh_r(r)         
investflh_t(t)      
investflh               
investflheur_it(i,t)  
investflheur_i(i)      
investflheur_t(t)      
investflheur  
;

$if      set flh     investflh_irt(ir_flh(i,r),toptimize(t))    = IFLH.L(i,r,t) ;
$if not  set flh     investflh_irt(ir_flh(i,r),toptimize(t))    = 0 ;
investflh_ir(i,r)       = sum(toptimize(t), nyrs(t) * investflh_irt(i,r,t)) ;
investflh_it(i,t)       = sum(r, investflh_irt(i,r,t)) ;
investflh_rt(r,t)       = sum(i, investflh_irt(i,r,t)) ;
investflh_i(i)          = sum((r,toptimize(t)), nyrs(t) * investflh_irt(i,r,t)) ;
investflh_r(r)          = sum((i,toptimize(t)), investflh_irt(i,r,t)) ;
investflh_t(t)          = sum((i,r), investflh_irt(i,r,t)) ;
investflh               = sum((i,r,toptimize(t)), nyrs(t) * investflh_irt(i,r,t)) ;

$if      set flheur  investflheur_it(i_flh(i),toptimize(t))    = IFLHEUR.L(i,t) ;
$if not  set flheur  investflheur_it(i_flh(i),toptimize(t))    = 0 ;
investflheur_i(i)       = sum((toptimize(t)), nyrs(t) * investflheur_it(i,t)) ;
investflheur_t(t)       = sum((i), investflheur_it(i,t)) ;
investflheur            = sum((i,toptimize(t)), nyrs(t) * investflheur_it(i,t)) ;

Parameter
stock(t,*)
accspendings(t,*)
flheur_stock_rpt(*,i,t)
flheur_ls_rpt(*,i,*)
flheur_check_rpt(*,i,r,v)
flheur_cost_rpt(*,i,v)
;

$if      set flheur  stock(t,"WindOn") = FLHTRYEUR.L("WindOn_q90",t) + eps ;
$if not  set flheur  stock(t,"WindOn") = eps ;
$if      set flheur  stock(t,"WindOff") = FLHTRYEUR.L("WindOff_q90",t) + eps ;
$if not  set flheur  stock(t,"WindOff") = eps ;
$if      set flheur  stock(t,"WindOn-Start")  = flheurstockLO("WindOn_q90","ls-1") ;
$if      set flheur  stock(t,"WindOn-ls-1")  = flheurstockUP("WindOn_q90","ls-1") ;
$if      set flheur  stock(t,"WindOn-ls-2")  = flheurstockUP("WindOn_q90","ls-2") ;
$if      set flheur  stock(t,"WindOn-ls-3")  = flheurstockUP("WindOn_q90","ls-3") ;
$if      set flheur  stock(t,"WindOn-ls-4")  = flheurstockUP("WindOn_q90","ls-4") ;
$if      set flheur  stock(t,"WindOn-ls-5")  = flheurstockUP("WindOn_q90","ls-5") ;
$if      set flheur  stock(t,"WindOn-ls-6")  = flheurstockUP("WindOn_q90","ls-6") ;
$if      set flheur  stock(t,"WindOn-End")  = flheurstockUP("WindOn_q90","ls-7") ;

$if      set flheur  stock(t,"WindOff-Start")  = flheurstockLO("WindOff_q90","ls-1") ;
$if      set flheur  stock(t,"WindOff-ls-1")  = flheurstockUP("WindOff_q90","ls-1") ;
$if      set flheur  stock(t,"WindOff-ls-2")  = flheurstockUP("WindOff_q90","ls-2") ;
$if      set flheur  stock(t,"WindOff-ls-3")  = flheurstockUP("WindOff_q90","ls-3") ;
$if      set flheur  stock(t,"WindOff-ls-4")  = flheurstockUP("WindOff_q90","ls-4") ;
$if      set flheur  stock(t,"WindOff-ls-5")  = flheurstockUP("WindOff_q90","ls-5") ;
$if      set flheur  stock(t,"WindOff-ls-6")  = flheurstockUP("WindOff_q90","ls-6") ;
$if      set flheur  stock(t,"WindOff-End")  = flheurstockUP("WindOff_q90","ls-7") ;

$if      set flheur  accspendings(t,i_flh(i)) = sum(tt$(tt.val le t.val), IFLHEUR.L(i,tt) * nyrs(tt)) + eps ;
$if not  set flheur  accspendings(t,i_flh(i)) = eps ;

$if      set flheur  flheur_stock_rpt("stock",i_flh(i),t)                            = stock(t,i) ;
$if      set flheur  flheur_stock_rpt("accsp",i_flh(i),t)                            = accspendings(t,i) ;

$if      set flheur  flheur_ls_rpt("border",i_flh(i),"Low")                         = flheurstockLO(i,"ls-1") ;
$if      set flheur  flheur_ls_rpt("border",i_flh(i),"ls-1")                         = flheurstockUP(i,"ls-1") ;
$if      set flheur  flheur_ls_rpt("border",i_flh(i),"ls-2")                         = flheurstockUP(i,"ls-2") ;
$if      set flheur  flheur_ls_rpt("border",i_flh(i),"ls-3")                         = flheurstockUP(i,"ls-3") ;
$if      set flheur  flheur_ls_rpt("border",i_flh(i),"ls-4")                         = flheurstockUP(i,"ls-4") ;
$if      set flheur  flheur_ls_rpt("border",i_flh(i),"ls-5")                         = flheurstockUP(i,"ls-5") ;
$if      set flheur  flheur_ls_rpt("border",i_flh(i),"ls-6")                         = flheurstockUP(i,"ls-6") ;
$if      set flheur  flheur_ls_rpt("border",i_flh(i),"Up")                          = flheurstockUP(i,"ls-7") ;
$if      set flheur  flheur_ls_rpt("slope",i_flh(i),"ls-1")                          = flheurindexSLOPE(i,"ls-1") ;
$if      set flheur  flheur_ls_rpt("slope",i_flh(i),"ls-2")                          = flheurindexSLOPE(i,"ls-2") ;
$if      set flheur  flheur_ls_rpt("slope",i_flh(i),"ls-3")                          = flheurindexSLOPE(i,"ls-3") ;
$if      set flheur  flheur_ls_rpt("slope",i_flh(i),"ls-4")                          = flheurindexSLOPE(i,"ls-4") ;
$if      set flheur  flheur_ls_rpt("slope",i_flh(i),"ls-5")                          = flheurindexSLOPE(i,"ls-5") ;
$if      set flheur  flheur_ls_rpt("slope",i_flh(i),"ls-6")                          = flheurindexSLOPE(i,"ls-6") ;
$if      set flheur  flheur_ls_rpt("slope",i_flh(i),"Up")                          = flheurindexSLOPE(i,"ls-7") ;

$if      set flheur  flheur_check_rpt("flheur",i_flh(i),r,v)$(v.val ge 2020 and v.val le 2022)       = irnwflh_check(i,v,r) ;
$if      set flheur  flheur_check_rpt("flheur",i_flh(i),r,v)$(v.val ge 2023)       = sum(s,hours(s) * vrsceur_nor(s,i,v,r) * flheurindex_reg(i,r,v)) * sum(tv(t,v), sum(flhls, RHOFLHEUR.L(i,t-1,flhls) * flheurindexSLOPE(i,flhls))) ;
$if      set flheur  flheur_check_rpt("true",i_flh(i),r,v)$(v.val ge 2020)         = irnwflh_check(i,v,r) ;
$if      set flheur  flheur_check_rpt("diff",i_flh(i),r,v)$(v.val ge 2020)         = flheur_check_rpt("flheur",i,r,v) / irnwflh_check(i,v,r) - 1;
$if      set flheur  flheur_check_rpt("index_reg",i_flh(i),r,v)$(v.val ge 2020)    = flheurindex_reg(i,r,v) ;

$if      set flheur  flheur_cost_rpt("flh",i_flh(i),v)$(v.val ge 2020) = flheur(i,v) ;
$if      set flheur  flheur_cost_rpt("index",i_flh(i),v)$(v.val ge 2020) = flheurindex(i,v) ;
$if      set flheur  flheur_cost_rpt("invcost",i_flh(i),v)$(v.val ge 2020) = capcost(i,v,"Germany") ; 
$if      set flheur  flheur_cost_rpt("fomcost",i_flh(i),v)$(v.val ge 2020) = fomcost(i,v,"Germany") ;

parameter
irnwflheur_check(i,r,v)
;

irnwflheur_check(i,r,v)$(sameas(i,"OpenPV_q90") and v.val ge 2020) = irnwflh_check(i,v,r) ;
irnwflheur_check(i,r,v)$(sameas(i,"WindOn_q90") and v.val ge 2020) = irnwflh_check(i,v,r) ;
irnwflheur_check(i,r,v)$(sameas(i,"WindOff_q90") and v.val ge 2020) = irnwflh_check(i,v,r) + eps ;

$if set flhbenchmark execute_unload 'precal\data\researchflhdata_out.gdx', irnwflheur_check ;
$if set flhbenchmark execute 'gdxxrw.exe precal\data\researchflhdata_out.gdx    o=precal\data\researchflh_data.xlsx   par=irnwflheur_check rng=flhcheck!a1'

parameter
invsum_i(i,r,t)
invsum_j(j,r,t)
invsum_k(k,r,t)
invsum_i_total(i,r) 
invsum_j_total(j,r)
invsum_k_total(k,r) 
;

invsum_i(i,r,t) = sum(inv,  share(inv,i,r) * IX.L(i,r,t)    * sum(tv(t,v)$ivrt(i,v,r,t),  capcost(i,v,r)  *  zeta(inv,i,v,r))) / dfact(t) ;
invsum_j(j,r,t) = sum(inv, gshare(inv,j,r) * IGL(j,r,t)    * sum(tv(t,v)$jvrt(j,v,r,t), gcapcost(j,v,r)  * gzeta(inv,j,v,r))) / dfact(t) ;
invsum_k(k,r,t) = sum(rr$tmap(k,r,rr), sum(inv, tshare(inv,k,r) * IT.L(k,r,rr,t) * sum(tv(t,v)$tvrt(k,v,r,t), tcapcost(k,r,rr) * tzeta(inv,k,v,r)))) / dfact(t) ;

invsum_i_total(i,r) = sum(t, invsum_i(i,r,t)) ;
invsum_j_total(j,r) = sum(t, invsum_j(j,r,t)) ;
invsum_k_total(k,r) = sum(t, invsum_k(k,r,t)) ;

parameter
tech_weights_i(i,r,t)
tech_weights_j(j,r,t)
tech_weights_k(k,r,t)
tech_weights_i_total(i,r)
tech_weights_j_total(j,r) 
tech_weights_k_total(k,r)
irnw_weight_cap(r,i,v,t)
irnw_weight_gen(r,i,v,t)
;

alias(j,jj) ;
alias(k,kk) ;

tech_weights_i(i,r,t) = invsum_i(i,r,t) / (sum(ii, invsum_i(ii,r,t)) + sum(j, invsum_j(j,r,t)) + sum(k, invsum_k(k,r,t))) ;
tech_weights_j(j,r,t) = invsum_j(j,r,t) / (sum(i, invsum_i(i,r,t)) + sum(jj, invsum_j(jj,r,t)) + sum(k, invsum_k(k,r,t))) ;
tech_weights_k(k,r,t) = invsum_k(k,r,t) / (sum(i, invsum_i(i,r,t)) + sum(j, invsum_j(j,r,t)) + sum(kk, invsum_k(kk,r,t))) ;

tech_weights_i_total(i,r) = invsum_i_total(i,r) / (sum(ii, invsum_i_total(ii,r)) + sum(j, invsum_j_total(j,r)) + sum(k, invsum_k_total(k,r))) ;
tech_weights_j_total(j,r) = invsum_j_total(j,r) / (sum(i, invsum_i_total(i,r)) + sum(jj, invsum_j_total(jj,r)) + sum(k, invsum_k_total(k,r))) ;
tech_weights_k_total(k,r) = invsum_k_total(k,r) / (sum(i, invsum_i_total(i,r)) + sum(j, invsum_j_total(j,r)) + sum(kk, invsum_k_total(kk,r))) ;



$if set techweights execute_unload   'precal\data\tech_weights.gdx',             invsum_i, invsum_j, invsum_k, tech_weights_i, tech_weights_j, tech_weights_k,
$if set techweights                                            invsum_i_total, invsum_j_total, invsum_k_total, tech_weights_i_total, tech_weights_j_total, tech_weights_k_total ;

irnw_weight_cap(r,i,v,t) = XC.L(i,v,r,t) ;
irnw_weight_gen(r,i,v,t) = XTWHL(i,v,r,t) ;

$if set irnwweights execute_unload   'precal\choose_decision\irnw_weights.gdx', irnw_weight_cap, irnw_weight_gen ;ight_gen ;