set
set_profits /Gen,Gen-nchp,Gen-chp,Gen-oldv,Gen-nchp-oldv,Gen-newv,Gen-irnw,Gen-Wind,Gen-Wind-oldv,Gen-Wind-newv,Gen-WiOn,Gen-WiOf,Gen-Sola,Gen-Sola-oldv,Gen-Sola-newv,Gen-Nucl,
             Gen-Nucl-oldv,Gen-Nucl-newv,Gen-Coal,Gen-Lign,Gen-Hydr,Gen-NGas,Gen-GCCS,Gen-Bioe,Gen-BCCS,Sto,Sto-Pump,Sto-Batt,Sto-Hydr,Tra,Tot,Tot-nchp/
;

Parameter
revenue_ivrt(i,v,r,t)
revenue_jvrt(j,v,r,t)
cost_ivrt(i,v,r,t)
cost_jvrt(j,v,r,t)
profit_ivrt(i,v,r,t)
profit_jvrt(j,v,r,t)
revenue_ivr(i,v,r)
revenue_jvr(j,v,r)
revenue_iv(i,v)
revenue_jv(j,v)
cost_ivr(i,v,r)
cost_jvr(j,v,r)
cost_iv(i,v)
cost_jv(j,v)
profit_ivr(i,v,r)
profit_jvr(j,v,r)
profit_jv(j,v)
profit_iv(i,v)
Revenue_ir(i,r)
Revenue_jr(j,r)  
Cost_ir(i,r)         
Cost_jr(j,r)       
Profit_ir(i,r)     
Profit_jr(j,r)

ProfitGen_ivr(i,v,r)
ProfitCap_ivr(i,v,r)
ProfitCap_jvr(j,v,r) 
ProfitCapSha_ivr(i,v,r)
ProfitCapSha_jvr(j,v,r)

ProfitGen_iv(i,v)
ProfitCap_iv(i,v)
ProfitCap_jv(j,v)
ProfitCapSha_iv(i,v)
ProfitCapSha_jv(j,v)
Revenue_rpt(t,r,set_profits)
Cost_rpt(t,r,set_profits)
Profits_rpt(t,r,set_profits)
;

Revenue_ivrt(i,v,r,t) = sum(s, hours(s) * price(s,r,t) * XL(s,i,v,r,t)) * 1e-6 ;
Revenue_jvrt(j,v,r,t) = sum(s, hours(s) * price(s,r,t) * GDL(s,j,v,r,t) * dchrgpen(j,v,r)) * 1e-6 ;

Cost_ivrt(i,v,r,t)    = sum(s, hours(s) * discostco2(i,v,r,t) * XL(s,i,v,r,t)) * 1e-6 + XC.L(i,v,r,t) *  fomcost(i,v,r) * 1e-3
                      + sum(inv, share(inv,i,r) * sum(tv(tt,v), IX.L(i,r,tt)) * (capcost(i,v,r) + deccost(i,v,r)) * zeta(inv,i,v,r)) * 1e-3 ;
Cost_jvrt(j,v,r,t)    = sum(s, hours(s) * price(s,r,t) * GL(s,j,v,r,t)) * 1e-6 + GCL(j,v,r,t) * gfomcost(j,v,r) * 1e-3
                      + sum(inv, gshare(inv,j,r) * sum(tv(tt,v), IGL(j,r,tt)) * gcapcost(j,v,r) * gzeta(inv,j,v,r)) * 1e-3 ;

Profit_ivrt(i,v,r,t)  = Revenue_ivrt(i,v,r,t) - Cost_ivrt(i,v,r,t) ;
Profit_jvrt(j,v,r,t)  = Revenue_jvrt(j,v,r,t) - Cost_jvrt(j,v,r,t) ;

Revenue_ivr(i,v,r)    = sum(t$(t.val ge v.val), sum(s, hours(s) * price(s,r,t) * XL(s,i,v,r,t)) * 1e-6) ;
Revenue_jvr(j,v,r)    = sum(t$(t.val ge v.val), sum(s, hours(s) * price(s,r,t) * GDL(s,j,v,r,t) * dchrgpen(j,v,r)) * 1e-6) ;
Revenue_iv(i,v)       = sum(r, Revenue_ivr(i,v,r)) ;
Revenue_jv(j,v)       = sum(r, Revenue_jvr(j,v,r)) ;

Cost_ivr(i,v,r)       = sum(t$(t.val ge v.val), sum(s, hours(s) * discostco2(i,v,r,t) * XL(s,i,v,r,t)) * 1e-6 + XC.L(i,v,r,t) *  fomcost(i,v,r) * 1e-3
                      + sum(inv, share(inv,i,r) * sum(tv(tt,v), IX.L(i,r,tt)) * (capcost(i,v,r) + deccost(i,v,r)) * zeta(inv,i,v,r)) * 1e-3) ;
Cost_jvr(j,v,r)       = sum(t$(t.val ge v.val), sum(s, hours(s) * price(s,r,t) * GL(s,j,v,r,t)) * 1e-6 + GCL(j,v,r,t) * gfomcost(j,v,r) * 1e-3
                      + sum(inv, gshare(inv,j,r) * sum(tv(tt,v), IGL(j,r,tt)) * gcapcost(j,v,r) * gzeta(inv,j,v,r)) * 1e-3) ;
Cost_iv(i,v)          = sum(r, Cost_ivr(i,v,r)) ;
Cost_jv(j,v)          = sum(r, Cost_jvr(j,v,r)) ;
                     
Profit_ivr(i,v,r)     = Revenue_ivr(i,v,r) - Cost_ivr(i,v,r) ;
Profit_jvr(j,v,r)     = Revenue_jvr(j,v,r) - Cost_jvr(j,v,r) ;
Profit_iv(i,v)        = sum(r, Profit_ivr(i,v,r));
Profit_jv(j,v)        = sum(r, Profit_jvr(j,v,r)) ;


ProfitGen_ivr(i,v,r)$(sum(t$(t.val ge v.val), XTWHL(i,v,r,t))) = Profit_ivr(i,v,r) * 1e+3 / sum(t$(t.val ge v.val), XTWHL(i,v,r,t)) ;
ProfitCap_ivr(i,v,r)$(sum(tv(t,v), IX.L(i,r,t))) = Profit_ivr(i,v,r) * 1e+3 / sum(tv(t,v), IX.L(i,r,t)) ;  
ProfitCap_jvr(j,v,r)$(sum(tv(t,v), IGL(j,r,t))) = Profit_jvr(j,v,r) * 1e+3 / sum(tv(t,v), IGL(j,r,t)) ;  
ProfitCapSha_ivr(i,v,r)$(capcost(i,v,r) > 0) = ProfitCap_ivr(i,v,r) / capcost(i,v,r) ;
ProfitCapSha_jvr(j,v,r)$(gcapcost(j,v,r) > 0) = ProfitCap_jvr(j,v,r) / gcapcost(j,v,r) ;

ProfitGen_iv(i,v)$(sum(r, sum(t$(t.val ge v.val), XTWHL(i,v,r,t)))) = sum(r, Profit_ivr(i,v,r)) * 1e+3 / sum(r, sum(t$(t.val ge v.val), XTWHL(i,v,r,t))) ;
ProfitCap_iv(i,v)$(sum(r, sum(tv(t,v), IX.L(i,r,t)))) = sum(r, Profit_ivr(i,v,r)) * 1e+3 / sum(r, sum(tv(t,v), IX.L(i,r,t))) ;  
ProfitCap_jv(j,v)$(sum(r, sum(tv(t,v), IGL(j,r,t)))) = sum(r, Profit_jvr(j,v,r)) * 1e+3 / sum(r, sum(tv(t,v), IGL(j,r,t))) ;
ProfitCapSha_iv(i,v)$(smax(r, capcost(i,v,r)) > 0) = ProfitCap_iv(i,v) / smax(r, capcost(i,v,r)) ;
ProfitCapSha_jv(j,v)$(smax(r, gcapcost(j,v,r)) > 0) = ProfitCap_jv(j,v) / smax(r, gcapcost(j,v,r)) ;
 
Revenue_ir(i,r)       = sum(v, Revenue_ivr(i,v,r)) ;
Revenue_jr(j,r)       = sum(v, Revenue_jvr(j,v,r)) ;
Cost_ir(i,r)          = sum(v, Cost_ivr(i,v,r)) ;
Cost_jr(j,r)          = sum(v, Cost_jvr(j,v,r)) ;
Profit_ir(i,r)        = sum(v, Profit_ivr(i,v,r)) ;
Profit_jr(j,r)        = sum(v, Profit_jvr(j,v,r)) ;
* * Revenues
* Generation

Revenue_rpt(t,r,"Gen") = sum(s, hours(s) * price(s,r,t) * sum(ivrt(i,v,r,t), XL(s,i,v,r,t))) * 1e-6 ;
Revenue_rpt(t,r,"Gen-nchp") = sum(s, hours(s) * price(s,r,t) * sum(ivrt(i,v,r,t)$(not chp(i)), XL(s,i,v,r,t))) * 1e-6 ;
Revenue_rpt(t,r,"Gen-chp") = sum(s, hours(s) * price(s,r,t) * sum(ivrt(chp(i),v,r,t), XL(s,i,v,r,t))) * 1e-6 ;
Revenue_rpt(t,r,"Gen-oldv") = sum(s, hours(s) * price(s,r,t) * sum(ivrt(i,oldv(v),r,t), XL(s,i,v,r,t))) * 1e-6 ;
Revenue_rpt(t,r,"Gen-nchp-oldv") = sum(s, hours(s) * price(s,r,t) * sum(ivrt(i,oldv(v),r,t)$(not chp(i)), XL(s,i,v,r,t))) * 1e-6 ;
Revenue_rpt(t,r,"Gen-newv") = sum(s, hours(s) * price(s,r,t) * sum(ivrt(i,newv(v),r,t), XL(s,i,v,r,t))) * 1e-6 ;
Revenue_rpt(t,r,"Gen-irnw") = sum(s, hours(s) * price(s,r,t) * sum(ivrt(irnw(i),oldv(v),r,t), XL(s,i,v,r,t))) * 1e-6 ;
Revenue_rpt(t,r,"Gen-Wind") = sum(s, hours(s) * price(s,r,t) * sum(ivrt(wind(i),v,r,t), XL(s,i,v,r,t))) * 1e-6 ;
Revenue_rpt(t,r,"Gen-Wind-oldv") = sum(s, hours(s) * price(s,r,t) * sum(ivrt(wind(i),oldv(v),r,t), XL(s,i,v,r,t))) * 1e-6 ;
Revenue_rpt(t,r,"Gen-Wind-newv") = sum(s, hours(s) * price(s,r,t) * sum(ivrt(wind(i),newv(v),r,t), XL(s,i,v,r,t))) * 1e-6 ;
Revenue_rpt(t,r,"Gen-WiOn") = sum(s, hours(s) * price(s,r,t) * sum(ivrt(windon(i),v,r,t), XL(s,i,v,r,t))) * 1e-6 ;
Revenue_rpt(t,r,"Gen-WiOf") = sum(s, hours(s) * price(s,r,t) * sum(ivrt(windoff(i),v,r,t), XL(s,i,v,r,t))) * 1e-6 ;
Revenue_rpt(t,r,"Gen-Sola") = sum(s, hours(s) * price(s,r,t) * sum(ivrt(sol(i),v,r,t), XL(s,i,v,r,t))) * 1e-6 ;
Revenue_rpt(t,r,"Gen-Sola-oldv") = sum(s, hours(s) * price(s,r,t) * sum(ivrt(sol(i),oldv(v),r,t), XL(s,i,v,r,t))) * 1e-6 ;
Revenue_rpt(t,r,"Gen-Sola-newv") = sum(s, hours(s) * price(s,r,t) * sum(ivrt(sol(i),newv(v),r,t), XL(s,i,v,r,t))) * 1e-6 ;
Revenue_rpt(t,r,"Gen-Nucl") = sum(s, hours(s) * price(s,r,t) * sum(ivrt(nuc(i),v,r,t), XL(s,i,v,r,t))) * 1e-6 ;
Revenue_rpt(t,r,"Gen-Nucl-oldv") = sum(s, hours(s) * price(s,r,t) * sum(ivrt(nuc(i),oldv(v),r,t), XL(s,i,v,r,t))) * 1e-6 ;
Revenue_rpt(t,r,"Gen-Nucl-newv") = sum(s, hours(s) * price(s,r,t) * sum(ivrt(nuc(i),newv(v),r,t), XL(s,i,v,r,t))) * 1e-6 ;
Revenue_rpt(t,r,"Gen-Coal") = sum(s, hours(s) * price(s,r,t) * sum(ivrt(coa(i),v,r,t)$(not ccs(i) and not chp(i)), XL(s,i,v,r,t))) * 1e-6 ;
Revenue_rpt(t,r,"Gen-Lign") = sum(s, hours(s) * price(s,r,t) * sum(ivrt(lig(i),v,r,t)$(not ccs(i) and not chp(i)), XL(s,i,v,r,t))) * 1e-6 ;
Revenue_rpt(t,r,"Gen-Hydr") = sum(s, hours(s) * price(s,r,t) * sum(ivrt(hyd(i),v,r,t), XL(s,i,v,r,t))) * 1e-6 ;
Revenue_rpt(t,r,"Gen-NGas") = sum(s, hours(s) * price(s,r,t) * sum(ivrt(gas(i),v,r,t)$(not ccs(i) and not chp(i)), XL(s,i,v,r,t))) * 1e-6 ;
Revenue_rpt(t,r,"Gen-GCCS") = sum(s, hours(s) * price(s,r,t) * sum(ivrt(gas(i),v,r,t)$(ccs(i)), XL(s,i,v,r,t))) * 1e-6 ;
Revenue_rpt(t,r,"Gen-Bioe") = sum(s, hours(s) * price(s,r,t) * sum(ivrt(bio(i),v,r,t)$(not ccs(i) and not chp(i)), XL(s,i,v,r,t))) * 1e-6 ;
Revenue_rpt(t,r,"Gen-BCCS") = sum(s, hours(s) * price(s,r,t) * sum(ivrt(bio(i),v,r,t)$(ccs(i)), XL(s,i,v,r,t))) * 1e-6 ;
* Storage
Revenue_rpt(t,r,"Sto") = sum(s, hours(s) * price(s,r,t) * sum(jvrt(j,v,r,t), GDL(s,j,v,r,t) * dchrgpen(j,v,r))) * 1e-6 ;
Revenue_rpt(t,r,"Sto-Pump") = sum(s, hours(s) * price(s,r,t) * sum(jvrt(j,v,r,t)$(sameas(j,"PumpStorage")), GDL(s,j,v,r,t) * dchrgpen(j,v,r))) * 1e-6 ;
Revenue_rpt(t,r,"Sto-Batt") = sum(s, hours(s) * price(s,r,t) * sum(jvrt(j,v,r,t)$(sameas(j,"Storage_ST")), GDL(s,j,v,r,t) * dchrgpen(j,v,r))) * 1e-6 ;
Revenue_rpt(t,r,"Sto-Hydr") = sum(s, hours(s) * price(s,r,t) * sum(jvrt(j,v,r,t)$(sameas(j,"Storage_LT")), GDL(s,j,v,r,t) * dchrgpen(j,v,r))) * 1e-6 ;
* Assuming rent sharing between exporting and importing country (losses are dedicated to exporting country)
Revenue_rpt(t,r,"Tra") = sum(s, hours(s) * sum((k,rr)$tmap(k,r,rr), (price(s,r,t) + price(s,rr,t))/2 * E.L(s,k,r,rr,t) / trnspen(k,r,rr))) * 1e-6 ;
* Si,
Revenue_rpt(t,r,"Tot")       = Revenue_rpt(t,r,"Gen") + Revenue_rpt(t,r,"Sto") + Revenue_rpt(t,r,"Tra") ;
Revenue_rpt(t,r,"Tot-nchp")  = Revenue_rpt(t,r,"Gen-nchp") + Revenue_rpt(t,r,"Sto") + Revenue_rpt(t,r,"Tra") ;
* * Cost

Cost_rpt(t,r,"Gen") = sum(s, hours(s) * sum(ivrt(i,v,r,t), discostco2(i,v,r,t) * XL(s,i,v,r,t))) * 1e-6 + sum(ivrt(i,v,r,t), XC.L(i,v,r,t) *  fomcost(i,v,r)) * 1e-3
                                    + sum(i, sum(inv, share(inv,i,r) * IX.L(i,r,t) * sum(tv(t,v)$ivrt(i,v,r,t), (capcost(i,v,r) + deccost(i,v,r)) * zeta(inv,i,v,r)))) * 1e-3 ;
Cost_rpt(t,r,"Sto") = sum(s, hours(s) * price(s,r,t) * sum(jvrt(j,v,r,t), GL(s,j,v,r,t))) * 1e-6 + sum(jvrt(j,v,r,t), GCL(j,v,r,t) *  gfomcost(j,v,r)) * 1e-3
                                 + sum(j, sum(inv, gshare(inv,j,r) * IGL(j,r,t) * sum(tv(t,v)$jvrt(j,v,r,t), (gcapcost(j,v,r)) * gzeta(inv,j,v,r)))) * 1e-3 ;
Cost_rpt(t,r,"Tra") = sum(s, hours(s) * sum((k,rr)$tmap(k,r,rr), (price(s,rr,t) + price(s,r,t))/2 * E.L(s,k,rr,r,t))) * 1e-6 + sum((k,rr)$tmap(k,r,rr), TC.L(k,r,rr,t) * tfomcost(k,r,rr)) * 1e-3
                                 + sum((rr,k)$tmap(k,r,rr), sum(inv, tshare(inv,k,r) * IT.L(k,r,rr,t) * sum(tv(t,v)$tvrt(k,v,r,t), tcapcost(k,r,rr) * tzeta(inv,k,v,r)))) * 1e-3 ;
Cost_rpt(t,r,"Tot")     = Cost_rpt(t,r,"Gen") + Cost_rpt(t,r,"Sto") + Cost_rpt(t,r,"Tra") ;

* * Profits
Profits_rpt(t,r,set_profits) = Revenue_rpt(t,r,set_profits) - Cost_rpt(t,r,set_profits) ;