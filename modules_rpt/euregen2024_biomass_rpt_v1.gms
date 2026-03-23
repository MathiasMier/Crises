Set
biomass_set /"Use elec (TWh)","Use elec* (TWh)","Use remaining (TWh)","Use total (TWh)","Extraction (TWh)","European imports (TWh)","European exports (TWh)","International imports (TWh)","Extraction cost (million EUR)",
            "European trade cost (million EUR)","International trade cost (million EUR)","Total cost (million EUR)","Extraction cost (EUR/MWh)","European trade cost (EUR/MWh)","International trade cost (EUR/MWh)",
            "Total cost (EUR/MWh)" /
;


Parameter
Biomass_rpt(t,r,biomass_set)
Biomass_total_rpt(t,biomass_set)
Biomass_GER_rpt(t,biomass_set)
Biomass_FR_rpt(t,biomass_set)
;

Biomass_rpt(t,r,"Use elec (TWh)")               = sum(ivrt(bio(i),v,r,t), XTWH.L(i,v,r,t) / effrate(i,v,r)) + eps ;
Biomass_rpt(t,r,"Use elec* (TWh)")              = BCR.L(r,t) + eps ;
Biomass_rpt(t,r,"Use remaining (TWh)")          = dbrem(r,t) + eps ;
Biomass_rpt(t,r,"Use total (TWh)")              = BCR.L(r,t) + dbrem(r,t) + eps ;
Biomass_rpt(t,r,"Extraction (TWh)")             =
$if      set biomarket1_r                       sum(dbclass, DBSR.L(dbclass,r,t)) +
$if      set biomarket2_r                       sum(dbclass, DBSRAGG.L(dbclass,r,t) * DBCLAR(dbclass,r,t)) +
$if      set biomarket3_r                       sum(dbclass, DBSRAGG.L(dbclass,r,t)) +
$if      set biomarket1                         sum(dbclass, DBS.L(dbclass,t)) * (BCR.L(r,t) + dbrem(r,t)) / (BC.L(t) + sum(rr, dbrem(rr,t))) +
$if      set biomarket2                         sum(dbclass, DBSAGG.L(dbclass,t) * DBCLA(dbclass,t)) * (BCR.L(r,t) + dbrem(r,t)) / (BC.L(t) + sum(rr, dbrem(rr,t))) +
$if      set biomarket3                         sum(dbclass, DBSAGG.L(dbclass,t)) * (BCR.L(r,t) + dbrem(r,t)) / (BC.L(t) + sum(rr, dbrem(rr,t))) +
                                                eps ;
Biomass_rpt(t,r,"European imports (TWh)")       =
$if      set biotrade                           sum(dbclass, sum(dbmap(dbtrans,rr,r), DBE.L(dbtrans,dbclass,rr,r,t))) +
                                                eps ;
Biomass_rpt(t,r,"European exports (TWh)")       =
$if      set biotrade                           sum(dbclass, sum(dbmap(dbtrans,r,rr), DBE.L(dbtrans,dbclass,r,rr,t))) +
                                                eps ;
Biomass_rpt(t,r,"International imports (TWh)")  =
$if      set biobackstop                        DBIMP.L(r,t) +
                                                eps ;
Biomass_rpt(t,r,"Extraction cost (million EUR)")    =
$if      set biomarket1_r                       sum(dbclass, DBSR.L(dbclass,r,t) * dbcost_r(dbclass,r,t))  +
$if      set biomarket2_r                       sum(dbclass, DBSRAGG.L(dbclass,r,t) * DBCLAR(dbclass,r,t) * dbcost_r(dbclass,r,t)) +
$if      set biomarket3_r                       sum(dbclass, DBSRAGG.L(dbclass,r,t) * dbcost_r(dbclass,r,t)) +
$if      set biomarket1                         sum(dbclass, DBS.L(dbclass,t) * dbcost(dbclass,t)) * (BCR.L(r,t) + dbrem(r,t)) / (BC.L(t) + sum(rr, dbrem(rr,t))) +
$if      set biomarket2                         sum(dbclass, DBSAGG.L(dbclass,t) * DBCLAR(dbclass,r,t) * dbcost(dbclass,t)) * (BCR.L(r,t) + dbrem(r,t)) / (BC.L(t) + sum(rr, dbrem(rr,t))) +
$if      set biomarket3                         sum(dbclass, DBSAGG.L(dbclass,t) * dbcost(dbclass,t)) * (BCR.L(r,t) + dbrem(r,t)) / (BC.L(t) + sum(rr, dbrem(rr,t))) +
                                                eps ;
Biomass_rpt(t,r,"European trade cost (million EUR)") = 
*$if      set biotrade                           sum(dbclass, sum(dbmap(dbtrans,r,rr), DBE.L(dbtrans,dbclass,r,rr,t) * dbcost_rr(dbclass,dbtrans,r,rr,t))) +
$if      set biotrade                           sum(dbclass, sum(dbmap(dbtrans,rr,r), DBE.L(dbtrans,dbclass,rr,r,t) * dbcost_rr(dbclass,dbtrans,rr,r,t))) +
                                                eps ;
Biomass_rpt(t,r,"International trade cost (million EUR)") =
$if      set biobackstop                        DBIMP.L(r,t) * dbcost_imp(r,t) +
                                                eps ;
Biomass_rpt(t,r,"Total cost (million EUR)")     = Biomass_rpt(t,r,"Extraction cost (million EUR)") + Biomass_rpt(t,r,"European trade cost (million EUR)") + Biomass_rpt(t,r,"International trade cost (million EUR)") ;

Biomass_rpt(t,r,"Extraction cost (EUR/MWh)")$(Biomass_rpt(t,r,"Extraction (TWh)") > 0)                      = Biomass_rpt(t,r,"Extraction cost (million EUR)")          / Biomass_rpt(t,r,"Extraction (TWh)") ;
Biomass_rpt(t,r,"European trade cost (EUR/MWh)")$(Biomass_rpt(t,r,"European imports (TWh)") > 0)            = Biomass_rpt(t,r,"European trade cost (million EUR)")      / Biomass_rpt(t,r,"European imports (TWh)") ;
Biomass_rpt(t,r,"European trade cost (EUR/MWh)")$(Biomass_rpt(t,r,"European imports (TWh)") = 0)            = eps ;
Biomass_rpt(t,r,"International trade cost (EUR/MWh)")$(Biomass_rpt(t,r,"International imports (TWh)") > 0)  = Biomass_rpt(t,r,"International trade cost (million EUR)") / Biomass_rpt(t,r,"International imports (TWh)") ;
Biomass_rpt(t,r,"International trade cost (EUR/MWh)")$(Biomass_rpt(t,r,"International imports (TWh)") = 0)  = eps ;
Biomass_rpt(t,r,"Total cost (EUR/MWh)")$(Biomass_rpt(t,r,"Use total (TWh)") > 0)                            = Biomass_rpt(t,r,"Total cost (million EUR)")               / Biomass_rpt(t,r,"Use total (TWh)") ;

Biomass_total_rpt(t,biomass_set) = sum(r, Biomass_rpt(t,r,biomass_set)) ;
Biomass_GER_rpt(t,biomass_set) = sum(rge(r), Biomass_rpt(t,r,biomass_set)) ;
Biomass_FR_rpt(t,biomass_set) = sum(rfr(r), Biomass_rpt(t,r,biomass_set)) ;

Biomass_total_rpt(t,"Extraction cost (EUR/MWh)")$(Biomass_total_rpt(t,"Extraction (TWh)") > 0)                      = Biomass_total_rpt(t,"Extraction cost (million EUR)")          / Biomass_total_rpt(t,"Extraction (TWh)") ;
Biomass_total_rpt(t,"European trade cost (EUR/MWh)")$(Biomass_total_rpt(t,"European imports (TWh)") > 0)            = Biomass_total_rpt(t,"European trade cost (million EUR)")      / Biomass_total_rpt(t,"European imports (TWh)") ;
Biomass_total_rpt(t,"European trade cost (EUR/MWh)")$(Biomass_total_rpt(t,"European imports (TWh)") = 0)            = eps ;
Biomass_total_rpt(t,"International trade cost (EUR/MWh)")$(Biomass_total_rpt(t,"International imports (TWh)") > 0)  = Biomass_total_rpt(t,"International trade cost (million EUR)") / Biomass_total_rpt(t,"International imports (TWh)") ;
Biomass_total_rpt(t,"International trade cost (EUR/MWh)")$(Biomass_total_rpt(t,"International imports (TWh)") = 0)  = eps ;
Biomass_total_rpt(t,"Total cost (EUR/MWh)")$(Biomass_total_rpt(t,"Use total (TWh)") > 0)                            = Biomass_total_rpt(t,"Total cost (million EUR)")               / Biomass_total_rpt(t,"Use total (TWh)") ;