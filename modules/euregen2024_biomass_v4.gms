set
dbclass Dedicated biomass supply classes
dbtrans Dedicated biomass transport mode
dbmap(dbtrans,r,r) Transport mode mapping
fuelused fuel used
;

$gdxin precal\precal_%n%.gdx
$load dbclass,dbtrans,dbmap,fuelused
$gdxin

parameter
dbref_r(r,t) Reference dedicated biomass consumption (EJ) (regional)
dbref(t) Reference dedicated biomass consumption (EJ) (system)
dbelas_r(r,t) Supply elasticity for dedicated biomass (regional)
dbelas(t) Supply elasticity for dedicated biomass (system)
dbcost_r(dbclass,r,t) Cost of dedicated biomass by supply class (EUR per MWh th) (regional)
dbcost(dbclass,t) Cost of dedicated biomass by supply class (EUR per MWh th) (system)
dbcost_rr(dbclass,dbtrans,r,r,t) Trade cost of dedicated biomass by supply class (EUR per MWh th) (regional)
dbfueluse_rr(fuelused,dbclass,dbtrans,r,r,t) Trade fuel used (MWh per MWh)(regional)
dblim_r(dbclass,r,t) Class size of dedicated biomass supply function (regional)
dblim(dbclass,t) Class size of dedicated biomass supply function (system)
dblimagg_r(dbclass,r,t) Aggregated biomass supply (TWh) (regional)
dblimagg(dbclass,t) Aggregated biomass supply (TWh) (system)
dbcost_imp(r,t)
dbrem(r,t)
;

$gdxin precal\precal_%n%.gdx
$load dbref_r
$load dbref
$load dbelas_r
$load dbelas
$load dbcost_r
$load dbcost
$load dbcost_rr
$load dbfueluse_rr
$load dblim_r
$load dblim
$load dblimagg_r
$load dblimagg
$load dbrem
$gdxin

alias(dbclass,dbclasss) ;

dbcost_imp(r,t) = 80 ;

positive Variable
DBSR(dbclass,r,t) Dedicated biomass supply (regional) (TWh)
DBS(dbclass,t) Dedicated biomass supply (system) (TWh)
DBSRAGG(dbclass,r,t) Dedicated biomass supply (regional) (TWh)
DBSAGG(dbclass,t) Dedicated biomass supply (system) (TWh)
* With trade
DBE(dbtrans,dbclass,r,r,t) Biomass traded from r to r
DBIMP(r,t)
;

binary variable
DBCLA(dbclass,t)
DBCLAR(dbclass,r,t)
;

Equations
* Market equations without trade
biomarket(t) System-wide market for bioenergy or supply equal demand for bioenergy
biomarket_r(r,t) Supply equal demand for bioenergy (regional)
biomarket3lo(dbclass,t) System-wide market for bioenergy or supply equal demand for bioenergy
biomarket3up(dbclass,t) System-wide market for bioenergy or supply equal demand for bioenergy
biomarket3lo_r(dbclass,r,t) Supply equal demand for bioenergy (regional)
biomarket3up_r(dbclass,r,t) Supply equal demand for bioenergy (regional)
sumdbcla(t) Binary variable equation to determine last class for price setting (system)
sumdbclar(r,t) Binary variable equation to determine last class for price setting (regional)
biolimit(dbclass,t) Bioenergy limit when trade happens
;

biomarket(toptimize(t))..
$if      set biomarket1     sum(dbclass, DBS(dbclass,t)) +
$if      set biomarket2     sum(dbclass, DBSAGG(dbclass,t) * DBCLA(dbclass,t)) +           
$if      set biomarket3     sum(dbclass, DBSAGG(dbclass,t)) +
$if      set biobackstop    sum(r, DBIMP(r,t)) +
               0 =e= BC(t)
$if      set bioother     + sum(r, dbrem(r,t))          
               ;
               
biomarket_r(r,toptimize(t))..
* Domestic bioenergy extraction depending on classes
$if      set biomarket1_r   sum(dbclass, DBSR(dbclass,r,t)) +
$if      set biomarket2_r   sum(dbclass, DBSRAGG(dbclass,r,t) * DBCLA(dbclass,t)) +
$if      set biomarket3_r   sum(dbclass, DBSRAGG(dbclass,r,t)) +
$if      set biobackstop    DBIMP(r,t) +
* Inter-region imports of bioenergy                        
$if      set biotrade       sum(dbclass, sum(dbmap(dbtrans,rr,r), DBE(dbtrans,dbclass,rr,r,t))) +
* Domestic bioenergy usage for electricity and combined heat generation (over all classes)
                            0 =e= BCR(r,t)
* Domestic bioenergy usage for remaining sectors
$if      set bioother     + dbrem(r,t)
* Inter-region exports of bioenergy
$if      set biotrade     + sum(dbclass, sum(dbmap(dbtrans,r,rr), DBE(dbtrans,dbclass,r,rr,t)))  
                            ;
                        
* Binary variables that gives the last used class to set prices of this class
sumdbcla(toptimize(t))..
    sum(dbclass, DBCLA(dbclass,t)) =e= 1 ;
sumdbclar(r,toptimize(t))..
    sum(dbclass, DBCLAR(dbclass,r,t)) =e= 1 ;

* * Enforce class-specific biomass limits
* For biomass1
$if      set biomarket1     DBS.UP(dbclass,toptimize(t))       = dblim(dbclass,t) ;
$if      set biomarket1_r   DBSR.UP(dbclass,r,toptimize(t))    = dblim_r(dbclass,r,t) ;
* For biomass2
$if      set biomarket2_r   DBSRAGG.UP(dbclass,r,toptimize(t)) = dblimagg_r(dbclass,r,t) ;
$if      set biomarket2     DBSAGG.UP(dbclass,toptimize(t))    = dblimagg(dbclass,t) ;
$if      set biomarket2_r   DBSRAGG.LO("1",r,toptimize(t))     = 0 ;
$if      set biomarket2     DBSAGG.LO("1",toptimize(t))        = 0 ;
$if      set biomarket2_r   DBSRAGG.LO(dbclass,r,toptimize(t))$(dbclass.val ge 2) = dblimagg_r(dbclass-1,r,t) ;
$if      set biomarket2     DBSAGG.LO(dbclass,toptimize(t))$(dbclass.val ge 2)    = dblimagg(dbclass-1,t) ;
* For biomass3
biomarket3lo(dbclass,toptimize(t))$(dbclass.val ge 2)..
               DBSAGG(dbclass,t) =g= dblimagg(dbclass-1,t) * DBCLA(dbclass,t) ;
biomarket3up(dbclass,toptimize(t))..
               DBSAGG(dbclass,t) =l= dblimagg(dbclass,t)   * DBCLA(dbclass,t) ; 

biomarket3lo_r(dbclass,r,toptimize(t))$(dbclass.val ge 2)..
               DBSRAGG(dbclass,r,t) =g= dblimagg_r(dbclass-1,r,t) * DBCLAR(dbclass,r,t) ;
biomarket3up_r(dbclass,r,toptimize(t))..
               DBSRAGG(dbclass,r,t) =l= dblimagg_r(dbclass,r,t)   * DBCLAR(dbclass,r,t) ;
  
* For regional biomass markets with trade total biomass use cannot exceed total biomass limits (for each class)
biolimit(dbclass,toptimize(t))..
$if      set biomarket1_r   sum(r, DBSR(dbclass,r,t)) +
$if      set biomarket2_r   sum(r, DBSRAGG(dbclass,r,t) * DBCLA(dbclass,t)) +
$if      set biomarket3_r   sum(r, DBSRAGG(dbclass,r,t)) +
                            0 =l= dblim(dbclass,t) ;
