* * * Timeseries and calibration
set
s                                Segments
sm(s,m)                          Map between segments and months for assembling availability factors
hmaps(h,s)                       Map between hours and segments (which segment is for which real hour)
srep(s,h)                        Map from segments to representative (chosen) hours
sone(s)
stwo(s)
snum(s)
peak(s,r,t)                      Peak segment
;

$gdxin precal\precal_%n%.gdx
$load s,sm,hmaps,srep,sone,stwo,snum,peak
$gdxin

alias(s,ss) ;

parameter
hours(s)                                Number of hours per load segment
number                                  Number of segments
load(s,r,t)                             Base year load across segments including both retail and direct (GW) (corrected)
loadmax(r,t)
load_sec(s,r,t,l)
vrsc_exi(s,i,r,t)                       Capacity factor for existing resources
vrsc(s,i,v,r)                           Capacity factor for variable resources
loss(s,r,t)
irnwflh_h(i,v,r)                        Intermittent renewables full-load hours (hours)
irnwflh_s(i,v,r)                        Intermittent renewables full-load hours (segments)
irnwflh_exi_h(i,r,t)                        Intermittent renewables full-load hours (hours)
irnwflh_exi_s(i,r,t)                        Intermittent renewables full-load hours (segments)
inflowtot_h(j,v,r,t)
inflowtot_s(j,v,r,t)
inflowtot_nv_h(j,r,t)
inflowtot_nv_s(j,r,t)
inflow(s,j,v,r,t)
inflow_nv(s,j,r,t)
;

$gdxin precal\precal_%n%.gdx
$load hours,number
$if not  set hours  $load load
$if      set hours  $load load=load_s
$load loadmax
$if not  set hours  $load load_sec
$if      set hours  $load load_sec=load_sec_s
$if not  set hours  $load loss
$if      set hours  $load loss=loss_s
$if not  set hours  $load vrsc
$if      set hours  $load vrsc=vrsc_s
$if not  set hours  $load vrsc_exi
$if      set hours  $load vrsc_exi=vrsc_exi_s
$load irnwflh_h
$load irnwflh_s
$load irnwflh_exi_h
$load irnwflh_exi_s
$load inflowtot_h
$load inflowtot_s
$load inflowtot_nv_h
$load inflowtot_nv_s
$if not  set hours  $load inflow
$if not  set hours  $load inflow_nv
$if      set hours  $load inflow=inflow_s
$if      set hours  $load inflow_nv=inflow_nv_s
$gdxin

parameter
voll_s(s,r,t)
;

$gdxin precal\precal_%n%.gdx
$load voll_s
$gdxin

* * * Availability factor matrix (too large to read in)
parameter
af(s,i,v,r,t)         
afmin(s,i,v,r,t)
afmax(s,i,v,r,t)
gafmin(s,j,v,r,t)
gafmax(s,j,v,r,t)
gafminnv(s,j,r,t)
gafmaxnv(s,j,r,t)
hydtwh(r,t)
;

$gdxin precal\precal_%n%.gdx
$load af,afmin,afmax,gafminnv,gafmaxnv
$gdxin

$if      set hydrogensimple     $if not  set hydrogenimport     hydtwh(r,t) = round(sum(s, hours(s) * load_sec(s,r,t,"hyd") * sum(tv(t,v), dchrgpen("Storage_LT",v,r)) * 1),4) * 1e-3;
$if      set hydrogensimple     $if      set hydrogenimport     hydtwh(r,t) = round(sum(s, hours(s) * load_sec(s,r,t,"hyd") * sum(tv(t,v), dchrgpen("Storage_LT",v,r)) * 3),4) * 1e-3;

* * * Declare Model
positive variable
* Demand
BS(s,r,t)               Lost load (backstop demand option) (GW)
* Generation
X(s,i,v,r,t)            Unit dispatch by segment (GW)
XIRNW(s,r,t)            Unit dispatch by segment (GW)
XCS(s,i,v,r,t)          Copies of XC over s for sparsity purposes (GW)
* Storage
G(s,j,v,r,t)            Energy storage charge (GW)
GD(s,j,v,r,t)           Energy storage discharge (GW)
GCS(s,j,v,r,t)          Energy storage charge-discharge capacity (GW)
GB(s,j,v,r,t)           Energy storage accumulated balance (TWh)
GNV(s,j,r,t)            Energy storage charge (GW)
GDNV(s,j,r,t)           Energy storage discharge (GW)
GCSNV(s,j,r,t)          Energy storage charge-discharge capacity (GW)
GBNV(s,j,r,t)           Energy storage accumulated balance no vintage (TWh)
HIMP(j,r,t)             Imported hydrogen
HRES(j,r,t)             Used hydrogen outside
* Transmission
E(s,k,r,r,t)            Bilateral trade flows by load segment (GW)
TCS(s,k,r,r,t)          New Trade flow capacity (GW)

equation
* Demand equations
demand(s,r,t)                    Electricity market clearing condition
demand_rsa(s,r,t)                Regional system adequacy condition
* Generation and capacity
capacity_all(s,i,v,r,t)          Generation capacity constraint on dispatch
capacity(s,i,v,r,t)              Generation capacity constraint on dispatch
capacity_irnw(s,r,t)             IRNW generation capacity
capacity_chp(s,i,v,r,t)
capacity_nucmin_old(s,i,v,r,t)   Nuclear generation capacity old vintages
capacity_nucmax_old(s,i,v,r,t)   Nuclear generation capacity old vintages
capacity_nuc_new(s,i,v,r,t)      Nuclear generation capacity new vintages
capacity_nuc_old(s,i,v,r,t)      Nuclear generation capacity new vintages
capacity_biomin_old(s,i,v,r,t)   Generation capacity constraint on dispatch of bioenergy (to avoid implementing a subsidy on bioenergy)
capacity_biomax_old(s,i,v,r,t)   Generation capacity constraint on dispatch of bioenergy (to avoid implementing a subsidy on bioenergy)
capacity_bio_new(s,i,v,r,t)      Generation capacity constraint on dispatch of bioenergy (to avoid implementing a subsidy on bioenergy)
capacity_bio_old(s,i,v,r,t)      Generation capacity constraint on dispatch of bioenergy (to avoid implementing a subsidy on bioenergy)
* Storage equations
chargelim(s,j,v,r,t)
dischargelim(s,j,v,r,t)
chargelimc(s,j,v,r,t)
dischargelimd(s,j,v,r,t)
dischargelim_min(s,j,v,r,t)
dischargelim_max(s,j,v,r,t)
reservoirlim(s,j,v,r,t)
reservoirlimnonv(s,j,r,t)
reservoirlimr(s,j,v,r,t)
reservoirlimrnonv(s,j,r,t)
chargelimnv(s,j,r,t)
dischargelimnv(s,j,r,t)
dischargelimnv_min(s,j,r,t)
dischargelimnv_max(s,j,r,t)
reservoirlimnv(s,j,r,t)
storagebal(s,j,v,r,t)
storagebal_ann(j,v,r,t)
storagebal_firsts(s,j,v,r,t)
storagebal_lasts(s,j,v,r,t)    
storagebal_endbalance(j,v,r,t)
storagebalnv(s,j,r,t)
storagebalnv_ann(j,r,t)
storagebalnv_firsts(s,j,r,t)
storagebalnv_lasts(s,j,r,t)
storagebalnv_endbalance(j,r,t)
storagebalnv_old(s,j,r,t)
storagebalnv_old_ann(j,r,t)
storagebalnv_old_firsts(s,j,r,t)
storagebalnv_old_lasts(s,j,r,t)
storagebalnv_old_endbalance(j,r,t)
* Hydrogen
demand_hydrogen(r,t)
demand_hydrogenimport(r,t)
* Transmission equations
tcapacity(s,k,r,r,t)             Transmission capacity constraint on trade
* Structual equations
copyxc(s,i,v,r,t)                Make copies of XC in XCS
copygc(s,j,v,r,t)                Make copies of GC in GCS
copygcnv(s,j,r,t)                Make copies of GCNV in GCSNV
copytc(s,k,r,r,t)                Make copies of TC in TCS
;

* * * * * Demand equations
* * * Electricity market clearance condition (in each segment)
demand(s,r,toptimize(t))..
*        Scale from GW to TWh (so that dual variable (marginals/shadow price) is reported directly in EUR/MWh)
                         1e-3 * hours(s) * (
*        Dispatched generation in region
$if not  set mergeirnw     sum(ivrt(i,v,r,t),  X(s,i,v,r,t))
$if      set mergeirnw     sum(ivrt(notirnw(i),v,r,t),  X(s,i,v,r,t))
$if      set mergeirnw   + XIRNW(s,r,t)
*        Plus inter-region imports
$if      set trans       + sum(tmap(k,rr,r), E(s,k,rr,r,t))
*        Less inter-region exports (penalty for transmission losses is charged on the export site only)
$if      set trans       - sum(tmap(k,r,rr), E(s,k,r,rr,t) / trnspen(k,r,rr))
*        Plus discharges from storage times discharge efficiency (less supply than stored) less charges from storage (the penalties apply at the storage accumulation)
$if      set storage     $if not  set storagebalnv   + sum(jvrt(j,v,r,t), GD(s,j,v,r,t) * dchrgpen(j,v,r) - G(s,j,v,r,t))
$if      set storage     $if      set storagebalnv   + sum(nvj(j), GDNV(s,j,r,t) * dchrgpennv(j,r) - GNV(s,j,r,t)) + sum(jvrt(nonvj(j),v,r,t), GD(s,j,v,r,t) * dchrgpen(j,v,r) - G(s,j,v,r,t))
*        Plus a backstop option (lost load) representing segment-level demand response
$if      set hydrogen    + sum(bvrt(b,v,r,t), BX(s,b,v,r,t))
*        Plus a backstop option (lost load) representing segment-level demand response
$if      set lostload                           + BS(s,r,t) * (1 + loss(s,r,t))
$if      set lostload    $if      set elastic   + sum(bse, BSELAS(bse,s,r,t)) * (1 + loss(s,r,t))
         )
*        Equals (annually scaled) demand including losses
$if not  set elastic      =g= 1e-3 * hours(s) * round(load(s,r,t) * (1 + loss(s,r,t)),4) 
$if      set hydrogensimple - 1e-3 * hours(s) * round(load_sec(s,r,t,"hyd") * (1 + loss(s,r,t)),4)  
$if      set elastic      =g= 1e-3 * hours(s) * sum(bse, DS(bse,s,r,t)) * (1 + loss(s,r,t)) 
;

* * * Regional system adequacy constraint
* Hypothetical shadow electricity market with no transmission in the default version
* In the 4NEMO version it has changed by using capcredits. Here, transmission capacity is offered a tcapcredit of 0.1
demand_rsa(peak(s,r,topt2030plus(t)))..
*        Scale from GW to TWh (so that dual variable (marginals/shadow price) is reported directly in euro per MWh)
                         1e-3 * hours(s) * (
*        Upper bound on available generation in region
$if not  set flh        $if not  set flheur  + sum(ivrt(i,v,r,t),      XCS(s,i,v,r,t) *  capcred(i,v,r))
$if      set flh                            + sum(ivrt(i,v,r,t),       XC(i,v,r,t)    *  capcred(i,v,r))
$if      set flheur                         + sum(ivrt(i,v,r,t),       XC(i,v,r,t)    *  capcred(i,v,r))
*        Plus discharges from storage less charges (plus penalty)
$if      set storage    $if not  set storagebalnv   + sum(jvrt(j,v,r,t),       GC(j,v,r,t)  * gcapcred(j,v,r))
$if      set storage    $if      set storagebalnv   + sum(nvj(j),              GCNV(j,r,t)  * 0.9) + sum(jvrt(nonvj(j),v,r,t), GC(j,v,r,t)  * gcapcred(j,v,r))
$if      set storage    $if      set hydrogensimple + sum(jvrt(ghyd(j),v,r,t), GCD(j,v,r,t) * gcapcred(j,v,r))
*        Plus inter-region imports
$if      set trans                          + sum((k,rr)$tmap(k,rr,r), TC(k,rr,r,t)   * tcapcred(k,rr,r))
*        Plus hydrogen generation capacity
$if      set hydrogen                       + sum(bvrt(b,v,r,t),       BXCS(s,b,v,r,t))
         )
*        Equals (annually scaled) demand including losses
$if not  set elastic                         =g= 1e-3 * hours(s) * round(load(s,r,t) * (1 + loss(s,r,t)),4) 
$if      set hydrogensimple                    - 1e-3 * hours(s) * round(load_sec(s,r,t,"hyd") * (1 + loss(s,r,t)),4) 
$if      set elastic                         =g= 1e-3 * hours(s) * sum(bse, DS(bse,s,r,t)) * (1 + loss(s,r,t))
;

demand_hydrogen(r,toptimize(t))..
        HRES("Storage_LT",r,t) =e= hydtwh(r,t) ;

demand_hydrogenimport(r,toptimize(t))..
        HIMP("Storage_LT",r,t) =l= round(2/3 * hydtwh(r,t),4) ;
        
* * * * * Generation equations
* af are the monthly availability factor of dispatchable power and vrsc those of intermittent renewables (reliability is used if not af)
capacity_all(s,ivrt(i,v,r,t))..
                 X(s,i,v,r,t) =l=  XC(i,v,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (vrsc(s,i,v,r)-1)$vrsc(s,i,v,r)) ;
capacity(s,ivrt_capacity(i,v,r,t))..
                 X(s,i,v,r,t) =l=  XCS(s,i,v,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (vrsc(s,i,v,r)-1)$vrsc(s,i,v,r)) ;                 
* CHP
capacity_chp(s,ivrt(chp(i),oldv(v),r,t))..
$if not  set chpmustrun     X(s,i,v,r,t) =l=  XCS(s,i,v,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) ;                
$if      set chpmustrun     X(s,i,v,r,t) =e=  XCS(s,i,v,r,t) * af(s,i,v,r,t) ;              
* IRNW can neglect vintages because there are no variable cost (differentiation between new and existing profiles)
capacity_irnw(s,r,toptimize(t))..
                 XIRNW(s,r,t) =l= sum(ivrt(irnw_nohydro(i),newv(v),r,t), XCS(s,i,v,r,t) * vrsc(s,i,v,r) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)))
                                + sum(ivrt(irnw(i),oldv(v),r,t), XCS(s,i,v,r,t) * vrsc_exi(s,i,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t))) ;
*capacity_hydro(s,r,t)$(toptimize(t))..
*                 XHYDR(s,r,t) =l= sum(ivrt(i,v,r,t)$(sameas(i,"Hydro")) XCS(s,i,v,r,t) * vrsc_exi(s,i,r,t)) ;                                            
* Old nuclear is bounded by existing stuff
capacity_nucmin_old(s,ivrt(nuc(i),oldv(v),r,t))$(afmin(s,i,v,r,t) > 0)..
                 X(s,i,v,r,t) =g=  XCS(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * afmin(s,i,v,r,t) ;
capacity_nucmax_old(s,ivrt(nuc(i),oldv(v),r,t))..
                 X(s,i,v,r,t) =l=  XCS(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (afmax(s,i,v,r,t)-1)$afmax(s,i,v,r,t)) ;

* Old nuclear is must-run
capacity_nuc_old(s,ivrt(nuc(i),oldv(v),r,t))..
                 X(s,i,v,r,t) =l=  XCS(s,i,v,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) ;
* New nuclear is not must-run
capacity_nuc_new(s,ivrt(nuc(i),newv(v),r,t))..
                 X(s,i,v,r,t) =l=  XCS(s,i,v,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) ;
* Old bioenergy is bounded by existing stuff
capacity_biomin_old(s,ivrt_nochp(bio(i),oldv(v),r,t))$(afmin(s,i,v,r,t) > 0)..
                 X(s,i,v,r,t) =g=  XCS(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * afmin(s,i,v,r,t) ;
capacity_biomax_old(s,ivrt_nochp(bio(i),oldv(v),r,toptimize(t)))..
                 X(s,i,v,r,t) =l=  XCS(s,i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (afmax(s,i,v,r,t)-1)$afmax(s,i,v,r,t)) ;
* Old bioenergy is must-run
capacity_bio_old(s,ivrt_nochp(bio(i),oldv(v),r,toptimize(t)))..
                 X(s,i,v,r,t) =l=  XCS(s,i,v,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) ;
* New bioenergy is not must-run
capacity_bio_new(s,ivrt(bio(i),newv(v),r,t))..
                 X(s,i,v,r,t) =l=  XCS(s,i,v,r,t) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) ;
            
* * * * * Transmission equations
* * * Enforce capacity constraint on inter-region trade flows
tcapacity(s,tmapopt(k,r,rr,t))..
         E(s,k,r,rr,t) =l= (1 + (taf_expo(r,t)-1)$taf_expo(r,t)) * TCS(s,k,r,rr,t) ;

* * * * * Storage equations
* * * Storage charge-discharge and accumulation
* Charge must not exceed charge capacity (size of door - entry)
chargelim(s,
$if not  set storagebalnv    jvrt(j,v,r,t))$(not jres(j))..
$if      set storagebalnv    $if not  set hydrogensimple  jvrt(nonvj(j),v,r,t))..
$if      set storagebalnv    $if      set hydrogensimple  jvrt(gbat(j),v,r,t))..
         G(s,j,v,r,t)  =l= GCS(s,j,v,r,t) ;
         
chargelimc(s,jvrt(ghyd(j),v,r,t))..
         G(s,j,v,r,t)  =l= GCC(j,v,r,t) ;

* Discharge must not exceed charge capacity (size of door - exit)
dischargelim(s,
$if not  set storagebalnv    jvrt(j,v,r,t))..
$if      set storagebalnv    $if not  set hydrogensimple  jvrt(nonvj(j),v,r,t))..
$if      set storagebalnv    $if      set hydrogensimple  jvrt(gbat(j),v,r,t))..
         GD(s,j,v,r,t) =l= GCS(s,j,v,r,t) ;
         
dischargelimd(s,jvrt(ghyd(j),v,r,t))..
         GD(s,j,v,r,t) =l= GCD(j,v,r,t) ;
* Reservoir have some limitations that cannot get accessed by calibration (water shep, value of water, ..)
dischargelim_min(s,jvrt(jres(j),oldv(v),r,t))$(inflowtot_s(j,v,r,t) > 0 and gafmin(s,j,v,r,t) > 0)..
         GD(s,j,v,r,t) =g=  GCS(s,j,v,r,t) * gafmin(s,j,v,r,t) ;
dischargelim_max(s,jvrt(jres(j),oldv(v),r,t))$(inflowtot_s(j,v,r,t) > 0)..
         GD(s,j,v,r,t) =l=  GCS(s,j,v,r,t) * (1 + (gafmax(s,j,v,r,t)-1)$gafmax(s,j,v,r,t)) ;
* Reservoir lim
reservoirlim(s,jvrt(j,v,r,t))..
         GB(s,j,v,r,t) =l= ghours(j,v,r) * GC(j,v,r,t) * 1e-3 ;
reservoirlimnonv(s,
$if not  set hydrogensimple nonvj(j),r,t)..
$if      set hydrogensimple gbat(j),r,t)..
         GBNV(s,j,r,t) =l= sum(jvrt(j,newv(v),r,t), ghours(j,v,r) * GC(j,v,r,t)) * 1e-3 ; 
* Reservoir lim
reservoirlimr(s,jvrt(ghyd(j),v,r,t))..
         GB(s,j,v,r,t) =l= GCR(j,v,r,t) * 1e-3 ;
reservoirlimrnonv(s,ghyd(j),r,t)..
         GBNV(s,j,r,t) =l= sum(jvrt(j,newv(v),r,t), GCR(j,v,r,t)) * 1e-3 ;
         
* No vintage equations
chargelimnv(s,jpump(j),r,t)..
         GNV(s,j,r,t)  =l= GCSNV(s,j,r,t) ;
dischargelimnv(s,jpump(j),r,t)..
         GDNV(s,j,r,t) =l= GCSNV(s,j,r,t) ;
dischargelimnv_min(s,jres(j),r,t)$(inflowtot_nv_s(j,r,t) > 0 and gafminnv(s,j,r,t) > 0)..
         GDNV(s,j,r,t) =g=  GCSNV(s,j,r,t) * gafminnv(s,j,r,t) ;
dischargelimnv_max(s,jres(j),r,t)$(inflowtot_nv_s(j,r,t) > 0)..
         GDNV(s,j,r,t) =l=  GCSNV(s,j,r,t) * (1 + (gafmaxnv(s,j,r,t)-1)$gafmaxnv(s,j,r,t)) ;
reservoirlimnv(s,nvj(j),r,t)..
         GBNV(s,j,r,t) =l= ghoursnv(j,r,t) * GCNV(j,r,t) * 1e-3 ;                 

        
parameter
hhours(s,j) hourly weighting for storage technologies
;

$gdxin precal\precal_%n%.gdx
$load hhours
$gdxin

hhours(s,"Storage_ST") = 12 ;
hhours(s,"PumpStorage") = 12 ;
   
* Vintage specific storage accumulation
storagebal(s,jvrt(j,v,r,t))..
        GB(s,j,v,r,t) =e= GB(s-1,j,v,r,t) * (1 - dischrg(j,v,r)) + hhours(s,j) * (G(s,j,v,r,t) * chrgpen(j,v,r)  - GD(s,j,v,r,t)) * 1e-3 ;
storagebal_ann(jvrt(j,v,r,t))..
        sum(s, hours(s) * (G(s,j,v,r,t) * chrgpen(j,v,r) - GD(s,j,v,r,t))) * 1e-3 =g= 0 ;
storagebal_firsts(sone(s),jvrt(j,v,r,t))..
        GB(s,j,v,r,t)   =e= storage_endbalance(j,v,r,t) * ghours(j,v,r) * GC(j,v,r,t) * (1 - dischrg(j,v,r)) * 1e-3 + hhours(s,j) * (G(s,j,v,r,t) * chrgpen(j,v,r)  - GD(s,j,v,r,t)) * 1e-3 ;
storagebal_lasts(s,jvrt(j,v,r,t))$(s.val eq number)..
        GB(s,j,v,r,t)   =e= storage_endbalance(j,v,r,t) * ghours(j,v,r) * GC(j,v,r,t) * 1e-3 ;         
storagebal_endbalance(jvrt(j,v,r,t))..                       
        GB("1",j,v,r,t) =e= sum(snum(ss), GB(ss,j,v,r,t)) + hhours("1",j) * (G("1",j,v,r,t) * chrgpen(j,v,r)  - GD("1",j,v,r,t)) * 1e-3 ;  

* No vintages but charge/discharge vintage specific (for newly accumulated storage capacity that is subject to dynamically changing technology parameters)
storagebalnv(stwo(s),nonvj(j),r,toptimize(t))..
         GBNV(s,j,r,t)   =e= GBNV(s-1,j,r,t) * (1 - dischrgnv(j,r)) + hhours(s,j) * ((HIMP(j,r,t) - HRES(j,r,t))/8.76 + sum(jvrt(j,newv(v),r,t), G(s,j,v,r,t) * chrgpen(j,v,r)  - GD(s,j,v,r,t))) * 1e-3 ;
storagebalnv_ann(nonvj(j),r,toptimize(t))..
         HIMP(j,r,t) - HRES(j,r,t) + sum(s, hours(s) * sum(jvrt(j,newv(v),r,t), G(s,j,v,r,t) * chrgpen(j,v,r) - GD(s,j,v,r,t))) * 1e-3 =g= 0 ;
storagebalnv_firsts(sone(s),nonvj(j),r,toptimize(t))..
         GBNV(s,j,r,t)   =e= storage_endbalancenv(j,r,t) * sum(jvrt(j,newv(v),r,t), ghours(j,v,r) * GC(j,v,r,t)) * (1 - dischrgnv(j,r)) * 1e-3 + hhours(s,j) * ((HIMP(j,r,t) - HRES(j,r,t))/8.76 + sum(jvrt(j,newv(v),r,t), G(s,j,v,r,t) * chrgpen(j,v,r)  - GD(s,j,v,r,t))) * 1e-3 ;
storagebalnv_lasts(snum(s),nonvj(j),r,toptimize(t))..
         GBNV(s,j,r,t)    =e= storage_endbalancenv(j,r,t) * sum(jvrt(j,newv(v),r,t), ghours(j,v,r) * GC(j,v,r,t)) * 1e-3 ;
storagebalnv_endbalance(nonvj(j),r,toptimize(t))..
         GBNV("1",j,r,t) =e= sum(snum(ss), GBNV(ss,j,r,t)) + hhours("1",j) * ((HIMP(j,r,t) - HRES(j,r,t))/8.76 + sum(jvrt(j,newv(v),r,t), G("1",j,v,r,t) * chrgpen(j,v,r)  - GD("1",j,v,r,t))) * 1e-3 ;

* No vintages (for old accumulated storage capacity that is not subject to dynamically changing technology parameters)                        
storagebalnv_old(stwo(s),nvj(j),r,toptimize(t))..
         GBNV(s,j,r,t)   =l= GBNV(s-1,j,r,t) * (1 - dischrgnv(j,r)) + hhours(s,j) * (inflow_nv(s,j,r,t) + GNV(s,j,r,t) * chrgpennv(j,r)  - GDNV(s,j,r,t)) * 1e-3 ;
storagebalnv_old_ann(nvj(j),r,toptimize(t))..
         sum(s, hours(s) * (inflow_nv(s,j,r,t) + GNV(s,j,r,t) * chrgpennv(j,r) - GDNV(s,j,r,t))) * 1e-3 =g= 0 ;
storagebalnv_old_firsts(sone(s),nvj(j),r,toptimize(t))..
         GBNV(s,j,r,t)   =l= storage_endbalancenv(j,r,t) * ghoursnv(j,r,t) * GCNV(j,r,t) * (1 - dischrgnv(j,r)) * 1e-3 + hhours(s,j) * (inflow_nv(s,j,r,t) + GNV(s,j,r,t) * chrgpennv(j,r)  - GDNV(s,j,r,t)) * 1e-3 ;
storagebalnv_old_lasts(snum(s),nvj(j),r,toptimize(t))..
         GBNV(s,j,r,t)    =l= storage_endbalancenv(j,r,t) * ghoursnv(j,r,t) * GCNV(j,r,t) * 1e-3 ;
storagebalnv_old_endbalance(nvj(j),r,toptimize(t))..
         GBNV("1",j,r,t) =l= sum(snum(ss), GBNV(ss,j,r,t)) + hhours("1",j) * (inflow_nv("1",j,r,t) + GNV("1",j,r,t) * chrgpennv(j,r)  - GDNV("1",j,r,t)) * 1e-3 ;

* Fix reservoir charge to zero
$if not  set storagebalnv    G.FX(s,"Reservoir",v,r,t) = 0 ;
$if      set storagebalnv    GNV.FX(s,"Reservoir",r,t) = 0 ; 
 
* Segments
copyxc(s,ivrt(i,v,r,t))..                              XCS(s,i,v,r,t)  =e= XC(i,v,r,t)$(ord(s) eq 1)  + XCS(s-1,i,v,r,t)$(ord(s) > 1) ;
copygc(s,
$if not  set storagebalnv jvrt(j,v,r,t)
$if      set storagebalnv jvrt(nonvj(j),v,r,t)            
                         )..                          GCS(s,j,v,r,t)  =e= GC(j,v,r,t)$(ord(s) eq 1)  + GCS(s-1,j,v,r,t)$(ord(s) > 1) ;
copygcnv(s,nvj(j),r,toptimize(t))..                   GCSNV(s,j,r,t)  =e= GCNV(j,r,t)$(ord(s) eq 1)  + GCSNV(s-1,j,r,t)$(ord(s) > 1) ;
copytc(s,tmapopt(k,r,rr,t))..                         TCS(s,k,r,rr,t) =e= TC(k,r,rr,t)$(ord(s) eq 1) + TCS(s-1,k,r,rr,t)$(ord(s) > 1) ;