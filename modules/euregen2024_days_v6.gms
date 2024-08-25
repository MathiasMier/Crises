set
d /1*365/
sd
hd
srepdays(sd,d)
sdm(sd,m)
sdone(sd)
sdtwo(sd)
sdnum(sd)
peak_d(sd,hd,r,t)
;

$gdxin precal\precal_%n%.gdx
$load sd,hd,srepdays,sdm,sdone,sdtwo,sdnum,peak_d
$gdxin

alias(sd,sdd) ;

set
hdone(hd) /1/
hdtwo(hd)
;

hdtwo(hd)$(hd.val ge 2) = YES ;

parameter
days(sd)
numberdays
load_d(sd,hd,r,t)
loadmax_d(r,t)                                           
load_sec_d(sd,hd,r,t,l)
loss_d(sd,hd,r,t)
vrsc_exi_d(sd,hd,i,r,t)                      
vrsc_d(sd,hd,i,v,r)                                                                        
irnwflh_h(i,v,r)                     
irnwflh_d(i,v,r)                       
irnwflh_exi_h(i,r,t)                     
irnwflh_exi_d(i,r,t)
inflowtot_h_d(j,v,r,t)
inflowtot_d(j,v,r,t)
inflowtot_nv_h_d(j,r,t)
inflowtot_nv_d(j,r,t)
inflow_d(sd,hd,j,v,r,t)
inflow_nv_d(sd,hd,j,r,t)
;

$gdxin precal\precal_%n%.gdx
$load days,numberdays
$load load_d
$load loadmax_d
$load load_sec_d
$load loss_d
$load vrsc_exi_d
$load vrsc_d
$load irnwflh_h
$load irnwflh_d
$load irnwflh_exi_h
$load irnwflh_exi_d
$load inflowtot_h_d
$load inflowtot_d
$load inflowtot_nv_h_d
$load inflowtot_nv_d
$load inflow_d
$load inflow_nv_d 
$gdxin

$if      set load2021   load_d(sd,hd,r,"2022") = round(load_d(sd,hd,r,"2021") * daref(r,"2022") / daref(r,"2021"),4) ;
$if      set load2021   load_d(sd,hd,r,"2023") = round(load_d(sd,hd,r,"2021") * daref(r,"2023") / daref(r,"2021"),4) ;
$if      set load202x   load_d(sd,hd,r,topt2024(t)) = round(load_d(sd,hd,r,"2021") * daref(r,t) / daref(r,"2021"),4) ;

parameter
voll_d(sd,hd,r,t)
;

$gdxin precal\precal_%n%.gdx
$load voll_d
$gdxin

* * * Availability factor matrix (too large to read in)
parameter
af_d(sd,hd,i,v,r,t)   
afmin_d(sd,hd,i,v,r,t)
afmax_d(sd,hd,i,v,r,t)
gafmin_d(sd,hd,j,v,r,t)
gafmax_d(sd,hd,j,v,r,t)
gafminnv_d(sd,hd,j,r,t)
gafmaxnv_d(sd,hd,j,r,t)
hydtwh(r,t)
;

$gdxin precal\precal_%n%.gdx
$load af_d,afmin_d,afmax_d,gafmin_d,gafmax_d,gafminnv_d,gafmaxnv_d
$gdxin

$if      set hydrogensimple     $if not  set hydrogenimport     hydtwh(r,t) = round(sum((sd,hd), days(sd) * load_sec_d(sd,hd,r,t,"hyd") * sum(tv(t,v), dchrgpen("Storage_LT",v,r)) * 1),4) * 1e-3;
$if      set hydrogensimple     $if      set hydrogenimport     hydtwh(r,t) = round(sum((sd,hd), days(sd) * load_sec_d(sd,hd,r,t,"hyd") * sum(tv(t,v), dchrgpen("Storage_LT",v,r)) * 3),4) * 1e-3;

* * * Declare Model
positive variable
BS_D(sd,hd,r,t)         Lost load (backstop demand option) (GW)
*BSELAS_D(bse,sd,hd,r,t) Lost load (backstop demand option) (GW)
*DS_D(bse,sd,hd,r,t)     Segment load for elastic demand module (GW)
X_D(sd,hd,i,v,r,t)      Unit dispatch by segment (GW)
XIRNW_D(sd,hd,r,t)      Unit dispatch by segment (GW)
XCS_SD(sd,i,v,r,t)    Copies of XC over s for sparsity purposes (GW)
XCS_D(sd,hd,i,v,r,t)    Copies of XC over s for sparsity purposes (GW)
G_D(sd,hd,j,v,r,t)      Energy storage charge (GW)
GD_D(sd,hd,j,v,r,t)     Energy storage discharge (GW)
GCS_SD(sd,j,v,r,t)    Energy storage charge-discharge capacity (GW)
GCS_D(sd,hd,j,v,r,t)    Energy storage charge-discharge capacity (GW)
GB_D(sd,hd,j,v,r,t)     Energy storage accumulated balance (TWh)
GNV_D(sd,hd,j,r,t)      Energy storage charge (GW)
GDNV_D(sd,hd,j,r,t)     Energy storage discharge (GW)
GCSNV_SD(sd,j,r,t)    Energy storage charge-discharge capacity (GW)
GCNV(j,r,t)    Energy storage charge-discharge capacity (GW)
GBNV_D(sd,hd,j,r,t)     Energy storage accumulated balance no vintage (TWh)
E_D(sd,hd,k,r,r,t)      Bilateral trade flows by load segment (GW)
TCS_SD(sd,k,r,r,t)    New Trade flow capacity (GW)
TCS_D(sd,hd,k,r,r,t)    New Trade flow capacity (GW)
* Define net variables and equation to handle daily pattern  
GBNV_DD(sd,j,r,t)
GB_DD(sd,j,v,r,t)
* Hydrogen
HIMP(j,r,t)             Imported hydrogen (TWh)
HRES(j,r,t)             Used hydrogen outside (TWh)
;

equation
* Demand equations
demand_d(sd,hd,r,t)                    Electricity market clearing condition
demand_rsa_d(sd,hd,r,t)                Regional system adequacy condition
* Generation and capacity
capacity_all_d(sd,hd,i,v,r,t)          Generation capacity constraint on dispatch
capacity_d(sd,hd,i,v,r,t)              Generation capacity constraint on dispatch
capacity_irnw_d(sd,hd,r,t)             IRNW generation capacity
capacity_chp_d(sd,hd,i,v,r,t)          CHP generation capacity
capacity_chpmin_d(sd,hd,i,v,r,t)          CHP generation capacity
capacity_chpmax_d(sd,hd,i,v,r,t)          CHP generation capacity
capacity_nucmin_old_d(sd,hd,i,v,r,t)   Nuclear generation capacity old vintages
capacity_nucmax_old_d(sd,hd,i,v,r,t)   Nuclear generation capacity old vintages
capacity_nuc_new_d(sd,hd,i,v,r,t)      Nuclear generation capacity new vintages
capacity_nuc_old_d(sd,hd,i,v,r,t)      Nuclear generation capacity new vintages
capacity_biomin_old_d(sd,hd,i,v,r,t)   Generation capacity constraint on dispatch of bioenergy (to avoid implementing a subsidy on bioenergy)
capacity_biomax_old_d(sd,hd,i,v,r,t)   Generation capacity constraint on dispatch of bioenergy (to avoid implementing a subsidy on bioenergy)
capacity_bio_new_d(sd,hd,i,v,r,t)      Generation capacity constraint on dispatch of bioenergy (to avoid implementing a subsidy on bioenergy)
capacity_bio_old_d(sd,hd,i,v,r,t)      Generation capacity constraint on dispatch of bioenergy (to avoid implementing a subsidy on bioenergy)
* Storage equations
chargelim_d(sd,hd,j,v,r,t)
dischargelim_d(sd,hd,j,v,r,t)
dischargelim_min_d(sd,hd,j,v,r,t)
dischargelim_max_d(sd,hd,j,v,r,t)
reservoirlim_d(sd,hd,j,v,r,t)
reservoirlim_dd(sd,j,v,r,t)
reservoirlimnonv_d(sd,hd,j,r,t)
reservoirlimnonv_dd(sd,j,r,t)
chargelimnv_d(sd,hd,j,r,t)
dischargelimnv_d(sd,hd,j,r,t)
dischargelimnv_min_d(sd,hd,j,r,t)
dischargelimnv_max_d(sd,hd,j,r,t)
reservoirlimnv_d(sd,hd,j,r,t)
reservoirlimnv_dd(sd,j,r,t)
storagebal_sdone_hdone(sd,hd,j,v,r,t)
storagebal_sdtwo_hdone(sd,hd,j,v,r,t)
storagebal_hdtwo(sd,hd,j,v,r,t)
storagebal_sdone(sd,j,v,r,t)
storagebal_sdtwo(sd,j,v,r,t)
storagebal_ann_d(j,v,r,t)
storagebal_ann_d(j,v,r,t)
storagebalnv_sdone_hdone(sd,hd,j,r,t)
storagebalnv_sdtwo_hdone(sd,hd,j,r,t)
storagebalnv_hdtwo(sd,hd,j,r,t)
storagebalnv_sdone(sd,j,r,t)
storagebalnv_sdtwo(sd,j,r,t)
storagebalnv_ann_d(j,r,t)
storagebalnv_old_sdone_hdone(sd,hd,j,r,t)
storagebalnv_old_sdtwo_hdone(sd,hd,j,r,t)
storagebalnv_old_hdtwo(sd,hd,j,r,t)
storagebalnv_old_sdone(sd,j,r,t)
storagebalnv_old_sdtwo(sd,j,r,t)
storagebalnv_old_ann_d(j,r,t)
* Energy to power
chargelimc_d(sd,hd,j,v,r,t)
dischargelimd_d(sd,hd,j,v,r,t)
reservoirlimr_d(sd,hd,j,v,r,t)
reservoirlimr_dd(sd,j,v,r,t)
reservoirlimrnonv_d(sd,hd,j,r,t)
reservoirlimrnonv_dd(sd,j,r,t)
* Hydrogen
demand_hydrogen(r,t)
demand_hydrogenimport(r,t)
* Transmission equations
tcapacity_d(sd,hd,k,r,r,t)
* Structual equations
copyxc_sd(sd,i,v,r,t)                Make copies of XC in XCS
copygc_sd(sd,j,v,r,t)                Make copies of GC in GCS
copygcnv_sd(sd,j,r,t)                Make copies of GCNV in GCSNV
copytc_sd(sd,k,r,r,t)                Make copies of TC in TCS
copyxc_d(sd,hd,i,v,r,t)                Make copies of XC in XCS
copygc_d(sd,hd,j,v,r,t)                Make copies of GC in GCS
copygcnv_d(sd,hd,j,r,t)                Make copies of GCNV in GCSNV
copytc_d(sd,hd,k,r,r,t)                Make copies of TC in TCS
;

* * * Electricity market clearance condition (in each segment)
demand_d(sd,hd,r,toptimize(t))..
*        Scale from GW to TWh (so that dual variable (marginals/shadow price) is reported directly in EUR/MWh)
                         1e-3 * days(sd) * (
*        Dispatched generation in region
$if not  set mergeirnw     sum(ivrt(i,v,r,t),  X_D(sd,hd,i,v,r,t))
$if      set mergeirnw     sum(ivrt(notirnw(i),v,r,t),  X_D(sd,hd,i,v,r,t))
$if      set mergeirnw   + XIRNW_D(sd,hd,r,t)
*        Plus inter-region imports
$if      set trans       + sum(tmap(k,rr,r), E_D(sd,hd,k,rr,r,t))
*        Less inter-region exports (penalty for transmission losses is charged on the export site only)
$if      set trans       - sum(tmap(k,r,rr), E_D(sd,hd,k,r,rr,t) / trnspen(k,r,rr))
*        Plus discharges from storage times discharge efficiency (less supply than stored) less charges from storage (the penalties apply at the storage accumulation)
$if      set storage     $if not  set storagebalnv   + sum(jvrt(j,v,r,t), GD_D(sd,hd,j,v,r,t) * dchrgpen(j,v,r) - G_D(sd,hd,j,v,r,t))
$if      set storage     $if      set storagebalnv   + sum(nvj(j), GDNV_D(sd,hd,j,r,t) * dchrgpennv(j,r) - GNV_D(sd,hd,j,r,t)) + sum(jvrt(nonvj(j),v,r,t)$(not nvj(j)), GD_D(sd,hd,j,v,r,t) * dchrgpen(j,v,r) - G_D(sd,hd,j,v,r,t))
*        Plus a backstop option (lost load) representing segment-level demand response
$if      set hydrogen    + sum(bvrt(b,v,r,t), BX_D(sd,hd,b,v,r,t))
*        Plus a backstop option (lost load) representing segment-level demand response
$if      set lostload    + (BS_D(sd,hd,r,t)) * (1 + loss_d(sd,hd,r,t))
$if      set newlostload + (sum(bse, BSELAS_D(bse,sd,hd,r,t))) * (1 + loss_d(sd,hd,r,t))
         )
*        Equals (annually scaled) demand including losses
$if not  set elastic      =g= 1e-3 * days(sd) * round(load_d(sd,hd,r,t) * (1 + loss_d(sd,hd,r,t)),4)
$if      set hydrogensimple - 1e-3 * days(sd) * round(load_sec_d(sd,hd,r,t,"hyd") * (1 + loss_d(sd,hd,r,t)),4)  
$if      set elastic      =g= 1e-3 * days(sd) * sum(bse, DS_D(bse,sd,hd,r,t)) * (1 + loss_d(sd,hd,r,t)) 
;

* * * Regional system adequacy constraint
* Hypothetical shadow electricity market with no transmission in the default version
* In the 4NEMO version it has changed by using capcredits. Here, transmission capacity is offered a tcapcredit of 0.1
demand_rsa_d(peak_d(sd,hd,r,topt2030plus(t)))..
*        Scale from GW to TWh (so that dual variable (marginals/shadow price) is reported directly in euro per MWh)
                         1e-3 * days(sd) * (
*        Upper bound on available generation in region
$if not  set flh    $if not  set flheur  + sum(ivrt(i,v,r,t),       XC(i,v,r,t) *  capcred(i,v,r))
$if      set flh                           + sum(ivrt(i,v,r,t),       XC(i,v,r,t)    *  capcred(i,v,r))
$if      set flheur                     + sum(ivrt(i,v,r,t),       XC(i,v,r,t)    *  capcred(i,v,r))
*        Plus discharges from storage less charges (plus penalty)
$if      set storage    $if not  set storagebalnv   + sum(jvrt(j,v,r,t),       GC(j,v,r,t)  * gcapcred(j,v,r))
$if      set storage    $if      set storagebalnv   + sum(nvj(j),              GCNV(j,r,t)  * 0.9) + sum(jvrt(nonvj(j),v,r,t),       GC(j,v,r,t)  * gcapcred(j,v,r))
$if      set storage    $if      set hydrogensimple + sum(jvrt(ghyd(j),v,r,t), GCD(j,v,r,t) * gcapcred(j,v,r))
*        Plus inter-region imports
$if      set trans                          + sum((k,rr)$tmap(k,rr,r), TC(k,rr,r,t)   * tcapcred(k,rr,r))
*        Plus hydrogen generation capacity
$if      set hydrogen                       + sum(bvrt(b,v,r,t),       BXCS_D(sd,hd,b,v,r,t))
         )
*        Equals (annually scaled) demand including losses
$if not  set elastic                         =g= 1e-3 * days(sd) * round(load_d(sd,hd,r,t) * (1 + loss_d(sd,hd,r,t)),4) 
$if      set hydrogensimple                    - 1e-3 * days(sd) * round(load_sec_d(sd,hd,r,t,"hyd") * (1 + loss_d(sd,hd,r,t)),4) 
$if      set elastic                         =g= 1e-3 * days(sd) * sum(bse, DS_D(bse,sd,hd,r,t)) * (1 + loss_d(sd,hd,r,t)) 
;

demand_hydrogen(r,toptimize(t))..
        HRES("Storage_LT",r,t) =e= hydtwh(r,t) ;

demand_hydrogenimport(r,toptimize(t))..
        HIMP("Storage_LT",r,t) =l= round(2/3 * hydtwh(r,t),4) ;
        
* * * * * Generation and capacity equations

* af are the monthly availability factor of dispatchable power and vrsc those of intermittent renewables (reliability is used if not af)
capacity_all_d(sd,hd,ivrt(i,v,r,t))..
                 X_d(sd,hd,i,v,r,t) =l=  XC(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (vrsc_d(sd,hd,i,v,r)-1)$vrsc_d(sd,hd,i,v,r)) ;
capacity_d(sd,hd,ivrt_capacity(i,v,r,t))..
                 X_d(sd,hd,i,v,r,t) =l=  XC(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (vrsc_d(sd,hd,i,v,r)-1)$vrsc_d(sd,hd,i,v,r)) ;                 
* CHP
capacity_chp_d(sd,hd,ivrt(chp(i),oldv(v),r,t))..
                 X_d(sd,hd,i,v,r,t) =l=  XC(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) ;                
* IRNW can neglect vintages because there are no variable cost (differentiation between new and existing profiles)
capacity_irnw_d(sd,hd,r,toptimize(t))..
                 XIRNW_d(sd,hd,r,t) =l= sum(ivrt(irnw_nohydro(i),newv(v),r,t), XC(i,v,r,t) * vrsc_d(sd,hd,i,v,r) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)))
                                + sum(ivrt(irnw(i),oldv(v),r,t), XC(i,v,r,t) * vrsc_exi_d(sd,hd,i,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t))) ;
*capacity_hydro_d(sd,hd,r,t)$(toptimize(t))..
*                 XHYDR_d(sd,hd,r,t) =l= sum(ivrt(i,v,r,t)$(sameas(i,"Hydro")) XC(i,v,r,t) * vrsc_exi_d(sd,hd,i,r,t)) ;                                            
* Old nuclear is bounded by existing stuff
capacity_nucmin_old_d(sd,hd,ivrt(nuc(i),oldv(v),r,t))$(afmin_d(sd,hd,i,v,r,t) > 0)..
                 X_d(sd,hd,i,v,r,t) =g=  XC(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * afmin_d(sd,hd,i,v,r,t) ;
capacity_nucmax_old_d(sd,hd,ivrt(nuc(i),oldv(v),r,t))..
                 X_d(sd,hd,i,v,r,t) =l=  XC(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (afmax_d(sd,hd,i,v,r,t)-1)$afmax_d(sd,hd,i,v,r,t)) ;

* Old nuclear is must-run
capacity_nuc_old_d(sd,hd,ivrt(nuc(i),oldv(v),r,t))..
                 X_d(sd,hd,i,v,r,t) =l=  XC(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) ;
* New nuclear is not must-run
capacity_nuc_new_d(sd,hd,ivrt(nuc(i),newv(v),r,t))..
                 X_d(sd,hd,i,v,r,t) =l=  XC(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) ;
* Old bioenergy is bounded by existing stuff
capacity_biomin_old_d(sd,hd,ivrt_nochp(bio(i),oldv(v),r,t))$(afmin_d(sd,hd,i,v,r,t) > 0)..
                 X_d(sd,hd,i,v,r,t) =g=  XC(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * afmin_d(sd,hd,i,v,r,t) ;
capacity_biomax_old_d(sd,hd,ivrt_nochp(bio(i),oldv(v),r,toptimize(t)))..
                 X_d(sd,hd,i,v,r,t) =l=  XC(i,v,r,t) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (afmax_d(sd,hd,i,v,r,t)-1)$afmax_d(sd,hd,i,v,r,t)) ;
* Old bioenergy is must-run
capacity_bio_old_d(sd,hd,ivrt_nochp(bio(i),oldv(v),r,toptimize(t)))..
                 X_d(sd,hd,i,v,r,t) =l=  XC(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) ;
* New bioenergy is not must-run
capacity_bio_new_d(sd,hd,ivrt(bio(i),newv(v),r,t))..
                 X_d(sd,hd,i,v,r,t) =l=  XC(i,v,r,t) * (1 + (af_d(sd,hd,i,v,r,t)-1)$af_d(sd,hd,i,v,r,t)) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) ;

* * * * * Transmission equations
* * * Enforce capacity constraint on inter-region trade flows
tcapacity_d(sd,hd,tmapopt(k,r,rr,t))..
         E_D(sd,hd,k,r,rr,t) =l= TC(k,r,rr,t) ;

* * * * * Storage equations
* * * Storage charge-discharge and accumulation
* Charge must not exceed charge capacity (size of door - entry)
chargelim_d(sd,hd,
$if not  set storagebalnv    jvrt(j,v,r,t))$(not jres(j))..
$if      set storagebalnv    $if not  set hydrogensimple  jvrt(nonvj(j),v,r,t))..
$if      set storagebalnv    $if      set hydrogensimple  jvrt(gbat(j),v,r,t))..
         G_D(sd,hd,j,v,r,t)  =l= GC(j,v,r,t) ;
* Discharge must not exceed charge capacity (size of door - exit)
dischargelim_d(sd,hd,
$if not  set storagebalnv    jvrt(j,v,r,t))..
$if      set storagebalnv    $if not  set hydrogensimple  jvrt(nonvj(j),v,r,t))..
$if      set storagebalnv    $if      set hydrogensimple  jvrt(gbat(j),v,r,t))..
         GD_D(sd,hd,j,v,r,t) =l= GC(j,v,r,t) ;
         

chargelimc_d(sd,hd,jvrt(ghyd(j),v,r,t))..
         G_D(sd,hd,j,v,r,t)  =l= GCC(j,v,r,t) ;
* Discharge must not exceed charge capacity (size of door - exit)
dischargelimd_d(sd,hd,jvrt(ghyd(j),v,r,t))..
         GD_D(sd,hd,j,v,r,t) =l= GCD(j,v,r,t) ;

* Reservoir have some limitations that cannot get accessed by calibration (water shep, value of water, ..)
dischargelim_min_d(sd,hd,jvrt(jres(j),oldv(v),r,t))$(inflowtot_d(j,v,r,t) > 0 and gafmin_d(sd,hd,j,v,r,t) > 0)..
         GD_D(sd,hd,j,v,r,t) =g=  GC(j,v,r,t) * gafmin_d(sd,hd,j,v,r,t) ;
dischargelim_max_d(sd,hd,jvrt(jres(j),oldv(v),r,t))$(inflowtot_d(j,v,r,t) > 0 and gafmax_d(sd,hd,j,v,r,t) > 0)..
         GD_D(sd,hd,j,v,r,t) =l=  GC(j,v,r,t) * (1 + (gafmax_d(sd,hd,j,v,r,t)-1)$gafmax_d(sd,hd,j,v,r,t)) ;        
* Reservoir lim
reservoirlim_d(sd,hd,jvrt(j,v,r,t))..
         GB_D(sd,hd,j,v,r,t) =l= ghours(j,v,r) * GC(j,v,r,t) * 1e-3 ;
reservoirlim_dd(sd,jvrt(j,v,r,t))..
         GB_DD(sd,j,v,r,t) =l= ghours(j,v,r) * GC(j,v,r,t) * 1e-3 ;
reservoirlimnonv_d(sd,hd,
$if not  set hydrogensimple nonvj(j),r,t)..
$if      set hydrogensimple gbat(j),r,t)..
         GBNV_D(sd,hd,j,r,t) =l= sum(jvrt(j,newv(v),r,t), ghours(j,v,r) * GC(j,v,r,t)) * 1e-3 ;
reservoirlimnonv_dd(sd,
$if not  set hydrogensimple nonvj(j),r,t)..
$if      set hydrogensimple gbat(j),r,t)..
         GBNV_DD(sd,j,r,t) =l= sum(jvrt(j,newv(v),r,t), ghours(j,v,r) * GC(j,v,r,t)) * 1e-3 ;
* Reservoir lim
reservoirlimr_d(sd,hd,jvrt(ghyd(j),v,r,t))..
         GB_D(sd,hd,j,v,r,t) =l= GCR(j,v,r,t) * 1e-3 ;
reservoirlimr_dd(sd,jvrt(ghyd(j),v,r,t))..
         GB_DD(sd,j,v,r,t) =l= GCR(j,v,r,t) * 1e-3 ;
reservoirlimrnonv_d(sd,hd,ghyd(j),r,t)..
         GBNV_D(sd,hd,j,r,t) =l= sum(jvrt(j,newv(v),r,t), GCR(j,v,r,t)) * 1e-3 ;
reservoirlimrnonv_dd(sd,ghyd(j),r,t)..
         GBNV_DD(sd,j,r,t) =l= sum(jvrt(j,newv(v),r,t), GCR(j,v,r,t)) * 1e-3 ;   
      
* No vintage equations
chargelimnv_d(sd,hd,jpump(j),r,t)..
         GNV_D(sd,hd,j,r,t)  =l= GCNV(j,r,t) ;
dischargelimnv_d(sd,hd,jpump(j),r,t)..
         GDNV_D(sd,hd,j,r,t) =l= GCNV(j,r,t) ;
dischargelimnv_min_d(sd,hd,jres(j),r,t)$(inflowtot_nv_d(j,r,t) > 0 and gafminnv_d(sd,hd,j,r,t) > 0 and ghoursnv(j,r,t) > 4)..
         GDNV_D(sd,hd,j,r,t) =g=  GCNV(j,r,t) * gafminnv_d(sd,hd,j,r,t) ;
dischargelimnv_max_d(sd,hd,jres(j),r,t)$(inflowtot_nv_d(j,r,t) > 0 and gafmaxnv_d(sd,hd,j,r,t) > 0 and ghoursnv(j,r,t) > 4)..
         GDNV_D(sd,hd,j,r,t) =l=  GCNV(j,r,t) * (1 + (gafmaxnv_d(sd,hd,j,r,t)-1)$gafmaxnv_d(sd,hd,j,r,t)) ;
reservoirlimnv_d(sd,hd,nvj(j),r,t)..
         GBNV_D(sd,hd,j,r,t) =l= ghoursnv(j,r,t) * GCNV(j,r,t) * 1e-3 ;                 
reservoirlimnv_dd(sd,nvj(j),r,toptimize(t))..
         GBNV_DD(sd,j,r,t) =l= ghoursnv(j,r,t) * GCNV(j,r,t) * 1e-3 ;
  
* Vintage specific storage accumulation
storagebal_sdone_hdone(sdone(sd),hdone(hd),jvrt(j,v,r,t))..
        GB_D(sd,hd,j,v,r,t) =e= sum(sdnum(sdd), GB_DD(sdd,j,v,r,t)) * (1 - dischrg(j,v,r)) + (inflow_d(sd,hd,j,v,r,t) + G_D(sd,hd,j,v,r,t) * chrgpen(j,v,r)  - GD_D(sd,hd,j,v,r,t)) * 1e-3 ;
storagebal_sdtwo_hdone(sdtwo(sd),hdone(hd),jvrt(j,v,r,t))..
        GB_D(sd,hd,j,v,r,t) =e= GB_DD(sd-1,j,v,r,t) * (1 - dischrg(j,v,r)) + (inflow_d(sd,hd,j,v,r,t) + G_D(sd,hd,j,v,r,t) * chrgpen(j,v,r)  - GD_D(sd,hd,j,v,r,t)) * 1e-3 ;
storagebal_hdtwo(sd,hdtwo(hd),jvrt(j,v,r,t))..
        GB_D(sd,hd,j,v,r,t) =e= GB_D(sd,hd-1,j,v,r,t) * (1 - dischrg(j,v,r)) + (inflow_d(sd,hd,j,v,r,t) + G_D(sd,hd,j,v,r,t) * chrgpen(j,v,r)  - GD_D(sd,hd,j,v,r,t)) * 1e-3 ;
storagebal_sdone(sdone(sd),jvrt(j,v,r,t))..
        GB_DD(sd,j,v,r,t) =e= sum(sdnum(sdd), GB_DD(sdd,j,v,r,t)) * (1 - dischrg(j,v,r)) + days(sd) * sum(hd, inflow_d(sd,hd,j,v,r,t) + G_D(sd,hd,j,v,r,t) * chrgpen(j,v,r)  - GD_D(sd,hd,j,v,r,t)) * 1e-3 ;
storagebal_sdtwo(sdtwo(sd),jvrt(j,v,r,t))..
        GB_DD(sd,j,v,r,t) =e= GB_DD(sd-1,j,v,r,t) * (1 - dischrg(j,v,r)) + days(sd) * sum(hd, inflow_d(sd,hd,j,v,r,t) + G_D(sd,hd,j,v,r,t) * chrgpen(j,v,r)  - GD_D(sd,hd,j,v,r,t)) * 1e-3 ; 
storagebal_ann_d(jvrt(j,v,r,t))..
        sum(sd, days(sd) * sum(hd, inflow_d(sd,hd,j,v,r,t) + G_D(sd,hd,j,v,r,t) * chrgpen(j,v,r) - GD_D(sd,hd,j,v,r,t))) * 1e-3 =g= 0 ;

* No vintage specific storage accumulation with vintage specific charge and discharge (new vintages)
storagebalnv_sdone_hdone(sdone(sd),hdone(hd),nonvj(j),r,t)..
        GBNV_D(sd,hd,j,r,t) =l= sum(sdnum(sdd), GBNV_DD(sdd,j,r,t)) * (1 - dischrgnv(j,r)) + (HIMP(j,r,t) - HRES(j,r,t))/8760 + (sum(jvrt(j,newv(v),r,t), G_D(sd,hd,j,v,r,t) * chrgpen(j,v,r)  - GD_D(sd,hd,j,v,r,t))) * 1e-3 ;
storagebalnv_sdtwo_hdone(sdtwo(sd),hdone(hd),nonvj(j),r,t)..
        GBNV_D(sd,hd,j,r,t) =l= GBNV_DD(sd-1,j,r,t) * (1 - dischrgnv(j,r)) + (HIMP(j,r,t) - HRES(j,r,t))/8760 + (sum(jvrt(j,newv(v),r,t), G_D(sd,hd,j,v,r,t) * chrgpen(j,v,r)  - GD_D(sd,hd,j,v,r,t))) * 1e-3 ;
storagebalnv_hdtwo(sd,hdtwo(hd),nonvj(j),r,t)..
        GBNV_D(sd,hd,j,r,t) =l= GBNV_D(sd,hd-1,j,r,t) * (1 - dischrgnv(j,r)) + (HIMP(j,r,t) - HRES(j,r,t))/8760 + (sum(jvrt(j,newv(v),r,t), G_D(sd,hd,j,v,r,t) * chrgpen(j,v,r)  - GD_D(sd,hd,j,v,r,t))) * 1e-3 ;
storagebalnv_sdone(sdone(sd),nonvj(j),r,t)..
        GBNV_DD(sd,j,r,t) =l= sum(sdnum(sdd), GBNV_DD(sdd,j,r,t)) * (1 - dischrgnv(j,r)) + days(sd) * sum(hd, + (HIMP(j,r,t) - HRES(j,r,t))/8760) + days(sd) * sum(hd, sum(jvrt(j,newv(v),r,t), G_D(sd,hd,j,v,r,t) * chrgpen(j,v,r)  - GD_D(sd,hd,j,v,r,t))) * 1e-3 ;
storagebalnv_sdtwo(sdtwo(sd),nonvj(j),r,t)..
        GBNV_DD(sd,j,r,t) =l= GBNV_DD(sd-1,j,r,t) * (1 - dischrgnv(j,r)) + days(sd) * sum(hd, + (HIMP(j,r,t) - HRES(j,r,t))/8760) + days(sd) * sum(hd, sum(jvrt(j,newv(v),r,t), G_D(sd,hd,j,v,r,t) * chrgpen(j,v,r)  - GD_D(sd,hd,j,v,r,t))) * 1e-3  ;
storagebalnv_ann_d(nonvj(j),r,t)..
        HIMP(j,r,t) - HRES(j,r,t) + sum(sd, days(sd) * sum(hd, sum(jvrt(j,newv(v),r,t), G_D(sd,hd,j,v,r,t) * chrgpen(j,v,r) - GD_D(sd,hd,j,v,r,t)))) * 1e-3 =g= 0 ;

* No vintage specific storage accumulation with vintage specific charge and discharge (new vintages)
storagebalnv_old_sdone_hdone(sdone(sd),hdone(hd),nvj(j),r,t)..
        GBNV_D(sd,hd,j,r,t) =l= sum(sdnum(sdd), GBNV_DD(sdd,j,r,t)) * (1 - dischrgnv(j,r)) + (inflow_nv_d(sd,hd,j,r,t) + GNV_D(sd,hd,j,r,t) * chrgpennv(j,r)  - GDNV_D(sd,hd,j,r,t)) * 1e-3 ;
storagebalnv_old_sdtwo_hdone(sdtwo(sd),hdone(hd),nvj(j),r,t)..
        GBNV_D(sd,hd,j,r,t) =l= GBNV_DD(sd-1,j,r,t) * (1 - dischrgnv(j,r)) + (inflow_nv_d(sd,hd,j,r,t) + GNV_D(sd,hd,j,r,t) * chrgpennv(j,r)  - GDNV_D(sd,hd,j,r,t)) * 1e-3 ;
storagebalnv_old_hdtwo(sd,hdtwo(hd),nvj(j),r,t)..
        GBNV_D(sd,hd,j,r,t) =l= GBNV_D(sd,hd-1,j,r,t) * (1 - dischrgnv(j,r)) + (inflow_nv_d(sd,hd,j,r,t) + GNV_D(sd,hd,j,r,t) * chrgpennv(j,r)  - GDNV_D(sd,hd,j,r,t)) * 1e-3 ;
storagebalnv_old_sdone(sdone(sd),nvj(j),r,t)..
        GBNV_DD(sd,j,r,t) =l= sum(sdnum(sdd), GBNV_DD(sdd,j,r,t)) * (1 - dischrgnv(j,r)) + days(sd) * sum(hd, inflow_nv_d(sd,hd,j,r,t) + GNV_D(sd,hd,j,r,t) * chrgpennv(j,r)  - GDNV_D(sd,hd,j,r,t)) * 1e-3 ;
storagebalnv_old_sdtwo(sdtwo(sd),nvj(j),r,t)..
        GBNV_DD(sd,j,r,t) =l= GBNV_DD(sd-1,j,r,t) * (1 - dischrgnv(j,r)) + days(sd) * sum(hd, inflow_nv_d(sd,hd,j,r,t) + GNV_D(sd,hd,j,r,t) * chrgpennv(j,r)  - GDNV_D(sd,hd,j,r,t)) * 1e-3  ;
storagebalnv_old_ann_d(nvj(j),r,t)..
        sum(sd, days(sd) * sum(hd, inflow_nv_d(sd,hd,j,r,t) + GNV_D(sd,hd,j,r,t) * chrgpennv(j,r) - GDNV_D(sd,hd,j,r,t))) * 1e-3 =g= 0 ;
      
* Fix reservoir charge to zero
$if not  set storagebalnv    G_D.FX(sd,hd,"Reservoir",v,r,t) = 0 ;
$if      set storagebalnv    GNV_D.FX(sd,hd,"Reservoir",r,t) = 0 ;

* Days
copyxc_sd(sd,ivrt(i,v,r,t))..                   XCS_SD(sd,i,v,r,t)    =e= XC(i,v,r,t)$(ord(sd) eq 1) + XCS_SD(sd-1,i,v,r,t)$(ord(sd) > 1) ;
copyxc_d(sd,hd,ivrt(i,v,r,t))..                 XCS_D(sd,hd,i,v,r,t) =e= XCS_SD(sd,i,v,r,t)$(ord(hd) eq 1) + XCS_D(sd,hd,i,v,r,t)$(ord(hd) > 1) ;
copygc_sd(sd,
$if not  set storagebalnv jvrt(j,v,r,t)
$if      set storagebalnv jvrt(nonvj(j),v,r,t)
                         )..                    GCS_SD(sd,j,v,r,t)  =e= GC(j,v,r,t)$(ord(sd) eq 1)  + GCS_SD(sd,j,v,r,t)$(ord(sd) > 1) ;                         
copygc_d(sd,hd,
$if not  set storagebalnv jvrt(j,v,r,t)
$if      set storagebalnv jvrt(nonvj(j),v,r,t)
                         )..                    GCS_D(sd,hd,j,v,r,t)  =e= GCS_SD(sd,j,v,r,t)$(ord(hd) eq 1)  + GCS_D(sd,hd,j,v,r,t)$(ord(hd) > 1) ;
                         


copygcnv_sd(sd,nvj(j),r,toptimize(t))..         GCSNV_SD(sd,j,r,t)  =e= GCNV(j,r,t)$(ord(sd) eq 1)  + GCSNV_SD(sd,j,r,t)$(ord(sd) > 1) ;
copygcnv_d(sd,hd,nvj(j),r,toptimize(t))..       GCNV(j,r,t)  =e= GCSNV_SD(sd,j,r,t)$(ord(hd) eq 1)  + GCNV(j,r,t)$(ord(hd) > 1) ;

copytc_sd(sd,tmapopt(k,r,rr,t))..               TCS_SD(sd,k,r,rr,t) =e= TC(k,r,rr,t)$(ord(sd) eq 1) + TCS_SD(sd,k,r,rr,t)$(ord(sd) > 1) ;
copytc_d(sd,hd,tmapopt(k,r,rr,t))..             TCS_D(sd,hd,k,r,rr,t) =e= TCS_SD(sd,k,r,rr,t)$(ord(hd) eq 1) + TCS_D(sd,hd,k,r,rr,t)$(ord(hd) > 1) ;