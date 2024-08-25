* * * Minimum dispatch module (minimum dispatch of all technologies, this condition is deactivitated if not "techmin=yes")
SET
techmini(i)
;

$gdxin precal\precal_%n%.gdx
$load techmini
$gdxin

PARAMETER
stacost(i,v,r)      Start-up cost (EUR per MWh)
;

$gdxin precal\precal_%n%.gdx
$load stacost
$gdxin

BINARY VARIABLE
ONOFF(s,i,v,r,t)        Binary variable that indicates whether vintage is completely out (=0) or on (=1)    
;

POSITIVE VARIABLE
NUMONOFF(s,i,v,r,t)       Number of on off times
;

EQUATION
capacity_techmin(s,i,v,r,t)     Minimum dispatch of other capacity
eq_numonofflo(s,i,v,r,t)        Equation that determines whether vintage started or not
eq_numonoffup(s,i,v,r,t)        Equation that determines whether vintage started or not
;

* Minimum dispatch of all technologies (this condition is deactivitated if not "techmin=yes"
capacity_techmin(s,ivrt(techmini(i),v,r,toptimize(t)))$(mindisp(i,v,r) > 0)..
                 X(s,i,v,r,t) =g=  ONOFF(s,i,v,r,t) * XCS(s,i,v,r,t) * mindisp(i,v,r) * (1 + (reliability(i,v,r)-1)$reliability(i,v,r)) * (1 + (af(s,i,v,r,t)-1)$af(s,i,v,r,t)) * (1 + (vrsc(s,i,v,r)-1)$vrsc(s,i,v,r)) ;                

* Collecting number of starts
eq_numonofflo(stwo(s),ivrt(techmini(i),v,r,toptimize(t)))$(mindisp(i,v,r) > 0)..
                 NUMONOFF(s,i,v,r,t) =g= ONOFF(s,i,v,r,t) - ONOFF(s-1,i,v,r,t) ;
                 
eq_numonoffup(stwo(s),ivrt(techmini(i),v,r,toptimize(t)))$(mindisp(i,v,r) > 0)..
                 NUMONOFF(s,i,v,r,t) =l= 1 ;                

$if      set ramping    ONOFF.L(s,ivrt(techmini(i),v,r,t)) = 1 ;
$if      set ramcost    NUMONOFF.L(s,ivrt(techmini(i),v,r,t)) = 0 ;

* * * Ramping module (ramping up and down ability for a set of technologies, this condition is deactiviated if not "ramping=yes")
SET
ram(i)          Technologies with ramping constraints 
;

$gdxin precal\precal_%n%.gdx
$load ram
$gdxin

PARAMETER
ramrate(i,v,r)  ramping quota (% per hour)
ramcost(i,v,r)  ramping cost (EUR per MW(h))
effloss(i,v,r)  efficiency loss from ramping (% points)
;

$gdxin precal\precal_%n%.gdx
$load ramrate,ramcost,effloss
$gdxin

POSITIVE VARIABLE
RPNEG(s,i,v,r,t)                    Amount of ramrate (GW)
RPPOS(s,i,v,r,t)                    Amount of ramrate (GW)
;

EQUATION
capacity_ramrateneg(s,i,v,r,t)      Calculates the amount of ramrate
capacity_ramratepos(s,i,v,r,t)      Calculates the amount of ramrate
capacity_rampdown(s,i,v,r,t)        Constrains ramrate down
capacity_rampup(s,i,v,r,t)          Constrains ramrate up
;

capacity_ramrateneg(stwo(s),ivrt(ram(i),v,r,toptimize(t)))$(ramrate(i,v,r))..
                 RPNEG(s,i,v,r,t) =g= X(s,i,v,r,t) - X(s-1,i,v,r,t) ;

capacity_ramratepos(stwo(s),ivrt(ram(i),v,r,toptimize(t)))$(ramrate(i,v,r))..
                 RPPOS(s,i,v,r,t) =g= X(s-1,i,v,r,t) - X(s,i,v,r,t) ;
                 
capacity_rampdown(stwo(s),ivrt(ram(i),v,r,toptimize(t)))$(ramrate(i,v,r))..
                 RPPOS(s,i,v,r,t) =l=  ramrate(i,v,r) * XCS(s,i,v,r,t) ;
                 
capacity_rampup(stwo(s),ivrt(ram(i),v,r,toptimize(t)))$(ramrate(i,v,r))..
                 RPNEG(s,i,v,r,t) =l=  ramrate(i,v,r) * XCS(s,i,v,r,t) ;v,r,t) ;