parameter
eq0(r,t) equity base
eqold(r,t) existing equity to refill
dbold(r,t) existing debt to repay

reold(r,t) refill from existing capacity
rpold(r,t) repayment from existing capacity

eqshare(r) equity share of existing capital
dbshare(r) debt share of existing capital (or 1 - eqshare)
sigmamax(r,t) maximum debt-to-equity ratio for credit constraint

nyrsinstall(v)
nyrsv(v)

omegaacti(i,v,r,t) binary active or not
omegaactj(j,v,r,t)
*omegaactk(k,v,r,t)

omegalefi(i,v,r,t) fraction of repayment
omegalefj(j,v,r,t)
*omegalefk(k,v,r,t)

omegalini(i,v,r,t) linear repayment
omegalinj(j,v,r,t)
*omegalink(k,v,r,t)

a risk free interest rate without debt
b(r,t) linear increase in debt over equity

cap(i,v,r)
gcap(j,v,r)

rho /0.07/
tau /0.35/
vval(v)
;

vval(v) = v.val ;


cap(i,v,r)$(v.val le 2020) = capt(i,v,r,"2022") ;
gcap(j,v,r)$(v.val le 2020) = gcapt(j,v,r,"2022") ;

cap(i,"2021",r) = capt(i,"2021",r,"2022") ;
gcap(j,"2021",r) = gcapt(j,"2021",r,"2022") ;

cap(i,"2022",r) = capt(i,"2022",r,"2022") ;
gcap(j,"2022",r) = gcapt(j,"2022",r,"2022") ;

cap(i,"2023",r) = capt(i,"2023",r,"2023") ;
gcap(j,"2023",r) = gcapt(j,"2023",r,"2023") ;

a = 0.06 ;

nyrsinstall("1960") = 4 ;
nyrsinstall(v)$(v.val ge 1965) = vval(v) - vval(v-1) - 1;

nyrsv(v) = 5 ;
nyrsv(v)$(v.val ge 1965) = vval(v) - vval(v-1) ;

omegaacti(ivrt(i,v,r,t))$( invdepr(i,v,r) > 0 and t.val le v.val - nyrsinstall(v) +  invdepr(i,v,r) - 1) = 1 ; 
omegaactj(jvrt(j,v,r,t))$(ginvdepr(j,v,r) > 0 and t.val le v.val - nyrsinstall(v) + ginvdepr(j,v,r) - 1) = 1 ;

omegalefi(ivrt(i,v,r,t))$( invdepr(i,v,r) > 0 and t.val le v.val - nyrsinstall(v) +  invdepr(i,v,r) - 1) = (v.val - nyrsinstall(v) +  invdepr(i,v,r) - t.val) /  invdepr(i,v,r) ;
omegalefj(jvrt(j,v,r,t))$(ginvdepr(j,v,r) > 0 and t.val le v.val - nyrsinstall(v) + ginvdepr(j,v,r) - 1) = (v.val - nyrsinstall(v) + ginvdepr(j,v,r) - t.val) / ginvdepr(j,v,r) ;
omegalini(ivrt(i,v,r,t))$( invdepr(i,v,r) > 0 and t.val le v.val - nyrsinstall(v) +  invdepr(i,v,r) - 1) = nyrs(t) /  invdepr(i,v,r) ;
omegalinj(jvrt(j,v,r,t))$(ginvdepr(j,v,r) > 0 and t.val le v.val - nyrsinstall(v) + ginvdepr(j,v,r) - 1) = nyrs(t) / ginvdepr(j,v,r) ;

eqshare(r) = 0.05 ;
dbshare(r) = 1 - eqshare(r) ;

eqold(r,t) = sum(ivrt(i,v,r,t)$(v.val le 2021), omegalefi(i,v,r,t) * eqshare(r) * cap(i,v,r) * capcost(i,v,r)) + sum(jvrt(j,v,r,t)$(v.val le 2021), omegalefj(j,v,r,t) * eqshare(r) * gcap(j,v,r) * gcapcost(j,v,r)) ;
* + sum(kvrt(k,v,r,t), omegak(k,v,r,t) * eqshare(i,v,r) * sum(rr, tcap(k,v,r,rr))
dbold(r,t) = sum(ivrt(i,v,r,t)$(v.val le 2021), omegalefi(i,v,r,t) * dbshare(r) * cap(i,v,r) * capcost(i,v,r)) + sum(jvrt(j,v,r,t)$(v.val le 2021), omegalefj(j,v,r,t) * dbshare(r) * gcap(j,v,r) * gcapcost(j,v,r)) ;

reold(r,t) = sum(ivrt(i,v,r,t)$(v.val le 2021), omegalini(i,v,r,t) * eqshare(r) * cap(i,v,r) * capcost(i,v,r)) + sum(jvrt(j,v,r,t)$(v.val le 2021), omegalinj(j,v,r,t) * eqshare(r) * gcap(j,v,r) * gcapcost(j,v,r)) ;
* + sum(kvrt(k,v,r,t), omegak(k,v,r,t) * eqshare(i,v,r) * sum(rr, tcap(k,v,r,rr))
rpold(r,t) = sum(ivrt(i,v,r,t)$(v.val le 2021), omegalini(i,v,r,t) * dbshare(r) * cap(i,v,r) * capcost(i,v,r)) + sum(jvrt(j,v,r,t)$(v.val le 2021), omegalinj(j,v,r,t) * dbshare(r) * gcap(j,v,r) * gcapcost(j,v,r)) ;

eq0(r,t)$(t.val le 2023) =   sum(ivrt(i,v,r,t), omegalefi(i,v,r,t) * eqshare(r) * cap(i,v,r) * capcost(i,v,r)) + sum(jvrt(j,v,r,t),                 omegalefj(j,v,r,t) * eqshare(r) * gcap(j,v,r) * gcapcost(j,v,r)) ;
eq0(r,t)$(t.val ge 2024) = eq0(r,"2023") ;

b(r,t)$(eq0(r,t) > 0) = - 0.05 / 100 / eq0(r,t) ;
$if      set bup   b(r,t)$(eq0(r,t) > 0) = + 0.06 / 100 / eq0(r,t) ; 

sigmamax(r,t) = 100 ;

positive variable
EQ(r,t) equity
DB(r,t) debt
*IN(r,t) interest rate
SIGMA(r,t) debt-to-equity ratio

DBI(i,v,r) generation technology-specific debt use
DBJ(j,v,r) storage technology-specific debt use
*DBK(k,v,r) transmission technology-specific debt use

EQI(i,v,r) generation technology-specific equity use
EQJ(j,v,r) storage technology-specific equity use
*EQK(k,v,r) transmission technology-specific equity use
RE(r,t)
RP(r,t)
;

variable
INVCOSTENDO
;

equation
con_eq0(r,t) equity constraint
con_dbmax(r,t) debt constraint
con_eq(r,t) equity state constraint
con_db(r,t) debt state constraint
con_re(r,t) refill constraint
con_rp(r,t) repayment constraint

con_investi(i,v,r) debt plus equity equal to investment sum
con_investj(j,v,r) debt plus equity equal to investment sum
investcost_endo endogenous investment cost
;



con_eq(r,t)..
    EQ(r,t) =e= eqold(r,t) + sum(ivrt(i,v,r,t), omegalefi(i,v,r,t) * EQI(i,v,r)) + sum(jvrt(j,v,r,t), omegalefj(j,v,r,t) * EQJ(j,v,r)) ;
* + sum(ivrt(i,v,r,t), omegak(k,v,r,t) * EQK(i,v,r)) ;

con_db(r,t)..
    DB(r,t) =e= dbold(r,t) + sum(ivrt(i,v,r,t), omegalefi(i,v,r,t) * DBI(i,v,r)) + sum(jvrt(j,v,r,t), omegalefj(j,v,r,t) * DBJ(j,v,r)) ;

con_re(r,t)..
    RE(r,t) =e= reold(r,t) + sum(ivrt(i,v,r,t), omegalini(i,v,r,t) * EQI(i,v,r)) + sum(jvrt(j,v,r,t), omegalinj(j,v,r,t) * EQJ(j,v,r)) ;

con_rp(r,t)..
    RP(r,t) =e= rpold(r,t) + sum(ivrt(i,v,r,t), omegalini(i,v,r,t) * DBI(i,v,r)) + sum(jvrt(j,v,r,t), omegalinj(j,v,r,t) * DBJ(j,v,r)) ;
    
con_eq0(r,t)..
    EQ(r,t) =l= eq0(r,t) ;
    
con_dbmax(r,t)..
    DB(r,t) =l= sigmamax(r,t) * eq0(r,t) ;

con_investi(i,v,r)..
    EQI(i,v,r) + DBI(i,v,r) =g= sum(tv(t,v), IX(i,r,t)) * capcost(i,v,r) ;

con_investj(j,v,r)..
    EQJ(j,v,r) + DBJ(j,v,r) =g= sum(tv(t,v), IG(j,r,t)) * gcapcost(j,v,r) ;
    
* * * Investment cost definition
investcost_endo..
*        Surplus is defined in million EUR
         INVCOSTENDO =e=
*        Sum over all time period t
                !! begin period sum
                sum(toptimize(t),
*               Sum over all regions r
                !! begin region sum
                sum(r, 
*               INVESTMENT COST
                !! begin investment cost
                        dfact(t) / nyrs(t) * 
$if not  set bzero      rho * (1 - tau) * eq0(r,t) + RE(r,t) + a * DB(r,t) + b(r,t) * DB(r,t) * DB(r,t) + RP(r,t) +
$if      set bzero      rho * (1 - tau) * eq0(r,t) + RE(r,t) + a * DB(r,t) + RP(r,t) +
                        sum(tmap(k,r,rr), sum(inv, tshare(inv,k,r) * IT(k,r,rr,t) * sum(tvrttv(k,v,r,t), tcapcost(k,r,rr)                  * tzeta(inv,k,v,r)))) 
               !! end investment cost
               !! end region sum
               )
               !! end period sum
               ) ;