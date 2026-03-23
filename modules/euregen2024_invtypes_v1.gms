set
pub(inv) /pub/
pri(inv) /pri/
;

Parameter
invbudget(inv,r,t)
;

invbudget(pub(inv),r,t) = 100000 * daref(r,t) / daref("Germany",t)  ;

Positive Variable
IXINV(inv,i,r,t)
IGINV(inv,j,r,t)
IGCINV(inv,j,r,t)
IGDINV(inv,j,r,t)
IGRINV(inv,j,r,t)
ITINV(inv,k,r,r,t)
;

Equations
ix_investinv(i,r,t)
ig_investinv(j,r,t)
igc_investinv(j,r,t)
igd_investinv(j,r,t)
igr_investinv(j,r,t)
it_investinv(k,r,r,t)
investbudget(inv,r,t)
;

ix_investinv(new(i),r,toptimize(t))..
    IX(i,r,t) =e= sum(inv, IXINV(inv,i,r,t)) ;

ig_investinv(newj(j),r,toptimize(t))..
    IG(j,r,t) =e= sum(inv, IGINV(inv,j,r,t)) ;

igc_investinv(ghyd(j),r,toptimize(t))..
    IGC(j,r,t) =e= sum(inv, IGCINV(inv,j,r,t)) ;
    
igd_investinv(ghyd(j),r,toptimize(t))..
    IGD(j,r,t) =e= sum(inv, IGDINV(inv,j,r,t)) ;
    
igr_investinv(ghyd(j),r,toptimize(t))..
    IGR(j,r,t) =e= sum(inv, IGRINV(inv,j,r,t)) ;
    
it_investinv(k,r,rr,t)..
    IT(k,r,rr,t) =e= sum(inv, ITINV(inv,k,r,rr,t)) ;
    
investbudget(pub(inv),r,t)..
      sum(new(i),  sum(tv(t,v),  capcost(i,v,r)) * IXINV(inv,i,r,t))
    + sum(newj(j), sum(tv(t,v), gcapcost(j,v,r)) * IGINV(inv,j,r,t))
    + sum(ghyd(j), sum(tv(t,v), gcapcostc(j,v,r)) * IGCINV(inv,j,r,t))   
    + sum(ghyd(j), sum(tv(t,v), gcapcostd(j,v,r)) * IGDINV(inv,j,r,t))
    + sum(ghyd(j), sum(tv(t,v), gcapcostr(j,v,r)) * IGRINV(inv,j,r,t))
    + sum(tmap(k,r,rr), tcapcost(k,r,rr) * ITINV(inv,k,r,rr,t))
    =l= invbudget(inv,r,t) ;
    
Positive variable
INVCOSTMIXEDINV
;

Equations
investcost_mixed_inv
;
*shortcut zu excel 

investcost_mixed_inv..
*        Surplus is defined in million EUR
         INVCOSTMIXEDINV =e=
*        Sum over all time period t
                !! begin period sum
                sum(toptimize(t),
*               Sum over all regions r
                !! begin region sum
                sum(r, 
*               INVESTMENT COST
                !! begin investment cost
                        sum(new(i),       sum(inv, IXINV(inv,i,r,t)    * sum(ivrttv(i,v,r,t), (capcost(i,v,r) + deccost(i,v,r)) *  zeta(inv,i,v,r)))) +
                        sum(newj(j),      sum(inv, IGINV(inv,j,r,t)    * sum(jvrttv(j,v,r,t), gcapcost(j,v,r)                   * gzeta(inv,j,v,r)))) +
                        sum(ghyd(j),       sum(inv, IGCINV(inv,j,r,t)   * sum(jvrttv(j,v,r,t), gcapcostc(j,v,r)                  * gzeta(inv,j,v,r)))) +
                        sum(ghyd(j),       sum(inv, IGDINV(inv,j,r,t)   * sum(jvrttv(j,v,r,t), gcapcostd(j,v,r)                  * gzeta(inv,j,v,r)))) +
                        sum(ghyd(j),       sum(inv, IGRINV(inv,j,r,t)   * sum(jvrttv(j,v,r,t), gcapcostr(j,v,r)                  * gzeta(inv,j,v,r)))) +
                        sum(tmap(k,r,rr), sum(inv, ITINV(inv,k,r,rr,t) * sum(tvrttv(k,v,r,t), tcapcost(k,r,rr)                  * tzeta(inv,k,v,r)))) 
               !! end investment cost
               !! end region sum
               )
               !! end period sum
               ) ;
    