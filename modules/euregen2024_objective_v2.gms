Positive variable
INVCOSTMIXED
GINVCOSTMIXED
TINVCOSTMIXED
INVCOSTOLD
GINVCOSTOLD
TINVCOSTOLD
INVCOSTMIXEDLEA
INVCOSTOLDLEA
;

Equations
investcost_mixed
ginvestcost_mixed
tinvestcost_mixed
investcost_old
ginvestcost_old
tinvestcost_old
investcost_mixed_learning
investcost_old_learning
;

* * * Investment cost definition
investcost_mixed..
*        Surplus is defined in million EUR
         INVCOSTMIXED =e=
*        Sum over all time period t
                !! begin period sum
                sum(toptimize(t),
*               Sum over all regions r
                !! begin region sum
                sum(r, 
*               INVESTMENT COST
                !! begin investment cost
                        sum(new(i),       sum(inv,  share(inv,i,r) * IX(i,r,t)    * sum(ivrttv(i,v,r,t), (capcost(i,v,r) + deccost(i,v,r)) *  zeta(inv,i,v,r)))) +
                        sum(newj(j),      sum(inv, gshare(inv,j,r) * IG(j,r,t)    * sum(jvrttv(j,v,r,t), gcapcost(j,v,r)                   * gzeta(inv,j,v,r)))) +
                        sum(ghyd(j),       sum(inv, gshare(inv,j,r) * IGC(j,r,t)   * sum(jvrttv(j,v,r,t), gcapcostc(j,v,r)                  * gzeta(inv,j,v,r)))) +
                        sum(ghyd(j),       sum(inv, gshare(inv,j,r) * IGD(j,r,t)   * sum(jvrttv(j,v,r,t), gcapcostd(j,v,r)                  * gzeta(inv,j,v,r)))) +
                        sum(ghyd(j),       sum(inv, gshare(inv,j,r) * IGR(j,r,t)   * sum(jvrttv(j,v,r,t), gcapcostr(j,v,r)                  * gzeta(inv,j,v,r)))) +
                        sum(tmap(k,r,rr), sum(inv, tshare(inv,k,r) * IT(k,r,rr,t) * sum(tvrttv(k,v,r,t), tcapcost(k,r,rr)                  * tzeta(inv,k,v,r)))) 
               !! end investment cost
               !! end region sum
               )
               !! end period sum
               ) ;
               
ginvestcost_mixed..
*        Surplus is defined in million EUR
         GINVCOSTMIXED =e=
*        Sum over all time period t
                !! begin period sum
                sum(toptimize(t),
*               Sum over all regions r
                !! begin region sum
                sum(r, 
*               INVESTMENT COST
                !! begin investment cost
                        sum(newj(j),      sum(inv, gshare(inv,j,r) * IG(j,r,t)    * sum(jvrttv(j,v,r,t), gcapcost(j,v,r)                   * gzeta(inv,j,v,r)))) +
                        sum(ghyd(j),       sum(inv, gshare(inv,j,r) * IGC(j,r,t)   * sum(jvrttv(j,v,r,t), gcapcostc(j,v,r)                  * gzeta(inv,j,v,r)))) +
                        sum(ghyd(j),       sum(inv, gshare(inv,j,r) * IGD(j,r,t)   * sum(jvrttv(j,v,r,t), gcapcostd(j,v,r)                  * gzeta(inv,j,v,r)))) +
                        sum(ghyd(j),       sum(inv, gshare(inv,j,r) * IGR(j,r,t)   * sum(jvrttv(j,v,r,t), gcapcostr(j,v,r)                  * gzeta(inv,j,v,r)))) 
               !! end investment cost
               !! end region sum
               )
               !! end period sum
               ) ;
             

tinvestcost_mixed..
*        Surplus is defined in million EUR
         TINVCOSTMIXED =e=
*        Sum over all time period t
                !! begin period sum
                sum(toptimize(t),
*               Sum over all regions r
                !! begin region sum
                sum(r, 
*               INVESTMENT COST
                !! begin investment cost
                        sum(tmap(k,r,rr), sum(inv, tshare(inv,k,r) * IT(k,r,rr,t) * sum(tvrttv(k,v,r,t), tcapcost(k,r,rr)                  * tzeta(inv,k,v,r))))
               !! end investment cost
               !! end region sum
               )
               !! end period sum
               ) ;
                               
investcost_old..
*        Surplus is defined in million EUR
         INVCOSTOLD =e=
*        Sum over all time period t
                !! begin period sum
                sum(toptimize(t),             
                !! begin discounting
                dfact(t) * (
*               Sum over all regions r
                !! begin region sum
                sum(r, 
*               INVESTMENT COST
*               Old, excluding discouting via normal annui, and ccost)                
                !! begin investment cost (old)
*               We need nyrs because investments happens once only but production nyrs-times)
                1 / nyrs(t)  * (1 
*                                   Normal investor consider total investment cost in the period of investment
$if      set normal    + sum(new(i),       IX(i,r,t)    * sum(ivrttv(i,v,r,t), (capcost(i,v,r) + deccost(i,v,r)) *  endeffect(i,v,r,t)))
$if      set normal    + sum(newj(j),      IG(j,r,t)    * sum(jvrttv(j,v,r,t), gcapcost(j,v,r)                   * gendeffect(j,v,r,t)))
$if      set normal    + sum(ghyd(j),       IGC(j,r,t)   * sum(jvrttv(j,v,r,t), gcapcostc(j,v,r)                  * gendeffect(j,v,r,t)))
$if      set normal    + sum(ghyd(j),       IGD(j,r,t)   * sum(jvrttv(j,v,r,t), gcapcostd(j,v,r)                  * gendeffect(j,v,r,t)))
$if      set normal    + sum(ghyd(j),       IGR(j,r,t)   * sum(jvrttv(j,v,r,t), gcapcostr(j,v,r)                  * gendeffect(j,v,r,t)))
$if      set normal    + sum(tmap(k,r,rr), IT(k,r,rr,t) * sum(tvrttv(k,v,r,t), tcapcost(k,r,rr)                  * tendeffect(k,v,r,t)))
*                                   Investment costs follow from annuities (%) of borrowed capital (EUR/kW * GW)
$if      set annui     + sum(new(i),       sum(ivrttv(i,v,r,tt)$(tt.val le t.val), IX(i,r,tt)   * (capcost(i,v,r) + deccost(i,v,r)) *  deprtime(i,v,r,tt) * annuity_ir(i,v,r) * nyrs(t)))
$if      set annui     + sum(newj(j),      sum(jvrttv(j,v,r,tt)$(tt.val le t.val), IG(j,r,tt)   * gcapcost(j,v,r)                   * gdeprtime(j,v,r,tt) * annuity_jr(j,v,r) * nyrs(t)))
$if      set annui     + sum(ghyd(j),      sum(jvrttv(j,v,r,tt)$(tt.val le t.val), IGC(j,r,tt)  * gcapcostc(j,v,r)                  * gdeprtime(j,v,r,tt) * annuity_jr(j,v,r) * nyrs(t)))
$if      set annui     + sum(ghyd(j),      sum(jvrttv(j,v,r,tt)$(tt.val le t.val), IGD(j,r,tt)  * gcapcostd(j,v,r)                  * gdeprtime(j,v,r,tt) * annuity_jr(j,v,r) * nyrs(t)))
$if      set annui     + sum(ghyd(j),      sum(jvrttv(j,v,r,tt)$(tt.val le t.val), IGR(j,r,tt)  * gcapcostr(j,v,r)                  * gdeprtime(j,v,r,tt) * annuity_jr(j,v,r) * nyrs(t)))
$if      set annui     + sum(tmap(k,r,rr), sum(tvrttv(k,v,r,tt)$(tt.val le t.val), IT(k,r,rr,t) * tcapcost(k,r,rr)                  * tdeprtime(k,v,r,tt) * annuity_kr(k,v,r)   * nyrs(t)))
*                                   Investment costs follow from WACC (%) of capital stock (EUR/kW * GW)
$if      set ccost     + sum(new(i),       sum(ivrttv(i,v,r,tt)$(tt.val le t.val), IX(i,r,tt)   * (capcost(i,v,r) + deccost(i,v,r)) *  deprtime(i,v,r,tt) * drate * nyrs(t)))
$if      set ccost     + sum(newj(j),      sum(jvrttv(j,v,r,tt)$(tt.val le t.val), IG(j,r,tt)   * gcapcost(j,v,r)                   * gdeprtime(j,v,r,tt) * drate * nyrs(t)))
$if      set ccost     + sum(ghyd(j),       sum(jvrttv(j,v,r,tt)$(tt.val le t.val), IGC(j,r,tt)  * gcapcostc(j,v,r)                  * gdeprtime(j,v,r,tt) * drate * nyrs(t)))
$if      set ccost     + sum(ghyd(j),       sum(jvrttv(j,v,r,tt)$(tt.val le t.val), IGD(j,r,tt)  * gcapcostd(j,v,r)                  * gdeprtime(j,v,r,tt) * drate * nyrs(t)))
$if      set ccost     + sum(ghyd(j),       sum(jvrttv(j,v,r,tt)$(tt.val le t.val), IGR(j,r,tt)  * gcapcostr(j,v,r)                  * gdeprtime(j,v,r,tt) * drate * nyrs(t)))
$if      set ccost     + sum(tmap(k,r,rr), sum(tvrttv(k,v,r,tt)$(tt.val le t.val), IT(k,r,rr,t) * tcapcost(k,r,rr)                  * tdeprtime(k,v,r,tt) * drate * nyrs(t)))
                )
                !! end investment cost (old, excluding discount via investment cost factor)
                !! end discountng
                )
                !! end region sum
                )
                !! end period sum
                ) ;
                
ginvestcost_old..
*        Surplus is defined in million EUR
         GINVCOSTOLD =e=
*        Sum over all time period t
                !! begin period sum
                sum(toptimize(t),             
                !! begin discounting
                dfact(t) * (
*               Sum over all regions r
                !! begin region sum
                sum(r, 
*               INVESTMENT COST
*               Old, excluding discouting via normal annui, and ccost)                
                !! begin investment cost (old)
*               We need nyrs because investments happens once only but production nyrs-times)
                1 / nyrs(t)  * (1 
*                                   Normal investor consider total investment cost in the period of investment
$if      set normal    + sum(newj(j),      IG(j,r,t)    * sum(jvrttv(j,v,r,t), gcapcost(j,v,r)                   * gendeffect(j,v,r,t)))
$if      set normal    + sum(ghyd(j),      IGC(j,r,t)   * sum(jvrttv(j,v,r,t), gcapcostc(j,v,r)                  * gendeffect(j,v,r,t)))
$if      set normal    + sum(ghyd(j),      IGD(j,r,t)   * sum(jvrttv(j,v,r,t), gcapcostd(j,v,r)                  * gendeffect(j,v,r,t)))
$if      set normal    + sum(ghyd(j),      IGR(j,r,t)   * sum(jvrttv(j,v,r,t), gcapcostr(j,v,r)                  * gendeffect(j,v,r,t)))
*                                   Investment costs follow from annuities (%) of borrowed capital (EUR/kW * GW)
$if      set annui     + sum(newj(j),      sum(jvrttv(j,v,r,tt)$(tt.val le t.val), IG(j,r,tt)   * gcapcost(j,v,r)                   * gdeprtime(j,v,r,tt) * annuity_jr(j,v,r) * nyrs(t)))
$if      set annui     + sum(ghyd(j),      sum(jvrttv(j,v,r,tt)$(tt.val le t.val), IGC(j,r,tt)  * gcapcostc(j,v,r)                  * gdeprtime(j,v,r,tt) * annuity_jr(j,v,r) * nyrs(t)))
$if      set annui     + sum(ghyd(j),      sum(jvrttv(j,v,r,tt)$(tt.val le t.val), IGD(j,r,tt)  * gcapcostd(j,v,r)                  * gdeprtime(j,v,r,tt) * annuity_jr(j,v,r) * nyrs(t)))
$if      set annui     + sum(ghyd(j),      sum(jvrttv(j,v,r,tt)$(tt.val le t.val), IGR(j,r,tt)  * gcapcostr(j,v,r)                  * gdeprtime(j,v,r,tt) * annuity_jr(j,v,r) * nyrs(t)))
*                                   Investment costs follow from WACC (%) of capital stock (EUR/kW * GW)
$if      set ccost     + sum(newj(j),      sum(jvrttv(j,v,r,tt)$(tt.val le t.val), IG(j,r,tt)   * gcapcost(j,v,r)                   * gdeprtime(j,v,r,tt) * drate * nyrs(t)))
$if      set ccost     + sum(ghyd(j),      sum(jvrttv(j,v,r,tt)$(tt.val le t.val), IGC(j,r,tt)  * gcapcostc(j,v,r)                  * gdeprtime(j,v,r,tt) * drate * nyrs(t)))
$if      set ccost     + sum(ghyd(j),      sum(jvrttv(j,v,r,tt)$(tt.val le t.val), IGD(j,r,tt)  * gcapcostd(j,v,r)                  * gdeprtime(j,v,r,tt) * drate * nyrs(t)))
$if      set ccost     + sum(ghyd(j),      sum(jvrttv(j,v,r,tt)$(tt.val le t.val), IGR(j,r,tt)  * gcapcostr(j,v,r)                  * gdeprtime(j,v,r,tt) * drate * nyrs(t)))
                )
                !! end investment cost (old, excluding discount via investment cost factor)
                !! end discountng
                )
                !! end region sum
                )
                !! end period sum
                ) ;
                
tinvestcost_old..
*        Surplus is defined in million EUR
         TINVCOSTOLD =e=
*        Sum over all time period t
                !! begin period sum
                sum(toptimize(t),             
                !! begin discounting
                dfact(t) * (
*               Sum over all regions r
                !! begin region sum
                sum(r, 
*               INVESTMENT COST
*               Old, excluding discouting via normal annui, and ccost)                
                !! begin investment cost (old)
*               We need nyrs because investments happens once only but production nyrs-times)
                1 / nyrs(t)  * (1 
*                                   Normal investor consider total investment cost in the period of investment
$if      set normal    + sum(tmap(k,r,rr), IT(k,r,rr,t) * sum(tvrttv(k,v,r,t), tcapcost(k,r,rr)                  * tendeffect(k,v,r,t)))
*                                   Investment costs follow from annuities (%) of borrowed capital (EUR/kW * GW)
$if      set annui     + sum(tmap(k,r,rr), sum(tvrttv(k,v,r,tt)$(tt.val le t.val), IT(k,r,rr,t) * tcapcost(k,r,rr)                  * tdeprtime(k,v,r,tt) * annuity_kr(k,v,r) * nyrs(t)))
*                                   Investment costs follow from WACC (%) of capital stock (EUR/kW * GW)
$if      set ccost     + sum(tmap(k,r,rr), sum(tvrttv(k,v,r,tt)$(tt.val le t.val), IT(k,r,rr,t) * tcapcost(k,r,rr)                  * tdeprtime(k,v,r,tt) * drate * nyrs(t)))
                )
                !! end investment cost (old, excluding discount via investment cost factor)
                !! end discountng
                )
                !! end region sum
                )
                !! end period sum
                ) ;

                

investcost_mixed_learning..
*        Surplus is defined in million EUR
         INVCOSTMIXEDLEA =e= GINVCOSTMIXED + TINVCOSTOLD +
*        Sum over all time period t
                !! begin period sum
                sum(toptimize(t),
* European learning investment cost without region sum (mixed)
$if      set lbdeurcon  $if      set mixed       sum(new(i_lea(i)),  sum(inv, rshare(inv,i) * sum(ivttv(i,v,t),  CAPEXEUR_CON(i,v) * 1e+6 * rzeta(inv,i,v)))) +
$if      set lbdeurnlp  $if      set mixed       sum(new(i_lea(i)),  sum(inv, rshare(inv,i) * sum(ivttv(i,v,t),  CAPEXEUR_NLP(i,v) * 1e+6 * rzeta(inv,i,v)))) +
$if      set lbdeur     $if      set mixed       sum(new(i_lea(i)),  sum(inv, rshare(inv,i) * sum(ivttv(i,v,t),  CAPEXEUR_MIP(i,v) * 1e+6 * rzeta(inv,i,v)))) +
$if      set lbseur     $if      set mixed       sum(new(ki_lea(i)), sum(inv, rshare(inv,i) * sum(ivttv(i,v,t), KCAPEXEUR_MIP(i,v)        * rzeta(inv,i,v)))) +
*               Sum over all regions r
                !! begin region sum
                sum(r, 
*               INVESTMENT COST
                !! begin investment cost
* Investment cost for non-learning technologies
$if      set lbd        $if      set mixed       sum(new(notir_lea(i,r)),    sum(inv,  share(inv,i,r) * IX(i,r,t)    * sum(ivrttv(i,v,r,t),   (capcost(i,v,r) + deccost(i,v,r)) * zeta(inv,i,v,r)))) +
$if      set lbdeur     $if      set mixed       sum(new(notir_lea(i,r)),    sum(inv,  share(inv,i,r) * IX(i,r,t)    * sum(ivrttv(i,v,r,t),   (capcost(i,v,r) + deccost(i,v,r)) * zeta(inv,i,v,r)))) +
$if      set lbs        $if      set mixed       sum(new(notkir_lea(i,r)),   sum(inv,  share(inv,i,r) * IX(i,r,t)    * sum(ivrttv(i,v,r,t),   (capcost(i,v,r) + deccost(i,v,r)) * zeta(inv,i,v,r)))) +
$if      set lbseur     $if      set mixed       sum(new(notkir_lea(i,r)),   sum(inv,  share(inv,i,r) * IX(i,r,t)    * sum(ivrttv(i,v,r,t),   (capcost(i,v,r) + deccost(i,v,r)) * zeta(inv,i,v,r)))) +
* Investment cost for learning technologies
$if      set lbdcon     $if      set mixed       sum(new(ir_lea(i,r)),  sum(inv,  share(inv,i,r) * sum(ivrttv(i,v,r,t), CAPEX_CON(i,v,r)  * 1e+6 * zeta(inv,i,v,r)))) +
$if      set lbdnlp     $if      set mixed       sum(new(ir_lea(i,r)),  sum(inv,  share(inv,i,r) * sum(ivrttv(i,v,r,t), CAPEX_NLP(i,v,r)  * 1e+6 * zeta(inv,i,v,r)))) +
$if      set lbdmip     $if      set mixed       sum(new(ir_lea(i,r)),  sum(inv,  share(inv,i,r) * sum(ivrttv(i,v,r,t), CAPEX_MIP(i,v,r)  * 1e+6 * zeta(inv,i,v,r)))) +
$if      set lbs        $if      set mixed       sum(new(kir_lea(i,r)), sum(inv,  share(inv,i,r) * sum(ivrttv(i,v,r,t), KCAPEX_MIP(i,v,r)        * zeta(inv,i,v,r)))) +
               !! end investment cost (new)
               !! end region sum
               1)
               !! end period sum
               ) ;
               
investcost_old_learning..
*        Surplus is defined in million EUR
         INVCOSTOLDLEA =e= GINVCOSTOLD + TINVCOSTOLD +
*        Sum over all time period t
                !! begin period sum
                sum(toptimize(t),
* European learning investment cost without region sum
$if      set lbdeurcon  $if      set normal      1 / nyrs(t)  * dfact(t) * sum(new(i_lea(i)),  sum(ivttv(i,v,t),  CAPEXEUR_CON(i,v) * 1e+6 *  endeffecteur(i,v,t)))  +
$if      set lbdeurnlp  $if      set normal      1 / nyrs(t)  * dfact(t) * sum(new(i_lea(i)),  sum(ivttv(i,v,t),  CAPEXEUR_NLP(i,v) * 1e+6 *  endeffecteur(i,v,t)))  +
$if      set lbdeur     $if      set normal      1 / nyrs(t)  * dfact(t) * sum(new(i_lea(i)),  sum(ivttv(i,v,t),  CAPEXEUR_MIP(i,v) * 1e+6 *  endeffecteur(i,v,t)))  +
$if      set lbseur     $if      set normal      1 / nyrs(t)  * dfact(t) * sum(new(ki_lea(i)), sum(ivttv(i,v,t), KCAPEXEUR_MIP(i,t) * 1e+6 *  endeffecteur(i,v,t)))  +
*               Sum over all regions r
                !! begin region sum
                sum(r, 
*               DISCOUNTING                
                !! begin discounting
                dfact(t) * (               
*               INVESTMENT COST
*               Old, excluding discouting via normal annui, and ccost)                
                !! begin investment cost (old)
*               We need nyrs because investments happens once only but production nyrs-times)
                1 / nyrs(t)  * (1 
* Investment cost for non-learning technologies
$if      set lbd        $if      set normal    + sum(new(notir_lea(i,r)),  IX(i,r,t) * sum(ivrttv(i,v,r,t), (capcost(i,v,r) + deccost(i,v,r)) * endeffect(i,v,r,t)))
$if      set lbdeur     $if      set normal    + sum(new(notir_lea(i,r)),  IX(i,r,t) * sum(ivrttv(i,v,r,t), (capcost(i,v,r) + deccost(i,v,r)) * endeffect(i,v,r,t)))    
$if      set lbs        $if      set normal    + sum(new(notkir_lea(i,r)), IX(i,r,t) * sum(ivrttv(i,v,r,t), (capcost(i,v,r) + deccost(i,v,r)) * endeffect(i,v,r,t)))
$if      set lbseur     $if      set normal    + sum(new(notkir_lea(i,r)), IX(i,r,t) * sum(ivrttv(i,v,r,t), (capcost(i,v,r) + deccost(i,v,r)) * endeffect(i,v,r,t)))
* Investment cost for learning technologies
$if      set lbdcon     $if      set normal    + sum(new(ir_lea(i,r)),  sum(ivrttv(i,v,r,t),  CAPEX_CON(i,v,r) * 1e+6 *  endeffect(i,v,r,t)))
$if      set lbdnlp     $if      set normal    + sum(new(ir_lea(i,r)),  sum(ivrttv(i,v,r,t),  CAPEX_NLP(i,v,r) * 1e+6 *  endeffect(i,v,r,t)))
$if      set lbdmip     $if      set normal    + sum(new(ir_lea(i,r)),  sum(ivrttv(i,v,r,t),  CAPEX_MIP(i,v,r) * 1e+6 *  endeffect(i,v,r,t)))
$if      set lbs        $if      set normal    + sum(new(kir_lea(i,r)), sum(ivrttv(i,v,r,t), KCAPEX_MIP(i,v,r)        * kendeffect(i,r,t)))
               !! end investment cost
               )
               !! end discounting
               )
               !! end region sum
               )
               !! end period sum
               ) ;

* * * Objective function definition
objdef..
*        Surplus is defined in million EUR
         SURPLUS =e=
*        Sum over all time period t
$if not  set learning   $if      set mixed  $if not  set invtypes  INVCOSTMIXED +
$if not  set learning   $if      set mixed  $if      set invtypes  INVCOSTMIXEDINV +
$if not  set learning   $if not  set mixed  $if not  set invendo   INVCOSTOLD +
$if not  set learning   $if not  set mixed  $if      set invendo   INVCOSTENDO +
$if      set learning   $if      set mixed                         INVCOSTMIXEDLEA +
$if      set learning   $if not  set mixed                         INVCOSTOLDLEA +
                !! begin period sum
                sum(toptimize(t),
                !! begin region sum
                sum(r, 
*               DISCOUNTING                
                !! begin discounting
                dfact(t) * (               
*               DISPATCH COST
*               Are measured in EUR/MWh and generation in GWh, so that we need to correct by 1e-3
                !! begin dispatch cost (regional)
* * Segments
*                       Dispatch cost (EUR/MWh) for generation (GWh)
$if not  set days       $if not  set static     + 1e-3 * sum(ivrt(i,v,r,t),            discost(i,v,r,t) * sum(s, hours(s) * X(s,i,v,r,t)))
$if      set hours      $if      set static     + 1e-3 * sum(ivrt(i,v,r,t),            sum(s, hours(s) * X(s,i,v,r,t) * discost_hours(s,i,v,r,t)))
$if      set segments   $if      set static     + 1e-3 * sum(ivrt(i,v,r,t),            sum(s, hours(s) * X(s,i,v,r,t) * discost_static(i,v,r,t)))
*                       Ramping cost (EUR/MWh) for ramping up and down (GWh) plus efficiency losses when not operating optimal (assumed to be linear and at mean for new vintages to avoid non-linearities)
$if not  set days       $if      set ramcost    + 1e-3 * sum(ivrt(i,v,r,t),            ramcost(i,v,r)   * sum(s, hours(s) * (RPNEG(s,i,v,r,t) + RPPOS(s,i,v,r,t))))
*                       Start-up cost (EUR/MW) for starting power plant after offtime 
$if not  set days       $if      set stacost    + 1e-3 * sum(ivrt(i,v,r,t),            stacost(i,v,r)   * sum(s, hours(s) * NUMONOFF(s,i,v,r,t)))
*                       Variable operation and maintenance cost (EUR/MWh) for storage charge and discharge (GWh)
$if not  set days       $if      set storage    $if not  set storagebalnv   + 1e-3 * sum(jvrt(j,v,r,t),            gvomcost(j,v,r)  * sum(s, hours(s) * GD(s,j,v,r,t)))
$if not  set days       $if      set storage    $if      set storagebalnv   + 1e-3 * sum(jvrt(nonvj(j),v,r,t),     gvomcost(j,v,r)  * sum(s, hours(s) * GD(s,j,v,r,t)))
$if not  set days       $if      set storage    $if      set storagebalnv   + 1e-3 * sum(nvj(j),                   sum(tv(t,v), gvomcost(j,v,r)  * sum(s, hours(s) * GDNV(s,j,r,t))))
*                       Variable operation and maintenance cost (EUR/MWh) for exports only (GWh)
$if not  set days       $if      set trans      + 1e-3 * sum((k,rr)$tmap(k,r,rr),      tvomcost(k,r,rr) * sum(s, hours(s) * E(s,k,r,rr,t)))
* * Days
*                       Dispatch cost (EUR/MWh) for generation (GWh)
$if      set days       $if not  set static     + 1e-3 * sum(ivrt(i,v,r,t),            discost(i,v,r,t) * sum(sd, days(sd) * sum(hd, X_D(sd,hd,i,v,r,t))))
$if      set days       $if      set static     + 1e-3 * sum(ivrt(i,v,r,t),            sum(sd, days(sd) * sum(hd, X_D(sd,hd,i,v,r,t) * discost_static(i,v,r,t))))
*                       Ramping cost (EUR/MWh) for ramping up and down (GWh) plus efficiency losses when not operating optimal (assumed to be linear and at mean for new vintages to avoid non-linearities)
$if      set days       $if      set ramcost    + 1e-3 * sum(ivrt(i,v,r,t),            ramcost(i,v,r)   * sum(sd, days(sd) * sum(hd, (RPNEG_D(sd,hd,i,v,r,t) + RPPOS_D(sd,hd,i,v,r,t)))))
*                       Start-up cost (EUR/MW) for starting power plant after offtime 
$if      set days       $if      set stacost    + 1e-3 * sum(ivrt(i,v,r,t),            stacost(i,v,r)   * sum(sd, days(sd) * sum(hd, NUMONOFF_D(sd,hd,i,v,r,t))))
*                       Variable operation and maintenance cost (EUR/MWh) for storage charge and discharge (GWh)
$if      set days       $if      set storage    $if not  set storagebalnv   + 1e-3 * sum(jvrt(j,v,r,t),            gvomcost(j,v,r)  * sum(sd, days(sd) * sum(hd, GD_D(sd,hd,j,v,r,t))))
$if      set days       $if      set storage    $if      set storagebalnv   + 1e-3 * sum(jvrt(nonvj(j),v,r,t),     gvomcost(j,v,r)  * sum(sd, days(sd) * sum(hd, GD_D(sd,hd,j,v,r,t))))
$if      set days       $if      set storage    $if      set storagebalnv   + 1e-3 * sum(nvj(j),                   sum(tv(t,v), gvomcost(j,v,r)  * sum(sd, days(sd) * sum(hd, GDNV_D(sd,hd,j,r,t)))))
$if      set storage    $if      set hydrogenimport                         +        sum(ghyd(j),                  himport(r,t) * HIMP(j,r,t))
*                       Variable operation and maintenance cost (EUR/MWh) for exports only (GWh)
$if      set days       $if      set trans      + 1e-3 * sum((k,rr)$tmap(k,r,rr),      tvomcost(k,r,rr) * sum(sd, days(sd) * sum(hd, E_D(sd,hd,k,r,rr,t))))
* * Both
*                        Cost of biomass fuel supply (we add "true" cost of receiving biomass by accouting for a step-wise biomass supply function; dispatch cost follow from pfuel)
$if      set biomarket1_r      + sum(dbclass, DBSR(dbclass,r,t)    * (dbcost_r(dbclass,r,t) - pfuel("Bioenergy",r,t)))
$if      set biomarket2_r      + sum(dbclass, DBSRAGG(dbclass,r,t) * (dbcost_r(dbclass,r,t) - pfuel("Bioenergy",r,t)))
$if      set biomarket3_r      + sum(dbclass, DBSRAGG(dbclass,r,t) * (dbcost_r(dbclass,r,t) - pfuel("Bioenergy",r,t)))
* Assume all trade cost on the importing side
$if      set biotrade          + sum(dbclass, sum(dbmap(dbtrans,rr,r), DBE(dbtrans,dbclass,rr,r,t) * dbcost_rr(dbclass,dbtrans,rr,r,t)))
$if      set biobackstop       + DBIMP(r,t) * (dbcost_imp(r,t) - pfuel("Bioenergy",r,t))
*                        Cost of natural gas fuel supply (we add "true" cost of receiving natural gas by accouting for a step-wise natural gas supply function; dispatch cost follow from pfuel)
$if      set gasmark_r  + 1e-3 * sum(ngclass, NGSR(ngclass,r,t) * (ngcost_r(ngclass,r,t) - pfuel("Gas",r,t)))
                !! end dispatch cost (regional)              
*               POLICY COST
*               Are from the perspective of the investor/firm/generator whose costs decrease when receiving a subsidy but increase by taxing (this is not a welfare optimum)
                !! begin policy cost
*                       Production subsidy (also for old vintage capacity in the moment, one can play around with newv(v))
*                       MM (todo): Think about introducing subsidy by vintage level to reflect feed-in tariff structures
$if      set subsidiestaxes $if not  set days       - 1e-3 * sum(ivrt(irnw(i),v,r,t),             irnwsub(r,t)            * sum(s, X(s,i,v,r,t) * hours(s) ))
$if      set subsidiestaxes $if not  set days       - 1e-3 * sum(ivrt(rnw(i),v,r,t),              rnwsub(r,t)             * sum(s, X(s,i,v,r,t) * hours(s) ))
$if      set subsidiestaxes $if not  set days       - 1e-3 * sum(ivrt(sol(i),v,r,t),              solsub(r,t)             * sum(s, X(s,i,v,r,t) * hours(s) ))
$if      set subsidiestaxes $if not  set days       - 1e-3 * sum(ivrt(wind(i),v,r,t),             windsub(r,t)            * sum(s, X(s,i,v,r,t) * hours(s) ))
$if      set subsidiestaxes $if not  set days       - 1e-3 * sum(ivrt(nuc(i),v,r,t),              nucsub(r,t)             * sum(s, X(s,i,v,r,t) * hours(s) ))
$if      set subsidiestaxes $if not  set days       - 1e-3 * sum(ivrt(lowcarb(i),v,r,t),          lowcarbsub(r,t)         * sum(s, X(s,i,v,r,t) * hours(s) ))
$if      set subsidiestaxes $if      set days       - 1e-3 * sum(ivrt(irnw(i),v,r,t),             irnwsub(r,t)            * sum(sd, days(sd) * sum(hd, X_D(sd,hd,i,v,r,t))))
$if      set subsidiestaxes $if      set days       - 1e-3 * sum(ivrt(rnw(i),v,r,t),              rnwsub(r,t)             * sum(sd, days(sd) * sum(hd, X_D(sd,hd,i,v,r,t))))
$if      set subsidiestaxes $if      set days       - 1e-3 * sum(ivrt(sol(i),v,r,t),              solsub(r,t)             * sum(sd, days(sd) * sum(hd, X_D(sd,hd,i,v,r,t))))
$if      set subsidiestaxes $if      set days       - 1e-3 * sum(ivrt(wind(i),v,r,t),             windsub(r,t)            * sum(sd, days(sd) * sum(hd, X_D(sd,hd,i,v,r,t))))
$if      set subsidiestaxes $if      set days       - 1e-3 * sum(ivrt(nuc(i),v,r,t),              nucsub(r,t)             * sum(sd, days(sd) * sum(hd, X_D(sd,hd,i,v,r,t))))
$if      set subsidiestaxes $if      set days       - 1e-3 * sum(ivrt(lowcarb(i),v,r,t),          lowcarbsub(r,t)         * sum(sd, days(sd) * sum(hd, X_D(sd,hd,i,v,r,t))))
*                       Capacity subsidy (paid for new vintage capacity only)
$if      set subsidiestaxes                         - 1e-3 * sum(ivrt(irnw(i),newv(v),r,t),       irnwsub_cap(r,t)        * sum(tv(t,v), IX(i,r,t) ))
$if      set subsidiestaxes                         - 1e-3 * sum(ivrt(rnw(i),newv(v),r,t),        rnwsub_cap(r,t)         * sum(tv(t,v), IX(i,r,t) ))
$if      set subsidiestaxes                         - 1e-3 * sum(ivrt(sol(i),newv(v),r,t),        solsub_cap(r,t)         * sum(tv(t,v), IX(i,r,t) ))
$if      set subsidiestaxes                         - 1e-3 * sum(ivrt(wind(i),newv(v),r,t),       windsub_cap(r,t)        * sum(tv(t,v), IX(i,r,t) ))
$if      set subsidiestaxes                         - 1e-3 * sum(ivrt(nuc(i),newv(v),r,t),        nucsub_cap(r,t)         * sum(tv(t,v), IX(i,r,t) ))
$if      set subsidiestaxes                         - 1e-3 * sum(ivrt(lowcarb(i),newv(v),r,t),    lowcarbsub_cap(r,t)     * sum(tv(t,v), IX(i,r,t) ))
                !! end policy cost
*               SOCIAL COST
               !! begin social cost
*                       Cost (EUR/MWh) of lost load/backstop (GWh)
$if not  set days                               + 1e-3 * voll(r,t) * sum(s, BS(s,r,t) * hours(s))
$if not  set days       $if     set elastic     + 1e-3 * sum(bse, vollelas(bse,r,t) * sum(s, BSELAS(bse,s,r,t) * hours(s)))
$if      set days                               + 1e-3 * voll(r,t) * sum(sd, days(sd) * sum(hd, BS_D(sd,hd,r,t)))
$if      set days       $if     set elastic     + 1e-3 * sum(bse, vollelas(bse,r,t) * sum(sd, days(sd) * sum(hd, BSELAS_D(bse,sd,hd,r,t))))
*                       Public acceptance cost (EUR/MWh) for incremental nuclear generation (GWh)
*                       MM (todo): Think about public acceptance cost for nuclear capacity (and also other capacities)
$if not  set days       $if      set scn        + round( dfact_scc(t)  / dfact(t) * 1e-3,4) * sum(ivrt(i,v,r,t), scn_emit(i,v,r,t)) * sum(s, X(s,i,v,r,t) * hours(s) ))
*                       Social cost of air pollution (EUR per MWh) from generation (GWh)
$if not  set days       $if      set socialcost + round( dfact_scap(t) / dfact(t) * 1e-3,4) * sum(ivrt(i,v,r,t), scap_emit(i,v,r,t) * sum(s, X(s,i,v,r,t) * hours(s) ))
*                       Social cost of carbon (EUR per MWh) from generation (GWh)
$if not  set days       $if      set socialcost + round( dfact_scc(t)  / dfact(t) * 1e-3,4) * sum(ivrt(i,v,r,t), scc_emit(i,v,r,t)  * sum(s, X(s,i,v,r,t) * hours(s) ))
*                       Public acceptance cost (EUR/MWh) for incremental nuclear generation (GWh)
*                       MM (todo): Think about public acceptance cost for nuclear capacity (and also other capacities                       
$if      set days       $if      set scn        + round( dfact_scc(t)  / dfact(t) * 1e-3,4) * sum(ivrt(i,v,r,t), scn_emit(i,v,r,t)) * sum(sd, days(sd) * sum(hd, X_D(sd,hd,i,v,r,t))))
*                       Social cost of air pollution (EUR per MWh) from generation (GWh)
$if      set days       $if      set socialcost + round( dfact_scap(t) / dfact(t) * 1e-3,4) * sum(ivrt(i,v,r,t), scap_emit(i,v,r,t) * sum(sd, days(sd) * sum(hd, X_D(sd,hd,i,v,r,t))))
*                       Social cost of carbon (EUR per MWh) from generation (GWh)
$if      set days       $if      set socialcost + round( dfact_scc(t)  / dfact(t) * 1e-3,4) * sum(ivrt(i,v,r,t), scc_emit(i,v,r,t)  * sum(sd, days(sd) * sum(hd, X_D(sd,hd,i,v,r,t))))
*                       MM (todo): Introduce wind turbine public cost from visibility and noise (metric was already implemented once from Christoph) here (wait for the calibration from the Master thesis of Patrick)
                !! end social cost
*               FIXED COST
                !! begin fixed cost
*                       Fixed operation and maintenance cost (EUR/kW) for generation capacity (GW)
                        + sum(ivrt(i,v,r,t),       XC(i,v,r,t)  *  fomcost(i,v,r))
*                       Fixed operation and maintenance cost (EUR/kW) for storage capacity (GW)
$if      set storage    $if not  set storagebalnv   + sum(jvrt(j,v,r,t),              GC(j,v,r,t)  * gfomcost(j,v,r))
$if      set storage    $if      set storagebalnv   + sum(jvrt(nonvj(j),v,r,t),       GC(j,v,r,t)  * gfomcost(j,v,r))
$if      set storage    $if      set hydrogensimple + sum(jvrt(ghyd(j),v,r,t),        GCC(j,v,r,t) * gfomcostc(j,v,r) + GCD(j,v,r,t) * gfomcostd(j,v,r) + GCR(j,v,r,t) * gfomcostr(j,v,r))
$if      set storage    $if      set storagebalnv   + sum(nvj(j), sum(tv(t,v),        GCNV(j,r,t)  * gfomcost(j,v,r)))


*                       Fixed operation and maintenance cost (EUR/kW) for transmission capacity (GW)
$if      set trans      + sum((k,rr)$tmap(k,r,rr), TC(k,r,rr,t) * tfomcost(k,r,rr))
                !! end fixed cost
                )
                !! end discounting
                )
                !! end region sum
*               DISPATCH COST (system-level)
                !! begin dispatch cost (system)
*               Cost of biomass and natural gas fuel supply come truely from system-wide prices
$if      set biomarket1        + dfact(t) * sum(dbclass, DBS(dbclass,t)    * (dbcost(dbclass,t) - pfuel("Bioenergy","Germany",t)))
$if      set biomarket2        + dfact(t) * sum(dbclass, DBSAGG(dbclass,t) * (dbcost(dbclass,t) - pfuel("Bioenergy","Germany",t)))
$if      set biomarket3        + dfact(t) * sum(dbclass, DBSAGG(dbclass,t) * (dbcost(dbclass,t) - pfuel("Bioenergy","Germany",t)))
$if      set biobackstop       + dfact(t) * sum(r, DBIMP(r,t) * dbcost_imp(r,t))
$if      set gasmark    + 1e-3 * dfact(t) * sum(ngclass, NGS(ngclass,t)    * (ngcost(ngclass,t) - round(sum(r, pfuel("Gas",r,t))/sum(r, 1),4)))
                !! end dispatch cost (system)
* RD investment cost
                !! begin research cost
$if      set flh     $if      set surplusbudget  + dfact(t) * sum(ir_flh(i,r), IFLH(i,r,t))
$if      set flheur  $if      set surplusbudget  + dfact(t) * sum(i_flh(i),    IFLHEUR(i,t))
$if      set flheur  $if      set surplusbudget  + dfact(t) * sum(i_flh(i),    IFLHEUR(i,t))
                )
                !! end research cost
                !! end time period sum
;