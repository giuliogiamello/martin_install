#procedure replaceeM(POWER)

*id eM(M?) =  1
*             -2*ep*Log(M)
*             +2*ep*Log(mu)
*             +2*ep^2*Log(M)^2
*             +2*ep^2*Log(mu)^2
*             -4*ep^2*Log(mu)*Log(M);

*id eM(M?) = sum_(aa,0,`POWER',1/fac_(aa)*(2*ep*Log(mu)-2*ep*Log(M))^aa);
id eM(M?) = sum_(aa,0,`POWER',1/fac_(aa)*(2*ep*Log(mu/M))^aa);

#endprocedure
* vim: syntax=form
