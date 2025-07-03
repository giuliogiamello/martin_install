!
! ----------------------------------------------------------
!
!     qgraf-3.6.10
!     a module for Feynman diagram generation
!
!     copyright 1990--2024 by Paulo Nogueira
!
!     not to be redistributed without explicit permission
!
!     related literature:
!
!      [1] Automatic Feynman graph generation
!          J. Comput. Phys. 105 (1993) 279--289
!          https://doi.org/10.1006/jcph.1993.1074
!
!      [2] Abusing Qgraf
!          Nucl. Instrum. Methods Phys. Res. A 559 (2006) 220--223
!          https://doi.org/10.1016/j.nima.2005.11.151
!
!      [3] Feynman graph generation and propagator mixing, I
!          Comput. Phys. Commun. 269 (2021) 108103
!          https://doi.org/10.1016/j.cpc.2021.108103
!
!     documentation:
!
!       [4] files 'qgraf-3.0.pdf' and 'qgraf-3.6.10.pdf'
!
! ----------------------------------------------------------
!
!     Disclaimer and License:
!
!     This program is copyrighted but may be used for Academic
!     Research and Teaching purposes in basic scientific fields,
!     specially Physics. It may be downloaded, compiled, and
!     then executed in a computer; if necessary, the source
!     file may be edited to modify a few internal parameters,
!     in accordance with the instructions provided in the
!     respective documentation.
!     Any other use is unauthorized including, but not limited
!     to, use in commercial and mission-critical environments.
!
!     As it should be clear -— not least because this is a
!     not-for-profit research tool -— there is no warranty of
!     any kind, including suitability for any purpose.
!
!     With two exceptions, detailed below, redistribution is
!     also unauthorized (the term "redistribution" applies to
!     any distribution not made by the author). A version of
!     this program is _available_ if it can be obtained from
!     the program's "official website" (which is maintained
!     either by the author, or on behalf of the author); a
!     version is _stable_ if it has been declared as such, by
!     its author, in that website.
!
!     Redistributing a version ("A") of this program, as part
!     of a larger package ("P"), is authorized when all of the
!     following conditions are met. To begin with, there are
!     three preliminary conditions:
!     (1) "A" is (i.e. has been, or was) a "stable" version,
!        which is no longer available;
!     (2) "A" is necessary to the functioning of another gratis
!        non-commercial program ("B") used in Physics Research;
!     (3a) the author(s) and maintainer(s) of "B" are no longer
!        able to update "B" so that an available, "stable"
!        version of this program may be used instead, OR
!     (3b) there is no available "stable" version of this
!        program owing to the closure of the official website.
!     In that case, programs "A" and "B" (as well as any other
!     necessary programs) may be distributed gratis in a single
!     package "P", provided that
!     (4) the licenses of all those programs included in "P"
!        allow such a distribution;
!     (5) the license of "P" states that the copy of "A" thus
!        obtained cannot be used for any purpose other than
!        contributing to the functioning of "P", and refer to
!        this license;
!     (6) "A" is distributed in source form only (currently
!        Fortran), encrypted or not;
!     (7) whenever possible, the intention of distributing "P"
!        is communicated by e-mail to the author of "A".
!
!     By making their programs dependent on this program, the
!     authors of said programs agree to make reasonable efforts
!     to move the dependence from older versions of this
!     program to an available "stable" version.
!
!     Users of this program agree to communicate to its author,
!     in a timely manner, any error that they may happen to
!     find.
!
! ----------------------------------------------------------
!
module qbounds
implicit none
save
intrinsic::max
integer,parameter::srec=81
integer,parameter::sfrec=71
integer,parameter::ssrec=63
integer,parameter::srecx=87
integer,parameter::scbuff=524287
integer,parameter::sibuff=4194303
integer,parameter::swbuff0=32767
integer,parameter::sxbuff=2047
integer,parameter::msfiles0=4
integer,parameter::nafiles0=4
integer,parameter::sjbuff=16383
integer,parameter::maxleg=11
integer,parameter::maxrho=7
integer,parameter::maxdeg=8
integer,parameter::maxn=2*max(maxleg-1,maxrho-1)
integer,parameter::maxli=maxn+maxrho
integer,parameter::qb1=(maxleg*(maxleg-1))/2
integer,parameter::qb2=2*maxn+2*maxrho+maxdeg
end module qbounds
module aski
use,intrinsic::iso_fortran_env
implicit none
save
intrinsic::selected_char_kind
integer,parameter::ascii=selected_char_kind('ascii')
integer::anull,acr,alf
integer::aspc,adq,asq,dsq
integer::acomma,acol,ascol,adot
integer::alsbr,arsbr,alcbr,arcbr,alpar,arpar
integer::aminus,aplus,aast,aslash
integer::aeq,agt,alt
integer::ausc,avbar,acirc
integer::anine,azero
integer::abo(0:6),acf(0:127),acz(0:127),punct1(0:0)
integer::dcol,dscol,dslash
integer,parameter::ncrs0=4
integer::crs(1:ncrs0,1:6)
type sttyp
integer::p,id,sqs,z,dqs,q,ns,minus,plus,ast
end type sttyp
type(sttyp),parameter::stty=sttyp(&
p=1,&
id=2,&
sqs=3,&
z=4,&
dqs=5,&
q=6,&
ns=7,&
minus=8,&
plus=9,&
ast=10)
end module aski
module cbuffer
use,non_intrinsic::aski
implicit none
save
integer,parameter::stcbdst0=5
character(kind=ascii,len=:),target,allocatable::xstcb,ystcb
character(kind=ascii,len=:),pointer::stcb
integer::stcbas(1:1),stcbdst(1:1),stcbs(1:1)
integer::scbff(0:stcbdst0)
integer::dcff0
end module cbuffer
module ikeys
implicit none
save
type dekcty
integer::conf,msg,outp,sty,lib,model,in,out,loop,loopk,opt&
,part,off,tru,fals,ubo
end type dekcty
type(dekcty),parameter::dekc=dekcty(&
conf=1,&
msg=2,&
outp=3,&
sty=4,&
lib=5,&
model=6,&
in=7,&
out=8,&
loop=9,&
loopk=10,&
opt=11,&
part=12,&
off=13,&
tru=14,&
fals=15,&
ubo=15)
integer::comc(1:dekc%ubo)
type dekmty
integer::smcc,skcc
integer::mccc,mcge,mcs,mce
integer::mfcc,mfss,mfs
integer::smp,ski,smq
integer::skp,skq
integer::mp
integer::mv
integer::ubo
end type dekmty
type(dekmty),parameter::dekm=dekmty(&
smcc=1,&
skcc=2,&
mccc=3,&
mcge=4,&
mcs=5,&
mfcc=6,&
mfss=7,&
smp=8,&
ski=9,&
mce=10,&
smq=11,&
skp=12,&
mfs=13,&
mp=14,&
mv=15,&
skq=16,&
ubo=16)
type mltyp
integer::mcf,mff,mpf,mvf,msk
end type mltyp
type(mltyp),parameter::mlty=mltyp(&
mcf=1,&
mff=2,&
mpf=3,&
mvf=4,&
msk=5)
type mccty
integer::smcc,skcc,mccc,mfcc
end type mccty
type(mccty),parameter::mcct=mccty(&
smcc=1,&
skcc=2,&
mccc=3,&
mfcc=4)
integer::pisk(0:0),wisk(0:0),cisk(0:0),fisk(0:0)
integer::oisk(0:0),aisk(0:0)
integer::popt0(0:0),wopt0(0:0),copt0(0:0),aopt0(0:0),xopt0(0:0)
integer::popt1(0:0),wopt1(0:0),copt1(0:0),aopt1(0:0),xopt1(0:0)
integer::popt2(0:0),wopt2(0:0),copt2(0:0),aopt2(0:0)
integer::popt3(0:0),wopt3(0:0),copt3(0:0),aopt3(0:0)
integer::popt4(0:0),wopt4(0:0),copt4(0:0),aopt4(0:0)
integer::popt5(0:0),wopt5(0:0),copt5(0:0),aopt5(0:0),sopt5(0:0)
integer::popt7(0:0),wopt7(0:0),copt7(0:0),aopt7(0:0)
integer::popt8(0:0),wopt8(0:0),copt8(0:0),aopt8(0:0)
integer::kc(0:0),kse(0:0),pske(0:0),wske(0:0),aske(0:0)
integer::nske,mcopt2,ncopt2
integer::xtstrp(0:0),xtstrl(0:0)
end module ikeys
module qerr
implicit none
save
type qerrtyp
integer::qend,qok,qw,ierr,qerr
end type qerrtyp
type(qerrtyp),parameter::qerrt=qerrtyp(&
qend=-1,&
qok=0,&
qw=1,&
ierr=2,&
qerr=3)
integer::qerrty(0:0)
end module qerr
module qintr
implicit none
save
type qcontrol
integer::d,e,t,p,c
end type qcontrol
type(qcontrol),parameter::qcntr=qcontrol(&
d=4,&
e=3,&
t=2,&
p=1,&
c=0)
integer::qco(0:4)
type qmatcs
integer::lc,oc
end type qmatcs
type(qmatcs),parameter::qmatc=qmatcs(&
lc=-1,&
oc=-2)
type(qmatcs),parameter::qmats=qmatcs(&
lc=9,&
oc=8)
integer,parameter::eoa=-32767
integer,parameter::nap=-131071
integer,parameter::cflags=14
integer,parameter::dflags=25
integer,parameter::jflags=17
integer::cflag(1:cflags),dflag(1:dflags),jflag(1:jflags)
integer::qdatp,qdatq,qvp,qvl,dprefp,dprefl,vap,vaq
integer::qtfpp,qtfpq,qgfmp,qgfmq
integer::iref,irefh,wiref,wirefh,wsint,spten,ptenp
integer::aaf(0:4)
end module qintr
module wbuffer
use,non_intrinsic::aski
use,non_intrinsic::qbounds
implicit none
save
integer,parameter::swbuff=swbuff0+(swbuff0+1)/2
integer::stwbpf(1:msfiles0),stwbqf(1:msfiles0)
character(kind=ascii,len=:),allocatable::stwb
character(kind=ascii,len=sxbuff+1)::stxb
character(kind=ascii,len=srec)::mlin
integer::nwer,wera(0:0),werb(0:0)
end module wbuffer
module qfiles
use,non_intrinsic::qbounds
implicit none
save
type qftyp
integer::ctyp,wtyp,mtyp,ltyp,styp,otyp,ptyp,ubo
end type qftyp
type(qftyp),parameter::qfty=qftyp(&
ctyp=1,&
wtyp=2,&
mtyp=3,&
ltyp=4,&
styp=5,&
otyp=6,&
ptyp=7,&
ubo=7)
integer::csyfty(1:qfty%ubo)
integer,parameter::mfiles0=2*msfiles0+nafiles0
integer::filp(1:mfiles0),filq(1:mfiles0),filu(1:mfiles0)
integer::film(1:mfiles0),filo(1:mfiles0),filt(1:mfiles0)
integer::nfiles,nsfiles,npfiles,dso
integer::stapi(1:4),cfrp(0:1),stapj(0:0)
integer::drecp(0:0),drecl(0:0),drecii(0:0),drect(0:0)
integer::irecc(0:0),frecc(0:0)
integer::ndrec,ncom
integer::aopp(0:0),aopl(0:0),aopna(0:0),aopnb(0:0)
end module qfiles
module qmix
use,non_intrinsic::qbounds
implicit none
save
integer::n,nli
integer::dis,dsym,nsym,psyms,psym(0:0)
integer::ndiagp,ndiagl,hhp,hhl,doffp,doffl,noffp,noffl
integer::nrho,rhop1,rhop2
integer::rho(1:maxdeg),xn(1:maxn),gam(1:maxn,1:maxn)
integer::vdeg(1:maxn),vxl(1:maxn),vfo(1:maxn)
integer::lmap(1:maxn,1:maxdeg),vmap(1:maxn,1:maxdeg)
integer::pmap(1:maxn,1:maxdeg),vlis(1:maxn),invlis(1:maxn)
integer::rdeg(1:maxn),sdeg(1:maxn),amap(1:maxn,1:maxdeg)
integer::ex(1:maxli),ey(1:maxli)
integer::ovm(1:maxn,1:maxdeg),iovm(1:maxn,1:maxdeg)
integer::ntadp,net(-3:3),xtail(1:maxn),xhead(1:maxn)
integer::flow(1:maxli,0:maxleg+maxrho),ege(1:maxn,1:maxn)
integer::zpro(0:maxli),zbri(0:maxli),zcho(0:maxli)
integer::rbri(0:maxli),sbri(0:maxli)
integer::vparti(0:0),vpartj(0:0)
end module qmix
module qmodel
implicit none
save
integer::smodp(0:0),smodq(0:0)
integer,parameter::mll0=5
integer,parameter::mll4=4
integer::mll(1:mll0),mllt(1:mll0)
integer,parameter::mflags=7
integer::mflag(1:mflags)
type phityp
integer::def,notdp,int,ntint,ext
end type phityp
type(phityp),parameter::phity=phityp(&
def=0,&
notdp=1,&
int=2,&
ntint=3,&
ext=5)
integer::nphi,nprop,npprop,nvert
integer::nblok,nblokp,blok(0:0)
integer::nudk,ngmk,npmk,nvmk
integer::namep(0:0),namel(0:0),link(0:0),antiq(0:0)
integer::nivd(0:0),dpntro(0:0),vparto(0:0),vval(0:0)
integer::nivdx(0:0)
integer::gmkp(0:0),gmkl(0:0),gmkr(0:0),gmko(0:0)
integer::gmkvp(0:0),gmkvl(0:0),gmkvt(0:0)
integer::pmkp(0:0),pmkl(0:0),pmkd(0:0),pmkt(0:0)
integer::pmkvma(0:0),pmkvmi(0:0),pmkzp(0:0),psfi(0:0)
integer::pmkvp(0:0),pmkvl(0:0),pmkvfpp(0:0),pmkvflp(0:0)
integer::vmkp(0:0),vmkl(0:0),vmkt(0:0),vmks(0:0)
integer::vmkmao(0:0),vmkmio(0:0),vmkzp(0:0),vsfi(0:0)
integer::vmkvp(0:0),vmkvl(0:0),vmkvpp(0:0),vmkvlp(0:0)
end module qmodel
module qproc
use,non_intrinsic::qbounds
implicit none
save
integer::nleg,nloop,loopx(1:5),inco,outg,leg(0:0)
integer::momep(0:0),momeq(0:0),momel(0:0)
integer,parameter::kpqs(1:4)=[107,112,113,115]
integer::ps1(1:maxleg),invps1(1:maxleg)
integer::ns1,p1l(1:qb1),p1r(1:qb1)
integer::ntf,tpc(0:0),tftyp(0:0),tfnarg(0:0)
integer::tfa(0:0),tfb(0:0),tfc(0:0),tfo(0:0),tf2(0:0)
integer::tfta(0:0),tftb(0:0),tftic(0:0)
integer::tfills(0:0),tfiuls(0:0)
end module qproc
module qsty
implicit none
save
integer,parameter::maxtak=3
integer,parameter::maxpot=6
integer,parameter::eosf=-8191
integer::iogp(1:4)
integer::ks,tak(1:maxtak),pot(0:maxpot)
integer::udkp(0:2),udkl(0:0),udkt(0:0),udki(0:0)
integer::kla(0:0),klo(0:0),bbc(0:0)
end module qsty
module zbuffer
use,non_intrinsic::qbounds
implicit none
save
integer,parameter::stibdst0=8
integer,parameter::stj0=4
integer,target,allocatable::xstib(:),ystib(:)
integer,pointer::stib(:)
integer::diff0
integer::stibas(1:1),stibdst(1:1),stibs(1:1)
integer::sibff(0:stibdst0)
integer::stjb(1:sjbuff+1)
integer::jbco(0:128)
integer::stjbs
integer::nstjb
end module zbuffer
subroutine qende
use,non_intrinsic::qintr
implicit none
save
if(cflag(1).gt.0)then
call propos(3)
end if
if(cflag(2).ge.0)then
if(cflag(11).eq.0)then
call spp(5,0,0,1)
end if
end if
call qclose(0,0)
stop
end subroutine qende
subroutine pg11
use,non_intrinsic::qintr
use,non_intrinsic::qmix
use,non_intrinsic::qmodel
use,non_intrinsic::qproc
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer::atyp,styp,nili,wot
integer::ii,i1,i2,i3,j1,j2,jj
if(qco(qcntr%p).ne.0)then
qco(qcntr%p)=0
if((jflag(1).ne.0).or.&
(jflag(2).ne.0))then
if(cflag(2).ge.0)then
if(cflag(11).eq.0)then
call spp(4,0,0,0)
go to 90
end if
end if
end if
rho(1)=nleg
rho(2)=0
rhop1=rho(1)+1
jj=rho(1)+2*(nloop-1)
do i1=nrho,3,-1
rho(i1)=jj/(i1-2)
jj=jj-rho(i1)*(i1-2)
end do
go to 50
else if(cflag(11).ne.0)then
go to 10
end if
40 continue
if(cflag(2).ge.0)then
if(cflag(11).eq.0)then
call spp(4*wot,0,0,0)
if((jflag(1).ne.0).or.&
(jflag(2).ne.0))then
go to 90
end if
end if
end if
10 continue
do i1=4,nrho
if(rho(i1).gt.0)then
go to 30
end if
end do
go to 90
30 continue
rho(i1)=rho(i1)-1
jj=i1-2+rho(3)
do i2=i1-1,3,-1
rho(i2)=jj/(i2-2)
jj=jj-rho(i2)*(i2-2)
end do
50 continue
do i1=3,nrho
if(rho(i1).gt.0)then
if(stib(nivd(0)+i1).eq.0)then
go to 10
end if
end if
end do
call sdiag(1,-1)
jflag(3)=1
wot=1
if(nivdx(0).ne.nap)then
do i1=3,nrho
if(rho(i1).gt.0)then
if(stib(nivdx(0)+i1).eq.0)then
go to 40
end if
end if
end do
end if
if(cflag(10).ne.0)then
do i1=3,nrho
j1=stib(vparti(0)+i1)
j2=stib(vpartj(0)+i1)
if(j1.ge.0)then
if(j2.eq.0)then
if(rho(i1).ne.j1)then
wot=-1
end if
else if(j2.gt.0)then
if(rho(i1).lt.j1)then
wot=-1
end if
else
if(rho(i1).gt.j1)then
wot=-1
end if
end if
end if
if(wot.lt.0)then
jflag(3)=0
go to 40
end if
end do
cflag(10)=1
end if
nili=-rho(1)
do i1=3,nrho
nili=nili+i1*rho(i1)
end do
nili=nili/2
if(npprop.eq.0)then
if(nili.gt.0)then
go to 40
end if
end if
if(nloop.eq.0)then
if((dflag(1).gt.0).or.&
(dflag(2).gt.0))then
if(nili.ne.0)then
go to 40
end if
else if((dflag(1).lt.0).or.&
(dflag(2).lt.0))then
if(nili.eq.0)then
go to 40
end if
end if
end if
if(zpro(nili).le.0)then
go to 40
end if
if(nloop.eq.0)then
if((zcho(0).gt.0).and.&
(zbri(nili).gt.0))then
go to 70
end if
else
do i1=nloop,nili
if(zcho(i1).gt.0)then
if(zbri(nili-i1).gt.0)then
do i2=0,nili-i1
if(rbri(i2).gt.0)then
if(sbri(nili-i1-i2).gt.0)then
go to 70
end if
end if
end do
end if
end if
end do
end if
go to 40
70 continue
if(dflag(18).ne.0)then
atyp=6
ii=stib(tftic(0)+atyp)
do i1=stib(tfta(0)+ii),stib(tftb(0)+ii)
i2=stib(tf2(0)+i1)
styp=stib(tftyp(0)+i2)
j1=0
j2=0
jj=stib(stib(tfo(0)+i2)+1)
if(stib(vmks(0)+jj).ne.1)then
mlin(1:srec)='pg11_1'
call mput(1,1,0,0,0)
end if
do i3=nrho,3,-1
if(rho(i3).gt.0)then
j1=j1+rho(i3)*stib(stib(vmkmio(0)+jj)+i3)
j2=j2+rho(i3)*stib(stib(vmkmao(0)+jj)+i3)
end if
end do
if(styp.gt.0)then
if(j1.gt.stib(tfb(0)+i2))then
go to 40
else if(j2.lt.stib(tfa(0)+i2))then
go to 40
end if
else
if(j1.ge.stib(tfa(0)+i2))then
if(j2.le.stib(tfb(0)+i2))then
go to 40
end if
end if
end if
end do
end if
qco(qcntr%p)=1
90 continue
jflag(3)=0
if(qco(qcntr%p).eq.0)then
if(jflag(15).ne.0)then
rho(3:nrho)=0
if(cflag(2).ge.0)then
if(cflag(11).eq.0)then
wot=-1
call spp(4*wot,0,0,0)
end if
end if
end if
end if
return
end subroutine pg11
subroutine ctxb(ii,jj,kk)
use,non_intrinsic::cbuffer
use,non_intrinsic::wbuffer
implicit none
save
integer,intent(in)::jj,kk
integer,intent(inout)::ii
integer::j1
j1=0
if(ii.lt.0)then
j1=1
else if(kk.lt.0)then
j1=1
else if(jj.le.0)then
j1=1
else if(kk+jj.gt.stcbas(1))then
j1=1
end if
if(j1.ne.0)then
mlin(1:srec)='ctxb_1'
call mput(1,1,0,0,0)
end if
if(ii+jj.gt.sxbuff)then
call uput(8)
end if
stxb(ii+1:ii+jj)=stcb(kk+1:kk+jj)
ii=ii+jj
return
end subroutine ctxb
subroutine spp(wot,ja,jb,nl)
use,non_intrinsic::cbuffer
use,non_intrinsic::ikeys
use,non_intrinsic::qintr
use,non_intrinsic::qmix
use,non_intrinsic::qmodel
use,non_intrinsic::qproc
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer,intent(in)::wot,ja,jb,nl
integer,external::wztos
integer::i1,i2,i3,i4,i5,i6,j1,j2,j3,j4,ii,jj,kk
integer::ggx,nps(0:4)
if(nl.ne.0)then
if(jflag(7).eq.0)then
write(unit=*,fmt='(a)')
jflag(7)=10
end if
end if
select case(abs(wot))
case(0)
go to 01
case(1)
go to 11
case(2)
go to 21
case(3)
go to 31
case(4)
go to 41
case(5)
go to 51
case default
mlin(1:srec)='spp_1'
call mput(1,1,0,0,0)
end select
01 continue
ii=1
call hrul(1,ii)
kk=(ssrec-(qvl-ii))/2
write(unit=*,fmt='(a,/,a,/,a,/)')stxb(1:ssrec),&
stcb(1:kk)//stcb(qvp:(qvp-1)+qvl),stxb(1:ssrec)
jflag(7)=10
jflag(6)=2
go to 90
11 continue
j1=ja
do i1=ja,jb
if(iachar(stcb(i1:i1)).eq.alf)then
if(j1.lt.i1)then
write(unit=*,fmt='(1x,a)')stcb(j1:i1-1)
else
write(unit=*,fmt='(1x,a)')
end if
j1=i1+1
end if
end do
go to 90
21 continue
call hrul(1,1)
write(unit=*,fmt='(a,/)')stxb(1:ssrec)
jflag(7)=10
go to 90
31 continue
do i1=1,2
nps(:)=0
do i2=1,nphi
jj=0
if((i1.eq.1).and.&
(stib(tpc(0)+i2).eq.phity%ext))then
jj=1
else if((i1.eq.2).and.&
(stib(tpc(0)+i2).ne.phity%ext))then
jj=1
end if
if(jj.eq.1)then
if(i2.lt.stib(link(0)+i2))then
if(stib(antiq(0)+i2).eq.0)then
nps(2)=nps(2)+1
else
nps(4)=nps(4)+1
end if
else if(i2.eq.stib(link(0)+i2))then
if(stib(antiq(0)+i2).eq.0)then
nps(1)=nps(1)+1
else
nps(3)=nps(3)+1
end if
end if
end if
end do
nps(0)=nps(1)+nps(2)+nps(3)+nps(4)
if(nps(0).gt.0)then
ii=0
call ctxb(ii,2,0)
call ctxb(ii,stib(xtstrl(0)+i1),stib(xtstrp(0)+i1)-1)
j1=stcbs(1)
jj=2
call vaocb(jj)
call dkar(nps(0),jj)
call ctxb(ii,jj,j1)
call ctxb(ii,stib(xtstrl(0)+14),stib(xtstrp(0)+14)-1)
if(nps(0).gt.0)then
do i2=1,4
if(nps(i2).gt.0)then
call ctxb(ii,2,0)
call dkar(nps(i2),jj)
call ctxb(ii,jj,j1)
j2=stib(xtstrp(0)+2+i2)
stcb(j1+1:j1+2)=stcb(j2:j2+1)
call ctxb(ii,2,j1)
end if
end do
end if
if(ii.ge.srec)then
j2=stib(xtstrp(0)+18)
j3=stib(xtstrl(0)+18)
ii=srec-1
stxb(srec-j3:ii)=stcb(j2+1:j2+j3)
end if
write(unit=*,fmt='(a,/)')stxb(1:ii)
end if
end do
j1=stcbs(1)
ii=0
call ctxb(ii,2,0)
call ctxb(ii,stib(xtstrl(0)+7),stib(xtstrp(0)+7)-1)
call dkar(nvert,jj)
call ctxb(ii,jj,j1)
call ctxb(ii,stib(xtstrl(0)+14),stib(xtstrp(0)+14)-1)
j2=ii+3
do i1=1,nrho
if(stib(nivd(0)+i1).gt.0)then
j3=ii
call ctxb(ii,2,0)
35 continue
call dkar(i1,jj)
call ctxb(ii,jj,j1)
stcb(j1+1:j1+1)='^'
call ctxb(ii,1,j1)
call dkar(stib(nivd(0)+i1),jj)
call ctxb(ii,jj,j1)
if(ii.ge.ssrec)then
if(j2.ne.0)then
ii=j3
call ctxb(ii,1,0)
stxb(ii:ii)=achar(alf)
call ctxb(ii,j2,0)
j2=0
go to 35
end if
end if
end if
end do
write(unit=*,fmt='(a,/)')stxb(1:ii)
call hrul(1,1)
write(unit=*,fmt='(a,/)')stxb(1:ssrec)
jflag(7)=10
go to 90
41 continue
call vaocb(1)
ggx=rho(1)+2*(loopx(4)-1)
call vaocb(1)
j1=stcbs(1)
j2=j1+1
ii=0
do i1=3,nrho
if(stib(nivd(0)+i1).gt.0)then
kk=wztos(ggx/(i1-2))+2
call dkar(i1,jj)
if((jflag(1).eq.0).and.&
(jflag(2).eq.0))then
if(rho(i1).gt.0)then
call ctxb(ii,jj,j1)
call ctxb(ii,stib(xtstrl(0)+29),stib(xtstrp(0)+29)-1)
call dkar(rho(i1),jj)
call ctxb(ii,jj,j1)
kk=kk-jj
else
call ctxb(ii,jj,0)
call ctxb(ii,stib(xtstrl(0)+30),stib(xtstrp(0)+30)-1)
end if
call ctxb(ii,kk,0)
else
call ctxb(ii,jj,j1)
call ctxb(ii,stib(xtstrl(0)+32),stib(xtstrp(0)+32)-1)
kk=kk-(stib(xtstrl(0)+32)-stib(xtstrl(0)+29))
call ctxb(ii,kk,0)
end if
end if
end do
ii=ii-2
j2=ii
jj=5
if(wot.gt.0)then
call ctxb(ii,jj,0)
call ctxb(ii,stib(xtstrl(0)+15),stib(xtstrp(0)+15)-1)
call ctxb(ii,jj,0)
call ctxb(ii,hhl,hhp)
if(dflag(25).eq.0)then
if((jflag(1).gt.0).or.&
(jflag(2).gt.0).or.&
(jflag(3).gt.0))then
call ctxb(ii,stib(xtstrl(0)+16),stib(xtstrp(0)+16)-1)
end if
end if
end if
i2=stib(xtstrl(0)+8)
j1=stib(xtstrl(0)+15)
if(j2.lt.i2)then
j4=(i2-j2)/2
else
j4=0
end if
if(jflag(6).lt.3)then
if(j2.lt.i2)then
j3=0
else
j3=(j2-i2)/2
end if
jj=j2-i2+2*jj+j1+j4-j3-3
i1=stib(xtstrp(0)+8)
i3=stib(xtstrp(0)+9+dflag(16))
i4=i3-1+stib(xtstrl(0)+9+dflag(16))
i5=stib(xtstrp(0)+31)
i6=(i5-1)+stib(xtstrl(0)+31)
write(unit=*,fmt='(a,/)')stcb(i5:i6)//stcb(1:j3+4)//stcb(i1:i1-1+i2)&
//stcb(1:jj)//stcb(i3:i4)
jflag(6)=3
end if
if(jflag(15).ne.0)then
if(jflag(1).gt.0)then
i5=stib(xtstrp(0)+33)
i6=(i5-1)+stib(xtstrl(0)+33)
write(unit=*,fmt='(a)')stcb(i5:i6)
else
call dkar(nloop,jj)
write(unit=*,fmt='(a)')stcb(1:7-jj)//stcb(stcbs(1)+1:stcbs(1)+jj)
end if
jflag(15)=0
end if
write(unit=*,fmt='(a)')stcb(1:j4+13)//stxb(1:ii)
jflag(7)=0
go to 90
51 continue
ii=0
call ctxb(ii,stib(xtstrl(0)+11),stib(xtstrp(0)+11)-1)
call ctxb(ii,ndiagl,ndiagp)
jj=0
if(ndiagl.eq.1)then
j1=ndiagp+1
if(iachar(stcb(j1:j1)).eq.azero+1)then
jj=1
end if
end if
j1=12+dflag(16)
call ctxb(ii,stib(xtstrl(0)+j1)-jj,stib(xtstrp(0)+j1)-1)
if(dflag(25).eq.0)then
if(jflag(1).ne.0)then
call ctxb(ii,stib(xtstrl(0)+16),stib(xtstrp(0)+16)-1)
end if
end if
if(jflag(8).gt.0)then
if(cflag(14).eq.0)then
call ctxb(ii,stib(xtstrl(0)+17),stib(xtstrp(0)+17)-1)
end if
end if
write(unit=*,fmt='(/,a,/)')stxb(1:ii)
jflag(7)=10
90 return
end subroutine spp
subroutine rflag0
use,non_intrinsic::qintr
use,non_intrinsic::qmodel
use,non_intrinsic::qproc
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer,parameter::ntr=2
integer,parameter::sb=3
integer,parameter::nsb=4
integer,parameter::rb=5
integer,parameter::nrb=6
integer,parameter::b=7
integer,parameter::nb=8
integer,parameter::sl=9
integer,parameter::nsl=10
integer,parameter::dl=11
integer,parameter::ndl=12
integer::intf(1:12)
integer::ii,i1,i2,j1,j2
if(nphi.eq.1)then
mflag(1)=1
end if
j1=0
j2=0
do i1=1,nphi
if(stib(antiq(0)+i1).eq.0)then
j1=1
else
j2=1
end if
end do
if(j1.eq.0)then
mflag(2)=-1
else if(j2.eq.0)then
mflag(2)=1
end if
mflag(3)=1
do i1=1,nvert
if(modulo(stib(vval(0)+i1),2).ne.0)then
mflag(3)=0
exit
end if
end do
if(mflag(3).ne.0)then
if(modulo(nleg,2).ne.0)then
jflag(1)=1
end if
end if
mflag(7)=1
do i1=1,nvert
j1=stib(vparto(0)+i1)
j2=j1+stib(vval(0)+i1)
ii=-1
do i2=j1+1,j2
ii=ii+stib(antiq(0)+stib(i2))
end do
if(abs(ii).ne.1)then
mflag(7)=0
exit
end if
end do
if(mflag(1).eq.0)then
jflag(16)=0
else if(mflag(4).ne.0)then
jflag(16)=0
else
jflag(16)=1
end if
if(nleg.eq.1)then
if(stib(tpc(0)+stib(leg(0)+1)).eq.phity%notdp)then
jflag(1)=1
end if
end if
if(dflag(22).ne.0)then
if((dflag(1).gt.0).or.&
(dflag(2).gt.0))then
j1=stib(tftic(0)+12)
do i1=stib(tfta(0)+j1),stib(tftb(0)+j1)
if(stib(tftyp(0)+stib(tf2(0)+i1)).gt.0)then
jflag(1)=1
end if
end do
end if
end if
if(dflag(16).ne.0)then
if(mflag(1).eq.0)then
mlin(1:srec)="option 'topol' does not apply here"
call mput(1,1,0,0,0)
end if
if(dflag(21).ne.0)then
j1=stib(tftic(0)+11)
do i1=stib(tfta(0)+j1),stib(tftb(0)+j1)
if(stib(tfnarg(0)+stib(tf2(0)+i1)).ne.nleg)then
mlin(1:srec)="'elink' statement incompatible with option 'topol'"
call mput(1,1,0,0,0)
end if
end do
end if
if(dflag(22).ne.0)then
mlin(1:srec)="'plink' statement incompatible with option 'topol'"
call mput(1,1,0,0,0)
end if
end if
if(dflag(17).ne.0)then
if((mflag(2).gt.0).or.&
(mflag(7).eq.0))then
mlin(1:srec)="option 'floop' does not apply here"
call mput(1,1,0,0,0)
end if
end if
if(jflag(17).eq.0)then
if(dflag(2).ne.0)then
jflag(17)=1
else if(dflag(5).ne.0)then
jflag(17)=1
else if(dflag(6).ne.0)then
jflag(17)=1
else if(dflag(7).ne.0)then
jflag(17)=1
else if(dflag(9).ne.0)then
jflag(17)=1
else if(dflag(10).ne.0)then
jflag(17)=1
else if(dflag(22).ne.0)then
jflag(17)=1
end if
end if
if(dflag(25).ne.0)then
go to 90
end if
intf(:)=0
if(dflag(1).ne.0)then
if(dflag(1).gt.0)then
intf(nrb)=1
intf(nsb)=1
intf(nb)=1
else
intf(b)=1
end if
end if
if(dflag(2).ne.0)then
if(dflag(2).gt.0)then
intf(nrb)=1
else
intf(rb)=1
intf(b)=1
end if
end if
if(dflag(3).ne.0)then
if(dflag(3).gt.0)then
intf(nsb)=1
else
intf(sb)=1
intf(b)=1
end if
end if
if(dflag(4).ne.0)then
if(dflag(4).lt.0)then
intf(b)=1
end if
end if
if(dflag(8).ne.0)then
if(dflag(8).gt.0)then
intf(nsb)=1
intf(nsl)=1
end if
end if
if(dflag(9).ne.0)then
if(dflag(9).lt.0)then
intf(ntr)=1
end if
end if
if(dflag(11).ne.0)then
if(dflag(11).gt.0)then
intf(nsl)=1
end if
end if
if(dflag(12).ne.0)then
if(dflag(12).gt.0)then
intf(nsl)=1
else
intf(sl)=1
intf(ntr)=1
end if
end if
if(dflag(13).ne.0)then
if(dflag(13).lt.0)then
intf(dl)=1
intf(ntr)=1
end if
end if
if(dflag(14).ne.0)then
if(dflag(14).gt.0)then
intf(ndl)=1
else
intf(ntr)=1
end if
end if
if(dflag(15).ne.0)then
if(dflag(15).gt.0)then
intf(nsl)=1
intf(ndl)=1
else
intf(ntr)=1
end if
end if
if((intf(sb).ne.0).and.&
(intf(nsb).ne.0))then
jflag(1)=1
else if((intf(rb).ne.0).and.&
(intf(nrb).ne.0))then
jflag(1)=1
else if((intf(b).ne.0).and.&
(intf(nb).ne.0))then
jflag(1)=1
else if((intf(sl).ne.0).and.&
(intf(nsl).ne.0))then
jflag(1)=1
else if((intf(dl).ne.0).and.&
(intf(ndl).ne.0))then
jflag(1)=1
end if
if(dflag(13).gt.0)then
if(dflag(12).gt.0)then
if(dflag(15).lt.0)then
jflag(1)=1
end if
end if
end if
if(dflag(1).lt.0)then
if(dflag(2).gt.0)then
if(dflag(3).gt.0)then
jflag(1)=1
end if
end if
end if
if(dflag(4).lt.0)then
if(dflag(5).gt.0)then
jflag(1)=1
end if
end if
if(dflag(9).gt.0)then
if(dflag(1).gt.0)then
if(dflag(10).lt.0)then
jflag(1)=1
end if
end if
end if
if(dflag(9).lt.0)then
if(dflag(8).gt.0)then
if(dflag(1).gt.0)then
if(dflag(10).gt.0)then
jflag(1)=1
end if
end if
end if
end if
if(dflag(4).lt.0)then
if(dflag(10).gt.0)then
if(dflag(15).gt.0)then
jflag(1)=1
end if
end if
end if
if(dflag(13).gt.0)then
if(dflag(12).gt.0)then
if(dflag(14).lt.0)then
jflag(1)=1
end if
end if
end if
if(dflag(14).lt.0)then
if(dflag(15).gt.0)then
jflag(1)=1
end if
end if
90 continue
return
end subroutine rflag0
subroutine rflag
use,non_intrinsic::qintr
use,non_intrinsic::qmix
use,non_intrinsic::qproc
implicit none
save
integer::i1,i2,i3,ii,jj
jflag(2)=0
if(nloop.gt.1)then
if(dflag(9).gt.0)then
if(dflag(1).lt.0)then
if(dflag(10).gt.0)then
jflag(2)=1
end if
end if
end if
if(dflag(9).gt.0)then
if(dflag(12).lt.0)then
jflag(2)=1
end if
end if
else if(nloop.eq.1)then
if(dflag(9).lt.0)then
jflag(2)=1
end if
end if
if(nloop.eq.0)then
if(dflag(1).gt.0)then
if(nrho.lt.nleg)then
jflag(2)=1
end if
end if
if(dflag(2).gt.0)then
if(nrho.lt.nleg)then
jflag(2)=1
end if
end if
if(dflag(3).lt.0)then
jflag(2)=1
end if
if(dflag(8).lt.0)then
jflag(2)=1
end if
if(dflag(9).lt.0)then
jflag(2)=1
end if
if(dflag(11).lt.0)then
jflag(2)=1
end if
if(dflag(12).lt.0)then
jflag(2)=1
end if
if(dflag(13).lt.0)then
jflag(2)=1
end if
if(dflag(14).lt.0)then
jflag(2)=1
end if
if(dflag(17).lt.0)then
jflag(2)=1
end if
if(dflag(15).lt.0)then
jflag(2)=1
end if
else if(nloop.gt.0)then
if(dflag(1).lt.0)then
if(dflag(10).gt.0)then
if(dflag(8).gt.0)then
jflag(2)=1
else if(dflag(15).gt.0)then
jflag(2)=1
end if
end if
end if
end if
if(dflag(2).gt.0)then
go to 90
end if
do i1=0,maxli
zpro(i1)=abs(zpro(i1))
zcho(i1)=abs(zcho(i1))
zbri(i1)=abs(zbri(i1))
rbri(i1)=abs(rbri(i1))
sbri(i1)=abs(sbri(i1))
end do
if(dflag(1).gt.0)then
do i1=1,maxli
rbri(i1)=-rbri(i1)
sbri(i1)=-sbri(i1)
zbri(i1)=-zbri(i1)
end do
else if(dflag(1).lt.0)then
zbri(0)=-zbri(0)
end if
if(dflag(2).gt.0)then
do i1=1,maxli
rbri(i1)=-rbri(i1)
end do
else if(dflag(2).lt.0)then
rbri(0)=-rbri(0)
end if
if(dflag(3).gt.0)then
do i1=1,maxli
sbri(i1)=-sbri(i1)
end do
else if(dflag(3).lt.0)then
sbri(0)=-sbri(0)
end if
if(dflag(4).lt.0)then
zbri(0)=-zbri(0)
end if
if(dflag(8).gt.0)then
do i1=1,maxli
sbri(i1)=-sbri(i1)
end do
end if
if(zcho(0).gt.0)then
if(dflag(3).lt.0)then
zcho(0)=-1
else if(dflag(4).lt.0)then
zcho(0)=-1
else if(dflag(5).lt.0)then
zcho(0)=-1
else if(dflag(6).lt.0)then
zcho(0)=-1
else if(dflag(7).lt.0)then
zcho(0)=-1
else if(dflag(8).lt.0)then
zcho(0)=-1
else if(dflag(15).lt.0)then
zcho(0)=-1
end if
end if
jj=nleg+3*(nloop-1)
if(nloop.eq.0)then
if(zcho(0).le.0)then
jflag(2)=1
end if
ii=0
do i1=0,jj
if(zpro(i1).gt.0)then
if(zbri(i1).gt.0)then
ii=1
exit
end if
end if
end do
if(ii.eq.0)then
jflag(2)=1
end if
else
ii=0
d01: do i1=nloop,jj
if(zpro(i1).gt.0)then
do i2=nloop,i1
if(zcho(i2).gt.0)then
if(zbri(i1-i2).gt.0)then
do i3=0,i1-i2
if(rbri(i3).gt.0)then
if(sbri(i1-i2-i3).gt.0)then
ii=1
exit d01
end if
end if
end do
end if
end if
end do
end if
end do d01
if(ii.eq.0)then
jflag(2)=1
end if
end if
if(jflag(4).ne.0)then
if(nloop.gt.0)then
jflag(2)=1
else if(nleg.gt.nrho)then
jflag(2)=1
end if
end if
90 continue
return
end subroutine rflag
subroutine propos(sec)
use,non_intrinsic::cbuffer
use,non_intrinsic::ikeys
use,non_intrinsic::qfiles
use,non_intrinsic::qintr
use,non_intrinsic::qmix
use,non_intrinsic::qmodel
use,non_intrinsic::qsty
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
intrinsic::date_and_time
integer,intent(in)::sec
integer,external::stdz
integer,parameter::dtz0=18
integer::ig,igc,igk,kma,lupi,lupj,lupt,ios
integer::i1,i2,ii,ij,j1,j2,k1,k2
integer::d0,f0,f1,f2,ip,stwbp,stwbq,stwbu
integer::dtzl
character(kind=ascii,len=dtz0),target::dtz(1:3)
character(kind=ascii,len=:),pointer::dtzp
if(abs(sec-2).ne.1)then
mlin(1:srec)='propos_1'
call mput(1,1,0,0,0)
end if
if(sec.eq.1)then
j1=0
do i1=1,nfiles
if((filt(i1).eq.qfty%otyp).or.&
(filt(i1).eq.qfty%ptyp))then
if(filt(i1).eq.qfty%otyp)then
if(filp(i1).ne.nap)then
call qopen(i1)
end if
end if
j1=j1+1
f1=film(i1)
stwbpf(f1)=(j1-1)*(swbuff+1)
stwbqf(f1)=stwbpf(f1)
end if
end do
ios=1
allocate(character(j1*(swbuff+1))::stwb,stat=ios)
if(ios.ne.0)then
call uput(0)
end if
j1=min(dekc%outp,dekc%sty)
i1=1
d0=0
do while(i1.lt.j1)
d0=d0+comc(i1)
i1=i1+1
end do
end if
ig=iogp(sec)
if(ig.ge.iogp(sec+1))then
go to 80
end if
f0=0
10 continue
f0=f0+1
if(f0.gt.nfiles)then
go to 90
else if(filt(f0).ne.qfty%otyp)then
if(filt(f0).ne.qfty%ptyp)then
go to 10
end if
end if
if((cflag(11).ne.0).or.&
(filp(f0).ne.nap))then
ig=ig+2
else
ig=ig+stib(ig+1)
go to 10
end if
f1=filo(f0)
f2=film(f0)
ii=0
if(f1.lt.1)then
ii=1
else if(f1.gt.nsfiles)then
ii=1
else if((filt(f0).ne.qfty%otyp).and.&
(filt(f0).ne.qfty%ptyp))then
ii=1
end if
if(ii.ne.0)then
mlin(1:srec)='propos_3'
call mput(1,1,0,0,0)
end if
if(sec.ne.1)then
if(stwbqf(f1).gt.stwbpf(f1))then
if(filt(f0).eq.qfty%otyp)then
call qout(f0)
stwbqf(f1)=stwbpf(f1)
else
mlin(1:srec)='propos_4'
call mput(1,1,0,0,0)
end if
end if
end if
stwbp=stwbpf(f1)
stwbq=stwbqf(f1)
stwbu=stwbp+swbuff
kma=-1
lupt=0
20 continue
igk=stib(ig)
if(igk.gt.0)then
stwbq=stwbq+1
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(stwbq:stwbq)=achar(igk)
ig=ig+1
go to 20
else if(igk.eq.eosf)then
if(lupt.ne.0)then
go to 80
end if
if(stwbq.gt.stwbp)then
stwbqf(f1)=stwbq
call qout(f0)
stwbqf(f1)=stwbp
end if
if(sec.ne.1)then
if(filt(f0).ne.qfty%ptyp)then
call qclose(f0,0)
end if
end if
go to 10
else if(igk.eq.eoa)then
mlin(1:srec)='propos_2'
call mput(1,1,0,0,0)
end if
igc=stib(ig+1)
if(igk.eq.-1)then
if(igc.eq.1)then
70 continue
if(stib(ig+4).eq.0)then
stib(ig+4)=ncom
stib(ig+5)=1
else
stib(ig+5)=stib(ig+5)+1
end if
if(stib(ig+4).lt.stib(ig+5))then
stib(ig+4)=0
ig=ig+1
lupt=0
lupi=0
else
lupt=1
lupi=stib(ig+5)
if((lupi.gt.d0).and.&
(lupi.le.d0+2*nsfiles))then
if((lupi.lt.d0+2*f2-1).or.&
(lupi.gt.d0+2*f2))then
go to 70
end if
end if
end if
else if(igc.eq.2)then
if(stib(ig+4).eq.0)then
stib(ig+4)=stib(frecc(0)+lupi)
stib(ig+5)=stib(irecc(0)+lupi)
else
stib(ig+5)=stib(ig+5)+1
end if
if(stib(ig+4).lt.stib(ig+5))then
stib(ig+4)=0
ig=ig+1
lupt=1
lupj=0
else
lupt=2
lupj=stib(ig+5)
end if
else if(igc.ne.-1)then
go to 80
end if
else if(igk.eq.-2)then
if(igc.eq.22)then
if(jflag(10).eq.0)then
if(kma.ge.0)then
stwbq=kma
kma=-1
end if
else if(stwbq.gt.stwbp)then
stwbq=stwbq-1
else
go to 80
end if
else
go to 80
end if
else if(igk.eq.-3)then
ip=stwbq+1
if(igc.eq.71)then
stwbq=stwbq+ndiagl
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(ip:stwbq)=stcb(ndiagp+1:ndiagp+ndiagl)
else if(igc.eq.81)then
stwbq=stwbq+qvl
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(ip:stwbq)=stcb(qvp:(qvp-1)+qvl)
else if(igc.eq.86)then
stwbq=(stwbq+1)+(qgfmq-qgfmp)
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(ip:stwbq)=stcb(qgfmp:qgfmq)
else if(abs(igc-84).eq.1)then
call date_and_time(date=dtz(1),time=dtz(2),zone=dtz(3))
j2=stwbq
do i1=1,3
dtzl=0
do i2=dtz0,1,-1
if(iachar(dtz(i1)(i2:i2)).ne.aspc)then
dtzl=i2
exit
end if
end do
if(dtzl.gt.0)then
ip=stwbq+1
stwbq=ip+dtzl
if((i1.eq.1).and.&
(dtzl.eq.8))then
j1=stdz(dtz(1),1,8)
else if((i1.eq.2).and.&
(dtzl.eq.10))then
j1=stdz(dtz(2),1,6)
if(j1.ne.0)then
j1=stdz(dtz(2),8,10)
end if
else
j1=0
end if
if(stwbq.gt.stwbu)then
go to 85
end if
if(j1.eq.0)then
dtzp=>dtz(i1)
stwb(ip:stwbq)=dtzp(1:dtzl)//achar(aspc)
else if(i1.eq.1)then
if(igc.eq.83)then
stwbq=stwbq+2
stwb(ip:stwbq)=dtz(1)(1:4)//achar(aslash)//&
dtz(1)(5:6)//achar(aslash)//dtz(1)(7:8)//achar(aspc)
else
dtzp=>dtz(1)
stwb(ip:stwbq)=dtzp(1:dtzl)//achar(aspc)
end if
else if(i1.eq.2)then
if(igc.eq.83)then
stwbq=stwbq+2
stwb(ip:stwbq)=dtz(2)(1:2)//achar(acol)//&
dtz(2)(3:4)//achar(acol)//dtz(2)(5:10)//achar(aspc)
else
dtzp=>dtz(2)
stwb(ip:stwbq)=dtzp(1:dtzl)//achar(aspc)
end if
end if
end if
end do
if(j2.lt.stwbq)then
stwbq=stwbq-1
end if
else
go to 80
end if
else if(igk.eq.-4)then
if(igc.eq.82)then
if(lupt.eq.1)then
k1=stib(irecc(0)+lupi)
k2=stib(frecc(0)+lupi)
else if(lupt.eq.2)then
k1=lupj
k2=lupj
else
go to 80
end if
do i1=k1,k2
j2=stib(drecl(0)+i1)
ip=stwbq+1
stwbq=stwbq+j2
if(stwbq.gt.stwbu)then
go to 85
end if
j1=stib(drecp(0)+i1)
stwb(ip:stwbq)=stcb(j1:j1+j2-1)
end do
else
go to 80
end if
else if(igk.eq.-6)then
if((igc.le.0).or.&
(igc.gt.nudk))then
go to 80
end if
if(stib(udkt(0)+igc).eq.1)then
ij=stib(udki(0)+igc)
if((ij.le.0).or.&
(ij.gt.ngmk))then
go to 80
end if
j1=stib(gmkvp(0)+ij)
j2=stib(gmkvl(0)+ij)
if(j2.gt.0)then
ip=stwbq+1
stwbq=stwbq+j2
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(ip:stwbq)=stcb(j1:j1+j2-1)
else if(j2.lt.0)then
go to 80
end if
else
go to 80
end if
else
go to 80
end if
ig=ig+stib(ig+2)
go to 20
80 continue
j1=stib(pske(0)+sec)
j2=j1-1+stib(wske(0)+sec)
mlin(1:srec)="runtime error while processing <"//stcb(j1:j2)//">"
call mput(1,1,0,0,0)
85 continue
call uput(7)
90 return
end subroutine propos
subroutine umpi(xx,situ)
use,non_intrinsic::qmix
use,non_intrinsic::wbuffer
implicit none
save
integer,intent(in)::xx
integer,intent(out)::situ
integer::aa(1:maxn),bb(1:maxn)
integer::i1,i2,i3,i4,ii,jj,kk
ntadp=0
situ=-1
do i1=rhop1,n
do i2=i1+1,n
if(gam(i1,i2).eq.1)then
gam(i1,i2)=0
kk=1
bb(1)=1
aa(1)=1
if(n.gt.1)then
aa(2:n)=0
end if
do i3=1,n
if(kk.eq.n)then
go to 20
end if
if(i3.gt.kk)then
gam(i1,i2)=1
if(xx.eq.1)then
go to 90
end if
ii=0
do i4=1,rho(1)
ii=ii+aa(i4)
end do
if(xx.eq.2)then
if((ii.eq.0).or.&
(ii.eq.rho(1)))then
go to 90
end if
else if(xx.eq.3)then
if((ii.eq.0).or.&
(ii.eq.rho(1)))then
ntadp=ntadp+1
xtail(ntadp)=i1
xhead(ntadp)=i2
end if
else if(xx.eq.4)then
if((ii.eq.1).or.&
(ii.eq.rho(1)-1))then
go to 90
end if
end if
go to 20
end if
jj=bb(i3)
do i4=1,jj-1
if(aa(i4).eq.0)then
if(gam(i4,jj).gt.0)then
kk=kk+1
bb(kk)=i4
aa(i4)=1
end if
end if
end do
do i4=jj+1,n
if(aa(i4).eq.0)then
if(gam(jj,i4).gt.0)then
kk=kk+1
bb(kk)=i4
aa(i4)=1
end if
end if
end do
end do
mlin(1:srec)='umpi_1'
call mput(1,1,0,0,0)
20 continue
gam(i1,i2)=1
end if
end do
end do
situ=1
90 return
end subroutine umpi
subroutine umvi(xx,situ)
use,non_intrinsic::qmix
use,non_intrinsic::qproc
use,non_intrinsic::wbuffer
implicit none
save
integer,intent(in)::xx
integer,intent(out)::situ
integer::ii,i1,i2,i3,i4,jj,j0,j1,j2,j3,kk,k1
integer::aa(1:maxn),bb(1:maxn),cc(1:maxn)
situ=1
if(xx.eq.1)then
if(nloop.eq.0)then
go to 90
end if
ii=0
if(rho(1).ne.1)then
ii=1
else if(nloop.gt.1)then
ii=1
end if
if(ii.ne.0)then
do i1=rhop1,n
if(gam(i1,i1).ne.0)then
go to 80
end if
end do
end if
else if(xx.eq.2)then
if(n-1-rhop1.le.0)then
go to 90
end if
else if(xx.eq.3)then
if(nloop.eq.0)then
go to 90
else if(rho(1).eq.0)then
go to 90
end if
go to 30
else
mlin(1:srec)='umvi_0'
call mput(1,1,0,0,0)
end if
d01: do i1=rhop1,n
if(xx.eq.1)then
kk=rho(1)
do i2=1,rho(1)
aa(i2)=1
bb(i2)=i2
end do
do i2=rhop1,n
aa(i2)=0
end do
k1=n
else if(xx.eq.2)then
kk=1
do i2=rhop1,n
aa(i2)=0
end do
if(i1.ne.rhop1)then
bb(1)=rhop1
else if(i1.ne.n)then
bb(1)=n
else
go to 90
end if
aa(bb(1))=1
k1=n-rho(1)
end if
do i2=1,k1
if(kk.eq.k1)then
cycle d01
end if
if(i2.gt.kk)then
go to 80
end if
j1=bb(i2)
if(j1.ne.i1)then
do i3=rhop1,j1-1
if(aa(i3).eq.0)then
if(gam(i3,j1).gt.0)then
kk=kk+1
bb(kk)=i3
aa(i3)=1
end if
end if
end do
do i3=j1+1,n
if(aa(i3).eq.0)then
if(gam(j1,i3).gt.0)then
kk=kk+1
bb(kk)=i3
aa(i3)=1
end if
end if
end do
end if
end do
mlin(1:srec)='umvi_1'
call mput(1,1,0,0,0)
end do d01
go to 90
30 continue
d02: do i1=rhop1,n
if(n.gt.0)then
aa(1:n)=0
end if
jj=0
kk=1
aa(i1)=-1
bb(1)=i1
d03: do i2=1,n
if(kk.eq.n)then
exit d03
else if(aa(i2).ne.0)then
cycle d03
end if
jj=jj+1
aa(i2)=jj
kk=kk+1
bb(kk)=i2
ii=kk
35 continue
j1=bb(ii)
do i3=i2+1,j1-1
if(aa(i3).eq.0)then
if(gam(i3,j1).gt.0)then
kk=kk+1
bb(kk)=i3
aa(i3)=jj
end if
end if
end do
do i3=j1+1,n
if(aa(i3).eq.0)then
if(gam(j1,i3).gt.0)then
kk=kk+1
bb(kk)=i3
aa(i3)=jj
end if
end if
end do
if(ii.lt.kk)then
if(kk.lt.n)then
ii=ii+1
go to 35
end if
end if
end do d03
if(kk.ne.n)then
mlin(1:srec)='umvi_2'
call mput(1,1,0,0,0)
end if
if(jj.gt.0)then
cc(1:jj)=0
end if
do i2=1,rho(1)
cc(aa(i2))=cc(aa(i2))+1
end do
do i2=1,jj
if(cc(i2).eq.1)then
go to 50
end if
end do
cycle d02
50 continue
j0=0
do i2=1,jj
bb(i2)=cc(i2)
if(cc(i2).eq.0)then
j0=j0+1
end if
end do
do i2=rhop1,n
if(i2.ne.i1)then
bb(aa(i2))=bb(aa(i2))+1
end if
end do
d04: do i2=1,jj
if(cc(i2).eq.1)then
j1=bb(i2)
j2=gam(i1,i1)
j3=j0
if(j1.eq.1)then
if(j2.gt.1)then
j2=j2-2
else if(j0.gt.0)then
i4=0
do i3=1,jj
if(cc(i3).eq.0)then
if(i4.eq.0)then
i4=i3
else if(bb(i3).lt.bb(i4))then
i4=i3
end if
end if
end do
j3=j3-1
j1=j1+bb(i4)
else
cycle d04
end if
end if
if(0.lt.j2+j3)then
go to 80
else if(j1+2.lt.n)then
go to 80
end if
end if
end do d04
end do d02
go to 90
80 continue
situ=-1
90 return
end subroutine umvi
integer function qbprod(im,in)
use,non_intrinsic::qintr
implicit none
save
integer,intent(in)::in
integer,intent(inout)::im
intrinsic::abs
integer::ier,j1,j2,jm,jn
jm=abs(im)
jn=abs(in)
if(jm.lt.jn)then
j1=jm
j2=jn
else
j1=jn
j2=jm
end if
if(j2.le.iref)then
ier=0
else if(j1.le.iref)then
if(irefh/j2.ge.j1)then
ier=0
else
ier=1
end if
else
ier=1
end if
if(ier.eq.0)then
qbprod=im*in
else
qbprod=0
call wput(-15,0,0)
end if
return
end function qbprod
subroutine dkar(im,iw)
use,non_intrinsic::cbuffer
use,non_intrinsic::qintr
use,non_intrinsic::wbuffer
implicit none
save
integer,intent(in)::im
integer,intent(out)::iw
integer,parameter::ib=1
integer::ii,jj,kk,jc,xm,ym
jc=stcbs(1)
if(im.lt.0)then
if(ib.eq.0)then
if(im+iref.lt.0)then
go to 80
end if
else if(im+irefh.lt.0)then
go to 80
end if
xm=-im
if(jc.ge.stcbas(1))then
call vaocb(scbuff)
end if
jc=jc+1
stcb(jc:jc)=achar(aminus)
else
if(ib.eq.0)then
if(im-iref.gt.0)then
go to 80
end if
else if(im-irefh.gt.0)then
go to 80
end if
xm=im
end if
ii=jc+1
10 continue
if(jc.ge.stcbas(1))then
call vaocb(scbuff)
end if
jc=jc+1
if(xm.lt.10)then
stcb(jc:jc)=achar(xm+azero)
else
ym=xm/10
stcb(jc:jc)=achar((xm-10*ym)+azero)
xm=ym
go to 10
end if
jj=jc
do while(ii.lt.jj)
kk=iachar(stcb(ii:ii))
stcb(ii:ii)=stcb(jj:jj)
stcb(jj:jj)=achar(kk)
ii=ii+1
jj=jj-1
end do
iw=jc-stcbs(1)
return
80 continue
call wput(-15,0,0)
end subroutine dkar
subroutine sdiag(ii,jj)
use,non_intrinsic::cbuffer
use,non_intrinsic::qintr
use,non_intrinsic::qmix
use,non_intrinsic::wbuffer
implicit none
save
integer,intent(in)::ii,jj
integer::i1,j1,xp,xl
if(ii.eq.1)then
xp=hhp
xl=hhl
else if(ii.eq.2)then
xp=noffp
xl=noffl
else if(ii.eq.3)then
xp=ndiagp
xl=ndiagl
else
mlin(1:srec)='sdiag_1'
call mput(1,1,0,0,0)
end if
if(jj.eq.0)then
do i1=xp+xl,xp+1,-1
j1=iachar(stcb(i1:i1))
if(j1.lt.anine)then
stcb(i1:i1)=achar(j1+1)
go to 90
else
stcb(i1:i1)=achar(azero)
end if
end do
if(xl.ge.wsint)then
mlin(1:srec)='sdiag_2'
call mput(1,1,0,0,0)
end if
j1=xp+1
stcb(j1:j1)=achar(azero+1)
j1=j1+xl
stcb(j1:j1)=achar(azero)
xl=xl+1
else if(jj.eq.-1)then
j1=xp+1
stcb(j1:j1)=achar(azero)
xl=1
else
mlin(1:srec)='sdiag_3'
call mput(1,1,0,0,0)
end if
if(ii.eq.1)then
hhl=xl
else if(ii.eq.2)then
noffl=xl
else if(ii.eq.3)then
ndiagl=xl
end if
90 return
end subroutine sdiag
integer function ilsd(s,j1,j2)
use,non_intrinsic::qintr
use,non_intrinsic::wbuffer
implicit none
save
integer,intent(in)::j1,j2
character(kind=ascii,len=*),intent(in)::s
integer::i1,xj1
ilsd=0
if(j1.le.0)then
ilsd=1
else if(j1.gt.j2)then
ilsd=1
else if(j2-j1.gt.wsint-1)then
ilsd=1
end if
if(ilsd.ne.0)then
go to 90
end if
ilsd=iachar(s(j1:j1))
if((ilsd.eq.aplus).or.&
(ilsd.eq.aminus))then
xj1=j1+1
else
xj1=j1
end if
ilsd=j2
do i1=xj1,j2
if(iachar(s(i1:i1)).ne.azero)then
ilsd=i1
exit
end if
end do
return
90 continue
mlin(1:srec)='ilsd_1'
call mput(1,1,0,0,0)
end function ilsd
subroutine kbp(j1,j2,k1,k2,k3)
use,non_intrinsic::cbuffer
use,non_intrinsic::qintr
use,non_intrinsic::qmodel
use,non_intrinsic::zbuffer
implicit none
save
integer,intent(in)::j1,j2
integer,intent(out)::k1,k2,k3
integer::i1,j3,j4,j5,kl
k1=0
k2=nap
k3=0
if((j1.le.0).or.&
(j1.gt.j2).or.&
(j2.gt.stcbas(1)))then
go to 90
end if
kl=j2-(j1-1)
d01: do i1=1,mll4
j3=mll(i1)
if(j3.eq.nap)then
cycle
end if
do while(stib(j3).ne.eoa)
j3=stib(j3)
k3=k3+1
j5=stib(j3+2)
if(j5.eq.kl)then
j4=stib(j3+1)
if(stcb(j1:j2).eq.stcb(j4:j4+(j5-1)))then
k1=i1
k2=j3
exit d01
end if
end if
end do
k3=0
end do d01
90 return
end subroutine kbp
subroutine vsf
use,non_intrinsic::qintr
use,non_intrinsic::qmix
use,non_intrinsic::qmodel
use,non_intrinsic::qproc
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer::i1,i2,i3,j1,j2,j3,j4,j5,j6,ii,p0,p1
integer::atyp,styp
atyp=7
ii=stib(tftic(0)+atyp)
j3=0
do i1=stib(tfta(0)+ii),stib(tftb(0)+ii)
i2=stib(tf2(0)+i1)
styp=stib(tftyp(0)+i2)
if(j3.eq.0)then
if((abs(styp).ne.atyp).or.&
(pmkd(0).eq.nap).or.&
(pmkt(0).eq.nap).or.&
(pmkvmi(0).eq.nap).or.&
(pmkvma(0).eq.nap).or.&
(pmkzp(0).eq.nap))then
mlin(1:srec)='vsf_1'
call mput(1,1,0,0,0)
end if
end if
j3=stib(stib(tfo(0)+i2)+1)
if((stib(pmkd(0)+j3).ne.1).or.&
(stib(pmkt(0)+j3).ne.4).or.&
(stib(pmkvmi(0)+j3).eq.nap).or.&
(stib(pmkvma(0)+j3).eq.nap).or.&
(stib(pmkzp(0)+j3).eq.nap))then
mlin(1:srec)='vsf_2'
call mput(1,1,0,0,0)
end if
if(styp.lt.0)then
cycle
end if
j1=stib(pmkvmi(0)+j3)
j2=stib(pmkvma(0)+j3)
if(styp.gt.0)then
if(j1.ge.0)then
if(j1.gt.stib(tfb(0)+i2))then
jflag(4)=1
end if
else if(j2.le.0)then
if(j2.lt.stib(tfa(0)+i2))then
jflag(4)=1
end if
end if
end if
if(psfi(0)==nap)then
psfi(0)=stibs(1)
call aoib(nphi+1)
stib(psfi(0)+1:stibs(1)-1)=0
stib(stibs(1))=eoa
end if
j4=stib(pmkzp(0)+j3)
j1=0
j2=0
do i3=1,nphi
j5=stib(j4+i3)
if(j5.gt.0)then
j2=j2+1
else if(j5.lt.0)then
j1=j1+1
end if
end do
if((j1.gt.0).and.&
(j2.gt.0))then
cycle
else if(j1.eq.0)then
j5=stib(tfb(0)+i2)
do i3=1,nphi
if(stib(j4+i3).gt.j5)then
jflag(14)=1
stib(psfi(0)+i3)=1
end if
end do
else if(j2.eq.0)then
j5=stib(tfa(0)+i2)
do i3=1,nphi
if(stib(j4+i3).lt.j5)then
jflag(14)=1
stib(psfi(0)+i3)=1
end if
end do
end if
end do
if(jflag(14).eq.0)then
if(psfi(0).ne.nap)then
call aoib(-(nphi+1))
psfi(0)=nap
end if
else
ii=0
do i1=1,nphi
if(stib(tpc(0)+i1).ne.phity%ext)then
if(stib(psfi(0)+i1).eq.0)then
ii=ii+1
exit
end if
end if
end do
if(ii.eq.0)then
jflag(4)=1
end if
end if
atyp=6
ii=stib(tftic(0)+atyp)
j3=0
do i1=stib(tfta(0)+ii),stib(tftb(0)+ii)
i2=stib(tf2(0)+i1)
styp=stib(tftyp(0)+i2)
if(j3.eq.0)then
if((abs(styp).ne.atyp).or.&
(vmks(0).eq.nap).or.&
(vmkt(0).eq.nap).or.&
(vmkmio(0).eq.nap).or.&
(vmkmao(0).eq.nap).or.&
(vmkzp(0).eq.nap))then
mlin(1:srec)='vsf_3'
call mput(1,1,0,0,0)
end if
end if
j3=stib(stib(tfo(0)+i2)+1)
if((stib(vmks(0)+j3).ne.1).or.&
(stib(vmkt(0)+j3).ne.4).or.&
(stib(vmkmio(0)+j3).eq.nap).or.&
(stib(vmkmao(0)+j3).eq.nap).or.&
(stib(vmkzp(0)+j3).eq.nap))then
mlin(1:srec)='vsf_4'
call mput(1,1,0,0,0)
end if
if(styp.lt.0)then
cycle
else
j1=0
j2=0
j6=0
do i3=nrho,3,-1
if(stib(nivd(0)+i3).gt.0)then
j4=stib(stib(vmkmio(0)+j3)+i3)
j5=stib(stib(vmkmao(0)+j3)+i3)
if(j6.eq.0)then
j6=1
j1=j4
j2=j5
else if(j1.gt.j4)then
j1=j4
else if(j2.lt.j5)then
j2=j5
end if
end if
end do
if(j1.ge.0)then
if(j1.gt.stib(tfb(0)+i2))then
jflag(1)=1
end if
else if(j2.le.0)then
if(j2.lt.stib(tfa(0)+i2))then
jflag(1)=1
end if
end if
end if
if(jflag(1).ne.0)then
go to 90
end if
if(vsfi(0).eq.nap)then
vsfi(0)=stibs(1)
call aoib(nvert+1)
stib(vsfi(0)+1:stibs(1)-1)=0
stib(stibs(1))=eoa
end if
j4=stib(vmkzp(0)+j3)
j1=0
j2=0
do i3=1,nvert
j5=stib(j4+i3)
if(j5.gt.0)then
j2=j2+1
else if(j5.lt.0)then
j1=j1+1
end if
end do
if((j1.gt.0).and.&
(j2.gt.0))then
cycle
else if(j1.eq.0)then
j5=stib(tfb(0)+i2)
do i3=1,nvert
if(stib(j4+i3).gt.j5)then
jflag(13)=1
stib(vsfi(0)+i3)=1
end if
end do
else if(j2.eq.0)then
j5=stib(tfa(0)+i2)
do i3=1,nvert
if(stib(j4+i3).lt.j5)then
jflag(13)=1
stib(vsfi(0)+i3)=1
end if
end do
end if
end do
if(jflag(14).ne.0)then
if(vsfi(0).eq.nap)then
vsfi(0)=stibs(1)
call aoib(nvert+1)
stib(vsfi(0)+1:stibs(1)-1)=0
stib(stibs(1))=eoa
end if
p0=stibs(1)
call aoib(nphi+1)
stib(p0+1:stibs(1)-1)=0
stib(stibs(1))=eoa
do i2=1,nleg
j1=p0+stib(link(0)+stib(leg(0)+i2))
stib(j1)=stib(j1)+1
end do
p1=stibs(1)
call aoib(nphi+1)
stib(p1+1:stibs(1)-1)=0
stib(stibs(1))=eoa
do i2=1,nvert
if(stib(vsfi(0)+i2).eq.0)then
j1=stib(vparto(0)+i2)
do i3=1,stib(vval(0)+i2)
j3=stib(j1+i3)
if(stib(psfi(0)+j3).ne.0)then
j2=p1+j3
stib(j2)=stib(j2)+1
if(stib(j2).gt.stib(p0+j3))then
stib(vsfi(0)+i2)=1
jflag(13)=1
exit
end if
end if
end do
do i3=1,stib(vval(0)+i2)
stib(p1+stib(j1+i3))=0
end do
end if
end do
call aoib(-2*(nphi+1))
end if
if(jflag(13).eq.0)then
if(vsfi(0).ne.nap)then
call aoib(-(nvert+1))
vsfi(0)=nap
end if
else
ii=nrho+1
nivdx(0)=stibs(1)
call aoib(ii)
stib(nivdx(0)+1:stibs(1)-1)=0
stib(stibs(1))=eoa
do i1=1,nvert
if(stib(vsfi(0)+i1).eq.0)then
p1=nivdx(0)+stib(vval(0)+i1)
stib(p1)=stib(p1)+1
end if
end do
ii=0
do i1=1,nrho
if(stib(nivdx(0)+i1).ne.0)then
ii=ii+1
exit
end if
end do
if(ii.eq.0)then
jflag(1)=1
end if
end if
if(jflag(14).ne.0)then
if(nleg.gt.2)then
if(nleg.gt.nrho)then
jflag(1)=1
else if(nivdx(0).ne.nap)then
if(stib(nivdx(0)+nleg).eq.0)then
jflag(1)=1
end if
end if
end if
end if
90 continue
return
end subroutine vsf
integer function stoz(s,j1,j2)
use,non_intrinsic::qintr
use,non_intrinsic::wbuffer
implicit none
save
integer,intent(in)::j1,j2
character(kind=ascii,len=*),intent(in)::s
integer::i1,jj,js
stoz=0
if(j1.le.0)then
stoz=1
else if(j1.gt.j2)then
stoz=1
else if(j2-j1.gt.wsint-1)then
stoz=1
end if
if(stoz.ne.0)then
go to 90
end if
js=iachar(s(j1:j1))
if((js.eq.aplus).or.&
(js.eq.aminus))then
i1=j1+1
else
i1=j1
end if
10 continue
jj=iachar(s(i1:i1))
if(acf(jj).ne.1)then
go to 90
end if
stoz=10*stoz+(jj-azero)
if(stoz-iref.gt.0)then
call wput(-15,0,0)
end if
if(i1.lt.j2)then
i1=i1+1
go to 10
end if
if(js.eq.aminus)then
stoz=-stoz
end if
return
90 continue
mlin(1:srec)='stoz_1'
call mput(1,1,0,0,0)
end function stoz
integer function sigz(s,ia,ib)
use,non_intrinsic::wbuffer
implicit none
save
integer,intent(in)::ia,ib
character(kind=ascii,len=*),intent(in)::s
integer::i1,ii,jj,ja
sigz=0
if(ia.le.0)then
go to 90
end if
jj=iachar(s(ia:ia))
if(jj.eq.aplus)then
ii=1
else if(jj.eq.aminus)then
ii=-1
else
ii=0
end if
ja=ia+abs(ii)
if(ja.gt.ib)then
go to 90
end if
do i1=ja,ib
jj=iachar(s(i1:i1))
if(acf(jj).ne.1)then
go to 90
else if(sigz.eq.0)then
if(jj.ne.azero)then
if(ii.lt.0)then
sigz=-1
else
sigz=1
end if
end if
end if
end do
return
90 continue
mlin(1:srec)='sigz_1'
call mput(1,1,0,0,0)
end function sigz
integer function wztos(nn)
use,non_intrinsic::qintr
use,non_intrinsic::zbuffer
implicit none
save
integer,intent(in)::nn
integer::mm,i1,j1
wztos=0
if(nn.ge.0)then
if(nn-irefh.gt.0)then
wztos=-1
else
mm=nn
end if
else
if(nn+irefh.lt.0)then
wztos=-1
else
mm=-nn
wztos=1
end if
end if
if(wztos.lt.0)then
call wput(-15,0,0)
end if
j1=ptenp
do i1=1,spten
j1=j1+1
if(mm.lt.stib(j1))then
wztos=wztos+i1
go to 90
end if
end do
wztos=wztos+spten+1
90 return
end function wztos
subroutine spak(s,ind,m,uc,nos)
use,non_intrinsic::cbuffer
use,non_intrinsic::qproc
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
character(kind=ascii,len=*),intent(in)::s
integer,intent(inout)::ind
integer,intent(in)::m,uc,nos
integer,external::stoz
integer::i1,i2,i3,j0,j1,j2,j3
integer::ii,ij,inos,jj,k,kk,sl,sp
if((m.ne.0).and.&
(m.ne.1))then
go to 80
else if((nos.ne.0).and.&
(nos.ne.1))then
go to 80
else if((uc.ne.0).and.&
(uc.ne.1))then
go to 80
end if
inos=nos
i1=0
sl=1
ii=2*srec+2
do while(iachar(s(sl:sl)).ne.ascol)
if(sl.lt.ii)then
if(i1.eq.0)then
if(iachar(s(sl:sl)).eq.acomma)then
i1=sl
end if
end if
sl=sl+1
else
go to 80
end if
end do
if(i1.eq.0)then
i1=sl
end if
if(i1.lt.3)then
go to 80
else if(sl.lt.3)then
go to 80
else if(modulo(i1,2).eq.0)then
go to 80
end if
sp=stcbs(1)+1
i1=(i1-1)/2
call aocb(i1+1)
ij=0
j2=-1
k=sp
do i2=1,i1
j2=j2+2
ii=iachar(s(j2:j2))
jj=iachar(s(j2+1:j2+1))
if((ii.lt.azero).or.&
(ii.gt.anine))then
go to 80
else if((jj.lt.azero).or.&
(jj.gt.anine))then
go to 80
end if
kk=10*ii+jj-496
if(uc.ne.0)then
if(kk.ge.abo(3))then
if(kk.le.abo(4))then
go to 80
end if
end if
end if
if(kk.ge.abo(5))then
if(kk.le.abo(6))then
ij=1
end if
end if
stcb(k:k)=achar(kk)
k=k+1
end do
stcb(k:k)=achar(alf)
if(uc.ne.0)then
if(ij.eq.0)then
go to 80
end if
end if
if(inos.ne.0)then
if(iachar(stcb(k-1:k-1)).ne.kpqs(4))then
inos=0
end if
end if
j0=stibs(1)
call aoib(2)
stib(j0+1)=sp
stib(j0+2)=i1
if(m.ne.0)then
if((nos.ne.0).or.&
(uc.ne.0))then
go to 80
end if
go to 90
end if
j2=2*i1+1
j1=j2+1
do while(j2.lt.sl)
j2=j2+1
j3=iachar(s(j2:j2))
if((j3.eq.acomma).or.&
(j3.eq.ascol))then
j2=j2-1
if(j2.lt.j1)then
go to 80
end if
call aoib(1)
stib(stibs(1))=stoz(s,j1,j2)
j2=j2+1
j1=j2+1
end if
end do
ind=ind+1
if(uc.ne.0)then
ii=stcbs(1)
call aocb(i1+1)
i3=ii+1
do i2=ii-i1,ii-1
j2=iachar(stcb(i2:i2))
if((j2.ge.abo(5)).and.&
(j2.le.abo(6)))then
j2=j2+abo(0)
end if
stcb(i3:i3)=achar(j2)
i3=i3+1
end do
stcb(i3:i3)=achar(alf)
j1=stibs(1)
jj=j1-j0
if(jj.lt.2)then
go to 80
end if
call aoib(jj)
ii=j1+1
stib(ii)=stib(ii-jj)+i1+1
do i2=2,jj
ii=ii+1
stib(ii)=stib(ii-jj)
end do
ind=ind+1
end if
if(inos.ne.0)then
j1=stibs(1)
jj=j1-j0
if(jj.lt.4)then
go to 80
end if
call aoib(jj)
ii=j1
do i2=1,jj
ii=ii+1
stib(ii)=stib(ii-jj)
end do
ii=j1+2
stib(ii)=stib(ii)-1
ind=ind+1
if(uc.ne.0)then
ii=ii+(jj/2)
stib(ii)=stib(ii)-1
ind=ind+1
end if
end if
go to 90
80 continue
mlin(1:srec)='spak_1'
call mput(1,1,0,0,0)
90 return
end subroutine spak
subroutine spok(s,aa,m,ind,uc,nos)
use,non_intrinsic::cbuffer
use,non_intrinsic::qproc
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer,parameter::dspok0=3
character(kind=ascii,len=*),intent(in)::s
integer,intent(in)::m,uc,nos
integer,intent(in)::aa(m)
integer,intent(inout)::ind
integer,external::stoz
integer::i1,j0,j1,j2,j3
integer::ii,ij,inos,jj,k,kk,sl,sp
if((m.lt.0).or.&
(m.gt.dspok0))then
go to 80
else if((nos.ne.0).and.&
(nos.ne.1))then
go to 80
else if((uc.ne.0).and.&
(uc.ne.1))then
go to 80
end if
inos=nos
j1=0
sl=1
ii=2*srec+2
do while(iachar(s(sl:sl)).ne.ascol)
if(sl.lt.ii)then
if(j1.eq.0)then
if(iachar(s(sl:sl)).eq.acomma)then
j1=sl
end if
end if
sl=sl+1
else
go to 80
end if
end do
if(j1.eq.0)then
j1=sl
end if
if(j1.lt.3)then
go to 80
else if(sl.lt.3)then
go to 80
else if(modulo(j1,2).eq.0)then
go to 80
end if
sp=stcbs(1)+1
sl=(j1-1)/2
call aocb(sl+1)
j2=-1
k=sp
ij=0
do i1=1,sl
j2=j2+2
ii=iachar(s(j2:j2))
jj=iachar(s(j2+1:j2+1))
if((ii.lt.azero).or.&
(ii.gt.anine))then
go to 80
else if((jj.lt.azero).or.&
(jj.gt.anine))then
go to 80
end if
kk=10*ii+jj-496
if(uc.ne.0)then
if(kk.ge.abo(3))then
if(kk.le.abo(4))then
go to 80
end if
end if
end if
if(kk.ge.abo(5))then
if(kk.le.abo(6))then
ij=1
end if
end if
stcb(k:k)=achar(kk)
k=k+1
end do
if(ij.eq.0)then
if(uc.ne.0)then
go to 80
end if
end if
stcb(k:k)=achar(anull)
if(inos.ne.0)then
if(iachar(stcb(k-1:k-1)).ne.kpqs(4))then
inos=0
end if
end if
j0=stibs(1)+1
call aoib(m+2)
stib(j0)=sp
stib(j0+1)=sl
if(m.eq.0)then
if(abs(nos)+abs(uc).eq.0)then
go to 90
else
go to 80
end if
end if
j0=j0+1
stib(j0+1:j0+m)=aa(1:m)
ind=ind+1
if(inos.ne.0)then
j2=stcbs(1)+1
call aocb(sl)
stcb(j2:j2+(sl-1))=stcb(sp:sp+(sl-2))//achar(anull)
j1=stibs(1)+1
call aoib(m+2)
stib(j1)=j2
j1=j1+1
stib(j1)=sl-1
stib(j1+1:j1+m)=aa(1:m)
ind=ind+1
end if
if(uc.ne.0)then
j2=stcbs(1)
kk=j2
call aocb(sl+1)
do i1=sp,sp+(sl-1)
j3=iachar(stcb(i1:i1))
if((j3.ge.abo(5)).and.&
(j3.le.abo(6)))then
j3=j3+abo(0)
end if
j2=j2+1
stcb(j2:j2)=achar(j3)
end do
stcb(stcbs(1):stcbs(1))=achar(anull)
j1=stibs(1)+1
call aoib(m+2)
stib(j1)=kk+1
j1=j1+1
stib(j1)=sl
stib(j1+1:j1+m)=aa(1:m)
ind=ind+1
end if
if((uc.ne.0).and.&
(inos.ne.0))then
j2=stcbs(1)+1
call aocb(sl)
stcb(j2:stcbs(1))=stcb(kk+1:kk+(sl-1))//achar(anull)
j1=stibs(1)+1
call aoib(m+2)
stib(j1)=j2
j1=j1+1
stib(j1)=sl-1
stib(j1+1:j1+m)=aa(1:m)
ind=ind+1
end if
go to 90
80 continue
mlin(1:srec)='spok_1'
call mput(1,1,0,0,0)
90 return
end subroutine spok
subroutine stds(s,ia,ib,iw,ja,jb)
use,non_intrinsic::cbuffer
implicit none
save
integer,intent(in)::ia,ib,iw
character(kind=ascii,len=*),intent(in)::s
integer,intent(out)::ja,jb
integer::i1,j1,ier,ii,jj,kk,ll,mm,itl,sc,sf,sl
ier=0
if(ia.le.0)then
ier=1
else if(ia.ge.ib)then
ier=1
else if(iachar(s(ia:ia)).ne.asq)then
ier=1
else if(iachar(s(ib:ib)).ne.asq)then
ier=1
else if(iw.lt.0)then
ier=1
else if(iw.gt.2)then
ier=1
end if
if(ier.ne.0)then
go to 90
end if
sc=0
sl=0
sf=0
itl=1
ii=0
ll=0
mm=0
do i1=ia,ib
jj=iachar(s(i1:i1))
if(jj.eq.asq)then
ii=ii+1
mm=1-mm
ll=0
if(ii.gt.1)then
kk=mm
else
kk=0
end if
else if(jj.eq.anull)then
if(mm.ne.0)then
go to 90
else if(ll.eq.1)then
go to 90
end if
sf=sf+1
ll=1
if(ii.gt.0)then
ii=0
sc=sc+1
end if
kk=0
else if(jj.eq.aspc)then
if(ii.gt.0)then
ii=0
if(mm.eq.0)then
sc=sc+1
end if
end if
kk=mm
else if((jj.lt.abo(1)).or.&
(jj.gt.abo(2)))then
go to 90
else
if(mm.ne.1)then
go to 90
end if
ii=0
kk=1
end if
if(kk.ne.0)then
if(iw.eq.2)then
if(jj.ne.aspc)then
itl=0
end if
end if
if((iw.eq.1).or.&
((iw.eq.2).and.(itl.eq.0)))then
sl=sl+1
call vaocb(sl)
j1=stcbs(1)+sl
stcb(j1:j1)=achar(jj)
end if
end if
end do
if(mm.ne.0)then
go to 90
end if
if(iachar(s(ib:ib)).eq.asq)then
sc=sc+1
end if
if(iw.eq.2)then
if(sl.gt.1)then
do i1=stcbs(1)+sl,stcbs(1)+1,-1
if(iachar(stcb(i1:i1)).ne.aspc)then
exit
end if
sl=sl-1
end do
end if
end if
90 continue
if(sl.gt.0)then
if(sl.lt.ib-ia-1)then
ja=stcbs(1)+1
call aocb(sl+1)
jb=stcbs(1)
stcb(jb:jb)=achar(anull)
jb=jb-1
else
ja=0
jb=0
end if
else if(sl.eq.0)then
ja=0
jb=0
else
ja=-1
jb=-1
end if
return
end subroutine stds
subroutine mstr0(s,ia,ib,pp,pl,ap,ind)
use,non_intrinsic::cbuffer
use,non_intrinsic::qintr
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer,intent(in)::ia,ib,pp,pl,ap
integer,intent(inout)::ind
character(kind=ascii,len=*),intent(in)::s
integer::ii,jj,kk,iind,ls
if(ind.ne.0)then
if(ind.ne.1)then
mlin(1:srec)='mstr0_1'
call mput(1,1,0,0,0)
end if
end if
iind=ind
if(pp.eq.nap)then
ind=-1
else if(pl.eq.nap)then
ind=-1
else
ind=0
ls=ib-ia+1
kk=1
ii=stib(pl+1)
do while(ii.ne.eoa)
if(ii.eq.ls)then
jj=stib(pp+kk)
if(stcb(jj:jj-1+ls).eq.s(ia:ib))then
ind=kk
if(iind.eq.0)then
exit
end if
end if
end if
kk=kk+1
ii=stib(pl+kk)
end do
if(ap.ne.nap)then
if(ind.gt.0)then
ind=stib(ap+ind)
end if
end if
end if
if(ind.lt.0)then
mlin(1:srec)='mstr0_2'
call mput(1,1,0,0,0)
end if
return
end subroutine mstr0
subroutine mstr1(s,ia,ib,pp,kp,kl,ind)
use,non_intrinsic::cbuffer
use,non_intrinsic::qintr
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer,intent(in)::ia,ib,pp,kp,kl
integer,intent(out)::ind
character(kind=ascii,len=*),intent(in)::s
integer::jj,kk,ls,p1
if(pp.eq.nap)then
ind=-1
else if(kp.le.0)then
ind=-1
else if(kl.le.0)then
ind=-1
else
ind=0
ls=ib-(ia-1)
kk=0
p1=stib(pp)
do while(p1.ne.eoa)
kk=kk+1
if(stib(p1+kl).eq.ls)then
jj=stib(p1+kp)
if(stcb(jj:jj-1+ls).eq.s(ia:ib))then
ind=kk
exit
end if
end if
p1=stib(p1)
end do
end if
if(ind.lt.0)then
mlin(1:srec)='mstr1_1'
call mput(1,1,0,0,0)
end if
return
end subroutine mstr1
integer function stdcfn(s,ia,ib)
use,non_intrinsic::wbuffer
implicit none
save
integer,intent(in)::ia,ib
character(kind=ascii,len=*),intent(in)::s
integer::i1,j1
if((ia.le.0).or.&
(ia.gt.ib))then
mlin(1:srec)='stdcfn_1'
call mput(1,1,0,0,0)
end if
if(acf(iachar(s(ia:ia))).le.0)then
stdcfn=0
else if(acf(iachar(s(ib:ib))).le.0)then
stdcfn=0
else
stdcfn=1
do i1=ia+1,ib-1
j1=iachar(s(i1:i1))
if(acf(j1).le.0)then
if(j1.ne.adot)then
if(j1.ne.aminus)then
if(j1.ne.ausc)then
stdcfn=0
end if
end if
end if
if(acf(iachar(s(i1+1:i1+1))).le.0)then
stdcfn=0
end if
end if
if(stdcfn.eq.0)then
exit
end if
end do
end if
return
end function stdcfn
integer function stdw(s,ia,ib)
use,non_intrinsic::wbuffer
implicit none
save
integer,intent(in)::ia,ib
character(kind=ascii,len=*),intent(in)::s
integer::i1
if((ia.le.0).or.&
(ia.gt.ib))then
mlin(1:srec)='stdw_1'
call mput(1,1,0,0,0)
end if
if(acf(iachar(s(ia:ia))).lt.2)then
stdw=0
else
stdw=1
do i1=ia+1,ib
if(acf(iachar(s(i1:i1))).lt.0)then
stdw=0
exit
end if
end do
end if
return
end function stdw
integer function stdz(s,ia,ib)
use,non_intrinsic::cbuffer
use,non_intrinsic::wbuffer
implicit none
save
integer,intent(in)::ia,ib
character(kind=ascii,len=*),intent(in)::s
integer::i1,jj
jj=0
if(ia.le.0)then
jj=1
else if(ia.gt.ib)then
jj=1
else if(ib.gt.stcbas(1))then
jj=1
end if
if(jj.ne.0)then
mlin(1:srec)='stdz_1'
call mput(1,1,0,0,0)
end if
stdz=0
jj=iachar(s(ia:ia))
if((jj.eq.aplus).or.&
(jj.eq.aminus))then
if(ia.lt.ib)then
stdz=1
end if
else if(acf(jj).eq.1)then
stdz=1
end if
if(stdz.eq.1)then
do i1=ia+1,ib
if(acf(iachar(s(i1:i1))).ne.1)then
stdz=0
exit
end if
end do
end if
return
end function stdz
subroutine nows(j1)
use,non_intrinsic::cbuffer
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer,intent(in)::j1
integer::i1,j2,j3
if((j1.le.0).or.&
(j1.ge.sjbuff))then
mlin(1:srec)='nows_1'
call mput(1,1,0,0,0)
end if
j2=0
do i1=stjb(j1),stjb(j1+1)
if(iachar(stcb(i1:i1)).eq.aspc)then
j2=j2+1
end if
end do
if(j2.gt.0)then
j3=stjb(j1+1)-stjb(j1)-(j2-1)
j2=stcbs(1)
call aocb(j3+1)
j3=j2
do i1=stjb(j1),stjb(j1+1)
if(iachar(stcb(i1:i1)).ne.aspc)then
j3=j3+1
stcb(j3:j3)=stcb(i1:i1)
end if
end do
stjb(j1)=j2+1
stjb(j1+1)=stcbs(1)-1
stcb(stcbs(1):stcbs(1))=achar(anull)
end if
return
end subroutine nows
integer function stdq(s,ia,ib)
use,non_intrinsic::wbuffer
implicit none
save
integer,intent(in)::ia,ib
character(kind=ascii,len=*),intent(in)::s
integer::i1,jj,xden,xnum,xsla
if((ia.le.0).or.&
(ia.gt.ib))then
mlin(1:srec)='stdq_1'
call mput(1,1,0,0,0)
end if
xsla=0
xnum=0
xden=0
jj=iachar(s(ia:ia))
if((jj.eq.aplus).or.&
(jj.eq.aminus))then
stdq=1
else if(acf(jj).eq.1)then
stdq=1
xnum=1
else
stdq=0
end if
if(stdq.eq.1)then
do i1=ia+1,ib
jj=iachar(s(i1:i1))
if(acf(jj).eq.1)then
if(xsla.eq.0)then
xnum=1
else if(jj.ne.azero)then
xden=1
end if
else if(jj.eq.aslash)then
if(xsla.ne.0)then
stdq=0
go to 90
end if
xsla=1
else
stdq=0
go to 90
end if
end do
if(xnum.ne.1)then
stdq=0
else if(xsla.ne.0)then
if(xden.ne.1)then
stdq=0
end if
end if
end if
90 return
end function stdq
subroutine compac
use,non_intrinsic::cbuffer
use,non_intrinsic::qfiles
use,non_intrinsic::qintr
use,non_intrinsic::qmix
use,non_intrinsic::qmodel
use,non_intrinsic::qproc
use,non_intrinsic::qsty
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer::i,ia,ii,ij,ik,il,j,jj,jk,j1,j2,k,kk,k0,other
integer::f0,f1,f2,lupi,lupj,lupt,kma
integer::ig,igc,igk,igut,ip,stwbp,stwbq,stwbt,stwbu
integer::lupti(0:maxtak)
ig=iogp(2)
if(ig.ge.iogp(3))then
go to 80
end if
f0=0
01 continue
f0=f0+1
if(f0.gt.nfiles)then
go to 90
else if(filt(f0).ne.qfty%otyp)then
if(filt(f0).ne.qfty%ptyp)then
go to 01
end if
end if
if((cflag(11).ne.0).or.&
(filp(f0).ne.nap))then
ig=ig+2
else
ig=ig+stib(ig+1)
go to 01
end if
f1=filo(f0)
f2=film(f0)
stwbp=stwbpf(f1)
stwbq=stwbqf(f1)
stwbt=stwbp+swbuff0
stwbu=stwbp+swbuff
lupti(0:maxtak)=0
kma=-1
lupt=0
10 continue
igk=stib(ig)
if(igk.gt.0)then
stwbq=stwbq+1
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(stwbq:stwbq)=achar(igk)
ig=ig+1
go to 10
end if
if(igk.gt.-3)then
igc=stib(ig+1)
if(igk.eq.-1)then
lupt=igc
if(lupt.gt.0)then
if(stib(ig+4).eq.0)then
if(lupt.eq.1)then
stib(ig+4)=inco
stib(ig+5)=0
lupti(0)=1
lupti(1)=1
else if(lupt.eq.2)then
stib(ig+4)=nleg
stib(ig+5)=inco
lupti(0)=1
lupti(1)=2
else if(lupt.eq.3)then
stib(ig+4)=nli-nleg
stib(ig+5)=0
lupti(0)=1
lupti(1)=3
else if(lupt.eq.4)then
stib(ig+4)=n-nleg
stib(ig+5)=0
lupti(0)=1
lupti(1)=4
else if(lupt.eq.5)then
stib(ig+4)=vdeg(nleg+lupi)
stib(ig+5)=0
if(lupti(0).ne.1)then
go to 80
end if
lupti(0)=2
lupti(2)=5
end if
end if
stib(ig+5)=stib(ig+5)+1
if(stib(ig+4).lt.stib(ig+5))then
stib(ig+4)=0
lupti(0)=lupti(0)-1
if(lupti(0).gt.0)then
lupt=lupti(lupti(0))
else
lupt=0
end if
ig=ig+1
else
if(lupt.eq.5)then
lupj=stib(ig+5)
else
lupi=stib(ig+5)
end if
end if
else if(lupt.ne.-1)then
go to 80
end if
else if(igk.eq.-2)then
if(igc.eq.22)then
if(jflag(10).eq.0)then
if(kma.ge.0)then
stwbq=kma
kma=-1
end if
else if(stwbq.gt.stwbp)then
stwbq=stwbq-1
else
go to 80
end if
else
go to 80
end if
else
go to 80
end if
else if(igk.gt.-5)then
igc=stib(ig+1)
if(igk.eq.-3)then
if((igc.gt.70).and.&
(igc.lt.80))then
if(igc.lt.74)then
if(igc.eq.72)then
jj=dsym
if(dsym.gt.1)then
stwbq=stwbq+2
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(stwbq-1:stwbq)=achar(azero+1)//achar(aslash)
end if
else if(igc.eq.73)then
jj=dsym
end if
else if(igc.lt.77)then
if(igc.eq.74)then
jj=nli-nleg
else if(igc.eq.75)then
jj=nleg
else if(igc.eq.76)then
jj=nloop
end if
else
if(igc.eq.77)then
jj=n-nleg
else if(igc.eq.78)then
jj=inco
else if(igc.eq.79)then
jj=nleg-inco
end if
end if
if(igc.ne.71)then
call dkar(jj,jk)
ip=stwbq+1
stwbq=stwbq+jk
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(ip:stwbq)=stcb(stcbs(1)+1:stcbs(1)+jk)
else
ip=stwbq+1
stwbq=stwbq+noffl
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(ip:stwbq)=stcb(noffp+1:noffp+noffl)
end if
else
if(igc.eq.61)then
stwbq=stwbq+1
if(stwbq.gt.stwbu)then
go to 85
end if
if(dis.gt.0)then
stwb(stwbq:stwbq)=achar(aplus)
else if(dis.lt.0)then
stwb(stwbq:stwbq)=achar(aminus)
else
go to 80
end if
else if(igc.eq.62)then
if(dis.lt.0)then
stwbq=stwbq+1
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(stwbq:stwbq)=achar(aminus)
end if
else if(igc.eq.63)then
stwbq=stwbq+1
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(stwbq:stwbq)=achar(azero+qco(qcntr%e))
else if(igc.eq.64)then
stwbq=stwbq+1
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(stwbq:stwbq)=achar(azero+qco(qcntr%t))
else if(igc.eq.65)then
stwbq=stwbq+1
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(stwbq:stwbq)=achar(azero+qco(qcntr%p))
else if(igc.eq.66)then
stwbq=stwbq+1
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(stwbq:stwbq)=achar(azero+qco(qcntr%c))
else
go to 80
end if
end if
else if(igk.eq.-4)then
if(lupt.eq.3)then
if(igc.lt.40)then
if(abs(igc-32).eq.1)then
i=lupi
j=pmap(ex(i),ey(i))
if(igc.eq.33)then
j=stib(link(0)+j)
end if
il=stib(namel(0)+j)
ip=stwbq+1
stwbq=stwbq+il
if(stwbq.gt.stwbu)then
go to 85
end if
ia=stib(namep(0)+j)
stwb(ip:stwbq)=stcb(ia:ia-1+il)
else if((igc.eq.32).or.&
(igc.eq.34))then
i=lupi
if(igc.eq.32)then
ik=1
else
ik=-1
end if
j=ey(i)
i=ex(i)
k0=stwbq
other=0
if(vmap(i,j).eq.i)then
if(ik*(lmap(i,j)-j).lt.0)then
stwbq=stwbq+1
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(stwbq:stwbq)=achar(aminus)
end if
ip=stwbq+1
stwbq=stwbq+stib(momel(0))
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(ip:stwbq)=stcb(stib(momep(0)):stib(momeq(0)))
ii=amap(i,j)
do k=nleg+1,nleg+nloop
if(flow(ii,k).ne.0)then
call dkar(k-nleg,jk)
ip=stwbq+1
stwbq=stwbq+jk
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(ip:stwbq)=stcb(stcbs(1)+1:stcbs(1)+jk)
go to 70
end if
end do
else
if(vmap(i,j).lt.i)then
ij=-ik
else
ij=ik
end if
ii=amap(i,j)
do k=nleg+1,nleg+nloop
jj=flow(ii,k)
if(jj.ne.0)then
if((other.eq.1).or.&
(ij.eq.jj))then
stwbq=stwbq+1
if(stwbq.gt.stwbu)then
go to 85
end if
if(ij.eq.jj)then
stwb(stwbq:stwbq)=achar(aminus)
else
stwb(stwbq:stwbq)=achar(aplus)
end if
end if
ip=stwbq+1
stwbq=stwbq+stib(momel(0))
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(ip:stwbq)=stcb(stib(momep(0)):stib(momeq(0)))
call dkar(k-nleg,jk)
ip=stwbq+1
stwbq=stwbq+jk
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(ip:stwbq)=stcb(stcbs(1)+1:stcbs(1)+jk)
other=1
end if
end do
do k=1,nleg
jj=flow(ii,invps1(k))
if(jj.ne.0)then
if(k.gt.inco)then
jj=-jj
end if
if((other.eq.1).or.&
(ij.eq.jj))then
stwbq=stwbq+1
if(stwbq.gt.stwbu)then
go to 85
end if
if(ij.eq.jj)then
stwb(stwbq:stwbq)=achar(aminus)
else
stwb(stwbq:stwbq)=achar(aplus)
end if
end if
ip=stwbq+1
stwbq=stwbq+stib(momel(0)+k)
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(ip:stwbq)=stcb(stib(momep(0)+k):stib(momeq(0)+k))
other=1
end if
end do
if(k0.eq.stwbq)then
stwbq=stwbq+1
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(stwbq:stwbq)=achar(azero)
end if
end if
else if(igc.eq.35)then
i=lupi
j=pmap(ex(i),ey(i))
stwbq=stwbq+1
if(stwbq.gt.stwbu)then
go to 85
end if
if(stib(antiq(0)+j).eq.0)then
stwb(stwbq:stwbq)=achar(aplus)
else
stwb(stwbq:stwbq)=achar(aminus)
end if
else
go to 80
end if
else if(igc.le.50)then
if(igc.lt.44)then
if(igc.eq.40)then
jj=3
else if(igc.eq.41)then
jj=lupi
else if(igc.eq.42)then
jj=2*lupi-1
else if(igc.eq.43)then
i=lupi
j=ey(i)
i=ex(i)
jj=iovm(i,j)
else
go to 80
end if
else if(igc.lt.47)then
if(igc.eq.44)then
i=lupi
jj=ex(i)-nleg
else if(igc.eq.45)then
jj=2*lupi
else if(igc.eq.46)then
i=lupi
j=lmap(ex(i),ey(i))
i=vmap(ex(i),ey(i))
jj=iovm(i,j)
else
go to 80
end if
else
if(igc.eq.47)then
i=lupi
jj=vmap(ex(i),ey(i))-nleg
else if(igc.eq.48)then
i=lupi
jj=vdeg(ex(i))
else if(igc.eq.49)then
i=lupi
jj=vdeg(vmap(ex(i),ey(i)))
else
go to 80
end if
end if
call dkar(jj,jk)
ip=stwbq+1
stwbq=stwbq+jk
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(ip:stwbq)=stcb(stcbs(1)+1:stcbs(1)+jk)
else
go to 80
end if
else if((lupt.eq.1).or.&
(lupt.eq.2))then
if(igc.lt.40)then
if(abs(igc-32).eq.1)then
i=lupi
j=stib(link(0)+stib(leg(0)+i))
if(igc.eq.33)then
j=stib(link(0)+j)
end if
if(lupi.gt.inco)then
j=stib(link(0)+j)
end if
il=stib(namel(0)+j)
ip=stwbq+1
stwbq=stwbq+il
if(stwbq.gt.stwbu)then
go to 85
end if
ia=stib(namep(0)+j)
stwb(ip:stwbq)=stcb(ia:ia-1+il)
else if((igc.eq.32).or.&
(igc.eq.34))then
i=lupi
if(igc.eq.34)then
stwbq=stwbq+1
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(stwbq:stwbq)=achar(aminus)
end if
ip=stwbq+1
stwbq=stwbq+stib(momel(0)+i)
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(ip:stwbq)=stcb(stib(momep(0)+i):stib(momeq(0)+i))
else if(igc.eq.35)then
i=lupi
j=stib(leg(0)+i)
stwbq=stwbq+1
if(stwbq.gt.stwbu)then
go to 85
end if
if(stib(antiq(0)+j).eq.0)then
stwb(stwbq:stwbq)=achar(aplus)
else
stwb(stwbq:stwbq)=achar(aminus)
end if
else
go to 80
end if
else if(igc.lt.50)then
if(igc.eq.40)then
i=lupi
if(i.le.inco)then
jj=1
else
jj=2
end if
else if(igc.eq.42)then
i=lupi
if(i.le.inco)then
jj=1-2*i
else
jj=2*(inco-i)
end if
else if(igc.eq.43)then
i=lupi
j=lmap(invps1(i),1)
i=vmap(invps1(i),1)
jj=iovm(i,j)
else if(igc.eq.44)then
i=lupi
jj=vmap(invps1(i),1)-nleg
else if(igc.eq.48)then
i=lupi
j=lmap(invps1(i),1)
jj=vdeg(vmap(invps1(i),1))
else
go to 80
end if
call dkar(jj,jk)
ip=stwbq+1
stwbq=stwbq+jk
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(ip:stwbq)=stcb(stcbs(1)+1:stcbs(1)+jk)
else if(igc.lt.60)then
if(igc.eq.51)then
i=lupi
else if(igc.eq.52)then
i=lupi
else if(igc.eq.53)then
i=lupi-inco
else
go to 80
end if
call dkar(i,jk)
ip=stwbq+1
stwbq=stwbq+jk
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(ip:stwbq)=stcb(stcbs(1)+1:stcbs(1)+jk)
else
go to 80
end if
else if((lupt.eq.4).or.&
(lupt.eq.5))then
if(igc.lt.40)then
if(abs(igc-32).eq.1)then
i=nleg+lupi
j=lupj
j=pmap(i,ovm(i,j))
if(igc.eq.33)then
j=stib(link(0)+j)
end if
il=stib(namel(0)+j)
ip=stwbq+1
stwbq=stwbq+il
if(stwbq.gt.stwbu)then
go to 85
end if
ia=stib(namep(0)+j)
stwb(ip:stwbq)=stcb(ia:ia-1+il)
else if(abs(igc-33).eq.1)then
i=nleg+lupi
j=lupj
j=ovm(i,j)
ij=vmap(i,j)
if(igc.eq.32)then
kk=1
else
kk=-1
end if
k0=stwbq
other=0
if(ij.le.nleg)then
ij=ps1(ij)
if(ij.gt.inco)then
kk=-kk
end if
if(kk.lt.0)then
stwbq=stwbq+1
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(stwbq:stwbq)=achar(aminus)
end if
ip=stwbq+1
stwbq=stwbq+stib(momel(0)+ij)
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(ip:stwbq)=stcb(stib(momep(0)+ij):stib(momeq(0)+ij))
else if(ij.eq.i)then
if(kk*(lmap(i,j)-j).lt.0)then
stwbq=stwbq+1
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(stwbq:stwbq)=achar(aminus)
end if
ip=stwbq+1
stwbq=stwbq+stib(momel(0))
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(ip:stwbq)=stcb(stib(momep(0)):stib(momeq(0)))
ii=amap(i,j)
do k=nleg+1,nleg+nloop
if(flow(ii,k).ne.0)then
call dkar(k-nleg,jk)
ip=stwbq+1
stwbq=stwbq+jk
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(ip:stwbq)=stcb(stcbs(1)+1:stcbs(1)+jk)
go to 70
end if
end do
else
if(ij.lt.i)then
ik=-kk
else
ik=kk
end if
ii=amap(i,j)
do k=nleg+1,nleg+nloop
jj=flow(ii,k)
if(jj.ne.0)then
if((other.eq.1).or.&
(ik.eq.jj))then
stwbq=stwbq+1
if(stwbq.gt.stwbu)then
go to 85
end if
if(ik.eq.jj)then
stwb(stwbq:stwbq)=achar(aminus)
else
stwb(stwbq:stwbq)=achar(aplus)
end if
end if
ip=stwbq+1
stwbq=stwbq+stib(momel(0))
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(ip:stwbq)=stcb(stib(momep(0)):stib(momeq(0)))
call dkar(k-nleg,jk)
ip=stwbq+1
stwbq=stwbq+jk
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(ip:stwbq)=stcb(stcbs(1)+1:stcbs(1)+jk)
other=1
end if
end do
do k=1,nleg
jj=flow(ii,invps1(k))
if(jj.ne.0)then
if(k.gt.inco)then
jj=-jj
end if
if((other.eq.1).or.&
(ik.eq.jj))then
stwbq=stwbq+1
if(stwbq.gt.stwbu)then
go to 85
end if
if(ik.eq.jj)then
stwb(stwbq:stwbq)=achar(aminus)
else
stwb(stwbq:stwbq)=achar(aplus)
end if
end if
ip=stwbq+1
stwbq=stwbq+stib(momel(0)+k)
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(ip:stwbq)=stcb(stib(momep(0)+k):stib(momeq(0)+k))
other=1
end if
end do
if(k0.eq.stwbq)then
stwbq=stwbq+1
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(stwbq:stwbq)=achar(azero)
end if
end if
else if(igc.eq.35)then
i=nleg+lupi
j=lupj
j=pmap(i,ovm(i,j))
stwbq=stwbq+1
if(stwbq.gt.stwbu)then
go to 85
end if
if(stib(antiq(0)+j).eq.0)then
stwb(stwbq:stwbq)=achar(aplus)
else
stwb(stwbq:stwbq)=achar(aminus)
end if
end if
else if(igc.le.50)then
if(igc.eq.41)then
i=nleg+lupi
j=lupj
j=ovm(i,j)
ij=vmap(i,j)
if(ij.le.nleg)then
ij=ps1(ij)
if(ij.le.inco)then
jj=1-2*ij
else
jj=2*(inco-ij)
end if
else
jj=amap(i,j)-nleg
end if
else if(igc.lt.46)then
if(igc.eq.40)then
i=nleg+lupi
j=lupj
j=ovm(i,j)
ij=vmap(i,j)
if(ij.le.nleg)then
if(ps1(ij).le.inco)then
jj=1
else
jj=2
end if
else
jj=3
end if
else if((igc.eq.42).or.&
(igc.eq.45))then
i=nleg+lupi
j=lupj
j=ovm(i,j)
ij=vmap(i,j)
if(ij.le.nleg)then
ij=ps1(ij)
if(ij.le.inco)then
jj=1-2*ij
else
jj=2*(inco-ij)
end if
else
kk=amap(i,j)-nleg
if((ex(kk).eq.i).and.&
(ey(kk).eq.j))then
jj=2*kk-1
else
jj=2*kk
end if
end if
if(igc.eq.45)then
if(jj.gt.0)then
jj=jj-1+2*modulo(jj,2)
else
jj=0
end if
end if
else if(igc.eq.43)then
jj=lupj
else if(igc.eq.44)then
jj=lupi
else
go to 80
end if
else
if(igc.eq.46)then
i=nleg+lupi
j=lupj
ij=ovm(i,j)
ii=vmap(i,ij)
if(ii.gt.nleg)then
jj=iovm(ii,lmap(i,ij))
else
jj=0
end if
else if(igc.eq.47)then
i=nleg+lupi
j=lupj
jj=vmap(i,ovm(i,j))-nleg
if(jj.lt.0)then
jj=0
end if
else if(igc.eq.48)then
jj=vdeg(nleg+lupi)
else if(igc.eq.49)then
i=nleg+lupi
j=lupj
jj=vmap(i,ovm(i,j))
if(jj.le.nleg)then
jj=0
else
jj=vdeg(jj)
end if
else
go to 80
end if
end if
call dkar(jj,jk)
ip=stwbq+1
stwbq=stwbq+jk
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(ip:stwbq)=stcb(stcbs(1)+1:stcbs(1)+jk)
else
go to 80
end if
else
go to 80
end if
else
go to 80
end if
else if(igk.eq.-6)then
igc=stib(ig+1)
if((igc.le.0).or.&
(igc.gt.nudk))then
go to 80
end if
igut=stib(udkt(0)+igc)
if(igut.eq.1)then
ij=stib(udki(0)+igc)
if((ij.le.0).or.&
(ij.gt.ngmk))then
go to 80
end if
j1=stib(gmkvp(0)+ij)
j2=stib(gmkvl(0)+ij)
if(j2.gt.0)then
ip=stwbq+1
stwbq=stwbq+j2
if(stwbq.gt.stwbu)then
go to 85
end if
stwb(ip:stwbq)=stcb(j1:j1-1+j2)
else if((j2.lt.0).or.&
(stib(ig+3).ne.0))then
go to 80
end if
else if(igut.eq.2)then
if(lupt.eq.1)then
i=lupi
j=stib(link(0)+stib(leg(0)+i))
else if(lupt.eq.2)then
i=lupi
j=stib(leg(0)+i)
else if(lupt.eq.3)then
i=lupi
j=pmap(ex(i),ey(i))
else if(lupt.eq.4)then
go to 80
else if(lupt.eq.5)then
i=nleg+lupi
j=lupj
j=pmap(i,ovm(i,j))
else
go to 80
end if
if(stib(ig+3).ne.0)then
j=stib(link(0)+j)
end if
if((j.le.0).or.&
(j.gt.nphi))then
go to 80
end if
ij=stib(udki(0)+igc)
if((ij.le.0).or.&
(ij.gt.npmk))then
go to 80
end if
il=stib(stib(pmkvflp(0)+j)+ij)
if(il.gt.0)then
ip=stwbq+1
stwbq=stwbq+il
if(stwbq.gt.stwbu)then
go to 85
end if
ia=stib(stib(pmkvfpp(0)+j)+ij)
stwb(ip:stwbq)=stcb(ia:ia-1+il)
else if(il.lt.0)then
go to 80
end if
else if(igut.eq.3)then
if((lupt.eq.1).or.&
(lupt.eq.2))then
i=lupi
jj=vmap(invps1(i),1)
else if(lupt.eq.3)then
i=lupi
if(stib(ig+3).eq.0)then
jj=ex(i)
else
jj=vmap(ex(i),ey(i))
end if
else if(lupt.eq.4)then
jj=nleg+lupi
else if(lupt.eq.5)then
jj=nleg+lupi
else
go to 80
end if
j=stib(vfo(jj))
ij=stib(udki(0)+igc)
if((ij.le.0).or.&
(ij.gt.nvmk))then
go to 80
else if((j.le.0).or.&
(j.gt.nvert))then
go to 80
end if
il=stib(stib(vmkvlp(0)+ij)+j)
if(il.gt.0)then
ip=stwbq+1
stwbq=stwbq+il
if(stwbq.gt.stwbu)then
go to 85
end if
ia=stib(stib(vmkvpp(0)+ij)+j)
stwb(ip:stwbq)=stcb(ia:ia-1+il)
else if(il.lt.0)then
go to 80
end if
else
go to 80
end if
else if(igk.eq.eosf)then
if(lupt.ne.0)then
go to 80
end if
if(stwbq.gt.stwbp)then
if(stwbq.lt.stwbt)then
stwbqf(f1)=stwbq
else if(stwbq.lt.stwbu)then
stwbqf(f1)=stwbq
call qout(f0)
stwbqf(f1)=stwbp
else
go to 80
end if
end if
go to 01
else
go to 80
end if
70 continue
ig=ig+stib(ig+2)
go to 10
80 continue
mlin(1:srec)="run-time error while processing <diagram>"
call mput(1,1,0,0,0)
85 continue
call uput(7)
90 return
end subroutine compac
subroutine qifi
use,non_intrinsic::cbuffer
use,non_intrinsic::qfiles
use,non_intrinsic::qintr
use,non_intrinsic::wbuffer
implicit none
save
integer::i1,i2,j1,ifile
ifile=0
do i1=1,nfiles
if(filt(i1).eq.qfty%ctyp)then
ifile=i1
exit
end if
end do
if(dso.ne.0)then
do i1=2,nfiles-1
if(filt(i1).eq.qfty%otyp)then
if(filt(i1+dso).ne.qfty%styp)then
mlin(1:srec)="wrong style/output file pairing"
call mput(1,1,0,0,ifile)
else if(nsfiles.gt.1)then
if(filp(i1).eq.nap)then
if(filp(i1+dso).eq.nap)then
mlin(1:srec)="null style-file/output-file pair"
call mput(1,1,0,0,ifile)
end if
end if
end if
if(cflag(11).ne.0)then
if(filp(i1).eq.nap)then
if(filp(i1+dso).ne.nap)then
filt(i1)=qfty%ptyp
filp(i1)=0
npfiles=npfiles+1
end if
end if
end if
end if
end do
end if
do i1=1,nfiles-1
if((filp(i1).ne.nap).and.&
(filp(i1).ne.0))then
j1=filq(i1)-filp(i1)
do i2=i1+1,nfiles
if(filp(i2).ne.nap)then
if(j1.eq.filq(i2)-filp(i2))then
if(stcb(filp(i1):filq(i1))&
.eq.stcb(filp(i2):filq(i2)))then
mlin(1:srec)="input files should be distinct"
call mput(1,1,0,0,0)
end if
end if
end if
end do
end if
end do
do i1=1,qfty%ubo
j1=0
do i2=1,nfiles
if(filt(i2).eq.i1)then
j1=j1+1
film(i2)=j1
end if
end do
end do
if(cflag(11).ne.0)then
j1=0
do i1=1,nfiles
if((filt(i1).eq.qfty%otyp).or.&
(filt(i1).eq.qfty%ptyp))then
j1=j1+1
film(i1)=j1
end if
end do
end if
j1=0
do i1=1,nfiles
if(filt(i1).eq.qfty%ptyp)then
j1=j1+1
filo(i1)=j1
end if
end do
do i1=1,nfiles
if(filt(i1).eq.qfty%otyp)then
j1=j1+1
filo(i1)=j1
end if
end do
return
end subroutine qifi
subroutine style
use,non_intrinsic::cbuffer
use,non_intrinsic::qfiles
use,non_intrinsic::qintr
use,non_intrinsic::qsty
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer,external::styki,udstyk
character(kind=ascii,len=srec)::styb
integer::f0,f1,ieosf,iout,isec,lgm,qc,slin
integer::ii,ij,i1,jj,j1,j2,kp1,kp2,k1,k2
integer::clin(1:msfiles0),nlin(1:msfiles0),cccf(1:msfiles0)
integer::dlin(1:msfiles0),ddlin(1:msfiles0)
j1=0
do i1=1,nfiles
if(filt(i1).eq.qfty%styp)then
if(filp(i1).ne.nap)then
call qopen(i1)
end if
j1=j1+1
nlin(j1)=0
dlin(j1)=0
ddlin(j1)=0
end if
end do
ks=0
call aoib(2)
udkp(1)=stibs(1)-1
stib(udkp(1))=eoa
stib(stibs(1))=eoa
isec=-1
cccf(:)=-1
04 continue
if(isec.gt.0)then
call aoib(1)
stib(stibs(1))=eosf
end if
isec=isec+1
if(isec.gt.4)then
call rsfki
go to 90
end if
if(isec.gt.0)then
iogp(isec)=stibs(1)+1
end if
ieosf=0
f0=1
10 continue
f0=f0+1
if(f0.gt.nfiles)then
go to 04
else if(filt(f0).ne.qfty%styp)then
go to 10
else if(filp(f0).eq.nap)then
go to 10
end if
if(filp(f0-dso).ne.nap)then
iout=cflag(1)
else
iout=0
end if
f1=film(f0)
if(abs(isec-2).le.1)then
call aoib(2)
if(f1.gt.1)then
stib(ieosf)=stibs(1)-ieosf
end if
ieosf=stibs(1)
stib(ieosf-1)=eosf
end if
if(isec.eq.2)then
if(ddlin(f1).eq.3)then
go to 80
end if
if(filt(f0-dso).eq.qfty%otyp)then
if(filp(f0-dso).ne.nap)then
if(cflag(6).ne.0)then
if(modulo(ddlin(f1),2).eq.0)then
mlin(1:srec)="does not contain expected mark"
call mput(1,1,0,0,-f0)
end if
end if
end if
end if
end if
clin(f1)=0
jflag(10)=1
20 continue
slin=0
call qrr(f0,nlin(f1),slin,qc)
if(slin.lt.0)then
if(isec.lt.4)then
mlin(1:srec)="is incomplete"
call mput(1,1,0,0,-f0)
end if
go to 10
else if(slin.eq.0)then
if(abs(isec-2).le.1)then
call aoib(1)
stib(stibs(1))=alf
end if
go to 20
end if
styb(1:slin+1)=stcb(stcbs(1)+1:stcbs(1)+slin)//achar(alf)
ij=0
if(clin(f1).eq.0)then
j1=iachar(styb(1:1))
j2=iachar(styb(slin:slin))
if((j1.eq.alt).and.&
(j2.eq.agt))then
ij=styki(styb,2,slin-1,isec,iout)
if((ij.gt.0).and.&
(ij.ne.24).and.&
(ij.ne.25))then
if(stib(kla(0)+ij).eq.0)then
if(ij.eq.isec+1)then
if(ks.ne.0)then
go to 80
end if
if(ij.eq.1)then
dlin(f1)=nlin(f1)+1
end if
go to 10
else
go to 80
end if
end if
end if
end if
end if
if(abs(isec-2).eq.2)then
ii=1
if(cccf(f1).gt.0)then
if(qc.ne.0)then
ii=0
else
go to 80
end if
else if(cccf(f1).lt.0)then
if(qc.ne.0)then
cccf(f1)=1
ii=0
end if
end if
if(ii.eq.0)then
go to 20
end if
j1=iachar(styb(1:1))
j2=iachar(styb(slin:slin))
if((j1.eq.alt).and.&
(j2.eq.agt))then
ii=styki(styb,2,slin-1,isec,iout)
if(ii.eq.24)then
if(cccf(f1).lt.0)then
cccf(f1)=0
end if
if(clin(f1).ne.0)then
go to 80
else
clin(f1)=nlin(f1)
go to 20
end if
else if(ii.eq.25)then
if(cccf(f1).ne.0)then
go to 80
else if(clin(f1).eq.0)then
go to 80
else
clin(f1)=0
go to 20
end if
else if((isec.eq.0).and.&
(clin(f1).eq.0).and.&
(ii.eq.1))then
go to 10
else if(cccf(f1).ne.0)then
go to 80
else
go to 20
end if
end if
else if(clin(f1).ne.0)then
go to 80
end if
j1=0
j2=0
k1=0
k2=0
kp1=0
kp2=0
lgm=0
do i1=1,slin
jj=iachar(styb(i1:i1))
if((j2.eq.0).and.&
(jj.eq.alt))then
j1=j1+1
if(j1.eq.2)then
call aoib(1)
stib(stibs(1))=jj
j1=0
else if(iachar(styb(i1+1:i1+1)).ne.alt)then
if(lgm.ne.0)then
go to 80
end if
lgm=1-lgm
kp1=i1+1
end if
else if((j1.eq.0).and.&
(jj.eq.alsbr))then
j2=j2+1
if(j2.eq.2)then
call aoib(1)
stib(stibs(1))=jj
j2=0
else if(iachar(styb(i1+1:i1+1)).ne.alsbr)then
if(j1.ne.0)then
go to 80
else if(lgm.ne.0)then
go to 80
end if
lgm=1-lgm
kp1=i1+1
end if
else if((j2.eq.0).and.&
(jj.eq.agt))then
if(j1.eq.0)then
k1=k1+1
if(k1.eq.2)then
call aoib(1)
stib(stibs(1))=jj
k1=0
else if(iachar(styb(i1+1:i1+1)).ne.agt)then
go to 80
end if
else
if(lgm.eq.0)then
go to 80
end if
lgm=1-lgm
j1=0
kp2=i1-1
if(kp2-kp1.lt.0)then
go to 80
end if
ii=styki(styb,kp1,kp2,isec,iout)
if(ii.eq.86)then
if(isec.ne.1)then
go to 80
else if(nlin(f1).ne.dlin(f1))then
go to 80
end if
if((ddlin(f1).eq.0).or.&
(ddlin(f1).eq.2))then
ddlin(f1)=ddlin(f1)+1
end if
else if(nlin(f1).eq.dlin(f1))then
if(ddlin(f1).lt.2)then
ddlin(f1)=ddlin(f1)+2
end if
end if
if(ii.le.0)then
go to 80
else if(ii.lt.5)then
if((kp1.ne.2).or.&
(kp2+1.ne.slin))then
go to 80
else if((ks.ne.0).or.&
(clin(f1).ne.0))then
go to 80
end if
else if(ii.eq.24)then
if(clin(f1).ne.0)then
go to 80
end if
if(abs(isec-2).eq.2)then
if((kp1.ne.2).or.&
(kp2+1.ne.slin))then
go to 80
end if
end if
clin(f1)=nlin(f1)
else if(ii.eq.25)then
if(clin(f1).eq.0)then
go to 80
end if
if(abs(isec-2).eq.2)then
if((kp1.ne.2).or.&
(kp2+1.ne.slin))then
go to 80
end if
end if
clin(f1)=0
else if(clin(f1).eq.0)then
call ktoc(isec,ii,nlin(f1),f0)
else
mlin(1:srec)='style_2'
call mput(1,1,0,0,0)
end if
end if
else if((j1.eq.0).and.&
(jj.eq.arsbr))then
if(j2.eq.0)then
k2=k2+1
if(k2.eq.2)then
call aoib(1)
stib(stibs(1))=jj
k2=0
else if(iachar(styb(i1+1:i1+1)).ne.arsbr)then
go to 80
end if
else
j2=0
if(j1.ne.0)then
go to 80
else if(lgm.eq.0)then
go to 80
end if
lgm=1-lgm
kp2=i1-1
if(kp2-kp1.lt.0)then
go to 80
end if
if(clin(f1).ne.0)then
mlin(1:srec)='style_3'
end if
ii=udstyk(styb,kp1,kp2)
if(ii.eq.0)then
go to 80
end if
end if
else if((j1.eq.0).and.&
(j2.eq.0))then
if(clin(f1).eq.0)then
call aoib(1)
stib(stibs(1))=jj
else if(jj.ne.aspc)then
if(abs(isec-2).lt.2)then
go to 80
end if
end if
end if
end do
if(lgm.ne.0)then
go to 80
end if
if(clin(f1).ne.0)then
go to 20
end if
call aoib(1)
stib(stibs(1))=alf
go to 20
80 continue
mlin(1:srec)="wrong syntax,"
if(clin(f1).eq.0)then
ii=nlin(f1)
else
ii=clin(f1)
end if
call mput(1,1,ii,nlin(f1),f0)
90 continue
return
end subroutine style
subroutine ktoc(isec,kco,nlin,ifile)
use,non_intrinsic::ikeys
use,non_intrinsic::qintr
use,non_intrinsic::qsty
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer,intent(in)::isec,kco,nlin,ifile
integer,external::lupty
integer::astak(1:maxtak)
integer::i1,ii,ik,j1,j2,ka,kk,lupt
ka=-1
do i1=1,nske
if(stib(kc(0)+i1).eq.kco)then
ik=i1
ka=stib(kla(0)+i1)
j1=stib(pske(0)+i1)-1
j2=stib(wske(0)+i1)
exit
end if
end do
ii=0
if(max(3,5,2,6).gt.maxpot)then
ii=1
else if(abs(isec-2).gt.1)then
ii=1
else if(abs(ka-3).gt.2)then
ii=1
end if
if(ii.ne.0)then
mlin(1:srec)='ktoc_1'
call mput(1,1,0,0,0)
end if
kk=stib(klo(0)+i1)
if(ks.eq.0)then
if(kk.ne.0)then
go to 80
end if
else if(kk.ne.0)then
do i1=ks,1,-1
if(modulo(kk,pot(tak(i1))).ge.pot(tak(i1)-1))then
go to 70
end if
end do
go to 80
70 continue
else if(ks.ne.0)then
if(ka.eq.1)then
go to 80
end if
end if
if(ka.eq.1)then
lupt=lupty(kco)
if(lupt.gt.0)then
if(ks.ge.maxtak)then
mlin(1:srec)="too many nested loops,"
call mput(1,1,nlin,nlin,ifile)
end if
if((lupt.eq.2).and.&
(abs(isec-2).eq.1))then
if(tak(1).ne.1)then
go to 80
end if
else if(lupt.eq.5)then
if(tak(1).ne.4)then
go to 80
end if
else if(lupt.ne.6)then
if(ks.ne.0)then
go to 80
end if
end if
ii=stibs(1)+1
call aoib(6)
stib(ii)=-ka
stib(ii+1)=lupt
stib(ii+2)=6
stib(ii+3)=0
stib(ii+4)=0
stib(stibs(1))=0
ks=ks+1
astak(ks)=ii
tak(ks)=lupt
else if(lupt.eq.-1)then
if(ks.le.0)then
go to 80
end if
call aoib(3)
ii=stibs(1)-astak(ks)
stib(stibs(1)-2)=-ka
stib(stibs(1)-1)=-1
stib(stibs(1))=2-ii
stib(astak(ks)+3)=ii
ks=ks-1
end if
else if(ka.eq.5)then
call aoib(1)
stib(stibs(1))=alf
else
call aoib(3)
stib(stibs(1)-2)=-ka
stib(stibs(1)-1)=kco
stib(stibs(1))=3
end if
if(stib(bbc(0)+ik).ne.0)then
jflag(10)=0
end if
go to 90
80 continue
mlin(1:srec)="wrong syntax,"
call mput(1,1,nlin,nlin,ifile)
90 return
end subroutine ktoc
integer function styki(s,s1,s2,level,iout)
use,non_intrinsic::ikeys
use,non_intrinsic::qintr
use,non_intrinsic::qsty
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer,intent(in)::s1,s2,level,iout
character(kind=ascii,len=*),intent(in)::s
integer::ij
if(s1.gt.s2)then
mlin(1:srec)='styki_1'
call mput(1,1,0,0,0)
end if
styki=0
ij=0
call mstr0(s,s1,s2,pske(0),wske(0),aske(0),ij)
if(ij.le.0)then
go to 90
end if
styki=stib(kc(0)+ij)
if(styki.le.4)then
if(styki.ne.level+1)then
styki=0
end if
else if(abs(level-2)-1.le.0)then
if(modulo(stib(kse(0)+ij),pot(level)).lt.pot(level-1))then
styki=0
end if
end if
if(iout.gt.0)then
if(abs(styki-33).eq.1)then
jflag(17)=1
end if
end if
90 return
end function styki
integer function udstyk(s,s1,s2)
use,non_intrinsic::cbuffer
use,non_intrinsic::qintr
use,non_intrinsic::qmodel
use,non_intrinsic::qsty
use,non_intrinsic::zbuffer
implicit none
save
integer,intent(in)::s1,s2
character(kind=ascii,len=*),intent(in)::s
integer,external::stdw
integer::i1,ii,j1,j2,j3,jj,kd,kt,kstep
udstyk=0
jj=ks
if(jj.eq.0)then
kt=4
else
if(tak(jj).eq.6)then
jj=jj-1
end if
kt=max(tak(jj)+3,5)
if(kt.gt.8)then
go to 90
end if
end if
kd=0
j1=s1-1+dprefl
if(s2.ge.j1)then
if(s(s1:j1).eq.stcb(dprefp:dprefp-1+dprefl))then
if(s2.gt.j1)then
kd=1
else
go to 90
end if
end if
end if
j1=s1+kd*dprefl
jj=s2-j1+1
if(stdw(s,j1,s2).eq.0)then
go to 90
end if
if(nudk.gt.0)then
call mstr1(s,j1,s2,udkp(1),1,2,udstyk)
end if
j2=stibs(1)
kstep=4
call aoib(kstep)
stib(j2+1)=-6
if(udstyk.eq.0)then
nudk=nudk+1
stib(j2+2)=nudk
else
stib(j2+2)=udstyk
end if
stib(j2+3)=kstep
stib(j2+4)=kd
ii=udkp(1)
if(udstyk.ne.0)then
do i1=1,udstyk
ii=stib(ii)
end do
j2=ii-1+kt
if(stib(j2).eq.0)then
stib(j2)=1+kd
else if(stib(j2).eq.2-kd)then
stib(j2)=3
end if
go to 90
end if
udstyk=stib(j2+2)
do i1=2,nudk
ii=stib(ii)
end do
j3=stcbs(1)+1
call aocb(jj+1)
stcb(j3:stcbs(1))=s(j1:s2)//achar(alf)
j2=stibs(1)
kstep=8
call aoib(kstep)
stib(j2-1)=stib(j2-1)+kstep
stib(ii)=j2+1
stib(j2+1)=eoa
stib(j2+2)=j3
stib(j2+3)=jj
stib(j2+4:j2+8)=0
stib(j2+kt)=kd+1
90 return
end function udstyk
integer function lupty(ll)
use,non_intrinsic::wbuffer
implicit none
save
integer,intent(in)::ll
integer,parameter::lup1=11,lup2=19
if((ll.lt.lup1).or.&
(ll.gt.lup2))then
mlin(1:srec)='lupty_1'
call mput(1,1,0,0,0)
end if
if(ll.ne.lup2)then
if(ll.lt.13)then
lupty=ll-10
else
lupty=ll-12
end if
else
lupty=-1
end if
return
end function lupty
subroutine stpa(ia,ib,jf)
use,non_intrinsic::cbuffer
use,non_intrinsic::qfiles
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer,intent(in)::ia,ib,jf
integer,external::stdw,stdz,stdq
integer::i1,i2,ic,icc,ier,incr,it,itt,j1,j2,j3
integer::sts,st0,s0,splic,dplic,bplic
if((ia.le.0).or.&
(ib.lt.ia))then
mlin(1:srec)='stpa_1'
call mput(1,1,0,0,0)
end if
sts=1
call vaojb(sts)
st0=stjbs+sts
s0=st0
stjb(s0)=0
j1=0
j2=0
splic=0
dplic=0
bplic=0
i1=ia-1
ic=0
incr=1
ier=0
do while(i1.lt.ib)
i1=i1+incr
incr=1
if(i1.le.ib)then
ic=iachar(stcb(i1:i1))
if(ic.eq.alf)then
if(bplic.ne.0)then
ier=1
exit
end if
end if
end if
icc=0
itt=0
if(i1.lt.ib)then
do i2=1,ncrs0
if(ic.eq.crs(i2,1))then
if(iachar(stcb(i1+1:i1+1)).eq.crs(i2,2))then
if(crs(i2,csyfty(jf)).ne.0)then
icc=crs(i2,3)
itt=crs(i2,4)
end if
exit
end if
end if
end do
end if
if(bplic.eq.0)then
if(icc.eq.dsq)then
icc=0
itt=0
else if(icc.ne.0)then
incr=incr+1
end if
else if(splic.ne.0)then
if(icc.eq.dsq)then
incr=incr+1
j2=j2+1
cycle
end if
end if
if(bplic.eq.0)then
if(itt.eq.0)then
itt=stib(punct1(0)+ic)
icc=ic
end if
if(itt.le.0)then
if(j1.eq.0)then
j1=i1
if(ic.eq.asq)then
splic=1
bplic=1
else if(ic.eq.adq)then
dplic=1
bplic=1
end if
else
j3=0
if(ic.eq.asq)then
j3=1
else if(ic.eq.adq)then
j3=1
end if
if(j3.ne.0)then
if(i1.gt.ia)then
if(ic.eq.iachar(stcb(i1-1:i1-1)))then
j3=0
end if
end if
end if
if(j3.ne.0)then
ier=1
exit
end if
end if
j2=i1
else
if(j2.ne.0)then
it=0
if(splic.ne.0)then
it=stty%sqs
splic=0
else if(dplic.ne.0)then
it=stty%dqs
dplic=0
else if(j1.eq.j2)then
it=abs(stib(punct1(0)+iachar(stcb(j1:j1))))
end if
if(it.eq.0)then
if(stdw(stcb,j1,j2).ne.0)then
it=stty%id
else if(stdz(stcb,j1,j2).ne.0)then
it=stty%z
else if(stdq(stcb,j1,j2).ne.0)then
it=stty%q
else if((j1.lt.j2).and.&
((iachar(stcb(j2:j2)).eq.aminus).or.&
(iachar(stcb(j2:j2)).eq.aplus)))then
if(stdz(stcb,j1,j1).ne.0)then
if(stdz(stcb,j1,j2-1).ne.0)then
it=stty%ns
end if
end if
else
ier=1
exit
end if
end if
sts=sts+3
call vaojb(sts)
st0=stjbs+sts
stjb(st0-2)=it
stjb(st0-1)=j1
stjb(st0)=j2
stjb(s0)=stjb(s0)+1
j1=0
j2=0
end if
if((icc.ne.aspc).and.&
(icc.ne.alf))then
sts=sts+3
call vaojb(sts)
st0=stjbs+sts
stjb(st0-2)=stty%p
stjb(st0-1)=icc
stjb(st0)=i1
stjb(s0)=stjb(s0)+1
end if
end if
else
if(splic.ne.0)then
if(ic.eq.asq)then
bplic=1-bplic
end if
else if(dplic.ne.0)then
if(ic.eq.adq)then
bplic=1-bplic
end if
end if
j2=i1
end if
end do
if(abs(bplic)+abs(splic)+abs(dplic).ne.0)then
ier=1
end if
if(ier.ne.0)then
stjb(stjbs+1)=-1
else
call aojb(sts)
end if
return
end subroutine stpa
subroutine qmatcha(jk1,jk2,sm,sml)
use,non_intrinsic::qintr
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer,intent(in)::jk1,jk2,sml
character(kind=ascii,len=sml),intent(in)::sm
integer,external::sttyf
integer,parameter::dpqs(1:10)=[100,112,113,115,40,92,41,91,124,93]
integer,parameter::jdpqs(0:10)=[0,3,3,0,3,0,0,0,0,0,0]
integer,parameter::kdpqs(1:10)=[2,2,1,2,1,1,1,1,1,1]
integer,parameter::istz(1:9)=[105,115,116,122,42,43,45,49,50]
integer::qstak(0:17)
integer::ic1,ic2,i1,j0,j1,j2,j3,jk3,ll,mf,pf,qs
ll=0
do while(ll.lt.sml)
ll=ll+1
if(sm(ll:ll).eq.achar(anull))then
ll=ll-1
exit
end if
end do
if((ll.eq.0).or.&
(ll.ge.sml))then
mlin(1:srec)='qmatcha_1'
call mput(1,1,0,0,0)
end if
jbco(0)=0
qstak(:)=0
qs=0
mf=0
pf=0
i1=1
j0=0
j1=0
if(jk1.lt.jk2)then
jk3=1
else
jk3=-1
end if
d01: do while(i1.le.ll)
ic1=iachar(sm(i1:i1))
j1=0
j2=0
do while(j2.lt.size(dpqs))
j2=j2+1
if(ic1.eq.dpqs(j2))then
j1=j2
exit
end if
end do
if((j1.le.0).or.&
(ll-i1.lt.kdpqs(j1)-1))then
pf=1
exit d01
end if
if(qs.eq.0)then
mf=qstak(0)
else
mf=max(qstak(qs-7),qstak(qs-6))
end if
if(mf.eq.0)then
j2=jdpqs(j1)
if(j2.ne.0)then
if(j0.gt.0)then
j0=j0+jk3*j2
else
j0=jk1
end if
end if
if(jk3*j0.gt.jk3*jk2)then
if(qs.eq.0)then
qstak(0)=1
else
qstak(qs-7)=1
end if
mf=1
end if
end if
select case(j1)
case(1)
ic2=iachar(sm(i1+1:i1+1))
if(ic2.eq.acol)then
if(mf.eq.0)then
if(stjb(j0).ne.stty%p)then
mf=1
else if(stjb(j0+1).ne.dcol)then
mf=1
end if
end if
else if(ic2.eq.ascol)then
if(mf.eq.0)then
if(stjb(j0).ne.stty%p)then
mf=1
else if(stjb(j0+1).ne.dscol)then
mf=1
end if
end if
else if(ic2.eq.aslash)then
if(mf.eq.0)then
if(stjb(j0).ne.stty%p)then
mf=1
else if(stjb(j0+1).ne.dslash)then
mf=1
end if
end if
else if(ic2.eq.istz(3))then
if(mf.eq.0)then
if(stjb(j0).ne.stty%p)then
mf=1
else
jbco(0)=jbco(0)+1
if(jbco(0).ge.size(jbco))then
pf=1
exit d01
end if
jbco(jbco(0))=stjb(j0+1)
end if
end if
else
pf=1
exit d01
end if
case(2)
ic2=iachar(sm(i1+1:i1+1))
if(stib(punct1(0)+ic2).gt.0)then
if(mf.eq.0)then
if(stjb(j0).ne.stty%p)then
mf=1
else if(stjb(j0+1).ne.ic2)then
mf=1
end if
end if
else if(ic2.eq.istz(3))then
if(mf.eq.0)then
if(stjb(j0).ne.stty%p)then
mf=1
else
jbco(0)=jbco(0)+1
if(jbco(0).ge.size(jbco))then
pf=1
exit d01
end if
jbco(jbco(0))=stjb(j0+1)
end if
end if
else if(ic2.ne.aast)then
pf=1
exit d01
end if
case(3)
if(qs.eq.0)then
i1=ll+1
j0=jk2
else
pf=1
end if
exit d01
case(4)
if(mf.eq.0)then
if(stjb(j0).eq.0)then
mf=1
else if(stjb(j0).eq.stty%p)then
mf=1
else if(stjb(j0).eq.stty%dqs)then
mf=1
end if
end if
ic2=iachar(sm(i1+1:i1+1))
if(ic2.eq.istz(1))then
if(mf.eq.0)then
if(stjb(j0).ne.stty%id)then
mf=1
end if
end if
else if(ic2.eq.istz(2))then
if(mf.eq.0)then
if(stjb(j0).ne.stty%sqs)then
mf=1
end if
end if
else if(ic2.eq.istz(3))then
if(mf.eq.0)then
jbco(0)=jbco(0)+1
if(jbco(0).ge.size(jbco))then
pf=1
exit d01
end if
jbco(jbco(0))=stjb(j0)
end if
else if(ic2.eq.istz(4))then
if(mf.eq.0)then
if(stjb(j0).ne.stty%z)then
mf=1
end if
end if
else if(ic2.eq.istz(5))then
if(mf.eq.0)then
if(stjb(j0).ne.stty%ast)then
mf=1
end if
end if
else if(ic2.eq.istz(6))then
if(mf.eq.0)then
if(stjb(j0).ne.stty%plus)then
mf=1
end if
end if
else if(ic2.eq.istz(7))then
if(mf.eq.0)then
if(stjb(j0).ne.stty%minus)then
mf=1
end if
end if
else if(ic2.eq.istz(8))then
if(mf.eq.0)then
if(stjb(j0).ne.stty%id)then
if(stjb(j0).ne.stty%sqs)then
if(stjb(j0).ne.stty%z)then
if(stjb(j0).ne.stty%q)then
mf=1
end if
end if
end if
end if
end if
else if(ic2.eq.istz(9))then
if(mf.eq.0)then
if(stjb(j0).ne.stty%ns)then
mf=1
end if
end if
else
pf=1
exit d01
end if
case(5)
qs=qs+qmats%lc
jbco(0)=jbco(0)+2
if((qs.ge.size(qstak)).or.&
(jbco(0).ge.size(jbco)))then
pf=1
exit d01
end if
jbco(jbco(0)-1)=qmatc%lc
jbco(jbco(0))=0
qstak(qs-8)=0
qstak(qs-7)=0
qstak(qs-6)=mf
qstak(qs-5)=0
qstak(qs-4)=i1
qstak(qs-3)=j0
qstak(qs-2)=jbco(0)
qstak(qs-1)=jbco(0)
qstak(qs)=qmatc%lc
case(6)
j2=0
if(qs.lt.qmats%lc)then
j2=1
else if((qstak(qs).ne.qmatc%lc).or.&
(qstak(qs-5).gt.1).or.&
(i1-1.le.qstak(qs-4)))then
j2=1
end if
if(j2.ne.0)then
pf=1
exit d01
end if
j2=qstak(qs-5)
if(mf.eq.0)then
if(j2.eq.0)then
qstak(qs-8)=-(j0+1)
else
qstak(qs-8)=1
qstak(qs-3)=j0
qstak(qs-1)=jbco(0)
j3=qstak(qs-2)
jbco(j3)=jbco(j3)+1
mf=1
end if
else if(j2.gt.0)then
if(qstak(qs-8).lt.0)then
j0=-(qstak(qs-8)+1)
qstak(qs-7)=0
qstak(qs-8)=0
mf=0
end if
end if
qstak(qs-5)=j2+1
case(7)
j2=0
if(qs.lt.qmats%lc)then
j2=1
else if((qstak(qs).ne.qmatc%lc).or.&
(qstak(qs-5).eq.1).or.&
(qstak(qs-5).gt.2).or.&
(i1-1.le.qstak(qs-4)))then
j2=1
end if
if(j2.ne.0)then
pf=1
exit d01
end if
if(mf.eq.0)then
j2=qstak(qs-2)
jbco(j2)=jbco(j2)+1
if(qstak(qs-5).ne.1)then
i1=qstak(qs-4)
qstak(qs-3)=j0
qstak(qs-1)=jbco(0)
qstak(qs-5)=0
else
mf=1
end if
end if
if(mf.ne.0)then
j0=qstak(qs-3)
jbco(0)=qstak(qs-1)
qs=qs-qmats%lc
mf=0
end if
case(8)
qs=qs+qmats%oc
jbco(0)=jbco(0)+2
if((qs.ge.size(qstak)).or.&
(jbco(0).ge.size(jbco)))then
pf=1
exit d01
end if
jbco(jbco(0)-1)=qmatc%oc
jbco(jbco(0))=0
qstak(qs-7)=0
qstak(qs-6)=mf
qstak(qs-5)=0
qstak(qs-4)=i1
qstak(qs-3)=j0
qstak(qs-2)=jbco(0)
qstak(qs-1)=jbco(0)
qstak(qs)=qmatc%oc
case(9:10)
j2=0
if(qs.lt.qmats%oc)then
j2=1
else if(qstak(qs).ne.qmatc%oc)then
j2=1
else if(j1.eq.9)then
if(i1-1.le.qstak(qs-4))then
j2=1
end if
else
if(qstak(qs-5).eq.0)then
j2=1
end if
end if
if(j2.ne.0)then
pf=1
exit d01
end if
qstak(qs-5)=qstak(qs-5)+1
j2=qstak(qs-2)
if(mf.eq.0)then
if(jbco(j2).eq.0)then
qstak(qs-1)=jbco(0)
qstak(qs-3)=j0
jbco(j2)=qstak(qs-5)
mf=1
end if
end if
if(j1.eq.9)then
qstak(qs-4)=i1
if(jbco(j2).eq.0)then
qstak(qs-7)=0
j0=qstak(qs-3)
mf=0
end if
else
jbco(0)=qstak(qs-1)
qs=qs-qmats%oc
if(jbco(j2).eq.0)then
mf=1
else
mf=0
end if
end if
case default
pf=1
exit d01
end select
if(mf.ne.0)then
if(qs.eq.0)then
qstak(0)=1
else
qstak(qs-7)=mf
end if
end if
if(qstak(0).ne.0)then
exit d01
end if
i1=i1+kdpqs(j1)
end do d01
if(pf.ne.0)then
mlin(1:srec)='qmatcha_2'
call mput(1,1,0,0,0)
end if
if(qs.ne.0)then
jbco(0)=-1
else if(qstak(0).ne.0)then
jbco(0)=-1
else if(i1-1.ne.ll)then
jbco(0)=-1
else if(j0.ne.jk2)then
jbco(0)=-1
else if(jbco(0).lt.size(jbco))then
jbco(jbco(0)+1)=eoa
end if
return
end subroutine qmatcha
subroutine hsort(ia,ib)
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer,intent(in)::ia,ib
integer::i0,hi,hj,hl,hr,xtmp
if((ia.le.0).or.&
(ia.gt.ib))then
mlin(1:srec)='hsort_1'
call mput(1,1,0,0,0)
else if(ia.eq.ib)then
go to 90
end if
i0=ia-1
hr=ib-i0
hl=(hr/2)+1
20 continue
if(hl.gt.1)then
hl=hl-1
xtmp=stib(i0+hl)
else
xtmp=stib(i0+hr)
stib(i0+hr)=stib(i0+1)
hr=hr-1
if(hr.eq.1)then
stib(i0+1)=xtmp
go to 90
end if
end if
hj=hl
40 continue
hi=hj
hj=hi+hi
if(hj.le.hr)then
if(hj.lt.hr)then
if(stib(i0+hj).lt.stib(i0+hj+1))then
hj=hj+1
end if
end if
if(xtmp.lt.stib(i0+hj))then
stib(i0+hi)=stib(i0+hj)
go to 40
end if
end if
stib(i0+hi)=xtmp
go to 20
90 return
end subroutine hsort
subroutine xipht(ia,ib,ixipht)
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer,intent(in)::ia,ib,ixipht
integer::ii,jj,i1,j1,j2,j3
ii=0
if(ia.le.0)then
ii=1
else if(ib.le.0)then
ii=1
else if(ib.lt.ia)then
ii=1
else if(ib.gt.stibas(1))then
ii=1
else if(stibs(1).le.0)then
ii=1
else if(stibs(1).ge.stibas(1)-1)then
ii=1
else if(ixipht.lt.0)then
if(ia+ixipht.le.0)then
ii=1
end if
end if
if(ii.ne.0)then
mlin(1:srec)='xipht_1'
call mput(1,1,0,0,0)
end if
if(ixipht.lt.0)then
j1=ia
j2=ib
j3=1
else if(ixipht.gt.0)then
jj=ixipht-(stibs(1)-ib)
if(jj.gt.0)then
jj=(stibas(1)-ia)-ixipht
if(jj.ge.0)then
mlin(1:srec)='xipht_2'
call mput(1,1,0,0,0)
end if
end if
j1=ib
j2=ia
j3=-1
end if
if(ixipht.ne.0)then
do i1=j1,j2,j3
stib(i1+ixipht)=stib(i1)
end do
end if
return
end subroutine xipht
subroutine trm(ia,ib,istib)
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer,external::qbprod
integer,intent(in)::ia,ib,istib
integer::ab,i1,ii,ix,jj,kk,st0,xtmp,ytmp
ii=0
if(ia.le.0)then
ii=1
else if(ib.le.0)then
ii=1
else if(istib.le.0)then
ii=1
else if(stibas(1)-istib.lt.0)then
ii=1
else
jj=ia
kk=ib
ab=qbprod(jj,kk)
st0=istib-(ab-1)
if(st0.le.0)then
ii=1
end if
end if
if(ii.ne.0)then
mlin(1:srec)='trm_1'
call mput(1,1,0,0,0)
end if
do i1=1,ab-2
ix=0
jj=i1
10 continue
ii=jj/ia
kk=ii+ib*(jj-ii*ia)
if(ix.eq.1)then
ytmp=stib(st0+kk)
stib(st0+kk)=xtmp
xtmp=ytmp
end if
if(kk.gt.i1)then
jj=kk
go to 10
else if(kk.eq.i1)then
if(ix.eq.0)then
if(kk.ne.jj)then
jj=i1
ix=1
xtmp=stib(st0+i1)
go to 10
end if
end if
end if
end do
return
end subroutine trm
subroutine qrr(junit,nlin,slin,qc)
use,non_intrinsic::cbuffer
use,non_intrinsic::qfiles
use,non_intrinsic::wbuffer
implicit none
save
integer,intent(in)::junit
integer,intent(inout)::nlin
integer,intent(out)::qc,slin
integer::ii,iunit,i1,ios,jj,j1,j2
ii=0
if(junit.le.0)then
ii=1
else if(junit.gt.mfiles0)then
ii=1
else if(filt(junit).eq.qfty%otyp)then
ii=1
end if
if(ii.ne.0)then
mlin(1:srec)='qrr_1'
call mput(1,1,0,0,0)
end if
iunit=filu(junit)
ios=1
read(unit=iunit,fmt='(a)',iostat=ios)stxb(1:srecx)
if(ios.ne.0)then
if(ios.eq.iostat_eor)then
call wput(-19,0,0)
else if(ios.ne.iostat_end)then
call uput(1)
end if
call qclose(junit,0)
slin=-1
go to 90
end if
nlin=nlin+1
slin=0
do i1=srecx,1,-1
if(iachar(stxb(i1:i1)).ne.aspc)then
slin=i1
exit
end if
end do
j1=0
if(nlin.eq.1)then
if(slin.gt.2)then
if(ichar(stxb(1:1)).eq.239)then
if(ichar(stxb(2:2)).eq.187)then
if(ichar(stxb(3:3)).eq.191)then
j1=3
slin=slin-j1
end if
end if
end if
end if
end if
if(slin.ge.srec)then
mlin(1:srec)="line too long,"
call mput(1,1,nlin,nlin,junit)
end if
jj=stcbs(1)
ii=slin+1
call vaocb(ii)
i1=jj+ii
stcb(i1:i1)=achar(alf)
qc=0
if(slin.gt.0)then
do i1=jj+1,jj+slin
j1=j1+1
j2=iachar(stxb(j1:j1))
if((j2.lt.abo(1)).or.&
(j2.gt.abo(2)))then
mlin(1:srec)="wrong characters,"
call mput(1,1,nlin,nlin,junit)
end if
stcb(i1:i1)=achar(j2)
end do
i1=jj+1
if(acf(iachar(stcb(i1:i1))).eq.-1)then
qc=1
end if
end if
90 return
end subroutine qrr
subroutine qout(junit)
use,non_intrinsic::qfiles
use,non_intrinsic::qintr
use,non_intrinsic::wbuffer
implicit none
save
integer,intent(in)::junit
integer::f1,ii,i1,j1,j2,j3,ios,iunit
f1=filo(junit)
j1=stwbpf(f1)+1
j2=stwbqf(f1)
ii=0
if(j1.le.0)then
ii=1
else if(j2.lt.j1)then
ii=1
else if(len(stwb).le.j2)then
ii=1
else if(iachar(stwb(j2:j2)).ne.alf)then
ii=1
end if
if(ii.ne.0)then
mlin(1:srec)='qout_2'
call mput(1,1,0,0,0)
end if
iunit=filu(junit)
if(filt(junit).eq.qfty%ptyp)then
mlin(1:srec)='qout_3'
call mput(1,1,0,0,0)
else if(cflag(4).eq.0)then
j3=j1
do i1=j1,j2
if(iachar(stwb(i1:i1)).eq.alf)then
ios=1
if(j3.lt.i1)then
write(unit=iunit,fmt='(a)',iostat=ios)stwb(j3:i1-1)
else
write(unit=iunit,fmt='(a)',iostat=ios)
end if
if(ios.ne.0)then
call uput(1)
end if
j3=i1+1
end if
end do
else
if(j1.lt.j2)then
write(unit=iunit,fmt='(a)',iostat=ios)stwb(j1:j2-1)
else
write(unit=iunit,fmt='(a)',iostat=ios)
end if
if(ios.ne.0)then
call uput(1)
end if
end if
return
end subroutine qout
subroutine qopen(junit)
use,non_intrinsic::cbuffer
use,non_intrinsic::qfiles
use,non_intrinsic::qintr
use,non_intrinsic::wbuffer
implicit none
save
integer,intent(in)::junit
logical::lun,loun
integer::j1,j2,ii,jstat,ios,iunit,ite
ii=0
if(junit.le.0)then
ii=1
else if(junit.gt.mfiles0)then
ii=1
else if(filu(junit)+1.ne.0)then
ii=1
end if
if(ii.ne.0)then
mlin(1:srec)='qopen_1'
call mput(1,1,0,0,0)
end if
if((filt(junit).eq.qfty%otyp).or.&
(filt(junit).eq.qfty%wtyp))then
jstat=1
lun=.true.
else
jstat=0
lun=.false.
end if
ite=0
j1=filp(junit)
j2=filq(junit)
10 continue
ios=1
loun=.true.
inquire(file=stcb(j1:j2),exist=lun,opened=loun,iostat=ios)
if(ios.ne.0)then
call uput(1)
else if(loun)then
mlin(1:srec)="is already open (or a duplicate)"
call mput(1,1,0,0,-junit)
else if(jstat.eq.0)then
if(.not.lun)then
mlin(1:srec)="could not be found"
call mput(1,1,0,0,-junit)
end if
else if(lun)then
if(cflag(6).eq.0)then
mlin(1:srec)="already exists"
call mput(1,1,0,0,-junit)
else
if(ite.eq.0)then
call qopd(j1,j2,ii)
if(ii.ne.0)then
ite=ite+1
go to 10
end if
else
ii=0
end if
if(ii.eq.0)then
mlin(1:srec)="could not be opened"
call mput(1,1,0,0,-junit)
end if
end if
end if
ios=1
if(jstat.eq.0)then
open(access='sequential',action='read',file=stcb(j1:j2),iostat=ios,&
newunit=iunit,pad='yes',position='rewind',status='old')
else
open(access='sequential',action='write',file=stcb(j1:j2),&
iostat=ios,newunit=iunit,status='new')
end if
if(ios.ne.0)then
mlin(1:srec)="could not be opened"
call mput(1,1,0,0,-junit)
end if
filu(junit)=iunit
return
end subroutine qopen
subroutine qopd(j1,j2,jj)
use,non_intrinsic::cbuffer
use,non_intrinsic::qintr
use,non_intrinsic::wbuffer
implicit none
save
integer,intent(in)::j1,j2
integer,intent(inout)::jj
integer::ios,iunit,slen,ii,i1,j3,j4
j3=j1
do i1=j2,j1,-1
if(iachar(stcb(i1:i1)).eq.aslash)then
j3=i1+1
exit
end if
end do
j4=qtfpq-qtfpp
ii=0
if(j2-j3.le.j4)then
ii=1
else if(stcb(j3:j3+j4).ne.stcb(qtfpp:qtfpq))then
ii=1
end if
if(ii.ne.0)then
go to 90
end if
ios=1
open(access='sequential',action='read',file=stcb(j1:j2),iostat=ios,&
newunit=iunit,pad='yes',position='rewind',status='old')
if(ios.ne.0)then
close(unit=iunit,status='keep')
go to 90
end if
ios=1
read(unit=iunit,fmt='(a)',iostat=ios)stxb(1:srec)
jj=0
if(ios.ne.0)then
if(ios.ne.iostat_end)then
close(unit=iunit,status='keep')
go to 90
else
jj=1
end if
end if
if(jj.eq.0)then
slen=0
do i1=srec,1,-1
if(iachar(stxb(i1:i1)).ne.aspc)then
slen=i1
exit
end if
end do
j3=iachar(stcb(qgfmp:qgfmp))
j4=iachar(stcb(qgfmq:qgfmq))
ii=qgfmq-qgfmp
if(slen.lt.ii)then
go to 90
end if
do i1=1,slen-ii
if(iachar(stxb(i1:i1)).eq.j3)then
if(iachar(stxb(i1+ii:i1+ii)).eq.j4)then
if(stxb(i1:i1+ii).eq.stcb(qgfmp:qgfmq))then
jj=1
end if
end if
end if
end do
if(jj.eq.0)then
if(iachar(stcb(qgfmq:qgfmq)).eq.aspc)then
if(stxb(slen-ii+1:slen).eq.stcb(qgfmp:qgfmq-1))then
jj=1
end if
end if
end if
end if
if(jj.ne.0)then
ios=1
close(unit=iunit,status='delete',iostat=ios)
if(ios.ne.0)then
go to 90
end if
end if
90 continue
return
end subroutine qopd
subroutine qclose(junit,istop)
use,non_intrinsic::qfiles
use,non_intrinsic::qintr
use,non_intrinsic::wbuffer
implicit none
save
integer,intent(in)::junit,istop
logical::lun
character(kind=ascii,len=6)::fstat
integer::i1,ii,jj,j1,j2,iunit,ios
ii=0
if(istop.lt.0)then
ii=1
else if(istop.gt.1)then
ii=1
else if(junit.lt.0)then
ii=1
else if(junit.gt.mfiles0)then
ii=1
else if(junit.ne.0)then
if(filt(junit).eq.qfty%ptyp)then
ii=1
end if
end if
if(ii.ne.0)then
mlin(1:srec)='qclose_1'
call mput(-1,1,0,0,0)
end if
if(junit.eq.0)then
j1=1
j2=mfiles0
else
j1=junit
j2=junit
end if
do i1=j1,j2
iunit=filu(i1)
if((iunit+1.ne.0).and.&
(filt(i1).ne.qfty%ptyp))then
ios=1
lun=.false.
inquire(unit=iunit,opened=lun,iostat=ios)
if((ios.eq.0).and.&
(lun))then
jj=0
if(filt(i1).eq.qfty%otyp)then
if(istop.ne.0)then
if(cflag(1).gt.0)then
jj=6
end if
end if
else if(filt(i1).eq.qfty%wtyp)then
if(cflag(14).gt.1)then
write(unit=iunit,fmt='(a)',iostat=ios)
if(ios.ne.0)then
call uput(1)
end if
else
jj=6
end if
end if
if(jj.eq.0)then
jj=4
fstat(1:jj)='keep'
else
fstat(1:jj)='delete'
end if
ios=1
close(unit=iunit,status=fstat(1:jj),iostat=ios)
end if
if(ios.ne.0)then
call uput(1)
end if
filu(i1)=-1
end if
end do
return
end subroutine qclose
subroutine inputs
use,non_intrinsic::cbuffer
use,non_intrinsic::ikeys
use,non_intrinsic::qfiles
use,non_intrinsic::qintr
use,non_intrinsic::qmix
use,non_intrinsic::qmodel
use,non_intrinsic::qproc
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer,parameter::datls=5
integer,external::sigz,stoz,ilsd
integer::icom,ii,ij,ik,i1,i2,i3,jj,jk,j1,j2,j3,j4,j5,j6,kid,kk,ll
integer::atyp,bot,nphia,ntf1,tfch,tfcl,tfcn,tfv
integer::dbl,ifile,newc,nlin,nmp,nmp0,pp0,pp1,pp2,slin,top
integer::phase,qc,stjr,stjr0,styp,tsl
integer::dreci(0:0),comii(0:0),comfi(0:0)
integer::comt(1:dekc%ubo)
integer::xli(1:maxleg)
if(stjbs.ne.stj0)then
mlin(1:srec)='inputs_1'
call mput(1,1,0,0,0)
end if
ifile=0
do i1=1,nfiles
if(filt(i1).eq.qfty%ctyp)then
call qopen(i1)
ifile=i1
exit
end if
end do
phase=1
nmp=0
icom=dekc%conf
ncom=0
inco=0
outg=0
nleg=0
vparti(0)=nap
vpartj(0)=nap
ntf=0
stjr0=nap
tftyp(0)=nap
tfnarg(0)=nap
tfa(0)=nap
tfb(0)=nap
tfc(0)=nap
tf2(0)=nap
tfo(0)=nap
tfills(0)=nap
tfiuls(0)=nap
tftb(0)=nap
tfta(0)=nap
tftic(0)=nap
comc(:)=0
rbri(:)=1
sbri(:)=1
zbri(:)=1
zcho(:)=1
zpro(:)=1
cfrp(0)=nap
ndrec=0
newc=1
dbl=0
nlin=0
tsl=0
03 continue
slin=0
call qrr(ifile,nlin,slin,qc)
if(slin.eq.-1)then
if(newc.eq.0)then
go to 80
else
do i1=dekc%ubo,1,-1
if(stib(oisk(0)+i1).ne.0)then
if(comc(i1).eq.0)then
mlin(1:srec)="is incomplete"
call mput(1,1,0,0,-ifile)
end if
end if
end do
end if
go to 301
end if
if(newc.ne.0)then
bot=stcbs(1)+1
dbl=nlin
end if
if((slin.eq.0).or.&
(qc.ne.0))then
if(newc.ne.0)then
go to 03
else
go to 80
end if
end if
top=stcbs(1)+slin
ii=top+1
if(iachar(stcb(ii:ii)).ne.alf)then
go to 80
else if(iachar(stcb(top:top)).eq.aspc)then
go to 80
end if
tsl=tsl+slin+1
if(tsl.gt.sxbuff)then
mlin(1:srec)="statement too long,"
go to 80
end if
jflag(7)=0
if(iachar(stcb(top:top)).eq.ascol)then
newc=1
tsl=0
else
newc=0
end if
ndrec=ndrec+1
ii=stibs(1)+1
if(cfrp(0).eq.nap)then
cfrp(0)=ii
cfrp(1)=ii
else
stib(cfrp(1))=ii
cfrp(1)=ii
end if
call aoib(datls+1)
ii=slin+1
jj=stibs(1)
stib(jj)=eoa
stib(jj-1)=nlin-dbl
stib(jj-2)=nlin
stib(jj-3)=ii
stib(jj-4)=stcbs(1)+1
stib(cfrp(1))=nap
call aocb(ii)
if(newc.eq.0)then
go to 03
end if
stjr=stjbs
if(stjr0.eq.nap)then
stjr0=stjr
end if
call stap(bot,top+1,qfty%ctyp)
if(stibs(1).ne.jj)then
mlin(1:srec)='inputs_2'
call mput(1,1,0,0,0)
end if
ii=0
if(stapi(1).le.0)then
ii=1
else if(stapi(1).gt.size(comc))then
ii=1
else if(stapi(2).lt.0)then
ii=1
end if
if(stapi(1).eq.dekc%lib)then
ii=1
end if
if(ii.ne.0)then
go to 80
end if
comt(:)=0
do while((icom.ne.stapi(1)).and.&
(comt(icom).eq.0).and.&
(stib(oisk(0)+icom).eq.0))
comt(icom)=1
icom=stib(fisk(0)+icom)
end do
do while((icom.lt.stapi(1)).and.&
((comc(icom).ne.0).or.&
(stib(oisk(0)+icom).eq.0)))
icom=icom+1
end do
if(icom.ne.stapi(1))then
go to 80
end if
if(comc(icom).ne.0)then
if(stib(oisk(0)+icom).ne.0)then
go to 80
end if
end if
comc(icom)=comc(icom)+1
ncom=ncom+1
stib(jj)=icom
if(icom.gt.dekc%loopk)then
if(cflag(1)*cflag(8).ne.0)then
if(comc(dekc%loopk).eq.0)then
mlin(1:srec)="missing 'loop_momentum' statement"
call mput(1,1,0,0,ifile)
end if
end if
end if
pp1=stjr+2
pp2=pp1+3*(stjb(pp1-1)-1)
if(icom.eq.dekc%conf)then
if(ncom.ne.1)then
go to 80
end if
j1=stjb(stjbs-2)+3
do i1=1,stapi(2)
j1=j1+6
ij=0
call mstr0(stcb,stjb(j1),stjb(j1+1),popt0(0),wopt0(0),aopt0(0),ij)
if(ij.le.0)then
go to 80
end if
ik=stib(xopt0(0)+ij)
ij=stib(copt0(0)+ij)
if(cflag(11).ne.0)then
end if
if(cflag(ij).eq.0)then
cflag(ij)=ik
else if(cflag(ij).eq.ik)then
call wput(10,0,0)
else
call wput(-10,0,0)
end if
end do
if(cflag(11).eq.0)then
cflag(2)=cflag(3)
else
cflag(2)=-1
end if
else if(icom.eq.dekc%msg)then
if(comc(dekc%msg).ne.1)then
go to 80
end if
ll=-1
if(stapi(2).eq.0)then
ll=0
else if(stapi(2).eq.1)then
j1=pp1+7
j3=stjb(j1)
j4=stjb(j1+1)
call stds(stcb,j3,j4,1,j5,j6)
if(j5.eq.0)then
stjb(j1)=j3+1
stjb(j1+1)=j4-1
ll=(j4-j3)-1
else if(j5.gt.0)then
stjb(j1)=j5
stjb(j1+1)=j6
ll=j6-j5+1
else
go to 80
end if
end if
if(ll.gt.0)then
if(cflag(5).gt.0)then
call nows(j1)
ll=stjb(j1+1)-stjb(j1)+1
end if
end if
if(ll.gt.0)then
nfiles=nfiles+1
filt(nfiles)=qfty%wtyp
filp(nfiles)=stjb(j1)
filq(nfiles)=stjb(j1+1)
call qopen(nfiles)
cflag(14)=1
if(cflag(6).ne.0)then
if(filu(nfiles)+1.ne.0)then
write(unit=filu(nfiles),fmt='(a)')'%  '//stcb(qgfmp:qgfmq)
end if
end if
else if(ll.lt.0)then
go to 80
end if
end if
if(cflag(3).ge.0)then
if(cflag(11).eq.0)then
if(ncom.eq.1)then
call spp(0,0,0,1)
end if
call spp(1,bot,top+1,0)
end if
end if
if((icom.eq.dekc%conf).or.&
(icom.eq.dekc%msg))then
go to 143
else if(icom.eq.dekc%outp)then
go to 102
else if(icom.eq.dekc%sty)then
go to 103
else if(icom.eq.dekc%model)then
go to 104
else if((icom.eq.dekc%in).or.&
(icom.eq.dekc%out))then
go to 106
else if(icom.eq.dekc%loop)then
go to 107
else if(icom.eq.dekc%loopk)then
go to 108
else if(icom.eq.dekc%opt)then
go to 109
else if(icom.eq.dekc%part)then
go to 110
else if(icom.eq.dekc%off)then
go to 111
else if((icom.eq.dekc%tru).or.&
(icom.eq.dekc%fals))then
go to 130
else
go to 80
end if
102 continue
ii=0
if(dso.eq.0)then
dso=1
if(comc(dekc%outp).ne.1)then
ii=1
else if(comc(dekc%sty).ne.0)then
ii=1
end if
else
if(comc(dekc%outp).gt.msfiles0)then
ii=1
else if(comc(dekc%sty).gt.msfiles0)then
ii=1
else if(comc(dekc%outp).ne.comc(dekc%sty)+max(0,dso))then
ii=1
end if
end if
if(ii.ne.0)then
go to 80
end if
ll=-1
if(stapi(2).eq.0)then
ll=0
else if(stapi(2).eq.1)then
j1=pp1+7
j3=stjb(j1)
j4=stjb(j1+1)
call stds(stcb,j3,j4,1,j5,j6)
if(j5.eq.0)then
stjb(j1)=j3+1
stjb(j1+1)=j4-1
ll=(j4-j3)-1
else if(j5.gt.0)then
stjb(j1)=j5
stjb(j1+1)=j6
ll=j6-j5+1
else
go to 80
end if
end if
if(ll.gt.0)then
if(cflag(5).gt.0)then
call nows(j1)
ll=stjb(j1+1)-stjb(j1)+1
end if
end if
if(ll.gt.0)then
nfiles=nfiles+1
filt(nfiles)=qfty%otyp
filp(nfiles)=stjb(j1)
filq(nfiles)=stjb(j1+1)
else if(ll.eq.0)then
if(dso.lt.0)then
if(filp(nfiles-1).eq.nap)then
mlin(1:srec)="style-file name(s) missing,"
call mput(1,1,0,0,ifile)
end if
end if
else
go to 80
end if
go to 143
103 continue
ii=0
if(dso.eq.0)then
dso=-1
if(comc(dekc%outp).ne.0)then
ii=1
else if(comc(dekc%sty).ne.1)then
ii=1
end if
else
if(comc(dekc%sty).gt.msfiles0)then
ii=1
else if(comc(dekc%outp).ne.comc(dekc%sty)+min(0,dso))then
ii=1
end if
end if
if(ii.ne.0)then
go to 80
end if
ll=-1
if(stapi(2).eq.0)then
ll=0
else if(stapi(2).eq.1)then
j1=pp1+7
j3=stjb(j1)
j4=stjb(j1+1)
call stds(stcb,j3,j4,1,j5,j6)
if(j5.eq.0)then
stjb(j1)=j3+1
stjb(j1+1)=j4-1
ll=(j4-j3)-1
else if(j5.gt.0)then
stjb(j1)=j5
stjb(j1+1)=j6
ll=j6-j5+1
else
go to 80
end if
end if
if(ll.gt.0)then
if(cflag(5).gt.0)then
call nows(j1)
ll=stjb(j1+1)-stjb(j1)+1
end if
end if
nfiles=nfiles+1
filt(nfiles)=qfty%styp
nsfiles=nsfiles+1
if(ll.gt.0)then
filp(nfiles)=stjb(j1)
filq(nfiles)=stjb(j1+1)
else if(ll.eq.0)then
if(dso.gt.0)then
if(filp(nfiles-1).ne.nap)then
mlin(1:srec)="style-file name(s) missing,"
call mput(1,1,0,0,ifile)
end if
end if
else
go to 80
end if
go to 143
104 continue
if(comc(dekc%outp).ne.comc(dekc%sty))then
go to 80
end if
if(comc(dekc%model).ne.1)then
go to 80
end if
if(cflag(1).lt.0)then
if(cflag(11).ne.0)then
mlin(1:srec)="config option 'nolist' should not be used in this mode"
call mput(1,1,0,0,ifile)
end if
cflag(1)=0
else if(cflag(11).ne.0)then
cflag(1)=1
else
do i1=1,nfiles
if(filt(i1).eq.qfty%otyp)then
if(filp(i1).ne.nap)then
cflag(1)=1
exit
end if
end if
end do
end if
if(cflag(2).eq.0)then
if(cflag(1).eq.0)then
cflag(2)=1
end if
else if(cflag(2).gt.0)then
cflag(2)=cflag(2)-1
end if
ll=-1
if(stapi(2).eq.0)then
ll=0
else if(stapi(2).gt.2)then
go to 80
else
j1=pp1+7
if(stapi(2).eq.2)then
smodp(0)=stjb(j1)
smodq(0)=stjb(j1+1)
cflag(9)=1
j1=j1+6
end if
j3=stjb(j1)
j4=stjb(j1+1)
call stds(stcb,j3,j4,1,j5,j6)
if(j5.eq.0)then
stjb(j1)=j3+1
stjb(j1+1)=j4-1
ll=(j4-j3)-1
else if(j5.gt.0)then
stjb(j1)=j5
stjb(j1+1)=j6
ll=j6-j5+1
else
go to 80
end if
end if
if(ll.gt.0)then
if(cflag(5).gt.0)then
call nows(j1)
ll=stjb(j1+1)-stjb(j1)+1
end if
end if
if(ll.gt.0)then
nfiles=nfiles+1
filt(nfiles)=qfty%mtyp
filp(nfiles)=stjb(j1)
filq(nfiles)=stjb(j1+1)
else if(ll.eq.0)then
mlin(1:srec)="model-file name is missing,"
call mput(1,1,0,0,ifile)
else
go to 80
end if
call qifi
ii=stjbs
do i1=1,nfiles
if(filt(i1).eq.qfty%styp)then
if(filp(i1).ne.nap)then
call style
exit
end if
end if
end do
if(stjbs.ne.ii)then
mlin(1:srec)='inputs_9'
call mput(1,1,0,0,0)
end if
call model
if(stjbs.ne.ii)then
mlin(1:srec)='inputs_10'
call mput(1,1,0,0,0)
end if
go to 143
106 continue
nphia=stapi(2)
kk=4
if(icom.eq.dekc%in)then
if(comc(dekc%in).ne.1)then
go to 80
end if
inco=nphia
leg(0)=stibs(1)+kk
ii=kk*(maxleg+2)
call aoib(ii)
else
if(comc(dekc%out).ne.1)then
go to 80
end if
outg=nphia
end if
if(nphia.gt.0)then
if(pp2-pp1.eq.3*(2*nphia+1))then
if(icom.eq.dekc%in)then
cflag(8)=0
else if(cflag(8).ne.0)then
go to 80
end if
else if(pp2-pp1.eq.3*(5*nphia+1))then
if(icom.eq.dekc%in)then
cflag(8)=1
else if(inco.eq.0)then
cflag(8)=1
else if(cflag(8).eq.0)then
go to 80
end if
end if
j1=6+9*cflag(8)
j2=pp1+7-j1
do i1=1,nphia
nleg=nleg+1
if(nleg.gt.maxleg)then
mlin(1:srec)="too many legs"
call mput(1,1,0,0,0)
end if
j2=j2+j1
jk=0
call mstr0(stcb,stjb(j2),stjb(j2+1),namep(0),namel(0),nap,jk)
if(jk.eq.0)then
mlin(1:srec)="unknown external particle(s)"
call mput(1,1,0,0,0)
end if
leg(0)=leg(0)+1
if(icom.eq.dekc%in)then
stib(leg(0))=stib(link(0)+jk)
else
stib(leg(0))=jk
end if
if(cflag(8).eq.0)then
call aocb(1)
j3=stcbs(1)
if(icom.eq.dekc%in)then
stcb(j3:j3)=achar(kpqs(2))
else
stcb(j3:j3)=achar(kpqs(3))
end if
leg(0)=leg(0)+1
stib(leg(0))=j3
call dkar(i1,ii)
call aocb(ii+1)
leg(0)=leg(0)+1
stib(leg(0))=stcbs(1)-1
leg(0)=leg(0)+1
stib(leg(0))=ii+1
stcb(stcbs(1):stcbs(1))=achar(anull)
else
j4=j2+6
leg(0)=leg(0)+1
stib(leg(0))=stjb(j4)
leg(0)=leg(0)+1
stib(leg(0))=stjb(j4+1)
leg(0)=leg(0)+1
stib(leg(0))=stjb(j4+1)-(stjb(j4)-1)
end if
end do
else if(nphia.ne.0)then
go to 80
end if
if(icom.eq.dekc%out)then
stib(leg(0)+1:leg(0)+kk)=eoa
leg(0)=leg(0)+kk
ii=nleg+2
call trm(kk,ii,leg(0))
momel(0)=leg(0)-(ii-1)
momeq(0)=momel(0)-ii
momep(0)=momeq(0)-ii
leg(0)=momep(0)-ii
stib(leg(0))=eoa
call aocb(2)
ii=stcbs(1)-1
stcb(ii:stcbs(1))=achar(kpqs(1))//achar(anull)
stib(momep(0))=ii
stib(momeq(0))=ii
stib(momel(0))=1
if(nleg.ne.inco+outg)then
mlin(1:srec)='inputs_8'
call mput(1,1,0,0,0)
end if
if(nleg.eq.0)then
mlin(1:srec)="no external particles"
call mput(1,1,0,0,0)
end if
end if
go to 143
107 continue
if(comc(dekc%loop).ne.1)then
go to 80
end if
if(stapi(2).ne.1)then
if(stapi(2).ne.2)then
go to 80
end if
end if
j1=pp1+7
loopx(1)=stoz(stcb,stjb(j1),stjb(j1+1))
loopx(2)=loopx(1)
if(stapi(2).eq.2)then
j1=j1+3
ij=0
call mstr0(stcb,stjb(j1),stjb(j1+1),popt4(0),wopt4(0),aopt4(0),ij)
if(ij.le.0)then
go to 80
end if
j1=j1+3
loopx(2)=stoz(stcb,stjb(j1),stjb(j1+1))
end if
loopx(3)=min(loopx(1),loopx(2))
loopx(4)=max(loopx(1),loopx(2))
if(loopx(3).lt.0)then
mlin(1:srec)="check loops in"
call mput(1,1,0,0,ifile)
else if((loopx(4).gt.maxrho).or.&
(loopx(4)+nleg.gt.max(maxleg,maxrho)))then
mlin(1:srec)="too many legs and/or loops"
call mput(1,1,0,0,0)
end if
if(nleg+loopx(3).lt.2)then
mlin(1:srec)="check legs, loops, in"
call mput(1,1,0,0,ifile)
end if
if((nleg.eq.2).and.&
(loopx(3).eq.0))then
mlin(1:srec)="case legs=2, loops=0 not accepted"
call mput(1,1,0,0,0)
end if
if(loopx(4).eq.0)then
if(maxli.gt.0)then
zcho(1:maxli)=0
end if
end if
if(loopx(3).gt.0)then
zcho(0:loopx(3)-1)=0
end if
if(loopx(1).gt.loopx(2))then
loopx(5)=-1
else
loopx(5)=1
end if
go to 143
108 continue
if(comc(dekc%loopk).ne.1)then
go to 80
else if(stapi(2).eq.1)then
j1=pp1+7
stib(momep(0))=stjb(j1)
stib(momeq(0))=stjb(j1+1)
stib(momel(0))=stjb(j1+1)-(stjb(j1)-1)
else if(stapi(2).ne.0)then
go to 80
end if
go to 143
109 continue
if(comc(dekc%opt).ne.1)then
go to 80
end if
ii=0
j1=pp1+1
do i1=1,stapi(2)
j1=j1+6
ij=0
call mstr0(stcb,stjb(j1),stjb(j1+1),popt1(0),wopt1(0),aopt1(0),ij)
if(ij.le.0)then
go to 80
end if
ik=stib(xopt1(0)+ij)
ij=stib(copt1(0)+ij)
if(dflag(ij).ne.0)then
ii=ii+1
if(dflag(ij)*ik.lt.0)then
jflag(1)=1
end if
end if
dflag(ij)=ik
end do
if(ii.gt.0)then
call wput(11,0,0)
end if
go to 143
110 continue
ii=0
if(comc(dekc%part).ne.1)then
ii=1
else if(stapi(2).eq.0)then
go to 143
else if(stapi(2).lt.2)then
ii=1
else if((stapi(2).gt.2).and.&
(modulo(stapi(2),2).ne.0))then
ii=1
end if
if(ii.ne.0)then
go to 80
end if
cflag(10)=1
vparti(0)=stibs(1)+1
call aoib(nrho+2)
stib(vparti(0):stibs(1)-1)=-1
stib(stibs(1))=eoa
vpartj(0)=stibs(1)+1
call aoib(nrho+2)
stib(vpartj(0):stibs(1)-1)=0
stib(stibs(1))=eoa
j1=pp1+4
ii=0
do i1=2,stapi(2),2
j1=j1+3
if(stjb(j1+1)-stjb(j1).gt.wsint)then
go to 80
end if
j2=stoz(stcb,stjb(j1),stjb(j1+1))
if(j2.lt.3)then
ii=1
else if(j2.gt.nrho)then
ii=1
else if(stib(nivd(0)+j2).eq.0)then
ii=1
else if(stib(vparti(0)+j2).ge.0)then
ii=1
end if
if(ii.ne.0)then
exit
end if
j1=j1+6
if(stjb(j1-1).eq.stty%z)then
j4=0
j5=0
else
j1=j1+3
if(stjb(j1-1).eq.stty%z)then
j4=0
else if(stjb(j1-1).eq.stty%ns)then
j3=stjb(j1+1)
j3=iachar(stcb(j3:j3))
if(j3.eq.aplus)then
j4=1
else if(j3.eq.aminus)then
j4=-1
else
ii=1
exit
end if
else
ii=1
exit
end if
j5=3
end if
stib(vpartj(0)+j2)=j4
j4=abs(j4)
j3=stoz(stcb,stjb(j1),stjb(j1+1)-j4)
if(j3.lt.0)then
ii=1
exit
end if
j1=j1+j5
stib(vparti(0)+j2)=j3
end do
if(ii.ne.0)then
call wput(-17,0,0)
end if
call aoib(1)
stib(stibs(1))=eoa
go to 143
111 continue
if(comc(dekc%off).ne.1)then
go to 80
else if(stapi(2).eq.0)then
go to 143
else if(stapi(2).eq.1)then
j1=pp1+7
j2=stjb(j1+1)
kk=sigz(stcb,stjb(j1),j2)
if(kk.lt.0)then
go to 80
else if(kk.eq.0)then
go to 143
end if
cflag(7)=1
else
go to 80
end if
j3=ilsd(stcb,stjb(j1),j2)
noffl=j2-(j3-1)
if(noffl.ge.wsint)then
ii=stoz(stcb,j1,j2)
end if
stcb(noffp+1:noffp+noffl)=stcb(j3:j2)
doffl=noffl
stcb(doffp+1:doffp+doffl)=stcb(j3:j2)
go to 143
130 continue
if(stapi(2).le.0)then
go to 80
end if
ntf=ntf+1
go to 143
143 continue
if((icom.eq.dekc%tru).or.&
(icom.eq.dekc%fals))then
j1=stjbs
nmp=nmp+1
ii=3
call aojb(ii)
do i1=1,2
j1=j1+1
stjb(j1)=stapi(i1)
end do
j1=j1+1
stjb(j1)=ncom
else
call aojb(-1)
end if
go to 03
301 continue
if(stib(cfrp(1)).ne.nap)then
mlin(1:srec)='inputs_6'
call mput(1,1,0,0,0)
end if
if(cflag(2).ge.0)then
if(cflag(11).eq.0)then
call spp(2,0,0,1)
end if
end if
j2=stibs(1)
ii=ndrec+1
call aoib(datls*ii)
drecii(0)=stibs(1)-ii
dreci(0)=drecii(0)-ii
drecl(0)=dreci(0)-ii
drecp(0)=drecl(0)-ii
stib(stibs(1))=eoa
stib(drecii(0))=eoa
stib(dreci(0))=eoa
stib(drecl(0))=eoa
j1=cfrp(0)
j2=0
do while(j1.ne.nap)
j2=j2+1
stib(drecp(0)+j2)=stib(j1+1)
stib(drecl(0)+j2)=stib(j1+2)
stib(dreci(0)+j2)=stib(j1+3)
stib(drecii(0)+j2)=stib(j1+4)
j1=stib(j1)
end do
ncom=0
j1=0
do while(j1.lt.ndrec)
j1=j1+1
ii=stib(drecp(0)+j1)
jj=ii-1
j2=j1
308 continue
jj=jj+stib(drecl(0)+j2)
if(j2.lt.ndrec)then
if(stib(drecii(0)+j2+1).ne.0)then
j2=j2+1
go to 308
end if
end if
j1=j2
ncom=ncom+1
call aoib(2)
stib(stibs(1)-1)=ii
stib(stibs(1))=jj
end do
call aoib(2)
stib(stibs(1)-1)=eoa
stib(stibs(1))=eoa
call trm(2,ncom+1,stibs(1))
ii=ncom+1
comfi(0)=stibs(1)-ii
comii(0)=comfi(0)-ii
irecc(0)=stibs(1)
call aoib(2*ii)
frecc(0)=irecc(0)+ii
stib(frecc(0))=eoa
stib(stibs(1))=eoa
j1=0
j2=0
do while(j1.lt.ndrec)
j1=j1+1
if(stib(drecii(0)+j1).eq.0)then
if(j2.gt.0)then
stib(frecc(0)+j2)=j1-1
end if
j2=j2+1
stib(irecc(0)+j2)=j1
end if
end do
if((j1.ne.ndrec).or.&
(j2.ne.ncom))then
mlin(1:srec)='inputs_4'
call mput(1,1,0,0,0)
end if
stib(frecc(0)+ncom)=ndrec
phase=2
if(ntf.gt.0)then
ii=ntf+1
tftyp(0)=stibs(1)
jj=9*ii
call aoib(jj)
tfnarg(0)=tftyp(0)+ii
tfa(0)=tfnarg(0)+ii
tfb(0)=tfa(0)+ii
tfc(0)=tfb(0)+ii
tf2(0)=tfc(0)+ii
tfo(0)=tf2(0)+ii
tfills(0)=tfo(0)+ii
tfiuls(0)=tfills(0)+ii
stib(tfills(0))=eoa
stib(tfiuls(0))=eoa
stib(tfnarg(0))=eoa
stib(tfa(0))=eoa
stib(tfb(0))=eoa
stib(tfc(0))=eoa
stib(tf2(0))=eoa
stib(tfo(0))=eoa
stib(stibs(1))=eoa
end if
ntf1=0
pp0=nap
nmp0=1
501 continue
do i1=nmp0,nmp
if(pp0.eq.nap)then
pp0=stjr0+1
else
pp0=stjb(pp0-2)+1
end if
pp1=pp0+1
pp2=pp1+3*(stjb(pp0)-1)
pp0=stjb(pp0-2)+1
icom=stjb(pp0)
nmp0=i1+1
j1=stjb(pp0+2)
dbl=stib(dreci(0)+stib(irecc(0)+j1))
nlin=dbl+stib(drecii(0)+stib(frecc(0)+j1))
if((icom.eq.dekc%tru).or.&
(icom.eq.dekc%fals))then
go to 611
else
go to 80
end if
end do
go to 701
611 continue
jj=stjb(pp1-1)
if(jj.lt.7)then
go to 80
end if
if(icom.eq.dekc%tru)then
tfv=1
else if(icom.eq.dekc%fals)then
tfv=-1
else
go to 80
end if
j1=stjb(pp1+7)
j2=stjb(pp1+8)
ij=0
call mstr0(stcb,j1,j2,popt2(0),wopt2(0),aopt2(0),ij)
if(ij.eq.0)then
go to 80
end if
atyp=stib(copt2(0)+ij)
if(atyp.le.0)then
go to 80
end if
styp=atyp*tfv
if(atyp.eq.6)then
dflag(18)=1
else if(atyp.eq.7)then
dflag(19)=1
else if(atyp.eq.11)then
dflag(21)=1
else if(atyp.eq.12)then
dflag(22)=1
else if(atyp.eq.15)then
dflag(20)=1
end if
j1=stjb(pp1-1)
j3=-1
if(atyp.lt.11)then
j3=j1-8
j4=2
else if(atyp.eq.11)then
j3=j1-10
j4=3
else if(atyp.eq.12)then
j3=j1-4
j4=0
end if
if(j3.eq.0)then
j3=1
else if(j3.lt.0)then
go to 80
end if
j3=(j3-1)/2
if(atyp.ge.11)then
if(j3.eq.0)then
go to 80
end if
end if
if(j4.gt.1)then
j1=stoz(stcb,stjb(pp2-11),stjb(pp2-10))
j2=stoz(stcb,stjb(pp2-5),stjb(pp2-4))
if(j1.gt.j2)then
go to 80
end if
if(atyp.lt.6)then
if(j1.lt.0)then
go to 80
end if
if(atyp.gt.1)then
jflag(17)=1
end if
else if(atyp.eq.11)then
if(j1.le.0)then
go to 80
end if
end if
else
j1=0
j2=0
end if
if(j3.eq.0)then
go to 673
end if
ntf1=ntf1+1
stib(tftyp(0)+ntf1)=styp
stib(tfnarg(0)+ntf1)=j3
stib(tfa(0)+ntf1)=j1
stib(tfb(0)+ntf1)=j2
stib(tfc(0)+ntf1)=0
stib(tfo(0)+ntf1)=stibs(1)
call aoib(j3+1)
if(dflag(18).ne.0)then
stib(tfills(0)+ntf1)=stibs(1)
call aoib(maxn+1)
stib(tfiuls(0)+ntf1)=stibs(1)
call aoib(maxn+1)
end if
stib(stibs(1))=eoa
if(atyp.gt.5)then
go to 637
end if
j4=stib(tfo(0)+ntf1)
ii=pp1+7
do i1=1,stib(tfnarg(0)+ntf1)
ii=ii+6
jk=0
call mstr0(stcb,stjb(ii),stjb(ii+1),namep(0),namel(0),nap,jk)
if(jk.eq.0)then
mlin(1:srec)="unknown field,"
go to 81
end if
stib(j4+i1)=jk
end do
call hsort(j4+1,j4+stib(tfnarg(0)+ntf1))
tfcl=stib(tfnarg(0)+ntf1)
tfch=1
tfcn=1
i3=1
do i1=2,stib(tfnarg(0)+ntf1)
i2=j4+i1
if((stib(i2).eq.stib(i2-1)).or.&
(stib(i2).eq.stib(link(0)+stib(i2-1))))then
i3=i3+1
if(i3.gt.tfch)then
tfch=i3
end if
else
if(i3.lt.tfcl)then
tfcl=i3
end if
tfcn=tfcn+1
i3=1
end if
end do
if(i3.lt.tfcl)then
tfcl=i3
end if
if(tfcn.ne.nprop)then
tfcl=0
end if
if(styp.gt.0)then
do i2=0,maxli
if((i2*tfch.lt.stib(tfa(0)+ntf1)).or.&
(i2*tfcl.gt.stib(tfb(0)+ntf1)))then
if(styp.eq.1)then
zpro(i2)=0
else if(styp.eq.2)then
zbri(i2)=0
else if(styp.eq.3)then
sbri(i2)=0
else if(styp.eq.4)then
rbri(i2)=0
else if(styp.eq.5)then
zcho(i2)=0
end if
end if
end do
else if(styp.lt.0)then
do i2=0,maxli
if(i2*tfch.le.stib(tfb(0)+ntf1))then
if(i2*tfcl.ge.stib(tfa(0)+ntf1))then
if(styp.eq.-1)then
zpro(i2)=0
else if(styp.eq.-2)then
zbri(i2)=0
else if(styp.eq.-3)then
sbri(i2)=0
else if(styp.eq.-4)then
rbri(i2)=0
else if(styp.eq.-5)then
zcho(i2)=0
end if
end if
end if
end do
end if
go to 501
673 continue
if(styp.gt.0)then
do i2=0,maxli
if((i2.lt.j1).or.&
(i2.gt.j2))then
if(styp.eq.1)then
zpro(i2)=0
else if(styp.eq.2)then
zbri(i2)=0
else if(styp.eq.3)then
sbri(i2)=0
else if(styp.eq.4)then
rbri(i2)=0
else if(styp.eq.5)then
zcho(i2)=0
end if
end if
end do
else
do i2=0,maxli
if(i2.ge.j1)then
if(i2.le.j2)then
if(styp.eq.-1)then
zpro(i2)=0
else if(styp.eq.-2)then
zbri(i2)=0
else if(styp.eq.-3)then
sbri(i2)=0
else if(styp.eq.-4)then
rbri(i2)=0
else if(styp.eq.-5)then
zcho(i2)=0
end if
end if
end if
end do
end if
go to 501
637 continue
if(atyp.eq.6)then
if(stib(tfnarg(0)+ntf1).ne.1)then
go to 80
end if
j1=stjb(pp1+13)
j2=stjb(pp1+14)
kid=0
call mstr0(stcb,j1,j2,vmkp(0),vmkl(0),nap,kid)
if(kid.gt.0)then
if(kid.gt.nvmk)then
kid=0
else if(stib(vmkt(0)+kid).ne.4)then
kid=0
end if
else
kid=0
end if
if(kid.eq.0)then
mlin(1:srec)="invalid argument in vsum,"
go to 81
end if
stib(vmks(0)+kid)=1
stib(stib(tfo(0)+ntf1)+1)=kid
else if(atyp.eq.7)then
if(stib(tfnarg(0)+ntf1).ne.1)then
go to 80
end if
j1=stjb(pp1+13)
j2=stjb(pp1+14)
kid=0
call mstr0(stcb,j1,j2,pmkp(0),pmkl(0),nap,kid)
if(kid.gt.0)then
if(kid.gt.npmk)then
kid=0
else if(stib(pmkt(0)+kid).ne.4)then
kid=0
else if(stib(pmkd(0)+kid).ne.1)then
kid=0
end if
else
kid=0
end if
if(kid.eq.0)then
mlin(1:srec)="invalid argument in psum,"
go to 81
end if
stib(stib(tfo(0)+ntf1)+1)=kid
j1=stib(pmkvmi(0)+kid)
j2=stib(pmkvma(0)+kid)
do i2=0,maxli
if(styp.gt.0)then
if((i2*j1.gt.stib(tfb(0)+ntf1)).or.&
(i2*j2.lt.stib(tfa(0)+ntf1)))then
zpro(i2)=0
end if
else
if((i2*j1.ge.stib(tfa(0)+ntf1)).and.&
(i2*j2.le.stib(tfb(0)+ntf1)))then
zpro(i2)=0
end if
end if
end do
else if((atyp.ge.11).and.&
(atyp.le.15))then
if(nleg.lt.2)then
call wput(-14,0,0)
end if
j3=stib(tfnarg(0)+ntf1)
if(j3.le.0)then
go to 80
else if(j3.gt.nleg)then
go to 80
end if
if(atyp.eq.11)then
if(j3.lt.stib(tfb(0)+ntf1))then
go to 80
end if
else
if(j3.eq.nleg)then
go to 80
end if
end if
if(nleg.gt.0)then
xli(1:nleg)=0
end if
do i1=1,j3
j1=pp1+7+6*i1
j2=stoz(stcb,stjb(j1),stjb(j1+1))
if(j2.ge.0)then
go to 80
else if(j2+2*nleg.lt.0)then
go to 80
end if
j1=(-j2)/2
if(j2+2*j1.eq.0)then
j1=j1+inco
if(j1.gt.nleg)then
go to 80
end if
else
j1=j1+1
if(j1.gt.inco)then
go to 80
end if
end if
if(xli(j1).ne.0)then
go to 80
end if
xli(j1)=1
j4=stib(tfo(0)+ntf1)
stib(j4+i1)=j1
end do
if(atyp.eq.11)then
j1=pp1+13+6*j3
ij=0
call mstr0(stcb,stjb(j1),stjb(j1+1),popt3(0),wopt3(0),aopt3(0),ij)
if(ij.le.0)then
go to 80
end if
j1=stib(copt3(0)+ij)
stib(tfc(0)+ntf1)=j1
j2=0
if(j1.ne.0)then
if(j3.eq.nleg)then
if(stib(tfa(0)+ntf1).eq.1)then
if(stib(tfb(0)+ntf1).eq.nleg)then
j2=styp
end if
end if
end if
else
if(stib(tfa(0)+ntf1).eq.1)then
if(stib(tfb(0)+ntf1).eq.j3)then
j2=styp
end if
end if
end if
if(j2.ne.0)then
if(j2.gt.0)then
call wput(12,0,0)
else
jflag(1)=1
call wput(13,0,0)
end if
end if
end if
else
mlin(1:srec)='inputs_3'
call mput(1,1,0,0,0)
end if
go to 501
701 continue
if(cflag(2).gt.0)then
if(cflag(11).eq.0)then
call spp(3,0,0,0)
end if
end if
vmkmio(0)=nap
vmkmao(0)=nap
if(dflag(18).ne.0)then
call vsig
end if
i1=0
kk=0
705 continue
i1=i1+1
j1=stib(copt2(0)+i1)
if(j1.gt.0)then
ik=i1
if(j1.gt.kk)then
kk=j1
else if(j1.lt.kk)then
mlin(1:srec)='inputs_5'
call mput(1,1,0,0,0)
end if
go to 705
end if
ii=ncopt2+1
jj=2*ii
call aoib(jj)
tftb(0)=stibs(1)-ii
tfta(0)=tftb(0)-ii
stib(tftb(0))=eoa
stib(stibs(1))=eoa
jj=mcopt2+1
tftic(0)=stibs(1)
call aoib(jj)
stib(stibs(1))=eoa
if(mcopt2.gt.0)then
stib(tftic(0)+1:tftic(0)+mcopt2)=nap
end if
do i1=1,ncopt2
j1=stib(copt2(0)+i1)
if(stib(tftic(0)+j1).eq.nap)then
stib(tftic(0)+j1)=i1
end if
end do
if(ncopt2.gt.0)then
stib(tfta(0)+1:tfta(0)+ncopt2)=0
stib(tftb(0)+1:tftb(0)+ncopt2)=-1
end if
ii=0
do i1=1,mcopt2
j1=stib(tftic(0)+i1)
if(j1.gt.0)then
do i2=1,ntf
if(abs(stib(tftyp(0)+i2)).eq.i1)then
ii=ii+1
if(stib(tfta(0)+j1).eq.0)then
stib(tfta(0)+j1)=ii
end if
stib(tftb(0)+j1)=ii
stib(tf2(0)+ii)=i2
end if
end do
end if
end do
j1=stcbs(1)+1
j2=2
ii=stib(momel(0))+j2
call aocb(ii)
j3=stcbs(1)-j2
stcb(j1:j3)=stcb(stib(momep(0)):stib(momeq(0)))
do i1=1,loopx(4)
call dkar(i1,jk)
ik=j3+jk
do i2=1,nleg
if(stib(momel(0)+i2).eq.stib(momel(0))+jk)then
if(stcb(j1:ik).eq.stcb(stib(momep(0)+i2):stib(momeq(0)+i2)))then
mlin(1:srec)="conflict between names of external and internal "&
//"momenta"
if(cflag(1).gt.0)then
call mput(1,1,0,0,0)
else if(cflag(2).gt.0)then
call mput(0,0,0,0,0)
else
jflag(8)=min(jflag(8)+1,iref)
end if
exit
end if
end if
end do
end do
call aocb(-ii)
ii=1
do i1=1,nleg
if(stib(antiq(0)+stib(leg(0)+i1)).ne.0)then
ii=-ii
end if
end do
if(ii.lt.0)then
call wput(9,0,0)
jflag(1)=1
end if
if(nleg.gt.1)then
ii=abs(stib(blok(0)+stib(leg(0)+1)))
do i1=2,nleg
if(abs(stib(blok(0)+stib(leg(0)+i1))).ne.ii)then
call wput(8,0,0)
jflag(1)=1
exit
end if
end do
end if
if((nleg.ne.2).or.&
(loopx(4).ne.0))then
d01: do i1=1,nleg
jj=stib(link(0)+stib(leg(0)+i1))
do i2=1,nrho
if(stib(nivd(0)+i2).gt.0)then
if(stib(stib(stib(dpntro(0)+i2)+jj)+1).eq.jj)then
cycle d01
end if
end if
end do
jflag(1)=1
kk=0
if(i1.gt.inco)then
ii=stib(namep(0)+stib(leg(0)+i1))
do i2=inco+1,i1-1
if(stib(leg(0)+i2).eq.stib(leg(0)+i1))then
kk=1
end if
end do
if(kk.eq.0)then
j1=ii-1+stib(namel(0)+stib(leg(0)+i1))
call wput(7,ii,j1)
end if
else
ii=stib(namep(0)+jj)
do i2=1,i1-1
if(stib(leg(0)+i2).eq.stib(leg(0)+i1))then
kk=1
end if
end do
if(kk.eq.0)then
j1=ii-1+stib(namel(0)+jj)
call wput(6,ii,j1)
end if
end if
end do d01
end if
do i1=1,nleg
jj=stib(tpc(0)+stib(link(0)+stib(leg(0)+i1)))
if((jj.eq.phity%int).or.&
(jj.eq.phity%ntint))then
call wput(-20,0,0)
exit
end if
end do
do i1=1,nphi
j1=tpc(0)+i1
if(stib(j1).eq.phity%int)then
stib(j1)=phity%def
else if(stib(j1).eq.phity%ntint)then
stib(j1)=phity%notdp
end if
end do
go to 90
80 continue
mlin(1:srec)="wrong syntax or unexpected statement,"
81 continue
call mput(1,1,dbl,nlin,ifile)
90 continue
do i1=1,2*nmp
call aojb(-1)
end do
if((nstjb.ne.0).or.&
(stjbs.ne.stj0))then
mlin(1:srec)='inputs_11'
call mput(1,1,0,0,0)
end if
call sdiag(1,-1)
if(cflag(1).gt.0)then
call iki
end if
call rflag0
vsfi(0)=nap
psfi(0)=nap
nivdx(0)=nap
if((dflag(18).ne.0).or.&
(dflag(19).ne.0))then
call vsf
end if
return
end subroutine inputs
subroutine iki
use,non_intrinsic::cbuffer
use,non_intrinsic::qfiles
use,non_intrinsic::qmodel
use,non_intrinsic::qintr
use,non_intrinsic::qsty
use,non_intrinsic::zbuffer
implicit none
save
integer::i1,i2,j1,j2,jj,kk
integer::ifile,ierr
ifile=0
do i1=1,nfiles
if(filt(i1).eq.qfty%mtyp)then
ifile=i1
exit
end if
end do
ierr=0
do i1=1,nudk
j1=stib(udkp(0)+i1)
j2=j1-1+stib(udkl(0)+i1)
kk=0
jj=0
call mstr0(stcb,j1,j2,gmkp(0),gmkl(0),nap,jj)
if(jj.gt.0)then
kk=1
end if
if(kk.eq.0)then
jj=0
call mstr0(stcb,j1,j2,pmkp(0),pmkl(0),nap,jj)
if(jj.gt.0)then
kk=2
end if
end if
if(kk.eq.0)then
jj=0
call mstr0(stcb,j1,j2,vmkp(0),vmkl(0),nap,jj)
if(jj.gt.0)then
kk=3
end if
end if
if(kk.eq.0)then
call emodel(18,0,1,0,0,ifile)
ierr=1
exit
end if
stib(udkt(0)+i1)=kk
stib(udki(0)+i1)=jj
end do
j1=udkp(1)
do i1=1,nudk
j1=stib(j1)
if(stib(udkt(0)+i1).eq.1)then
do i2=3,7
if(stib(j1+i2).gt.1)then
call emodel(12,0,1,0,0,ifile)
ierr=1
exit
end if
end do
else if(stib(udkt(0)+i1).eq.2)then
if(stib(j1+3).ne.0)then
call emodel(13,0,1,0,0,ifile)
ierr=1
end if
if(stib(pmkd(0)+stib(udki(0)+i1)).eq.3)then
if(stib(j1+6).ne.0)then
call emodel(14,0,1,0,0,ifile)
ierr=1
end if
else
if(stib(j1+6).ne.0)then
call emodel(14,0,1,0,0,ifile)
ierr=1
end if
do i2=4,7
if(stib(j1+i2).gt.1)then
call emodel(16,0,1,0,0,ifile)
ierr=1
exit
end if
end do
end if
else if(stib(udkt(0)+i1).eq.3)then
if(stib(j1+3).ne.0)then
call emodel(13,0,1,0,0,ifile)
ierr=1
end if
do i2=4,7
if((i2.ne.5).and.&
(stib(j1+i2).gt.1))then
call emodel(17,0,1,0,0,ifile)
ierr=1
exit
end if
end do
end if
if(ierr.ne.0)then
call qclose(0,0)
if(cflag(11).eq.0)then
stop
else
stop
go to 90
end if
end if
end do
90 continue
return
end subroutine iki
subroutine rsfki
use,non_intrinsic::qintr
use,non_intrinsic::qmodel
use,non_intrinsic::qsty
use,non_intrinsic::zbuffer
implicit none
save
integer::ii,i1,j1
ii=nudk+1
udkp(0)=stibs(1)
call aoib(ii)
stib(stibs(1))=eoa
udkl(0)=stibs(1)
call aoib(ii)
stib(stibs(1))=eoa
udkt(0)=stibs(1)
call aoib(ii)
stib(stibs(1))=eoa
udki(0)=stibs(1)
call aoib(ii)
stib(stibs(1))=eoa
if(nudk.gt.0)then
stib(udkt(0)+1:udkt(0)+nudk)=0
stib(udki(0)+1:udki(0)+nudk)=0
j1=udkp(1)
do i1=1,nudk
j1=stib(j1)
stib(udkp(0)+i1)=stib(j1+1)
stib(udkl(0)+i1)=stib(j1+2)
end do
end if
return
end subroutine rsfki
subroutine vsig
use,non_intrinsic::cbuffer
use,non_intrinsic::qintr
use,non_intrinsic::qmix
use,non_intrinsic::qmodel
use,non_intrinsic::zbuffer
implicit none
save
integer,external::stoz
integer::ii,ij,jj,i1,i2,j1,j2,p1,p2,p3
ii=nvmk+1
vmkzp(0)=stibs(1)
call aoib(ii)
stib(stibs(1))=eoa
vmkmio(0)=stibs(1)
call aoib(ii)
stib(stibs(1))=eoa
vmkmao(0)=stibs(1)
call aoib(ii)
stib(stibs(1))=eoa
do i1=1,nvmk
if(stib(vmks(0)+i1).ne.1)then
stib(vmkzp(0)+i1)=nap
stib(vmkmio(0)+i1)=nap
stib(vmkmao(0)+i1)=nap
else
p3=stibs(1)
stib(vmkzp(0)+i1)=p3
ii=nvert
call aoib(ii)
ii=nrho
p1=stibs(1)
stib(vmkmio(0)+i1)=p1
call aoib(ii)
p2=stibs(1)
stib(vmkmao(0)+i1)=p2
call aoib(ii)
call vaoib(ii)
stib(stibs(1)+1:stibs(1)+ii)=0
ii=stib(vmkvpp(0)+i1)
jj=stib(vmkvlp(0)+i1)
do i2=1,nvert
j1=stib(ii+i2)
j2=(j1-1)+stib(jj+i2)
ij=stoz(stcb,j1,j2)
stib(p3+i2)=ij
j1=stib(vval(0)+i2)
j2=stibs(1)+j1
if(stib(j2).ne.0)then
if(ij.gt.stib(p2+j1))then
stib(p2+j1)=ij
else if(ij.lt.stib(p1+j1))then
stib(p1+j1)=ij
end if
else
stib(j2)=1
stib(p1+j1)=ij
stib(p2+j1)=ij
end if
end do
end if
end do
if(stib(stibs(1)).ne.eoa)then
call aoib(1)
stib(stibs(1))=eoa
end if
return
end subroutine vsig
subroutine g10
use,non_intrinsic::qintr
use,non_intrinsic::qmix
use,non_intrinsic::qmodel
use,non_intrinsic::qproc
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer::ii,i1,i2,i3,i4,i5,jj,j1,j2,j3,kk
integer::atyp,aux,a1,a2,b1,b2,styp
integer::vaux(1:maxli),xli(1:maxleg),xlj(1:maxn)
if(qco(qcntr%e).eq.0)then
if(dflag(16).eq.0)then
go to 29
end if
else
qco(qcntr%e)=0
end if
27 continue
call g21
if(qco(qcntr%t).eq.0)then
go to 90
end if
do i1=1,n
vaux(i1)=xn(i1)
end do
do i1=1,rho(1)
vlis(i1)=i1
invlis(i1)=i1
end do
do i1=rhop1,n
invlis(i1)=0
end do
aux=rho(1)
jj=rhop1
10 continue
if(aux.lt.n)then
20 continue
if(invlis(jj).ne.0)then
jj=jj+1
go to 20
end if
ii=jj
do i1=ii+1,n
if(invlis(i1).eq.0)then
if(vaux(i1).gt.vaux(ii))then
ii=i1
end if
end if
end do
if(vaux(ii).eq.0)then
mlin(1:srec)='g10_1'
call mput(1,1,0,0,0)
end if
aux=aux+1
vlis(aux)=ii
invlis(ii)=aux
rdeg(ii)=vaux(ii)
sdeg(ii)=rdeg(ii)+gam(ii,ii)
do i1=rhop1,n
vaux(i1)=vaux(i1)+gam(i1,ii)
end do
go to 10
end if
if(n.gt.0)then
vaux(1:n)=0
end if
do i1=1,n
ii=vlis(i1)
kk=0
j1=1
30 continue
if(kk.lt.vdeg(ii))then
jj=vlis(j1)
aux=1
do i3=1,gam(ii,jj)
kk=kk+1
vmap(ii,kk)=jj
vaux(jj)=vaux(jj)+1
if(ii.ne.jj)then
lmap(ii,kk)=vaux(jj)
else
lmap(ii,kk)=vaux(jj)+aux
aux=-aux
end if
end do
j1=j1+1
go to 30
end if
if(kk.ne.vdeg(ii))then
mlin(1:srec)='g10_2'
call mput(1,1,0,0,0)
end if
end do
do i1=rhop1,n
do i2=1,vdeg(i1)-1
if(invlis(vmap(i1,i2)).gt.invlis(vmap(i1,i2+1)))then
mlin(1:srec)='g10_3'
call mput(1,1,0,0,0)
end if
end do
end do
do i1=1,n
do i2=1,vdeg(i1)
j1=vmap(i1,i2)
j2=lmap(i1,i2)
if(vmap(j1,j2).ne.i1)then
mlin(1:srec)='g10_4'
call mput(1,1,0,0,0)
end if
if(lmap(j1,j2).ne.i2)then
mlin(1:srec)='g10_5'
call mput(1,1,0,0,0)
end if
end do
end do
if(cflag(1)+jflag(17).ne.0)then
do i1=rhop1,n
do i2=1,vdeg(i1)
j3=vmap(i1,i2)
if(j3.gt.rho(1))then
j1=min(i1,j3)
j2=max(i1,j3)
ii=ege(j1,j2)
jj=gam(i1,j3)-1
if(jj.eq.0)then
amap(i1,i2)=ii
else if(i1.eq.j3)then
amap(i1,i2)=ii+((i2-1-rdeg(i1))/2)
else
kk=0
do i3=i2-1,1,-1
if(vmap(i1,i3).eq.j3)then
kk=kk+1
else
exit
end if
end do
amap(i1,i2)=ii+kk
end if
end if
end do
end do
end if
if(dflag(9).ne.0)then
call ccyc(1,j1)
if(j1.ne.dflag(9))then
qco(qcntr%t)=0
go to 27
end if
end if
do i1=1,rho(1)
ps1(i1)=i1
end do
go to 70
29 continue
do i1=rho(1)-1,1,-1
if(ps1(i1).lt.ps1(i1+1))then
j1=i1
go to 38
end if
end do
qco(qcntr%t)=0
go to 27
38 continue
do i1=j1+2,rho(1)
if(ps1(i1).lt.ps1(j1))then
j2=i1-1
go to 61
end if
end do
j2=rho(1)
61 continue
ii=ps1(j1)
ps1(j1)=ps1(j2)
ps1(j2)=ii
j1=j1+1
j2=rho(1)
33 continue
if(j1.lt.j2)then
ii=ps1(j1)
ps1(j1)=ps1(j2)
ps1(j2)=ii
j1=j1+1
j2=j2-1
go to 33
end if
70 continue
do i1=1,ns1
if(ps1(p1r(i1)).lt.ps1(p1l(i1)))then
do i2=p1r(i1)+1,rho(1)-1
do i3=i2+1,rho(1)
if(ps1(i2).lt.ps1(i3))then
ii=ps1(i2)
ps1(i2)=ps1(i3)
ps1(i3)=ii
end if
end do
end do
go to 29
end if
end do
do i1=1,rho(1)
invps1(ps1(i1))=i1
end do
if(dflag(21).ne.0)then
ii=stib(tftic(0)+11)
do i1=stib(tfta(0)+ii),stib(tftb(0)+ii)
i3=stib(tf2(0)+i1)
styp=stib(tftyp(0)+i3)
j1=stib(tfnarg(0)+i3)
j2=stib(tfo(0)+i3)
do i2=rhop1,n
xlj(i2)=0
end do
ii=0
do i2=1,j1
j3=vmap(invps1(stib(j2+i2)),1)
if(xlj(j3).eq.0)then
xlj(j3)=1
ii=ii+1
end if
end do
if((ii.lt.stib(tfa(0)+i3)).or.&
(ii.gt.stib(tfb(0)+i3)))then
if(styp.gt.0)then
go to 29
else
go to 55
end if
end if
if(stib(tfc(0)+i3).eq.0)then
if(styp.gt.0)then
go to 55
else
go to 29
end if
end if
if(rho(1).gt.0)then
xli(1:rho(1))=0
end if
do i2=1,j1
xli(invps1(stib(j2+i2)))=1
end do
do i2=1,rho(1)
if(xli(i2).eq.0)then
if(xlj(vmap(i2,1)).ne.0)then
if(styp.gt.0)then
go to 29
else
go to 55
end if
end if
end if
end do
if(styp.lt.0)then
go to 29
end if
55 continue
end do
end if
if(dflag(22).ne.0)then
do i1=12,12
ii=stib(tftic(0)+i1)
d02: do i2=stib(tfta(0)+ii),stib(tftb(0)+ii)
j3=stib(tf2(0)+i2)
styp=stib(tftyp(0)+j3)
j1=stib(tfnarg(0)+j3)
j2=stib(tfo(0)+j3)
if(rho(1).gt.0)then
xli(1:rho(1))=0
end if
do i3=1,j1
xli(invps1(stib(j2+i3)))=1
end do
do i3=rhop1,nli
if(flow(i3,0).eq.1)then
kk=0
do i4=1,rho(1)
if(flow(i3,i4).eq.0)then
if(xli(i4).ne.0)then
kk=kk+1
end if
else
if(xli(i4).eq.0)then
kk=kk+1
end if
end if
end do
if(kk.gt.0)then
kk=kk-rho(1)
end if
if(kk.eq.0)then
if(styp.gt.0)then
cycle d02
else
go to 29
end if
end if
end if
end do
if(styp.gt.0)then
go to 29
end if
end do d02
end do
end if
do i1=1,rho(1)
amap(i1,1)=i1
amap(vmap(i1,1),lmap(i1,1))=i1
end do
if(dflag(18).ne.0)then
do i1=1,ntf
styp=stib(tftyp(0)+i1)
atyp=abs(styp)
if(atyp.ne.6)then
cycle
end if
j3=stib(stib(tfo(0)+i1)+1)
a1=stib(vmkmio(0)+j3)
b1=stib(vmkmao(0)+j3)
i3=stib(tfills(0)+i1)
i4=stib(tfiuls(0)+i1)
a2=-stib(tfa(0)+i1)
b2=-stib(tfb(0)+i1)
do i2=n,rhop1,-1
i5=vdeg(vlis(i2))
if(styp.gt.0)then
a2=a2+stib(b1+i5)
b2=b2+stib(a1+i5)
else
a2=a2+stib(a1+i5)
b2=b2+stib(b1+i5)
end if
stib(i3+i2)=a2
stib(i4+i2)=b2
end do
end do
end if
qco(qcntr%e)=1
if(cflag(1).eq.0)then
go to 90
end if
do i1=1,nli
vaux(i1)=-2
end do
do i1=1,n
do i2=1,vdeg(i1)
ii=amap(i1,i2)
if(ii.gt.0)then
if(ii.le.nli)then
vaux(ii)=vaux(ii)+1
end if
end if
end do
end do
do i1=1,nli
if(vaux(i1).ne.0)then
mlin(1:srec)='g10_7'
call mput(1,1,0,0,0)
end if
end do
90 return
end subroutine g10
subroutine g21
use,non_intrinsic::qintr
use,non_intrinsic::qmix
use,non_intrinsic::qmodel
use,non_intrinsic::qproc
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer::bond,dsum,limax,limin,lin,loop,msum,n1,ntree
integer::aux,col,ii,i1,i2,i3,i4,jj,j1,j2,j3,j4,kk
integer::xc(1:maxdeg)
integer::xl(1:maxn),xt(1:maxn)
integer::bound(1:maxdeg),degr(1:maxdeg),xs(1:maxn)
integer::xg(1:maxn,1:maxn),ds(1:maxn,1:maxn)
integer::p1s(1:maxleg,1:maxleg)
integer::aa(1:maxn),bb(1:maxn)
integer::uset(0:maxn),xset(1:maxn),xp(1:maxn),a1(1:maxn)
integer::str(1:maxn),dta(1:maxn),lps(1:maxn)
integer::head(1:maxli),tail(1:maxli),intree(1:maxli)
integer::emul(1:maxdeg,1:maxli),nemul(1:maxdeg)
if(nrho.le.0)then
go to 97
end if
do i1=1,nrho
if((rho(i1).lt.0).or.&
(rho(i1).gt.maxn))then
go to 97
end if
end do
if(qco(qcntr%t).eq.0)then
go to 05
else
qco(qcntr%t)=0
end if
27 continue
call pg11
if(qco(qcntr%p).eq.0)then
go to 90
end if
n=0
jj=0
do i1=1,nrho
jj=jj+i1*rho(i1)
do i2=1,rho(i1)
n=n+1
vdeg(n)=i1
end do
end do
if(n.gt.maxn)then
go to 97
end if
nli=jj/2
if(jj.ne.nli+nli)then
go to 97
end if
loop=nli-n+1
rhop2=rho(1)+loop
if(loop.lt.0)then
go to 97
end if
j1=0
if(dflag(13).gt.0)then
j1=1
else if(dflag(14).gt.0)then
j1=1
else if(dflag(15).gt.0)then
j1=1
end if
do i1=rhop1,n
if(j1.ne.0)then
str(i1)=1
else if(vdeg(i1).ne.vdeg(n))then
str(i1)=min(vdeg(i1),loop+1)
else if(n.gt.2)then
str(i1)=min(vdeg(i1)-1,loop+1)
else
str(i1)=min(vdeg(i1),loop+1)
end if
end do
j1=1
xl(1)=1
do i1=2,n
if(vdeg(i1).ne.vdeg(i1-1))then
xt(j1)=i1-1
j1=j1+1
xl(j1)=i1
end if
end do
xt(j1)=n
if(n.gt.0)then
xn(1:n)=0
end if
if(rho(1).eq.0)then
xc(1)=0
go to 59
end if
kk=0
n1=1
do i1=2,nrho
if(rho(i1).gt.0)then
n1=n1+1
degr(n1)=i1
bound(n1)=i1*rho(i1)
kk=kk+bound(n1)
end if
end do
xc(1)=max(0,rho(1)-kk)
61 continue
ii=rho(1)-xc(1)
do i1=n1,2,-1
xc(i1)=min(ii,bound(i1))
ii=ii-xc(i1)
end do
43 continue
do i1=n1,2,-1
ii=xc(i1)
do i2=xl(i1),xt(i1)
xn(i2)=min(ii,degr(i1))
ii=ii-xn(i2)
xs(i2)=ii
end do
end do
go to 12
76 continue
d01: do i1=n1,2,-1
do i2=xt(i1)-1,xl(i1),-1
if(xn(i2).gt.1)then
j1=i2
go to 89
end if
end do
cycle d01
89 continue
xn(j1)=xn(j1)-1
xs(j1)=xs(j1)+1
do i2=j1+1,xt(i1)
xn(i2)=min(xn(j1),xs(i2-1))
xs(i2)=xs(i2-1)-xn(i2)
end do
if(xs(xt(i1)).gt.0)then
do i2=j1-1,xl(i1),-1
if(xn(i2).gt.1)then
j1=i2
go to 89
end if
end do
cycle d01
end if
do i2=i1+1,n1
ii=xc(i2)
do i3=xl(i2),xt(i2)
xn(i3)=min(ii,degr(i2))
ii=ii-xn(i3)
xs(i3)=ii
end do
end do
go to 12
end do d01
do i1=n1,3,-1
if(xc(i1).gt.0)then
j1=i1
go to 45
end if
end do
go to 85
45 continue
do i1=j1-1,2,-1
if(xc(i1).lt.bound(i1))then
j1=i1
go to 55
end if
end do
go to 85
55 continue
xc(j1)=xc(j1)+1
ii=-1
do i1=j1+1,n1
ii=ii+xc(i1)
end do
do i1=n1,j1+1,-1
xc(i1)=min(ii,bound(i1))
ii=ii-xc(i1)
end do
if(ii.ne.0)then
mlin(1:srec)='g21_2'
call mput(1,1,0,0,0)
end if
go to 43
85 continue
xc(1)=xc(1)+2
if(xc(1).le.rho(1))then
go to 61
end if
qco(qcntr%p)=0
go to 27
12 continue
if(rho(1).lt.n)then
if(xc(1).gt.0)then
qco(qcntr%p)=0
go to 27
end if
end if
if(n.gt.rhop1)then
j1=0
if(dflag(1).gt.0)then
j1=1
end if
do i1=rhop1,n
if(xn(i1)+j1.ge.vdeg(i1))then
go to 76
end if
end do
end if
ii=1
if(loop.eq.0)then
ii=0
else if(dflag(11).gt.0)then
ii=0
else if(dflag(12).gt.0)then
ii=0
else if(dflag(15).gt.0)then
ii=0
else
if(dflag(8).gt.0)then
if(rho(1)-1.ne.0)then
ii=0
end if
end if
if(dflag(9).gt.0)then
if(loop-1.ne.0)then
ii=0
end if
end if
end if
if(ii.eq.0)then
do i1=rhop1,n
dta(i1)=0
end do
else
do i1=rhop1,n
ii=0
if(n.gt.rhop1)then
if(dflag(1).gt.0)then
ii=2
else if((dflag(3).gt.0).and.&
(xn(i1).eq.0))then
ii=2
else
ii=1
end if
end if
jj=max(0,min(vdeg(i1)-xn(i1)-ii,loop+loop))/2
dta(i1)=jj+jj
if(dflag(14).gt.0)then
dta(i1)=min(dta(i1),2)
end if
end do
end if
59 continue
do i1=1,n-1
xg(i1,i1+1:n)=0
end do
do i1=2,xc(1),2
xg(i1-1,i1)=1
end do
limin=rhop1
limax=max(limin,n-1)
lin=limin
j1=xc(1)
do i1=limin,n
if(xn(i1).gt.0)then
a1(i1)=j1+1
do i2=1,xn(i1)
j1=j1+1
xg(j1,i1)=1
vmap(j1,1)=i1
end do
else
a1(i1)=0
end if
end do
dsum=-1
70 continue
dsum=dsum+1
if(dsum.gt.loop)then
go to 76
end if
ii=2*dsum
do i1=n,rhop1,-1
jj=min(ii,dta(i1))
xg(i1,i1)=jj
ii=ii-jj
end do
if(ii.eq.0)then
go to 19
else
go to 76
end if
32 continue
do i1=n,rhop1,-1
if(xg(i1,i1).gt.0)then
j1=i1
go to 33
end if
end do
go to 70
33 continue
do i1=j1-1,rhop1,-1
if(xg(i1,i1).lt.dta(i1))then
xg(i1,i1)=xg(i1,i1)+2
j2=i1
go to 64
end if
end do
go to 70
64 continue
ii=-2
do i1=j2+1,j1
ii=ii+xg(i1,i1)
end do
do i1=n,j2+1,-1
jj=min(ii,dta(i1))
xg(i1,i1)=jj
ii=ii-jj
end do
if(ii.ne.0)then
mlin(1:srec)='g21_3'
call mput(1,1,0,0,0)
end if
19 continue
do i1=limin,n
ds(limin,i1)=vdeg(i1)-xn(i1)-xg(i1,i1)
end do
uset(0)=0
xset(1)=1
jj=1
do i1=2,n
if(vdeg(i1-1).ne.vdeg(i1))then
uset(jj)=i1-1
jj=jj+1
else if(xn(i1-1).ne.xn(i1))then
uset(jj)=i1-1
jj=jj+1
else
ii=xg(i1-1,i1-1)-xg(i1,i1)
if(ii.gt.0)then
uset(jj)=i1-1
jj=jj+1
else if(ii.lt.0)then
go to 32
end if
end if
xset(i1)=jj
end do
uset(jj)=n
lps(lin)=loop-dsum+1
10 continue
ii=ds(lin,lin)
bond=min(str(lin),lps(lin))
do i1=n,lin+1,-1
jj=min(ii,bond,ds(lin,i1))
xg(lin,i1)=jj
ii=ii-jj
end do
if(ii.gt.0)then
go to 15
end if
go to 28
05 continue
lin=limax
go to 17
15 continue
if(lin.eq.limin)then
go to 32
end if
lin=lin-1
17 continue
if((lin.lt.limin).or.&
(lin.gt.n))then
mlin(1:srec)='g21_5'
call mput(1,1,0,0,0)
end if
do col=n,lin+1,-1
aux=xg(lin,col)-1
if(aux.ge.0)then
go to 23
end if
end do
go to 15
23 continue
bond=min(str(lin),lps(lin))
do i1=col-1,lin+1,-1
if(min(ds(lin,i1),bond).gt.xg(lin,i1))then
xg(lin,i1)=xg(lin,i1)+1
do i2=n,i1+1,-1
xg(lin,i2)=min(aux,bond,ds(lin,i2))
aux=aux-xg(lin,i2)
end do
go to 28
end if
aux=aux+xg(lin,i1)
end do
go to 15
38 continue
aux=-1
do i1=col,n
aux=aux+xg(lin,i1)
end do
go to 23
28 continue
if(lin.eq.n)then
go to 200
end if
msum=0
do i1=lin+1,n
ii=xg(lin,i1)-1
if(ii.gt.0)then
msum=msum+ii
end if
end do
if(msum.ge.lps(lin))then
go to 17
end if
if(lin.gt.limin)then
if(xset(lin).eq.xset(lin-1))then
do i1=limin,lin-2
ii=xg(i1,lin-1)-xg(i1,lin)
if(ii.gt.0)then
go to 35
else if(ii.lt.0)then
mlin(1:srec)='g21_7'
call mput(1,1,0,0,0)
end if
end do
do col=lin+1,n
ii=xg(lin-1,col)-xg(lin,col)
if(ii.lt.0)then
go to 38
else if(ii.gt.0)then
go to 35
end if
end do
end if
end if
35 continue
do col=lin+2,n
if(xset(col).eq.xset(col-1))then
do i1=limin,lin
ii=xg(i1,col-1)-xg(i1,col)
if(ii.lt.0)then
go to 38
else if(ii.gt.0)then
exit
end if
end do
end if
end do
do i1=lin+1,n
ds(lin+1,i1)=ds(lin,i1)-xg(lin,i1)
if(ds(lin+1,i1).lt.0)then
mlin(1:srec)='g21_8'
call mput(1,1,0,0,0)
end if
end do
lin=lin+1
lps(lin)=lps(lin-1)-msum
go to 10
200 continue
if(dflag(12).lt.0)then
do i1=n,rhop1,-1
if(xg(i1,i1).gt.0)then
go to 210
end if
end do
go to 05
end if
210 continue
if(dflag(13).lt.0)then
do i1=n-1,rhop1,-1
do i2=i1+1,n
if(xg(i1,i2)-1.gt.0)then
go to 220
end if
end do
end do
go to 05
end if
220 continue
j1=rhop1
j2=n-1
lin=n
do i1=j1,j2
aa(i1)=0
end do
aa(n)=1
i1=n-rho(1)
bb(i1)=lin
kk=i1-1
21 continue
if(kk.gt.0)then
ii=bb(i1)
i1=i1-1
do i2=j2,ii+1,-1
if(aa(i2).eq.0)then
if(xg(ii,i2).ne.0)then
bb(kk)=i2
kk=kk-1
aa(i2)=-aa(ii)
end if
end if
end do
if(aa(j2).ne.0)then
j2=j2-1
end if
do i2=j1,ii-1
if(aa(i2).eq.0)then
if(xg(i2,ii).ne.0)then
bb(kk)=i2
kk=kk-1
aa(i2)=-aa(ii)
end if
end if
end do
if(aa(j1).ne.0)then
j1=j1+1
end if
if(i1.eq.kk)then
do i2=j2,j1,-1
if(aa(i2).eq.0)then
aa(i2)=1
bb(kk)=i2
kk=kk-1
lin=i2
j2=lin-1
go to 21
end if
end do
mlin(1:srec)='g21_9'
call mput(1,1,0,0,0)
end if
go to 21
end if
if(lin.ne.n)then
go to 17
end if
if(dflag(11).ne.0)then
do i1=rhop1,n
if(xg(i1,i1).ne.0)then
if(dflag(11).gt.0)then
go to 05
else
go to 230
end if
end if
do i2=i1+1,n
if(xg(i1,i2).ne.0)then
if(aa(i1)+aa(i2).ne.0)then
if(dflag(11).gt.0)then
go to 05
else
go to 230
end if
end if
end if
end do
end do
if(dflag(11).lt.0)then
go to 05
end if
end if
230 continue
if(dflag(14).lt.0)then
do i1=n,rhop1,-1
if(xg(i1,i1)-3.gt.0)then
go to 240
end if
do i2=i1+1,n
if(xg(i1,i2)-1.gt.0)then
go to 240
end if
end do
end do
go to 05
end if
240 continue
if(dflag(15).lt.0)then
do i1=n,rhop1,-1
do i2=i1,n
if(xg(i1,i2)-1.gt.0)then
go to 250
end if
end do
end do
go to 05
end if
250 continue
do i1=1,n
do i2=1,i1-1
j1=xg(i2,i1)
gam(i1,i2)=j1
gam(i2,i1)=j1
end do
gam(i1,i1)=xg(i1,i1)
end do
if(dflag(1).ne.0)then
call umpi(1,ii)
if(ii.ne.dflag(1))then
go to 05
end if
end if
if(dflag(3).ne.0)then
call umpi(2,ii)
if(ii.ne.dflag(3))then
go to 05
end if
end if
if(dflag(4).ne.0)then
call umpi(4,ii)
if(ii.ne.dflag(4))then
go to 05
end if
end if
if(dflag(5).ne.0)then
call umvi(3,ii)
if(ii.ne.dflag(5))then
go to 05
end if
end if
if(dflag(8).ne.0)then
if(rho(1).ne.1)then
call umvi(1,ii)
else
call umvi(3,ii)
end if
if(ii.ne.dflag(8))then
go to 05
end if
end if
if(dflag(10).ne.0)then
call umvi(2,ii)
if(ii.ne.dflag(10))then
go to 05
end if
end if
ntadp=0
if(mflag(6).ne.0)then
call umpi(3,ii)
if(ii.ne.1)then
mlin(1:srec)='g21_11'
call mput(1,1,0,0,0)
end if
end if
nsym=0
do i1=1,rho(1)-1
p1s(i1,i1+1:rho(1))=0
end do
do i1=1,n
xp(i1)=i1
end do
go to 93
77 continue
do i1=xset(n),1,-1
do i2=uset(i1)-1,uset(i1-1)+1,-1
if(xp(i2).lt.xp(i2+1))then
go to 102
end if
end do
end do
go to 63
102 continue
j1=uset(i1)
do i1=i2+2,j1
if(xp(i1).lt.xp(i2))then
go to 202
end if
end do
i1=j1+1
202 continue
i1=i1-1
ii=xp(i1)
xp(i1)=xp(i2)
xp(i2)=ii
i1=i2+1
i2=j1
204 continue
if(i1.lt.i2)then
ii=xp(i1)
xp(i1)=xp(i2)
xp(i2)=ii
i1=i1+1
i2=i2-1
go to 204
end if
do i1=j1+1,n
xp(i1)=i1
end do
93 continue
if(rho(1).gt.0)then
if(xp(rho(1)).ne.rho(1))then
go to 63
end if
end if
do i1=rhop1,n-1
do i2=i1,n
ii=gam(xp(i1),xp(i2))-gam(i1,i2)
if(ii.gt.0)then
go to 05
end if
if(ii.lt.0)then
j1=xset(i2)
j3=uset(j1)
do i3=i2+1,j3-1
do i4=i3+1,j3
if(xp(i3).lt.xp(i4))then
ii=xp(i3)
xp(i3)=xp(i4)
xp(i4)=ii
end if
end do
end do
do i3=j1+1,xset(n)
j2=j3+1
j3=uset(i3)
j4=j2+j3
do i4=j2,j3
xp(i4)=j4-i4
end do
end do
go to 77
end if
end do
end do
if(rho(1).eq.0)then
go to 114
end if
i1=1
110 continue
j1=vmap(i1,1)
if(xp(j1).eq.j1)then
i1=i1+xn(j1)
if(i1.le.rho(1))then
go to 110
end if
go to 114
else
p1s(i1,a1(xp(j1)))=1
go to 77
end if
114 continue
if(jflag(16).eq.0)then
if(psym(0).lt.0)then
psyms=0
psym(0)=stibs(1)
call aoib(1)
stib(stibs(1))=eoa
end if
jj=n-rho(1)
ii=nsym*jj-psyms
if(ii.gt.0)then
if(psym(0).eq.stibs(1)-1-psyms)then
call aoib(ii)
psyms=psyms+ii
stib(stibs(1))=eoa
else
mlin(1:srec)='g21_13'
call mput(1,1,0,0,0)
end if
end if
ii=nsym*jj
if(ii.le.psyms)then
if(nsym.gt.0)then
ii=psym(0)+(ii-n)
do i1=rhop1,n
stib(ii+i1)=xp(i1)
end do
end if
else
mlin(1:srec)='g21_14'
call mput(1,1,0,0,0)
end if
end if
nsym=nsym+1
go to 77
63 continue
ns1=0
do i1=2,rho(1)
j1=i1-1
if(vmap(j1,1).eq.vmap(i1,1))then
p1s(j1,i1)=1
end if
do i2=1,j1
if(p1s(i2,i1).eq.1)then
ns1=ns1+1
p1l(ns1)=i2
p1r(ns1)=i1
end if
end do
end do
if(cflag(1)+jflag(17).eq.0)then
go to 57
end if
do i1=1,rho(1)
tail(i1)=i1
head(i1)=vmap(i1,1)
end do
if(vdeg(n).gt.0)then
nemul(1:vdeg(n))=0
end if
ii=rhop1
do i1=rhop1,n
do i2=i1,n
jj=gam(i1,i2)
if(jj.eq.0)then
ege(i1,i2)=0
else
ege(i1,i2)=ii
if(i1.eq.i2)then
jj=jj/2
else
nemul(jj)=nemul(jj)+1
emul(jj,nemul(jj))=ii
end if
ii=ii+jj
do i3=ii-jj,ii-1
tail(i3)=i1
head(i3)=i2
end do
end if
end do
end do
if(jflag(17).eq.0)then
go to 57
end if
do i1=rhop1,nli
intree(i1)=0
end do
if(n.gt.0)then
bb(1:n)=0
end if
do i1=rhop1,n
aa(i1)=0
end do
kk=0
ntree=0
ii=vdeg(n)
51 continue
if(ii.le.0)then
go to 52
end if
jj=nemul(ii)
58 continue
if(jj.le.0)then
ii=ii-1
go to 51
end if
j1=tail(emul(ii,jj))
j2=head(emul(ii,jj))
if(aa(j1).eq.aa(j2))then
if(aa(j1).eq.0)then
ntree=ntree+1
aa(j1)=ntree
aa(j2)=ntree
else
jj=jj-1
go to 58
end if
else
if(aa(j1).eq.0)then
aa(j1)=aa(j2)
else if(aa(j2).eq.0)then
aa(j2)=aa(j1)
else
j3=aa(j2)
do i1=rhop1,n
if(aa(i1).eq.j3)then
aa(i1)=aa(j1)
end if
end do
end if
end if
intree(ege(j1,j2))=1
bb(j1)=bb(j1)+1
bb(j2)=bb(j2)+1
kk=kk+1
if(kk.lt.n-rhop1)then
jj=jj-1
go to 58
end if
52 continue
do i1=1,rho(1)
intree(i1)=1
end do
do i1=1,nli
do i2=1,rhop2
flow(i1,i2)=0
end do
end do
do i1=1,rho(1)
flow(i1,i1)=1
end do
ii=rho(1)
do i1=rhop1,nli
if(intree(i1).eq.0)then
ii=ii+1
flow(i1,ii)=1
end if
end do
ii=rhop1
73 continue
if(bb(ii).eq.1)then
go to 71
else if(ii.lt.n)then
ii=ii+1
go to 73
else
go to 83
end if
71 continue
do i1=rhop1,ii-1
if(xg(i1,ii).ne.0)then
j1=ege(i1,ii)
if(intree(j1).ne.0)then
go to 75
end if
end if
end do
do i1=ii+1,n
if(xg(ii,i1).ne.0)then
j1=ege(ii,i1)
if(intree(j1).ne.0)then
go to 75
end if
end if
end do
mlin(1:srec)='g21_16'
call mput(1,1,0,0,0)
75 continue
if(rho(1).gt.1)then
if(xn(ii).gt.0)then
do i1=1,rho(1)
if(vmap(i1,1).eq.ii)then
if(tail(j1).eq.ii)then
flow(j1,i1)=flow(j1,i1)+1
else
flow(j1,i1)=flow(j1,i1)-1
end if
end if
end do
end if
end if
do i1=rhop1,n
if(i1.ne.ii)then
if(i1.lt.ii)then
jj=ege(i1,ii)
else
jj=ege(ii,i1)
end if
do i2=jj,jj+gam(ii,i1)-1
if(intree(i2).eq.0)then
if((head(j1).eq.head(i2)).or.&
(tail(j1).eq.tail(i2)))then
do i3=1,rhop2
flow(j1,i3)=flow(j1,i3)-flow(i2,i3)
end do
else
do i3=1,rhop2
flow(j1,i3)=flow(j1,i3)+flow(i2,i3)
end do
end if
end if
end do
end if
end do
intree(j1)=0
bb(tail(j1))=bb(tail(j1))-1
bb(head(j1))=bb(head(j1))-1
ii=rhop1
go to 73
83 continue
do i1=rhop1,nli
ii=0
jj=0
do i2=1,rho(1)
kk=flow(i1,i2)
if(kk.gt.0)then
ii=ii+1
else if(kk.lt.0)then
jj=jj+1
end if
end do
if((ii.gt.0).and.&
(jj.gt.0))then
mlin(1:srec)='g21_17'
call mput(1,1,0,0,0)
end if
if(2*(ii+jj).gt.rho(1))then
if(jj.eq.0)then
do i2=1,rho(1)
flow(i1,i2)=flow(i1,i2)-1
end do
else
do i2=1,rho(1)
flow(i1,i2)=flow(i1,i2)+1
end do
end if
end if
end do
do i1=1,nli
do i2=1,rhop2
if(abs(flow(i1,i2)).gt.1)then
mlin(1:srec)='g21_18'
call mput(1,1,0,0,0)
end if
end do
end do
net(:)=0
d02: do i1=rhop1,nli
do i2=rhop1,rhop2
if(flow(i1,i2).ne.0)then
if(head(i1).ne.tail(i1))then
kk=-1
else
kk=-2
end if
flow(i1,0)=kk
net(kk)=net(kk)+1
cycle d02
end if
end do
do i2=1,rho(1)
if(flow(i1,i2).ne.0)then
if(dflag(2).gt.0)then
go to 05
end if
flow(i1,0)=1
net(1)=net(1)+1
cycle d02
end if
end do
net(2)=net(2)+1
flow(i1,0)=2
end do d02
net(3)=net(1)+net(2)
net(-3)=net(-1)+net(-2)
net(0)=net(-3)+net(3)
ii=-2*net(-2)
do i1=rhop1,n
ii=ii+gam(i1,i1)
end do
if(ii.ne.0)then
mlin(1:srec)='g21_123'
call mput(1,1,0,0,0)
end if
if(dflag(2).lt.0)then
if(net(1).eq.0)then
go to 05
end if
end if
if(zbri(net(3)).le.0)then
go to 05
else if(zcho(net(-3)).le.0)then
go to 05
else if(rbri(net(1)).le.0)then
go to 05
else if(sbri(net(2)).le.0)then
go to 05
end if
if(dflag(7).ne.0)then
call gsig(ii,head,tail)
if(dflag(7).ne.ii)then
go to 05
end if
end if
do i1=1,rho(1)
ege(i1,vmap(i1,1))=i1
end do
do i1=rhop1,n
ii=0
do i2=1,rho(1)
jj=0
do i3=1,n
if(i3.ne.i1)then
j2=gam(i1,i3)
if(j2.gt.0)then
if(i1.lt.i3)then
j1=ege(i1,i3)
else
j1=ege(i3,i1)
end if
j3=head(j1)-i1
do i4=j1,j1+j2-1
if(j3.eq.0)then
jj=jj+flow(i4,i2)
else
jj=jj-flow(i4,i2)
end if
end do
end if
end if
end do
if(jj.ne.ii)then
if(i2.eq.1)then
ii=jj
else
mlin(1:srec)='g21_19'
call mput(1,1,0,0,0)
end if
end if
end do
end do
do i1=rhop1,n
do i2=rhop1,rhop2
ii=0
do i3=1,n
if(i3.ne.i1)then
j2=gam(i1,i3)
if(j2.gt.0)then
if(i1.lt.i3)then
j1=ege(i1,i3)
else
j1=ege(i3,i1)
end if
j3=head(j1)-i1
do i4=j1,j1+j2-1
if(j3.eq.0)then
ii=ii+flow(i4,i2)
else
ii=ii-flow(i4,i2)
end if
end do
end if
end if
end do
if(ii.ne.0)then
mlin(1:srec)='g21_20'
call mput(1,1,0,0,0)
end if
end do
end do
57 continue
qco(qcntr%t)=1
go to 90
97 continue
mlin(1:srec)='g21_1'
call mput(1,1,0,0,0)
90 return
end subroutine g21
subroutine gsig(situ,head,tail)
use,non_intrinsic::qmix
use,non_intrinsic::wbuffer
implicit none
save
integer,intent(in)::head(1:maxli),tail(1:maxli)
integer,intent(out)::situ
integer::aa(1:maxli),bb(1:maxli)
integer::ii,jj,rr,ss,tt
integer::f1,i1,i2,i3,i4,j3,j4
situ=1
if(rho(1).eq.0)then
go to 80
end if
do i1=rhop1,nli
f1=flow(i1,0)
if(abs(f1).eq.1)then
do i2=1,i1-1
ii=1
if(i2.lt.rhop1)then
if(f1.eq.1)then
ii=0
end if
else if(f1.eq.flow(i2,0))then
ii=0
end if
if(ii.eq.0)then
do i3=-1,1,2
if(f1.lt.0)then
do i4=rhop1,rhop2
ii=flow(i1,i4)+i3*flow(i2,i4)
if(ii.ne.0)then
go to 10
end if
end do
end if
ii=0
jj=0
do i4=1,rho(1)
if(flow(i1,i4)+i3*flow(i2,i4).ne.0)then
if(jj.ne.0)then
go to 10
end if
ii=ii+1
else
if(ii.ne.0)then
go to 10
end if
jj=jj+1
end if
end do
go to 80
10 continue
end do
end if
end do
end if
end do
if(rho(1)-1.ne.0)then
if(net(2)-1.le.0)then
go to 90
end if
else
if(net(2).ne.0)then
go to 80
else
go to 90
end if
end if
rr=0
do i3=rhop1,n
if(xn(i3).eq.0)then
aa(i3)=0
else
aa(i3)=1
rr=rr+1
bb(rr)=i3
end if
end do
if(rr.le.0)then
mlin(1:srec)='gsig_1'
call mput(1,1,0,0,0)
end if
ss=1
30 continue
tt=bb(ss)
do i3=rhop1,n
if(aa(i3).eq.0)then
if(i3.ne.tt)then
if(i3.lt.tt)then
j3=i3
j4=tt
else
j3=tt
j4=i3
end if
if(gam(j3,j4).ne.0)then
if(flow(ege(j3,j4),0).ne.2)then
aa(i3)=1
rr=rr+1
bb(rr)=i3
end if
end if
end if
end if
end do
if(ss.lt.rr)then
ss=ss+1
go to 30
end if
do i1=rhop1,nli
if(flow(i1,0).eq.2)then
if(aa(head(i1)).eq.0)then
if(aa(tail(i1)).eq.0)then
go to 80
end if
end if
end if
end do
go to 90
80 continue
situ=-1
90 return
end subroutine gsig
subroutine g43
use,non_intrinsic::cbuffer
use,non_intrinsic::qerr
use,non_intrinsic::qintr
use,non_intrinsic::qmix
use,non_intrinsic::qmodel
use,non_intrinsic::qproc
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer,external::qbprod,stoz
integer::i,ii,ij,ik,i1,i2,i3,i4,i5,j,jj,jk,j1,j2,j3,j4,k,kk
integer::a1,a2,aux,b1,b2,igo,jgo
integer::aoff,atyp,narg,nf,nf1,styp,vi,vind,vj,vk,vv
integer::xli(qb2)
if(qco(qcntr%d).eq.0)then
go to 58
else
qco(qcntr%d)=0
end if
27 continue
call g10
if(qco(qcntr%e).eq.0)then
qerrty(0)=qerrt%qend
go to 90
end if
do i1=1,rho(1)
j1=stib(leg(0)+ps1(i1))
pmap(i1,1)=j1
pmap(vmap(i1,1),lmap(i1,1))=stib(link(0)+j1)
end do
vind=rho(1)
57 continue
vind=vind+1
vv=vlis(vind)
vfo(vv)=stib(stib(dpntro(0)+vdeg(vv))+pmap(vv,1))
100 continue
do i1=1,rdeg(vv)
ii=stib(vfo(vv)+i1)-pmap(vv,i1)
if(ii.lt.0)then
vfo(vv)=vfo(vv)+vdeg(vv)+1
go to 100
else if(ii.gt.0)then
go to 104
end if
end do
go to 163
104 continue
if(vind.ne.rhop1)then
vind=vind-1
vv=vlis(vind)
go to 58
end if
qco(qcntr%e)=0
go to 27
58 continue
vfo(vv)=vfo(vv)+vdeg(vv)+1
do i1=rdeg(vv),1,-1
if(stib(vfo(vv)+i1).ne.pmap(vv,i1))then
go to 104
end if
end do
163 continue
if(jflag(13).ne.0)then
if(stib(vsfi(0)+stib(vfo(vv))).ne.0)then
go to 58
end if
end if
do i1=rdeg(vv)+1,sdeg(vv),2
j1=vfo(vv)+i1
j2=stib(j1)
j3=stib(j1+1)
if(j2.gt.j3)then
go to 58
else if(j2.ne.stib(link(0)+j3))then
go to 58
else if(i1+1.ne.sdeg(vv))then
if(j2.gt.stib(j1+2))then
go to 58
end if
end if
end do
if(jflag(14).eq.0)then
do i1=rdeg(vv)+1,vdeg(vv)
pmap(vv,i1)=stib(vfo(vv)+i1)
end do
else
do i1=rdeg(vv)+1,vdeg(vv)
j1=stib(vfo(vv)+i1)
if(stib(psfi(0)+j1).ne.0)then
go to 58
end if
pmap(vv,i1)=j1
end do
end if
do i1=sdeg(vv)+1,vdeg(vv)-1
if(vmap(vv,i1).eq.vmap(vv,i1+1))then
if(pmap(vv,i1).gt.pmap(vv,i1+1))then
go to 58
end if
end if
end do
if(mflag(5).ne.0)then
do i1=rdeg(vv)+1,vdeg(vv)
if(stib(tpc(0)+pmap(vv,i1)).eq.phity%ext)then
go to 58
end if
end do
end if
if(mflag(6).ne.0)then
do i1=1,ntadp
if(xtail(i1).eq.vv)then
jj=xhead(i1)
else if(xhead(i1).eq.vv)then
jj=xtail(i1)
else
jj=0
end if
if(jj.ne.0)then
do i2=1,vdeg(vv)
if(jj.eq.vmap(vv,i2))then
if(stib(tpc(0)+pmap(vv,i2)).eq.phity%notdp)then
go to 58
end if
end if
end do
end if
end do
end if
do i1=sdeg(vv)+1,vdeg(vv)
pmap(vmap(vv,i1),lmap(vv,i1))=stib(link(0)+pmap(vv,i1))
end do
if(vind.lt.n)then
go to 57
end if
if(dflag(17).eq.0)then
go to 333
end if
if(nloop.eq.0)then
if(dflag(17).gt.0)then
go to 333
else if(dflag(17).lt.0)then
go to 58
end if
end if
if(n.gt.0)then
xli(1:n)=0
end if
do i1=1,n
if(xli(i1).eq.0)then
ii=i1
kk=1
820 continue
do i2=1,vdeg(ii)
if(i2.ne.xli(ii))then
jj=pmap(ii,i2)
if(stib(antiq(0)+jj).ne.0)then
k=ii
ii=vmap(k,i2)
xli(ii)=lmap(k,i2)
if(ii.gt.rho(1))then
if(ii.ne.i1)then
kk=-kk
go to 820
end if
if(kk.gt.0)then
if(dflag(17).gt.0)then
go to 58
else
go to 333
end if
end if
end if
exit
end if
end if
end do
end if
end do
if(dflag(17).lt.0)then
go to 58
end if
333 continue
if(jflag(16).ne.0)then
dsym=nsym
else
dsym=1
jk=psym(0)-rho(1)
do i1=2,nsym
if(mflag(1).eq.0)then
do i2=rhop1,n
j1=stib(jk+i2)
j2=rdeg(i2)+1
400 continue
if(j2.le.vdeg(i2))then
j3=vmap(i2,j2)
j4=1
410 continue
if(j4.le.vdeg(i2))then
if(vmap(j1,j4).ne.stib(jk+j3))then
j4=j4+1
go to 410
end if
end if
jj=gam(i2,j3)
do i3=1,jj
xli(i3)=pmap(j1,i3+j4-1)
end do
do i3=1,jj-1
do i4=i3+1,jj
if(xli(i3).gt.xli(i4))then
ii=xli(i3)
xli(i3)=xli(i4)
xli(i4)=ii
end if
end do
end do
do i3=1,jj
ii=xli(i3)-pmap(i2,j2)
if(ii.lt.0)then
go to 58
else if(ii.gt.0)then
go to 339
end if
j2=j2+1
end do
go to 400
end if
end do
end if
if(mflag(4).ne.0)then
do i2=rhop1,n
j1=stib(jk+i2)
if(i2.ne.j1)then
ii=stib(vfo(j1))-stib(vfo(i2))
if(ii.lt.0)then
go to 58
else if(ii.gt.0)then
go to 339
end if
end if
end do
end if
dsym=dsym+1
339 continue
jk=jk+(n-rho(1))
end do
end if
if(dflag(18).ne.0)then
jgo=0
vi=n
atyp=6
j1=stib(tftic(0)+atyp)
do i1=stib(tfta(0)+j1),stib(tftb(0)+j1)
i2=stib(tf2(0)+i1)
styp=stib(tftyp(0)+i2)
aoff=stib(tfo(0)+i2)
j3=stib(aoff+1)
jj=stib(vmkzp(0)+j3)
ii=0
do k=rhop1,n
ii=ii+stib(jj+stib(vfo(k)))
end do
j1=stib(tfa(0)+i2)
j2=stib(tfb(0)+i2)
igo=0
if(styp.gt.0)then
if((ii.lt.j1).or.&
(ii.gt.j2))then
igo=1
end if
else
if((ii.ge.j1).and.&
(ii.le.j2))then
igo=1
end if
end if
if(igo.ne.0)then
jgo=1
a1=stib(vmkmio(0)+j3)
b1=stib(vmkmao(0)+j3)
a2=stib(stib(tfills(0)+i2)+rhop1)
b2=stib(stib(tfiuls(0)+i2)+rhop1)
if(styp.gt.0)then
do i3=rhop1,n-1
i4=vlis(i3)
i5=stib(jj+stib(vfo(i4)))
i4=vdeg(i4)
a2=a2+(i5-stib(b1+i4))
b2=b2+(i5-stib(a1+i4))
if((a2.lt.0).or.&
(b2.gt.0))then
if(vi.gt.i3)then
vi=i3
vj=jj
vk=i5
end if
exit
end if
end do
else
do i3=rhop1,n-1
i4=vlis(i3)
i5=stib(jj+stib(vfo(i4)))
i4=vdeg(i4)
a2=a2+(i5-stib(a1+i4))
b2=b2+(i5-stib(b1+i4))
if((a2.ge.0).and.&
(b2.le.0))then
if(vi.gt.i3)then
vi=i3
vj=jj
vk=i5
end if
exit
end if
end do
end if
end if
end do
if(jgo.ne.0)then
if(vi.lt.n)then
vind=vi
vv=vlis(vind)
end if
go to 58
end if
end if
do i=1,ntf
narg=stib(tfnarg(0)+i)
styp=stib(tftyp(0)+i)
atyp=abs(styp)
if((narg.eq.0).and.&
(atyp.eq.0))then
cycle
else if((narg.le.0).or.&
(atyp.le.0))then
mlin(1:srec)='g43_2'
call mput(1,1,0,0,0)
else if(atyp.eq.6)then
cycle
else if(atyp.gt.10)then
cycle
end if
aoff=stib(tfo(0)+i)
if(atyp.eq.1)then
ii=0
do j=rhop1,n
do jj=rdeg(j)+1,sdeg(j),2
jk=pmap(j,jj)
do ij=1,narg
ik=stib(aoff+ij)
if(ik.eq.jk)then
ii=ii+1
else if(stib(link(0)+ik).eq.jk)then
ii=ii+1
end if
end do
end do
do jj=sdeg(j)+1,vdeg(j)
jk=pmap(j,jj)
do ij=1,narg
ik=stib(aoff+ij)
if(ik.eq.jk)then
ii=ii+1
else if(stib(link(0)+ik).eq.jk)then
ii=ii+1
end if
end do
end do
end do
else if(atyp.eq.5)then
ii=0
do j=rhop1,n
do jj=rdeg(j)+1,sdeg(j),2
jk=pmap(j,jj)
if(flow(amap(j,jj),0).lt.0)then
do ij=1,narg
ik=stib(aoff+ij)
if(ik.eq.jk)then
ii=ii+1
else if(stib(link(0)+ik).eq.jk)then
ii=ii+1
end if
end do
end if
end do
do jj=sdeg(j)+1,vdeg(j)
jk=pmap(j,jj)
if(flow(amap(j,jj),0).lt.0)then
do ij=1,narg
ik=stib(aoff+ij)
if(ik.eq.jk)then
ii=ii+1
else if(stib(link(0)+ik).eq.jk)then
ii=ii+1
end if
end do
end if
end do
end do
else if(atyp.lt.5)then
if(atyp.eq.2)then
j1=1
j2=2
else if(atyp.eq.3)then
j1=2
j2=2
else if(atyp.eq.4)then
j1=1
j2=1
else
mlin(1:srec)='g43_3'
call mput(1,1,0,0,0)
end if
ii=0
do j=rhop1,n
do jj=rdeg(j)+1,sdeg(j),2
ij=flow(amap(j,jj),0)
if((ij.eq.j1).or.&
(ij.eq.j2))then
jk=pmap(j,jj)
do ij=1,narg
ik=stib(aoff+ij)
if(ik.eq.jk)then
ii=ii+1
else if(stib(link(0)+ik).eq.jk)then
ii=ii+1
end if
end do
end if
end do
do jj=sdeg(j)+1,vdeg(j)
ij=flow(amap(j,jj),0)
if((ij.eq.j1).or.&
(ij.eq.j2))then
jk=pmap(j,jj)
do ij=1,narg
ik=stib(aoff+ij)
if(ik.eq.jk)then
ii=ii+1
else if(stib(link(0)+ik).eq.jk)then
ii=ii+1
end if
end do
end if
end do
end do
else if(atyp.eq.7)then
ii=0
jj=stib(aoff+1)
do j=rhop1,n
do k=rdeg(j)+1,sdeg(j),2
kk=pmap(j,k)
ik=stib(stib(pmkvfpp(0)+kk)+jj)
ij=stoz(stcb,ik,ik-1+stib(stib(pmkvflp(0)+kk)+jj))
ii=ii+ij
end do
do k=sdeg(j)+1,vdeg(j)
kk=pmap(j,k)
ik=stib(stib(pmkvfpp(0)+kk)+jj)
ij=stoz(stcb,ik,ik-1+stib(stib(pmkvflp(0)+kk)+jj))
ii=ii+ij
end do
end do
end if
j1=stib(tfa(0)+i)
j2=stib(tfb(0)+i)
if(styp.gt.0)then
if((ii.lt.j1).or.&
(ii.gt.j2))then
go to 58
end if
else if(styp.lt.0)then
if((ii.ge.j1).and.&
(ii.le.j2))then
go to 58
end if
end if
end do
call sdiag(2,0)
call sdiag(1,0)
call sdiag(3,0)
if(cflag(1).eq.0)then
go to 505
end if
if(nloop.eq.0)then
go to 231
end if
do i=rhop1,n
j=rdeg(i)+1
420 continue
if(j.le.vdeg(i))then
ii=vmap(i,j)
aux=gam(i,ii)
k=j+aux
if(i.ne.ii)then
430 continue
if(j.lt.k)then
kk=1
aux=pmap(i,j)
440 continue
if(j+kk.lt.k)then
if(aux.eq.pmap(i,j+kk))then
kk=kk+1
dsym=qbprod(dsym,kk)
go to 440
end if
end if
j=j+kk
go to 430
end if
else
do while(j.lt.k)
kk=1
aux=pmap(i,j)
460 continue
if(j+kk+kk.lt.k)then
if(aux.eq.pmap(i,j+kk+kk))then
kk=kk+1
dsym=qbprod(dsym,kk)
go to 460
end if
end if
if(aux.eq.stib(link(0)+aux))then
jj=2
do ii=1,kk
dsym=qbprod(dsym,jj)
end do
end if
j=j+kk+kk
end do
end if
go to 420
end if
end do
231 continue
do i=rhop1,n
d01: do j=1,vdeg(i)
if(vmap(i,j).gt.nleg)then
k=pmap(i,j)
if(k.le.stib(link(0)+k))then
if(k.eq.stib(link(0)+k))then
if(i.gt.vmap(i,j))then
cycle d01
else if((i.eq.vmap(i,j)).and.&
(modulo(j-rdeg(i),2).eq.0))then
cycle d01
end if
end if
k=amap(i,j)-nleg
ex(k)=i
ey(k)=j
end if
end if
end do d01
end do
do i=rhop1,n
if(vdeg(i).gt.0)then
xli(1:vdeg(i))=0
end if
do j=1,vdeg(i)
k=stib(stib(vparto(0)+stib(vfo(i)))+j)
do i3=1,vdeg(i)
if(xli(i3).eq.0)then
if(pmap(i,i3).eq.k)then
xli(i3)=1
ovm(i,j)=i3
iovm(i,i3)=j
go to 14
end if
end if
end do
mlin(1:srec)='g43_4'
call mput(1,1,0,0,0)
14 continue
end do
end do
dis=1
if(mflag(2).gt.0)then
go to 505
end if
nf=0
nf1=0
do i=rhop1,n
do j=1,vdeg(i)
k=ovm(i,j)
ii=pmap(i,k)
if(stib(antiq(0)+ii).ne.0)then
nf=nf+1
ij=vmap(i,k)
if(ij.le.nleg)then
ij=ps1(ij)
if(ij.le.inco)then
jj=1-2*ij
else
jj=2*(inco-ij)
end if
else if(ii.lt.stib(link(0)+ii))then
jj=2*(amap(i,k)-nleg)-1
else if(ii.gt.stib(link(0)+ii))then
jj=2*(amap(i,k)-nleg)
else if(i.lt.ij)then
jj=2*(amap(i,k)-nleg)-1
else if(i.gt.ij)then
jj=2*(amap(i,k)-nleg)
else if(modulo(k-rdeg(i),2).ne.0)then
jj=2*(amap(i,k)-nleg)-1
else
jj=2*(amap(i,k)-nleg)
end if
if(ij.lt.0)then
nf1=nf1+1
end if
xli(nf)=jj
end if
end do
end do
266 continue
ii=0
do i1=1,nf
if(xli(i1).gt.ii)then
ii=xli(i1)
end if
end do
if(ii.gt.0)then
j1=0
j2=0
j3=nf
293 continue
if(xli(j3).gt.ii-2)then
j2=j1
j1=xli(j3)
if(j3.ne.nf)then
xli(j3)=xli(nf)
dis=-dis
end if
nf=nf-1
end if
if((j3.gt.1).and.&
(j2.eq.0))then
j3=j3-1
go to 293
end if
if(j1.gt.j2)then
dis=-dis
end if
go to 266
end if
do i1=1,inco
do i2=nf,1,-1
if(xli(i2).eq.1-2*i1)then
if(i2.ne.nf)then
xli(i2)=xli(nf)
dis=-dis
end if
nf=nf-1
go to 292
end if
end do
292 continue
end do
do i1=rho(1),inco+1,-1
do i2=nf,1,-1
if(xli(i2).eq.2*(inco-i1))then
if(i2.ne.nf)then
xli(i2)=xli(nf)
dis=-dis
end if
nf=nf-1
go to 302
end if
end do
302 continue
end do
if(nf.ne.0)then
mlin(1:srec)='g43_5'
call mput(1,1,0,0,0)
end if
505 continue
qco(qcntr%d)=1
if(cflag(1).gt.0)then
call compac
end if
90 continue
return
end subroutine g43
subroutine init0
use,non_intrinsic::cbuffer
use,non_intrinsic::qerr
use,non_intrinsic::qintr
use,non_intrinsic::zbuffer
implicit none
save
integer,external::wztos
character(kind=ascii,len=1)::char0
integer::i1,ii,jj,kk,ios
if(selected_char_kind('ascii').eq.-1)then
call uput(2)
end if
qerrty(0)=qerrt%qok
i1=0
diff0=max(128/storage_size(i1),1)
sibff(stibdst0)=sibuff
do i1=stibdst0,1,-1
sibff(i1-1)=sibff(i1)/2
if(sibff(i1).le.max(sibff(i1-1),diff0))then
call uput(9)
end if
end do
ios=1
allocate(xstib(sibff(0)+1),stat=ios)
if(ios.ne.0)then
call uput(0)
end if
stibdst(1)=0
stibas(1)=sibff(0)-diff0
stib=>xstib
stibs(1)=0
nstjb=0
dcff0=max(128/storage_size(char0),1)
scbff(stcbdst0)=scbuff
do i1=stcbdst0,1,-1
scbff(i1-1)=scbff(i1)/2
if(scbff(i1).le.max(scbff(i1-1),dcff0))then
call uput(11)
end if
end do
ios=1
allocate(character(scbff(0)+1)::xstcb,stat=ios)
if(ios.ne.0)then
call uput(0)
end if
stcbdst(1)=0
stcbas(1)=scbff(0)-dcff0
stcb=>xstcb
stcbs(1)=0
wsint=15
ii=huge(wsint)
jj=ii/10
kk=1
spten=0
do while(kk.lt.jj)
spten=spten+1
kk=10*kk
end do
ptenp=stibs(1)
call aoib(spten+1)
stib(stibs(1))=eoa
kk=1
do i1=1,spten
kk=10*kk
stib(i1)=kk
end do
ii=(ii/2)+1
jj=0
kk=1
iref=1
do i1=1,4*spten
jj=1-jj
if(jj.eq.0)then
iref=2*iref
end if
kk=2*kk
if(kk.ge.ii)then
iref=iref-1
exit
end if
end do
if(jj.eq.0)then
iref=iref+(iref*12)/29
end if
irefh=iref*iref
if((iref.lt.32767).or.&
(irefh.lt.iref))then
call uput(3)
end if
wiref=wztos(iref)
wirefh=wztos(irefh)
anull=0
alf=10
acr=13
aeq=iachar('=')
aminus=iachar('-')
anine=iachar('9')
aplus=iachar('+')
aast=iachar('*')
azero=iachar('0')
ii=max(aplus,aminus,azero,anine,aeq,aast)
jj=min(aplus,aminus,azero,anine,aeq,aast)
aspc=iachar(' ')
adq=iachar('"')
asq=iachar("'")
ausc=iachar('_')
ii=max(ii,aspc,adq,asq,ausc)
jj=min(jj,aspc,adq,asq,ausc)
acomma=iachar(',')
acol=iachar(':')
ascol=iachar(';')
adot=iachar('.')
avbar=iachar('|')
acirc=iachar('^')
ii=max(ii,acol,acomma,ascol,adot,avbar,acirc)
jj=min(jj,acol,acomma,ascol,adot,avbar,acirc)
alsbr=iachar('[')
arsbr=iachar(']')
alcbr=iachar('{')
arcbr=iachar('}')
alpar=iachar('(')
arpar=iachar(')')
ii=max(ii,alsbr,arsbr,alcbr,arcbr,alpar,arpar)
jj=min(jj,alsbr,arsbr,alcbr,arcbr,alpar,arpar)
alt=iachar('<')
agt=iachar('>')
aslash=iachar('/')
ii=max(ii,alt,agt,aslash)
jj=min(jj,alt,agt,aslash)
dcol=257
dscol=258
dslash=259
dsq=260
crs(1,1)=acol
crs(1,2)=acol
crs(1,3)=dcol
crs(1,4)=stty%p
crs(1,5)=0
crs(1,6)=1
crs(2,1)=ascol
crs(2,2)=ascol
crs(2,3)=dscol
crs(2,4)=stty%p
crs(2,5)=0
crs(2,6)=1
crs(3,1)=aslash
crs(3,2)=aslash
crs(3,3)=dslash
crs(3,4)=stty%p
crs(3,5)=1
crs(3,6)=0
crs(4,1)=asq
crs(4,2)=asq
crs(4,3)=dsq
crs(4,4)=stty%p
crs(4,5)=1
crs(4,6)=1
abo(1)=aspc
abo(2)=126
abo(3)=65
abo(4)=90
abo(5)=97
abo(6)=122
ii=max(ii,abo(3),abo(4),abo(5),abo(6))
jj=min(jj,abo(3),abo(4),abo(5),abo(6))
if((jj.lt.abo(1)).or.&
(ii.gt.abo(2)))then
call uput(2)
end if
ii=0
if(anine-azero.ne.9)then
ii=1
else if(abo(4)-abo(3).ne.25)then
ii=1
else if(abo(6)-abo(5).ne.25)then
ii=1
else if((abo(4).ge.abo(5)).and.&
(abo(6).ge.abo(3)))then
ii=1
end if
if(ii.ne.0)then
call uput(2)
end if
abo(0)=abo(3)-abo(5)
acf(0:127)=[(-2,i1=0,127)]
acf(iachar('%'))=-1
acf(iachar('*'))=-1
acf(iachar('#'))=-1
acf(ausc)=0
acf(azero:anine)=1
acf(abo(3):abo(4))=2
acf(abo(5):abo(6))=3
acz(0:127)=-1
acz(azero:anine)=1
acz(aminus)=2
acz(aplus)=2
acz(aast)=3
acz(alpar)=6
acz(arpar)=6
acz(acomma)=7
acz(aspc)=0
acf(abo(5):abo(6))=5
stjbs=stj0
stjb(1)=eoa
stjb(2:3)=nap
stjb(stj0)=eoa
punct1(0)=stibs(1)+1
ii=129
call aoib(ii)
stib(stibs(1))=eoa
stib(punct1(0):punct1(0)+127)=0
stib(punct1(0)+alf)=1
stib(punct1(0)+aspc)=1
stib(punct1(0)+acomma)=1
stib(punct1(0)+acol)=1
stib(punct1(0)+ascol)=1
stib(punct1(0)+avbar)=1
stib(punct1(0)+aeq)=1
stib(punct1(0)+alsbr)=2
stib(punct1(0)+arsbr)=2
stib(punct1(0)+alpar)=3
stib(punct1(0)+arpar)=3
stib(punct1(0)+alcbr)=4
stib(punct1(0)+arcbr)=4
stib(punct1(0)+acirc)=5
stib(punct1(0)+aminus)=-stty%minus
stib(punct1(0)+aplus)=-stty%plus
stib(punct1(0)+aast)=-stty%ast
call initq
return
end subroutine init0
subroutine initq
use,non_intrinsic::cbuffer
use,non_intrinsic::ikeys
use,non_intrinsic::qfiles
use,non_intrinsic::qintr
use,non_intrinsic::qmix
use,non_intrinsic::qmodel
use,non_intrinsic::qsty
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
intrinsic::command_argument_count,get_command_argument
integer::naop,nisk,nopt0,nopt1,nopt2,nopt3,nopt4,nopt5,nopt7,nopt8
integer::nos,nxts,stibr,uc
integer::ii,ik,i1,jj,j1,kk
integer::apak1(1:1),apak2(1:2),apak3(1:3)
integer::weri(0:0),aopc(0:0),llim(1:3),ulim(1:4)
character(kind=ascii,len=240)::coarg
integer,external::stdcfn
llim(1:3)=[2047,8191,2047]
ulim(1:4)=[32767,134217727,131071,134217727]
if((scbuff.lt.llim(2)).or.&
(scbuff.gt.ulim(2)))then
call uput(11)
end if
if((swbuff.lt.llim(2)).or.&
(swbuff.gt.ulim(3)))then
call uput(12)
end if
if((sxbuff.lt.llim(1)).or.&
(sxbuff.gt.ulim(1)))then
call uput(13)
end if
if((sibuff.lt.llim(3)).or.&
(sibuff.gt.ulim(4)))then
call uput(9)
end if
if((sjbuff.lt.llim(3)).or.&
(sjbuff.gt.ulim(3)))then
call uput(10)
end if
if((srec.lt.81).or.&
(srec.gt.llim(1)))then
call uput(14)
end if
if((srecx.le.srec).or.&
(srecx.gt.sxbuff))then
call uput(17)
end if
if((sfrec.ge.min(srec,128)).or.&
(sfrec.le.48))then
call uput(15)
end if
if((ssrec.ge.min(srec,128)).or.&
(ssrec.le.48))then
call uput(16)
end if
ii=maxdeg
if(ii.lt.3)then
mlin(1:srec)="parameter 'maxdeg' not properly set"
call mput(1,1,0,0,0)
end if
ii=maxleg
if(ii.lt.3)then
mlin(1:srec)="parameter 'maxleg' not properly set"
call mput(1,1,0,0,0)
end if
ii=maxrho
if(ii.lt.3)then
mlin(1:srec)="parameter 'maxrho' not properly set"
call mput(1,1,0,0,0)
end if
ii=maxn
jj=2*max(maxleg,maxrho)-2
if(ii.lt.jj)then
mlin(1:srec)="parameter 'maxn' not properly set"
call mput(1,1,0,0,0)
end if
ii=maxli
jj=2*max(maxleg,maxrho)+maxrho-3
if(ii.lt.jj)then
mlin(1:srec)="parameter 'maxli' not properly set"
call mput(1,1,0,0,0)
end if
cflag(:)=0
dflag(:)=0
jflag(:)=0
mflag(:)=0
iogp(:)=0
qco(:)=0
j1=2*srec
do i1=1,j1
stxb(i1:i1)=achar(azero)
end do
j1=j1+1
stxb(j1:j1)=achar(ascol)
ii=0
call spak(stxb(1:j1),ii,0,0,0)
call spak('817182657013191422141716;',ii,1,0,0)
qvp=stib(stibs(1)-1)
qvl=stib(stibs(1))
call aoib(-2)
call spak('131386698283737978;',ii,1,0,0)
vap=stib(stibs(1)-1)
vaq=(vap-1)+stib(stibs(1))
call aoib(-2)
call spak('847780637073766963;',ii,1,0,0)
qtfpp=stib(stibs(1)-1)
qtfpq=(qtfpp-1)+stib(stibs(1))
call aoib(-2)
call spak('0010101000817182657013716978698265846968007073766'//&
'900776582750010101000;',ii,1,0,0)
qgfmp=stib(stibs(1)-1)
qgfmq=(qgfmp-1)+stib(stibs(1))
call aoib(-2)
jj=wsint+1
ndiagp=stcbs(1)
call aocb(jj)
stcb(stcbs(1):stcbs(1))=achar(alf)
call sdiag(3,-1)
hhp=stcbs(1)
call aocb(jj)
stcb(stcbs(1):stcbs(1))=achar(alf)
noffp=stcbs(1)
noffl=1
call aocb(jj)
ii=noffp+1
stcb(ii:ii)=achar(azero)
stcb(stcbs(1):stcbs(1))=achar(alf)
doffp=stcbs(1)
doffl=1
call aocb(jj)
ii=doffp+1
stcb(ii:ii)=achar(azero)
stcb(stcbs(1):stcbs(1))=achar(alf)
rho(:)=0
psym(0)=nap
nfiles=0
nsfiles=0
npfiles=0
dso=0
filp(:)=nap
filq(:)=nap
film(:)=0
filo(:)=0
filt(:)=0
filu(:)=-1
nxts=0
call spak('78797813808279806571658479828326000008;',nxts,0,0,0)
call spak('808279806571658479828326000008;',nxts,0,0,0)
call spak('4611;',nxts,0,0,0)
call spak('3511;',nxts,0,0,0)
call spak('4613;',nxts,0,0,0)
call spak('3513;',nxts,0,0,0)
call spak('866982847367698326000008;',nxts,0,0,0)
call spak('861368697182696983;',nxts,0,0,0)
call spak('036873657182657783;',nxts,0,0,0)
call spak('03718265807283;',nxts,0,0,0)
call spak('0000000000000000847984657600290000;',nxts,0,0,0)
call spak('00677978786967846968006873657182657783;',nxts,0,0,0)
call spak('0067797878696784696800718265807283;',nxts,0,0,0)
call spak('0900;',nxts,0,0,0)
call spak('14141414;',nxts,0,0,0)
call spak('0000001010;',nxts,0,0,0)
call spak('00000008870109;',nxts,0,0,0)
call spak('141414;',nxts,0,0,0)
call spak('12007673786900;',nxts,0,0,0)
call spak('1200767378698300;',nxts,0,0,0)
call spak('000000876582787378712600;',nxts,0,0,0)
call spak('00000069828279822600;',nxts,0,0,0)
call spak('677978848279761370737669;',nxts,0,0,0)
call spak('776983836571691370737669;',nxts,0,0,0)
call spak('77796869761370737669;',nxts,0,0,0)
call spak('767366826582891370737669;',nxts,0,0,0)
call spak('83848976691370737669;',nxts,0,0,0)
call spak('7985848085841370737669;',nxts,0,0,0)
call spak('62;',nxts,0,0,0)
call spak('13;',nxts,0,0,0)
call spak('000000037679798083;',nxts,0,0,0)
call spak('6210;',nxts,0,0,0)
call spak('000000000010000000;',nxts,0,0,0)
kk=2
call aoib(kk)
stib(stibs(1)-(kk-1):stibs(1))=eoa
ii=nxts+1
call trm(kk,ii,stibs(1))
xtstrl(0)=stibs(1)-ii
xtstrp(0)=xtstrl(0)-ii
if(cflag(11).eq.0)then
ii=command_argument_count()
jj=0
if(ii.eq.1)then
call get_command_argument(ii,coarg,jj)
kk=0
if(iachar(coarg(1:1)).eq.aminus)then
if((jj.eq.vaq-(vap-1)).and.&
(coarg(1:jj).eq.stcb(vap:vaq)))then
kk=1
else if((jj.eq.vaq-vap).and.&
(coarg(1:jj).eq.stcb(vap+1:vaq)))then
kk=1
end if
if(kk.ne.0)then
write(unit=*,fmt='(a)')stcb(qvp:(qvp-1)+qvl)
stop
end if
else if((jj.gt.0).and.&
(jj.lt.min(len(coarg),srec)))then
filp(1)=stcbs(1)+1
call aocb(jj+1)
filq(1)=stcbs(1)-1
stcb(filp(1):filq(1))=coarg(1:jj)
stcb(stcbs(1):stcbs(1))=achar(anull)
nfiles=1
ii=1-stdcfn(stcb,filp(1),filq(1))
end if
end if
if(ii.gt.0)then
mlin(1:srec)="wrong command-line argument(s)"
call mput(1,1,0,0,0)
end if
end if
if(nfiles.eq.0)then
call spak('817182657014686584;',ii,1,0,0)
filp(1)=stib(stibs(1)-1)
filq(1)=filp(1)-1+stib(stibs(1))
call aoib(-2)
nfiles=1
end if
filt(1)=qfty%ctyp
uc=1
nisk=0
stibr=stibs(1)
apak3=[dekc%conf,dekc%msg,0]
call spok('677978707371;',apak3,3,nisk,uc,0)
apak3=[dekc%msg,dekc%outp,0]
call spok('7769838365716983;',apak3,3,nisk,uc,0)
apak3=[dekc%outp,dekc%sty,0]
call spok('798584808584;',apak3,3,nisk,uc,0)
apak3=[dekc%sty,dekc%outp,0]
call spok('8384897669;',apak3,3,nisk,uc,0)
apak3=[dekc%lib,dekc%model,0]
call spok('767366;',apak3,3,nisk,uc,0)
apak3=[dekc%model,dekc%in,1]
call spok('7779686976;',apak3,3,nisk,uc,0)
apak3=[dekc%in,dekc%out,1]
call spok('7378;',apak3,3,nisk,uc,0)
apak3=[dekc%out,dekc%loop,1]
call spok('798584;',apak3,3,nisk,uc,0)
apak3=[dekc%loop,dekc%loopk,1]
call spok('7679798083;',apak3,3,nisk,uc,0)
apak3=[dekc%loopk,dekc%opt,0]
call spok('76797980637779776978848577;',apak3,3,nisk,uc,0)
apak3=[dekc%opt,dekc%part,1]
call spok('79808473797883;',apak3,3,nisk,uc,0)
apak3=[dekc%part,dekc%off,0]
call spok('806582847384737978;',apak3,3,nisk,uc,0)
apak3=[dekc%off,dekc%part,0]
call spok('737868698863797070836984;',apak3,3,nisk,uc,0)
apak3=[dekc%tru,dekc%fals,0]
call spok('84828569;',apak3,3,nisk,uc,0)
apak3=[dekc%fals,dekc%tru,0]
call spok('7065768369;',apak3,3,nisk,uc,0)
kk=5
call aoib(kk)
stib(stibs(1)-(kk-1):stibs(1))=eoa
ii=nisk+1
call trm(kk,ii,stibs(1))
pisk(0)=stibr
wisk(0)=pisk(0)+ii
aisk(0)=wisk(0)+ii
cisk(0)=aisk(0)+ii
jj=stibs(1)
call squiiz(aisk(0),jj,ii,kk-2,ik)
fisk(0)=cisk(0)+ik
oisk(0)=fisk(0)+ik
uc=1
nopt0=0
stibr=stibs(1)
call spak('787976738384,1,-1;',nopt0,0,uc,1)
call spak('73787079,3,1;',nopt0,0,uc,1)
call spak('86698266798369,3,2;',nopt0,0,uc,1)
call spak('787973787079,3,-1;',nopt0,0,uc,1)
call spak('7670,4,-1;',nopt0,0,uc,1)
call spak('7879667665787583,5,1;',nopt0,0,uc,1)
call spak('81686976,6,1;',nopt0,0,uc,nos)
kk=4
call aoib(kk)
stib(stibs(1)-(kk-1):stibs(1))=eoa
ii=nopt0+1
call trm(kk,ii,stibs(1))
popt0(0)=stibr
wopt0(0)=popt0(0)+ii
aopt0(0)=wopt0(0)+ii
copt0(0)=aopt0(0)+ii
jj=stibs(1)
call squiiz(aopt0(0),jj,ii,kk-2,ik)
xopt0(0)=copt0(0)+ik
uc=1
nopt1=0
stibr=stibs(1)
call spak('787966827368716983,1,1;',nopt1,0,uc,1)
call spak('7978698073,1,1;',nopt1,0,uc,1)
call spak('66827368716983,1,-1;',nopt1,0,uc,1)
call spak('7978698082,1,-1;',nopt1,0,uc,1)
call spak('78798266827368716983,2,1;',nopt1,0,uc,1)
call spak('8266827368716983,2,-1;',nopt1,0,uc,1)
call spak('78798366827368716983,3,1;',nopt1,0,uc,1)
call spak('78798465688079766983,3,1;',nopt1,0,uc,1)
call spak('8366827368716983,3,-1;',nopt1,0,uc,1)
call spak('8465688079766983,3,-1;',nopt1,0,uc,1)
call spak('79788372697676,4,1;',nopt1,0,uc,1)
call spak('7970708372697676,4,-1;',nopt1,0,uc,1)
call spak('7978837269767688,5,1;',nopt1,0,uc,1)
call spak('797070837269767688,5,-1;',nopt1,0,uc,1)
call spak('7879837371776583,7,1;',nopt1,0,uc,1)
call spak('837371776583,7,-1;',nopt1,0,uc,1)
call spak('7879837865737683,8,1;',nopt1,0,uc,1)
call spak('837865737683,8,-1;',nopt1,0,uc,1)
call spak('6789677673,9,1;',nopt1,0,uc,1)
call spak('6789677682,9,-1;',nopt1,0,uc,1)
call spak('7978698673,10,1;',nopt1,0,uc,1)
call spak('7978698682,10,-1;',nopt1,0,uc,1)
call spak('667380658284,11,1;',nopt1,0,uc,1)
call spak('787978667380658284,11,-1;',nopt1,0,uc,1)
call spak('7879836976707679798083,12,1;',nopt1,0,uc,1)
call spak('836976707679798083,12,-1;',nopt1,0,uc,1)
call spak('787968737679798083,13,1;',nopt1,0,uc,1)
call spak('68737679798083,13,-1;',nopt1,0,uc,1)
call spak('78798065826576766976,14,1;',nopt1,0,uc,1)
call spak('8065826576766976,14,-1;',nopt1,0,uc,1)
call spak('837377807669,15,1;',nopt1,0,uc,1)
call spak('787984837377807669,15,-1;',nopt1,0,uc,1)
call spak('8479807976,16,1;',nopt1,0,uc,1)
call spak('707679798083,17,1;',nopt1,0,uc,1)
call spak('787984707679798083,17,-1;',nopt1,0,uc,1)
call spak('7879798084,25,1;',nopt1,0,uc,1)
kk=4
call aoib(kk)
stib(stibs(1)-(kk-1):stibs(1))=eoa
ii=nopt1+1
call trm(kk,ii,stibs(1))
popt1(0)=stibr
wopt1(0)=popt1(0)+ii
aopt1(0)=wopt1(0)+ii
copt1(0)=aopt1(0)+ii
jj=stibs(1)
call squiiz(aopt1(0),jj,ii,kk-2,ik)
xopt1(0)=copt1(0)+ik
uc=1
nopt2=0
stibr=stibs(1)
call spak('7380827980,1;',nopt2,0,uc,0)
call spak('668273687169,2;',nopt2,0,uc,0)
call spak('83668273687169,3;',nopt2,0,uc,0)
call spak('82668273687169,4;',nopt2,0,uc,0)
call spak('6772798268,5;',nopt2,0,uc,0)
call spak('86838577,6;',nopt2,0,uc,0)
call spak('80838577,7;',nopt2,0,uc,0)
call spak('6976737875,11;',nopt2,0,uc,0)
call spak('8076737875,12;',nopt2,0,uc,0)
kk=3
call aoib(kk)
stib(stibs(1)-(kk-1):stibs(1))=eoa
ii=nopt2+1
call trm(kk,ii,stibs(1))
popt2(0)=stibr
wopt2(0)=popt2(0)+ii
aopt2(0)=wopt2(0)+ii
copt2(0)=aopt2(0)+ii
jj=stibs(1)
call squiiz(aopt2(0),jj,ii,kk-2,ik)
ii=stibs(1)-jj-1
mcopt2=0
ncopt2=0
do i1=1,ii
j1=stib(copt2(0)+i1)
if((j1.eq.0).or.&
(j1.lt.mcopt2))then
mlin(1:srec)='initq_1'
call mput(1,1,0,0,0)
else
if(j1.gt.mcopt2)then
ncopt2=ncopt2+1
end if
mcopt2=j1
end if
end do
uc=1
nopt3=0
stibr=stibs(1)
call spak('69886776,1;',nopt3,0,uc,0)
call spak('73786776,0;',nopt3,0,uc,0)
kk=3
call aoib(kk)
stib(stibs(1)-(kk-1):stibs(1))=eoa
ii=nopt3+1
call trm(kk,ii,stibs(1))
popt3(0)=stibr
wopt3(0)=popt3(0)+ii
aopt3(0)=wopt3(0)+ii
copt3(0)=aopt3(0)+ii
jj=stibs(1)
call squiiz(aopt3(0),jj,ii,kk-2,ik)
uc=1
nos=0
nopt4=0
stibr=stibs(1)
call spak('8479,1;',nopt4,0,uc,nos)
call spak('84728285,1;',nopt4,0,uc,nos)
call spak('84728279857172,1;',nopt4,0,uc,nos)
kk=3
call aoib(kk)
stib(stibs(1)-(kk-1):stibs(1))=eoa
ii=nopt4+1
call trm(kk,ii,stibs(1))
popt4(0)=stibr
wopt4(0)=popt4(0)+ii
aopt4(0)=wopt4(0)+ii
copt4(0)=aopt4(0)+ii
jj=stibs(1)
call squiiz(aopt4(0),jj,ii,kk-2,ik)
pot(0)=1
do i1=1,maxpot
pot(i1)=2*pot(i1-1)
end do
call spak('6885657613;',ii,1,0,0)
dprefp=stib(stibs(1)-1)
dprefl=stib(stibs(1))
call aoib(-2)
uc=0
nske=0
stibr=stibs(1)
call spak('8082797679718569,1,0,0,0,0;',nske,0,uc,0)
call spak('68736571826577,2,0,0,0,0;',nske,0,uc,0)
call spak('6980737679718569,3,0,0,0,0;',nske,0,uc,0)
call spak('69887384,4,0,0,0,0;',nske,0,uc,0)
call spak('677977776578686376797980,11,1,5,0,0;',nske,0,uc,0)
call spak('6779777765786863767378696376797980,12,1,5,1,0;',nske&
,0,uc,0)
call spak('73786376797980,13,1,2,0,0;',nske,0,uc,0)
call spak('736376797980,13,1,2,0,0;',nske,0,uc,0)
call spak('7985846376797980,14,1,2,0,0;',nske,0,uc,0)
call spak('796376797980,14,1,2,0,0;',nske,0,uc,0)
call spak('808279806571658479826376797980,15,1,2,0,0;',nske,0,&
uc,0)
call spak('806376797980,15,1,2,0,0;',nske,0,uc,0)
call spak('8669828469886376797980,16,1,2,0,0;',nske,0,uc,0)
call spak('866376797980,16,1,2,0,0;',nske,0,uc,0)
call spak('8265896376797980,17,1,2,8,0;',nske,0,uc,0)
call spak('826376797980,17,1,2,8,0;',nske,0,uc,0)
call spak('697868,19,1,7,31,0;',nske,0,uc,0)
call spak('66656775,22,2,7,0,0;',nske,0,uc,0)
call spak('78698776737869,29,5,7,0,0;',nske,0,uc,0)
call spak('7073697668,31,4,2,23,0;',nske,0,uc,0)
call spak('7779776978848577,32,4,2,23,0;',nske,0,uc,0)
call spak('68856576137073697668,33,4,2,23,0;',nske,0,uc,0)
call spak('68856576137779776978848577,34,4,2,23,0;',nske,0,uc,0)
call spak('70736976686383737178,35,4,2,23,0;',nske,0,uc,0)
call spak('70736976686384898069,40,4,2,23,0;',nske,0,uc,0)
call spak('80827980657165847982637378686988,41,4,2,20,0;',nske,&
0,uc,0)
call spak('80637378686988,41,4,2,20,0;',nske,0,uc,0)
call spak('7073697668637378686988,42,4,2,23,0;',nske,0,uc,0)
call spak('70637378686988,42,4,2,23,0;',nske,0,uc,0)
call spak('826589637378686988,43,4,2,23,0;',nske,0,uc,0)
call spak('82637378686988,43,4,2,23,0;',nske,0,uc,0)
call spak('866982846988637378686988,44,4,2,31,0;',nske,0,uc,0)
call spak('86637378686988,44,4,2,31,0;',nske,0,uc,0)
call spak('68856576137073697668637378686988,45,4,2,20,0;',nske,&
0,uc,0)
call spak('688565761370637378686988,45,4,2,20,0;',nske,0,uc,0)
call spak('6885657613826589637378686988,46,4,2,20,0;',nske,0,uc&
,0)
call spak('688565761382637378686988,46,4,2,20,0;',nske,0,uc,0)
call spak('6885657613866982846988637378686988,47,4,2,20,0;',&
nske,0,uc,0)
call spak('688565761386637378686988,47,4,2,20,0;',nske,0,uc,0)
call spak('86698284698863686971826969,48,4,2,31,0;',nske,0,uc,0)
call spak('8663686971826969,48,4,2,31,0;',nske,0,uc,0)
call spak('688565761386698284698863686971826969,49,4,2,20,0;',&
nske,0,uc,0)
call spak('68856576138663686971826969,49,4,2,20,0;',nske,0,uc,0)
call spak('766971637378686988,51,4,2,2,0;',nske,0,uc,0)
call spak('76637378686988,51,4,2,2,0;',nske,0,uc,0)
call spak('7378637378686988,52,4,2,1,0;',nske,0,uc,0)
call spak('73637378686988,52,4,2,1,0;',nske,0,uc,0)
call spak('798584637378686988,53,4,2,2,0;',nske,0,uc,0)
call spak('79637378686988,53,4,2,2,0;',nske,0,uc,0)
call spak('83737178,61,3,2,0,0;',nske,0,uc,0)
call spak('7773788583,62,3,2,0,0;',nske,0,uc,0)
call spak('78698763697673787583,63,3,2,0,0;',nske,0,uc,0)
call spak('786987638479807976797189,64,3,2,0,0;',nske,0,uc,0)
call spak('78698763806582847384737978,65,3,2,0,0;',nske,0,uc,0)
call spak('786987637679798083,66,3,2,0,0;',nske,0,uc,nos)
call spak('68736571826577637378686988,71,3,6,0,0;',nske,0,uc,0)
call spak('838977776984828963706567847982,72,3,2,0,0;',nske,0,&
uc,0)
call spak('838977776984828963788577666982,73,3,2,0,0;',nske,0,&
uc,0)
call spak('8082798065716584798283,74,3,2,0,0;',nske,0,uc,0)
call spak('76697183,75,3,2,0,0;',nske,0,uc,0)
call spak('7679798083,76,3,2,0,0;',nske,0,uc,0)
call spak('8669828473676983,77,3,2,0,0;',nske,0,uc,0)
call spak('76697183637378,78,3,2,0,0;',nske,0,uc,0)
call spak('7669718363798584,79,3,2,0,0;',nske,0,uc,0)
call spak('80827971826577,81,3,5,0,0;',nske,0,uc,0)
call spak('677977776578686368658465,82,4,5,3,0;',nske,0,uc,0)
call spak('708576766384737769,83,3,5,0,0;',nske,0,uc,0)
call spak('8265876384737769,85,3,5,0,0;',nske,0,uc,0)
call spak('81707376696377658275,86,3,1,0,0;',nske,0,uc,nos)
kk=7
if(stibs(1)-stibr.ne.kk*nske)then
mlin(1:srec)='initq_2'
call mput(1,1,0,0,0)
end if
call aoib(kk)
stib(stibs(1)-(kk-1):stibs(1))=eoa
ii=nske+1
call trm(kk,ii,stibs(1))
pske(0)=stibr
wske(0)=pske(0)+ii
aske(0)=wske(0)+ii
kc(0)=aske(0)+ii
jj=stibs(1)
call squiiz(aske(0),jj,ii,kk-2,ik)
kla(0)=kc(0)+ik
kse(0)=kla(0)+ik
klo(0)=kse(0)+ik
bbc(0)=klo(0)+ik
nopt5=0
uc=1
nos=1
stibr=stibs(1)
apak2=[dekm%smcc,0]
call spok('838566777968697683;',apak2,2,nopt5,uc,nos)
apak2=[dekm%skcc,mlty%mpf]
call spok('806383696784798283;',apak2,2,nopt5,uc,nos)
apak2=[dekm%skcc,mlty%mvf]
call spok('866383696784798283;',apak2,2,nopt5,uc,nos)
apak2=[dekm%mccc,mlty%mcf]
call spok('677978838465788483;',apak2,2,nopt5,uc,nos)
apak2=[dekm%mfcc,mlty%mff]
call spok('7063708578678473797883;',apak2,2,nopt5,uc,nos)
apak2=[dekm%mfcc,mlty%mpf]
call spok('8063708578678473797883;',apak2,2,nopt5,uc,nos)
apak2=[dekm%mfcc,mlty%mvf]
call spok('8663708578678473797883;',apak2,2,nopt5,uc,nos)
apak2=[dekm%smp,0]
call spok('8385667779686976;',apak2,2,nopt5,uc,0)
apak2=[dekm%ski,0]
call spok('7378677685686983;',apak2,2,nopt5,uc,nos)
apak2=[dekm%ski,1]
call spok('6988677685686983;',apak2,2,nopt5,uc,nos)
apak2=[dekm%smq,0]
call spok('697868;',apak2,2,nopt5,uc,0)
apak2=[dekm%skp,0]
call spok('836967847982;',apak2,2,nopt5,uc,0)
kk=4
call aoib(kk)
stib(stibs(1)-(kk-1):stibs(1))=eoa
ii=nopt5+1
call trm(kk,ii,stibs(1))
popt5(0)=stibr
wopt5(0)=popt5(0)+ii
aopt5(0)=wopt5(0)+ii
copt5(0)=aopt5(0)+ii
jj=stibs(1)
call squiiz(aopt5(0),jj,ii,kk-2,ik)
sopt5(0)=copt5(0)+ik
nopt7=0
uc=1
nos=0
stibr=stibs(1)
apak1=[stty%z]
call spok('73788469716982;',apak1,1,nopt7,uc,nos)
apak1=[stty%z]
call spok('90;',apak1,1,nopt7,uc,nos)
kk=3
call aoib(kk)
stib(stibs(1)-(kk-1):stibs(1))=eoa
ii=nopt7+1
call trm(kk,ii,stibs(1))
popt7(0)=stibr
wopt7(0)=popt7(0)+ii
aopt7(0)=wopt7(0)+ii
copt7(0)=aopt7(0)+ii
jj=stibs(1)
call squiiz(aopt7(0),jj,ii,kk-2,ik)
nopt8=0
nos=1
uc=1
stibr=stibs(1)
call spak('78798465688079766983,1;',nopt8,0,uc,nos)
call spak('6988846982786576,5;',nopt8,0,uc,nos)
call spak('7378846982786576,2;',nopt8,0,uc,nos)
kk=3
call aoib(kk)
stib(stibs(1)-(kk-1):stibs(1))=eoa
ii=nopt8+1
call trm(kk,ii,stibs(1))
popt8(0)=stibr
wopt8(0)=popt8(0)+ii
aopt8(0)=wopt8(0)+ii
copt8(0)=aopt8(0)+ii
jj=stibs(1)
call squiiz(aopt8(0),jj,ii,kk-2,ik)
nphi=0
nprop=0
npprop=0
nvert=0
nblok=0
nblokp=0
nudk=0
ngmk=0
npmk=0
nvmk=0
aaf(0)=6
aaf(1)=6
aaf(2)=6
aaf(3)=5
aaf(4)=5
uc=0
nwer=0
call spak('777968697600676578006669008380767384007378847900687'&
//'38374797378840083856613777968697683,1;',nwer,0,0,0)
call spak('658400766965838400797869007073697668007383007879840'&
//'073780065788900737884698265678473797800866982846988,2;',&
nwer,0,0,0)
call spak('787900656784856576008082798065716584798283120065767'&
//'600707369766883008465717169680007698884698278657607,3;',&
nwer,0,0,0)
call spak('777968697613707376690072658300688580767367658469008'&
//'669828473676983,4;',nwer,0,0,0)
call spak('677978838465788400708578678473797800686984696784696'&
//'82600,5;',nwer,0,0,0)
call spak('787900866982846988007765890076737875138580008773847'&
//'200737867797773787100707369766800,6;',nwer,0,0,0)
call spak('787900866982846988007765890076737875138580008773847'&
//'200798584717973787100707369766800,7;',nwer,0,0,0)
call spak('698884698278657600707369766883006765787879840066690'&
//'0677978786967846968,8;',nwer,0,0,0)
call spak('796868007885776669820079700069888469827865760065788'&
//'47367797777858473787100707369766883,9;',nwer,0,0,0)
call spak('677269677500076779787073710700838465846977697884,10'&
//';',nwer,0,0,0)
call spak('67726967750007798084737978830700838465846977697884,'&
//'11;',nwer,0,0,0)
call spak('84827386736576000769767378750700838465846977697884,'&
//'12;',nwer,0,0,0)
call spak('737867797883738384697884000769767378750700838465846'&
//'977697884,13;',nwer,0,0,0)
call spak('107673787510008384658469776978848300826981857382690'&
//'0658400766965838400180076697183,14;',nwer,0,0,0)
call spak('766582716900737884697169820082696568007982007169786'&
//'98265846968,15;',nwer,0,0,0)
call spak('697780848913838482737871007085786784737978006869846'&
//'9678469682600,16;',nwer,0,0,0)
call spak('737880858400806582847384737978007383007879840086657'&
//'67368,17;',nwer,0,0,0)
call spak('707376690078857766698283000880798284657600866982838'&
//'58300677978848279761370737669090068790078798400776584677'&
//'2,18;',nwer,0,0,0)
call spak('857869888069678469680070737669008469827773786584737'&
//'978,19;',nwer,0,0,0)
call spak('797869007982007779826900698884698278657600707369766'&
//'88300726586690066696978006869677665826968000773788469827'&
//'8657607,20;',nwer,0,0,0)
kk=3
call aoib(kk)
stib(stibs(1)-(kk-1):stibs(1))=eoa
ii=nwer+1
call trm(kk,ii,stibs(1))
weri(0)=stibs(1)-ii
werb(0)=weri(0)-ii
wera(0)=werb(0)-ii
do i1=1,nwer
stib(werb(0)+i1)=stib(werb(0)+i1)-1+stib(wera(0)+i1)
if(stib(weri(0)+i1).ne.i1)then
mlin(1:srec)='initq_3'
call mput(1,1,0,0,0)
end if
end do
call aoib(-ii)
weri(0)=nap
uc=0
naop=0
call spak('656683,1,1,1;',naop,0,0,0)
call spak('8472698465,1,0,2;',naop,0,0,0)
call spak('6869768465,2,2,3;',naop,0,0,0)
call spak('777968,2,2,4;',naop,0,0,0)
call spak('777378,1,0,5;',naop,0,0,0)
call spak('776588,1,0,6;',naop,0,0,0)
call spak('8079876982,2,2,7;',naop,0,0,0)
kk=5
call aoib(kk)
stib(stibs(1)-(kk-1):stibs(1))=eoa
ii=naop+1
call trm(kk,ii,stibs(1))
aopc(0)=stibs(1)-ii
aopnb(0)=aopc(0)-ii
aopna(0)=aopnb(0)-ii
aopl(0)=aopna(0)-ii
aopp(0)=aopl(0)-ii
do i1=1,naop
if(stib(aopc(0)+i1).ne.i1)then
mlin(1:srec)='initq_4'
call mput(1,1,0,0,0)
end if
end do
call aoib(-ii)
aopc(0)=nap
csyfty(:)=0
csyfty(qfty%ctyp)=5
csyfty(qfty%mtyp)=6
jflag(6)=1
return
end subroutine initq
subroutine emodel(jj,k1,k2,k3,k4,k5)
use,non_intrinsic::wbuffer
implicit none
save
integer,intent(in)::jj
integer::k1,k2,k3,k4,k5
select case(jj)
case(1)
mlin(1:srec)="field appears in more than one propagator,"
case(2)
mlin(1:srec)="no propagators found in"
case(3)
mlin(1:srec)="vertex degree is too small,"
case(4)
mlin(1:srec)="vertex degree is too large,"
case(5)
mlin(1:srec)="odd vertex in"
case(6)
mlin(1:srec)="unknown field in"
case(7)
mlin(1:srec)="no vertices found in"
case(8)
mlin(1:srec)="at least one field is not in any vertex"
case(9)
mlin(1:srec)="duplicate M-function or M-constant,"
case(10)
mlin(1:srec)="unexpected M-function(s),"
case(11)
mlin(1:srec)="undefined M-function or M-constant,"
case(12)
mlin(1:srec)="M-constants cannot be prefixed with 'dual-'"
case(13)
mlin(1:srec)="M-functions cannot appear outside a loop,"
case(14)
mlin(1:srec)="f-functions cannot appear in the (strict) vertex loop,"
case(15)
mlin(1:srec)="p-functions cannot appear in the (strict) vertex loop,"
case(16)
mlin(1:srec)="p-functions cannot be prefixed with 'dual-'"
case(17)
mlin(1:srec)="v-functions cannot be prefixed with 'dual-' except in the propagator_loop"
case(18)
mlin(1:srec)="M-function appearing in style-file was not found in"
case(19)
mlin(1:srec)="wrong dimension for M-function(s),"
case(20)
mlin(1:srec)="duplicate sector,"
case(21)
mlin(1:srec)="unknown or undefined sector,"
case(22)
mlin(1:srec)="unreferenced sector,"
case(23)
mlin(1:srec)="empty or incomplete sector,"
case(24)
mlin(1:srec)="duplicate submodel,"
case(25)
mlin(1:srec)="unknown or undefined submodel(s),"
case(26)
mlin(1:srec)="empty or incomplete submodel definition,"
case(27)
mlin(1:srec)="target submodel not found in"
case(28)
mlin(1:srec)="no target submodel in"
case(29)
mlin(1:srec)="unexpected statement,"
case(30)
mlin(1:srec)="undefined M-constant in"
case(31)
mlin(1:srec)="both constants and functions have to be declared in the"
case(32)
mlin(1:srec)="wrong-type value for constant or function in"
case(33)
mlin(1:srec)="sector_name is also a submodel_name,"
end select
call mput(k1,k2,k3,k4,k5)
return
end subroutine emodel
subroutine squiiz(ia,ib,jj,kk,uu)
use,non_intrinsic::qintr
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer,intent(in)::ia,ib,jj,kk
integer,intent(out)::uu
integer::i1,i2,i3,ii,j1,xp1,xp2,xp3
if((ia.le.0).or.&
(ia.ge.ib))then
go to 80
else if((jj.le.1).or.&
(kk.le.0))then
go to 80
else if(ib-ia.ne.jj*kk)then
go to 80
end if
ii=ia-jj
do i1=0,kk
ii=ii+jj
if(stib(ii).ne.eoa)then
go to 80
end if
end do
xp1=stibs(1)
call aoib(jj)
stib(xp1+1:stibs(1)-1)=0
stib(stibs(1))=eoa
i3=1
stib(xp1+1)=1
xp2=ia+1
do i1=2,jj-1
xp2=xp2+1
j1=xp2-jj
do i2=1,kk
j1=j1+jj
if(stib(j1).ne.stib(j1-1))then
i3=i3+1
stib(xp1+i1)=1
exit
end if
end do
end do
ii=i3+1
call aoib(ii*kk)
xp2=stibs(1)
do i1=kk-1,0,-1
stib(xp2)=eoa
xp2=xp2-ii
end do
uu=0
do i1=1,jj-1
if(stib(xp1+i1).ne.0)then
uu=uu+1
xp2=ia+i1-jj
xp3=xp1+jj+uu-ii
do i2=1,kk
xp2=xp2+jj
xp3=xp3+ii
stib(xp3)=stib(xp2)
end do
end if
stib(xp1+i1)=uu
end do
uu=uu+1
ii=xp1-ia
if(ii.gt.0)then
call xipht(xp1+1,stibs(1),-ii)
call aoib(-ii)
else
go to 80
end if
go to 90
80 continue
mlin(1:srec)='squiiz_1'
call mput(1,1,0,0,0)
90 return
end subroutine squiiz
subroutine stap(ia,ib,jf)
use,non_intrinsic::cbuffer
use,non_intrinsic::ikeys
use,non_intrinsic::qfiles
use,non_intrinsic::qintr
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer,intent(in)::ia,ib,jf
integer,external::stoz,sttyf
integer::i1,i2,ii,ij,ik,j1,j2,j3,j4,j5,j7,j8,j9,jj
integer::p0,p1,p2,q0,q1,sts
character(kind=ascii,len=88)::smat
p0=stjbs
call stpa(ia,ib,jf)
p0=p0+1
jj=stjb(p0)
if(jj.le.0)then
go to 80
end if
p1=p0+1
p2=p1+3*(jj-1)
j7=stcbs(1)
ii=iachar(stcb(j7:j7))
if((ii.ne.alf).and.&
(ii.ne.0))then
mlin(1:srec)='stap_1'
call mput(1,1,0,0,0)
end if
j4=p1
j5=0
sts=0
j8=1
do i1=jj-2,0,-1
j3=j4
j4=j4+3
if((abs(stjb(j3)-4).eq.stty%p).and.&
(abs(stjb(j4)-4).eq.stty%p))then
if(stjb(j3).ne.stjb(j4))then
go to 80
end if
if(j5.eq.0)then
if(i1.ne.0)then
j5=j5+1
else
j5=j5-1
end if
j1=j3+1
j2=stjb(j1+1)
ii=j2-stjb(j1)
sts=sts+ii
call vaocb(sts)
if(ii-1.gt.0)then
j8=j8-1
end if
j9=j3
do i2=stjb(j1),j2-1
j7=j7+1
stcb(j7:j7)=stcb(i2:i2)
end do
end if
j2=stjb(j4+1)+1
ii=stjb(j4+2)-j2
if(ii.gt.0)then
j8=j8-1
j9=j4
sts=sts+ii
call vaocb(sts)
do i2=j2,stjb(j4+2)-1
j7=j7+1
stcb(j7:j7)=stcb(i2:i2)
end do
end if
stjb(j3)=0
else
j5=-j5
end if
if(j5.lt.0)then
if(j8.lt.0)then
sts=sts+2
j5=stcbs(1)+1
call aocb(sts)
j7=j7+1
stcb(j7:stcbs(1))=stcb(j5:j5)//achar(alf)
j2=j3+1
stjb(j2)=j5
stjb(j2+1)=j7
else
if(j3.ne.j9)then
stjb(j9)=stjb(j3)
stjb(j3)=0
end if
end if
j5=0
j7=stcbs(1)
j8=1
sts=0
end if
end do
j3=p1-3
j4=j3
do i1=1,jj
j4=j4+3
if(stjb(j4).ne.0)then
j3=j3+3
if(j3.ne.j4)then
stjb(j3)=stjb(j4)
stjb(j3+1)=stjb(j4+1)
stjb(j3+2)=stjb(j4+2)
end if
else
stjb(p0)=stjb(p0)-1
if(stjb(p0).le.0)then
go to 80
end if
end if
end do
if(stjb(p0).ne.jj)then
jj=stjb(p0)
p2=p1+3*(jj-1)
call aojb(-1)
ii=p2-p1+4
call aojb(ii)
end if
stapi(:)=0
if(jf.eq.qfty%ctyp)then
go to 100
else if(jf.eq.qfty%mtyp)then
go to 300
else
go to 80
end if
100 continue
if(jj.lt.3)then
go to 80
end if
smat(:)='p;q'//achar(anull)
call qmatcha(p2,p1,smat,len(smat))
smat(:)='sip=q'//achar(anull)
call qmatcha(p1,p2,smat,len(smat))
q0=p1+1
ij=0
call mstr0(stcb,stjb(q0),stjb(q0+1),pisk(0),wisk(0),aisk(0),ij)
if(ij.le.0)then
go to 80
end if
ij=stib(cisk(0)+ij)
if(ij.le.0)then
go to 80
end if
stapi(1)=ij
if(jj.eq.3)then
if((stapi(1).eq.dekc%conf).or.&
(stapi(1).eq.dekc%msg).or.&
(stapi(1).eq.dekc%outp).or.&
(stapi(1).eq.dekc%sty).or.&
(stapi(1).eq.dekc%in).or.&
(stapi(1).eq.dekc%out).or.&
(stapi(1).eq.dekc%loopk).or.&
(stapi(1).eq.dekc%opt).or.&
(stapi(1).eq.dekc%part).or.&
(stapi(1).eq.dekc%off))then
smat(:)='sip=p;'//achar(anull)
call qmatcha(p1,p2,smat,len(smat))
if(jbco(0).ne.0)then
go to 80
else
go to 90
end if
end if
end if
if((stapi(1).eq.dekc%msg).or.&
(stapi(1).eq.dekc%outp).or.&
(stapi(1).eq.dekc%sty))then
smat(:)='sip=ssp;'//achar(anull)
call qmatcha(p1,p2,smat,len(smat))
if(jbco(0).ne.0)then
go to 80
end if
stapi(2)=1
else if((stapi(1).eq.dekc%loop).or.&
(stapi(1).eq.dekc%off))then
smat(:)='sip=szp;'//achar(anull)
call qmatcha(p1,p2,smat,len(smat))
if(jbco(0).eq.0)then
stapi(2)=1
else if(stapi(1).eq.dekc%loop)then
smat(:)='sip=szsiszp;'//achar(anull)
call qmatcha(p1,p2,smat,len(smat))
if(jbco(0).eq.0)then
stapi(2)=2
end if
end if
if(jbco(0).ne.0)then
go to 80
end if
else if(stapi(1).eq.dekc%loopk)then
smat(:)='sip=sip;'//achar(anull)
call qmatcha(p1,p2,smat,len(smat))
if(jbco(0).ne.0)then
go to 80
end if
stapi(2)=1
else if(stapi(1).eq.dekc%part)then
smat(:)='sip=(szp^[sz|p(s2p)|p(szp)])p;'//achar(anull)
call qmatcha(p1,p2,smat,len(smat))
ii=0
if(jbco(0).lt.2)then
ii=1
else if(jbco(1).ne.qmatc%lc)then
ii=1
else if(jbco(2).lt.1)then
ii=1
else if(jbco(0).ne.2*(jbco(2)+1))then
ii=1
end if
j1=2
do i1=1,jbco(2)
j1=j1+1
if(jbco(j1).ne.qmatc%oc)then
ii=1
end if
j1=j1+1
if(jbco(j1).lt.1)then
ii=1
else if(jbco(j1).gt.3)then
ii=1
end if
end do
if(ii.ne.0)then
go to 80
end if
stapi(2)=2*jbco(2)
else if(stapi(1).eq.dekc%model)then
smat(:)='si[p=sid/|p=]ssp;'//achar(anull)
call qmatcha(p1,p2,smat,len(smat))
if(jbco(0).ne.2)then
go to 80
else if(jbco(1).ne.qmatc%oc)then
go to 80
else if(jbco(2).lt.1)then
go to 80
else if(jbco(2).gt.2)then
go to 80
end if
stapi(2)=3-jbco(2)
else if((stapi(1).eq.dekc%conf).or.&
(stapi(1).eq.dekc%opt))then
smat(:)='sip=(si\p;\p,)'//achar(anull)
call qmatcha(p1,p2,smat,len(smat))
if(jbco(0).ne.2)then
go to 80
else if(jbco(1).ne.qmatc%lc)then
go to 80
else if(jbco(2).lt.1)then
go to 80
end if
stapi(2)=jbco(2)
else if((stapi(1).eq.dekc%in).or.&
(stapi(1).eq.dekc%out))then
smat(:)='sip=(si\p;\p,)'//achar(anull)
call qmatcha(p1,p2,smat,len(smat))
ii=0
if(jbco(0).ne.2)then
ii=1
else if(jbco(1).ne.qmatc%lc)then
ii=1
else if(jbco(2).lt.1)then
ii=1
end if
if(ii.ne.0)then
smat(:)='sip=(sip[sip]\p;\p,)'//achar(anull)
call qmatcha(p1,p2,smat,len(smat))
ii=0
if(jbco(0).ne.2)then
ii=1
else if(jbco(1).ne.qmatc%lc)then
ii=1
else if(jbco(2).lt.1)then
ii=1
end if
if(ii.ne.0)then
go to 80
end if
end if
stapi(2)=jbco(2)
else if((stapi(1).eq.dekc%tru).or.&
(stapi(1).eq.dekc%fals))then
smat(:)='sip=sip[(s1p,)szp]p;'//achar(anull)
call qmatcha(p1,p2,smat,len(smat))
if(jbco(0).lt.2)then
go to 80
else if(jbco(1).ne.qmatc%lc)then
go to 80
else if(jbco(2).lt.0)then
go to 80
end if
q0=p1+7
ij=0
call mstr0(stcb,stjb(q0),stjb(q0+1),popt2(0),wopt2(0),aopt2(0),ij)
if(ij.le.0)then
go to 80
end if
ij=stib(copt2(0)+ij)
if(ij.le.0)then
go to 80
end if
select case(ij)
case(1:5)
smat(:)='sip=sip[(sip,)szp,szp]p;'//achar(anull)
call qmatcha(p1,p2,smat,len(smat))
if(jbco(0).ne.2)then
go to 80
else if(jbco(1).ne.qmatc%lc)then
go to 80
else if(jbco(2).lt.0)then
go to 80
end if
stapi(2)=jbco(2)+2
case(6:7)
smat(:)='sip=sip[sip,szp,szp]p;'//achar(anull)
call qmatcha(p1,p2,smat,len(smat))
if(jbco(0).ne.0)then
go to 80
end if
stapi(2)=3
case(11)
smat(:)='sip=sip[(szp,)sip,szp,szp]p;'//achar(anull)
call qmatcha(p1,p2,smat,len(smat))
if(jbco(0).ne.2)then
go to 80
else if(jbco(1).ne.qmatc%lc)then
go to 80
else if(jbco(2).le.0)then
go to 80
end if
stapi(2)=jbco(2)+2
case(12)
smat(:)='sip=sip[(sz\p]p;\p,)'//achar(anull)
call qmatcha(p1,p2,smat,len(smat))
if(jbco(0).ne.2)then
go to 80
else if(jbco(1).ne.qmatc%lc)then
go to 80
else if(jbco(2).le.0)then
go to 80
end if
stapi(2)=jbco(2)+1
case default
go to 80
end select
else
go to 80
end if
go to 90
300 continue
if(jj.lt.3)then
go to 80
end if
smat(:)='p]q'//achar(anull)
call qmatcha(p2,p1,smat,len(smat))
if(jbco(0).ne.0)then
go to 80
end if
q0=0
q1=0
smat(:)='[p[|si]q'//achar(anull)
call qmatcha(p1,p2,smat,len(smat))
if(jbco(0).ne.2)then
go to 80
else if(jbco(1).ne.qmatc%oc)then
go to 80
else if(jbco(2).eq.1)then
go to 301
else if(jbco(2).eq.2)then
go to 304
else
go to 80
end if
301 continue
smat(:)='p[si[p,|p]]q'//achar(anull)
call qmatcha(p1,p2,smat,len(smat))
if(jbco(0).eq.2)then
if(jbco(1).eq.qmatc%oc)then
if(jbco(2).le.0)then
go to 80
else if(jbco(2).gt.2)then
go to 80
end if
if(jbco(2).eq.1)then
smat(:)='p[sip,sip,[sz|s+|s-]q'//achar(anull)
call qmatcha(p1,p2,smat,len(smat))
if(jbco(0).eq.2)then
if(jbco(1).eq.qmatc%oc)then
if(abs(jbco(2)-2).le.1)then
stapi(1)=dekm%mp
go to 307
end if
end if
end if
end if
stapi(1)=dekm%mv
go to 307
end if
end if
if(jj.gt.4)then
smat(:)='p[[d;|p;|]sip=q'//achar(anull)
call qmatcha(p1,p2,smat,len(smat))
if(jbco(0).eq.2)then
if(jbco(1).eq.qmatc%oc)then
if(jbco(2).eq.1)then
stapi(1)=dekm%mfss
else if(jbco(2).eq.2)then
stapi(1)=dekm%mfs
else if(jbco(2).eq.3)then
stapi(1)=dekm%mcge
else
go to 80
end if
go to 307
end if
end if
end if
if(jj.gt.3)then
smat(:)='p[si[si|]d:q'//achar(anull)
call qmatcha(p1,p2,smat,len(smat))
if(jbco(0).eq.2)then
if(jbco(1).eq.qmatc%oc)then
if(jbco(2).eq.1)then
q1=p1+4
q0=q1+3
stapi(2)=1
else if(jbco(2).eq.2)then
q0=p1+4
else
go to 80
end if
go to 307
end if
end if
end if
go to 80
304 continue
q0=p1+1
307 continue
if(q0.gt.0)then
if(stjb(p1).ne.stty%id)then
ik=0
else
ik=1
end if
call mstr0(stcb,stjb(q0),stjb(q0+1),popt5(0),wopt5(0),aopt5(0),ik)
if(ik.le.0)then
go to 80
end if
stapi(1)=stib(copt5(0)+ik)
ij=stib(sopt5(0)+ik)
if(stapi(2).eq.0)then
stapi(2)=ij
else
stapi(2)=-ij
end if
end if
if(q1.gt.0)then
ik=0
call mstr0(stcb,stjb(q1),stjb(q1+1),popt7(0),wopt7(0),aopt7(0),ik)
if(ik.le.0)then
go to 80
end if
end if
if((stapi(1).eq.dekm%smcc).or.&
(stapi(1).eq.dekm%skcc))then
smat(:)='p[sid:(si\p]\p,)'//achar(anull)
call qmatcha(p1,p2,smat,len(smat))
if(jbco(0).ne.2)then
go to 80
else if(jbco(1).ne.qmatc%lc)then
go to 80
else if(jbco(2).lt.1)then
go to 80
end if
stapi(3)=jbco(2)
else if(stapi(1).eq.dekm%ski)then
smat(:)='p[sid:[p]|(si\p]\p,)]'//achar(anull)
call qmatcha(p1,p2,smat,len(smat))
if(jbco(0).eq.2)then
if(jbco(1).ne.qmatc%oc)then
go to 80
else if(jbco(2).ne.1)then
go to 80
end if
stapi(3)=0
else if(jbco(0).eq.4)then
if(jbco(1).ne.qmatc%oc)then
go to 80
else if(jbco(2).ne.2)then
go to 80
else if(jbco(3).ne.qmatc%lc)then
go to 80
else if(jbco(4).lt.1)then
go to 80
end if
stapi(3)=jbco(4)
else
go to 80
end if
else if((stapi(1).eq.dekm%smp).or.&
(stapi(1).eq.dekm%smq).or.&
(stapi(1).eq.dekm%skp))then
smat(:)='sip[sip]'//achar(anull)
call qmatcha(p1,p2,smat,len(smat))
if(jbco(0).ne.0)then
go to 80
end if
stapi(3)=1
else if((stapi(1).eq.dekm%mccc).or.&
(stapi(1).eq.dekm%mfcc))then
if(stapi(2).gt.0)then
smat(:)='p[sid:(si\p]\p,)'//achar(anull)
call qmatcha(p1,p2,smat,len(smat))
if(jbco(0).ne.2)then
go to 80
else if(jbco(1).ne.qmatc%lc)then
go to 80
else if(jbco(2).lt.1)then
go to 80
end if
else if(stapi(2).lt.0)then
smat(:)='p[sisid:(si\p]\p,)'//achar(anull)
call qmatcha(p1,p2,smat,len(smat))
if(jbco(0).ne.2)then
go to 80
else if(jbco(1).ne.qmatc%lc)then
go to 80
else if(jbco(2).lt.0)then
go to 80
end if
else
go to 80
end if
stapi(3)=jbco(2)
else if(stapi(1).eq.dekm%mcge)then
smat(:)='p[sip=s1p]'//achar(anull)
call qmatcha(p1,p2,smat,len(smat))
if(jbco(0).ne.0)then
go to 80
end if
stapi(3)=2
else if((stapi(1).eq.dekm%mfss).or.&
(stapi(1).eq.dekm%mfs))then
smat(:)='p[[d;|p;]sip=[s1|p(s1p)|p(s1p,s1p)]'//&
'[p,p(s1p)|p,p(s1p,s1p)|]p]'//achar(anull)
call qmatcha(p1,p2,smat,len(smat))
if(jbco(0).eq.6)then
if(jbco(1).eq.qmatc%oc)then
if(jbco(3).eq.qmatc%oc)then
if((jbco(4).eq.1).and.&
(jbco(6).eq.3))then
stapi(3)=2
else if((jbco(4).eq.2).and.&
(jbco(6).eq.2))then
stapi(3)=4
stapi(4)=3
else if((jbco(4).eq.2).and.&
(jbco(6).eq.3))then
stapi(3)=2
stapi(4)=2
else if((jbco(4).eq.3).and.&
(jbco(6).eq.1))then
stapi(3)=4
stapi(4)=4
else if((jbco(4).eq.3).and.&
(jbco(6).eq.3))then
stapi(3)=3
stapi(4)=1
else
go to 80
end if
end if
end if
end if
if(stapi(3).eq.0)then
go to 80
end if
else if(stapi(1).eq.dekm%mp)then
smat(:)='p[sip,sip,[sz|s+|s-](p,si)p]'//achar(anull)
call qmatcha(p1,p2,smat,len(smat))
if(jbco(0).lt.0)then
smat(:)='p[sip,sip,[sz|s+|s-](p,si)p;'//&
'(sip=[s1|p(s1p)|p(s1p,s1p)]\p]\p,)'//achar(anull)
call qmatcha(p1,p2,smat,len(smat))
end if
ii=1
if(jbco(0).lt.4)then
ii=0
else if(jbco(1).ne.qmatc%oc)then
ii=0
else if(abs(jbco(2)-2).gt.1)then
ii=0
else if(jbco(3).ne.qmatc%lc)then
ii=0
else if(jbco(4).lt.0)then
ii=0
end if
if(ii.eq.0)then
go to 80
end if
if(jbco(0).eq.4)then
stapi(3)=3+jbco(4)
else
if(jbco(5).ne.qmatc%lc)then
ii=0
else if(jbco(6).lt.0)then
ii=0
else if(jbco(0)-6.ne.2*jbco(6))then
ii=0
end if
if(ii.eq.0)then
go to 80
end if
j1=6
do i1=1,jbco(6)
j1=j1+1
if(jbco(j1).ne.qmatc%oc)then
ii=0
end if
j1=j1+1
if(abs(jbco(j1)-2).gt.1)then
ii=0
end if
end do
if(ii.eq.0)then
go to 80
end if
stapi(3)=3+jbco(4)
stapi(4)=jbco(6)
end if
stapi(2)=mlty%mpf
else if(stapi(1).eq.dekm%mv)then
smat(:)='p[(si\p]\p,)'//achar(anull)
call qmatcha(p1,p2,smat,len(smat))
if(jbco(0).eq.2)then
if(jbco(1).eq.qmatc%lc)then
if(jbco(2).gt.0)then
stapi(3)=jbco(2)
end if
end if
end if
if(jbco(0).lt.0)then
smat(:)='p[(si\p;\p,)(sip=s1\p]\p,)'//achar(anull)
call qmatcha(p1,p2,smat,len(smat))
if(jbco(0).eq.4)then
if(jbco(1).eq.qmatc%lc)then
if(jbco(2).gt.0)then
if(jbco(3).eq.qmatc%lc)then
if(jbco(4).gt.0)then
stapi(3)=jbco(2)
stapi(4)=jbco(4)
end if
end if
end if
end if
end if
end if
if(stapi(3).eq.0)then
go to 80
end if
stapi(2)=mlty%mvf
else
go to 80
end if
go to 90
80 continue
stapi(1)=0
90 continue
return
end subroutine stap
subroutine aocb(delta)
use,non_intrinsic::cbuffer
use,non_intrinsic::wbuffer
implicit none
save
integer,intent(in)::delta
integer::ii,jj,kk,ios
if(delta.gt.0)then
jj=(stcbas(1)-stcbs(1))-delta
kk=stcbdst(1)
do while((jj.lt.0).and.&
(kk.lt.stcbdst0))
kk=kk+1
jj=(scbff(kk)-dcff0-stcbs(1))-delta
end do
if(jj.lt.0)then
call uput(6)
else if(kk.gt.stcbdst(1))then
stcbdst(1)=kk
ii=scbff(kk)+1
if(.not.allocated(ystcb))then
ios=1
allocate(character(ii)::ystcb,stat=ios)
if(ios.ne.0)then
call uput(0)
end if
ystcb(1:stcbas(1))=xstcb(1:stcbas(1))
xstcb(1:stcbas(1))=achar(anull)
ios=1
deallocate(xstcb,stat=ios)
stcb=>ystcb
else if(.not.allocated(xstcb))then
ios=1
allocate(character(ii)::xstcb,stat=ios)
if(ios.ne.0)then
call uput(0)
end if
xstcb(1:stcbas(1))=ystcb(1:stcbas(1))
ystcb(1:stcbas(1))=achar(anull)
ios=1
deallocate(ystcb,stat=ios)
stcb=>xstcb
else
mlin(1:srec)='aocb_1'
call mput(1,1,0,0,0)
end if
if(ios.ne.0)then
call uput(0)
end if
stcbas(1)=ii-dcff0
end if
stcbs(1)=stcbs(1)+delta
else if(delta.lt.0)then
stcbs(1)=stcbs(1)+delta
if(stcbs(1).lt.0)then
mlin(1:srec)='aocb_2'
call mput(1,1,0,0,0)
end if
end if
return
end subroutine aocb
subroutine vaocb(delta)
use,non_intrinsic::cbuffer
use,non_intrinsic::wbuffer
implicit none
save
integer,intent(in)::delta
integer::ii
if((delta.lt.0).or.&
(stcbs(1).lt.0))then
mlin(1:srec)='vaocb_1'
call mput(1,1,0,0,0)
else if(stcbs(1).gt.stcbas(1)-delta)then
ii=delta
call aocb(ii)
call aocb(-ii)
end if
return
end subroutine vaocb
subroutine aoib(delta)
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer,intent(in)::delta
integer::ii,jj,kk,ios
if(delta.gt.0)then
jj=(stibas(1)-stibs(1))-delta
kk=stibdst(1)
do while((jj.lt.0).and.&
(kk.lt.stibdst0))
kk=kk+1
jj=(sibff(kk)-diff0-stibs(1))-delta
end do
if(jj.lt.0)then
call uput(4)
else if(kk.gt.stibdst(1))then
stibdst(1)=kk
ii=sibff(kk)+1
if(.not.allocated(ystib))then
ios=1
allocate(ystib(ii),stat=ios)
if(ios.ne.0)then
call uput(0)
end if
ystib(1:stibas(1))=xstib(1:stibas(1))
xstib(1:stibas(1))=0
ios=1
deallocate(xstib,stat=ios)
stib=>ystib
else if(.not.allocated(xstib))then
ios=1
allocate(xstib(ii),stat=ios)
if(ios.ne.0)then
call uput(0)
end if
xstib(1:stibas(1))=ystib(1:stibas(1))
ystib(1:stibas(1))=0
ios=1
deallocate(ystib,stat=ios)
stib=>xstib
else
mlin(1:srec)='aoib_1'
call mput(1,1,0,0,0)
end if
if(ios.ne.0)then
call uput(0)
end if
stibas(1)=ii-diff0
end if
stibs(1)=stibs(1)+delta
else if(delta.lt.0)then
stibs(1)=stibs(1)+delta
if(stibs(1).lt.0)then
mlin(1:srec)='aoib_2'
call mput(1,1,0,0,0)
end if
end if
return
end subroutine aoib
subroutine aojb(delta)
use,non_intrinsic::qintr
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer,parameter::stj1=stj0-1
integer,parameter::sjb0=sjbuff-stj0
integer,intent(in)::delta
integer::dd,ii,jj,kk
jj=stjbs
if(jj.lt.0)then
go to 80
else if(jj.lt.stj0)then
go to 80
else if(jj.gt.sjbuff)then
go to 80
end if
ii=stjb(jj)
if(ii.gt.0)then
go to 80
else if(ii.ne.eoa)then
go to 80
end if
ii=stjb(jj-1)
if(ii.gt.0)then
go to 80
else if(ii.ne.nap)then
go to 80
end if
ii=stjb(jj-stj1)
if(ii.gt.0)then
go to 80
else if(ii.ne.eoa)then
go to 80
end if
if(delta.gt.0)then
if(delta.le.sjb0)then
dd=delta+stj0
kk=(sjbuff-jj)-dd
if(kk.ge.0)then
go to 10
end if
end if
call uput(5)
10 continue
stjbs=jj+dd
stjb(jj-1)=stjbs
stjb(stjbs)=eoa
stjb(stjbs-1)=nap
stjb(stjbs-2)=jj
stjb(stjbs-stj1)=eoa
nstjb=nstjb+1
go to 90
else if(delta+1.eq.0)then
kk=stjb(jj-2)
if(kk.lt.0)then
go to 80
else if(kk-stj0.lt.0)then
go to 80
else if(kk-sjb0.gt.0)then
go to 80
else if(kk-(jj-stj0).gt.0)then
go to 80
end if
ii=stjb(kk)
if(ii.gt.0)then
go to 80
else if(ii.ne.eoa)then
go to 80
end if
ii=stjb(kk-1)
if(ii.lt.0)then
go to 80
else if(ii.ne.stjbs)then
go to 80
end if
ii=stjb(kk-stj1)
if(ii.gt.0)then
go to 80
else if(ii.ne.eoa)then
go to 80
end if
stjbs=kk
stjb(kk-1)=nap
nstjb=nstjb-1
go to 90
end if
80 continue
mlin(1:srec)='aojb_1'
call mput(1,1,0,0,0)
90 return
end subroutine aojb
subroutine vaoib(delta)
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer,intent(in)::delta
integer::ii
if((delta.gt.0).and.&
(stibs(1).ge.0))then
if((stibas(1)-delta).lt.stibs(1))then
ii=delta
call aoib(ii)
call aoib(-ii)
end if
else
mlin(1:srec)='vaoib_1'
call mput(1,1,0,0,0)
end if
return
end subroutine vaoib
subroutine vaojb(delta)
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer,intent(in)::delta
if((delta.le.0).or.&
(stjbs.le.0))then
mlin(1:srec)='vaojb_1'
call mput(1,1,0,0,0)
end if
if(delta.gt.sjbuff-stjbs)then
call uput(5)
end if
return
end subroutine vaojb
subroutine mput(istop,imsg,nl1,nl2,nf1)
use,non_intrinsic::cbuffer
use,non_intrinsic::ikeys
use,non_intrinsic::qerr
use,non_intrinsic::qfiles
use,non_intrinsic::qintr
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer,intent(in)::istop,imsg,nl1,nl2,nf1
integer,external::wztos
integer::cf2,cf6,jstop,slin1,slin2,slin3,stab
integer::i1,i2,j1,j2,j3,j4,j5,jj,f0
jstop=abs(istop)
if(jstop.eq.0)then
if(imsg.ne.0)then
jstop=1
end if
end if
if(jstop.ne.0)then
if(jflag(6).eq.1)then
if(cflag(11).eq.0)then
call spp(0,0,0,0)
end if
jflag(6)=2
end if
cf2=1
else
cf2=cflag(2)
end if
j1=0
if(abs(nf1).gt.nfiles)then
j1=1
else if(nf1.ne.0)then
j2=filt(abs(nf1))
if(j2.le.0)then
j1=1
else if(j2.gt.qfty%ubo)then
j1=1
else if(j2.eq.qfty%ptyp)then
j1=1
end if
end if
if(j1.ne.0)then
if(cflag(11).eq.0)then
if(cf2.gt.0)then
write(unit=*,fmt='(/,a,/)')" error: mput_2"
end if
stop
else
mlin(:)=" error: mput_2"//achar(anull)
qerrty(0)=qerrt%qerr
stop
go to 90
end if
end if
cf6=min(cflag(14),1)
if(cf6.gt.0)then
do i1=1,nfiles
if(filt(i1).eq.qfty%wtyp)then
f0=filu(i1)
exit
end if
end do
end if
slin1=ssrec-1
slin2=2*srec
slin3=(stcbas(1)-wiref)-stcbs(1)
stab=0
if(cflag(11).eq.0)then
if(cf2.gt.0)then
call hrul(2,2)
if(jflag(7).eq.0)then
write(unit=*,fmt='(a)')
end if
write(unit=*,fmt='(a)')stxb(1:slin1)
end if
end if
if(cf6*cflag(14).eq.1)then
call hrul(2,1)
write(unit=f0,fmt='(/,a)')stxb(1:sfrec)
end if
do j2=srec-1,1,-1
if(iachar(mlin(j2:j2)).ne.aspc)then
go to 30
end if
end do
j1=0
j2=0
go to 40
30 continue
do j1=1,j2
if(iachar(mlin(j1:j1)).ne.aspc)then
exit
end if
end do
40 continue
if(jflag(6).eq.0)then
if(cflag(11).eq.0)then
if(cf2.gt.0)then
if(j1.gt.0)then
write(unit=*,fmt='(a)')"   error: "//mlin(j1:j2)
end if
end if
end if
go to 70
end if
j3=21+jstop
i1=stib(xtstrp(0)+j3)
i2=stib(xtstrl(0)+j3)
stab=i2-1
if(i2.ge.slin2)then
if(cflag(11).eq.0)then
if(cf2.gt.0)then
write(unit=*,fmt='(/,a,/)')" error: mput_1"
end if
stop
else
mlin(:)=" error: mput_2"//achar(anull)
qerrty(0)=qerrt%qerr
stop
go to 90
end if
end if
stxb(1:i2)=stcb(i1:i1-1+i2)
if(nf1.ge.0)then
if(j1.gt.0)then
j3=min(slin2-i2,j2-j1+1)
if(j3.gt.0)then
stxb(i2+1:i2+j3)=mlin(j1:j1-1+j3)
i2=i2+j3
end if
end if
end if
if(nf1.ne.0)then
j4=filt(abs(nf1))
if(nf1.gt.0)then
if(i2.lt.slin2)then
i2=i2+1
stxb(i2:i2)=stcb(1:1)
end if
end if
j3=22+j4
i1=stib(xtstrl(0)+j3)
if(i2+i1.le.slin2)then
j3=stib(xtstrp(0)+j3)
stxb(i2+1:i2+i1)=stcb(j3:j3-1+i1)
end if
i2=i2+i1
if(nsfiles.gt.1)then
if((j4.eq.qfty%styp).or.&
(j4.eq.qfty%otyp))then
if(i2.lt.slin2+1)then
i2=i2+2
stxb(i2-1:i2)=' #'
end if
j4=film(abs(nf1))
if(i2+wztos(j4).le.slin2)then
call dkar(j4,jj)
j5=stcbs(1)
call ctxb(i2,jj,j5)
end if
end if
end if
end if
if(nf1.lt.0)then
if(j1.gt.0)then
if(j2-j1+2.le.slin2-i2)then
stxb(i2+1:i2+2+(j2-j1))=stcb(1:1)//mlin(j1:j2)
i2=i2+(j2-j1)+2
end if
end if
end if
if(nl1.gt.0)then
if(nl1.eq.nl2)then
i1=stib(xtstrl(0)+19)
if(i2+i1.le.slin2)then
j3=stib(xtstrp(0)+19)
stxb(i2+1:i2+i1)=stcb(j3:j3-1+i1)
end if
i2=i2+i1
if(i2+wztos(nl1).le.slin2)then
if(slin3.ge.0)then
call dkar(nl1,jj)
j5=stcbs(1)
call ctxb(i2,jj,j5)
else
i1=stib(xtstrl(0)+18)
j3=stib(xtstrp(0)+18)
stxb(i2+1:i2+i1)=stcb(j3:j3-1+i1)
i2=i2+i1
end if
end if
else
i1=stib(xtstrl(0)+20)
if(i2+i1.le.slin2)then
j3=stib(xtstrp(0)+20)
stxb(i2+1:i2+i1)=stcb(j3:j3-1+i1)
end if
i2=i2+i1
if(i2+wztos(nl1).le.slin2)then
if(slin3.ge.0)then
call dkar(nl1,jj)
j5=stcbs(1)
call ctxb(i2,jj,j5)
else
i1=stib(xtstrl(0)+18)
j3=stib(xtstrp(0)+18)
stxb(i2+1:i2+i1)=stcb(j3:j3-1+i1)
i2=i2+i1
end if
end if
if(i2+1.le.slin2)then
stxb(i2+1:i2+1)=achar(aminus)
end if
i2=i2+1
if(i2+wztos(nl2).le.slin2)then
if(slin3.ge.0)then
call dkar(nl2,jj)
j5=stcbs(1)
call ctxb(i2,jj,j5)
else
i1=stib(xtstrl(0)+18)
j3=stib(xtstrp(0)+18)
stxb(i2+1:i2+i1)=stcb(j3:j3-1+i1)
i2=i2+i1
end if
end if
end if
end if
if(cflag(11).eq.0)then
if(cf2.gt.0)then
if(i2.lt.slin1)then
write(unit=*,fmt='(a)')stxb(1:i2)
else
j1=slin1
do while(j1.gt.0)
if(iachar(stxb(j1:j1)).eq.aspc)then
exit
end if
j1=j1-1
end do
if(j1.gt.1)then
write(unit=*,fmt='(a)')stxb(1:j1-1)
end if
write(unit=*,fmt='(a)')stcb(1:stab)//stxb(j1:i2)
end if
end if
end if
if(cf6.gt.0)then
j2=0
do i1=1,i2
if(iachar(stxb(i1:i1)).eq.aspc)then
j2=j2+1
else
exit
end if
end do
if(imsg.eq.0)then
j5=stib(xtstrl(0)+21)
else
j5=stib(xtstrl(0)+22)
end if
do i1=j5+1,i2
if(iachar(stxb(i1:i1)).eq.aspc)then
j5=j5+1
else
exit
end if
end do
do i1=j5+1,i2
j2=j2+1
stxb(j2:j2)=stxb(i1:i1)
end do
if(j2.le.sfrec)then
if(cflag(11).eq.0)then
write(unit=f0,fmt='(a)')stxb(2:j2)
else
mlin(1:srec)=stxb(2:j2)//achar(anull)
end if
else
j1=sfrec+1
do while(j1.gt.0)
if(iachar(stxb(j1:j1)).eq.aspc)then
exit
end if
j1=j1-1
end do
if(cflag(11).eq.0)then
if(j1.gt.2)then
write(unit=f0,fmt='(a)')stxb(2:j1-1)
end if
stab=min(3,srec)
write(unit=f0,fmt='(a)')stcb(1:stab)//stxb(j1:j2)
else
j3=0
if(j1.gt.2)then
j3=j1-2
mlin(1:j3)=stxb(2:j3+1)
end if
stab=min(3,srec)
j3=j3+1
j5=(j3+1)+stab+(j2-j1)
mlin(j3:j3+j5)=stcb(1:stab)//stxb(j1:j2)//achar(anull)
end if
end if
end if
70 continue
if(cflag(11).eq.0)then
if(cf2.gt.0)then
call hrul(2,2)
write(unit=*,fmt='(a,/)')stxb(1:slin1)
jflag(7)=10
end if
end if
if(cf6.gt.0)then
call hrul(2,1)
write(unit=f0,fmt='(a)')stxb(1:sfrec)
cflag(14)=cflag(14)+1
end if
if(istop.gt.0)then
call qclose(0,istop)
if(cflag(11).eq.0)then
stop
else
stop
go to 90
end if
else
qerrty(0)=qerrt%qw
end if
90 continue
return
end subroutine mput
subroutine hrul(ii,jj)
use,non_intrinsic::wbuffer
implicit none
save
integer,intent(in)::ii,jj
integer::i1,j1
j1=max(0,jj)
j1=min(4,j1)
do i1=1,j1
stxb(i1:i1)=achar(aspc)
end do
if(ii.eq.1)then
do i1=j1+1,max(sfrec,ssrec)
stxb(i1:i1)=achar(aminus)
end do
else if(ii.eq.2)then
do i1=j1+1,max(sfrec,ssrec)
stxb(i1:i1)=achar(aeq)
end do
end if
return
end subroutine hrul
subroutine uput(ind)
use,non_intrinsic::qintr
use,non_intrinsic::wbuffer
implicit none
save
integer,intent(in)::ind
integer::i1,ii
select case(ind)
case(0)
mlin(1:srec)="memory allocation error"
case(1)
mlin(1:srec)="system/filesystem I/O error"
case(2)
mlin(1:srec)="unsupported character set"
case(3)
mlin(1:srec)="largest available integer is too small"
case(4)
mlin(1:srec)="parameter 'sibuff' too small"
case(5)
mlin(1:srec)="parameter 'sjbuff' too small"
case(6)
mlin(1:srec)="parameter 'scbuff' too small"
case(7)
mlin(1:srec)="parameter 'swbuff0' too small"
case(8)
mlin(1:srec)="parameter 'sxbuff' too small"
case(9)
mlin(1:srec)="parameter 'sibuff' not properly set"
case(10)
mlin(1:srec)="parameter 'sjbuff' not properly set"
case(11)
mlin(1:srec)="parameter 'scbuff' not properly set"
case(12)
mlin(1:srec)="parameter 'swbuff' not properly set"
case(13)
mlin(1:srec)="parameter 'sxbuff' not properly set"
case(14)
mlin(1:srec)="parameter 'srec' not properly set"
case(15)
mlin(1:srec)="parameter 'sfrec' not properly set"
case(16)
mlin(1:srec)="parameter 'ssrec' not properly set"
case(17)
mlin(1:srec)="parameter 'srecx' not properly set"
case default
mlin(1:srec)='uput_1'
end select
ii=sxbuff-max(80,sfrec,ssrec)
if(cflag(11).eq.0)then
do i1=srec,1,-1
if(iachar(mlin(i1:i1)).ne.aspc)then
if(ii.gt.0)then
call hrul(2,2)
write(unit=*,fmt='(/,a,/,a,/,a,/)')stxb(1:ssrec-1),&
"   error: "//mlin(1:i1),stxb(1:ssrec-1)
else
write(unit=*,fmt='(//,a,/)')"   error: "//mlin(1:i1)
end if
exit
end if
end do
stop
else
stop
go to 90
end if
90 continue
return
end subroutine uput
subroutine wput(sind,j1,j2)
use,non_intrinsic::cbuffer
use,non_intrinsic::qintr
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer,intent(in)::sind,j1,j2
integer::i1,i2,ii,jj,kk,ll,aind
if(sind.gt.0)then
if(cflag(2).le.0)then
jflag(8)=min(jflag(8)+1,iref)
if(cflag(14).eq.0)then
go to 90
end if
end if
end if
aind=abs(sind)
ii=0
if(aind.gt.nwer)then
ii=1
else if(j1.lt.0)then
ii=1
else if(j1.eq.0)then
if(j2.ne.0)then
ii=1
end if
else
jj=j2-j1
if(jj.lt.0)then
ii=1
else if(jj.ge.srec-1)then
ii=1
end if
end if
if(ii.ne.0)then
mlin(1:srec)='wput_1'
call mput(1,1,0,0,0)
end if
i1=stib(wera(0)+aind)
i2=stib(werb(0)+aind)
ll=i2-i1
ii=0
if(i1.le.0)then
ii=1
else if(i1.ge.stcbs(1))then
ii=1
else if(ll.lt.0)then
ii=1
else if(ll.ge.srec)then
ii=1
end if
if(ii.ne.0)then
mlin(1:srec)='wput_2'
call mput(1,1,0,0,0)
end if
ll=ll+1
if(j1.ne.0)then
jj=(ll-j1)+(j2+1)
else
jj=0
end if
kk=srec-6
if(jj.gt.0)then
if(jj.lt.srec)then
mlin(1:srec)=stcb(i1:i2)//stcb(j1:j2)
else if(ll.le.kk)then
mlin(1:srec)=stcb(i1:i2)//stcb(j1:j1+(kk-ll))//' ...'
else
mlin(1:srec)=stcb(i1:i1+kk)//' ...'
end if
else
if(ll.lt.srec)then
mlin(1:srec)=stcb(i1:i2)
else
mlin(1:srec)=stcb(i1:i1+kk)//' ...'
end if
end if
if(sind.gt.0)then
call mput(0,0,0,0,0)
else
call mput(1,1,0,0,0)
end if
90 return
end subroutine wput
subroutine ccyc(xx,situ)
use,non_intrinsic::qmix
use,non_intrinsic::qproc
use,non_intrinsic::wbuffer
implicit none
save
integer,intent(in)::xx
integer,intent(out)::situ
integer::i1,i2,i3,j1,j2,j3,jj,k1,k2,kk
integer::bb(1:maxli),cc(1:maxli)
if(xx.eq.1)then
if(nloop-1.le.0)then
situ=1
go to 90
end if
else
mlin(1:srec)='ccyc_2'
call mput(1,1,0,0,0)
end if
j1=0
do i1=rhop1,nli
if(flow(i1,0).lt.0)then
j1=j1+1
bb(j1)=j1
cc(j1)=i1
end if
end do
jj=1-net(-3)
if(jj.lt.0)then
k1=1
k2=net(-3)
d01: do i1=1,net(-3)
j1=cc(i1)
do i2=i1+1,net(-3)
if(bb(i1).ne.bb(i2))then
kk=0
j2=cc(i2)
d02: do i3=rhop1,rhop2
if(flow(j1,i3).ne.0)then
if(flow(j2,i3).ne.0)then
kk=1
exit d02
end if
end if
end do d02
if(kk.ne.0)then
if(bb(k1).eq.1)then
k1=k1+1
end if
if(bb(k2).eq.1)then
k2=k2-1
end if
j2=min(bb(i1),bb(i2))
j3=max(bb(i1),bb(i2))
do i3=k1,k2
if(bb(i3).eq.j3)then
bb(i3)=j2
end if
end do
jj=jj+1
if(jj.eq.0)then
exit d01
end if
end if
end if
end do
end do d01
end if
if(jj.lt.0)then
situ=-1
else
situ=1
end if
90 return
end subroutine ccyc
subroutine smodel(ifile,nlin1,nlin2,slin,jxa,jxb)
use,non_intrinsic::cbuffer
use,non_intrinsic::ikeys
use,non_intrinsic::qfiles
use,non_intrinsic::qintr
use,non_intrinsic::qmodel
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer,intent(in)::ifile
integer,intent(inout)::nlin1,nlin2
integer,intent(out)::jxa,jxb,slin
integer,external::stdz
integer,parameter::llof(0:5)=[5,4,6,8,12,5]
integer,parameter::ffj(1:4,1:6)=reshape([&
0,0,llof(3),3,llof(3)+2,6,&
llof(1),3,0,0,0,0,&
llof(1),3,llof(3),12,llof(3)+2,6,&
llof(1),21,llof(3),-18,llof(3)+2,6]&
,[4,6],order=[2,1])
integer::msc(1:dekm%ubo),mspec(1:dekm%ubo),mz(1:dekm%ubo,0:2)
integer::mcco(1:mll0),styp0(0:2)
integer::styp,sstyp,mz0
integer::newc,qc,njb,ic1
integer::i1,i2,j1,j2,j3,j4,j5,j6,j7
integer::ii,jj,p0,p1,p2,q0,q1,q2
integer::nsekt,sekt0,sekty1,sekty0,sl1
integer::sekts(0:0),sekta(0:0),sektb(0:0)
integer::ncons,nsmod,tsm,smod0,smodn(0:0),smodpq(0:0),smodc(0:0)
njb=nstjb
if(nlin2.eq.0)then
styp=0
styp0(0:2)=[0,dekm%smp,dekm%skp]
nsekt=0
sekt0=0
sekty0=0
sekty1=0
sl1=0
ncons=0
nsmod=0
tsm=0
smod0=0
smodn(0)=nap
smodpq(0)=nap
smodc(0)=nap
sekts(0)=nap
sekta(0)=nap
sektb(0)=nap
mcco(:)=0
mll(:)=nap
mllt(:)=nap
msc(:)=0
msc(dekm%smcc)=1
msc(dekm%mccc)=2
msc(dekm%mcge)=3
msc(dekm%mcs)=4
msc(dekm%mfcc)=2
msc(dekm%mfss)=4
msc(dekm%mce)=3
msc(dekm%mfs)=4
msc(dekm%mp)=5
msc(dekm%mv)=5
mspec(:)=-1
mspec(dekm%mcge)=0
mspec(dekm%mp)=0
mspec(dekm%mv)=0
mspec(dekm%smcc)=mcct%smcc
mspec(dekm%skcc)=mcct%skcc
mspec(dekm%mccc)=mcct%mccc
mspec(dekm%mfcc)=mcct%mfcc
mz0=0
mz(:,:)=0
mz(dekm%smcc,0)=1
mz(dekm%skcc,0)=2
mz(dekm%mccc,0)=3
mz(dekm%mcge,0)=3
mz(dekm%mcs,0)=3
mz(dekm%mfcc,0)=4
mz(dekm%mfss,0)=4
mz(dekm%smp,0)=5
mz(dekm%skp,0)=6
mz(dekm%mp,0)=7
mz(dekm%mv,0)=8
mz(dekm%ski,1)=1
mz(dekm%mce,1)=2
mz(dekm%smq,1)=3
mz(dekm%mfs,2)=1
mz(dekm%mp,2)=2
mz(dekm%mv,2)=2
mz(dekm%skq,2)=3
newc=1
end if
70 continue
if(jflag(11).ne.0)then
j1=mll(mlty%mcf)
if(j1.ne.nap)then
j2=0
do while(stib(j1).ne.eoa)
j1=stib(j1)
j2=j2+1
if(stib(smodc(0)+j2).ne.0)then
cycle
end if
if(stib(j1+llof(1)).eq.nap)then
j3=j1+1
j4=stib(j3)+(stib(j3+1)-1)
j5=j1+llof(2)
j6=stib(j5+1)
j5=stib(j5)
if(j5.eq.nap)then
call emodel(30,1,1,0,0,ifile)
end if
j7=(j4-stib(j3))+(j6-j5)+5
jxa=stcbs(1)+1
jxb=jxa+j7
call aocb(j7+1)
stib(smodc(0)+j2)=1
stcb(jxa:stcbs(1))='['//stcb(stib(j3):j4)//'='&
//stcb(j5:j6)//']'//achar(alf)
jflag(11)=-1
newc=1
go to 71
end if
end do
jflag(11)=0
end if
end if
slin=0
call qrr(ifile,nlin2,slin,qc)
if(slin.lt.0)then
if(newc.eq.0)then
go to 521
end if
go to 700
end if
if(newc.ne.0)then
nlin1=nlin2
jxa=stcbs(1)+1
end if
if((slin.eq.0).or.&
(qc.ne.0))then
if(newc.ne.0)then
go to 70
else
go to 521
end if
end if
jxb=stcbs(1)+slin
if(iachar(stcb(jxb+1:jxb+1)).ne.alf)then
go to 521
else if(iachar(stcb(jxb:jxb)).eq.aspc)then
go to 521
end if
71 continue
if(jxb-jxa.gt.sxbuff-1)then
mlin(1:srec)="statement too long,"
call mput(1,1,nlin1,nlin2,ifile)
end if
if(jflag(11).lt.0)then
go to 90
end if
call aocb(slin+1)
if(iachar(stcb(jxb:jxb)).ne.arsbr)then
newc=0
stcb(jxb+1:jxb+1)=achar(aspc)
go to 70
end if
newc=1
jxb=jxb+1
if(njb.lt.nstjb)then
call aojb(-1)
end if
if(styp.eq.dekm%smq)then
styp=dekm%smp
else if(styp.eq.dekm%skq)then
styp=dekm%skp
end if
styp0(mz0)=max(styp0(mz0),styp)
call stap(jxa,jxb,qfty%mtyp)
ii=0
if(stapi(1).le.0)then
go to 110
else if(stapi(1).gt.size(msc))then
ii=1
else if(stapi(3).lt.0)then
ii=1
else if(stapi(4).lt.0)then
ii=1
end if
if(ii.ne.0)then
mlin(1:srec)='smodel_1'
call mput(1,1,0,0,0)
end if
styp=stapi(1)
sstyp=stapi(2)
if((smod0.eq.0).and.&
(sekt0.eq.0))then
mz0=0
if(styp.eq.dekm%mfs)then
styp=dekm%mcs
end if
else if(sekt0.eq.0)then
mz0=1
if(styp.eq.dekm%mcge)then
styp=dekm%mce
end if
else if(smod0.eq.0)then
mz0=2
if(styp.eq.dekm%smq)then
styp=dekm%skq
end if
else
go to 110
end if
ii=0
if(mz(styp,mz0).eq.0)then
ii=1
end if
if(styp0(0).eq.0)then
if(mspec(styp).eq.0)then
jflag(12)=0
else
jflag(12)=1
end if
else
if(mz(styp,mz0).lt.mz(styp0(mz0),mz0))then
if(styp.ne.dekm%skp)then
ii=1
end if
end if
if(jflag(12).eq.0)then
if(mspec(styp).ne.0)then
ii=1
end if
end if
end if
if(ii.ne.0)then
go to 110
end if
if((styp.le.dekm%smp).or.&
(styp.gt.dekm%smq))then
if(smod0.ne.0)then
ii=1
end if
else
if(smod0.eq.0)then
ii=1
end if
end if
if(mz0.eq.1)then
if(styp0(mz0).eq.dekm%smp)then
if(styp.ne.dekm%ski)then
ii=1
end if
end if
end if
if((styp.le.dekm%skp).or.&
(styp.gt.dekm%skq))then
if(sekt0.ne.0)then
ii=1
else if(sekty0.ne.0)then
ii=1
end if
end if
if(ii.ne.0)then
go to 110
end if
if(smodc(0).eq.nap)then
if(nsmod.gt.0)then
if(mll(mlty%mcf).ne.nap)then
if(styp.gt.dekm%mcs)then
j4=mll(mlty%mcf)
do while(stib(j4).ne.eoa)
j4=stib(j4)
ncons=ncons+1
end do
smodc(0)=stibs(1)
ii=ncons+1
call aoib(ii)
stib(stibs(1))=eoa
end if
end if
end if
end if
if(sekts(0).eq.nap)then
if(nsekt.gt.0)then
if(styp.gt.dekm%skcc)then
sekts(0)=stibs(1)
ii=nsekt+1
call aoib(2*ii)
sektb(0)=stibs(1)-ii
if(nsmod.eq.0)then
stib(sekts(0)+1:sektb(0)-1)=1
stib(sektb(0)+1:stibs(1)-1)=0
else
sekta(0)=stibs(1)
call aoib(ii)
stib(sekts(0)+1:stibs(1)-1)=0
stib(sekta(0))=eoa
end if
stib(sektb(0))=eoa
stib(stibs(1))=eoa
end if
end if
end if
jj=mspec(styp)
if(jj.gt.0)then
mcco(jj)=1
end if
p0=stjb(stjbs-2)
p1=p0+2
p2=p1+3*(stjb(p0+1)-1)
if((styp.eq.dekm%skp).or.&
(styp.eq.dekm%skq))then
ii=0
if(mll(mlty%msk).eq.nap)then
ii=1
else if(styp.eq.dekm%skq)then
if(sekt0.eq.0)then
ii=1
else if(sekty0.eq.0)then
ii=1
end if
jj=0
if(styp0(mz0).eq.dekm%mfs)then
jj=1
else if(styp0(mz0).eq.dekm%skp)then
jj=1
end if
if(jj.ne.0)then
call emodel(23,1,1,sl1,nlin2,ifile)
end if
end if
if(ii.ne.0)then
go to 110
end if
q0=p0+9
j1=stjb(q0)
j2=stjb(q0+1)
j3=0
j4=mll(mlty%msk)
do while(stib(j4).ne.eoa)
j4=stib(j4)
j5=stib(j4+1)
j6=j5+(stib(j4+2)-1)
j3=j3-1
if(j6-j5.eq.j2-j1)then
if(stcb(j1:j2).eq.stcb(j5:j6))then
j3=-j3
exit
end if
end if
end do
if(j3.lt.1)then
call emodel(21,1,1,nlin1,nlin2,ifile)
end if
if(styp.eq.dekm%skp)then
if(stib(sektb(0)+j3).ne.0)then
call emodel(20,1,1,nlin1,nlin2,ifile)
end if
stib(sektb(0)+j3)=1
sekt0=j3
sekty0=stib(j4+3)
sl1=nlin1
if(sekty0.lt.sekty1)then
go to 110
end if
sekty1=sekty0
else
if(j3.ne.sekt0)then
go to 110
end if
styp0(0)=dekm%skp
styp0(mz0)=dekm%skp
sekt0=0
sl1=0
sekty0=0
do i1=1,mll4
j4=mll(i1)
if(j4.eq.nap)then
cycle
else if(i1.eq.mlty%mcf)then
cycle
end if
do while(stib(j4).ne.eoa)
j4=stib(j4)
j3=j4+llof(1)
stib(j3:j3+1)=nap
if(i1.eq.mlty%mff)then
j3=j4+llof(3)
stib(j3:j3+3)=nap
end if
end do
end do
end if
else if(styp.eq.dekm%smcc)then
if(stapi(3).lt.1)then
go to 110
end if
if(cflag(9).eq.0)then
call emodel(28,1,1,0,0,1)
end if
nsmod=stapi(3)
ii=nsmod+1
smodn(0)=stibs(1)
call aoib(3*ii-1)
smodpq(0)=smodn(0)+ii
stib(smodn(0)+1:smodpq(0)-1)=0
stib(smodpq(0)+1:stibs(1)-1)=nap
stib(smodpq(0))=eoa
stib(stibs(1))=eoa
j1=smodpq(0)
q0=p0+6
do i1=1,nsmod
q0=q0+6
j1=j1+1
j3=stjb(q0)
stib(j1)=j3
j1=j1+1
j4=stjb(q0+1)
stib(j1)=j4
j6=smodpq(0)
do i2=1,i1-1
j5=j6+1
j6=j5+1
if(j4-j3.eq.stib(j6)-stib(j5))then
if(stcb(j3:j4).eq.stcb(stib(j5):stib(j6)))then
call emodel(24,1,1,nlin1,nlin2,ifile)
end if
end if
end do
if(tsm.eq.0)then
if(j4-j3.eq.smodq(0)-smodp(0))then
if(stcb(j3:j4).eq.stcb(smodp(0):smodq(0)))then
tsm=i1
end if
end if
end if
end do
if(tsm.eq.0)then
call emodel(27,1,1,0,0,ifile)
end if
else if(styp.eq.dekm%skcc)then
if(stapi(3).lt.1)then
go to 110
end if
q1=mlty%msk
if(mll(q1).eq.nap)then
mll(q1)=stibs(1)+1
call aoib(2)
stib(stibs(1))=eoa
mllt(q1)=mll(q1)
stib(mllt(q1))=eoa
else
j4=stib(mll(q1))
if(stib(j4+3).eq.sstyp)then
go to 110
end if
j4=mllt(q1)
if(stib(j4+3).eq.sstyp)then
go to 110
end if
end if
q0=p0+6
do i1=1,stapi(3)
q0=q0+6
j1=stjb(q0)
j2=stjb(q0+1)
if(nsekt.gt.0)then
call mstr1(stcb,j1,j2,mll(q1),1,2,jj)
if(jj.ne.0)then
call emodel(20,1,1,nlin1,nlin2,ifile)
end if
end if
j4=smodpq(0)
do i2=1,nsmod
j3=j4+1
j4=j3+1
if(j2-j1.eq.stib(j4)-stib(j3))then
if(stcb(j1:j2).eq.stcb(stib(j3):stib(j4)))then
call emodel(33,1,1,nlin1,nlin2,ifile)
end if
end if
end do
nsekt=nsekt+1
j3=stibs(1)+1
ii=llof(5)
call aoib(ii)
stib(stibs(1))=eoa
stib(mllt(q1))=j3
stib(j3)=eoa
stib(j3+1)=j1
stib(j3+2)=j2-(j1-1)
stib(j3+3)=sstyp
stib(stibs(1))=eoa
mllt(q1)=j3
end do
else if((styp.eq.dekm%smp).or.&
(styp.eq.dekm%smq))then
ii=0
if(nsmod.eq.0)then
ii=1
else if(nsekt.eq.0)then
ii=1
else if(stapi(3).ne.1)then
ii=1
else if((smodc(0).eq.nap).and.&
(mll(mlty%mcf).ne.nap))then
ii=1
end if
if(ii.ne.0)then
go to 110
end if
if(styp.eq.dekm%smq)then
if(styp0(mz0).eq.dekm%smp)then
call emodel(26,1,1,sl1,nlin2,ifile)
end if
if(smodc(0).ne.nap)then
j3=0
j4=mll(mlty%mcf)
do while(stib(j4).ne.eoa)
j4=stib(j4)
j3=j3+1
if(stib(smodc(0)+j3).lt.0)then
stib(j4+llof(1):j4+llof(1)+1)=nap
end if
end do
end if
else
if(smodc(0).ne.nap)then
stib(smodc(0)+1:smodc(0)+ncons)=0
j3=0
j4=mll(mlty%mcf)
do while(stib(j4).ne.eoa)
j4=stib(j4)
j3=j3+1
if(stib(j4+llof(1)).ne.nap)then
stib(smodc(0)+j3)=1
end if
end do
end if
sl1=nlin1
end if
q0=p0+9
j1=stjb(q0)
j2=stjb(q0+1)
ii=0
j4=smodpq(0)
do i1=1,nsmod
j3=j4+1
j4=j3+1
if(j2-j1.eq.stib(j4)-stib(j3))then
if(stcb(stib(j3):stib(j4)).eq.stcb(j1:j2))then
ii=i1
exit
end if
end if
end do
if(styp.eq.dekm%smp)then
if(ii.eq.0)then
call emodel(25,1,1,nlin1,nlin2,ifile)
else if(stib(smodn(0)+ii).ne.0)then
call emodel(24,1,1,nlin1,nlin2,ifile)
end if
smod0=ii
else
if(ii.ne.smod0)then
go to 110
end if
stib(smodn(0)+smod0)=1
if((smod0.eq.tsm).and.&
(mll(mlty%mcf).ne.nap))then
jflag(11)=1
else
jflag(11)=0
end if
smod0=0
sl1=0
styp0(0)=dekm%smp
styp0(mz0)=dekm%smp
end if
else if(styp.eq.dekm%ski)then
if(nsmod.eq.0)then
ii=1
else if(styp0(mz0).ne.dekm%smp)then
ii=1
else if(sekta(0).eq.nap)then
ii=1
end if
if(ii.ne.0)then
go to 110
end if
stib(sekta(0)+1:sekta(0)+nsekt)=sstyp
q0=p0+6
do i1=1,stapi(3)
q0=q0+6
j1=stjb(q0)
j2=stjb(q0+1)
j4=mll(mlty%msk)
j3=0
d03: do while(stib(j4).ne.eoa)
j4=stib(j4)
j5=stib(j4+1)
j6=j5+(stib(j4+2)-1)
j3=j3-1
if(j6-j5.eq.j2-j1)then
if(stcb(j1:j2).eq.stcb(j5:j6))then
j3=-j3
if(stib(sekta(0)+j3).ne.sstyp)then
call emodel(20,1,1,nlin1,nlin2,ifile)
end if
stib(sekta(0)+j3)=1-sstyp
exit d03
end if
end if
end do d03
if(j3.lt.1)then
call emodel(21,1,1,nlin1,nlin2,ifile)
end if
end do
if(smod0.eq.tsm)then
do i1=1,nsekt
if(stib(sekta(0)+i1).eq.1)then
stib(sekts(0)+i1)=1
end if
end do
else
do i1=1,nsekt
if(stib(sekta(0)+i1).eq.1)then
if(stib(sekts(0)+i1).eq.0)then
stib(sekts(0)+i1)=-1
end if
end if
end do
end if
else if(msc(styp).eq.2)then
if(stapi(3).lt.1)then
go to 110
end if
q1=abs(sstyp)
ii=0
if(q1.eq.0)then
ii=1
else if(q1.gt.mll4)then
ii=1
end if
if(ii.ne.0)then
mlin(1:srec)='smodel_4'
call mput(1,1,0,0,0)
end if
if(sstyp.lt.0)then
q0=p0+9
j2=stty%z
else
q0=p0+6
j2=0
end if
if(mll(q1).eq.nap)then
mll(q1)=stibs(1)+1
call aoib(2)
stib(stibs(1))=eoa
mllt(q1)=mll(q1)
stib(mllt(q1))=eoa
else
j4=stib(mll(q1))
if(stib(j4+3).eq.j2)then
go to 110
end if
j4=mllt(q1)
if(stib(j4+3).eq.j2)then
go to 110
end if
end if
do i1=1,stapi(3)
q0=q0+6
call kbp(stjb(q0),stjb(q0+1),j3,j4,jj)
if(j4.ne.nap)then
call emodel(9,1,1,nlin1,nlin2,ifile)
end if
if(q1.ne.mlty%mff)then
ii=llof(0)+llof(1)
else
ii=llof(0)+llof(1)+llof(3)
end if
j3=stibs(1)+1
call aoib(ii)
stib(mllt(q1))=j3
mllt(q1)=j3
stib(j3)=eoa
stib(j3+1)=stjb(q0)
stib(j3+2)=stjb(q0+1)-(stjb(q0)-1)
stib(j3+3)=j2
stib(j3+llof(1):stibs(1)-1)=nap
stib(stibs(1))=eoa
end do
else if(msc(styp).eq.3)then
q1=mlty%mcf
if(mll(q1).eq.nap)then
mll(q1)=stibs(1)+1
call aoib(2)
stib(stibs(1))=eoa
mllt(q1)=mll(q1)
stib(mllt(q1))=eoa
end if
q0=p0+6
j1=stjb(q0)
j2=stjb(q0+1)
call kbp(j1,j2,j3,j4,jj)
if(jflag(12).eq.0)then
if(styp.ne.dekm%mcge)then
go to 110
else if(j4.ne.nap)then
go to 110
end if
j4=stibs(1)+1
ii=llof(0)+llof(1)
call aoib(ii)
stib(mllt(q1))=j4
stib(j4)=eoa
stib(j4+1)=j1
stib(j4+2)=j2
stib(j4+3)=0
stib(j4+llof(1):stibs(1)-1)=nap
stib(stibs(1))=eoa
mllt(q1)=j4
else
if(mcco(mspec(dekm%mccc)).eq.0)then
call emodel(31,1,1,nlin1,nlin2,ifile)
else if(j4.eq.nap)then
go to 110
else if(jj.lt.1)then
go to 110
else if(j3.ne.q1)then
go to 110
end if
if(smod0.eq.0)then
if(stib(j4+llof(2)).ne.nap)then
go to 110
end if
end if
if(smodc(0).ne.nap)then
if(stib(smodc(0)+jj).ne.0)then
go to 110
end if
stib(smodc(0)+jj)=-1
end if
end if
q0=q0+6
j5=stjb(q0)
j6=stjb(q0+1)
if(stib(j4+3).eq.stty%z)then
if(iachar(stcb(j5:j5)).eq.asq)then
j5=j5+1
j6=j6-1
end if
ii=1
if(j5.gt.j6)then
ii=0
else
ii=stdz(stcb,j5,j6)
end if
if(ii.eq.0)then
call emodel(32,1,1,nlin1,nlin2,ifile)
end if
end if
if((smod0.eq.0).or.&
(smod0.eq.tsm))then
j3=j4+llof(1)
if(stib(j3).ne.nap)then
go to 110
end if
stib(j3)=j5
stib(j3+1)=j6
go to 90
end if
else if(msc(styp).eq.4)then
if(sstyp.ne.0)then
mlin(1:srec)='smodel_2'
call mput(1,1,0,0,0)
end if
q0=p0+9
j1=stjb(q0)
j2=stjb(q0+1)
call kbp(j1,j2,j3,j4,jj)
if(j4.eq.nap)then
call emodel(11,1,1,nlin1,nlin2,ifile)
end if
ii=0
if((j3.lt.0).or.&
(j3.gt.mll4))then
ii=1
end if
if(sekt0.ne.0)then
if(sekty0.eq.mlty%mpf)then
if(j3.eq.mlty%mvf)then
ii=1
end if
else if(sekty0.eq.mlty%mvf)then
if(j3.ne.mlty%mvf)then
ii=1
end if
else
ii=1
end if
end if
if(ii.ne.0)then
go to 110
end if
if(j3.eq.mlty%mcf)then
if(mcco(mspec(dekm%mccc)).eq.0)then
call emodel(31,1,1,nlin1,nlin2,ifile)
end if
ii=0
if(nsmod.eq.0)then
ii=1
else if(stapi(4).ne.0)then
ii=1
end if
if(stib(j4+llof(1)).ne.nap)then
ii=1
else if(stib(j4+llof(2)).ne.nap)then
ii=1
end if
if(ii.ne.0)then
go to 110
end if
j5=j4+llof(2)
j7=q0+6
stib(j5)=stjb(j7)
stib(j5+1)=stjb(j7+1)
if(stib(j4+3).eq.stty%z)then
j1=stib(j5)
j2=stib(j5+1)
if(iachar(stcb(j1:j1)).eq.asq)then
j1=j1+1
j2=j2-1
end if
ii=1
if(j1.gt.j2)then
ii=0
else
ii=stdz(stcb,j1,j2)
end if
if(ii.eq.0)then
call emodel(32,1,1,nlin1,nlin2,ifile)
end if
end if
go to 70
end if
if(mcco(mspec(dekm%mfcc)).eq.0)then
call emodel(31,1,1,nlin1,nlin2,ifile)
end if
ii=0
q0=q0+5
if(j3.ne.mlty%mff)then
if(stjb(q0).eq.stty%p)then
ii=1
end if
else
if(stjb(q0).ne.stty%p)then
ii=1
end if
end if
if(ii.ne.0)then
call emodel(19,1,1,nlin1,nlin2,ifile)
end if
j7=q0+1
if(j3.ne.mlty%mff)then
if(sekt0.eq.0)then
j5=j4+llof(2)
else
j5=j4+llof(1)
end if
if(stib(j5).ne.nap)then
go to 110
end if
stib(j5)=stjb(j7)
stib(j5+1)=stjb(j7+1)
if(stib(j4+3).eq.stty%z)then
j1=stib(j5)
j2=stib(j5+1)
if(iachar(stcb(j1:j1)).eq.asq)then
j1=j1+1
j2=j2-1
end if
ii=1
if(j1.gt.j2)then
ii=0
else
ii=stdz(stcb,j1,j2)
end if
if(ii.eq.0)then
call emodel(32,1,1,nlin1,nlin2,ifile)
end if
end if
else
do i2=1,size(ffj,dim=2),2
j6=ffj(stapi(4),i2)
if(j6.gt.0)then
j5=j4+j6
if(sekt0.eq.0)then
if(i2.eq.1)then
j5=j5+2
else
j5=j5+4
end if
end if
if(stib(j5).ne.nap)then
go to 110
end if
j7=j7+ffj(stapi(4),i2+1)
stib(j5)=stjb(j7)
stib(j5+1)=stjb(j7+1)
if(stib(j4+3).eq.stty%z)then
j1=stib(j5)
j2=stib(j5+1)
if(iachar(stcb(j1:j1)).eq.asq)then
j1=j1+1
j2=j2-1
end if
ii=1
if(j1.gt.j2)then
ii=0
else
ii=stdz(stcb,j1,j2)
end if
if(ii.eq.0)then
call emodel(32,1,1,nlin1,nlin2,ifile)
end if
end if
end if
end do
end if
else if(msc(styp).eq.5)then
ii=0
if(sekt0.ne.0)then
if(sekty0.eq.mlty%mpf)then
if(styp.eq.dekm%mv)then
ii=1
else if(sstyp.eq.mlty%mvf)then
ii=1
end if
else if(sekty0.eq.mlty%mvf)then
if(styp.eq.dekm%mp)then
ii=1
else if(sstyp.eq.mlty%mpf)then
ii=1
end if
end if
end if
if(styp.eq.dekm%mp)then
if(sekty1.eq.mlty%mvf)then
ii=1
end if
sekty1=mlty%mpf
else if(styp.eq.dekm%mv)then
sekty1=mlty%mvf
end if
if(ii.ne.0)then
go to 110
end if
if(sekt0.gt.0)then
if(stib(sekts(0)+sekt0).le.0)then
go to 70
end if
end if
if(stapi(4).eq.0)then
ic1=ascol
else
ic1=acomma
end if
do i1=1,mll4
j1=mll(i1)
if(j1.eq.nap)then
cycle
else if(i1.eq.mlty%mcf)then
cycle
else if(i1.eq.mlty%mff)then
if(styp.ne.dekm%mp)then
cycle
end if
else if(i1.eq.mlty%mpf)then
if(styp.ne.dekm%mp)then
cycle
end if
else if(i1.eq.mlty%mvf)then
if(styp.ne.dekm%mv)then
cycle
end if
end if
q0=p1+3+6*stapi(3)
d02: do while(stib(j1).ne.eoa)
j1=stib(j1)
j7=j1+1
j5=stib(j7)
j7=stib(j7+1)-1
j6=j5+j7
do i2=q0,p2-6,3
if(stjb(i2).eq.stty%p)then
if(stjb(i2+1).eq.aeq)then
j3=i2-1
j4=stjb(j3)
j3=stjb(j3-1)
if(j7.eq.j4-j3)then
if(stcb(j5:j6).eq.stcb(j3:j4))then
cycle d02
end if
end if
end if
end if
end do
if(i1.ne.mlty%mff)then
if(stib(j1+llof(1)).ne.nap)then
q1=j1+llof(1)
else if(stib(j1+llof(2)).ne.nap)then
q1=j1+llof(2)
else
call emodel(11,1,1,nlin1,nlin2,ifile)
end if
j7=(j6-j5)+(stib(q1+1)-stib(q1))+4
call aocb(j7)
stcb(jxb-1:jxb+j7)=achar(ic1)//stcb(j5:j6)//'='&
//stcb(stib(q1):stib(q1+1))//']'//achar(alf)
jxb=jxb+j7
slin=slin+j7
else
j7=0
q1=p0+6
q2=q1+6
if(stjb(q2+1)-stjb(q2).eq.stjb(q1+1)-stjb(q1))then
if(stcb(stjb(q1):stjb(q1+1)).eq.&
stcb(stjb(q2):stjb(q2+1)))then
j7=1
end if
end if
if(j7.ne.0)then
if(stib(j1+llof(1)).ne.nap)then
q1=j1+llof(1)
else if(stib(j1+llof(2)).ne.nap)then
q1=j1+llof(2)
else
call emodel(11,1,1,nlin1,nlin2,ifile)
end if
j7=(j6-j5)+(stib(q1+1)-stib(q1))+6
call aocb(j7)
stcb(jxb-1:jxb+j7)=achar(ic1)//stcb(j5:j6)//'=('&
//stcb(stib(q1):stib(q1+1))//')]'//achar(alf)
jxb=jxb+j7
slin=slin+j7
else
if(stib(j1+llof(3)).ne.nap)then
q1=j1+llof(3)
else if(stib(j1+llof(4)).ne.nap)then
q1=j1+llof(4)
else
call emodel(11,1,1,nlin1,nlin2,ifile)
end if
if(stib(q1+2).eq.nap)then
call emodel(11,1,1,nlin1,nlin2,ifile)
end if
j7=(j6-j5)+(stib(q1+1)-stib(q1))+(stib(q1+3)-stib(q1+2))+8
call aocb(j7)
stcb(jxb-1:jxb+j7)=achar(ic1)//stcb(j5:j6)//&
'=('//stcb(stib(q1):stib(q1+1))//','//&
stcb(stib(q1+2):stib(q1+3))//')]'//achar(alf)
jxb=jxb+j7
slin=slin+j7
end if
end if
ic1=acomma
end do d02
end do
go to 90
end if
go to 70
521 continue
mlin(1:srec)="wrong syntax,"
call mput(1,1,nlin1,nlin2,ifile)
110 continue
call emodel(29,1,1,nlin1,nlin2,ifile)
700 continue
if(smod0.ne.0)then
call emodel(26,1,1,sl1,nlin2,ifile)
end if
if(tsm.eq.0)then
if(cflag(9).ne.0)then
call emodel(27,1,1,0,0,ifile)
end if
end if
do i1=1,nsmod
if(stib(smodn(0)+i1).eq.0)then
call emodel(25,1,1,0,0,ifile)
end if
end do
if(sekt0.ne.0)then
call emodel(23,1,1,sl1,nlin2,ifile)
end if
if(nsmod.ne.0)then
do i1=1,nsekt
if(stib(sektb(0)+i1).eq.0)then
call emodel(23,1,1,0,0,ifile)
else if(stib(sekts(0)+i1).eq.0)then
call emodel(22,1,1,0,0,ifile)
end if
end do
end if
90 continue
if(njb.lt.nstjb)then
call aojb(-1)
end if
if(nstjb.ne.njb)then
mlin(1:srec)='smodel_3'
call mput(1,1,0,0,0)
end if
if(slin.gt.0)then
slin=jxb-jxa
end if
return
end subroutine smodel
subroutine model
use,non_intrinsic::cbuffer
use,non_intrinsic::ikeys
use,non_intrinsic::qfiles
use,non_intrinsic::qintr
use,non_intrinsic::qmix
use,non_intrinsic::qmodel
use,non_intrinsic::qsty
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer,external::stdq,stdw,stdz,stoz
integer::ii,ij,i1,i2,id1,id2,idin,is1,is2,ix,jj,j1,j5,j6
integer::ixa,ixb,ifile,njb
integer::dbl,fpass,ibot,ic,itop,kid,kin,kk,knf
integer::level,mpal,nest,quote,slin,vel,vin
integer::newid,nlin,off1,off2,pal,plm,psize,xfile,qmod
integer::llk(1:3),llkt(1:3),llp(2:3),llpt(2:3)
integer::apmkd(0:0),avmkd(0:0)
integer::sucpal(0:11,0:11),acf0(0:127)
njb=nstjb
acf0(0:31)=-1
acf0(32:126)=0
acf0(127)=-1
acf0(alf)=1
acf0(aspc)=2
acf0(adq)=3
acf0(asq)=4
acf0(iachar('('))=5
acf0(iachar(')'))=6
acf0(iachar(','))=7
acf0(iachar(';'))=8
acf0(aeq)=9
acf0(iachar('['))=10
acf0(iachar(']'))=11
sucpal(:,:)=-1
sucpal(0,2)=0
sucpal(0,10)=1
sucpal(1,0)=1
sucpal(1,1)=1
sucpal(1,2)=1
sucpal(1,7)=1
sucpal(1,8)=2
sucpal(1,9)=3
sucpal(1,11)=10
sucpal(2,0)=2
sucpal(2,1)=2
sucpal(2,2)=2
sucpal(2,9)=3
sucpal(3,0)=3
sucpal(3,1)=3
sucpal(3,2)=3
sucpal(3,3)=-1
sucpal(3,4)=5
sucpal(3,5)=6
sucpal(3,7)=2
sucpal(3,11)=10
sucpal(4,0)=4
sucpal(4,2)=4
sucpal(4,3)=3
sucpal(4,4)=4
sucpal(4,5)=4
sucpal(4,6)=4
sucpal(4,7)=4
sucpal(4,8)=4
sucpal(4,9)=4
sucpal(4,10)=4
sucpal(4,11)=4
sucpal(5,0)=5
sucpal(5,2)=5
sucpal(5,3)=5
sucpal(5,4)=3
sucpal(5,5)=5
sucpal(5,6)=5
sucpal(5,7)=5
sucpal(5,8)=5
sucpal(5,9)=5
sucpal(5,10)=5
sucpal(5,11)=5
sucpal(6,0)=6
sucpal(6,1)=6
sucpal(6,2)=6
sucpal(6,3)=-1
sucpal(6,4)=8
sucpal(6,6)=9
sucpal(6,7)=6
sucpal(7,0)=7
sucpal(7,2)=7
sucpal(7,3)=6
sucpal(7,4)=7
sucpal(7,5)=7
sucpal(7,6)=7
sucpal(7,7)=7
sucpal(7,8)=7
sucpal(7,9)=7
sucpal(7,10)=7
sucpal(7,11)=7
sucpal(8,0)=8
sucpal(8,2)=8
sucpal(8,3)=8
sucpal(8,4)=6
sucpal(8,5)=8
sucpal(8,6)=8
sucpal(8,7)=8
sucpal(8,8)=8
sucpal(8,9)=8
sucpal(8,10)=8
sucpal(8,11)=8
sucpal(9,1)=9
sucpal(9,2)=9
sucpal(9,7)=2
sucpal(9,11)=10
sucpal(10,1)=11
if(filp(3).ne.nap)then
xfile=0
else
xfile=1
end if
05 continue
xfile=xfile+1
if(xfile.eq.1)then
do i1=1,nfiles
if(filt(i1).eq.qfty%ltyp)then
if(filp(i1).eq.nap)then
go to 05
end if
call qopen(i1)
ifile=i1
exit
end if
end do
else if(xfile.eq.2)then
do i1=1,nfiles
if(filt(i1).eq.qfty%mtyp)then
call qopen(i1)
ifile=i1
exit
end if
end do
end if
nlin=0
dbl=0
level=1
qmod=0
if(xfile.ne.1)then
llk(:)=nap
llkt(:)=nap
llp(:)=nap
llpt(:)=nap
nrho=0
end if
70 continue
if(njb.lt.nstjb)then
call aojb(-1)
end if
if(xfile.eq.1)then
go to 05
else if(xfile.eq.2)then
call smodel(ifile,dbl,nlin,slin,ixa,ixb)
end if
if(slin.eq.0)then
go to 521
else if(slin.lt.0)then
go to 777
end if
ibot=ixa
itop=ixa+(slin-1)
do i1=ibot,itop
if(iachar(stcb(i1:i1)).ne.aspc)then
ibot=i1
exit
end if
end do
ii=0
nest=0
quote=0
do i1=ibot,itop
jj=iachar(stcb(i1:i1))
if(jj.eq.asq)then
if(nest.ne.1)then
go to 521
end if
if(quote.eq.0)then
quote=asq
end if
if(quote.eq.asq)then
ii=1-ii
end if
else if(jj.eq.adq)then
if(nest.ne.1)then
go to 521
end if
if(quote.eq.0)then
quote=adq
end if
if(quote.eq.adq)then
ii=1-ii
end if
else if(ii.eq.0)then
if(stcb(i1:i1).eq.'[')then
if(nest.ne.0)then
go to 521
end if
nest=1
else if(stcb(i1:i1).eq.']')then
if(nest.ne.1)then
go to 521
end if
nest=2
else if(stcb(i1:i1).eq.',')then
if(nest.ne.1)then
go to 521
end if
quote=0
else if(jj.ne.aspc)then
if(nest.ne.1)then
go to 521
end if
end if
else
if(nest.ne.1)then
go to 521
end if
end if
end do
if(ii.ne.0)then
go to 521
end if
if(stcb(itop:itop).ne.']')then
go to 521
end if
if(nest.ne.2)then
go to 521
end if
50 continue
pal=0
mpal=11
vel=0
vin=0
idin=0
kin=0
kid=0
knf=0
id1=0
id2=0
is1=0
is2=0
plm=0
itop=ixb
ix=ibot
280 continue
ic=iachar(stcb(ix:ix))
if(sucpal(pal,acf0(ic)).lt.0)then
go to 521
end if
if(pal.lt.3)then
if(pal.eq.1)then
if(acf0(ic).eq.0)then
if(id1.eq.0)then
id1=ix
end if
id2=ix
else if(acf0(ic).gt.2)then
if(stcb(ix:ix).eq.',')then
if(level.eq.1)then
if(xfile.eq.1)then
go to 521
end if
if(ngmk.gt.0)then
call rgmki(llk(1),llkt(1))
end if
level=2
fpass=1
call aoib(1)
stib(stibs(1))=eoa
off1=0
end if
else if(stcb(ix:ix).eq.';')then
if(level.lt.2)then
go to 521
end if
else if(stcb(ix:ix).eq.'=')then
if((level.ne.1).or.&
(kin.ne.0))then
go to 521
end if
kin=1
go to 283
else if(stcb(ix:ix).eq.']')then
if(level.lt.2)then
go to 521
end if
end if
if(level.eq.2)then
idin=idin+1
if(idin.gt.4)then
go to 521
end if
go to 281
else if(level.eq.3)then
idin=idin+1
go to 282
end if
end if
else if(pal.eq.2)then
if(stcb(ix:ix).eq.'=')then
kin=kin+1
go to 283
else if(acf0(ic).eq.0)then
if(id1.eq.0)then
id1=ix
end if
id2=ix
end if
end if
else if(pal.lt.6)then
if(pal.eq.3)then
if((vel.ne.0).or.&
(vin.ne.0))then
go to 521
end if
if(iachar(stcb(ix:ix)).eq.asq)then
plm=1-plm
if(is1.eq.0)then
is1=ix
end if
is2=ix
else
if(acf0(ic).eq.0)then
if(is1.eq.0)then
is1=ix
end if
is2=ix
else if(stcb(ix:ix).eq.'(')then
if(is1.ne.0)then
go to 521
end if
vel=1
else if((stcb(ix:ix).eq.',').or.&
(stcb(ix:ix).eq.']'))then
go to 285
end if
end if
else if(pal.eq.4)then
else if(pal.eq.5)then
if((vel.ne.0).or.&
(vin.ne.0).or.&
(plm.ne.1))then
go to 521
end if
if(iachar(stcb(ix:ix)).eq.asq)then
plm=1-plm
end if
is2=ix
end if
else if(pal.lt.9)then
if(pal.eq.6)then
if((vel.ne.1).or.&
(plm.ne.0))then
go to 521
end if
if(iachar(stcb(ix:ix)).eq.asq)then
plm=1-plm
if(is1.eq.0)then
is1=ix
end if
is2=ix
else
if(acf0(ic).eq.0)then
if(is1.eq.0)then
is1=ix
end if
is2=ix
else if((stcb(ix:ix).eq.',').or.&
(stcb(ix:ix).eq.')'))then
vin=vin+1
go to 285
end if
end if
else if(pal.eq.7)then
else if(pal.eq.8)then
if((vel.ne.1).or.&
(plm.ne.1))then
go to 521
end if
if(iachar(stcb(ix:ix)).eq.asq)then
plm=1-plm
end if
is2=ix
end if
else if(pal.lt.12)then
if(pal.eq.9)then
if((level.gt.2).or.&
(level.le.0))then
go to 521
end if
if((stcb(ix:ix).eq.',').or.&
(stcb(ix:ix).eq.']'))then
vin=0
vel=0
end if
else if(pal.eq.10)then
else if(pal.eq.11)then
end if
else
go to 521
end if
287 continue
pal=sucpal(pal,acf0(ic))
if(ix.lt.itop)then
ix=ix+1
go to 280
end if
if(vel.ne.0)then
go to 521
else if(pal.ne.mpal)then
go to 521
else if(plm.ne.0)then
go to 521
else if(id1.ne.0)then
go to 521
else if(is1.ne.0)then
go to 521
end if
if(level.eq.1)then
go to 70
else if(level.eq.2)then
if(fpass.eq.0)then
do i1=1,npmk
if(stib(apmkd(0)+i1).eq.3)then
if(stib(llpt(2)+1).eq.0)then
call emodel(19,1,1,dbl,nlin,ifile)
end if
end if
if(stib(apmkd(0)+i1).ne.stib(pmkd(0)+i1))then
ii=1
if(stib(apmkd(0)+i1).eq.2)then
if(stib(pmkd(0)+i1).eq.3)then
ii=stib(llpt(2)+1)
end if
end if
if(ii.ne.0)then
if(stib(apmkd(0)+i1).eq.0)then
call emodel(11,1,1,dbl,nlin,ifile)
else
call emodel(19,1,1,dbl,nlin,ifile)
end if
end if
end if
end do
nprop=nprop+1
end if
else if(level.eq.3)then
if(fpass.eq.0)then
do i1=1,nvmk
if(stib(avmkd(0)+i1).eq.0)then
call emodel(11,1,1,dbl,nlin,ifile)
end if
end do
jj=stib(llpt(3)+1)
if(jj.lt.3)then
call emodel(3,1,1,dbl,nlin,ifile)
end if
ii=0
do i1=1,jj
if(stib(antiq(0)+stib(llpt(3)+1+i1)).ne.0)then
ii=1-ii
end if
end do
if(ii.ne.0)then
call emodel(5,1,1,dbl,nlin,ifile)
end if
end if
end if
if(level.eq.2)then
if(fpass.ne.0)then
ii=stibs(1)+1
call aoib(4)
stib(ii:stibs(1))=eoa
ii=npmk+1
call trm(4,ii,stibs(1))
pmkd(0)=stibs(1)-ii
pmkl(0)=pmkd(0)-ii
pmkp(0)=pmkl(0)-ii
if(npmk.gt.0)then
jj=pmkp(0)-llk(2)+1
call xipht(pmkp(0)+1,stibs(1),-jj)
call aoib(-jj)
pmkd(0)=pmkd(0)-jj
pmkl(0)=pmkl(0)-jj
pmkp(0)=pmkp(0)-jj
end if
apmkd(0)=stibs(1)
call aoib(ii)
stib(stibs(1))=eoa
psize=7+2*npmk
end if
if(npmk.gt.0)then
stib(apmkd(0)+1:apmkd(0)+npmk)=0
end if
if(fpass.ne.0)then
fpass=0
if(idin.lt.3)then
go to 521
end if
go to 50
end if
else if(level.eq.3)then
if(fpass.ne.0)then
ii=stibs(1)+1
call aoib(3)
stib(ii:stibs(1))=eoa
ii=nvmk+1
call trm(3,ii,stibs(1))
vmkl(0)=stibs(1)-ii
vmkp(0)=vmkl(0)-ii
avmkd(0)=vmkp(0)-ii
call xipht(vmkp(0),stibs(1),-1)
call aoib(-1)
vmkl(0)=vmkl(0)-1
vmkp(0)=vmkp(0)-1
avmkd(0)=avmkd(0)-1
else
nvert=nvert+1
end if
if(nvmk.gt.0)then
stib(avmkd(0)+1:avmkd(0)+nvmk)=0
end if
if(fpass.ne.0)then
fpass=0
go to 50
end if
end if
go to 70
281 continue
if(idin.eq.3)then
if(stdw(stcb,id1,id2).ne.0)then
if(knf.ne.2)then
call emodel(6,1,1,dbl,nlin,ifile)
end if
knf=0
if(nphi.eq.0)then
call emodel(2,1,1,0,0,ifile)
end if
call rpi(llp(2),llpt(2))
level=3
fpass=1
go to 50
end if
if(stdz(stcb,id1,id2).eq.0)then
if(id1.ne.id2)then
go to 521
end if
ii=iachar(stcb(id1:id1))
if(ii.ne.aplus)then
if(ii.ne.aminus)then
go to 521
end if
end if
if(qmod.eq.0)then
qmod=1
else if(qmod.ne.1)then
go to 521
end if
jj=0
else
jj=stoz(stcb,id1,id2)
if(id1.eq.id2)then
go to 521
else if(abs(jj).ne.1)then
go to 521
end if
if(qmod.eq.0)then
qmod=2
else if(qmod.ne.2)then
go to 521
end if
end if
if(knf.ne.0)then
call emodel(1,1,1,dbl,nlin,ifile)
end if
if(fpass.eq.0)then
if(jj.eq.0)then
ii=iachar(stcb(id1:id1))
if(ii.eq.aplus)then
stib(off1+3)=0
else if(ii.eq.aminus)then
stib(off1+3)=1
else
go to 521
end if
else
stib(off1+3)=max(0,-jj)
end if
stib(off1+4)=phity%def
if(off2.ne.0)then
stib(off2+3)=stib(off1+3)
stib(off2+4)=stib(off1+4)
end if
stib(llpt(2))=off1+1
stib(off1+1)=eoa
llpt(2)=off1+1
if(off2.ne.0)then
stib(llpt(2))=off2+1
stib(off2+1)=eoa
llpt(2)=off2+1
end if
end if
go to 284
else if(idin.gt.3)then
if(fpass.eq.0)then
ij=0
call mstr0(stcb,id1,id2,popt8(0),wopt8(0),aopt8(0),ij)
if(ij.eq.0)then
go to 521
end if
ij=stib(copt8(0)+ij)
if(idin.eq.4)then
stib(off1+4)=ij
else if(idin.eq.5)then
if((ij.eq.phity%int).and.&
(stib(off1+4).eq.phity%notdp))then
stib(off1+4)=phity%ntint
else if((ij.eq.phity%notdp).and.&
(stib(off1+4).eq.phity%int))then
stib(off1+4)=phity%ntint
else
go to 521
end if
else
go to 521
end if
if(off2.ne.0)then
stib(off2+4)=stib(off1+4)
end if
end if
go to 284
end if
if(id1.le.0)then
go to 521
else if(id1.gt.id2)then
go to 521
else if(stdw(stcb,id1,id2).eq.0)then
mlin(1:srec)="unacceptable field name,"
call mput(1,1,dbl,nlin,ifile)
end if
if(fpass.ne.0)then
go to 284
end if
kk=id2-id1+1
if(kk.le.0)then
go to 521
end if
newid=1
if(nphi.gt.0)then
call mstr1(stcb,id1,id2,llp(2),4,5,ij)
if(ij.ne.0)then
newid=0
end if
end if
if(newid.eq.0)then
if(idin.eq.1)then
knf=1
else if(idin.eq.2)then
if(knf.eq.1)then
knf=2
else if(ij.ne.nphi)then
knf=1
end if
end if
end if
if(knf.ne.0)then
go to 284
end if
if(idin.eq.1)then
if(nprop.eq.0)then
call aoib(2)
stib(stibs(1)-1)=eoa
stib(stibs(1))=eoa
llp(2)=stibs(1)-1
llpt(2)=llp(2)
end if
off1=stibs(1)
off2=0
call aoib(psize)
stib(off1+2:stibs(1)-1)=0
stib(stibs(1))=eoa
stib(off1+5)=id1
stib(off1+6)=id2-id1+1
nphi=nphi+1
else if(idin.eq.2)then
newid=1
i1=stib(off1+5)
i2=stib(off1+6)
if(id2-id1+1.eq.i2)then
if(stcb(id1:id2).eq.stcb(i1:i1-1+i2))then
newid=0
end if
end if
if(newid.eq.0)then
stib(off1+2)=0
go to 284
end if
off2=stibs(1)
call aoib(psize)
stib(off2+2:stibs(1)-1)=0
stib(stibs(1))=eoa
stib(off2+5)=id1
stib(off2+6)=id2-id1+1
nphi=nphi+1
stib(off1+2)=1
stib(off2+2)=-1
end if
284 continue
id1=0
go to 287
282 continue
if(id1.le.0)then
go to 521
else if(id1.gt.id2)then
go to 521
else if(stdw(stcb,id1,id2).eq.0)then
mlin(1:srec)="unacceptable field name,"
call mput(1,1,dbl,nlin,ifile)
end if
if(idin.gt.nrho)then
if(idin.gt.maxdeg)then
call emodel(4,1,1,dbl,nlin,ifile)
end if
nrho=idin
end if
if(fpass.ne.0)then
go to 286
end if
if(idin.eq.1)then
if(nvert.eq.0)then
call aoib(1)
stib(stibs(1))=eoa
llp(3)=stibs(1)
llpt(3)=llp(3)
end if
ii=stibs(1)+1
jj=2+2*nvmk
call aoib(jj)
stib(llpt(3))=ii
llpt(3)=ii
stib(llpt(3))=eoa
stib(llpt(3)+1:stibs(1))=0
end if
ij=0
call mstr0(stcb,id1,id2,namep(0),namel(0),nap,ij)
if(ij.eq.0)then
call emodel(6,1,1,dbl,nlin,ifile)
end if
call aoib(1)
stib(stibs(1))=0
stib(llpt(3)+1+idin)=ij
stib(llpt(3)+1)=stib(llpt(3)+1)+1
286 continue
id1=0
go to 287
283 continue
if(id1.le.0)then
go to 521
else if(id1.gt.id2)then
go to 521
else if(stdw(stcb,id1,id2).eq.0)then
go to 521
end if
if(level.eq.1)then
if(kin.gt.1)then
go to 521
end if
if(ngmk.eq.0)then
call aoib(1)
stib(stibs(1))=eoa
llk(1)=stibs(1)
llkt(1)=llk(1)
else
call mstr1(stcb,id1,id2,llk(1),1,2,ij)
if(ij.ne.0)then
call emodel(9,1,1,dbl,nlin,ifile)
end if
end if
ij=0
call mstr0(stcb,id1,id2,udkp(0),udkl(0),nap,ij)
ii=stibs(1)+1
call aoib(4)
if(ij.eq.0)then
stib(ii+1)=id1
else
stib(ii+1)=stib(udkp(0)+ij)
end if
stib(ii+2)=id2-id1+1
stib(llkt(1))=ii
llkt(1)=ii
stib(ii)=eoa
stib(stibs(1))=eoa
ngmk=ngmk+1
else if(level.eq.2)then
if(fpass.ne.0)then
ij=0
call mstr0(stcb,id1,id2,gmkp(0),gmkl(0),nap,ij)
if(ij.ne.0)then
call emodel(9,1,1,dbl,nlin,ifile)
end if
if(npmk.eq.0)then
call aoib(2)
stib(stibs(1)-1)=eoa
stib(stibs(1))=eoa
llk(2)=stibs(1)-1
llkt(2)=llk(2)
else
call mstr1(stcb,id1,id2,llk(2),1,2,ij)
if(ij.ne.0)then
call emodel(9,1,1,dbl,nlin,ifile)
end if
end if
ij=0
call mstr0(stcb,id1,id2,udkp(0),udkl(0),nap,ij)
ii=stibs(1)+1
call aoib(4)
if(ij.eq.0)then
stib(ii+1)=id1
stib(ii+2)=id2-id1+1
else
stib(ii+1)=stib(udkp(0)+ij)
stib(ii+2)=stib(udkl(0)+ij)
end if
stib(stibs(1))=0
stib(llkt(2))=ii
llkt(2)=ii
stib(llkt(2))=eoa
npmk=npmk+1
else
kid=0
call mstr0(stcb,id1,id2,pmkp(0),pmkl(0),nap,kid)
if(kid.eq.0)then
call emodel(10,1,1,dbl,nlin,ifile)
end if
if(stib(apmkd(0)+kid).ne.0)then
call emodel(9,1,1,dbl,nlin,ifile)
end if
end if
else if(level.eq.3)then
if(fpass.ne.0)then
ij=0
call mstr0(stcb,id1,id2,gmkp(0),gmkl(0),nap,ij)
if(ij.ne.0)then
call emodel(9,1,1,dbl,nlin,ifile)
end if
ij=0
call mstr0(stcb,id1,id2,pmkp(0),pmkl(0),nap,ij)
if(ij.ne.0)then
call emodel(9,1,1,dbl,nlin,ifile)
end if
if(nvmk.eq.0)then
call aoib(1)
stib(stibs(1))=eoa
llk(3)=stibs(1)
llkt(3)=llk(3)
else
call mstr1(stcb,id1,id2,llk(3),1,2,ij)
if(ij.ne.0)then
call emodel(9,1,1,dbl,nlin,ifile)
end if
end if
ij=0
call mstr0(stcb,id1,id2,udkp(0),udkl(0),nap,ij)
ii=stibs(1)+1
call aoib(3)
if(ij.eq.0)then
stib(stibs(1)-1)=id1
stib(stibs(1))=id2-id1+1
else
stib(stibs(1)-1)=stib(udkp(0)+ij)
stib(stibs(1))=stib(udkl(0)+ij)
end if
stib(llkt(3))=ii
llkt(3)=ii
stib(llkt(3))=eoa
nvmk=nvmk+1
else
kid=0
call mstr0(stcb,id1,id2,vmkp(0),vmkl(0),nap,kid)
if(kid.eq.0)then
call emodel(10,1,1,dbl,nlin,ifile)
end if
if(stib(avmkd(0)+kid).ne.0)then
call emodel(9,1,1,dbl,nlin,ifile)
end if
end if
end if
id1=0
go to 287
285 continue
if(is1.eq.0)then
go to 521
else if(is1.gt.is2)then
go to 521
end if
if(level.eq.1)then
call aoib(2)
j1=llkt(1)+3
if(vel.eq.0)then
stib(j1)=1
else
stib(j1)=vin+1
end if
if(vel.ne.0)then
call emodel(19,1,1,dbl,nlin,ifile)
end if
else if(level.eq.2)then
if(fpass.ne.0)then
j1=llkt(2)+3
if(vel.eq.0)then
stib(j1)=1
else
stib(j1)=3
end if
go to 184
else
j1=apmkd(0)+kid
if(vel.eq.0)then
stib(j1)=1
else
stib(j1)=vin+1
end if
if(stib(j1).gt.stib(pmkd(0)+kid))then
call emodel(19,1,1,dbl,nlin,ifile)
end if
end if
if(vin.gt.2)then
call emodel(19,1,1,dbl,nlin,ifile)
end if
else if(level.eq.3)then
if(vel.ne.0)then
go to 521
end if
if(fpass.ne.0)then
go to 184
else
stib(avmkd(0)+kid)=1
end if
end if
if(iachar(stcb(is1:is1)).eq.asq)then
call stds(stcb,is1,is2,1,j5,j6)
if(j5.lt.0)then
go to 521
else if(j5.gt.0)then
is1=j5
kk=j6-j5+1
else
is1=is1+1
kk=is2-is1
end if
else
if(stdw(stcb,is1,is2).eq.0)then
if(stdq(stcb,is1,is2).eq.0)then
go to 521
end if
end if
kk=is2-is1+1
end if
if(level.eq.1)then
stib(stibs(1)-1)=is1
stib(stibs(1))=kk
else if(level.eq.2)then
jj=6+kid
if(vel.ne.0)then
if(vin.eq.1)then
j1=off1+jj
else if(vin.eq.2)then
j1=off2+jj
end if
stib(j1)=is1
stib(j1+npmk)=kk
else
j1=off1+jj
stib(j1)=is1
stib(j1+npmk)=kk
if(off2.ne.0)then
j1=off2+jj
stib(j1)=is1
stib(j1+npmk)=kk
end if
end if
else if(level.eq.3)then
jj=llpt(3)+stib(llpt(3)+1)+2*kid
stib(jj)=is1
stib(jj+1)=kk
end if
184 continue
is1=0
go to 287
521 continue
mlin(1:srec)="wrong syntax,"
call mput(1,1,dbl,nlin,ifile)
777 continue
if(nvert.eq.0)then
call emodel(7,1,1,0,0,ifile)
end if
call rvi(llp(3),llpt(3))
if(njb.lt.nstjb)then
call aojb(-1)
end if
if(nstjb.ne.njb)then
mlin(1:srec)='model_2'
call mput(1,1,0,0,0)
end if
return
end subroutine model
subroutine rgmki(p1,p2)
use,non_intrinsic::qmodel
use,non_intrinsic::qintr
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer,intent(in)::p1,p2
integer::ii,i1,j1,j2,j3,jj,tgmkr
if((p1.eq.nap).or.&
(p2.eq.nap))then
mlin(1:srec)='rgmki_2'
call mput(1,1,0,0,0)
end if
if(ngmk.eq.0)then
gmkp(0)=nap
gmkl(0)=nap
gmkr(0)=nap
gmko(0)=nap
gmkvp(0)=nap
gmkvl(0)=nap
call aoib(-1)
go to 90
end if
ii=ngmk+1
gmkp(0)=stibs(1)
call aoib(ii)
stib(stibs(1))=eoa
gmkl(0)=stibs(1)
call aoib(ii)
stib(stibs(1))=eoa
gmkr(0)=stibs(1)
call aoib(ii)
stib(stibs(1))=eoa
gmko(0)=stibs(1)
call aoib(ii)
stib(stibs(1))=eoa
j1=0
tgmkr=0
jj=p1
do while(stib(jj).ne.eoa)
jj=stib(jj)
j1=j1+1
stib(gmkp(0)+j1)=stib(jj+1)
stib(gmkl(0)+j1)=stib(jj+2)
stib(gmkr(0)+j1)=stib(jj+3)
stib(gmko(0)+j1)=tgmkr
tgmkr=tgmkr+max(1,stib(jj+3)-1)
end do
if(jj.ne.p2)then
mlin(1:srec)='rgmki_1'
call mput(1,1,0,0,0)
end if
ii=tgmkr+1
gmkvp(0)=stibs(1)
call aoib(2*ii)
gmkvl(0)=gmkvp(0)+ii
stib(gmkvl(0))=eoa
stib(stibs(1))=eoa
j1=gmkr(0)
j2=0
jj=p1
do while(stib(jj).ne.eoa)
jj=stib(jj)
j1=j1+1
j3=jj+3
do i1=1,max(1,stib(j1)-1)
j2=j2+1
j3=j3+1
stib(gmkvp(0)+j2)=stib(j3)
j3=j3+1
stib(gmkvl(0)+j2)=stib(j3)
end do
end do
90 continue
return
end subroutine rgmki
subroutine rpi(p1,p2)
use,non_intrinsic::cbuffer
use,non_intrinsic::qintr
use,non_intrinsic::qmodel
use,non_intrinsic::qproc
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer,intent(in)::p1,p2
integer,external::stdw,stdz,stoz
integer::ii,i1,i2,i3,ij,j1,j2,j3,j4,p0
if((p1.eq.nap).or.&
(p2.eq.nap))then
mlin(1:srec)='rpi_2'
call mput(1,1,0,0,0)
end if
ii=nphi+1
call aoib(6*ii)
stib(stibs(1))=eoa
blok(0)=stibs(1)-ii
stib(blok(0))=eoa
link(0)=blok(0)-ii
stib(link(0))=eoa
antiq(0)=link(0)-ii
stib(antiq(0))=eoa
tpc(0)=antiq(0)-ii
stib(tpc(0))=eoa
namel(0)=tpc(0)-ii
stib(namel(0))=eoa
namep(0)=namel(0)-ii
if(npmk.gt.0)then
pmkvfpp(0)=stibs(1)
call aoib(ii)
stib(stibs(1))=eoa
pmkvflp(0)=stibs(1)
call aoib(ii)
stib(stibs(1))=eoa
else
pmkvfpp(0)=nap
pmkvflp(0)=nap
end if
j3=0
j4=p1
do while(stib(j4).ne.eoa)
j4=stib(j4)
j3=j3+1
stib(link(0)+j3)=stib(j4+1)
stib(antiq(0)+j3)=stib(j4+2)
stib(tpc(0)+j3)=stib(j4+3)
stib(namep(0)+j3)=stib(j4+4)
stib(namel(0)+j3)=stib(j4+5)
if(npmk.gt.0)then
stib(pmkvfpp(0)+j3)=j4+5
stib(pmkvflp(0)+j3)=j4+5+npmk
end if
end do
if(j4.ne.p2)then
mlin(1:srec)='rpi_1'
call mput(1,1,0,0,0)
end if
do i1=1,nphi
j1=stib(tpc(0)+i1)
j2=stib(link(0)+i1)
stib(link(0)+i1)=j2+i1
if(j1.eq.phity%ext)then
mflag(5)=1
else
if((j1.eq.phity%notdp).or.&
(j1.eq.phity%ntint))then
mflag(6)=1
end if
if(j2.ge.0)then
npprop=npprop+1
end if
end if
end do
if(npprop.eq.0)then
call wput(6,0,0)
end if
if(npmk.eq.0)then
pmkt(0)=nap
pmkzp(0)=nap
pmkvmi(0)=nap
pmkvma(0)=nap
go to 90
end if
ii=npmk+1
pmkt(0)=stibs(1)
call aoib(ii)
stib(stibs(1))=eoa
do i1=1,npmk
ij=0
d01: do i2=4,2,-2
if(ij.eq.0)then
do i3=1,nphi
j1=stib(stib(pmkvfpp(0)+i3)+i1)
j2=j1+(stib(stib(pmkvflp(0)+i3)+i1)-1)
if(j1.gt.j2)then
ij=1
cycle d01
else if(i2.eq.4)then
if(stdz(stcb,j1,j2).eq.0)then
cycle d01
end if
else if(i2.eq.2)then
if(stdw(stcb,j1,j2).eq.0)then
cycle d01
end if
end if
end do
ij=i2
end if
end do d01
if(ij.eq.0)then
ij=1
end if
stib(pmkt(0)+i1)=ij
end do
ii=npmk+1
pmkzp(0)=stibs(1)
call aoib(ii)
stib(stibs(1))=eoa
pmkvmi(0)=stibs(1)
call aoib(ii)
stib(stibs(1))=eoa
pmkvma(0)=stibs(1)
call aoib(ii)
stib(stibs(1))=eoa
do i1=1,npmk
if(stib(pmkt(0)+i1).eq.4)then
p0=stibs(1)
stib(pmkzp(0)+i1)=p0
ii=nphi
call aoib(ii)
do i2=1,nphi
j1=stib(stib(pmkvfpp(0)+i2)+i1)
j2=j1+(stib(stib(pmkvflp(0)+i2)+i1)-1)
ij=stoz(stcb,j1,j2)
if(i2.gt.1)then
if(ij.lt.j3)then
j3=ij
else if(ij.gt.j4)then
j4=ij
end if
else
j3=ij
j4=ij
end if
stib(p0+i2)=ij
end do
else
j3=0
j4=0
end if
stib(pmkvmi(0)+i1)=j3
stib(pmkvma(0)+i1)=j4
end do
if(stib(stibs(1)).ne.eoa)then
call aoib(1)
stib(stibs(1))=eoa
end if
90 continue
return
end subroutine rpi
subroutine rvi(p1,p2)
use,non_intrinsic::cbuffer
use,non_intrinsic::qintr
use,non_intrinsic::qmix
use,non_intrinsic::qmodel
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer,intent(in)::p1,p2
integer,external::sigz,stdw,stdz
integer::xtmp(1:maxdeg+1),nrot(1:maxdeg),rotvpo(1:maxdeg)
integer::hi,hj,ii,ij,jj,jk,kk
integer::h1,h2,i1,i2,i3,i4,j1,j2
if((p1.eq.nap).or.&
(p2.eq.nap))then
mlin(1:srec)='rvi_6'
call mput(1,1,0,0,0)
end if
ii=nrho+1
nivd(0)=stibs(1)
call aoib(ii)
stib(stibs(1))=eoa
ii=nvert+1
vval(0)=stibs(1)
call aoib(ii)
stib(stibs(1))=eoa
vparto(0)=stibs(1)
call aoib(ii)
stib(stibs(1))=eoa
jj=nvmk+1
vmkvpp(0)=stibs(1)
call aoib(jj)
stib(stibs(1))=eoa
vmkvlp(0)=stibs(1)
call aoib(jj)
stib(stibs(1))=eoa
j1=stibs(1)
call aoib(2*nvmk*ii)
do i1=1,nvmk
stib(vmkvpp(0)+i1)=j1
j1=j1+ii
stib(j1)=eoa
stib(vmkvlp(0)+i1)=j1
j1=j1+ii
stib(j1)=eoa
end do
if(nrho.lt.3)then
mlin(1:srec)='rvi_4'
call mput(1,1,0,0,0)
end if
if(nrho.gt.0)then
nrot(1:nrho)=0
stib(nivd(0)+1:nivd(0)+nrho)=0
end if
j1=0
jj=p1
do while(stib(jj).ne.eoa)
jj=stib(jj)
j1=j1+1
j2=stib(jj+1)
stib(vval(0)+j1)=j2
j2=j2+nivd(0)
stib(j2)=stib(j2)+1
end do
if((j1.ne.nvert).or.&
(jj.ne.p2))then
mlin(1:srec)='rvi_1'
call mput(1,1,0,0,0)
end if
j1=0
jj=p1
do while(stib(jj).ne.eoa)
jj=stib(jj)
j1=j1+1
j2=jj+stib(vval(0)+j1)
do i1=1,nvmk
j2=j2+2
stib(stib(vmkvpp(0)+i1)+j1)=stib(j2)
stib(stib(vmkvlp(0)+i1)+j1)=stib(j2+1)
end do
end do
if(j1.ne.nvert)then
mlin(1:srec)='rvi_3'
call mput(1,1,0,0,0)
end if
kk=p1
if(p1.gt.1)then
if(stib(p1-1).eq.eoa)then
kk=p1-1
end if
end if
ij=stib(p1)
stib(p1)=eoa
j1=0
50 continue
jj=ij
if(jj.ne.eoa)then
j1=j1+1
ij=stib(ij)
j2=jj-kk+1
call xipht(jj+2,jj+1+stib(vval(0)+j1),-j2)
stib(vparto(0)+j1)=kk
kk=kk+stib(vval(0)+j1)
go to 50
end if
if(j1.ne.nvert)then
mlin(1:srec)='rvi_5'
call mput(1,1,0,0,0)
end if
kk=nivd(0)-kk-1
call xipht(nivd(0),stibs(1),-kk)
call aoib(-kk)
nivd(0)=nivd(0)-kk
vval(0)=vval(0)-kk
vparto(0)=vparto(0)-kk
vmkvpp(0)=vmkvpp(0)-kk
vmkvlp(0)=vmkvlp(0)-kk
do i1=1,nvmk
stib(vmkvpp(0)+i1)=stib(vmkvpp(0)+i1)-kk
stib(vmkvlp(0)+i1)=stib(vmkvlp(0)+i1)-kk
end do
stib(nivd(0))=eoa
jj=nvmk+1
vmkt(0)=stibs(1)
call aoib(jj)
stib(stibs(1))=eoa
vmks(0)=stibs(1)
call aoib(jj)
stib(stibs(1))=eoa
if(nvmk.gt.0)then
stib(vmks(0)+1:vmks(0)+nvmk)=0
end if
do i1=1,nvmk
ii=stib(vmkvpp(0)+i1)
jj=stib(vmkvlp(0)+i1)
ij=0
do i2=4,2,-2
if(ij.eq.0)then
do i3=1,nvert
j1=stib(ii+i3)
j2=stib(ii+i3)-1+stib(jj+i3)
if(j1.gt.j2)then
ij=1
go to 90
else if(i2.eq.4)then
if(stdz(stcb,j1,j2).eq.0)then
go to 90
end if
else if(i2.eq.2)then
if(stdw(stcb,j1,j2).eq.0)then
go to 90
end if
end if
end do
ij=i2
end if
90 continue
end do
if(ij.eq.0)then
ij=1
end if
stib(vmkt(0)+i1)=ij
end do
do i1=1,nrho
if(stib(nivd(0)+i1).gt.0)then
rotvpo(i1)=stibs(1)
do i2=1,nvert
if(i1.ne.stib(vval(0)+i2))then
cycle
else if(dflag(18).ne.0)then
end if
j1=stib(vparto(0)+i2)
do i3=1,i1
xtmp(i3)=stib(j1+i3)
end do
do i3=1,i1-1
do i4=i3+1,i1
if(xtmp(i3).gt.xtmp(i4))then
ii=xtmp(i3)
xtmp(i3)=xtmp(i4)
xtmp(i4)=ii
end if
end do
end do
go to 830
805 continue
j1=0
do i3=i1-1,1,-1
if(xtmp(i3).lt.xtmp(i3+1))then
j1=i3
exit
end if
end do
if(j1.eq.0)then
cycle
end if
j2=i1
do i3=j1+2,i1
if(xtmp(i3).le.xtmp(j1))then
j2=i3-1
exit
end if
end do
ii=xtmp(j1)
xtmp(j1)=xtmp(j2)
xtmp(j2)=ii
jj=(i1-j1)/2
j1=j1+1
do i3=0,jj-1
ii=xtmp(j1+i3)
xtmp(j1+i3)=xtmp(i1-i3)
xtmp(i1-i3)=ii
end do
830 continue
j1=stibs(1)+1
call aoib(i1+1)
stib(j1)=i2
do i3=1,i1
stib(j1+i3)=xtmp(i3)
end do
nrot(i1)=nrot(i1)+1
go to 805
end do
j1=stibs(1)+1
call aoib(i1+1)
stib(j1)=0
do i2=1,i1
stib(j1+i2)=nphi+1
end do
end if
end do
call aoib(1)
stib(stibs(1))=eoa
d01: do i1=1,nrho
if((stib(nivd(0)+i1).gt.0).and.&
(nrot(i1).gt.1))then
h2=nrot(i1)
h1=(h2/2)+1
720 continue
if(h1.gt.1)then
h1=h1-1
ii=rotvpo(i1)+(i1+1)*(h1-1)
do i2=1,i1+1
xtmp(i2)=stib(ii+i2)
end do
else
ii=rotvpo(i1)+(i1+1)*(h2-1)
do i2=1,i1+1
xtmp(i2)=stib(ii+i2)
end do
do i2=1,i1+1
stib(ii+i2)=stib(rotvpo(i1)+i2)
end do
h2=h2-1
if(h2.eq.1)then
do i2=1,i1+1
stib(rotvpo(i1)+i2)=xtmp(i2)
end do
cycle d01
end if
end if
hj=h1
740 continue
hi=hj
hj=hi+hi
if(hj.le.h2)then
if(hj.lt.h2)then
kk=0
jj=rotvpo(i1)+(i1+1)*(hj-1)
jk=jj+i1+1
j1=1
745 continue
if(j1.le.i1)then
j1=j1+1
if(stib(jj+j1).lt.stib(jk+j1))then
kk=-1
else if(stib(jj+j1).gt.stib(jk+j1))then
kk=1
else
go to 745
end if
end if
if(kk.eq.-1)then
hj=hj+1
else if(kk.eq.0)then
mflag(4)=1
end if
end if
kk=0
jj=rotvpo(i1)+(i1+1)*(hj-1)
j1=1
755 continue
if(j1.le.i1)then
j1=j1+1
if(xtmp(j1).lt.stib(jj+j1))then
kk=-1
else if(xtmp(j1).gt.stib(jj+j1))then
kk=1
else
go to 755
end if
end if
if(kk.eq.-1)then
ii=rotvpo(i1)+(i1+1)*(hi-1)
do i2=1,i1+1
stib(ii+i2)=stib(jj+i2)
end do
go to 740
else if(kk.eq.0)then
mflag(4)=1
end if
end if
ii=rotvpo(i1)+(i1+1)*(hi-1)
do i2=1,i1+1
stib(ii+i2)=xtmp(i2)
end do
go to 720
end if
end do d01
if(mflag(4).ne.0)then
call wput(4,0,0)
end if
do i1=1,nrho
if(nrot(i1).gt.0)then
ii=rotvpo(i1)
do i2=1,nrot(i1)
j1=ii+2
j2=j1+i1+1
jj=i1
84 continue
if(jj.gt.0)then
kk=stib(j2)-stib(j1)
if(kk.eq.0)then
jj=jj-1
j1=j1+1
j2=j2+1
go to 84
else if(kk.lt.0)then
mlin(1:srec)='rvi_2'
call mput(1,1,0,0,0)
end if
end if
ii=ii+i1+1
end do
end if
end do
ii=nrho+1
dpntro(0)=stibs(1)
call aoib(ii)
stib(stibs(1))=eoa
do i1=1,nrho
if(nrot(i1).gt.0)then
stib(dpntro(0)+i1)=stibs(1)
call aoib(nphi+1)
stib(stibs(1))=nap
ii=rotvpo(i1)+2
jj=0
do i2=1,nrot(i1)+1
if(stib(ii).ne.jj)then
if(stib(ii).le.nphi)then
kk=stib(ii)
else
kk=nphi
end if
do i3=jj+1,kk
stib(stib(dpntro(0)+i1)+i3)=ii-1
end do
jj=stib(ii)
end if
ii=ii+i1+1
end do
else
stib(dpntro(0)+i1)=nap
end if
end do
stib(stibs(1))=eoa
call sact
return
end subroutine rvi
subroutine sact
use,non_intrinsic::qintr
use,non_intrinsic::qmix
use,non_intrinsic::qmodel
use,non_intrinsic::wbuffer
use,non_intrinsic::zbuffer
implicit none
save
integer::ap(0:0),k1,k2,k3,nphix
integer::h0,i1,i2,j1,j2,ii,jj
ap(0)=nap
h0=-(nphi+1)
ii=0
do i1=1,nphi
j1=blok(0)+i1
stib(j1)=h0
ii=ii+1
do i2=1,nrho
if(stib(nivd(0)+i2).gt.0)then
if(stib(stib(stib(dpntro(0)+i2)+i1)+1).eq.i1)then
ii=ii-1
stib(j1)=0
exit
end if
end if
end do
if(stib(j1).eq.h0)then
j2=blok(0)+stib(link(0)+i1)
if(stib(j2).eq.h0)then
nblokp=nblokp+1
stib(j1)=h0+nblokp
stib(j2)=h0+nblokp
end if
end if
end do
if(ii.gt.0)then
call emodel(8,0,0,0,0,0)
end if
ap(0)=stibs(1)
call aoib(nphi+1)
stib(stibs(1))=eoa
k1=0
k2=0
k3=0
nphix=nphi-nblokp
d01: do while(k2.lt.nphix)
d02: do i1=k3+1,nphi
k3=i1
j2=stib(link(0)+i1)
if(stib(blok(0)+i1).eq.0)then
nblok=nblok+1
stib(blok(0)+i1)=nblok
k2=k2+1
stib(ap(0)+k2)=i1
if(stib(blok(0)+j2).eq.0)then
stib(blok(0)+j2)=nblok
k2=k2+1
stib(ap(0)+k2)=j2
end if
if(k2.eq.nphix)then
exit d01
end if
exit
end if
end do d02
d03: do while(k1.lt.k2)
k1=k1+1
jj=stib(ap(0)+k1)
do i1=2,nrho
if(stib(dpntro(0)+i1).gt.0)then
j1=stib(stib(dpntro(0)+i1)+jj)+1
do while(stib(j1).eq.jj)
j2=stib(j1+1)
if(stib(blok(0)+j2).eq.0)then
stib(blok(0)+j2)=nblok
k2=k2+1
stib(ap(0)+k2)=j2
j2=stib(link(0)+j2)
if(stib(blok(0)+j2).eq.0)then
stib(blok(0)+j2)=nblok
k2=k2+1
stib(ap(0)+k2)=j2
end if
if(k2.eq.nphix)then
exit d01
end if
end if
j1=j1+i1+1
end do
end if
end do
end do d03
end do d01
ii=nphi+1
if(ap(0).eq.stibs(1)-ii)then
ap(0)=nap
call aoib(-ii)
end if
if((nblok.gt.nphix).or.&
(k2.ne.nphix))then
mlin(1:srec)='sact_1'
call mput(1,1,0,0,0)
end if
if(nblok.gt.1)then
call wput(1,0,0)
end if
if(nblokp.gt.0)then
do i1=1,nphi
j1=stib(link(0)+i1)
if(i1.gt.j1)then
if(stib(blok(0)+i1).lt.0)then
if(stib(blok(0)+j1).gt.0)then
stib(blok(0)+i1)=-stib(blok(0)+j1)
end if
else if(stib(blok(0)+j1).lt.0)then
if(stib(blok(0)+i1).gt.0)then
stib(blok(0)+j1)=-stib(blok(0)+i1)
end if
end if
end if
end do
j1=blok(0)
do i1=1,nphi
j1=j1+1
if(-stib(j1).gt.nblok)then
stib(j1)=stib(j1)-h0+nblok
end if
end do
end if
return
end subroutine sact
program qgraf
use,non_intrinsic::qintr
use,non_intrinsic::qproc,only:nloop,loopx
implicit none
save
integer::ii,jj
call init0
call inputs
if(cflag(1).gt.0)then
call propos(1)
end if
do ii=loopx(1),loopx(2),loopx(5)
nloop=ii
call rflag
qco(:)=1
jj=1
jflag(15)=1
do while(jj.ne.0)
call g43
jj=qco(qcntr%d)
qco(:)=0
end do
if(jflag(1).gt.0)then
exit
end if
end do
jflag(7)=0
call qende
end program qgraf
