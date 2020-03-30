      PROGRAM PLOTRESPS

c     Plots residuals using resid2.tmp -> pgplot.ps
c     INPUT: resid2.tmp, switch.tmp
      
      implicit none

      real*8 t(99999),f(99999),r1(99999),r2(99999),err(99999),
     &     phase(99999)
      real*8 period,pepoch,flim1,flim2
      real xp(99999),yp(99999),eyp(99999),xepoch,xmin,xmax,ymin,ymax,
     :     ctroph
      integer ntoas
      logical ERRB,FSEP,PREFITON,ROMAN,BNRY,COLOR,
     :     XMJD,XYR,XOPH,XNB,YPH,YMU,YMS

      common/SWITCH/ ERRB,FSEP,PREFITON,ROMAN,COLOR
      common/AXIS/ XMJD,XYR,XOPH,XNB,YPH,YMU,YMS

      common/DATA/ t,r1,r2,err,phase,period,pepoch
      common/XYM/ xmin,xmax,ymin,ymax
      common/PLOT/ xp,yp,eyp,xepoch,ctroph,ntoas
      common/FREQ/ f,flim1,flim2

      BNRY=.false.
      
* --- read plot parameters from switch.dat ---

      open(11,file='switch.tmp',status='old')

      read(11,*) xmin,xmax,ymin,ymax
      read(11,'(l1)') PREFITON
      read(11,'(l1)') XMJD
      read(11,'(l1)') XYR
      read(11,'(l1)') XNB
      read(11,'(l1)') XOPH
      read(11,'(l1)') YPH
      read(11,'(l1)') YMU
      read(11,'(l1)') YMS
      read(11,'(l1)') ERRB
      read(11,'(l1)') FSEP
      read(11,'(l1)') ROMAN
      read(11,'(l1)') COLOR
      read(11,*) flim1,flim2

      close(11)

      call LOADRES(ntoas,BNRY)
      call PLOTSET(PREFITON,t,phase,r1,r2,err,period,pepoch)
      call PLOTR()

      END

*=======================================================================

      SUBROUTINE LOADRES(ntoas,BNRY)

* Reads TOAs (t), frequency (f), pre-fit (r1) and post-fit (r2) 
* residuals from resid2.tmp 

      implicit none

      real*8 t(99999),f(99999),r1(99999),r2(99999),err(99999),
     &     phase(99999),pepoch
      real*8 period,flim1,flim2
      real*8 ct,dt2,dt2sec,ph,frq,weight,terr,y,ddm
      integer ntoas
      character line*80
      logical BNRY
      
      common /DATA/ t,r1,r2,err,phase,period,pepoch
      common /FREQ/ f,flim1,flim2

* --- residuals from resid2.tmp  ---
      
      open(11,file='resid2.tmp',form='unformatted',status='old')
      ntoas=0
 101  read(11,END=102) ct,dt2,dt2sec,ph,frq,weight,terr,y,ddm
      ntoas=ntoas+1
      t(ntoas)=ct
      f(ntoas)=frq
      r1(ntoas)=y
      r2(ntoas)=dt2sec*1.d6
      err(ntoas)=terr
      phase(ntoas)=ph
      if(.NOT.BNRY.and.ph.ne.0) BNRY=.true.
      goto 101
 102  close(11)
      period=dt2sec/dt2

      open(11,file='tempo.lis',status='old')
 103  read(11,'(a)') line
      if(line(1:3).ne.'PSR') goto 103
      read(line,'(65x,f15.0)') pepoch
      close(11)

      RETURN
      END

*=======================================================================

      SUBROUTINE PLOTSET(PREFITON,t,phase,r1,r2,err,period,pepoch)

      implicit none
      integer*4 i,ntoas
      real*8 t(99999),r1(99999),r2(99999),err(99999),phase(99999),
     +     period,pepoch
      real x(99999),y(99999),ey(99999),xepoch,ctroph
      logical PREFITON,XMJD,XYR,XOPH,XNB,YPH,YMU,YMS

      common/AXIS/ XMJD,XYR,XOPH,XNB,YPH,YMU,YMS
      common/PLOT/ x,y,ey,xepoch,ctroph,ntoas

      do i=1,ntoas
         if(XMJD) x(i)=t(i)-50000.0
         if(XYR)  x(i)=(t(i)-39126.0)/365.25+1966
         if(XNB)  x(i)=i
         if(XOPH)then
            x(i)=phase(i)
            if(x(i).lt.ctroph-0.5) x(i)=x(i)+1.0
            if(x(i).ge.ctroph+0.5) x(i)=x(i)-1.0
         endif
         if (PREFITON) then
            if(YPH) y(i)=r1(i)
            if(YPH) ey(i)=err(i)/1.d6/period
            if(YMU) y(i) =r1(i)*period*1.d6
            if(YMU) ey(i)=err(i)
            if(YMS) y(i) =r1(i)*period*1.d3
            if(YMS) ey(i)=err(i)/1.d3
         else
            if(YPH) y(i) =r2(i) /1.d6/period
            if(YPH) ey(i)=err(i)/1.d6/period
            if(YMU) y(i) =r2(i)
            if(YMU) ey(i)=err(i)
            if(YMS) y(i) =r2(i)/1.d3
            if(YMS) ey(i)=err(i)/1.d3
         endif
      enddo
      if(XMJD) xepoch=pepoch-50000.0
      if(XYR)  xepoch=(pepoch-39126.0)/365.25+1966

      RETURN
      END

*=======================================================================

      SUBROUTINE PLOTR()

      implicit none
      integer i,ntoas,n1,n2,n3,PGBEG
      real*8 f(99999),flim1,flim2
      real x(99999),y(99999),ey(99999),xepoch,ctroph
      real xf1(99999),yf1(99999),eyf1(99999)
      real xf2(99999),yf2(99999),eyf2(99999)
      real xf3(99999),yf3(99999),eyf3(99999)
      real xmin,xmax,ymin,ymax
      character XSTR30*30,YSTR30*30
      logical ERRB,FSEP,PREFITON,ROMAN,COLOR
      logical XMJD,XYR,XNB,XOPH,YPH,YMU,YMS

      common/SWITCH/ ERRB,FSEP,PREFITON,ROMAN,COLOR
      common/AXIS/ XMJD,XYR,XOPH,XNB,YPH,YMU,YMS

      common/XYM/ xmin,xmax,ymin,ymax
      common/PLOT/ x,y,ey,xepoch,ctroph,ntoas
      common/FREQ/ f,flim1,flim2

      n1=0
      n2=0
      n3=0
      if (FSEP) then
         do i=1,ntoas
            if (f(i).gt.0.and.f(i).le.flim1) then
               n1=n1+1
               xf1(n1) =x(i)
               yf1(n1) =y(i)
               eyf1(n1)=ey(i)
            endif
            if (f(i).gt.flim1.and.f(i).le.flim2) then
               n2=n2+1
               xf2(n2) =x(i)
               yf2(n2) =y(i)
               eyf2(n2)=ey(i)
            endif
            if (f(i).gt.flim2) then
               n3=n3+1
               xf3(n3) =x(i)
               yf3(n3) =y(i)
               eyf3(n3)=ey(i)
            endif
         enddo
      endif

      if(COLOR)then
         IF (PGBEG(0,'/cps',1,1) .NE. 1) STOP ! for UNIX 
      else
         IF (PGBEG(0,'/ps',1,1) .NE. 1) STOP ! for UNIX 
      endif

c      call PGPAPER(8.5,0.6)
      call PGVPORT(0.1,.95,0.2,.95)

      if(ROMAN)then
         call PGSCF(2)
      else
         call PGSCF(3)
      endif
      call PGSCH(1.4)
      call PGSLS(1)
      call PGSLW(5)
      call PGWINDOW(xmin,xmax,ymin,ymax)
      call PGBOX('bcnts',0.,0,'bcnts',0.,0)

      if(XMJD) XSTR30='MJD-50000.0'
      if(XYR)  XSTR30='year'
      if(XOPH) XSTR30='orbital phase'
      if(XNB)  XSTR30='Nb. of TOA'
      if(YPH)  YSTR30='residual (phase)'
      if(YMU)  YSTR30='residual (\\gms)'
      if(YMS)  YSTR30='residual (ms)'

      call PGLAB(XSTR30,YSTR30,' ')
            
      call PGSLS(4)
      call PGMOVE(xmin,0.0)
      call PGDRAW(xmax,0.0)
      call PGSLS(1)

      if (.NOT.FSEP) then
         call PGSCI(1)
         call PGPOINT(ntoas,x,y,16)
         if (ERRB) then
            do i=1,ntoas
               CALL pgerry(1,x(i),y(i)+ey(i),y(i)-ey(i),0.)
            enddo
         endif  
      else
         call PGSLW(1)

         if(COLOR)then
            call PGSCI(2)
            call PGPOINT(n1,xf1,yf1,16)
         else
            call PGPOINT(n1,xf1,yf1,7)
         endif
         if (ERRB) then
            do i=1,n1
               call PGERRY(1,xf1(i),yf1(i)+eyf1(i),yf1(i)-eyf1(i),0.)
            enddo
         endif

         if(COLOR)then         
            call PGSCI(6)
            call PGPOINT(n2,xf2,yf2,16)
         else
            call PGPOINT(n2,xf2,yf2,6)
         endif
         if (ERRB) then
            do i=1,n2
               CALL pgerry(1,xf2(i),yf2(i)+eyf2(i),yf2(i)-eyf2(i),0.)
            enddo
         endif

         if(COLOR)then
            call PGSCI(4)
            call PGPOINT(n3,xf3,yf3,16)
         else
            call PGPOINT(n3,xf3,yf3,9)
         endif
         if (ERRB) then
            do i=1,n3
               call PGERRY(1,xf3(i),yf3(i)+eyf3(i),yf3(i)-eyf3(i),0.)
            enddo
         endif

      endif

c      if(XMJD.or.XYR)then
c         call PGSLS(4)
c         call PGMOVE(xepoch,ymin)
c         call PGDRAW(xepoch,ymax)
c      endif

      call PGEND

      RETURN
      END
