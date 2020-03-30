      PROGRAM PLOTRES

c     Plots residuals using resid2.tmp
c     t:   barycentre TOA
c     r1:  pre-fit residual
c     r2:  post-fit residual
c     err: error of TOA
c     f:   barycentre frequency
      
      implicit none
      real*8 t(99999),f(99999),r1(99999),r2(99999),err(99999),
     :     phase(99999)
      real*8 period,pepoch,flim1,flim2,dDM,var1,var2
      real xp(99999),yp(99999),eyp(99999),
     :     xmin,xmax,ymin,ymax,xmin1,xmax1,ymin1,ymax1,xepoch,ctroph,
     :     xc,yc,xcc,ycc
      integer ii,iii,ntoas,icurse,PGCURSE,n1,n2,n3,ibtt,lpath,
     :     istat,system
      character key,ckey,name(99999)*15,button*2,path*80
      logical ERRB,FSEP,PREFITON,BNRY,XZOOM,YZOOM,ROMAN,COLOR,
     :     XMJD,XYR,XOPH,XNB,YPH,YMU,YMS

      common/DATA/ t,r1,r2,err,phase,period,pepoch
      common/DATAc/ name
      common/TOA/ ntoas,xp
      common/AXIS/ XMJD,XYR,XOPH,XNB,YPH,YMU,YMS
      common/XMYM/ xmin,xmax,ymin,ymax,xepoch,ctroph
      common/PLOT/ yp,eyp
      common/SWITCH/ PREFITON,ERRB,FSEP,ROMAN
      common/CUR/ xc,yc
      common/FREQ/ f,flim1,flim2
      common/COLOUR/ n1,n2,n3

      call GETENV('TEMPO_PLOTRES',path)
      lpath=index(path,' ')-1

      BNRY     = .false.
      PREFITON = .true.
      XMJD     = .true.
      XYR      = .false.
      XOPH     = .false.
      XNB      = .false.
      YPH      = .true.
      YMU      = .false.
      YMS      = .false.
      XZOOM    = .false.
      YZOOM    = .false.
      ERRB     = .true.
      FSEP     = .false.
      ROMAN    = .false.
      
      write(*,'(/'' ==========  PLOT RESIDUALS  ==========''/)')

      call LOADRES(ntoas,BNRY)

      if(flim1.gt.0)     write(*,'(1x,12Hflim1 (MHz): ,f13.3)') flim1
      if(flim2.lt.1.d29) write(*,'(1x,12Hflim2 (MHz): ,f13.3)') flim2

      call INTERFACE(BNRY)

* --- pre-fit ---

      call PLOTSET(PREFITON,ERRB)
      call SETMARK(XZOOM,YZOOM)
      call PLOTR()

* ---------------------
* -- MENUE ------------
* ---------------------

 100  call GETBUTTON(button,ibtt)
      call WINDOW(2)

* --- exit ---

      if (button.eq.'q ') goto 999

* --- pre-fit ---

      if (button.eq.'a ') PREFITON=.true.

* --- post-fit ---

      if (button.eq.'b ') PREFITON=.false.

* --- x-axis: MJD ---

      if (button.eq.'x1'.and..NOT.XMJD) then
         XMJD  = .true.
         XYR   = .false.
         XNB   = .false.
         XOPH  = .false.
         XZOOM = .false.
      endif

* --- x-axis: years ---

      if (button.eq.'x2'.and..NOT.XYR) then
         XMJD=.false.
         XYR =.true.
         XNB =.false.
         XOPH =.false.
         XZOOM = .false.
      endif

* --- x-axis: Nb. TOA ---
      
      if (button.eq.'x3'.and..NOT.XNB) then
         XMJD=.false.
         XYR =.false.
         XNB =.true.
         XOPH =.false.
         XZOOM = .false.
      endif

* --- x-axis: orbital phase ---
      
      if(button.eq.'x4'.and.BNRY)then
         XMJD=.false.
         XYR =.false.
         XNB =.false.
         XOPH =.true.
         XZOOM = .false.
         call MARK(ibtt,2)         
         call XTERM()
         write(*,'(/'' Center of plot (0..1): '',$)')
         read(*,*) ctroph
         ctroph=ctroph-INT(ctroph)
         if(ctroph.lt.0) ctroph=ctroph+1.0
      endif

* --- y-axis: phase ---

      if(button.eq.'y1'.and..NOT.YPH)then
         YPH =.true.
         YMU =.false.
         YMS =.false.
         YZOOM = .false.
      endif

* --- y-axis: mus ---

      if(button.eq.'y2'.and..NOT.YMU)then
         YPH =.false.
         YMU =.true.
         YMS =.false.
         YZOOM = .false.
      endif
      
* --- y-axis: ms ---

      if(button.eq.'y3'.and..NOT.YMS)then
         YPH =.false.
         YMU =.false.
         YMS =.true.
         YZOOM = .false.
      endif
      
* --- X zoom in ---

      if(button.eq.'zx')then
         call MARK(ibtt,2)         
         write(*,'(/'' Click at X1 and X2'')')
         xc=xmin
         yc=0.
         icurse=PGCURSE(xc,yc,key)
         if(key.ne.'A') goto 301
         xcc=xc
         icurse=PGCURSE(xc,yc,key)         
         if(key.ne.'A') goto 301
         xmin1=xcc
         xmax1=xc
         if (xmax.lt.xmin) then
            xmin1=xc
            xmax1=xcc
         endif
 301     XZOOM=.true.
      endif

* --- X full size ---

      if(button.eq.'xx') XZOOM=.false.
      
* --- Y zoom in ---

      if(button.eq.'zy')then
         call MARK(ibtt,2)         
         write(*,'(/'' Click at Y1 and Y2'')')         
         xc=xmin
         yc=0.
         icurse=PGCURSE(xc,yc,key)
         if(key.ne.'A') goto 302
         ycc=yc
         icurse=PGCURSE(xc,yc,key)         
         if(key.ne.'A') goto 302
         ymin1=ycc
         ymax1=yc
         if (ymax.lt.ymin) then
            ymin1=yc
            ymax1=ycc
         endif
 302     YZOOM=.true.
      endif

* --- Y full size ---

      if (button.eq.'yy') YZOOM=.false.

* --- error bars ON/OFF ---
      
      if (button.eq.'e ') then 
         if(ERRB)then
            ERRB=.false. 
         else
            ERRB=.true.
         endif
      endif

* --- select frequency range ---

      if (button.eq.'f ') then
         if(FSEP)then
            FSEP=.false.
         else
            FSEP=.true.
            call MARK(ibtt,2)
            call XTERM()
            write(*,'(/'' 3 frequency ranges: 0..f1..f2..'')')
            write(*,'('' f1, f2 (GHz): '',$)')
            read(*,*) flim1,flim2
            flim1=flim1*1.d3
            flim2=flim2*1.d3
         endif

         if(FSEP) then
            print*
            write(*,'(/''Three frequency ranges (GHz):'')')
            write(*,'(10H red:     ,f5.2,4H to ,f5.2,3H  [,i4,1H])') 
     :           0.0,flim1/1.d3,n1
            write(*,'(10H yellow:  ,f5.2,4H to ,f5.2,3H  [,i4,1H])') 
     :           flim1/1.d3,flim2/1.d3,n2
            write(*,'(13H green:    > ,f5.2,9H        [,i4,1H])') 
     :           flim2/1.d3,n3
         endif
      endif   

* --- click on TOAs ---

      if (button.eq.'ct') then
         call MARK(ibtt,2)
         xc=xmin
         yc=0.
         write(*,'(/'' Right mouse button -> STOP'')')
 710     call CLOSEST(ckey,ii)
         if (ckey.ne.'X') then
            if(PREFITON)      write(*,'('' Nb.: '',i5,'' / TOA: '',
     :           f11.5,'' / freq.: '',f8.2,'' / res.: '',f12.5)') 
     :           ii,t(ii),f(ii),r1(ii)
            if(.NOT.PREFITON) write(*,'('' Nb.: '',i5,'' / TOA: '',
     :           f11.5,'' / freq.: '',f8.2,'' / res.: '',f12.5)') 
     :           ii,t(ii),f(ii),r2(ii)
            goto 710
         else
            goto 720
         endif
 720     call MARK(ibtt,0)
      endif

* --- DM difference ---

      if (button.eq.'dm') then
         call MARK(ibtt,2)
         write(*,'(/'' Right mouse button -> STOP'')')
 805     xc=xmin
         yc=0.
 810     call CLOSEST(ckey,ii)
         if(ckey.eq.'X') goto 820
         print*

         if (PREFITON) then
            write(*,'(8H 1.TOA: ,f12.6,2x,f8.5,2x,f12.6,2x,a15)') 
     :           t(ii),r1(ii),f(ii),name(ii)
         else
            write(*,'(8H 1.TOA: ,f12.6,2x,f8.5,2x,f12.6,2x,a15)') 
     :           t(ii),r2(ii),f(ii),name(ii)
         endif
         call CLOSEST(ckey,iii)
         if(ckey.eq.'X') goto 820
         if (PREFITON) then
            write(*,'(8H 2.TOA: ,f12.6,2x,f8.5,2x,f12.6,2x,a15)') 
     :           t(iii),r1(iii),f(iii),name(iii)
         else
            write(*,'(8H 2.TOA: ,f12.6,2x,f8.5,2x,f12.6,2x,a15)') 
     :           t(iii),r2(iii),f(iii),name(iii)
         endif

         if (PREFITON) then
            var1=(r1(ii)-r1(iii))*period*1.d3/4.15
         else
            var1=(r2(ii)-r2(iii))/1.d3/4.15
         endif
         var2=1d6*(1.0/f(ii)**2-1.0/f(iii)**2)
         dDM=var1/var2
         write(*,'(15H dDM (pc/cm3): ,f12.6)') dDM

 820     call MARK(ibtt,0)
      endif

c --- change fond ---

      if(button.eq.'rm')then
         if(ROMAN)then
            ROMAN=.false.
         else
            ROMAN=.true.
         endif
      endif

c --- plot ---

      call SETMARK(XZOOM,YZOOM)
      call PLOTSET(PREFITON,ERRB)

      if(XZOOM)then
         xmin=xmin1
         xmax=xmax1
      endif
      if(YZOOM)then
         ymin=ymin1
         ymax=ymax1
      endif

      if(button.ne.'pl' .and. button.ne.'pc')then
         call PLOTR()
      else
         COLOR=.false.
         if(button.eq.'pc') COLOR=.true.

         write(*,'(/'' Plotting to pgplot.ps ... '',$)')

         open(14,file='switch.tmp')
         write(14,*) xmin,xmax,ymin,ymax
         write(14,'(l1)') PREFITON
         write(14,'(l1)') XMJD
         write(14,'(l1)') XYR
         write(14,'(l1)') XNB
         write(14,'(l1)') XOPH
         write(14,'(l1)') YPH
         write(14,'(l1)') YMU
         write(14,'(l1)') YMS
         write(14,'(l1)') ERRB
         write(14,'(l1)') FSEP
         write(14,'(l1)') ROMAN
         write(14,'(l1)') COLOR
         write(14,*) flim1,flim2
         write(14,*) ctroph
         close(14)

         istat=system(path(1:lpath)//'plotres_ps')
         istat=system('rm switch.tmp')

         write(*,'(''finished'')')
      endif              

      goto 100

 999  call PGEND
      print*
      END

*=======================================================================

      SUBROUTINE LOADRES(ntoas,BNRY)

* Reads TOAs (t), frequency (f), pre-fit (r1) and post-fit (r2) 
* residuals from resid2.tmp 

      implicit none

      real*8 t(99999),f(99999),r1(99999),r2(99999),err(99999),
     :     phase(99999)
      real*8 period,pepoch,flim1,flim2
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

      write(*,'('' Nb. of TOAs:       '',i4)') ntoas

      flim1=0
      flim2=1.0d99

      open(11,file='tempo.lis',status='old')
 103  read(11,'(a)') line
      if(line(1:3).ne.'PSR') goto 103
      read(line,'(65x,f15.0)') pepoch
      close(11)

      RETURN
      END

*=======================================================================

      SUBROUTINE PLOTSET(PREFITON,ERRB)

      implicit none

      integer*4 i,ntoas
      real*8 t(99999),r1(99999),r2(99999),err(99999),phase(99999),
     +     period,pepoch
      real x(99999),y(99999),ey(99999)
      real xmin,xmax,ymin,ymax,h,xepoch,ctroph,ef
      logical PREFITON,ERRB,XMJD,XYR,XOPH,XNB,YPH,YMU,YMS

      common/DATA/ t,r1,r2,err,phase,period,pepoch
      common/AXIS/ XMJD,XYR,XOPH,XNB,YPH,YMU,YMS
      common/TOA/ ntoas,x
      common/XMYM/ xmin,xmax,ymin,ymax,xepoch,ctroph
      common/PLOT/ y,ey

      ef=0.
      if(ERRB) ef=1.

      xmin=+1E30
      xmax=-1E30
      ymin=+1E30
      ymax=-1E30

      do i=1,ntoas
         if(XMJD) x(i)=t(i)-50000.0
         if(XYR)  x(i)=(t(i)-39126.0)/365.25+1966
         if(XOPH)then
            x(i)=phase(i)
            if(x(i).lt.ctroph-0.5) x(i)=x(i)+1.0
            if(x(i).ge.ctroph+0.5) x(i)=x(i)-1.0
         endif
         if(XNB)  x(i)=i
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
         if (.NOT.XOPH) then
            if(xmin.gt.x(i)) xmin=x(i)
            if(xmax.lt.x(i)) xmax=x(i)
         endif
         if(ymin.gt.y(i)-ey(i)*ef) ymin=y(i)-ey(i)*ef
         if(ymax.lt.y(i)+ey(i)*ef) ymax=y(i)+ey(i)*ef
      enddo
      if (XOPH) then
         xmin=ctroph-0.5
         xmax=ctroph+0.5
      endif

      if (.NOT.XOPH) then
         h=xmax-xmin
         xmin=xmin-0.05*h
         xmax=xmax+0.05*h
      endif
      h=ymax-ymin
      ymin=ymin-0.05*h
      ymax=ymax+0.05*h

      if(XMJD) xepoch=pepoch-50000.0
      if(XYR)  xepoch=(pepoch-39126.0)/365.25+1966      
      
      RETURN
      END

*=======================================================================

      SUBROUTINE PLOTR()

      implicit none

      real*8 f(99999),flim1,flim2
      real x(99999),y(99999),ey(99999),xepoch,ctroph
      real xf1(99999),yf1(99999),eyf1(99999)
      real xf2(99999),yf2(99999),eyf2(99999)
      real xf3(99999),yf3(99999),eyf3(99999)
      real xmin,xmax,ymin,ymax
      integer i,ntoas,n1,n2,n3
      character*30 XSTR30,YSTR30
      logical ERRB,FSEP,PREFITON,ROMAN,XMJD,XYR,XOPH,XNB,YPH,YMU,YMS

      common/SWITCH/ PREFITON,ERRB,FSEP,ROMAN
      common/AXIS/ XMJD,XYR,XOPH,XNB,YPH,YMU,YMS

      common/TOA/ ntoas,x
      common/XMYM/ xmin,xmax,ymin,ymax,xepoch,ctroph
      common/PLOT/ y,ey
      common/FREQ/ f,flim1,flim2
      common/COLOUR/ n1,n2,n3

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

      call CLEARW()
      call PGSCI(1)
      call PGSLS(1)
      call PGSLW(1)
      call PGSCF(3)
      call PGSCH(1.4)
      if(ROMAN)then
         call PGSCF(2)
      else
         call PGSCF(3)
      endif

      call WINDOW(2)
      call PGBOX('bcnts',0.,0,'bcnts',0.,0)
      
      if(XMJD) XSTR30='MJD-50000.0'
      if(XYR)  XSTR30='year'
      if(XOPH)  XSTR30='orbital phase'
      if(XNB)  XSTR30='Nb. of TOA'
      if(YPH)  YSTR30='residual (phase)'
      if(YMU)  YSTR30='residual (us)'
      if(YMS)  YSTR30='residual (ms)'

      call PGLAB(XSTR30,YSTR30,' ')
      
      call PGSLS(4)
      call PGMOVE(xmin,0.0)
      call PGDRAW(xmax,0.0)
      call PGSLS(1)

      if (.NOT.FSEP) then
         call PGSCI(7)
         call PGPOINT(ntoas,x,y,16)
         if (ERRB) then
            do i=1,ntoas
               CALL pgerry(1,x(i),y(i)+ey(i),y(i)-ey(i),0.)
            enddo
         endif  
      else
         call PGSCI(2)
         CALL pgpoint(n1,xf1,yf1,16)
         if (ERRB) then
            do i=1,n1
               CALL pgerry(1,xf1(i),yf1(i)+eyf1(i),yf1(i)-eyf1(i),0.)
            enddo
         endif
         call PGSCI(7)
         CALL pgpoint(n2,xf2,yf2,16)
         if (ERRB) then
            do i=1,n2
               CALL pgerry(1,xf2(i),yf2(i)+eyf2(i),yf2(i)-eyf2(i),0.)
            enddo
         endif
         call PGSCI(3)
         CALL pgpoint(n3,xf3,yf3,16)
         if (ERRB) then
            do i=1,n3
               CALL pgerry(1,xf3(i),yf3(i)+eyf3(i),yf3(i)-eyf3(i),0.)
            enddo
         endif
      endif       
      
      if(XMJD.or.XYR)then
         call PGSCI(1)
         call PGSLS(4)
         call PGMOVE(xepoch,ymin)
         call PGDRAW(xepoch,ymax)
      endif

      RETURN
      END

*=======================================================================

      SUBROUTINE CLOSEST(key,ii)

      implicit none
      real xp(99999),yp(99999),eyp(99999)
      real xc,yc,d,d0,toa,res,xx,yy,x,y
      real xmin,xmax,ymin,ymax,xepoch,ctroph
      integer i,ii,ntoa,icurse,PGCURSE
      character key*1

      external PGCURSE

      common/CUR/ xc,yc
      common/TOA/ ntoa,xp
      common/XMYM/ xmin,xmax,ymin,ymax,xepoch,ctroph
      common/PLOT/ yp,eyp

      xx=xmax-xmin
      yy=ymax-ymin

 100  icurse=PGCURSE(xc,yc,key)
      if(key.eq.'X') goto 110

      d0=10.
      do i=1,ntoa
         x=(xp(i)-xc)*1.9/xx
         y=(yp(i)-yc)*1.0/yy
         d=SQRT(x*x+y*y)
         if(d.lt.d0) then
            ii=i
            d0=d
         endif
      enddo

      if (d0.lt.0.02) then
         toa=xp(ii)
         res=yp(ii)
         call PGSCI(4)
         call PGPOINT(1,toa,res,5)
      else
         goto 100
      endif

 110  RETURN
      END

*=======================================================================

      SUBROUTINE GETBUTTON(button,i)

      implicit none
      real x(99),y(99),r0,r,xc,yc
      integer i,n,icurse,PGCURSE
      external PGCURSE
      character key*1,button*2,buttonkey(99)*2

      common/BK/buttonkey
      common/circ/ x,y,r0,n
      
      call WINDOW(1)

 100  icurse=PGCURSE(xc,yc,key) 
      do i=1,n
         r=SQRT((xc-x(i))**2+(yc-y(i))**2)
         if (r.lt.r0) then
            button=buttonkey(i)
            goto 999
         endif
      enddo

      if(xc.ge.1.and.xc.le.27.and.yc.ge.1.and.yc.le.5) then
         button='q '
         goto 999
      endif
      goto 100

 999  RETURN
      END
      
*=======================================================================

      SUBROUTINE WINDOW(i)
      
      real xmin,xmax,ymin,ymax,xepoch,ctroph
      integer i
      common/XMYM/ xmin,xmax,ymin,ymax,xepoch,ctroph

      if (i.eq.1) then
         call PGVPORT(0.,1.,0.,1.)
         call PGWINDOW(0.,200.,0.,120.)
      endif

      if (i.eq.2) then
         call PGVPORT(0.08,.98,0.3,.98)
         call PGWINDOW(xmin,xmax,ymin,ymax)
      endif

      RETURN
      END

*=======================================================================      

      SUBROUTINE CLEARW()

      real h
      common/H/ h

      call WINDOW(1)
      call PGSFS(1)
      call PGSCI(0)
      call PGRECT(1.,199.,h+1,119.)
      call PGSFS(2)

      RETURN
      END

*=======================================================================
 
      SUBROUTINE XTERM()

      real x,y
      real xmin,xmax,ymin,ymax,pepoch,ctroph
      common/XMYM/ xmin,xmax,ymin,ymax,pepoch,ctroph

      x=xmin+(xmax-xmin)*0.5
      y=ymin+(ymax-ymin)*0.45

      call PGSCH(4.)
      call PGSCI(5)
      call PGPTEXT(x,y,0.,0.5,'XTERM INPUT !')

      RETURN
      END

*=======================================================================

      SUBROUTINE MARK(i,icolor)

      implicit none
      real x(99),y(99),r
      integer i,n,icolor
      common/circ/ x,y,r,n

      call WINDOW(1)

      call PGSFS(1)
      call PGSCI(icolor)
      if(i.le.n) call PGCIRC(x(i),y(i),0.6*r)
      call PGSFS(2)

      call WINDOW(2)

      RETURN
      END

*=======================================================================
*=======================================================================

      SUBROUTINE SETMARK(XZOOM,YZOOM)

      implicit none

      integer i
      logical PREFITON,XZOOM,YZOOM,ERRB,FSEP,ROMAN,
     :     XMJD,XYR,XOPH,XNB,YPH,YMU,YMS

      common/SWITCH/ PREFITON,ERRB,FSEP,ROMAN
      common/AXIS/ XMJD,XYR,XOPH,XNB,YPH,YMU,YMS

      do i=1,99
         call MARK(i,0)
      enddo      

      if(     PREFITON) call MARK( 1,3)
      if(.NOT.PREFITON) call MARK( 2,3)
      if(XMJD)          call MARK( 3,3)
      if(XYR)           call MARK( 4,3)
      if(XNB)           call MARK( 5,3)
      if(XOPH)          call MARK( 6,7)
      if(YPH)           call MARK( 7,3)
      if(YMU)           call MARK( 8,3)
      if(YMS)           call MARK( 9,3)
      if(XZOOM)         call MARK(10,7)
      if(.NOT.XZOOM)    call MARK(11,3)
      if(YZOOM)         call MARK(12,7)
      if(.NOT.YZOOM)    call MARK(13,3)
      if(ERRB)          call MARK(14,7)
      if(FSEP)          call MARK(15,7)
      if(ROMAN)         call MARK(18,7)
      call MARK(19,1)
      call MARK(20,1)

      RETURN
      END

*=======================================================================

      SUBROUTINE INTERFACE(BNRY)

      implicit none
      integer i,n,PGBEG
      real x(99),y(99),r,dx,dy,h
      character text(99)*80,buttonkey(99)*2
      logical BNRY

      common/BK/buttonkey
      common/circ/ x,y,r,n
      common/H/ h

      data n/20/
      data (x(i),i=1,20)/3,3,
     :     27,27,27,27,
     :     53,53,53,
     :     75,75,75,75,
     :     100,100,100,100,
     :     129,129,129/
      data (y(i),i=1,20)/15,10,
     :     15.5,11.16,6.83,2.5,
     :     15.5,11.16,6.83,
     :     15.5,11.16,6.83,2.5,
     :     15.5,11.16,6.83,2.5,
     :     15.5,11.16,6.83/


      r=1.6
      dx=1.5*r
      dy=-0.7*r

      IF (PGBEG(0,'/xw',1,1) .NE. 1) STOP ! for UNIX 
      call PGASK(.false.)
      call PGPAPER(10.,0.6)
      call PGSCF(1)
      call PGSFS(2)
      call PGSCH(1.2)

      call WINDOW(1)

      text(1)='PRE-FIT'
      buttonkey(1)='a '
      text(2)='POST-FIT'
      buttonkey(2)='b '
      text(3)='X: MJD'
      buttonkey(3)='x1'
      text(4)='X: years'
      buttonkey(4)='x2'
      text(5)='X: Nb. TOA'
      buttonkey(5)='x3'
      if(BNRY)then
         text(6)='X: orbit'
         buttonkey(6)='x4'
      else
         text(6)=' '
         buttonkey(6)='  '
      endif
      text(7)='Y: phase'
      buttonkey(7)='y1'
      text(8)='Y: us'
      buttonkey(8)='y2'
      text(9)='Y: ms'
      buttonkey(9)='y3'
      text(10)='X zoom in'
      buttonkey(10)='zx'
      text(11)='X full size'
      buttonkey(11)='xx'
      text(12)='Y zoom in'
      buttonkey(12)='zy'
      text(13)='Y full size'
      buttonkey(13)='yy'
      text(14)='error bars'
      buttonkey(14)='e '
      text(15)='3 freq.'
      buttonkey(15)='f '
      text(16)='click on TOA'
      buttonkey(16)='ct'
      text(17)='DM diff.'
      buttonkey(17)='dm'
      text(18)='Font: rm'
      buttonkey(18)='rm'
      text(19)='pgplot.ps'
      buttonkey(19)='pl'
      text(20)='pgplot.ps (c)'
      buttonkey(20)='pc'

      call PGSCI(2)
      h=y(1)+4
      call PGRECT(0.,200.,h,120.)
      call PGRECT(0.      ,x(3)-dx-1,7.,18.)
      call PGRECT(x( 3)-dx,x(7)-dx-1,0.,18.)
      call PGRECT(x( 7)-dx,x(10)-dx-1.,0.,18.)
      call PGRECT(x(10)-dx,x(14)-dx-1.,0.,18.)
      call PGRECT(x(14)-dx,x(18)-dx-1.,0.,18.)
      call PGRECT(x(19)-dx,x(19)+26,0.,18.)

      call PGSCI(1)
      do i=1,n
         call PGCIRC(x(i),y(i),r)
         call PGPTEXT(x(i)+dx,y(i)+dy,0.,0.,text(i))
      enddo

      call PGSCI(2)
      call PGRECT(0.,x(3)-dx-1,0.,6.)
      call PGSCI(1)
      call PGRECT(1.,x(3)-dx-2,1.,5.)
      call PGPTEXT(0.5*(x(3)-dx-1),2.,0.,.5,'EXIT')

      RETURN
      END

