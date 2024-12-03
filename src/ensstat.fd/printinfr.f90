subroutine printinfr(gfld,ivar)

! SUBPROGRAM:    printinfr         
!
! PRGMMR: Bo Cui         DATE: 2013-06-11
!
! PROGRAM HISTORY LOG: 
!   2024-11-27  Bo Cui: added to check for undefined values
!
! USAGE:   print grib2 data information
!
! INPUT:  gfld,ivar 

use grib_mod
use params

implicit none

type(gribfield) :: gfld

integer,dimension(200) :: jids,jpdt,jgdt,ipdt,igdt
integer :: currlen=0
logical :: unpack=.true.

integer kf,j,ivar,i
real    fldmin,fldmax,sample

kf=gfld%ngrdpts

if (gfld%fld(1) .eq. 9.9990003E+20 ) then  ! checking undefined values
  fldmax=0.0
  fldmin=99999.99
else
  fldmax=gfld%fld(1)
  fldmin=gfld%fld(1)
endif

do j=2,kf               
  if (gfld%fld(j) .eq. 9.9990003E+20 ) then ! checking undefined values
    cycle
  end if
  if (gfld%fld(j).gt.fldmax) fldmax=gfld%fld(j)
  if (gfld%fld(j).lt.fldmin) fldmin=gfld%fld(j)
enddo

if (gfld%fld(8601) .eq. 9.9990003E+20 ) then
  sample=-9999999.
else
  sample=gfld%fld(8601)
endif

! print out

! gfld%ipdtnum 1/11: ens. fcst or control, high reslution
! gfld%ipdtnum 2/12: ens. average fcst
! gfld%ipdtnum 0/8:  cdas reanalysis 

! gfld%ipdtnum 11: ens. fcst or control in a continuous or non-continuous time interval
! gfld%ipdtnum 12: derived fcst based on ens. members in a continuous/non-continuous time interval
! gfld%ipdtnum 8:  Rstatistically processed values in a continuous/non-continuous time interval

if(gfld%ipdtnum.eq.11.or.gfld%ipdtnum.eq.12) then
  write(6,100) 
  write(6,102) ivar,gfld%ipdtnum,(gfld%ipdtmpl(i),i=1,3),gfld%ipdtmpl(10),gfld%ipdtmpl(12),   &
                    (gfld%idsect(i),i=6,9),gfld%ipdtmpl(9),gfld%ipdtmpl(30),  &
                    (gfld%ipdtmpl(i),i=16,17), &
                     kf,fldmax,fldmin,sample          
  write(6,*) 

elseif(gfld%ipdtnum.eq.8.or.gfld%ipdtnum.eq.0) then
  write(6,300) 
  write(6,302) ivar,gfld%ipdtnum,(gfld%ipdtmpl(i),i=1,3),gfld%ipdtmpl(10),gfld%ipdtmpl(12),   &
                    (gfld%idsect(i),i=6,9),gfld%ipdtmpl(9), &
                     kf,fldmax,fldmin,sample           
  write(6,*) 

else
  write(6,200) 
  write(6,202) ivar,gfld%ipdtnum,(gfld%ipdtmpl(i),i=1,3),gfld%ipdtmpl(10),gfld%ipdtmpl(12),   &
                    (gfld%idsect(i),i=6,9),gfld%ipdtmpl(9),(gfld%ipdtmpl(i),i=16,17), &
                     kf,fldmax,fldmin,sample           
  write(6,*) 
endif

100 format(' REC PDTN PD1 PD2 PD3 PD10   PD12  YEAR MN DY HR FHR  TR ',  &
           'E16 E17    LEN       MAX         MIN      EXAMPLE')
102 format(i4,i5,4i4,i8,i6,3i3,2i4,2i4,i8,3f11.2)

200 format(' REC PDTN PD1 PD2 PD3 PD10   PD12  YEAR MN DY HR FHR ',  &
           'E16 E17    LEN       MAX         MIN      EXAMPLE')
202 format(i4,i5,4i4,i8,i6,3i3,3i4,i8,3f11.2)

300 format(' REC PDTN PD1 PD2 PD3 PD10   PD12  YEAR MN DY HR FHR ',  &
           '           LEN       MAX         MIN      EXAMPLE')
302 format(i4,i5,4i4,i8,i6,3i3,i4,8x,i8,3f11.2)

return
end
