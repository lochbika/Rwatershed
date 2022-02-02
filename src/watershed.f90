subroutine watershed_simple(indata,outdata,nx,ny,periodic)

  implicit none
  ! particle and local maxima coordinate arrays
  integer, allocatable :: ipartcoor(:,:)  ! initial particle coordinates
  integer, allocatable :: tpartcoor(:,:)  ! temporary particle coordinates
  integer, allocatable :: partcoor(:,:)   ! final particle coordinates
  integer, allocatable :: locmaxcoor(:,:) ! local maxima coordinates
  ! number of particles and local maxima
  integer :: npart,nlocmax
  ! iterators
  integer :: x,y,i,tp
  ! in and output for subroutine
  integer, intent(in) :: nx,ny              ! dimension sizes of io data
  real(kind=8), intent(in) :: indata(nx,ny) ! input data matrix
  integer, intent(out) :: outdata(nx,ny)    ! output data matrix
  logical, intent(in) :: periodic           ! use periodic boundaries?
  ! data for local maximum and direction detection
  real(kind=8) :: lmaxt,neighb(9),fillval
  integer :: dir(nx,ny)
  logical :: locmax(nx,ny)

  ! initialize variables and arrays
  fillval=HUGE(fillval)
  outdata=0
  locmax=.false.
  dir=0
  npart=nx*ny
  nlocmax=0

  ! find local minima and directions
  do y=1,ny
    do x=1,nx
      ! extract the 8 neighboring grid points
      neighb=fillval
      ! the center gridpoint is always assigned the same way
      neighb(5)=indata(x,y)
      ! for the rest, we have to take care of the boundaries
      ! we are in the upper left corner
      if(x.eq.1 .AND. y.eq.1)then
        neighb(6)=indata(x+1,y)
        neighb(8)=indata(x,y+1)
        neighb(9)=indata(x+1,y+1)
        if(periodic)then
          neighb(1)=indata(nx,ny)
          neighb(2)=indata(x,ny)
          neighb(3)=indata(x+1,ny)
          neighb(4)=indata(nx,y)
          neighb(7)=indata(nx,y+1)
        end if
      ! we are in the upper right corner
      else if(x.eq.nx .AND. y.eq.1)then
        neighb(4)=indata(x-1,y)
        neighb(7)=indata(x-1,y+1)
        neighb(8)=indata(x,y+1)
        if(periodic)then
          neighb(1)=indata(x-1,ny)
          neighb(2)=indata(x,ny)
          neighb(3)=indata(1,ny)
          neighb(6)=indata(1,y)
          neighb(9)=indata(1,y+1)
        end if
      ! we are in the lower left corner
      else if(x.eq.1 .AND. y.eq.ny)then
        neighb(2)=indata(x,y-1)
        neighb(3)=indata(x+1,y-1)
        neighb(6)=indata(x+1,y)
        if(periodic)then
          neighb(1)=indata(nx,y-1)
          neighb(4)=indata(nx,y)
          neighb(7)=indata(nx,1)
          neighb(8)=indata(x,1)
          neighb(9)=indata(x+1,1)
        end if
      ! we are in the lower right corner
      else if(x.eq.nx .AND. y.eq.ny)then
        neighb(1)=indata(x-1,y-1)
        neighb(2)=indata(x,y-1)
        neighb(4)=indata(x-1,y)
        if(periodic)then
          neighb(3)=indata(1,y-1)
          neighb(6)=indata(1,y)
          neighb(7)=indata(x-1,1)
          neighb(8)=indata(x,1)
          neighb(9)=indata(1,1)
        end if
      ! we are in between upper left and lower left corner
      else if(x.eq.1 .AND. y.ne.1 .AND. y.ne.ny)then
        neighb(2)=indata(x,y-1)
        neighb(3)=indata(x+1,y-1)
        neighb(6)=indata(x+1,y)
        neighb(8)=indata(x,y+1)
        neighb(9)=indata(x+1,y+1)
        if(periodic)then
          neighb(1)=indata(nx,y-1)
          neighb(4)=indata(nx,y)
          neighb(7)=indata(nx,y+1)
        end if
      ! we are in between upper left and upper right corner
      else if(y.eq.1 .AND. x.ne.1 .AND. x.ne.nx)then
        neighb(4)=indata(x-1,y)
        neighb(6)=indata(x+1,y)
        neighb(7)=indata(x-1,y+1)
        neighb(8)=indata(x,y+1)
        neighb(9)=indata(x+1,y+1)
        if(periodic)then
          neighb(1)=indata(x-1,ny)
          neighb(2)=indata(x,ny)
          neighb(3)=indata(x+1,ny)
        end if
      ! we are in between lower left and lower right corner
      else if(y.eq.ny .AND. x.ne.1 .AND. x.ne.nx)then
        neighb(1)=indata(x-1,y-1)
        neighb(2)=indata(x,y-1)
        neighb(3)=indata(x+1,y-1)
        neighb(4)=indata(x-1,y)
        neighb(6)=indata(x+1,y)
        if(periodic)then
          neighb(7)=indata(x-1,1)
          neighb(8)=indata(x,1)
          neighb(9)=indata(x+1,1)
        end if
      ! we are in between upper right and lower right corner
      else if(x.eq.nx .AND. y.ne.1 .AND. y.ne.ny)then
        neighb(1)=indata(x-1,y-1)
        neighb(2)=indata(x,y-1)
        neighb(4)=indata(x-1,y)
        neighb(7)=indata(x-1,y+1)
        neighb(8)=indata(x,y+1)
        if(periodic)then
          neighb(3)=indata(1,y-1)
          neighb(6)=indata(1,y)
          neighb(9)=indata(1,y+1)
        end if
      ! NOW, this is the case when we don't have to care about the boundaries
      else
        neighb(1)=indata(x-1,y-1)
        neighb(2)=indata(x,y-1)
        neighb(3)=indata(x+1,y-1)
        neighb(4)=indata(x-1,y)
        neighb(6)=indata(x+1,y)
        neighb(7)=indata(x-1,y+1)
        neighb(8)=indata(x,y+1)
        neighb(9)=indata(x+1,y+1)
      end if

      ! so, now we know the neighboring grid points
      ! is this grid point a local minimum??
      lmaxt=fillval
      tp=0
      do i=1,9
        if(neighb(i)<=lmaxt)then
          tp=i
          lmaxt=neighb(i)
        end if
      end do
      if(tp.eq.5)then
        locmax(x,y)=.true.
        nlocmax=nlocmax+1
      end if
      ! save the direction
      dir(x,y)=tp
    end do
  end do

  ! we have the directions for each pixel
  ! and know which of them are local minima

  ! lets seed drops and let them flow to them
  ! get their initial coordinates
  allocate(ipartcoor(npart,2),partcoor(npart,2),locmaxcoor(nlocmax,2))
  tp=0 ! counter for locmax
  i=0  ! counter for particles
  do y=1,ny
    do x=1,nx
      i=i+1
      ipartcoor(i,1)=x
      ipartcoor(i,2)=y
      partcoor(i,1)=x
      partcoor(i,2)=y
      if(locmax(x,y))then
        tp=tp+1
        locmaxcoor(tp,1)=x
        locmaxcoor(tp,2)=y
      end if
    end do
  end do

  ! now start the iterative process to move particles
  allocate(tpartcoor(npart,2))

  do i=1,npart ! loop to move particles
    tpartcoor=partcoor
    do tp=1,npart ! move each particle in this iteration
      ! is this particle already at a local maximum? Then, don't move it!
      if(locmax( tpartcoor(tp,1),tpartcoor(tp,2) ))then
        cycle
      end if
      ! otherwise, get direction and move it!
      if(dir( tpartcoor(tp,1),tpartcoor(tp,2) ).eq.1)then
        tpartcoor(tp,1)=tpartcoor(tp,1)-1
        tpartcoor(tp,2)=tpartcoor(tp,2)-1
      else if(dir( tpartcoor(tp,1),tpartcoor(tp,2) ).eq.2)then
        tpartcoor(tp,2)=tpartcoor(tp,2)-1
      else if(dir( tpartcoor(tp,1),tpartcoor(tp,2) ).eq.3)then
        tpartcoor(tp,1)=tpartcoor(tp,1)+1
        tpartcoor(tp,2)=tpartcoor(tp,2)-1
      else if(dir( tpartcoor(tp,1),tpartcoor(tp,2) ).eq.4)then
        tpartcoor(tp,1)=tpartcoor(tp,1)-1
      else if(dir( tpartcoor(tp,1),tpartcoor(tp,2) ).eq.6)then
        tpartcoor(tp,1)=tpartcoor(tp,1)+1
      else if(dir( tpartcoor(tp,1),tpartcoor(tp,2) ).eq.7)then
        tpartcoor(tp,1)=tpartcoor(tp,1)-1
        tpartcoor(tp,2)=tpartcoor(tp,2)+1
      else if(dir( tpartcoor(tp,1),tpartcoor(tp,2) ).eq.8)then
        tpartcoor(tp,2)=tpartcoor(tp,2)+1
      else if(dir( tpartcoor(tp,1),tpartcoor(tp,2) ).eq.9)then
        tpartcoor(tp,1)=tpartcoor(tp,1)+1
        tpartcoor(tp,2)=tpartcoor(tp,2)+1
      end if
      ! check if we moved across boundaries
      if(periodic)then
        if(tpartcoor(tp,1)<1)tpartcoor(tp,1)=nx
        if(tpartcoor(tp,1)>nx)tpartcoor(tp,1)=1
        if(tpartcoor(tp,2)<1)tpartcoor(tp,2)=ny
        if(tpartcoor(tp,2)>ny)tpartcoor(tp,2)=1
      else ! reset
        if(tpartcoor(tp,1)<1)tpartcoor(tp,1)=partcoor(tp,1)
        if(tpartcoor(tp,1)>nx)tpartcoor(tp,1)=partcoor(tp,1)
        if(tpartcoor(tp,2)<1)tpartcoor(tp,2)=partcoor(tp,2)
        if(tpartcoor(tp,2)>ny)tpartcoor(tp,2)=partcoor(tp,2)
      end if
    end do

    ! check if we moved any particle
    if(ALL(partcoor==tpartcoor))then
      !write(*,*)"We finished after",i," iterations!"
      exit
    end if
    ! update partcoor
    partcoor=tpartcoor
  end do

  ! now find which particles ended up in which local maximum
  ! and assign IDs to gridpoints
  do i=1,npart
    do tp=1,nlocmax
      ! if coordinates match assign ID to gridpoint where the particle was originally
      if(partcoor(i,1).eq.locmaxcoor(tp,1) .AND. partcoor(i,2).eq.locmaxcoor(tp,2))then
        outdata( ipartcoor(i,1),ipartcoor(i,2) ) = tp
        exit
      end if
    end do
  end do

end subroutine watershed_simple
