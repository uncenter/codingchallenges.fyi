program ccwc

   implicit none

   integer :: i, numberOfArguments
   character(len=:), allocatable :: argumentValue
   character(len=:), allocatable :: nextArgumentValue
   integer :: recentResult, skipNextResult
   integer, dimension(3) :: results

   numberOfArguments = COMMAND_ARGUMENT_COUNT()

   do i = 1, numberOfArguments

      if (skipNextResult == 1) then
         skipNextResult = 0
         cycle
      end if

      allocate (character(len=256) :: argumentValue)
      allocate (character(len=256) :: nextArgumentValue)

      call GET_COMMAND_ARGUMENT(i, argumentValue)
      call GET_COMMAND_ARGUMENT(i + 1, nextArgumentValue)

      select case (trim(argumentValue))
      case ('-c')
         skipNextResult = 1
         call CountBytes(trim(nextArgumentValue), recentResult)
         print *, recentResult, trim(nextArgumentValue)
      case ('-l')
         skipNextResult = 1
         call CountLines(trim(nextArgumentValue), recentResult)
         print *, recentResult, trim(nextArgumentValue)
      case ('-w')
         skipNextResult = 1
         call CountWords(trim(nextArgumentValue), recentResult)
         print *, recentResult, trim(nextArgumentValue)
      case default
         call CountLines(trim(argumentValue), recentResult)
         results(1) = recentResult
         recentResult = 0
         call CountWords(trim(argumentValue), recentResult)
         results(2) = recentResult
         recentResult = 0
         call CountBytes(trim(argumentValue), recentResult)
         results(3) = recentResult
         print *, results, trim(argumentValue)
      end select

      deallocate (argumentValue)
      deallocate (nextArgumentValue)
   end do

contains

   subroutine CountBytes(filename, file_size)
      character(len=*), intent(in) :: filename
      integer, intent(out) :: file_size
      integer :: ios

      open (unit=1, file=filename, status='old', action='read', iostat=ios)
      if (ios /= 0) then
         print *, 'ccwc: ', filename, ': open: No such file or directory'
         stop
      end if

      inquire (unit=1, size=file_size)

      close (1)

   end subroutine CountBytes

   subroutine CountLines(filename, lines)
      character(len=*), intent(in) :: filename
      integer, intent(out) :: lines
      integer :: ios

      lines = 0

      open (unit=1, file=filename, status='old', action='read', iostat=ios)
      if (ios /= 0) then
         print *, 'ccwc: ', filename, ': open: No such file or directory'
         stop
      end if

      do
         read (1, *, iostat=ios)
         if (ios /= 0) exit
         lines = lines + 1
      end do

      close (1)

   end subroutine CountLines

   subroutine CountWords(filename, words)
      character(len=*), intent(in) :: filename
      integer, intent(out) :: words
      integer :: ios, result, i
      character(1000) :: line

      character(1) :: currentChar
      character(1) :: lastChar

      open (unit=1, file=filename, status='old', action='read', iostat=ios)
      if (ios /= 0) then
         print *, 'ccwc: ', filename, ': open: No such file or directory'
         stop
      end if

      do
         read (1, '(A)', iostat=ios) line
         if (ios /= 0) exit
         do i = 1, len_trim(line)
            currentChar = line(i:i)
            call IsWhitespace(currentChar, result)
            if (result == 1) then
               call IsWhitespace(lastChar, result)
               if (result == 0) then
                  words = words + 1
               end if
            end if
            lastChar = currentChar
         end do
         call IsWhitespace(lastChar, result)
         if (result == 0) then
            words = words + 1
            lastChar = ' '
         end if
      end do

      close (1, status='keep')

   end subroutine CountWords

   subroutine IsWhitespace(char, result)
      character(len=*), intent(in) :: char
      integer, intent(out) :: result

      result = 0

      if (iachar(char) == 9 .or. &
          iachar(char) == 10 .or. &
          iachar(char) == 32) then
         result = 1
      end if
   end subroutine IsWhitespace

end program ccwc
