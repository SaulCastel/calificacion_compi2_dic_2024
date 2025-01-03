module calculator_suite
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use parser
    implicit none
    private
    character(len=*), parameter :: filePrefix = 'test/inputs/'

    public :: collect_calculator_suite

    contains

    !> Collect all exported unit tests
    subroutine collect_calculator_suite(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [&
            new_unittest("1+4*(6/3)", test1),&
            new_unittest("(63/9)*(49/7)", test2),&
            new_unittest("(15+5)/5", test3),&
            new_unittest("(8-3)*(6/2)", test4),&
            new_unittest("(18/(9-6))+7", test5),&
            new_unittest("(25-(10/2))*2", test6)&
        ]

    end subroutine collect_calculator_suite

    subroutine test1(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: input
        integer :: result

        input = getFileContents(filePrefix//'test1.txt')

        result = parse(input)
        call check(error, result, 9)
        if (ALLOCATED(error)) return
    end subroutine test1

    subroutine test2(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: input
        integer :: result

        input = getFileContents(filePrefix//'test2.txt')

        result = parse(input)
        call check(error, result, 49)
        if (ALLOCATED(error)) return
    end subroutine test2

    subroutine test3(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: input
        integer :: result

        input = getFileContents(filePrefix//'test3.txt')

        result = parse(input)
        call check(error, result, 4)
        if (ALLOCATED(error)) return
    end subroutine test3

    subroutine test4(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: input
        integer :: result

        input = getFileContents(filePrefix//'test4.txt')

        result = parse(input)
        call check(error, result, 15)
        if (ALLOCATED(error)) return
    end subroutine test4

    subroutine test5(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: input
        integer :: result

        input = getFileContents(filePrefix//'test5.txt')

        result = parse(input)
        call check(error, result, 13)
        if (ALLOCATED(error)) return
    end subroutine test5

    subroutine test6(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: input
        integer :: result

        input = getFileContents(filePrefix//'test6.txt')

        result = parse(input)
        call check(error, result, 40)
        if (ALLOCATED(error)) return
    end subroutine test6

    function getFileContents(filename) result(contents)
        character(len=*) :: filename
        character(:), allocatable :: contents
        integer :: inputLen

        inquire(file=filename, size=inputLen)
        allocate(character(len=inputLen) :: contents)
        open (1, file=filename, status='old', action='read', access='stream', form='unformatted')
        read (1) contents
    end function getFileContents
end module calculator_suite
