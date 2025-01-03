module expressions_suite
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use parser
    implicit none
    private
    character(len=*), parameter :: filePrefix = 'test/inputs/'

    public :: collect_expressions_suite

    contains

    !> Collect all exported unit tests
    subroutine collect_expressions_suite(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [&
            new_unittest("Asersion negativa", test_neg_assertion),&
            new_unittest("Asersion positiva", test_assertion),&
            new_unittest("Punto", test_period),&
            new_unittest("Operador $", test_dollar),&
            new_unittest("Operador @", test_pluck),&
            new_unittest("Conteo", test_count),&
            new_unittest("Conteo con delimitador", test_count_delimiter),&
            new_unittest("Repeticion min-max", test_min_max),&
            new_unittest("Repeticion min-max con delimitador", test_min_max_delimiter)&
        ]

    end subroutine collect_expressions_suite

    subroutine test_neg_assertion(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: result
        character(len=:), allocatable :: input

        input = getFileContents(filePrefix//'asersion_negativa.txt')

        result = parse(input)
        call check(error, result, 'a3')
        if (ALLOCATED(error)) return
    end subroutine test_neg_assertion

    subroutine test_assertion(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: result
        character(len=:), allocatable :: input

        input = getFileContents(filePrefix//'asersion.txt')

        result = parse(input)
        call check(error, result, 'b5')
        if (ALLOCATED(error)) return
    end subroutine test_assertion

    subroutine test_period(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: result
        character(len=:), allocatable :: input

        input = getFileContents(filePrefix//'punto.txt')

        result = parse(input)
        call check(error, result, 'c#')
        if (ALLOCATED(error)) return
    end subroutine test_period

    subroutine test_dollar(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: result
        character(len=:), allocatable :: input

        input = getFileContents(filePrefix//'dolar.txt')

        result = parse(input)
        call check(error, result, 'd123')
        if (ALLOCATED(error)) return
    end subroutine test_dollar

    subroutine test_pluck(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: result
        character(len=:), allocatable :: input

        input = getFileContents(filePrefix//'pluck.txt')

        result = parse(input)
        call check(error, result, 'e6')
        if (ALLOCATED(error)) return
    end subroutine test_pluck

    subroutine test_count(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: result
        character(len=:), allocatable :: input

        input = getFileContents(filePrefix//'conteo.txt')

        result = parse(input)
        call check(error, result, 'fff')
        if (ALLOCATED(error)) return
    end subroutine test_count

    subroutine test_count_delimiter(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: result
        character(len=:), allocatable :: input

        input = getFileContents(filePrefix//'conteo_delimitador.txt')

        result = parse(input)
        call check(error, result, 'ggg')
        if (ALLOCATED(error)) return
    end subroutine test_count_delimiter

    subroutine test_min_max(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: result
        character(len=:), allocatable :: input

        input = getFileContents(filePrefix//'min_max.txt')

        result = parse(input)
        call check(error, result, 'hh')
        if (ALLOCATED(error)) return
    end subroutine test_min_max

    subroutine test_min_max_delimiter(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: result
        character(len=:), allocatable :: input

        input = getFileContents(filePrefix//'min_max_delimitador.txt')

        result = parse(input)
        call check(error, result, 'ii')
        if (ALLOCATED(error)) return
    end subroutine test_min_max_delimiter

    function getFileContents(filename) result(contents)
        character(len=*) :: filename
        character(:), allocatable :: contents
        integer :: inputLen

        inquire(file=filename, size=inputLen)
        allocate(character(len=inputLen) :: contents)
        open (1, file=filename, status='old', action='read', access='stream', form='unformatted')
        read (1) contents
    end function getFileContents
end module expressions_suite
