project('test-archivo-avanzado', 'fortran',
  version : '0.1',
  default_options : ['warning_level=3'])

testdrive_dep = dependency('test-drive', fallback: ['test-drive', 'testdrive_dep'])

tests = [
  'calculator'
]

test_srcs = files(
  'test/main.f90',
  'src/parser.f90'
)

foreach t: tests
  test_srcs += files('test/@0@_suite.f90'.format(t.underscorify()))
endforeach

tester = executable(
  'tester',
  sources: test_srcs,
  dependencies: [testdrive_dep]
)

test('all tests', tester)
