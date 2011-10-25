# This defines the functions used by the testing code for each checker.
# The script 'run-tests.sh' in each checker directory should include
# this file.

HERE="$(cd "$(dirname "$0")" ; pwd)"

function run_cases {
  set -e
  local tool=$1
  local where=${2:-$HERE}
  local flags=${3:--Wall -pedantic -Werror}
  local num_passed=0
  local num_failed=0

  # make sure the directory for test results exists
  mkdir -p ${where}/tests/actual

  # run the individal tests
  for test_file in ${where}/tests/cases/*
  do
    local test_name=$(basename ${test_file})
    local expected=${where}/tests/expected/${test_name}.out
    local actual=${where}/tests/actual/${test_name}.out

    echo "Running ${test_name}"
    gfortran ${flags} ${test_file}
    # this pushd is here to prevent segfaults in ${tool} due
    # to not finding compass_parameters in pwd
    pushd ${where} > /dev/null
    ./${tool} ${test_file} > ${actual}
    popd > /dev/null

    # check the output
    local result=$(diff -u ${expected} ${actual})
    if [ "$result" == "0" ]
    then
      echo "${test_name} successful"
      num_passed=$(expr $num_passed + 1)
    else
      echo "${test_name} FAILED"
      num_failed=$(expr $num_failed + 1)
    fi
  done

  echo "Total tests passed: $num_passed"
  echo "Total tests failed: $num_failed"
  return $num_failed
}
