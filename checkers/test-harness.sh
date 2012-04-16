# This defines the functions used by the testing code for each checker.
# The script 'run-tests.sh' in each checker directory should include
# this file.

HERE="$(cd "$(dirname "$0")" ; pwd)"

function warn_compass_parameters {
  # If COMPASS_PARAMETERS is set we warn that
  # the tests might fail spuriously
  if [ ! -z "${COMPASS_PARAMETERS}" ]
  then
    echo ""
    echo "When \$COMPASS_PARAMETERS is set Compass outputs differently which can break the tests"
    echo "If the tests are failing try unsetting \$COMPASS_PARAMETERS and running the tests again."
    echo ""
  fi
}

function run_cases {
  set -e
  local tool=$1
  local where=${2:-$HERE}
  local flags=${3:--Wall -pedantic -Werror}
  local num_passed=0
  local num_failed=0

  warn_compass_parameters

  # make sure the directory for test results exists
  mkdir -p ${where}/tests/actual

  echo ""

  # run the individal tests
  for test_file in ${where}/tests/cases/*
  do
    local test_name=$(basename ${test_file})
    local expected=${where}/tests/expected/${test_name}.out
    local actual=${where}/tests/actual/${test_name}.out

    echo -n "Running ${test_name} ... "
    local gfortran_result=$(gfortran ${flags} ${test_file} &>${actual} ; echo $?)
    if [ $gfortran_result != "0" ]
    then
      echo "FAILED"
      num_failed=$(expr $num_failed + 1)
      continue
    fi
    # this pushd is here to prevent segfaults in ${tool} due
    # to not finding compass_parameters in pwd
    pushd ${where} > /dev/null
    ./${tool} ${test_file} &>${actual}
    popd > /dev/null

    # check the output
    local result=$(diff -u ${expected} ${actual} ; echo $?)
    if [ $gfortran_result == "0" ] && [ "$result" == "0" ]
    then
      echo "successful"
      num_passed=$(expr $num_passed + 1)
    else
      echo "FAILED"
      num_failed=$(expr $num_failed + 1)
    fi
  done

  echo ""
  echo "Total tests passed: $num_passed"
  echo "Total tests failed: $num_failed"
  echo ""
  return $num_failed
}
