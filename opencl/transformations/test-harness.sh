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
  local template=${HERE}/cl_template.c
  local template_out=${HERE}/rose_cl_template.c

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
    pushd ${where} > /dev/null
    ${tool} ${test_file} ${template} &>/dev/null
    mv ${template_out} ${actual}
    popd > /dev/null

    # check the output
    local result=$(diff -u ${expected} ${actual} ; echo $?)
    if [ "$result" == "0" ]
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
