# "make" or "make lunar" - build lunar executable
# "make run"             - build lunar executable and run it
# "make test"            - build and run unit tests
# "make clean"           - delete executable and test output files

# GNU Fortran
F95:=gfortran
F95FLAGS:=-std=f95 -ffree-form -Wall

DIFF:=diff
SED:=sed

% : %.f95
	$(F95) $(F95FLAGS) -o $@ $<

lunar: lunar.f95

run: lunar
	./lunar
.PHONY: run

# lunar-test.f95 is generated from lunar.f95, by removing "!TEST:" comment
# prefixes, resulting in a program that echoes all input back to output.  This
# is useful for testing.
lunar-test.f95: lunar.f95
	$(SED) -e 's/!TEST://g' lunar.f95 > lunar-test.f95

lunar-test: lunar-test.f95

test: test_success test_failure test_good
.PHONY: test

test_good: lunar-test
	./lunar-test <test/good_input.txt >good_output.txt
	$(DIFF) test/good_output_expected.txt good_output.txt
.PHONY: test_good

test_success: lunar-test
	./lunar-test <test/success_input.txt >success_output.txt
	$(DIFF) test/success_output_expected.txt success_output.txt
.PHONY: test_success

test_failure: lunar-test
	./lunar-test <test/failure_input.txt >failure_output.txt
	$(DIFF) test/failure_output_expected.txt failure_output.txt
.PHONY: test_failure

clean:
	- $(RM) lunar
	- $(RM) lunar-test
	- $(RM) lunar-test.f95
	- $(RM) success_output.txt
	- $(RM) failure_output.txt
	- $(RM) good_output.txt
.PHONY: clean
