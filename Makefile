# GNU Fortran
F95:=gfortran
F95FLAGS:=-std=f95 -ffree-form -g -Wall -Wno-unused-variable -Wno-unused-label

% : %.f95
	$(F95) $(F95FLAGS) -o $@ $<

lunar: lunar.f95

run: lunar
	./lunar
.PHONY: run

clean:
	- $(RM) lunar
.PHONY: clean
