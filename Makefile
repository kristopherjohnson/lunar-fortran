# GNU Fortran
F95:=gfortran
F95FLAGS:=-Wall -Wno-unused-variable -Wno-unused-label -std=f95 -ffree-form

% : %.f95
	$(F95) $(F95FLAGS) -o $@ $<

lunar: lunar.f95

run: lunar
	./lunar
.PHONY: run

clean:
	- $(RM) lunar
.PHONY: clean
