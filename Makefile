FC = gfortran
FFLAGS = -O3 -ftree-parallelize-loops=${OMP_NUM_THREADS} -fopenmp
LDFLAGS = 

EXEC = test_reduce
OBJDIR=obj
F90SRC= test_reduce.F90
F90OBJ  = $(addprefix $(OBJDIR)/,$(F90SRC:.F90=.o))


.DEFAULT_GOAL = all
.PHONY: all clean
all: $(EXEC)

clean:
	-rm -rf $(OBJDIR)

$(EXEC): $(F90OBJ)
	$(FC) -o $@ $(FFLAGS) $(LDFLAGS) $<

$(OBJDIR)/%.o: %.F90
	@test -d $(@D) || mkdir -p $(@D)
	$(FC) -c -o $@ $(FFLAGS) $<
