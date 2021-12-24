COMPILER = GCC

ifeq ($(COMPILER), GCC)
	FC = gfortran
	FFLAGS = -O3 -fopenmp
else ifeq ($(COMPILER), NVHPC)
	FC=nvfortran
	FFLAGS=-O3 -mp -stdpar=multicore
endif

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
