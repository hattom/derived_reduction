COMPILER = GCC

ifeq ($(COMPILER), GCC)
	FC = gfortran
	FFLAGS = -O3 -fopenmp
else ifeq ($(COMPILER), NVHPC)
	FC=nvfortran
	FFLAGS=-O3 -mp -stdpar=multicore -Mbounds
else ifeq ($(COMPILER), Intel)
	FC=ifort
	FFLAGS=-O3 -qopenmp
endif

LDFLAGS =

EXEC = test_reduce
OBJDIR=obj
F90SRC= test_reduce.F90
F90OBJ  = $(addprefix $(OBJDIR)/,$(F90SRC:.F90=.o))

EXEC_MWE = test_reduce_mwe
F90SRC_MWE= test_reduce_mwe.F90
F90OBJ_MWE  = $(addprefix $(OBJDIR)/,$(F90SRC_MWE:.F90=.o))


.DEFAULT_GOAL = all
.PHONY: all clean
all: $(EXEC)

clean:
	-rm -rf $(OBJDIR)

$(EXEC): $(F90OBJ)
	$(FC) -o $@ $(FFLAGS) $(LDFLAGS) $<
$(EXEC_MWE): $(F90OBJ_MWE)
	$(FC) -o $@ $(FFLAGS) $(LDFLAGS) $<

$(OBJDIR)/%.o: %.F90
	@test -d $(@D) || mkdir -p $(@D)
	$(FC) -c -o $@ $(FFLAGS) $<
