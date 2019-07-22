FC = gfortran
FCFLAGS = -O2
FLFLAGS = -static -s
DIRS = obj mod bin
objects = obj/statistics_module.o obj/csv_module.o

$(shell mkdir -p $(DIRS))

ALL: bin/report1 bin/report2

bin/report1: obj/report1.o $(objects)
	$(FC) $(FLFLAGS) $^ -o $@

bin/report2: obj/report2.o $(objects)
	$(FC) $(FLFLAGS) $^ -o $@

obj/report1.o: src/report1.f95 obj/statistics_module.o obj/csv_module.o
	$(FC) $(FCFLAGS) -Imod -c $< -o $@

obj/report2.o: src/report2.f95 obj/statistics_module.o obj/csv_module.o
	$(FC) $(FCFLAGS) -Imod -c $< -o $@

obj/statistics_module.o: src/statistics_module.f95
	$(FC) $(FCFLAGS) -Jmod -c $< -o $@

obj/csv_module.o: src/csv_module.f95
	$(FC) $(FCFLAGS) -Jmod -c $< -o $@

.PHONY : clean
clean :
	$(RM) bin/report1 bin/report2 $(objects) $(modules)
