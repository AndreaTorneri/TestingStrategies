module load worker/1.6.10-intel-2018a

for childseed in 1 5 10; do
	for r in 2.5 5 8; do
		wsub -batch testing.pbs  -v childseed=$childseed,R_s=$r -data ./scenarios_september.csv -A (ADD HERE THE BUDGET CODE)
	done
done
