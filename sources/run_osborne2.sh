export ALGENCAN=/opt/algencan-3.1.1

rm -f osborne2

gfortran -O3 -w -fcheck=all -g osborne2.f90 -L$ALGENCAN/lib -lalgencan -lhsl sort.o subset.o -o osborne2

for i in {1..10}
do 
let "delta = i"
echo $delta > delta.txt
./osborne2
done


