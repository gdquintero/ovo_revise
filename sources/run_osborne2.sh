rm -f salida.txt
touch salida.txt

export ALGENCAN=/opt/algencan-3.1.1
rm -f osborne2
gfortran -O3 -w -fcheck=all -g osborne2.f90 -L$ALGENCAN/lib -lalgencan -lhsl sort.o subset.o -o osborne2

# for delta in 0.5 0.1 0.05 0.01 0.005 0.001 0.0005 0.0001
# do
#   for sigmin in 0.0001 0.001 0.01 0.1 1.0
#   do
#     for gamma in 2 5 10
#     do
#       echo 'Esta=============================' >> salida.txt
#       echo 'Esta=====',$delta $sigmin $gamma  >> salida.txt
#       echo 'Esta=============================' >> salida.txt
#       for noutliers in {0..15}
#       do
#         echo $delta $sigmin $gamma $noutliers > param.txt
#         ./osborne2 >> salida.txt
#       done
#     done
#   done
# done

delta=0.001
sigmin=0.1
gamma=5

for noutliers in {0..15}
  do
    echo $delta $sigmin $gamma $noutliers  > param.txt
    ./osborne2 >> salida.txt
  done


