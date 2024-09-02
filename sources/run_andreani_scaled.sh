rm -f salida.txt param.txt
touch salida.txt param.txt

export ALGENCAN=/opt/algencan-3.1.1
rm -f andreani_scaled
gfortran -O3 -w -fcheck=all -g andreani_scaled.f90 -L$ALGENCAN/lib -lalgencan -lhsl sort.o subset.o -o andreani_scaled

# delta=1.0d-3
# sigmin=1.0d-1
# gamma=5.0d0

delta=1.0d-1
sigmin=1.0d-1
gamma=5.d0

for noutliers in {0..15}
  do
    echo $delta $sigmin $gamma $noutliers  > param.txt
    ./andreani_scaled >> salida.txt
  done

# for delta in 0.1 0.01 0.001 0.0001
# do
#   for sigmin in 0.01 0.1 1.0
#   do
#     for gamma in 2 5 10
#     do
#       echo 'esta=============================' >> salida.txt
#       echo 'esta=====',$delta $sigmin $gamma  >> salida.txt
#       echo 'esta=============================' >> salida.txt
#       for noutliers in {0..12}
#       do
#         echo $delta $sigmin $gamma $noutliers > param.txt
#         ./andreani >> salida.txt
#       done
#     done
#   done
# done
