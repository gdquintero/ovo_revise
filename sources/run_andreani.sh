rm -f salida.txt param.txt
touch salida.txt param.txt

export ALGENCAN=/opt/algencan-3.1.1
rm -f andreani
gfortran -O3 -w -fcheck=all -g andreani.f90 -L$ALGENCAN/lib -lalgencan -lhsl sort.o subset.o -o andreani

delta=0.1
sigmin=0.1
gamma=5

for noutliers in {3..26}
  do
    echo $delta $sigmin $gamma $noutliers  > param.txt
    ./andreani
  done

# for delta in 1.0 0.1 0.01
# do
#   for sigmin in 0.1 1.0 10.0
#   do
#     for gamma in 2 5 10
#     do
#       echo 'esta=============================' >> salida.txt
#       echo 'esta=====',$delta $sigmin $gamma  >> salida.txt
#       echo 'esta=============================' >> salida.txt
#       for noutliers in {3..26}
#       do
#         echo $delta $sigmin $gamma $noutliers > param.txt
#         ./andreani >> salida.txt
#       done
#     done
#   done
# done
