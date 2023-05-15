#!/bin/sh

for f in $(cd sources; ls *.S)
do
    cpp -E sources/${f} -o generated/${f}
done