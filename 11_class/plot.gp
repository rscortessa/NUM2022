set terminal png
set xlabel 'x'
set ylabel 'y(x)'
plot "euler1.txt" u 1:2
set output '1.png'