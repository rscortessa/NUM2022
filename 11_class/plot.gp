set xlabel 'x'
set ylabel 'y(x)'
F='1.0 0.1 0.05'
do for [i=1:3]{
set terminal pngcairo
set output 'euler'.i.'.png'
set title word(F,i)
plot 'euler'.i.'.txt' u 1:2 with lines title 'euler' ,'euler-improv'.i.'.txt' u 1:2 with lines title 'middlepoint', 'euler2.txt' u 1:3 with points title 'exact'
     }