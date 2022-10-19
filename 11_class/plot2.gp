set xlabel 't'
set ylabel 'y(t),x(t)'
set logscale y
set terminal pngcairo
set output 'lotka.png'
set title 'lotka-Voltera solutions coupling =1'
plot 'lotkaeuler-improv.txt' u 1:2 with lines title 'x(t)' , 'lotkaeuler-improv.txt' u 1:3 with lines title 'y(t)'
  