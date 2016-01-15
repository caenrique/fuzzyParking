set terminal png transparent nocrop enhanced size 1080,720 font "arial,12"
set output '../img/surface.png'
set title "Superficie 3D que representa las reglas del controlador difuso"
set xlabel "Ángulo a"
set ylabel "Distancia x"
set zlabel "Volante" 
set grid layerdefault
set hidden3d
#set view 70, 225
set dgrid3d 21, 37, 50
splot 'results.dat' with lines