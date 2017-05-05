#!/usr/bin/env bash

for name in aero agb aeps agc aged ag asci ant arce arch art astr bio bmed brae bot bus chem cd chin crp ce coms cpe csc cm dsci danc data ese esm ersc econ educ ee engr engl edes enve es fpe fsn fr geog geol ger gs gsa gsb gse gsp grc hist hnrc hnrs ime itp isla ital jpns jour kine la laes ls msci mate math me mcro msl mu nr phil pem pew psc phys pols psy rpta rels scm socs soc ss span stat sie th univ wvit wgs wlc
do
    wget http://catalog.calpoly.edu/coursesaz/$name/ -O "$name.html"
done

