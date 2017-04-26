#!/bin/bash
for i in {1..100}
do
    eval "time ./umbrales $i instancia.tsp"
done
