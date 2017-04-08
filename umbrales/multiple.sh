#!/bin/bash
for i in {2..30}
do
    eval "time ./umbrales $i instancia.tsp"
done
