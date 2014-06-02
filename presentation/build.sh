# build once by default
pdflatex -interaction=nonstopmode $1

# watch for alterations
while inotifywait *.tex examples/* ; do
  pdflatex -interaction=nonstopmode $1 | grep -i -B20 error
done
