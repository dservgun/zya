# A simple script to generate pdf files from the tex
# files.

for i in ./latex/*.tex 
do 
  pdflatex -output-directory=./pdf $i
done
