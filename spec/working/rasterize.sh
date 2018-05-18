# this script accepts the location of batik-rasterizer.jar file
# if you do not want to tip in the path every time you can write it down in the else branch
if [ $1 ]; then
  COMMAND="java -jar $1"
else
  COMMAND="java -jar /home/stefan/studium/downloads/batik-1.6/batik-rasterizer.jar"
fi

for i in $( find -name *.svg ); do
  j=`echo $i | awk '{gsub(".svg",".pdf",$1); print($1)}'`
  echo $i
  echo $j
  $COMMAND $i -m application/pdf -d ../images/$j
done
