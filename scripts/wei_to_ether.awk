BEGIN {
  FS =  ","
}

{
  print $1, "," $2 "," $3 "," $4 "," $5/1000000000000000000 "," $6/1000000000000000000 "," $7/1000000000000000000, $8
}
