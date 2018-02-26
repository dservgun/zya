BEGIN {
  FS =  ","
}

{
  sum += $5
}

END { print sum/1000000000000000000 }