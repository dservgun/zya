BEGIN {
  FS =  ","
}

{
  sum += $5
}

END { print sum}
