{--| 

Some setup instructions.
#Creating users in postgres

sudo -u postgres psql
 > CREATE USER test password 'test';
 > CREATE DATABASE test_debug owner test;


--}

import Distribution.Simple
main = defaultMain

