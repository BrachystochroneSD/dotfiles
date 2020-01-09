#!/bin/sh

# sudo nmap -sS -Pn -iR 0 -p 25565 --open -oG testgrep.nmap
# sudo nmap -sS -Pn -iR 0 -p 80 --open -oG testgrep.nmap

grep 'Status: Up' testnmap | awk '($3!="()"){printf("IP: %s\nDomain: %s\n%s\n\n",$2,substr($3,2,length($3)-2),system("geoiplookup " $2))}($3=="()"){printf("IP: %s\nùs\n\n",$2,system("geoiplookup " $2))}' | awk '(NF!=1){print $0}' | awk '($1=="GeoIP"){print "Location: " $5 " " $6}($1!="GeoIP"){print $0}'
