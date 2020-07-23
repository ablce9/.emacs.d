#!/bin/sh
set -ex
# Get ssid
VPN=${VPN:-none}
ssid=$(ioreg -l -n AirPortDriver |awk '/\"IO80211SSID\" = / { print $8 }'| sed 's/\"//g')
echo ">>$ssid"
if [ $VPN = "office" ]
then
    location="vpn"
elif [ $ssid = "malicious_network4_5G" ]
then
    location="malicious_place"
elif [ $ssid = "lf-office-a" ]
then
    location="work-1"
else
    location="Automatic"
fi
newloc=$(/usr/sbin/scselect ${location} | sed 's/^.*(\(.*\)).*$/\1/')
echo ${newloc}

if [ ${location} != ${newloc} ]
then
    exit 1
fi
exit 0
