#!/bin/bash

bt_device_addr=$(pacmd list-cards | grep -i 'name:.*bluez_card' | sed -E 's/.*<?bluez_card\.([A-Z0-9_]+)>?/\1/')
device_mac=$(echo $bt_device_addr | sed 's/_/:/g')

a2dp_available=$(pacmd list-cards | grep -A30 bluez | grep "A2DP Sink" | sed -E 's/.* available: ([a-z]+)\)/\1/g')

if [[ "$a2dp_available" == "no" ]]
then
    dbus-send --system --dest=org.bluez --print-reply /org/bluez/hci0/dev_$bt_device_addr org.bluez.Device1.Connect

    pacmd set-card-profile bluez_card.$bt_device_addr off
    pacmd set-card-profile bluez_card.$bt_device_addr a2dp_sink
    pacmd set-default-sink bluez_sink.$bt_device_addr.a2dp_sink
fi
