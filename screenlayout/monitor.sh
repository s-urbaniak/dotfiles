# switch to USB-C input
ddccontrol -r 0x60 -w 6939 dev:/dev/i2c-7
ddcutil --bus 7 setvcp 0x60 0x1b

# switch to HDMI input
ddccontrol -r 0x60 -w 6929 dev:/dev/i2c-7
ddcutil --bus 7 setvcp 0x60 0x11

# contrast
ddccontrol -r 0x12 -w 50 dev:/dev/i2c-7
ddcutil --bus 7 setvcp 0x12 50

# brightness
ddccontrol -r 0x10 -w 50 dev:/dev/i2c-7
ddcutil --bus 7 setvcp 0x10 50

# sleep mode
# on
ddccontrol -r 0xd6 -w 4 dev:/dev/i2c-7
ddcutil --bus 7 setvcp d6 4

# off
ddccontrol -r 0xd6 -w 1 dev:/dev/i2c-7
ddcutil --bus 7 setvcp d6 1

# configure audio output via USB-C
pactl set-card-profile 0 output:hdmi-stereo-extra1+input:analog-stereo

# configure audio output via internal headphone jack
pactl set-card-profile 0 output:analog-stereo+input:analog-stereo
