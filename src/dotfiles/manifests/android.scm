(use-package-modules base bash gcc gl compression certs video
                     glib linux nss pulseaudio java fontutils
                     version-control virtualization xml xorg
                     android)

(packages->manifest
 (list bash
       coreutils
       sed
       git
       icedtea
       alsa-lib
       (list gcc "lib")
       glibc-locales
       fontconfig
       which
       dbus
       e2fsprogs
       eudev
       qemu-minimal
       alsa-lib
       expat
       libxcomposite
       libxcursor
       libxi
       libxtst
       mesa
       nss
       kmod
       adb
       ffmpeg
       v4l-utils
       mplayer
       nss-certs
       pulseaudio
       (list util-linux "lib")
       libx11
       zlib))
