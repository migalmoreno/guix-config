(define-module (migalmoreno manifests android)
  #:use-module (gnu packages android)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages java)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix profiles))

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
