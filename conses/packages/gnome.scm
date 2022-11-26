(define-module (conses packages gnome)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages tls)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:))

(define-public feedbackd-next
  (package
    (inherit feedbackd)
    (version "0.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://source.puri.sm/Librem5/feedbackd.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name (package-name feedbackd) version))
              (sha256
               (base32
                "1r6ficcmki2fr89m0nvi3z0xyzk87z1d4f9ljyyw5ayg48862lcp"))))
    (arguments
     `(#:configure-flags
       ;; Introspection data cannot currently be cross-compiled.
       (list "-Dintrospection=disabled")))))

(define-public sofia-sip
  (package
    (name "sofia-sip")
    (version "1.13.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/freeswitch/sofia-sip")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1nbyq9rpf0qy35309l72w9ffgbb2sryjsxmn2zh0z126phaqnpf4"))))
    (build-system gnu-build-system)
    (arguments
     (list
      ;; Tests require network access
      #:tests? #f
      #:phases #~(modify-phases %standard-phases
                   (add-before 'bootstrap 'remove-bootstrap.sh
                     (lambda _
                       (delete-file "bootstrap.sh")))
                   (replace 'bootstrap
                     (lambda _
                       (invoke "libtoolize")
                       (invoke "aclocal")
                       (invoke "autoheader")
                       (invoke "automake" "--add-missing")
                       (invoke "autoconf"))))))
    (native-inputs (list autoconf-wrapper
                         automake
                         libtool
                         pkg-config
                         openssl
                         glib
                         check))
    (home-page "https://github.com/freeswitch/sofia-sip")
    (synopsis "Open-source SIP User-Agent library")
    (description
     "Sofia-SIP is an open-source SIP User-Agent library, compliant
with the IETF RFC 3261 specification.  It can be used as a building block for SIP
software for uses such as VoIP, IM, and many other real-time and person-to-person
communication services.")
    (license license:lgpl2.1)))

(define-public callaudiod
  (package
    (name "callaudiod")
    (version "0.1.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/mobian1/callaudiod")
                    (commit version)))
              (sha256
               (base32
                "0h9sfimlcikaxx29z97mmm025z7m5kkw0l8hbnqsmrgrph0bspzg"))
              (file-name (git-file-name name version))))
    (build-system meson-build-system)
    (native-inputs (list alsa-lib
                         `(,glib "bin") gobject-introspection pkg-config
                         pulseaudio))
    (home-page "https://gitlab.com/mobian1/callaudiod")
    (synopsis "Call audio routing daemon")
    (description "@code{callaudiod} is a daemon for dealing with audio
routing during phone calls.")
    (license license:gpl3+)))


(define-public calls
  (package
    (name "calls")
    (version "43.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.gnome.org/GNOME/calls")
                    (commit (string-append "v" version))
                    (recursive? #t)))
              (sha256
               (base32
                "09nlxhnffmy2rvlpz6fd8fhba9rimqd2a33z600yzf7fl4vvvwby"))
              (file-name (git-file-name name version))))
    (build-system meson-build-system)
    (arguments
     (list #:glib-or-gtk? #t
           ;; The tests need network access
           #:tests? #f))
    (native-inputs (list callaudiod
                         desktop-file-utils
                         evolution-data-server
                         feedbackd-next
                         folks
                         gettext-minimal
                         `(,glib "bin")
                         gom
                         gsettings-desktop-schemas
                         `(,gtk+ "bin")
                         gtk-doc
                         libgee
                         libhandy
                         libpeas
                         libsecret
                         pkg-config
                         python-docutils
                         sofia-sip
                         vala))
    (inputs (list gsound
                  gstreamer
                  gst-plugins-bad
                  gst-plugins-base
                  gst-plugins-good
                  gst-plugins-ugly
                  modem-manager
                  gtk+))
    (home-page "https://gitlab.gnome.org/GNOME/calls")
    (synopsis "Phone dialer and call handler")
    (description "GNOME Calls is a phone dialer and call handler.")
    (license license:gpl3+)))
