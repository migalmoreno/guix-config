(define-module (conses packages gnome)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system meson)
  #:use-module ((guix licenses) #:prefix license:))

;; TODO: sofia_sip
;; TODO: callaudiod
;; TODO: libcall-ui

(define-public calls
  (package
    (name "calls")
    (version "43.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.gnome.org/GNOME/calls")
             (commit (string-append "v" version))))
       (sha256
        (base32 "046g61a88vzw4baqh1fhdn7g3cpiyslgddvx1q3nacb60zf4n619"))
       (file-name (git-file-name name version))))
    (build-system meson-build-system)
    (native-inputs
     (list
      vala
      pkg-config
      desktop-file-utils
      gtk-doc
      docbook-xsl
      docbook-xml))
    (inputs
     (list
      modem-manager
      libhandy
      evolution-data-server
      folks
      gsound
      gom
      gstreamer
      gst-plugins-bad
      gst-plugins-base
      gst-plugins-good
      gst-plugins-ugly
      feedbackd
      gtk+
      libpeas))
    (home-page "https://gitlab.gnome.org/GNOME/calls")
    (synopsis "A phone dialer and caller handler")
    (description "GNOME Calls is a phone dialer and call handler.")
    (license license:gpl3+)))
