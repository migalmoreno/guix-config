(list (channel
       (name 'rde)
       (url (format #f "file://~a/src/guile/rde" (getenv "HOME")))
       (branch "base-to-upstream")
       (introduction
        (make-channel-introduction
         "257cebd587b66e4d865b3537a9a88cccd7107c95"
         (openpgp-fingerprint
          "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
      (channel
       (name 'guix)
       (url (format #f "file://~a/src/guile/guix" (getenv "HOME")))
       (branch "base-to-upstream"))
      (channel
       (name 'nonguix)
       (url "https://gitlab.com/nonguix/nonguix")
       (branch "master")
       (introduction
        (make-channel-introduction
         "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
         (openpgp-fingerprint
          "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
      (channel
       (name 'home-service-dwl-guile)
       (url "https://github.com/engstrand-config/home-service-dwl-guile")
       (branch "main")
       (introduction
        (make-channel-introduction
         "314453a87634d67e914cfdf51d357638902dd9fe"
         (openpgp-fingerprint
          "C9BE B8A0 4458 FDDF 1268 1B39 029D 8EB7 7E18 D68C"))))
      (channel
       (name 'home-service-dtao-guile)
       (url "https://github.com/engstrand-config/home-service-dtao-guile")
       (branch "main")
       (introduction
        (make-channel-introduction
         "64d0b70c547095ddc840dd07424b9a46ccc2e64e"
         (openpgp-fingerprint
          "C9BE B8A0 4458 FDDF 1268 1B39 029D 8EB7 7E18 D68C"))))
      (channel
       (name 'farg)
       (url "https://github.com/engstrand-config/farg")
       (branch "main")))
