(list (channel
        (name 'nonguix)
        (url "file:///home/vega/src/guile/nonguix")
        (branch "base-to-upstream")
        (commit
          "026b5b5c47f606161113398cda710a7c85164937"))
      (channel
        (name 'guix)
        (url "file:///home/vega/src/guile/guix")
        (branch "base-to-upstream")
        (commit
          "f60c40ffc4a725aaaa5321d5f6b3d5098a9a23ed"))
      (channel
        (name 'rde)
        (url "file:///home/vega/src/guile/rde")
        (branch "base-to-upstream")
        (commit
          "86b221431cbfb08618d1cc201720f1cb4510767e")
        (introduction
          (make-channel-introduction
            "257cebd587b66e4d865b3537a9a88cccd7107c95"
            (openpgp-fingerprint
              "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0")))))
