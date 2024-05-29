(define-module (migalmoreno themes)
  #:use-module (farg source)
  #:use-module (farg theme)
  #:use-module (guix packages)
  #:use-module (guix download))

(define-public %light-wallpaper
  (origin
    (method url-fetch)
    (uri "https://w.wallhaven.cc/full/28/wallhaven-28vjgm.jpg")
    (sha256
     (base32
      "14b5h86jjimdzfw9krbc90abcd9kgvfhavqqq7xzxjxjbakrkzdl"))))

(define-public %dark-wallpaper
  (origin
    (method url-fetch)
    (uri "https://w.wallhaven.cc/full/dg/wallhaven-dgo6pl.jpg")
    (sha256
     (base32
      "09jap8g5232h8ham41jljvm1x7d87wjn0p42dy0x119cqd1ds1i3"))))

(define-public dark-theme
  (farg-source
   (theme
    (farg-theme
     (fg "#FFFFFF")
     (bg "#000000")
     (bg-alt "#1e1e1e")
     (accent-0 "#00BCFF")
     (accent-1 "#212121")
     (accent-2 "#6ae4b9")
     (accent-3 "#005a5f")
     (accent-4 "#0000c0")
     (alpha 0.8)
     (light? #f)
     (other '((red . "#ff5f59")))
     (wallpaper %dark-wallpaper)))))

(define-public light-theme
  (farg-source
   (theme
    (farg-theme
     (fg "#000000")
     (bg "#FFFFFF")
     (bg-alt "#f0f0f0")
     (accent-0 "#9fc6ff")
     (accent-1 "#f0f0f0")
     (accent-2 "#005a5f")
     (accent-3 "#003497")
     (accent-4 "#0000c0")
     (alpha 0.8)
     (light? #t)
     (other '((red . "#a60000")))
     (wallpaper %light-wallpaper)))))
