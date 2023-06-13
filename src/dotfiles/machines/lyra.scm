(define-module (dotfiles machines lyra)
  #:use-module (guix scripts offload))

(list
 (build-machine
  (name "192.168.1.29")
  (systems (list "aarch64-linux"))
  (user "root")
  (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBN0KzBIzlhPkr3BhcuKt9ki6iYyMS97hpAEFIrNCa9O root@lyra")
  (private-key "/etc/ssh/ssh_host_ed25519_key")))
