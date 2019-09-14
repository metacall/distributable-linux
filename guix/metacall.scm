(define-module (gnu packages metacall)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:select (asl2.0))
  #:use-module (gnu packages))

(define-public metacall
  ; (package
  ;   (name "runtime")
  ;   (version "0.1.10")
  ;   (let (commit (string-append "v" version))
  ;         (revision "1")) 
  ;   (source (origin
  ;             (method git-fetch)
  ;             (uri (git-reference
  ;                  (url (string-append "https://github.com/metacall/core/"))
  ;                  (commit commit)))))
  ;   (build-system cmake-build-system)
  ;   (synopsis "MetaCall Runtime System.")
  ;   (description "All language runtime dependencies used by MetaCall Core.")
  ;   (home-page "https://metacall.io/")
  ;   (license asl2.0)))
  (package
    (name "core")
    (version "0.1.10")
    (let (commit (string-append "v" version))
          (revision "1")) 
    (source (origin
              (method git-fetch)
              (uri (git-reference
                   (url (string-append "https://github.com/metacall/core/"))
                   (commit commit)))))
    (build-system cmake-build-system)
    (synopsis "MetaCall Core.")
    (description "Library for calling functions between programming languages.")
    (home-page "https://metacall.io/")
    (license asl2.0)))
  ; (package
  ;   (name "cli")
  ;   (version "0.1.10")
  ;   (let (commit (string-append "v" version))
  ;         (revision "1")) 
  ;   (source (origin
  ;             (method git-fetch)
  ;             (uri (git-reference
  ;                  (url (string-append "https://github.com/metacall/core/"))
  ;                  (commit commit)))))
  ;   (build-system cmake-build-system)
  ;   (synopsis "MetaCall CLI.")
  ;   (description "Command Line Interface with REPL for loading and executing function calls.")
  ;   (home-page "https://metacall.io/")
  ;   (license asl2.0)))