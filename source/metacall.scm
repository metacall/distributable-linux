;
;	MetaCall Distributable by Parra Studios
;	Distributable infrastructure for MetaCall.
;
;	Copyright (C) 2016 - 2020 Vicente Eduardo Ferrer Garcia <vic798@gmail.com>
;
;	Licensed under the Apache License, Version 2.0 (the "License");
;	you may not use this file except in compliance with the License.
;	You may obtain a copy of the License at
;
;		http://www.apache.org/licenses/LICENSE-2.0
;
;	Unless required by applicable law or agreed to in writing, software
;	distributed under the License is distributed on an "AS IS" BASIS,
;	WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;	See the License for the specific language governing permissions and
;	limitations under the License.
;

(define-module (metacall)
  ; Guix Packages
  #:use-module (guix packages)
  #:use-module (guix modules)
  #:use-module (guix download)

  ; Build Systems
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system node)
  #:use-module (guix build json)
  #:use-module (guix build union)
  #:use-module ((guix licenses) #:prefix license:)

  ; GNU Packages
  #:use-module (gnu packages)

  ; Python Dependencies
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)

  ; Ruby Dependencies
  #:use-module (gnu packages readline)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages tcl)
  #:use-module (guix utils)

  ; NodeJS
  #:use-module (gnu packages node)
  #:use-module (gnu packages base)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages linux)

  ; Swig
  #:use-module (gnu packages swig)

  ; RapidJSON
  #:use-module (gnu packages web)
)

; NodeJS
(define-public libnode
  (package
    (name "libnode")
    (version "10.16.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://nodejs.org/dist/v" version
                                  "/node-v" version ".tar.xz"))
              (sha256
               (base32
                "0236jlb1hxhzqjlmmlxipcycrndiq92c8434iyy7zshh3n4pzqqq"))
              (modules '((guix build utils)))
              (snippet
               `(begin
                  ;; Remove bundled software.
                  (for-each delete-file-recursively
                            '("deps/cares"
                              "deps/http_parser"
                              "deps/icu-small"
                              "deps/nghttp2"
                              "deps/openssl"
                              "deps/uv"
                              "deps/zlib"))
                  (substitute* "Makefile"
                    ;; Remove references to bundled software.
                    (("deps/http_parser/http_parser.gyp") "")
                    (("deps/uv/include/\\*.h") "")
                    (("deps/uv/uv.gyp") "")
                    (("deps/zlib/zlib.gyp") ""))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     ;; TODO: Purge the bundled copies from the source.
     '(#:configure-flags '("--shared"
                           "--shared-cares"
                           "--shared-http-parser"
                           "--shared-libuv"
                           "--shared-nghttp2"
                           "--shared-openssl"
                           "--shared-zlib"
                           "--without-snapshot"
                           "--with-intl=system-icu")
       ;; Run only the CI tests.  The default test target requires additional
       ;; add-ons from NPM that are not distributed with the source.
       #:test-target "test-ci-js"
       #:tests? #f ; TODO: Enable tests by removing this line
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-files
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Fix hardcoded /bin/sh references.
             (substitute* '("lib/child_process.js"
                            "lib/internal/v8_prof_polyfill.js"
                            "test/parallel/test-child-process-spawnsync-shell.js"
                            "test/parallel/test-stdio-closed.js"
                            "test/sequential/test-child-process-emfile.js")
               (("'/bin/sh'")
                (string-append "'" (which "sh") "'")))

             ;; Fix hardcoded /usr/bin/env references.
             (substitute* '("test/parallel/test-child-process-default-options.js"
                            "test/parallel/test-child-process-env.js"
                            "test/parallel/test-child-process-exec-env.js")
               (("'/usr/bin/env'")
                (string-append "'" (which "env") "'")))

             ;; FIXME: These tests fail in the build container, but they don't
             ;; seem to be indicative of real problems in practice.
             (for-each delete-file
                       '("test/parallel/test-cluster-master-error.js"
                         "test/parallel/test-cluster-master-kill.js"
                         ;; See also <https://github.com/nodejs/node/issues/25903>.
                         "test/sequential/test-performance.js"))

             ;; This requires a DNS resolver.
             (delete-file "test/parallel/test-dns.js")

             ;; These tests have an expiry date: they depend on the validity of
             ;; TLS certificates that are bundled with the source.  We want this
             ;; package to be reproducible forever, so remove those.
             ;; TODO: Regenerate certs instead.
             (for-each delete-file
                       '("test/parallel/test-tls-passphrase.js"
                         "test/parallel/test-tls-server-verify.js"))
             #t))
         (replace 'configure
           ;; Node's configure script is actually a python script, so we can't
           ;; run it with bash.
           (lambda* (#:key outputs (configure-flags '()) inputs
                     #:allow-other-keys)
             (let* ((prefix (assoc-ref outputs "out"))
                    (flags (cons (string-append "--prefix=" prefix)
                                 configure-flags)))
               (format #t "build directory: ~s~%" (getcwd))
               (format #t "configure flags: ~s~%" flags)
               ;; Node's configure script expects the CC environment variable to
               ;; be set.
               (setenv "CC" (string-append (assoc-ref inputs "gcc") "/bin/gcc"))
               (apply invoke
                      (string-append (assoc-ref inputs "python")
                                     "/bin/python")
                      "configure" flags))))
         (add-after 'patch-shebangs 'patch-npm-shebang
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((bindir (string-append (assoc-ref outputs "out")
                                           "/bin"))
                    (npm    (string-append bindir "/npm"))
                    (target (readlink npm)))
               (with-directory-excursion bindir
                 (patch-shebang target (list bindir))
                 #t)))))))
    (native-inputs
     `(("python" ,python-2)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("procps" ,procps)
       ("util-linux" ,util-linux)
       ("which" ,which)))
    (native-search-paths
     (list (search-path-specification
            (variable "NODE_PATH")
            (files '("lib/node_modules")))))
    (inputs
     `(("c-ares" ,c-ares)
       ("http-parser" ,http-parser)
       ("icu4c" ,icu4c)
       ("libuv" ,libuv)
       ("nghttp2" ,nghttp2 "lib")
       ("openssl" ,openssl)
       ("zlib" ,zlib)))
    (synopsis "Evented I/O for V8 JavaScript")
    (description "Node.js is a platform built on Chrome's JavaScript runtime
for easily building fast, scalable network applications.  Node.js uses an
event-driven, non-blocking I/O model that makes it lightweight and efficient,
perfect for data-intensive real-time applications that run across distributed
devices.")
    (home-page "https://nodejs.org/")
    (license expat)
    (properties '((timeout . 3600))))) ; 1 h

; NodeJS Loader Dependencies
(define-public cherow
  (package
    (name "cherow")
    (version "1.6.9")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://registry.npmjs.org/cherow/-/cherow-" version ".tgz"))
        (sha256 (base32 "1m397n6lzj49rhr8742c2cbcyqjrrxa56l197xvrx1sk4jgmzymf"))
      )
    )
    (build-system node-build-system)
    (arguments
      `(
        #:phases
        (modify-phases %standard-phases
          (delete 'check)
          (delete 'build)
        )
      )
    )
    (home-page "https://github.com/cherow/cherow")
    (synopsis "A very fast and lightweight, self-hosted javascript parser.")
    (description "A very fast and lightweight, standards-compliant,
self-hosted javascript parser with high focus on both performance and stability.")
    (license license:expat)
  )
)

; NodeJS Port Dependencies
(define-public node-addon-api
  (package
    (name "node-addon-api")
    (version "1.7.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/nodejs/node-addon-api/archive/" version ".tar.gz"))
        (sha256 (base32 "0i3jc5ki4dlq8l2p1wn0rw1695kr47cjx1zlkzj6h4ymzyc0i1dk"))
      )
    )
    (build-system node-build-system)
    (arguments
      `(
        #:phases
        (modify-phases %standard-phases
          (delete 'check)
        )
      )
    )
    (home-page "https://github.com/nodejs/node-addon-api/")
    (synopsis "Module for using N-API from C++")
    (description "This module contains a header-only C++ wrapper classes ...")
    (license license:expat)
  )
)

; Ruby
(define-public dynruby
  (package
    (name "dynruby")
    (version "2.3.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://cache.ruby-lang.org/pub/ruby/"
                           (version-major+minor version)
                           "/ruby-" version ".tar.xz"))
       (sha256
        (base32
         "1zhxbjff08pvbnxvn58krns6q0p6g4977q6ykfn823gxhifn63wi"))
       (modules '((guix build utils)))
       (snippet `(begin
                   ;; Remove bundled libffi
                   (delete-file-recursively "ext/fiddle/libffi-3.2.1")
                   #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:tests? #f ; TODO: Enable tests by removing this line
       #:configure-flags
       (list
         "--enable-shared"
       )
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'replace-bin-sh-and-remove-libffi
           (lambda _
             (substitute* '("Makefile.in"
                            "ext/pty/pty.c"
                            "io.c"
                            "lib/mkmf.rb"
                            "process.c"
                            "test/rubygems/test_gem_ext_configure_builder.rb"
                            "test/rdoc/test_rdoc_parser.rb"
                            "test/ruby/test_rubyoptions.rb"
                            "test/ruby/test_process.rb"
                            "test/ruby/test_system.rb"
                            "tool/rbinstall.rb")
               (("/bin/sh") (which "sh")))
             #t)))))
    (inputs
     `(("readline" ,readline)
       ("openssl" ,openssl)
       ("bzip2" ,bzip2)
       ("libffi" ,libffi)
       ("gdbm" ,gdbm)
       ("libyaml" ,libyaml)
       ("ncurses" ,ncurses)
       ("tcl" ,tcl)
       ("tk" ,tk) ; TODO: This still fails, Ruby is not able to locate Tk/Tcl lib
       ("zlib" ,zlib)))
    (native-search-paths
     (list (search-path-specification
            (variable "GEM_PATH")
            (files (list (string-append "lib/ruby/vendor_ruby"))))))
    (synopsis "Programming language interpreter")
    (description "Ruby is a dynamic object-oriented programming language with
a focus on simplicity and productivity.")
    (home-page "https://www.ruby-lang.org")
    (license license:ruby)))

; MetaCall
(define-public metacall
  (package
    (name "metacall")
    (version "0.1.30")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/metacall/core/archive/v" version ".tar.gz"))
        (sha256 (base32 "1krc1jxrdcbwd4qj8kz8dcadq75v5k9q001iif0gm1z6gb0rmijq"))
      )
    )
    (build-system cmake-build-system)
    (arguments
      `(
        #:phases
        ; TODO: This may be hidding a CMake bug with rpath on all ports, so this must be reviewed in the future
        (modify-phases %standard-phases
          (add-before 'configure 'runpath-workaround
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (setenv "LDFLAGS" (string-append "-Wl,-rpath=" out "/lib"))
                #t)))
          (add-after 'build 'build-node-loader-bootstrap-cherow
            (lambda* (#:key outputs inputs #:allow-other-keys)
              (let* ((output (string-append (assoc-ref outputs "out") "/node_modules/cherow"))
                      (cherow (string-append (assoc-ref inputs "cherow") "/lib/node_modules/cherow/dist/commonjs/cherow.min.js")))
                (format #t "Cherow Path: ~a\n" cherow)
                (format #t "Output Path: ~a\n" output)
                (mkdir-p output)
                (copy-file cherow (string-append output "/index.js")))
              #t)))
        ; TODO: Enable tests
        #:tests? #f
        #:configure-flags
        (list
          ; Disable all unreproductible operations
          "-DOPTION_BUILD_GUIX=ON"

          ; Build wiht release mode
          "-DCMAKE_BUILD_TYPE=Release"

          ; Disable stack-smashing protection and source fortify in order to improve libc portability / compatibility
          "-DOPTION_BUILD_SECURITY=OFF"

          ; Distributable libs
          "-DOPTION_BUILD_DIST_LIBS=ON"

          ; Examples
          "-DOPTION_BUILD_EXAMPLES=ON"

          ; TODO: Enable fork safety
          "-DOPTION_FORK_SAFE=OFF"
          ; TODO: Enable tests
          "-DOPTION_BUILD_TESTS=OFF"

          ; TODO: Enable when tests
          "-DOPTION_BUILD_SCRIPTS=OFF"
          "-DOPTION_BUILD_SCRIPTS_PY=OFF"
          "-DOPTION_BUILD_SCRIPTS_RB=OFF"
          "-DOPTION_BUILD_SCRIPTS_FILE=OFF"
          "-DOPTION_BUILD_SCRIPTS_NODE=OFF"
          "-DOPTION_BUILD_SCRIPTS_CS=OFF"
          "-DOPTION_BUILD_LOADERS_JS=OFF"

          ; Serials
          "-DOPTION_BUILD_SERIALS=ON"
          "-DOPTION_BUILD_SERIALS_RAPID_JSON=ON"
          "-DOPTION_BUILD_SERIALS_METACALL=ON"

          ; Loaders
          "-DOPTION_BUILD_LOADERS=ON"
          "-DOPTION_BUILD_LOADERS_MOCK=ON"
          "-DOPTION_BUILD_LOADERS_PY=ON"
          "-DOPTION_BUILD_LOADERS_RB=ON"
          "-DOPTION_BUILD_LOADERS_FILE=ON"
          "-DOPTION_BUILD_LOADERS_NODE=ON"

          ; TODO: Avoid harcoded versions of Ruby
          (string-append "-DRUBY_EXECUTABLE=" (assoc-ref %build-inputs "dynruby") "/bin/ruby")
          (string-append "-DRUBY_INCLUDE_DIR=" (assoc-ref %build-inputs "dynruby") "/include/ruby-2.3.0")
          (string-append "-DRUBY_LIBRARY=" (assoc-ref %build-inputs "dynruby") "/lib/libruby.so")
          (string-append "-DRUBY_VERSION=2.3.8")

          ; TODO: Avoid harcoded versions of NodeJS
          (string-append "-DNODEJS_EXECUTABLE=" (assoc-ref %build-inputs "node") "/bin/node")
          (string-append "-DNODEJS_INCLUDE_DIR=" (assoc-ref %build-inputs "libnode") "/include/node")
          (string-append "-DNODEJS_LIBRARY=" (assoc-ref %build-inputs "libnode") "/lib/libnode.so.64")

          ; `# TODO: -DDOTNET_CORE_PATH=${METACALL_PATH}/netcore/share/dotnet/shared/Microsoft.NETCore.App/${METACALL_NETCORE_VERSION}/` \

          ; TODO: Remove this and enable loaders
          "-DOPTION_BUILD_LOADERS_CS=OFF"
          "-DOPTION_BUILD_SCRIPTS_JS=OFF"

          ; Ports
          "-DOPTION_BUILD_PORTS=ON"
          "-DOPTION_BUILD_PORTS_PY=ON"
          "-DOPTION_BUILD_PORTS_RB=ON"
          "-DOPTION_BUILD_PORTS_NODE=ON"

          ; Disable coverage
          "-DOPTION_COVERAGE=OFF"

          ; Python Port (Swig) requires conversion between constant to non-constant char pointer
          "-DCMAKE_CXX_FLAGS=-fpermissive"
        )
      )
    )
    (propagated-inputs
     `(
        ("python" ,python) ; Python Loader dependency
        ("dynruby" ,dynruby) ; Ruby Loader dependency
        ("libnode" ,libnode) ; NodeJS Loader dependency
        ("libuv" ,libuv) ; NodeJS Loader dependency
      )
    )
    (inputs
     `(
        ("rapidjson" ,rapidjson) ; RapidJson Serial dependency
        ("python" ,python) ; For building Python Port
        ("python2-gyp" ,python2-gyp) ; For building NodeJS Port
        ("dynruby" ,dynruby) ; For building Ruby Port
        ("node" ,node) ; For building NodeJS Port
        ("node-addon-api" ,node-addon-api) ; For building NodeJS Port
        ("swig" ,swig) ; For building ports
        ("cherow" ,cherow) ; NodeJS Loader dependency
      )
    )
    (home-page "https://metacall.io/")
    (synopsis "Inter-language foreign function interface call library")
    (description "METACALL is a library that allows calling functions,
methods or procedures between programming languages.
With METACALL you can transparently execute code from / to any
programming language, for example, call Python code from JavaScript code")
    (license license:asl2.0)
  )
)
