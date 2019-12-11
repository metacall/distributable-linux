;
;	MetaCall Distributable by Parra Studios
;	Distributable infrastructure for MetaCall.
;
;	Copyright (C) 2016 - 2019 Vicente Eduardo Ferrer Garcia <vic798@gmail.com>
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
  #:use-module (guix packages)
  #:use-module (guix modules)
  #:use-module (guix download)
  #:use-module (guix build json)
  #:use-module (guix build union)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system node)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages python)
  ; #:use-module (gnu packages ruby)
  ; Ruby Dependencies (meanwhile all patches are applied)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages tcl)
  ; /Ruby Dependencies
  #:use-module (gnu packages web)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages node)
  #:use-module (guix utils)
)

; Ruby patch (meanwhile https://debbugs.gnu.org/cgi/bugreport.cgi?bug=38500 is solved)
(define-public ruby-dynamic
  (package
    (name "ruby-dynamic")
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
       #:configure-flags (list "--enable-shared")
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

; MetaCall
(define-public metacall
  (package
    (name "metacall")
    (version "0.1.22")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/metacall/core/archive/v" version ".tar.gz"))
        (sha256 (base32 "087xm5b7pz30k454ds4qdr3dkrhskfj3y881ns10m33hk0zglwhs"))
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
                #t))))
        ; TODO: Enable tests
        #:tests? #f
        #:configure-flags
        (list
          "-DCMAKE_BUILD_TYPE=Release"
          ; Disable stack-smashing protection and source fortify in order to improve libc portability / compatibility
          "-DOPTION_BUILD_SECURITY=OFF"
          "-DOPTION_BUILD_DIST_LIBS=ON"
          ; TODO: Enable fork safety
          "-DOPTION_FORK_SAFE=OFF"
          ; TODO: Enable examples
          "-DOPTION_BUILD_EXAMPLES=OFF"
          ; TODO: Enable tests
          "-DOPTION_BUILD_TESTS=OFF"
          "-DOPTION_BUILD_SERIALS=ON"
          "-DOPTION_BUILD_SERIALS_RAPID_JSON=ON"
          "-DOPTION_BUILD_SERIALS_METACALL=ON"
          "-DOPTION_BUILD_LOADERS=ON"
          ; TODO: Enable when tests
          "-DOPTION_BUILD_SCRIPTS=OFF"
          "-DOPTION_BUILD_LOADERS_MOCK=ON"
          "-DOPTION_BUILD_LOADERS_PY=ON"
          "-DOPTION_BUILD_SCRIPTS_PY=OFF" ; TODO: Enable when tests
          "-DOPTION_BUILD_LOADERS_RB=ON"
          "-DOPTION_BUILD_SCRIPTS_RB=OFF" ; TODO: Enable when tests

          ; TODO: Ruby versions not harcoded
          (string-append "-DRUBY_EXECUTABLE=" (assoc-ref %build-inputs "ruby-dynamic") "/bin/ruby")
          (string-append "-DRUBY_INCLUDE_DIRS=" (assoc-ref %build-inputs "ruby-dynamic") "/include/ruby-2.3.0") ; (package-version ruby))
          (string-append "-DRUBY_LIBRARY=" (assoc-ref %build-inputs "ruby-dynamic") "/lib/libruby.so")
          (string-append "-DRUBY_VERSION=2.3.8") ; (package-version ruby))

          ; `# TODO: -DDOTNET_CORE_PATH=${METACALL_PATH}/netcore/share/dotnet/shared/Microsoft.NETCore.App/${METACALL_NETCORE_VERSION}/` \

          ; TODO: Remove this and enable loaders (and tests + scripts)
          "-DOPTION_BUILD_LOADERS_CS=OFF"
          "-DOPTION_BUILD_SCRIPTS_CS=OFF"
          "-DOPTION_BUILD_LOADERS_JS=OFF"
          "-DOPTION_BUILD_SCRIPTS_JS=OFF"
          "-DOPTION_BUILD_LOADERS_NODE=OFF"
          "-DOPTION_BUILD_SCRIPTS_NODE=OFF"
          "-DOPTION_BUILD_LOADERS_FILE=ON"
          "-DOPTION_BUILD_SCRIPTS_FILE=OFF"
          "-DOPTION_BUILD_PORTS=ON"
          "-DOPTION_BUILD_PORTS_NODE=ON"
          "-DOPTION_BUILD_PORTS_PY=ON"
          ; TODO: Ruby port
          "-DOPTION_BUILD_PORTS_RB=OFF"
          "-DOPTION_COVERAGE=OFF"

          ; Python Port (Swig) requires conversion between constant to non-constant char pointer
          "-DCMAKE_CXX_FLAGS=-fpermissive"
        )
      )
    )
    (propagated-inputs
     `(
        ("rapidjson" ,rapidjson)
        ("python" ,python)
        ("ruby-dynamic" ,ruby-dynamic)
      )
    )
    (native-inputs
     `(
        ("python" ,python)
        ("ruby-dynamic" ,ruby-dynamic)
        ("node" ,node)
        ("node-addon-api" ,node-addon-api)
        ("swig" ,swig)
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
