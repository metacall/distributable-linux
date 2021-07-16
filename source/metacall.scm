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
  #:use-module (guix build-system copy)
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

  ; NodeJS Dependencies
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

  ; NetCore Dependencies
  #:use-module (nongnu packages dotnet)

  ; Cobol Dependencies
  #:use-module (gnu packages cobol)
  #:use-module (gnu packages multiprecision)

  ; RPC Dependencies
  #:use-module (gnu packages curl)
)

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

; TypeScript Loader Dependencies
(define-public typescript
  (package
    (name "typescript")
    (version "4.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://registry.npmjs.org/typescript/-/typescript-" version ".tgz"))
        (sha256 (base32 "0jpi7za7ak0ba8bm300219vhrr4raacx6s8rz3czlwxim1byyn6g"))
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
    (home-page "https://www.typescriptlang.org/")
    (synopsis "TypeScript extends JavaScript by adding types to the language. ")
    (description "TypeScript is a language for application-scale JavaScript.
TypeScript adds optional types to JavaScript that support tools for large-scale JavaScript applications for any browser,
for any host, on any OS. TypeScript compiles to readable, standards-based JavaScript.")
    (license license:asl2.0)
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

; TODO: MetaCall CLI should set some enviroment variables in order to make it work for Guixers
; See metacall/install CLI script for knowing the needed variables and paths

; MetaCall
(define-public metacall
  (package
    (name "metacall")
    (version "0.4.14")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/metacall/core/archive/v" version ".tar.gz"))
        (sha256 (base32 "0w415byx1zm3mx7qpzs8sclx6ysd2yqnv31j5b8031blcvximpr9"))
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
          (add-before 'configure 'setenv
            ; TODO: Workaround for HOME directory, move this to netcore build system in the future
            (lambda _
              ; (let ((home (string-append (getenv "NIX_BUILD_TOP") "/dotnet-home")))
              (let ((home "/tmp")
                    (packages "/tmp/.nuget/packages"))
                    (setenv "DOTNET_SKIP_FIRST_TIME_EXPERIENCE" "true")
                    (setenv "HOME" home)
                    (mkdir-p home)
                    (mkdir-p packages))
            #t))
          (add-after 'build 'build-node-loader-bootstrap-cherow
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((output (string-append (getcwd) "/node_modules/cherow"))
                      (cherow (string-append (assoc-ref inputs "cherow") "/lib/node_modules/cherow/dist/commonjs/cherow.min.js")))
                (mkdir-p output)
                (copy-file cherow (string-append output "/index.js")))
              #t))
          (add-after 'build 'build-ts-loader-bootstrap-typescript
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((output (string-append (getcwd) "/node_modules/typescript"))
                      (typescript (string-append (assoc-ref inputs "typescript") "/lib/node_modules/typescript")))
                (mkdir-p output)
                (copy-recursively typescript output))
              #t)))
        ; TODO: Enable tests
        #:tests? #f
        #:configure-flags
        (list
          ; Disable developer warnings
          "-Wno-dev"

          ; Disable all unreproductible operations
          "-DOPTION_BUILD_GUIX=ON"

          ; Build wiht release mode
          "-DCMAKE_BUILD_TYPE=Release"

          ; Disable stack-smashing protection and source fortify in order to improve libc portability / compatibility
          "-DOPTION_BUILD_SECURITY=OFF"

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
          "-DOPTION_BUILD_LOADERS_TS=ON"
          "-DOPTION_BUILD_LOADERS_CS=ON"
          "-DOPTION_BUILD_LOADERS_JS=OFF" ; TODO: Implement V8 Loader
          "-DOPTION_BUILD_LOADERS_COB=ON"
          "-DOPTION_BUILD_LOADERS_RPC=ON"

          ; TODO: Avoid harcoded versions of Ruby
          (string-append "-DRUBY_EXECUTABLE=" (assoc-ref %build-inputs "dynruby") "/bin/ruby")
          (string-append "-DRUBY_INCLUDE_DIR=" (assoc-ref %build-inputs "dynruby") "/include/ruby-2.3.0")
          (string-append "-DRUBY_LIBRARY=" (assoc-ref %build-inputs "dynruby") "/lib/libruby.so")
          (string-append "-DRUBY_VERSION=" "2.3.8")

          ; TODO: Avoid harcoded versions of NodeJS
          (string-append "-DNODEJS_EXECUTABLE=" (assoc-ref %build-inputs "node") "/bin/node")
          (string-append "-DNODEJS_INCLUDE_DIR=" (assoc-ref %build-inputs "node") "/include/node")
          (string-append "-DNODEJS_LIBRARY=" (assoc-ref %build-inputs "libnode") "/lib/libnode.so.64")
          "-DNODEJS_CMAKE_DEBUG=ON"
          "-DNODEJS_SHARED_UV=ON"

          ; TODO: Avoid harcoded versions of NetCore
          (string-append "-DDOTNET_COMMAND=" (assoc-ref %build-inputs "dotnet") "/dotnet")
          (string-append "-DDOTNET_CORE_PATH=" (assoc-ref %build-inputs "dotnet") "/shared/Microsoft.NETCore.App/5.0.4/")

          ; TODO: Avoid harcoded versions of Cobol
          (string-append "-DCOBOL_EXECUTABLE=" (assoc-ref %build-inputs "gnucobol") "/bin/cobc")
          (string-append "-DCOBOL_INCLUDE_DIR=" (assoc-ref %build-inputs "gnucobol") "/include")
          (string-append "-DCOBOL_LIBRARY=" (assoc-ref %build-inputs "gnucobol") "/lib/libcob.so.4.0.0")

          ; RPC Loader
          (string-append "-DCURL_INCLUDE_DIR=" (assoc-ref %build-inputs "libcurl") "/include/curl")

          ; TODO: Finish all loaders
          "-DOPTION_BUILD_SCRIPTS_JS=OFF"

          ; Ports
          "-DOPTION_BUILD_PORTS=ON"
          "-DOPTION_BUILD_PORTS_PY=ON"
          "-DOPTION_BUILD_PORTS_RB=ON"
          "-DOPTION_BUILD_PORTS_NODE=ON"
          "-DOPTION_BUILD_PORTS_TS=OFF" ; TODO: Not implemented yet
          "-DOPTION_BUILD_PORTS_CS=ON"

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
        ("node" ,node) ; NodeJS NPM exports
        ("libnode" ,libnode) ; NodeJS Loader dependency
        ("libuv" ,libuv) ; NodeJS Loader dependency
        ("cherow" ,cherow) ; NodeJS Loader dependency
        ("typescript" ,typescript) ; TypeScript Loader dependency
        ("gnucobol" ,gnucobol) ; Cobol Loader dependency
        ("gmp" ,gmp) ; Cobol Loader dependency
        ; ("netcore-runtime" ,netcore-runtime) ; NetCore Loader dependency
        ; ("netcore-sdk" ,netcore-sdk) ; NetCore Loader dependency
        ("libcurl" ,curl-minimal) ; RPC Loader Dependency
      )
    )
    (native-inputs
     `(
        ("rapidjson" ,rapidjson) ; RapidJSON Serial dependency
        ("python2-gyp" ,python2-gyp) ; For building NodeJS Port
        ("swig" ,swig) ; For building ports
      )
    )
    (home-page "https://metacall.io/")
    (synopsis "Inter-language foreign function interface call library.")
    (description "METACALL is a library that allows calling functions,
methods or procedures between programming languages.
With METACALL you can transparently execute code from / to any
programming language, for example, call Python code from NodeJS code.")
    (license license:asl2.0)
  )
)
