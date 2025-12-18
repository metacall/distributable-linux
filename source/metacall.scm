;
;	MetaCall Distributable by Parra Studios
;	Distributable infrastructure for MetaCall.
;
;	Copyright (C) 2016 - 2025 Vicente Eduardo Ferrer Garcia <vic798@gmail.com>
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
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system python)
  #:use-module (guix build json)
  #:use-module (guix build union)
  #:use-module ((guix licenses) #:prefix license:)

  ; GNU Packages
  #:use-module (gnu packages)

  ; Python Dependencies
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)

  ; Ruby Dependencies
  #:use-module (gnu packages ruby)

  ; NodeJS Dependencies
  #:use-module (guix utils)
  #:use-module ((guix build utils) #:select (alist-replace))
  #:use-module (gnu packages node)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)

  ; RapidJSON
  #:use-module (gnu packages web)

  ; NetCore Dependencies (TODO)
  ; #:use-module (nongnu packages dotnet)
  ; #:use-module (nonguix build-system binary)

  ; Cobol Dependencies
  #:use-module (gnu packages cobol)
  #:use-module (gnu packages multiprecision)

  ; RPC Dependencies
  #:use-module (gnu packages curl)

  ; ; Backtrace Dependencies
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (gnu packages libunwind)
  ; #:use-module (gnu packages elf)
)

; NodeJS Loader Dependencies
(define-public espree
  (package
    (name "espree")
    (version "9.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/metacall/core-bootstrap.js-guix-package/releases/download/v0.0.4/espree-" version ".tgz"))
        (sha256 (base32 "1w8iy2wx6v7shr99jafi8mgcx7ma64x2mxx71kp1ixs19dg4pxr7"))
      )
    )
    (build-system node-build-system)
    (arguments
      `(
        #:phases
        (modify-phases %standard-phases
          (delete 'check)
          (delete 'configure)
          (delete 'build)
        )
      )
    )
    (home-page "https://github.com/eslint/espree")
    (synopsis "An Esprima-compatible JavaScript parser.")
    (description "Espree started out as a fork of Esprima v1.2.2, the last stable published released
of Esprima before work on ECMAScript 6 began. Espree is now built on top of Acorn, which has a modular
architecture that allows extension of core functionality. The goal of Espree is to produce output
that is similar to Esprima with a similar API so that it can be used in place of Esprima.")
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
          (delete 'configure)
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

; NetCore Loader Dependencies (TODO)
; (define-public codeanalysis-csharp
;   (package
;     (name "codeanalysis-csharp")
;     (version "3.2.1")
;     (source
;       (origin
;         (method url-fetch)
;         (uri (string-append "https://globalcdn.nuget.org/packages/microsoft.codeanalysis.csharp." version ".nupkg"))
;         (sha256 (base32 "02kyh5xsr3ciw71afzyis91m18iys1kpndl6h6ykayg9w36z9rz7"))
;       )
;     )
;     (build-system binary-build-system)
;     (arguments
;       `(#:phases
;         (modify-phases %standard-phases
;           (replace 'unpack
;             (lambda* (#:key source #:allow-other-keys)
;               (invoke "cp" source (string-append (getcwd) "/microsoft.codeanalysis.csharp.3.2.1.nupkg"))
;             )
;           )
;         )
;       )
;     )
;     (home-page "https://www.nuget.org/packages/Microsoft.CodeAnalysis.CSharp")
;     (synopsis ".NET Compiler Platform (Roslyn).")
;     (description ".NET Compiler Platform (Roslyn).")
;     (license license:expat)
;   )
; )

; (define-public codeanalysis-common
;   (package
;     (name "codeanalysis-common")
;     (version "3.2.1")
;     (source
;       (origin
;         (method url-fetch)
;         (uri (string-append "https://globalcdn.nuget.org/packages/microsoft.codeanalysis.common." version ".nupkg"))
;         (sha256 (base32 "1n3jc5fz78f7smzjanmq00iv3pdifnhkgmmsb9czrfbzc3v4c3d2"))
;       )
;     )
;     (build-system binary-build-system)
;     (arguments
;       `(#:phases
;         (modify-phases %standard-phases
;           (replace 'unpack
;             (lambda* (#:key source #:allow-other-keys)
;               (invoke "cp" source (string-append (getcwd) "/microsoft.codeanalysis.common.3.2.1.nupkg"))
;             )
;           )
;         )
;       )
;     )
;     (home-page "https://www.nuget.org/packages/Microsoft.CodeAnalysis.Common")
;     (synopsis ".NET Compiler Platform (Roslyn).")
;     (description ".NET Compiler Platform (Roslyn).")
;     (license license:expat)
;   )
; )

; (define-public codeanalysis-analyzers
;   (package
;     (name "codeanalysis-analyzers")
;     (version "2.9.3")
;     (source
;       (origin
;         (method url-fetch)
;         (uri (string-append "https://globalcdn.nuget.org/packages/microsoft.codeanalysis.analyzers." version ".nupkg"))
;         (sha256 (base32 "1kskwc9gyd2sx3zwx52qwfsl7s0xhaclmlnxvjsb4jgvpydv3xii"))
;       )
;     )
;     (build-system binary-build-system)
;     (arguments
;       `(#:phases
;         (modify-phases %standard-phases
;           (replace 'unpack
;             (lambda* (#:key source #:allow-other-keys)
;               (invoke "cp" source (string-append (getcwd) "/microsoft.codeanalysis.analyzers.2.9.3.nupkg"))
;             )
;           )
;         )
;       )
;     )
;     (home-page "https://www.nuget.org/packages/Microsoft.CodeAnalysis.Analyzers")
;     (synopsis ".NET Compiler Platform (Roslyn).")
;     (description ".NET Compiler Platform (Roslyn).")
;     (license license:expat)
;   )
; )

(define-public backward-cpp
  (package
    (name "backward-cpp")
    (version "1.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/bombela/backward-cpp")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1b2h03iwfhcsg8i4f125mlrjf8l1y7qsr2gsbkv0z03i067lykns"))))
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list
          "-DBACKWARD_TESTS=OFF"
          "-DBACKWARD_SHARED=OFF")))
    (build-system cmake-build-system)
    (synopsis "Stack trace pretty printer for C++")
    (description
     "Backward-cpp is a stack trace pretty printer for C++.
It can print annotated stack traces using debug info in the executable.")
    (home-page "https://github.com/bombela/backward-cpp")
    (license license:expat)))

(define-public plthook
  (package
    (name "plthook")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/metacall/plthook")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "07fxkd5mv6v02a4if2lz7rq236m5nfifrly8vjy6774zhn8zzs61"))))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let ((out #$output)
                (src #$source))
            (mkdir-p out)
            (copy-recursively src out))
          #t)))
    (synopsis "Hook function calls by replacing PLT (Procedure Linkage Table) entries.")
    (description
     "A utility library to hook library function calls issued by specified object files (executable and libraries).
This modifies PLT (Procedure Linkage Table) entries in ELF format used on most Unixes
or IAT (Import Address Table) entries in PE format used on Windows.")
    (home-page "https://github.com/metacall/plthook")
    (license license:expat)))

; MetaCall
(define-public metacall
  (package
    (name "metacall")
    (version "0.9.20")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
          "https://github.com/metacall/core/archive/v" version ".tar.gz"))
        (sha256 (base32 "1szvgl6zmascw6bl4vrpgrm0kfca8s6h29ma2kai0c0lkm7hq82h"))))

    (build-system cmake-build-system)
    (arguments
      `(
        #:phases
        ; TODO: This may be hidding a CMake bug with rpath on all ports,
        ; so this must be reviewed in the future
        (modify-phases %standard-phases
          (add-before 'configure 'runpath-workaround
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (setenv "LDFLAGS" (string-append "-Wl,-rpath=" out "/lib"))
                #t)))
; TODO
;           (add-before 'configure 'dotnet-packages
;            (lambda* (#:key inputs #:allow-other-keys)
;               (let (
;                     (global-pkgs "/tmp/.nuget/packages")
;                     (additional-pkgs "/tmp/.nuget/nupkgs")
;                    )
;                 (setenv "NUGET_PACKAGES" global-pkgs)
;                 (setenv "DOTNET_SKIP_FIRST_TIME_EXPERIENCE" "true")
;                 (setenv "HOME" "/tmp")
;                 (setenv "DOTNET_ROOT"
;                   (string-append (assoc-ref inputs "dotnet") "/share/dotnet"))
;                 (mkdir-p global-pkgs)
;                 (mkdir-p additional-pkgs)
;                 ; TODO: Avoid harcoded versions of CodeAnalysis
;                 (invoke "cp" (string-append (assoc-ref inputs "codeanalysis-csharp")
;                     "/microsoft.codeanalysis.csharp.3.2.1.nupkg") additional-pkgs)
;                 (invoke "cp" (string-append (assoc-ref inputs "codeanalysis-common")
;                     "/microsoft.codeanalysis.common.3.2.1.nupkg") additional-pkgs)
;                 (invoke "cp" (string-append (assoc-ref inputs "codeanalysis-analyzers")
;                     "/microsoft.codeanalysis.analyzers.2.9.3.nupkg") additional-pkgs)
;                 (with-output-to-file
;                   "source/loaders/cs_loader/netcore/source/NuGet.Config"
;                 (lambda ()
;                   (format #t "<?xml version=\"1.0\" encoding=\"utf-8\"?>
; <configuration>
;   <trustedSigners>
;     <author name=\"Microsoft\">
;       <certificate fingerprint=\"3F9001EA83C560D712C24CF213C3D312CB3BFF51EE89435D3430BD06B5D0EECE\" hashAlgorithm=\"SHA256\" allowUntrustedRoot=\"false\" />
;       <certificate fingerprint=\"AA12DA22A49BCE7D5C1AE64CC1F3D892F150DA76140F210ABD2CBFFCA2C18A27\" hashAlgorithm=\"SHA256\" allowUntrustedRoot=\"false\" />
;     </author>
;   </trustedSigners>
;   <packageSources>
;     <clear />
;     <add key=\"dotnet-pkgs\" value=\"~a\" />
;   </packageSources>
; </configuration>" (string-append (assoc-ref inputs "dotnet") "/share/dotnet/shared/Microsoft.NETCore.App/5.0.4/"))))
;               #t)))
          (add-after 'build 'build-node-loader-bootstrap-espree
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((output (string-append (getcwd) "/node_modules/espree"))
                      (espree (string-append
                        (assoc-ref inputs "espree") "/lib/node_modules/espree")))
                (mkdir-p output)
                (copy-recursively espree output))
              #t))
          (add-after 'build 'build-ts-loader-bootstrap-typescript
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((output (string-append (getcwd) "/node_modules/typescript"))
                      (typescript (string-append
                        (assoc-ref inputs "typescript") "/lib/node_modules/typescript")))
                (mkdir-p output)
                (copy-recursively typescript output))
              #t))
          (add-after 'install 'symlink-debug-executable
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((source-file (string-append (assoc-ref outputs "out") "/bin/metacallclid"))
                    (symlink-target (string-append (assoc-ref outputs "out") "/bin/metacallcli")))
                (when (file-exists? source-file)
                  (symlink source-file symlink-target)))))
          (add-after 'install 'symlink-debug-library
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((source-file (string-append (assoc-ref outputs "out") "/lib/libmetacalld.so"))
                    (symlink-target (string-append (assoc-ref outputs "out") "/lib/libmetacall.so")))
                (when (file-exists? source-file)
                  (symlink source-file symlink-target))))))

        ; TODO: Enable tests
        #:tests? #f
        ; Disable/Enable stripping in order to let debug symbols for debugging
        #:strip-binaries? #t
        #:configure-flags
        (list
          ; Disable developer warnings
          "-Wno-dev"

          ; Disable all unreproductible operations
          "-DOPTION_BUILD_GUIX=ON"

          ; Build with release mode
          "-DCMAKE_BUILD_TYPE=RelWithDebInfo"
          ; "-DCMAKE_BUILD_TYPE=Release"

          ; ; Build with debug mode
          ; "-DCMAKE_BUILD_TYPE=Debug"
          ; "-DOPTION_BUILD_ADDRESS_SANITIZER=ON"

          ; Disable stack-smashing protection and source fortify
          ; in order to improve libc portability / compatibility
          "-DOPTION_BUILD_SECURITY=OFF"

          ; Disable examples
          "-DOPTION_BUILD_EXAMPLES=OFF"

          ; Detours
          "-DOPTION_BUILD_DETOURS=ON"
          "-DOPTION_BUILD_DETOURS_PLTHOOK=ON"
          (string-append "-DPLTHook_SOURCE_DIR=" (assoc-ref %build-inputs "plthook"))

          ; Fork safety
          "-DOPTION_FORK_SAFE=ON"

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
          "-DOPTION_BUILD_LOADERS_CS=OFF" ; TODO: ON
          "-DOPTION_BUILD_LOADERS_JS=OFF" ; TODO: Implement V8 Loader
          "-DOPTION_BUILD_LOADERS_COB=ON"
          "-DOPTION_BUILD_LOADERS_RPC=ON"

          ; Ruby Loader
          (string-append "-DRuby_EXECUTABLE="
            (assoc-ref %build-inputs "ruby") "/bin/ruby")
          (string-append "-DRuby_INCLUDE_DIRS="
            (assoc-ref %build-inputs "ruby") "/include/ruby-3.3.0")
          (string-append "-DRuby_LIBRARY="
            (assoc-ref %build-inputs "ruby") "/lib/libruby.so")
          (string-append "-DRuby_VERSION=" "3.3.3")

          ; NodeJS Loader
          (string-append "-DNodeJS_EXECUTABLE="
            (assoc-ref %build-inputs "node") "/bin/node")
          (string-append "-DNodeJS_INCLUDE_DIR="
            (assoc-ref %build-inputs "node") "/include/node")
          (string-append "-DNodeJS_LIBRARY="
            (assoc-ref %build-inputs "libnode") "/lib/libnode.so.127")
          "-DNodeJS_CMAKE_DEBUG=ON"
          "-DNodeJS_SHARED_UV=ON"

          ; TODO
          ; (string-append "-DDOTNET_COMMAND="
          ;   (assoc-ref %build-inputs "dotnet") "/share/dotnet/dotnet")
          ; (string-append "-DDOTNET_CORE_PATH="
          ;   (assoc-ref %build-inputs "dotnet")
          ;   "/share/dotnet/shared/Microsoft.NETCore.App/5.0.4/")
          ; "-DDOTNET_ADDITIONAL_PACKAGES=/tmp/.nuget/nupkgs/"

          ; Cobol Loader
          (string-append "-DCOBOL_EXECUTABLE="
            (assoc-ref %build-inputs "gnucobol") "/bin/cobc")
          (string-append "-DCOBOL_INCLUDE_DIR="
            (assoc-ref %build-inputs "gnucobol") "/include")
          (string-append "-DCOBOL_LIBRARY="
            (assoc-ref %build-inputs "gnucobol") "/lib/libcob.so")

          ; RPC Loader
          (string-append "-DCURL_INCLUDE_DIR="
            (assoc-ref %build-inputs "curl") "/include/curl")

          ; TODO: Finish all loaders
          "-DOPTION_BUILD_SCRIPTS_JS=OFF"

          ; Ports
          "-DOPTION_BUILD_PORTS=ON"
          "-DOPTION_BUILD_PORTS_PY=ON"
          "-DOPTION_BUILD_PORTS_NODE=ON"
          "-DOPTION_BUILD_PORTS_TS=OFF" ; TODO: Not implemented yet
          "-DOPTION_BUILD_PORTS_CS=ON"
          "-DOPTION_BUILD_PORTS_RB=ON"

          ; Enable backtrace support
          "-DBACKWARD_TESTS=OFF"
          "-DSTACK_DETAILS_AUTO_DETECT=OFF"
          "-DSTACK_WALKING_UNWIND=OFF"
          "-DSTACK_WALKING_LIBUNWIND=ON"
          (string-append "-DBACKWARD_LIBRARIES=" (assoc-ref %build-inputs "libunwind") "/lib/libunwind.so")
          (string-append "-DBACKWARD_INCLUDE_DIRS=" (assoc-ref %build-inputs "libunwind") "/include")
          ; "-DSTACK_DETAILS_DWARF=ON"
          ; (string-append "-DBACKWARD_LIBRARIES="
          ;   (assoc-ref %build-inputs "libdwarf") "/lib/libdwarf.so" ";"
          ;   (assoc-ref %build-inputs "libelf") "/lib/libelf.so")
          ; (string-append "-DBACKWARD_INCLUDE_DIRS="
          ;   (assoc-ref %build-inputs "libdwarf") "/include/libdwarf-0" ";"
          ;   (assoc-ref %build-inputs "libelf") "/include")
          (string-append "-DBackwardCpp_SOURCE=" (assoc-ref %build-inputs "backward-cpp") "/lib/backward")
          "-DBACKWARD_SHARED=OFF"
          "-DOPTION_BUILD_PLUGINS_BACKTRACE=ON"

          ; Disable coverage
          "-DOPTION_COVERAGE=OFF")))

    (propagated-inputs
     `(
        ("python" ,python-3) ; Python Loader dependency
        ("ruby" ,ruby-3.3) ; Ruby Loader dependency
        ("node" ,node-lts) ; NodeJS Loader dependency
        ("libnode" ,libnode) ; NodeJS Loader dependency
        ("libuv" ,libuv) ; NodeJS Loader dependency
        ("espree" ,espree) ; NodeJS Loader dependency
        ("typescript" ,typescript) ; TypeScript Loader dependency
        ("gnucobol" ,gnucobol) ; Cobol Loader dependency
        ("gmp" ,gmp) ; Cobol Loader dependency
        ; TODO
        ; ("dotnet" ,dotnet) ; NetCore Loader dependency
        ; ("codeanalysis-csharp" ,codeanalysis-csharp) ; NetCore Loader dependency
        ; ("codeanalysis-common" ,codeanalysis-common) ; NetCore Loader dependency
        ; ("codeanalysis-analyzers" ,codeanalysis-analyzers) ; NetCore Loader dependency
        ("curl" ,curl) ; RPC Loader Dependency
        ("libunwind", libunwind))) ; Backtrace Plugin dependency
        ; ("libdwarf", libdwarf) ; Backtrace Plugin dependency
        ; ("libelf", libelf))) ; Backtrace Plugin dependency

    (native-inputs
     `(
        ("rapidjson" ,rapidjson) ; RapidJSON Serial dependency
        ("plthook" ,plthook) ; PLTHook dependency
        ("backward-cpp", backward-cpp))) ; Backtrace Plugin dependency

    ; Set all environment variables for subsequent packages
    (search-paths
      (list
            ; MetaCall
            (search-path-specification
              (variable "LOADER_LIBRARY_PATH")
              (files '("lib")))
            (search-path-specification
              (variable "SERIAL_LIBRARY_PATH")
              (files '("lib")))
            (search-path-specification
              (variable "DETOUR_LIBRARY_PATH")
              (files '("lib")))
            (search-path-specification
              (variable "PORT_LIBRARY_PATH")
              (files '("lib")))
            (search-path-specification
              (variable "CONFIGURATION_PATH")
              (file-type 'regular)
              (files '("configurations/global.json")))
            ; Python
            (search-path-specification
              (variable "PYTHONTZPATH")
              (files (list "share/zoneinfo")))
            ; PYTHONPATH is incompatible with Guix Python
            ; but we require it for tarball installation
            (search-path-specification
              (variable "PYTHONPATH")
              (files (list (string-append
                           "lib/python"
                           (version-major+minor (package-version python-3))
                           "/site-packages"))))
            ; NodeJS
            (search-path-specification
              (variable "NODE_PATH")
              (files '("lib/node_modules")))
            ; Ruby
            (search-path-specification
              (variable "GEM_PATH")
              (files (list (string-append "lib/ruby/vendor_ruby"))))
            (search-path-specification
              (variable "GEM_HOME")
              (files (list (string-append "lib/ruby/vendor_ruby"))))
            (search-path-specification
              (variable "BUNDLE_PATH")
              (files (list (string-append "lib/ruby/vendor_ruby"))))
            ; GCC
            (search-path-specification
              (variable "C_INCLUDE_PATH")
              (files '("include")))
            (search-path-specification
              (variable "CPLUS_INCLUDE_PATH")
              (files '("include/c++" "include")))
            (search-path-specification
              (variable "LIBRARY_PATH")
              (files '("lib" "lib64")))))

            ; TODO:
            ;
            ; Dynamic Linker Path
            ; #:use-module ((gnu packages bootstrap) #:select (glibc-dynamic-linker))
            ;
            ; (search-path-specification
            ;   (variable "GLIBC_LD_LIBRARY_PATH")
            ;   (file-type 'regular)
            ;   (files '(glibc-dynamic-linker)))

    (native-search-paths search-paths)

    (home-page "https://metacall.io/")
    (synopsis "Inter-language foreign function interface call library")
    (description "METACALL is a library that allows calling functions,
methods or procedures between programming languages.
With METACALL you can transparently execute code from / to any
programming language, for example, call Python code from NodeJS code.")
    (license license:asl2.0)))

; MetaCall Python Port
; TODO: Can it be unified with metacall package?
; https://www.futurile.net/2024/07/23/guix-package-structure-build-system-phases/
(define-public metacall-python-port
  (package
    (inherit metacall)
    (name "metacall-python-port")
    (build-system python-build-system)
    (arguments
    `(#:tests? #f
      #:phases (modify-phases %standard-phases
                  (add-before 'build 'change-directory
                    (lambda _
                      (chdir "source/ports/py_port")))
                  (delete 'test)
                  (delete 'sanity-check))))
    (inputs (list metacall))))
