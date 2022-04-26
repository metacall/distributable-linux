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

  ; Swig
  #:use-module (gnu packages swig)

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
)

; NodeJS Loader Dependencies
(define-public libnode-lts
(package/inherit node-lts
  (name "libnode-lts")
  (arguments
   (substitute-keyword-arguments (package-arguments node-lts)
     ((#:configure-flags flags ''())
      `(cons* "--shared" "--without-npm" ,flags))
     ((#:phases phases '%standard-phases)
      `(modify-phases ,phases
         (delete 'patch-npm-shebang)
         (delete 'patch-node-shebang)))))))

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
          (delete 'configure)
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

; TODO: MetaCall CLI should set some enviroment variables in order to make it work for Guixers
; See metacall/install CLI script for knowing the needed variables and paths

; MetaCall
(define-public metacall
  (package
    (name "metacall")
    (version "0.5.20")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/metacall/core/archive/v" version ".tar.gz"))
        (sha256 (base32 "0ks6j8ybfjdly311ygi4hsd3h2z0bw9bzlkymdy40xbwnlasjjqw"))
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
          (add-before 'configure 'ruby-workaround
            (lambda* (#:key inputs #:allow-other-keys)
              ; For some reason, FindRuby.cmake is working anymore, patch here
              ; the lib and include paths here directly meanwhile the problem is solved
              (substitute* "source/loaders/rb_loader/CMakeLists.txt"
                (("\\$\\{Ruby_INCLUDE_DIRS\\}") (string-append
                  (assoc-ref inputs "ruby") "/include/ruby-2.7.0" "\n"
                  (assoc-ref inputs "ruby") "/include/ruby-2.7.0/" ,(or (%current-target-system) (%current-system))))
                (("\\$\\{Ruby_LIBRARY\\}") (string-append
                  (assoc-ref inputs "ruby") "/lib/libruby.so")))
                (substitute* "source/ports/rb_port/CMakeLists.txt"
                  (("\\$\\{Ruby_INCLUDE_DIRS\\}") (string-append
                    (assoc-ref inputs "ruby") "/include/ruby-2.7.0" "\n"
                    (assoc-ref inputs "ruby") "/include/ruby-2.7.0/" ,(or (%current-target-system) (%current-system))))
                  (("\\$\\{Ruby_LIBRARY\\}") (string-append
                    (assoc-ref inputs "ruby") "/lib/libruby.so")))
                #t))
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
;                 (setenv "DOTNET_ROOT" (string-append (assoc-ref inputs "dotnet") "/share/dotnet"))
;                 (mkdir-p global-pkgs)
;                 (mkdir-p additional-pkgs)
;                 ; TODO: Avoid harcoded versions of CodeAnalysis
;                 (invoke "cp" (string-append (assoc-ref inputs "codeanalysis-csharp") "/microsoft.codeanalysis.csharp.3.2.1.nupkg") additional-pkgs)
;                 (invoke "cp" (string-append (assoc-ref inputs "codeanalysis-common") "/microsoft.codeanalysis.common.3.2.1.nupkg") additional-pkgs)
;                 (invoke "cp" (string-append (assoc-ref inputs "codeanalysis-analyzers") "/microsoft.codeanalysis.analyzers.2.9.3.nupkg") additional-pkgs)
;                 (with-output-to-file "source/loaders/cs_loader/netcore/source/NuGet.Config"
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
          "-DOPTION_BUILD_LOADERS_CS=OFF" ; TODO: ON
          "-DOPTION_BUILD_LOADERS_JS=OFF" ; TODO: Implement V8 Loader
          "-DOPTION_BUILD_LOADERS_COB=ON"
          "-DOPTION_BUILD_LOADERS_RPC=ON"

          ; TODO: This stopped working properly, delete the 'ruby-workaround
          (string-append "-DRUBY_EXECUTABLE=" (assoc-ref %build-inputs "ruby") "/bin/ruby")
          (string-append "-DRUBY_INCLUDE_DIRS=" (assoc-ref %build-inputs "ruby") "/include/ruby-2.7.0")
          (string-append "-DRUBY_LIBRARY=" (assoc-ref %build-inputs "ruby") "/lib/libruby.so")
          (string-append "-DRUBY_VERSION=" "2.7.2")

          (string-append "-DNODEJS_EXECUTABLE=" (assoc-ref %build-inputs "node-lts") "/bin/node")
          (string-append "-DNODEJS_INCLUDE_DIR=" (assoc-ref %build-inputs "node-lts") "/include/node")
          (string-append "-DNODEJS_LIBRARY=" (assoc-ref %build-inputs "libnode-lts") "/lib/libnode.so.83")
          "-DNODEJS_CMAKE_DEBUG=ON"
          "-DNODEJS_SHARED_UV=ON"

          ; TODO
          ; (string-append "-DDOTNET_COMMAND=" (assoc-ref %build-inputs "dotnet") "/share/dotnet/dotnet")
          ; (string-append "-DDOTNET_CORE_PATH=" (assoc-ref %build-inputs "dotnet") "/share/dotnet/shared/Microsoft.NETCore.App/5.0.4/")
          ; "-DDOTNET_ADDITIONAL_PACKAGES=/tmp/.nuget/nupkgs/"

          (string-append "-DCOBOL_EXECUTABLE=" (assoc-ref %build-inputs "gnucobol") "/bin/cobc")
          (string-append "-DCOBOL_INCLUDE_DIR=" (assoc-ref %build-inputs "gnucobol") "/include")
          (string-append "-DCOBOL_LIBRARY=" (assoc-ref %build-inputs "gnucobol") "/lib/libcob.so")

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
        ("python" ,python-3.9) ; Python Loader dependency
        ("ruby" ,ruby-2.7) ; Ruby Loader dependency
        ("node-lts" ,node-lts) ; NodeJS Loader dependency
        ("libnode-lts" ,libnode-lts) ; NodeJS Loader dependency
        ("libuv" ,libuv) ; NodeJS Loader dependency
        ("cherow" ,cherow) ; NodeJS Loader dependency
        ("typescript" ,typescript) ; TypeScript Loader dependency
        ("gnucobol" ,gnucobol) ; Cobol Loader dependency
        ("gmp" ,gmp) ; Cobol Loader dependency
        ; TODO
        ; ("dotnet" ,dotnet) ; NetCore Loader dependency
        ; ("codeanalysis-csharp" ,codeanalysis-csharp) ; NetCore Loader dependency
        ; ("codeanalysis-common" ,codeanalysis-common) ; NetCore Loader dependency
        ; ("codeanalysis-analyzers" ,codeanalysis-analyzers) ; NetCore Loader dependency
        ("libcurl" ,curl-minimal) ; RPC Loader Dependency
      )
    )
    (native-inputs
     `(
        ("rapidjson" ,rapidjson) ; RapidJSON Serial dependency
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
