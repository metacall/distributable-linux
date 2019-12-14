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
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages web)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages node)
  #:use-module (guix utils)
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

; MetaCall
(define-public metacall
  (package
    (name "metacall")
    (version "0.1.23")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/metacall/core/archive/v" version ".tar.gz"))
        (sha256 (base32 "0rdnhvcrap2m0241hq81ssxcci616cyicl0xpyqaik5hqblm9n79"))
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
          (string-append "-DRUBY_EXECUTABLE=" (assoc-ref %build-inputs "ruby") "/bin/ruby")
          (string-append "-DRUBY_INCLUDE_DIR=" (assoc-ref %build-inputs "ruby") "/include/ruby-2.5.0") ; (package-version ruby))
          (string-append "-DRUBY_LIBRARY=" (assoc-ref %build-inputs "ruby") "/lib/libruby.so")
          (string-append "-DRUBY_VERSION=2.5.3") ; (package-version ruby))

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
        ("ruby" ,ruby)
      )
    )
    (native-inputs
     `(
        ("python" ,python)
        ("ruby" ,ruby)
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
