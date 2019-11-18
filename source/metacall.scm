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
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (gnu packages python)
  #:use-module (gnu packages web)
  #:use-module (gnu packages swig)
)

(define-public metacall-runtime
  (package
    (name "metacall")
    (version "0.1.12")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/metacall/core/archive/v" version ".tar.gz"))
        (sha256 (base32 "1dr3ka4rvg63k4vlhn2f5idxfpl41wq5lxvwsr4rbb89ig75x09k"))
      )
    )
    (build-system cmake-build-system)
    (arguments
     '(
        ; TODO: Enable tests
        #:tests? #f
        #:configure-flags
        (list
          "-DCMAKE_BUILD_TYPE=Release"
          "-DOPTION_BUILD_DIST_LIBS=ON"
          ; TODO: Enable examples
          "-DOPTION_BUILD_EXAMPLES=OFF"
          ; TODO: Enable tests
          "-DOPTION_BUILD_TESTS=OFF"
          "-DOPTION_BUILD_SERIALS=ON"
          "-DOPTION_BUILD_SERIALS_RAPID_JSON=ON"
          "-DOPTION_BUILD_SERIALS_METACALL=ON"
          "-DOPTION_BUILD_LOADERS=ON"
          "-DOPTION_BUILD_SCRIPTS=OFF"
          "-DOPTION_BUILD_LOADERS_MOCK=ON"

          ; TODO: Remove this and enable python and ruby loaders
          "-DOPTION_BUILD_LOADERS_PY=OFF"
          "-DOPTION_BUILD_SCRIPTS_PY=OFF"
          "-DOPTION_BUILD_LOADERS_RB=OFF"
          "-DOPTION_BUILD_SCRIPTS_RB=OFF"

          ; -DPYTHON_EXECUTABLE=${METACALL_PATH}/python/bin/python${METACALL_PYTHON_VERSION} \
          ; -DOPTION_BUILD_LOADERS_PY=ON \
          ; -DOPTION_BUILD_SCRIPTS_PY=ON \
          ; `# -DRUBY_EXECUTABLE=${METACALL_PATH}/ruby/bin/ruby` \
          ; `# -DRUBY_INCLUDE_DIRS=${METACALL_PATH}/ruby/include/ruby-${METACALL_RUBY_VERSION}` \
          ; `# -DRUBY_LIBRARY=${METACALL_PATH}/ruby/lib/libruby.so` \
          ; `# -DRUBY_VERSION=${METACALL_RUBY_VERSION}` \
          ; `# TODO: -DOPTION_BUILD_LOADERS_RB=ON` \
          ; `# TODO: -DOPTION_BUILD_SCRIPTS_RB=ON` \
          ; -DOPTION_BUILD_LOADERS_RB=OFF \
          ; -DOPTION_BUILD_SCRIPTS_RB=OFF \
          ; `# TODO: -DDOTNET_CORE_PATH=${METACALL_PATH}/netcore/share/dotnet/shared/Microsoft.NETCore.App/${METACALL_NETCORE_VERSION}/` \
          "-DOPTION_BUILD_LOADERS_CS=OFF"
          "-DOPTION_BUILD_SCRIPTS_CS=OFF"
          "-DOPTION_BUILD_LOADERS_JS=OFF"
          "-DOPTION_BUILD_SCRIPTS_JS=OFF"
          "-DOPTION_BUILD_LOADERS_NODE=OFF"
          "-DOPTION_BUILD_SCRIPTS_NODE=OFF"
          "-DOPTION_BUILD_LOADERS_FILE=ON"
          "-DOPTION_BUILD_SCRIPTS_FILE=ON"
          "-DOPTION_BUILD_PORTS=ON"
          "-DOPTION_COVERAGE=OFF"

          ; Python Port (Swig) requires conversion between constant to non-constant char pointer
          "-DCMAKE_CXX_FLAGS=-fpermissive"
        )
      )
    )
    (inputs
     `(
        ("rapidjson" ,rapidjson)
        ("python-3" ,python)
      )
    )
    (native-inputs
     `(
        ("swig" ,swig)
      )
    )
    (home-page "https://metacall.io/")
    (synopsis "Inter-language foreign function interface call library")
    (description "METACALL is a library that allows calling functions,
  methods or procedures between programming languages.
  With METACALL you can transparently execute code from / to any
  programming language, for example, call Python code from JavaScript code")
    (license asl2.0)
  )
)
