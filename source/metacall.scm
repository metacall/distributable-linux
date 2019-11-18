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
    ; (inputs
    ;  `(("zlib" ,zlib))
    ; )
    (home-page "https://metacall.io/")
    (synopsis "Inter-language foreign function interface call library")
    (description "METACALL is a library that allows calling functions,
  methods or procedures between programming languages.
  With METACALL you can transparently execute code from / to any
  programming language, for example, call Python code from JavaScript code")
    (license asl2.0)
  )
)
