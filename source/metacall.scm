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

(define-module (gnu packages metacall)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:select (asl2.0))
  #:use-module (gnu packages))

; deps, core, cli

(define-public metacall
  (package
    (name "core")
    (version "0.1.12")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                   (url (string-append "https://github.com/metacall/core/"))
                   (commit (string-append "v" version))))))
              (file-name (git-file-name name version))
              (sha256
                (base32
                "0h32z15sa8sbq276j0iib0n707m8bs4p5ji9z2ah411446paad9q"))
    (build-system cmake-build-system)
    (synopsis "MetaCall Core.")
    (description "Library for calling functions between programming languages.")
    (home-page "https://metacall.io/")
    (license asl2.0)))
