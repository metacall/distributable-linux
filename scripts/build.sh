#!/bin/sh

#
#	MetaCall Distributable by Parra Studios
#	Distributable infrastructure for MetaCall.
#
#	Copyright (C) 2016 - 2019 Vicente Eduardo Ferrer Garcia <vic798@gmail.com>
#
#	Licensed under the Apache License, Version 2.0 (the "License")#
#	you may not use this file except in compliance with the License.
#	You may obtain a copy of the License at
#
#		http://www.apache.org/licenses/LICENSE-2.0
#
#	Unless required by applicable law or agreed to in writing, software
#	distributed under the License is distributed on an "AS IS" BASIS,
#	WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#	See the License for the specific language governing permissions and
#	limitations under the License.
#


export GUIX_LOCPATH=~/.guix-profile/lib/locale/
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export GIT_SSL_CAINFO="${GUIX_PROFILE}/etc/ssl/certs/ca-certificates.crt${GIT_SSL_CAINFO:+:}$GIT_SSL_CAINFO"
export SSL_CERT_DIR="${GUIX_PROFILE}/etc/ssl/certs${SSL_CERT_DIR:+:}$SSL_CERT_DIR"

guix package -i glibc-utf8-locales
guix package -i glibc-locales
guix package -A locale
guix package -i glibc-locales@2.23
guix package -i nss-certs
guix download https://github.com/metacall/core/
#guix package --install-from-file=/metacall.scm
