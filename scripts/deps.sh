#!/bin/sh

#
#	MetaCall Distributable by Parra Studios
#	Distributable infrastructure for MetaCall.
#
#	Copyright (C) 2016 - 2024 Vicente Eduardo Ferrer Garcia <vic798@gmail.com>
#
#	Licensed under the Apache License, Version 2.0 (the "License")
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

set -euxo pipefail

export GUILE_WARN_DEPRECATED='detailed'

# Clone nonguix
apk --update-cache add --virtual git-deps git
rm -rf /metacall/nonguix
git clone https://gitlab.com/nonguix/nonguix /metacall/nonguix
cd /metacall/nonguix
git checkout e0951349603581895e0ba61f0e7410368ea1902a # Fix nonguix version
apk del git-deps

# TODO:
# # Build (i386 workaround for NodeJS)
# if [[ "${TARGETPLATFORM:-}" == "linux/386" ]]; then
#     guix install --fallback --without-tests=node node-lts
#     guix install --fallback --without-tests=node-lts libnode
# fi

# Build
guix build --fallback \
    nss-certs \
     `# dotnet codeanalysis-csharp codeanalysis-common codeanalysis-analyzers` \
    espree typescript \
    --without-tests=node-lts --without-tests=libnode  --without-tests=node-18.19.0 \
    -L /metacall/nonguix -L /metacall/source 

