#!/bin/sh

#
#	MetaCall Distributable by Parra Studios
#	Distributable infrastructure for MetaCall.
#
#	Copyright (C) 2016 - 2025 Vicente Eduardo Ferrer Garcia <vic798@gmail.com>
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

# Generate a portable package tarball
# Uses --no-grafts option in order to avoid conflicts between duplicated versions

# Debug
if [ "$1" == "debug" ]; then
    # Patch the metacall.scm with debug build type and sanitizers
    sed -i 's/"-DCMAKE_BUILD_TYPE/; "-DCMAKE_BUILD_TYPE/g' /metacall/source/metacall.scm
    sed -i \
        -e '/"-DOPTION_BUILD_GUIX=ON"/a\' \
        -e '          "-DCMAKE_BUILD_TYPE=Debug" "-DOPTION_BUILD_ADDRESS_SANITIZER=ON"' \
        /metacall/source/metacall.scm
fi

# Build
guix build metacall metacall-python-port --fallback -L /metacall/nonguix -L /metacall/source

# Install
echo 'metacall' >> /metacall/source/metacall.scm
guix package --fallback --no-grafts -f /metacall/source/metacall.scm | tee build.log

# Lint
guix lint -L /metacall/nonguix -L /metacall/source metacall || true

# Pack
guix pack --no-grafts \
    -S /gnu/bin=bin -S /gnu/etc=etc -S /gnu/lib=lib -S /gnu/include=include -S /gnu/share=share \
    -RR metacall metacall-python-port nss-certs \
    -L /metacall/nonguix -L /metacall/source | tee build.log

# Copy
mv `grep 'tarball-pack.tar.gz$' build.log` /metacall/pack/tarball.tar.gz
