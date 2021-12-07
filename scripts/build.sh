#!/bin/sh

#
#	MetaCall Distributable by Parra Studios
#	Distributable infrastructure for MetaCall.
#
#	Copyright (C) 2016 - 2020 Vicente Eduardo Ferrer Garcia <vic798@gmail.com>
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

export GUILE_WARN_DEPRECATED='detailed'

# Generate a portable package tarball
`# Build` guix build --fallback metacall -L /metacall/nonguix -L /metacall/source \
`# Test` `# && guix package -i metacall -L /metacall/source` \
`# Lint` `# && guix lint metacall` \
`# Pack uses --no-grafts option in order to avoid conflicts between duplicated versions` \
`# Pack` && guix pack --no-grafts --target=x86_64-w64-mingw32 -S /gnu/bin=bin -S /gnu/etc=etc -S /gnu/lib=lib -RR metacall glibc-utf8-locales nss-certs -L /metacall/nonguix -L /metacall/source | tee build.log \
`# Copy` && mv `cat build.log | grep "tarball-pack.tar.gz"` /metacall/pack/tarball.tar.gz \
`# Exit` && exit 0 || exit 1

# TODO: Apparently this should be the standard way of finding the environment variables but it does not work
# `# Search Paths` && guix package --search-paths &> /metacall/pack/search-paths \
