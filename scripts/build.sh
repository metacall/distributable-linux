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


export GUILE_WARN_DEPRECATED='detailed'

# Download
#guix download https://github.com/metacall/core/archive/v0.1.18.tar.gz

# Build
guix build node-addon-api metacall -L /metacall/source

# Test
#guix package -i metacall -L /metacall/source

# Lint
#guix lint metacall

# Pack
#guix pack -RR metacall -L /metacall/source | tee build.log

# Copy
#cp `cat build.log | grep "tarball-pack.tar.gz"` /metacall/pack
