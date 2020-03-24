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
export METACALL_BUILD_TRIPLET=${METACALL_BUILD_TRIPLET:-x86_64-linux-gnu}

# TODO: Do no use target when building in the host architecture

# Build dependencies
`# Info` echo "Building MetaCall Dependencies ($METACALL_BUILD_TRIPLET)" # \
`# Build (Node Build System)` && guix build node-addon-api cherow -L /metacall/source \
`# Build (GNU Build System)` && guix build --target=$METACALL_BUILD_TRIPLET libnode dynruby -L /metacall/source \
`# Exit` && exit 0 || exit 1
