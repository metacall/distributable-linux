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

`# Clone nonguix` apk --update-cache add --virtual git-deps git \
    && rm -rf /metacall/nonguix \
    && git clone https://gitlab.com/nonguix/nonguix /metacall/nonguix \
    && cd /metacall/nonguix \
    && `# Fix nonguix version` git checkout bdad9592bb425647b5535a9758f27127f586bc28 \
    && apk del git-deps \
`# Build` && guix build --fallback \
    `# dotnet codeanalysis-csharp codeanalysis-common codeanalysis-analyzers` \
    cherow typescript libnode-lts \
    -L /metacall/nonguix -L /metacall/source \
`# Exit` && exit 0 || exit 1
