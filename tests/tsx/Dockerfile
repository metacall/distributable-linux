#
#	MetaCall Distributable by Parra Studios
#	Distributable infrastructure for MetaCall.
#
#	Copyright (C) 2016 - 2025 Vicente Eduardo Ferrer Garcia <vic798@gmail.com>
#
#	Licensed under the Apache License, Version 2.0 (the "License");
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

FROM busybox:1.31.1 AS tsx_test

# Image descriptor
LABEL copyright.name="Vicente Eduardo Ferrer Garcia" \
	copyright.address="vic798@gmail.com" \
	maintainer.name="Vicente Eduardo Ferrer Garcia" \
	maintainer.address="vic798@gmail.com" \
	vendor="MetaCall Inc." \
	version="0.1"

COPY out/tarball.tar.gz /

RUN cd / \
	&& tar --no-same-owner --no-same-permissions -xzf tarball.tar.gz \
	&& rm tarball.tar.gz

COPY tests/tsx/test.tsx /scripts/
COPY tests/tsx/tsconfig.json /scripts/
COPY tests/tsx/package.json /scripts/
COPY tests/tsx/package-lock.json /scripts/

# Needed in order to load the tsconfig.json
WORKDIR /scripts

ARG CACHE_INVALIDATE

RUN echo "Running tests ${CACHE_INVALIDATE}" \
	&& set -exo pipefail \
	&& source /gnu/etc/profile \
	&& export LOADER_SCRIPT_PATH="/scripts/" \
	&& export NODE_ENV=debug \
	&& export ASAN_OPTIONS="halt_on_error=0" \
	&& npm install \
	&& printf 'load ts test.tsx\ninspect\ncall hello("world")\nexit' | metacallcli \
	| grep \
		-e '<h1>Hello world</h1>'

ENTRYPOINT ["sh", "-c"]
