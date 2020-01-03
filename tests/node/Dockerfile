#
#	MetaCall Distributable by Parra Studios
#	Distributable infrastructure for MetaCall.
#
#	Copyright (C) 2016 - 2020 Vicente Eduardo Ferrer Garcia <vic798@gmail.com>
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

FROM node:10.17.0-buster-slim AS node_test

# Image descriptor
LABEL copyright.name="Vicente Eduardo Ferrer Garcia" \
	copyright.address="vic798@gmail.com" \
	maintainer.name="Vicente Eduardo Ferrer Garcia" \
	maintainer.address="vic798@gmail.com" \
	vendor="MetaCall Inc." \
	version="0.1"

COPY out/tarball.tar.gz /

RUN cd / \
	&& tar -xzf tarball.tar.gz \
	&& rm tarball.tar.gz

COPY tests/node/test.js /

COPY tests/scripts/ /scripts/

ARG CACHE_INVALIDATE

RUN export NODE_ENV=debug \
	&& echo "Running tests ${CACHE_INVALIDATE}" \
	&& cd / \
	&& sed -i "s#@METACALL_LIBRARY_PATH@#`find /gnu/store/ -type d -name '*metacall*[^R]' | head -n 1`/lib/libnode_port.node#g" test.js \
	&& node test.js | grep \
		-e 'Hello World' \
		-e '666667'

ENTRYPOINT ["sh", "-c"]
