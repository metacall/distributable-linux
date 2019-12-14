#
#	MetaCall Distributable by Parra Studios
#	Distributable infrastructure for MetaCall.
#
#	Copyright (C) 2016 - 2019 Vicente Eduardo Ferrer Garcia <vic798@gmail.com>
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

FROM gcc:9.2.0 AS builder

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

COPY tests/c/test.c /

COPY tests/scripts/ /scripts/

ARG CACHE_INVALIDATE

RUN export METACALL_PATH="`find /gnu/store/ -type d -name '*metacall*[^R]' | head -n 1`" \
	&& echo "Running tests ${CACHE_INVALIDATE}" \
	&& cd / \
	&& gcc \
		-I$METACALL_PATH/include \
		test.c \
		-Wl,-rpath=${METACALL_PATH}/lib \
		-Wl,--dynamic-linker=`find /gnu/store/ -type f -wholename '*-glibc-*/lib/ld-*.so' | head -n 1` \
		-L$METACALL_PATH/lib \
		-lmetacall \
	&& ./a.out | grep \
		-e 'Hello World' \
		-e 'abc'

ENTRYPOINT ["sh", "-c"]
