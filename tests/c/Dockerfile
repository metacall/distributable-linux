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

FROM gcc:9.2.0 AS c_test

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

RUN echo "Running tests ${CACHE_INVALIDATE}" \
	&& export LOADER_SCRIPT_PATH="/scripts/" \
	&& export METACALL_PATH="/gnu/store/`ls /gnu/store/ | grep metacall | head -n 1`" \
	&& export LOADER_LIBRARY_PATH="${METACALL_PATH}/lib" \
	&& export SERIAL_LIBRARY_PATH="${METACALL_PATH}/lib" \
	&& export DETOUR_LIBRARY_PATH="${METACALL_PATH}/lib" \
	&& export PORT_LIBRARY_PATH="${METACALL_PATH}/lib" \
	&& export CONFIGURATIONS_PATH="${METACALL_PATH}/configurations/global.json" \
	&& cd / \
	&& gcc \
		-I$METACALL_PATH/include \
		test.c \
		-B$(dirname `find /gnu/store/ -type f -wholename '*-glibc-*/lib/ld-*.so' | head -n 1`) \
		-Wl,--dynamic-linker=`find /gnu/store/ -type f -wholename '*-glibc-*/lib/ld-*.so' | head -n 1` \
		-Wl,-rpath=${METACALL_PATH}/lib \
		-L$METACALL_PATH/lib \
		-lmetacall \
	&& ./a.out | grep \
		-e 'Hello World' \
		-e 'abc' \
		-e '1099'

ENTRYPOINT ["sh", "-c"]
