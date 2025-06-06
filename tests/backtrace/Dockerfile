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

FROM debian:trixie-slim AS c_test

# Image descriptor
LABEL copyright.name="Vicente Eduardo Ferrer Garcia" \
	copyright.address="vic798@gmail.com" \
	maintainer.name="Vicente Eduardo Ferrer Garcia" \
	maintainer.address="vic798@gmail.com" \
	vendor="MetaCall Inc." \
	version="0.1"

SHELL ["/bin/bash", "-c"]

RUN apt-get update \
	&& apt-get install --no-install-recommends -y build-essential

COPY out/tarball.tar.gz /

RUN cd / \
	&& tar --no-same-owner --no-same-permissions -xzf tarball.tar.gz \
	&& rm tarball.tar.gz

COPY tests/backtrace/test.c /

COPY tests/scripts/ /scripts/

WORKDIR /scripts

ARG CACHE_INVALIDATE

RUN echo "Running tests ${CACHE_INVALIDATE}" \
	&& set -exo pipefail \
	&& source /gnu/etc/profile \
	&& export LDLIB_PATH="`find /gnu/store/ -type f -wholename '*-glibc-*/lib/ld-*.so*' | head -n 1`" \
	&& export LOADER_SCRIPT_PATH="/scripts/" \
	&& cd / \
	&& gcc \
		-I/gnu/include \
		test.c \
		-B$(dirname ${LDLIB_PATH}) \
		-Wl,--dynamic-linker=${LDLIB_PATH} \
		-Wl,-rpath=/gnu/lib \
		-L/gnu/lib \
		-lmetacall \
	&& if [ -f /gnu/bin/metacallclid ]; then \
		export LIBASAN_PATH="`find /gnu/store/ -name 'libasan.so' | head -n 1`" \
		&& export LIBUBSAN_PATH="`find /gnu/store/ -name 'libubsan.so' | head -n 1`" \
		&& export LD_PRELOAD="${LIBASAN_PATH} ${LIBUBSAN_PATH}" \
		&& export ASAN_OPTIONS="halt_on_error=0"; \
	fi \
	&& ./a.out &> output.txt \
		|| cat output.txt \
		&& grep output.txt \
			-e 'Stack trace (most recent call last)'

ENTRYPOINT ["sh", "-c"]
