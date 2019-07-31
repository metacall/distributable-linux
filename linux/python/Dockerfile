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

ARG METACALL_ARCH

FROM metacall/distributable:linux-base-${METACALL_ARCH} AS deps

RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
		zlib1g-dev \
		libffi-dev \
		libssl-dev \
		libbz2-dev \
		libncursesw5-dev \
		libgdbm-dev \
		liblzma-dev \
		libsqlite3-dev \
		tk-dev \
		uuid-dev \
		libreadline-dev

ARG METACALL_PATH

# Copy libraries
RUN \
	export LIBS=" \
	ffi \
	crypto ssl \
	formw menuw ncursesw panelw \
	gdbm gdbm_compat \
	sqlite3 \
	tk \
	"; \
		metacall-dist-lib /usr/lib/${METACALL_ARCH_PATH} ${METACALL_PATH}/python/lib 'lib${lib}\.so' && \
	export LIBS=" \
	crypt \
	z \
	bz2 \
	lzma \
	uuid \
	history readline \
	tinfo \
	"; \
		metacall-dist-lib /lib/${METACALL_ARCH_PATH} ${METACALL_PATH}/python/lib 'lib${lib}\.so' && \
	export PKGS=" \
	zlib \
	libffi \
	libcrypto libssl \
	formw menuw ncursesw panelw \
	liblzma \
	sqlite3 \
	uuid \
	tinfo \
	"; \
		metacall-dist-pkg /usr/lib/${METACALL_ARCH_PATH}/pkgconfig ${METACALL_PATH}/python '${pkg}.pc'

FROM metacall/distributable:linux-libc-${METACALL_ARCH} AS libc

FROM metacall/distributable:linux-base-${METACALL_ARCH} AS builder

ARG METACALL_PATH

# Copy libc dependencies
COPY --from=libc ${METACALL_PATH}/libc/lib/ ${METACALL_PATH}/libc/lib/

# Copy build dependencies
COPY --from=deps /usr/include/ /usr/include/
COPY --from=deps ${METACALL_PATH}/python/pkgconfig/ /usr/lib/${METACALL_ARCH_PATH}/pkgconfig/
COPY --from=deps ${METACALL_PATH}/python/lib/ ${METACALL_PATH}/python/lib/

ARG METACALL_PYTHON_VERSION

ENV LD_LIBRARY_PATH=${METACALL_PATH}/python/lib:${METACALL_PATH}/libc/lib \
	SHLIB_PATH=${METACALL_PATH}/python/lib:${METACALL_PATH}/libc/lib

# Build CPython (TODO: --enable-optimizations)
RUN export LDFLAGS="-L${METACALL_PATH}/python/lib" \
	&& git clone --branch ${METACALL_PYTHON_VERSION} --single-branch https://github.com/python/cpython.git \
	&& cd cpython \
	&& git checkout ${METACALL_PYTHON_VERSION} \
	&& ./configure --prefix=${METACALL_PATH}/python --with-lto --enable-shared CFLAGS="-D_FORTIFY_SOURCE=2 -g -fstack-protector-strong -Wformat -Werror=format-security" LDFLAGS="-Wl,-z,relro" \
	&& make -j$(nproc) \
	&& make install

FROM scratch AS python

ARG METACALL_PATH

COPY --from=builder ${METACALL_PATH}/python ${METACALL_PATH}/python
