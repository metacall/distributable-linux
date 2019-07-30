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

# Copy libraries
RUN LIBS=" \
	crypt \
	z \
	ffi \
	crypto ssl \
	bz2 \
	formw menuw ncursesw panelw \
	gdbm gdbm_compat \
	lzma \
	sqlite3 \
	tk \
	uuid \
	history readline \
	"; \
	mkdir -p /usr/local/metacall/python/lib \
	&& for lib in $LIBS; do \
		alias="lib${lib}.so" && \
		link=$(ls -la /usr/lib/${METACALL_ARCH_PATH}/${alias} | awk '{ print $11 }' | tr " " "\n") && \
		if [ "${link}" = "" ]; then \
			name="${alias}"; \
		else \
			name=$(basename "${link}"); \
		fi && \
		cp /usr/lib/${METACALL_ARCH_PATH}/${alias} /usr/local/metacall/python/lib/${name} && \
		if [ "${name}" != "${alias}" ]; then \
			ln -s /usr/local/metacall/python/lib/${name} /usr/local/metacall/python/lib/${alias}; \
		fi && \
		echo "${alias} -> ${name}"; \
	done

FROM metacall/distributable:linux-base-${METACALL_ARCH} AS builder

# Copy build dependencies
COPY --from=deps /usr/include/ /usr/include/
COPY --from=deps /usr/lib/${METACALL_ARCH_PATH}/pkgconfig/ /usr/lib/${METACALL_ARCH_PATH}/pkgconfig/
COPY --from=deps /usr/local/metacall/python/lib/ /usr/local/metacall/python/lib/

ARG METACALL_PYTHON_VERSION

ENV LD_LIBRARY_PATH=/usr/local/metacall/python/lib:$LD_LIBRARY_PATH \
	SHLIB_PATH=/usr/local/metacall/python/lib:$SHLIB_PATH

RUN ls -la /usr/local/metacall/python/lib

# Build CPython (TODO: --enable-optimizations)
RUN ldconfig -n -v /usr/local/metacall/python/lib \
	&& git clone --branch ${METACALL_PYTHON_VERSION} --single-branch https://github.com/python/cpython.git \
	&& cd cpython \
	&& git checkout ${METACALL_PYTHON_VERSION} \
	&& ./configure --prefix=/root/python --with-lto --enable-shared CFLAGS="-D_FORTIFY_SOURCE=2 -g -fstack-protector-strong -Wformat -Werror=format-security" LDFLAGS="-Wl,-z,relro" \
	&& make -j$(nproc) \
	&& make install

FROM scratch AS python

COPY --from=builder /root/python/ /
