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

FROM metacall/distributable:linux-base-${METACALL_ARCH} AS builder

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

ARG METACALL_PYTHON_VERSION

# TODO: --enable-optimizations

RUN git clone --branch ${METACALL_PYTHON_VERSION} --single-branch https://github.com/python/cpython.git \
	&& cd cpython \
	&& git checkout ${METACALL_PYTHON_VERSION} \
	&& ./configure --prefix=/root/python --with-lto --enable-shared CFLAGS="-D_FORTIFY_SOURCE=2 -g -fstack-protector-strong -Wformat -Werror=format-security" LDFLAGS="-Wl,-z,relro" \
	&& make -j$(nproc) \
	&& make install

FROM scratch AS python

COPY --from=builder /root/python/ /
