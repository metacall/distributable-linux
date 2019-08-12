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

FROM metacall/distributable:linux-libc-${METACALL_ARCH} AS libc

FROM metacall/distributable:linux-base-${METACALL_ARCH} AS builder

ARG METACALL_PATH

# Copy libc
COPY --from=libc ${METACALL_PATH}/libc ${METACALL_PATH}/libc

# Set linker flags
ENV LDFLAGS=" \
		-L${METACALL_PATH}/libc/lib \
		-Wl,-rpath=${METACALL_PATH}/libc/lib \
		-Wl,--dynamic-linker=${METACALL_PATH}/libc/lib/ld.so"

# Create output path
RUN mkdir -p ${METACALL_PATH}/python

# Install zlib
FROM builder AS builder_zlib

ARG METACALL_ZLIB_VERSION

RUN git clone -j8 --single-branch --branch v${METACALL_ZLIB_VERSION} https://github.com/madler/zlib.git zlib \
	&& cd zlib \
	&& ./configure \
		--prefix=${METACALL_PATH}/python \
	&& make -j $(nproc) \
	&& make install \
	&& cd .. \
	&& rm -rf zlib

# Install libffi
FROM builder AS builder_libffi

ARG METACALL_LIBFFI_VERSION

RUN git clone -j8 --single-branch --branch v${METACALL_LIBFFI_VERSION} https://github.com/libffi/libffi.git libffi \
	&& cd libffi \
	&& ./autogen.sh \
	&& sed -e '/^includesdir/ s/$(libdir).*$/$(includedir)/' -i include/Makefile.in \
	&& sed -e '/^includedir/ s/=.*$/=@includedir@/' -e 's/^Cflags: -I${includedir}/Cflags:/' -i libffi.pc.in \
	&& ./configure \
		--prefix=${METACALL_PATH}/python \
		--disable-static \
		--disable-docs \
	&& make -j $(nproc) \
	&& make install \
	&& cd .. \
	&& rm -rf libffi

# Install libssl
FROM builder AS builder_libssl

ARG METACALL_LIBSSL_VERSION

RUN git clone -j8 --recursive --single-branch --branch ${METACALL_LIBSSL_VERSION} https://github.com/openssl/openssl.git libssl \
	&& cd libssl \
	&& ./config \
		--prefix=${METACALL_PATH}/python \
		--openssldir=${METACALL_PATH}/python \
		--with-zlib-include=${METACALL_PATH}/python/include \
		--with-zlib-lib=${METACALL_PATH}/python/lib \
		shared \
		no-tests \
		zlib \
		zlib-dynamic \
	&& make -j $(nproc) \
	&& make install_sw \
	&& cd .. \
	&& rm -rf libssl

# Install libbz2
FROM builder AS builder_libbz2

ARG METACALL_LIBBZ2_VERESION

RUN git clone -j8 --single-branch --branch bzip2-${METACALL_LIBBZ2_VERESION} https://sourceware.org/git/bzip2.git bzip2 \
	&& cd bzip2 \
	&& sed -i 's@\(ln -s -f \)$(PREFIX)/bin/@\1@' Makefile \
	&& make -j $(nproc) -f Makefile-libbz2_so \
	&& cp libbz2.so.${METACALL_LIBBZ2_VERESION} ${METACALL_PATH}/python/lib/libbz2.so.${METACALL_LIBBZ2_VERESION} \
	&& ln -rs ${METACALL_PATH}/python/lib/libbz2.so.${METACALL_LIBBZ2_VERESION} ${METACALL_PATH}/python/lib/libbz2.so \
	&& make clean \
	&& make -j $(nproc) \
	&& make install PREFIX=${METACALL_PATH}/python \
	&& cd .. \
	&& rm -rf bzip2

# Install libncurses
FROM builder AS builder_libncurses

ARG METACALL_LIBNCURSES_VERSION

RUN curl https://ftp.gnu.org/gnu/ncurses/ncurses-${METACALL_LIBNCURSES_VERSION}.tar.gz --output ncurses.tar.gz \
	&& curl https://ftp.gnu.org/gnu/ncurses/ncurses-${METACALL_LIBNCURSES_VERSION}.tar.gz.sig --output ncurses.tar.gz.sig \
	&& gpg --verify ncurses.tar.gz.sig ncurses.tar.gz \
	&& tar -xzf ncurses.tar.gz \
	&& cd ncurses-${METACALL_LIBNCURSES_VERSION} \
	&& sed -i '/LIBTOOL_INSTALL/d' c++/Makefile.in \
	&& ./configure \
		--prefix=${METACALL_PATH}/python \
		--with-shared \
		--without-debug \
		--without-normal \
		--without-manpages \
		--without-ada \
		--enable-widec \
		--enable-pc-files \
	&& make -j $(nproc) \
	&& make install \
	&& cd .. \
	&& rm -rf ncurses-${METACALL_LIBNCURSES_VERSION}

# Install libgdbm
FROM builder AS builder_libgdbm

ARG METACALL_LIBGDBM_VERSION

RUN curl https://ftp.gnu.org/gnu/gdbm/gdbm-${METACALL_LIBGDBM_VERSION}.tar.gz --output gdbm.tar.gz \
	&& curl https://ftp.gnu.org/gnu/gdbm/gdbm-${METACALL_LIBGDBM_VERSION}.tar.gz.sig --output gdbm.tar.gz.sig \
	&& gpg --verify gdbm.tar.gz.sig gdbm.tar.gz \
	&& tar -xzf gdbm.tar.gz \
	&& cd gdbm-${METACALL_LIBGDBM_VERSION} \
	&& ./configure \
		--prefix=${METACALL_PATH}/python \
		--disable-static \
		--enable-libgdbm-compat \
	&& make -j $(nproc) \
	&& make install \
	&& cd .. \
	&& rm -rf gdbm-${METACALL_LIBGDBM_VERSION}

# Install liblzma
FROM builder AS builder_liblzma

ARG METACALL_LIBLZMA_VERSION

RUN git clone -j8 --single-branch --branch v${METACALL_LIBLZMA_VERSION} https://git.tukaani.org/xz.git xz \
	&& cd xz \
	&& ./autogen.sh \
	&& ./configure \
		--prefix=${METACALL_PATH}/python \
		--disable-xz \
		--disable-xzdec \
		--disable-lzmadec \
		--disable-lzmainfo \
		--disable-scripts \
		--disable-doc \
	&& make -j $(nproc) \
	&& make install \
	&& cd .. \
	&& rm -rf xz

# Install libsqlite3
FROM builder AS builder_libsqlite3

ARG METACALL_LIBSQLITE3_VERSION
ARG METACALL_LIBSQLITE3_SHA1

RUN export METACALL_LIBSQLITE3_VERSION=$(printf '%s' ${METACALL_LIBSQLITE3_VERSION} | tr '.' ' ' | xargs printf '%01g%02d%02d%02d') \
	&& curl https://sqlite.org/2019/sqlite-autoconf-${METACALL_LIBSQLITE3_VERSION}.tar.gz --output sqlite3.tar.gz \
	&& echo "${METACALL_LIBSQLITE3_SHA1}  sqlite3.tar.gz" > sqlite3.sha1 \
	&& sha1sum -c sqlite3.sha1 \
	&& tar -xzf sqlite3.tar.gz \
	&& cd sqlite-autoconf-${METACALL_LIBSQLITE3_VERSION} \
	&& ./configure \
		--prefix=${METACALL_PATH}/python \
		--disable-static \
		--enable-fts5 \
		CFLAGS="-g -O2 \
		-DSQLITE_ENABLE_FTS3=1 \
		-DSQLITE_ENABLE_FTS4=1 \
		-DSQLITE_ENABLE_COLUMN_METADATA=1 \
		-DSQLITE_ENABLE_UNLOCK_NOTIFY=1 \
		-DSQLITE_ENABLE_DBSTAT_VTAB=1 \
		-DSQLITE_SECURE_DELETE=1 \
		-DSQLITE_ENABLE_FTS3_TOKENIZER=1" \
	&& make -j $(nproc) \
	&& make install \
	&& cd .. \
	&& rm -rf sqlite3.tar.gz sqlite3.sha1 sqlite-autoconf-${METACALL_LIBSQLITE3_VERSION}

# # Install python dependencies
# RUN apt-get update \
# 	&& apt-get install -y --no-install-recommends \
# 		zlib1g-dev \
# 		libffi-dev \
# 		libssl-dev \
# 		libbz2-dev \
# 		libncursesw5-dev \
# 		libgdbm-dev \
# 		liblzma-dev \
# 		libsqlite3-dev \
# 		tk-dev \
# 		uuid-dev \
# 		libreadline-dev

FROM builder AS builder_python

# Copy all dependencies
COPY --from=builder_zlib ${METACALL_PATH}/python ${METACALL_PATH}/python
COPY --from=builder_libffi ${METACALL_PATH}/python ${METACALL_PATH}/python
COPY --from=builder_libssl ${METACALL_PATH}/python ${METACALL_PATH}/python
COPY --from=builder_libbz2 ${METACALL_PATH}/python ${METACALL_PATH}/python
COPY --from=builder_libncurses ${METACALL_PATH}/python ${METACALL_PATH}/python
COPY --from=builder_libgdbm ${METACALL_PATH}/python ${METACALL_PATH}/python
COPY --from=builder_liblzma ${METACALL_PATH}/python ${METACALL_PATH}/python
COPY --from=builder_libsqlite3 ${METACALL_PATH}/python ${METACALL_PATH}/python
# TODO: tk uuid libreadline

ARG METACALL_PYTHON_VERSION

# # Build CPython (TODO: --enable-optimizations)
# RUN export LIBRARY_PATH="${LIBRARY_PATH}:${METACALL_PATH}/libc/lib:${METACALL_PATH}/libc/lib64" \
# 	&& export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${METACALL_PATH}/libc/lib:${METACALL_PATH}/libc/lib64" \
# 	&& export LD_LOADER=`${METACALL_PATH}/libc/lib64/ld-linux*?.so*?` \
# 	&& git clone --branch v${METACALL_PYTHON_VERSION} --single-branch https://github.com/python/cpython.git \
# 	&& cd cpython \
# 	&& git checkout v${METACALL_PYTHON_VERSION} \
# 	&& ./configure \
# 		--prefix=${METACALL_PATH}/python \
# 		--with-lto \
# 		--enable-shared \
# 		CFLAGS="-D_FORTIFY_SOURCE=2 -g -fstack-protector-strong -Wformat -Werror=format-security" \
# 		LDFLAGS="-Wl,-z,relro -Wl,--dynamic-linker=\"${LD_LOADER}\"" \
# 	&& make -j$(nproc) \
# 	&& make install

# FROM scratch AS python

# ARG METACALL_PATH

# COPY --from=builder_python ${METACALL_PATH}/python ${METACALL_PATH}/python
