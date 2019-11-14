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

# Set c flags
ENV CFLAGS=" \
	-fPIC \
	-I${METACALL_PATH}/libc/include \
	-I${METACALL_PATH}/python/include"

# Set linker flags
ENV LDFLAGS=" \
	-L${METACALL_PATH}/libc/lib \
	-L${METACALL_PATH}/python/lib \
	-Wl,-rpath=${METACALL_PATH}/libc/lib \
	-Wl,-rpath=${METACALL_PATH}/python/lib \
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

COPY --from=builder_zlib ${METACALL_PATH}/python ${METACALL_PATH}/python

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
	&& sed -i "s@CFLAGS=@LDFLAGS=${LDFLAGS}\nCFLAGS=@" Makefile-libbz2_so \
	&& sed -i 's@-shared@-shared \$\(LDFLAGS\)@' Makefile-libbz2_so \
	&& sed -i 's@\(ln -s -f \)$(PREFIX)/bin/@\1@' Makefile \
	&& make -j $(nproc) -f Makefile-libbz2_so \
	&& mkdir -p ${METACALL_PATH}/python/lib \
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
	&& rm -rf ncurses.tar.gz ncurses.tar.gz.sig ncurses-${METACALL_LIBNCURSES_VERSION}

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
	&& rm -rf gdbm.tar.gz.sig gdbm.tar.gz gdbm-${METACALL_LIBGDBM_VERSION}

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

RUN export METACALL_LIBSQLITE3_VERSION_HEX=$(printf '%s' ${METACALL_LIBSQLITE3_VERSION} | tr '.' ' ' | xargs printf '%01g%02d%02d%02d') \
	&& curl https://sqlite.org/2019/sqlite-autoconf-${METACALL_LIBSQLITE3_VERSION_HEX}.tar.gz --output sqlite3.tar.gz \
	&& echo "${METACALL_LIBSQLITE3_SHA1}  sqlite3.tar.gz" > sqlite3.sha1 \
	&& sha1sum -c sqlite3.sha1 \
	&& tar -xzf sqlite3.tar.gz \
	&& cd sqlite-autoconf-${METACALL_LIBSQLITE3_VERSION_HEX} \
	&& ./configure \
		--prefix=${METACALL_PATH}/python \
		--disable-static \
		--enable-fts5 \
		CFLAGS="-O2 \
		-fPIC \
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
	&& rm -rf sqlite3.tar.gz sqlite3.sha1 sqlite-autoconf-${METACALL_LIBSQLITE3_VERSION_HEX}

# Install libtcl
FROM builder AS builder_libtcl

COPY --from=builder_zlib ${METACALL_PATH}/python ${METACALL_PATH}/python

ARG METACALL_LIBTCL_VERSION

RUN export METACALL_LIBTCL_VERSION_BRANCH=$(printf '%s' "${METACALL_LIBTCL_VERSION}" | tr '.' '-') \
	&& git clone -j8 --single-branch --branch core-${METACALL_LIBTCL_VERSION_BRANCH} https://github.com/tcltk/tcl.git tcl \
	&& cd tcl/unix \
	&& ./configure \
		--prefix=${METACALL_PATH}/python \
		--exec-prefix=${METACALL_PATH}/python \
		--enable-shared \
		--enable-threads \
	&& make -j $(nproc) \
	&& sed -i \
		-e "s@^\(TCL_SRC_DIR='\).*@\1${METACALL_PATH}/python/include'@" \
		-e "/TCL_B/s@='\(-L\)\?.*unix@='\1${METACALL_PATH}/python/lib@" \
		-e "/SEARCH/s/=.*/=''/" \
		tclConfig.sh \
	&& make install \
	&& make install-private-headers \
	&& ln -rs ${METACALL_PATH}/python/bin/tclsh ${METACALL_PATH}/python/bin/$(expr substr "${METACALL_LIBTCL_VERSION}" 1 3) \
	&& cd ../.. \
	&& rm -rf tcl

# Install libtk
FROM builder AS builder_libtk

COPY --from=builder_libtcl ${METACALL_PATH}/python ${METACALL_PATH}/python

ARG METACALL_LIBTK_VERSION

# TODO

# Install libuuid
FROM builder AS builder_libuuid

ARG METACALL_UTIL_LINUX_VERSION

RUN export CFLAGS="-fPIC -I${METACALL_PATH}/libc/include" \
	&& git clone -j8 --single-branch --branch v${METACALL_UTIL_LINUX_VERSION} https://github.com/karelzak/util-linux.git util-linux \
	&& cd util-linux \
	&& mkdir -p /var/lib/hwclock \
	&& ./autogen.sh \
	&& ./configure \
		--prefix=${METACALL_PATH}/python \
		--disable-all-programs \
		--disable-static \
		--enable-libuuid \
	&& make -j $(nproc) \
	&& make install \
	&& cd .. \
	&& rm -rf util-linux

# Install libreadline
FROM builder AS builder_libreadline

COPY --from=builder_libncurses ${METACALL_PATH}/python ${METACALL_PATH}/python

ARG METACALL_LIBREADLINE_VERSION

RUN curl https://ftp.gnu.org/gnu/readline/readline-${METACALL_LIBREADLINE_VERSION}.tar.gz --output readline.tar.gz \
	&& curl https://ftp.gnu.org/gnu/readline/readline-${METACALL_LIBREADLINE_VERSION}.tar.gz.sig --output readline.tar.gz.sig \
	&& gpg --verify readline.tar.gz.sig readline.tar.gz \
	&& tar -xzf readline.tar.gz \
	&& cd readline-${METACALL_LIBREADLINE_VERSION} \
	&& sed -i '/MV.*old/d' Makefile.in \
	&& sed -i '/{OLDSUFF}/c:' support/shlib-install \
	&& ./configure \
		--prefix=${METACALL_PATH}/python \
		--disable-static \
		--enable-shared \
		--enable-multibyte \
		--with-curses \
	&& make SHLIB_LIBS="-L${METACALL_PATH}/python/lib -lncursesw" -j $(nproc) \
	&& make SHLIB_LIBS="-L${METACALL_PATH}/python/lib -lncursesw" install \
	&& cd .. \
	&& rm -rf readline-${METACALL_LIBREADLINE_VERSION}

# Install libgmp
FROM builder AS builder_libgmp

ARG METACALL_LIBGMP_VERSION

RUN apt-get update \
	&& apt-get install xz-utils -y --no-install-recommends \
	&& curl https://gmplib.org/download/gmp/gmp-${METACALL_LIBGMP_VERSION}.tar.xz --output gmp.tar.xz \
	&& curl https://gmplib.org/download/gmp/gmp-${METACALL_LIBGMP_VERSION}.tar.xz.sig --output gmp.tar.xz.sig \
	&& gpg --verify gmp.tar.xz.sig gmp.tar.xz \
	&& tar -xf gmp.tar.xz \
	&& cd gmp-${METACALL_LIBGMP_VERSION} \
	&& cp configfsf.guess config.guess \
	&& cp configfsf.sub config.sub \
	&& ./configure \
		--prefix=${METACALL_PATH}/python \
		--disable-static \
		--enable-shared \
		--disable-cxx \
	&& make -j $(nproc) \
	&& make install \
	&& cd .. \
	&& rm -rf gmp-${METACALL_LIBGMP_VERSION}

# Install libmpfr
FROM builder AS builder_libmpfr

COPY --from=builder_libgmp ${METACALL_PATH}/python ${METACALL_PATH}/python

ARG METACALL_LIBMPFR_VERSION

# TODO: Move this to base image
RUN curl https://www.vinc17.net/key.asc --output mpfr-keyring.asc \
	&& gpg --quiet --import mpfr-keyring.asc \
	&& rm mpfr-keyring.asc

RUN curl https://www.mpfr.org/mpfr-current/mpfr-${METACALL_LIBMPFR_VERSION}.tar.gz --output mpfr.tar.gz \
	&& curl https://www.mpfr.org/mpfr-current/mpfr-${METACALL_LIBMPFR_VERSION}.tar.gz.asc --output mpfr.tar.gz.asc \
	&& gpg --verify mpfr.tar.gz.asc mpfr.tar.gz \
	&& tar -xzf mpfr.tar.gz \
	&& cd mpfr-${METACALL_LIBMPFR_VERSION} \
	&& ./configure \
		--prefix=${METACALL_PATH}/python \
		--disable-static \
		--enable-thread-safe \
	&& make -j $(nproc) \
	&& make install \
	&& cd .. \
	&& rm -rf mpfr-${METACALL_LIBMPFR_VERSION}

# Install libmpc
FROM builder AS builder_libmpc

COPY --from=builder_libmpfr ${METACALL_PATH}/python ${METACALL_PATH}/python

ARG METACALL_LIBMPC_VERSION

RUN curl https://ftp.gnu.org/gnu/mpc/mpc-${METACALL_LIBMPC_VERSION}.tar.gz --output mpc.tar.gz \
	&& curl https://ftp.gnu.org/gnu/mpc/mpc-${METACALL_LIBMPC_VERSION}.tar.gz.sig --output mpc.tar.gz.sig \
	&& gpg --verify mpc.tar.gz.sig mpc.tar.gz \
	&& tar -xzf mpc.tar.gz \
	&& cd mpc-${METACALL_LIBMPC_VERSION} \
	&& ./configure \
		--prefix=${METACALL_PATH}/python \
		--disable-static \
	&& make -j $(nproc) \
	&& make install \
	&& cd .. \
	&& rm -rf mpc-${METACALL_LIBMPC_VERSION}

# Install libgcc
FROM builder AS builder_libgcc

COPY --from=builder_zlib ${METACALL_PATH}/python ${METACALL_PATH}/python
COPY --from=builder_libmpc ${METACALL_PATH}/python ${METACALL_PATH}/python

ARG METACALL_LIBGCC_VERSION

RUN curl https://ftp.gnu.org/gnu/gcc/gcc-${METACALL_LIBGCC_VERSION}/gcc-${METACALL_LIBGCC_VERSION}.tar.gz --output gcc.tar.gz \
	&& curl https://ftp.gnu.org/gnu/gcc/gcc-${METACALL_LIBGCC_VERSION}/gcc-${METACALL_LIBGCC_VERSION}.tar.gz.sig --output gcc.tar.gz.sig \
	&& gpg --verify gcc.tar.gz.sig gcc.tar.gz \
	&& tar -xzf gcc.tar.gz \
	&& cd gcc-${METACALL_LIBGCC_VERSION} \
	&& case $(uname -m) in \
			x86_64) \
				sed -e '/m64=/s/lib64/lib/' -i.orig gcc/config/i386/t-linux64 \
			;; \
		esac \
	&& mkdir build \
	&& cd build \
	&& export SED=sed \
	&& ../configure \
		--prefix=${METACALL_PATH}/python \
		--enable-languages=c,c++ \
		--disable-multilib \
		--disable-bootstrap \
		--with-gmp=${METACALL_PATH}/python \
		--with-mpfr=${METACALL_PATH}/python \
		--with-system-zlib \
	&& make -j $(nproc) all-target-libgcc \
	&& make install-target-libgcc \
	&& cd .. \
	&& rm -rf gcc.tar.gz gcc.tar.gz.sig gcc-${METACALL_LIBGCC_VERSION}

# TODO: Review rpath and ldpath are not set correctly in output library libgcc_s.so.1
#	They point to the system libc and ld instead of our own versions in ${METACALL_PATH}/python/lib
RUN ls -la ${METACALL_PATH}/python/lib/libgcc_s.so.1 \
	&& ldd ${METACALL_PATH}/python/lib/libgcc_s.so.1

# Install libmpdec
FROM builder AS builder_libmpdec

ARG METACALL_LIBMPDEC_VERSION

RUN curl https://www.bytereef.org/software/mpdecimal/releases/mpdecimal-${METACALL_LIBMPDEC_VERSION}.tar.gz --output mpdecimal.tar.gz \
	&& echo "83c628b90f009470981cf084c5418329c88b19835d8af3691b930afccb7d79c7  mpdecimal.tar.gz" | sha256sum --check \
	&& tar -xzf mpdecimal.tar.gz \
	&& cd mpdecimal-${METACALL_LIBMPDEC_VERSION} \
	&& ./configure \
		--prefix=${METACALL_PATH}/python \
	&& make -j $(nproc) \
	&& make install \
	&& cd .. \
	&& rm -rf mpdecimal.tar.gz mpdecimal-${METACALL_LIBMPDEC_VERSION}

# Install libexpat
FROM builder AS builder_libexpat

ARG METACALL_LIBEXPAT_VERSION

RUN export METACALL_LIBEXPAT_VERSION_BRANCH=$(printf '%s' "${METACALL_LIBEXPAT_VERSION}" | tr '.' '_') \
	&& git clone -j8 --single-branch --branch R_${METACALL_LIBEXPAT_VERSION_BRANCH} https://github.com/libexpat/libexpat.git \
	&& cd libexpat/expat \
	&& ./buildconf.sh \
	&& ./configure \
		--prefix=${METACALL_PATH}/python \
		--disable-static \
	&& make -j$(nproc) \
	&& make install \
	&& cd ../.. \
	&& rm -rf libexpat

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
COPY --from=builder_libtk ${METACALL_PATH}/python ${METACALL_PATH}/python
COPY --from=builder_libuuid ${METACALL_PATH}/python ${METACALL_PATH}/python
COPY --from=builder_libreadline ${METACALL_PATH}/python ${METACALL_PATH}/python
COPY --from=builder_libgcc ${METACALL_PATH}/python ${METACALL_PATH}/python
COPY --from=builder_libmpdec ${METACALL_PATH}/python ${METACALL_PATH}/python
COPY --from=builder_libexpat ${METACALL_PATH}/python ${METACALL_PATH}/python

# Build CPython
# TODO: Add --enable-optimizations
# TODO: Remove --without-x11 and --with-out-ext=tk,tk/* and add x11/tk dependencies
ARG METACALL_PYTHON_VERSION

RUN git clone -j8 --single-branch --branch v${METACALL_PYTHON_VERSION} https://github.com/python/cpython.git \
	&& cd cpython \
	&& ./configure \
		--prefix=${METACALL_PATH}/python \
		--with-lto \
		--enable-shared \
		CFLAGS="${CFLAGS} \
			-I${METACALL_PATH}/python/include/ncursesw \
			-DHAVE_MEMMOVE=1 \
			-D_FORTIFY_SOURCE=2 -O3 -fstack-protector-strong -Wformat -Werror=format-security" \
		LDFLAGS="-Wl,-z,relro ${LDFLAGS} -lm -lexpat" \
	&& make -j$(nproc) \
	&& make install \
	&& cd .. \
	&& rm -rf cpython

FROM scratch AS python

ARG METACALL_PATH

COPY --from=builder_python ${METACALL_PATH}/python ${METACALL_PATH}/python
