#!/usr/bin/env sh

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

# Check if libs are defined
if [ -z ${LIBS+x} ]; then
	echo "Variable LIBS is unset, it must be declared in order to know what libs are going to be copied."
	help
	exit 1
fi

# Check if source path is defined
if [ -z ${1+x} ]; then
	echo "Source path is not defined."
	help
	exit 1
fi

# Check if destination path is defined
if [ -z ${2+x} ]; then
	echo "Destination path is not defined."
	help
	exit 1
fi

# Check if pattern is defined
if [ -z ${3+x} ]; then
	echo "Library name pattern is not defined."
	help
	exit 1
fi

# Help command
help() {
	echo "Usage:"
	echo
	echo "LIBS=\"...\" `basename $0` source destination pattern"
	echo "  source: folder where libs are located"
	echo "  destination: folder where libs are going to be copied"
	echo "  pattern: special pattern if any, use \${lib} to refer to the current lib being iterated"
	echo
	echo "Example:"
	echo "LIBS=\"ssl sqlite3\" `basename $0` /usr/lib/x86_64-linux-gnu /home/root/portable_libs \"lib\$\${lib}\\.so.*\""
}

exit 0

# Copy libraries
RUN LIBS=" \
	c \
	pthread \
	dl \
	util \
	m \
	"; \
	mkdir -p ${METACALL_PATH}/libc/lib && \
	for lib in $LIBS; do \
		alias="lib${lib}.so" && \
		name=$(ls -la /lib/${METACALL_ARCH_PATH} | grep "${alias}" | awk '{ print $9 }' | tr " " "\n") && \
		target=$(ls -la /lib/${METACALL_ARCH_PATH} | grep "${alias}" | awk '{ print $11 }' | tr " " "\n") && \
		cp /lib/${METACALL_ARCH_PATH}/${target} ${METACALL_PATH}/libc/lib/${target} && \
		if [ "${name}" != "${alias}" ]; then \
			ln -s ${METACALL_PATH}/libc/lib/${target} ${METACALL_PATH}/libc/lib/${name}; \
		fi && \
		echo "${name} -> ${target}"; \
	done && \
	alias="ld-linux.*\.so$" && \
	name=$(ls -la /lib/${METACALL_ARCH_PATH} | grep -E "${alias}" | awk '{ print $9 }' | tr " " "\n") && \
	target=$(ls -la /lib/${METACALL_ARCH_PATH} | grep -E "${alias}" | awk '{ print $11 }' | tr " " "\n") && \
	cp /lib/${METACALL_ARCH_PATH}/${target} ${METACALL_PATH}/libc/lib/${target} && \
	if [ "${name}" != "${alias}" ]; then \
		ln -s ${METACALL_PATH}/libc/lib/${target} ${METACALL_PATH}/libc/lib/${name}; \
	fi && \
	echo "${name} -> ${target}"

FROM scratch AS libc

ARG METACALL_PATH

COPY --from=deps ${METACALL_PATH}/libc/lib ${METACALL_PATH}/libc/lib
