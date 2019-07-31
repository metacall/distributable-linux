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

# Help command
help() {
	echo
	echo "Usage:"
	echo
	echo "RESOURCES=\"...\" `basename $0` resource-type source destination pattern"
	echo "  resource-type:"
	echo "    lib: copy libraries"
	echo "    pkg: copy pkgconfig for building with the library"
	echo "  source: folder where libs are located"
	echo "  destination: folder where libs are going to be copied"
	echo "  pattern: special pattern if any, use \${lib} or \${pkg} to refer to the current resource being iterated (it must be wrapped by simple commas)"
	echo
	echo "Example:"
	echo
	echo "  RESOURCES=\"ssl sqlite3\" `basename $0` lib /usr/lib/x86_64-linux-gnu /home/root/lib 'lib\${lib}\\.so.*'"
	echo "  RESOURCES=\"zlib\" `basename $0` pkg /usr/lib/x86_64-linux-gnu/pkgconfig /home/root/pkgconfig '${pkg}.pc'"
	echo
}

lib() {
	# Define variable alias
	type=${1}
	source=${2}
	dest=${3}
	pattern=${4}

	# Create destination folder if it is not created
	mkdir -p ${dest}

	# Iterate through lib list
	for lib in ${RESOURCES}; do
		alias=`eval echo "${pattern}"`
		name=$(ls -la ${source} | grep -E "${alias}" | awk '{ print $9 }' | tr '\n' ' ' | awk '{ print $NF }' | xargs)
		target=$(ls -la ${source} | grep -E "${alias}" | awk '{ print $11 }' | tr '\n' ' ' | awk '{ print $NF }' | xargs)

		# Filter properly the name
		if [ "${target}" = "" ]; then
			target="${name}"
		else
			target=$(basename "${target}")
		fi

		# Copy library
		cp ${source}/${target} ${dest}/${target}

		# Copy symlink to the new library
		if [ "${name}" != "${target}" ]; then
			ln -s ${dest}/${target} ${dest}/${name}
		fi

		# Show new locations
		echo "${name} -> ${target}"
	done
}

pkg() {
	# Define variable alias
	type=${1}
	source=${2}
	dest=${3}
	pattern=${4}

	# Create destination folder if it is not created
	mkdir -p ${dest}/pkgconfig

	# Iterate through lib list
	for pkg in ${RESOURCES}; do
		alias=`eval echo "${pattern}"`
		pkg="${source}/${alias}"
		#sed "s#^libdir=.*#libdir=${dest}/lib#g" ${pkg} > ${dest}/pkgconfig/${alias}; \

		# Show new locations
		echo "origin -------------------------------- ${dest}/lib"
		echo "${pkg} -------------------------------- ${dest}/pkgconfig/${alias}"

	done
}

main() {
	# Check if resources are defined
	if [ -z ${RESOURCES+x} ]; then
		echo "Error: Variable RESOURCES is unset, it must be declared in order to know what resources are going to be copied."
		help
		exit 1
	fi

	# Check if resource type is defined
	if [ -z ${1+x} ]; then
		echo "Error: Resource type is not defined."
		help
		exit 1
	fi

	# Check if resource type is defined
	if [ "${1}" != "lib" ] && [ "${1}" != "pkg" ]; then
		echo "Error: Invalid resource type, use lib or pkg."
		help
		exit 1
	fi

	# Check if source folder is defined
	if [ -z ${2+x} ]; then
		echo "Error: Source folder is not defined."
		help
		exit 1
	fi

	# Check if source folder exists
	if [ ! -d "${2}" ]; then
		echo "Error: Source folder does not exist."
		help
		exit 1
	fi

	# Check if destination folder is defined
	if [ -z ${3+x} ]; then
		echo "Error: Destination folder is not defined."
		help
		exit 1
	fi

	# Check if pattern is defined
	if [ -z ${4+x} ]; then
		echo "Error: Library name pattern is not defined."
		help
		exit 1
	fi

	if [ "${1}" = "lib" ]; then
		lib "$@"
	elif [ "${1}" = "pkg" ]; then
		pkg "$@"
	fi
}

main "$@"

exit 0
