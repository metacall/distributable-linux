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
	echo "LIBS=\"...\" `basename $0` source destination pattern"
	echo "  source: folder where libs are located"
	echo "  destination: folder where libs are going to be copied"
	echo "  pattern: special pattern if any, use \${lib} to refer to the current lib being iterated (it must be wrapped by simple commas)"
	echo
	echo "Example:"
	echo
	echo "  LIBS=\"ssl sqlite3\" `basename $0` /usr/lib/x86_64-linux-gnu /home/root/portable_libs 'lib\${lib}\\.so.*'"
	echo
}

main() {
# Check if libs are defined
	if [ -z ${LIBS+x} ]; then
		echo "Error: Variable LIBS is unset, it must be declared in order to know what libs are going to be copied."
		help
		exit 1
	fi

	# Check if source folder is defined
	if [ -z ${1+x} ]; then
		echo "Error: Source folder is not defined."
		help
		exit 1
	fi

	# Check if source folder exists
	if [ ! -d "${1}" ]; then
		echo "Error: Source folder does not exist."
		help
		exit 1
	fi

	# Check if destination folder is defined
	if [ -z ${2+x} ]; then
		echo "Error: Destination folder is not defined."
		help
		exit 1
	fi

	# Check if pattern is defined
	if [ -z ${3+x} ]; then
		echo "Error: Library name pattern is not defined."
		help
		exit 1
	fi

	# Define variable alias
	source=${1}
	dest=${2}
	pattern=${3}

	# Create destination folder if it is not created
	mkdir -p ${dest}

	# Iterate through lib list
	for lib in ${LIBS}; do
		alias=`eval echo "${pattern}"`
		name=$(ls -la ${source} | grep -E "${alias}" | awk '{ print $9 }' | tr ' ' '\n')
		target=$(ls -la ${source} | grep -E "${alias}" | awk '{ print $11 }' | tr ' ' '\n')

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

main "$@"

exit 0
