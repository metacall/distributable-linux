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

FROM busybox:1.31.1 AS cli_test

# Image descriptor
LABEL copyright.name="Vicente Eduardo Ferrer Garcia" \
	copyright.address="vic798@gmail.com" \
	maintainer.name="Vicente Eduardo Ferrer Garcia" \
	maintainer.address="vic798@gmail.com" \
	vendor="MetaCall Inc." \
	version="0.1"

COPY out/tarball.tar.gz /

RUN cd / \
	&& tar --no-same-owner --no-same-permissions -xzf tarball.tar.gz \
	&& rm tarball.tar.gz

COPY tests/scripts/ /scripts/

WORKDIR /scripts

ARG CACHE_INVALIDATE

# In debug mode using address sanitizer, use halt_on_error=0 to skip programming errors from
# the core or other runtimes, this should be reviewed in metacall/core, not here
RUN echo "Running tests ${CACHE_INVALIDATE}" \
	&& set -exo pipefail \
	&& export LOADER_SCRIPT_PATH="/scripts/" \
	&& source /gnu/etc/profile \
	&& export ASAN_OPTIONS="halt_on_error=0" \
	&& metacallcli test.mock \
	&& metacallcli mult.rb \
	&& metacallcli sum.py \
	&& printf "load mock test.mock\ninspect\nexit" \
	| metacallcli \
	| grep -e 'function three_str(a_str, b_str, c_str)' \
	&& printf "load rb mult.rb\ninspect\nexit" \
	| metacallcli \
	| grep -e 'function add(a, b)' -e 'function mult(a, b)' \
	&& printf "load py sum.py\ninspect\nexit" \
	| metacallcli \
	| grep -e 'function sum(a, b, c)' \
	&& metacallcli port-test.js \
	| grep "Node.js port works" \
	&& metacallcli port-test.py \
	| grep "Python port works"

# TODO: C# Loader
#	&& printf "load cs say.cs\ninspect\nexit" \
#	| metacallcli

# TODO: Ruby Port
#	&& metacallcli port-test.rb \
#	| grep "Ruby port works"

ENTRYPOINT ["sh", "-c"]
