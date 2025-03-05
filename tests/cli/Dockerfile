#
#	MetaCall Distributable by Parra Studios
#	Distributable infrastructure for MetaCall.
#
#	Copyright (C) 2016 - 2024 Vicente Eduardo Ferrer Garcia <vic798@gmail.com>
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

RUN echo "Running tests ${CACHE_INVALIDATE}" \
	&& export LOADER_SCRIPT_PATH="/scripts/" \
	&& source /gnu/etc/profile \
	&& metacallclid test.mock \
	&& metacallclid mult.rb \
	&& metacallclid sum.py \
	&& printf "load mock test.mock\ninspect\nexit" \
	| metacallclid \
	| grep -e 'function three_str(a_str, b_str, c_str)' \
	&& printf "load rb mult.rb\ninspect\nexit" \
	| metacallclid \
	| grep -e 'function add(a, b)' -e 'function mult(a, b)' \
	&& printf "load py sum.py\ninspect\nexit" \
	| metacallclid \
	| grep -e 'function sum(a, b, c)' \
	&& metacallclid port-test.js \
	| grep "Node.js port works" \
	&& metacallclid port-test.py \
	| grep "Python port works"

# TODO: C# Loader
#	&& printf "load cs say.cs\ninspect\nexit" \
#	| metacallclid

# TODO: Ruby Port
#	&& metacallclid port-test.rb \
#	| grep "Ruby port works"

ENTRYPOINT ["sh", "-c"]
