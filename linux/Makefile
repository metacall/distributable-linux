#
# MetaCall Kubernetes by Parra Studios
# Copyright (C) 2016 - 2017 Vicente Eduardo Ferrer Garcia <vic798@gmail.com>
#

.PHONY: all help default

# Default target
default: all

# TODO: Implement for all architectures

# All targets
all:
#	Build base
	@docker-compose -f base/docker-compose.yml build base64

#	Build python
	@docker-compose -f python/docker-compose.yml build base64

#	Build test
	@docker-compose -f test/docker-compose.yml build base64
