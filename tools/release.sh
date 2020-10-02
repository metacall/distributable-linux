#!/bin/sh

#
#	MetaCall Distributable by Parra Studios
#	Distributable infrastructure for MetaCall.
#
#	Copyright (C) 2016 - 2020 Vicente Eduardo Ferrer Garcia <vic798@gmail.com>
#
#	Licensed under the Apache License, Version 2.0 (the "License")
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

tag_url="https://api.github.com/repos/${GH_REPO}/releases/tags/${CI_COMMIT_TAG}"
release_url="https://api.github.com/repos/${GH_REPO}/releases"
token="Authorization: token ${GH_TOKEN}"

# Create release
# CI_COMMIT_TITLE as release name
# CI_COMMIT_DESCRIPTION as release body
payload=$(printf '{"tag_name": "%s","name": "%s","body": "%s","draft": %s,"prerelease": %s}' "${CI_COMMIT_TAG}" "${CI_COMMIT_TITLE}" "${CI_COMMIT_DESCRIPTION}" "false" "false" )
api_response=$(curl -s -d "${payload}" -H "${token}" ${release_url})
echo "${api_response}"

# Get tag info
tag_info=$(curl -sH ${token} ${tag_url})
# Get release ID
eval $(echo "${tag_info}" | grep -m 1 "id.:" | grep -w id | tr : = | tr -cd '[[:alnum:]]=')
[ "${id}" ] || { echo "Error: Failed to get release id for tag: ${tag}"; echo "${tag_info}" | awk 'length($0)<100' >&2; exit 1; }
# Upload asset
upload_url="https://uploads.github.com/repos/${GH_REPO}/releases/${id}/assets?name=${ASSET_NAME}"
upload_response=$(curl -s --data-binary @"${PWD}/out/tarball.tar.gz" -H "${token}" -H "Content-Type: application/octet-stream" ${upload_url})
echo "${upload_response}"

