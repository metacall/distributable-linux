;
;	MetaCall Distributable by Parra Studios
;	Distributable infrastructure for MetaCall.
;
;	Copyright (C) 2016 - 2020 Vicente Eduardo Ferrer Garcia <vic798@gmail.com>
;
;	Licensed under the Apache License, Version 2.0 (the "License");
;	you may not use this file except in compliance with the License.
;	You may obtain a copy of the License at
;
;		http://www.apache.org/licenses/LICENSE-2.0
;
;	Unless required by applicable law or agreed to in writing, software
;	distributed under the License is distributed on an "AS IS" BASIS,
;	WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;	See the License for the specific language governing permissions and
;	limitations under the License.
;

(list (channel
       (name 'guix)
       (url "https://git.savannah.gnu.org/git/guix.git")
       (commit "658d28987fc6c3f01ca04184c04c6252c5a9126a")) ; v1.1.0
      (channel
       (name 'nonguix)
       (url "https://gitlab.com/nonguix/nonguix")
       (branch "master")
       (commit "29290c3c31ef8f3b73c709811178d37002c1fbc9")))
