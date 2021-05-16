FROM gitpod/workspace-base:latest

# Dockerfile copied from https://github.com/complyue/GHCiCode under the
# BSD 3-Clause License
#
# Copyright (c) Compl Yue. All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# 1. Redistributions of source code must retain the above copyright notice, this
#    list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
#
# 3. Neither the name of the copyright holder nor the names of its
#    contributors may be used to endorse or promote products derived from
#    this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

USER gitpod

# or git will keep prompting
RUN git config --global pull.ff only

# ~/.ghcup and ~/.cabal as well as all stuff under user ${HOME} directory will,
# get lost across Gitpod workspace restarts,
# ghcup (along with GHC, cabal-install and els etc. under its management) and
# stack should be installed to /workspace/ so they are persisted as part of
# prebuilt workspace, and updatable without root priviledge, we add their
# `bin`s to PATH here
ENV PATH="/workspace/.local/bin:/workspace/.cabal/bin:/workspace/.ghcup/bin:${PATH}"

# And tell those tools the alternative home they should live in
ENV GHCUP_INSTALL_BASE_PREFIX=/workspace
ENV CABAL_DIR=/workspace/.cabal
ENV STACK_ROOT=/workspace/.stack
