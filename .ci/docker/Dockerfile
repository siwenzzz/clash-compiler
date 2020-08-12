FROM ubuntu:20.04

ENV LANG=C.UTF-8 LC_ALL=C.UTF-8 PATH=/opt/bin:/opt/ghc/bin:/opt/cabal/bin:$PATH

ARG DEPS_GHC="curl libc6-dev libgmp-dev pkg-config"
ARG DEPS_CABAL="zlib1g-dev"
ARG DEPS_CLASH="libtinfo-dev"
ARG DEPS_CLASH_COSIM="make"

COPY ppa-trusted-keys/* /etc/apt/trusted.gpg.d/

RUN echo "deb http://ppa.launchpad.net/hvr/ghc/ubuntu bionic main" \
      > /etc/apt/sources.list.d/hvr-ghc.list \
 && apt update \
 && apt install -y --no-install-recommends --no-install-suggests \
      $DEPS_GHC $DEPS_CABAL \
      $DEPS_CLASH $DEPS_CLASH_COSIM \
      iverilog ghdl-gcc \
      ca-certificates \
      git ssh

RUN apt update \
 && apt install -y \
      cabal-install-2.4 \
      cabal-install-3.0 \
      cabal-install-3.2 \
      ghc-8.4.4 \
      ghc-8.6.5 \
      ghc-8.8.4 \
      ghc-8.10.2 \
 && apt autoremove -y --purge \
 && apt clean \
 && rm -rf /var/lib/apt/lists/*
