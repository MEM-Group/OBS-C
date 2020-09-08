
From ubuntu:bionic

RUN apt-get update

RUN apt-get install --yes git

RUN apt-get install --yes emacs-nox

RUN apt-get install --yes build-essential m4 openjdk-8-jdk libgmp-dev \
    libmpfr-dev pkg-config flex bison z3 libz3-dev maven opam python3 \
    cmake gcc clang-8 lld-8 llvm-8-tools zlib1g-dev libboost-test-dev libyaml-dev libjemalloc-dev

RUN apt-get install --yes curl

RUN curl -sSL https://get.haskellstack.org/ | sh

RUN cd ~ \
    && git clone https://KanShuanglong:shuanglongkan2@github.com/KanShuanglong/K.git \
    && cd K/k-Jan/bin \
    && ./k-configure-opam

ENV JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64"

ENV PATH="$JAVA_HOME/bin:$PATH"

ENV PATH="/root/K/k-Jan/bin:$PATH"







