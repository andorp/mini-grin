FROM gitpod/workspace-full

USER root

RUN curl -sSL https://get.haskellstack.org/ | sh
RUN echo "deb http://archive.canonical.com/ubuntu cosmic partner" >> /etc/apt/source.list
RUN apt update
RUN apt install -y apt-utils
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 379CE192D401AB61
RUN add-apt-repository https://dl.bintray.com/souffle-lang/deb/
RUN apt-get install souffle

