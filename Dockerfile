from ubuntu:19.04

RUN apt-get update
RUN apt-get install -y curl libcurl3-gnutls make
RUN curl -sL https://github.com/roswell/roswell/releases/download/v19.06.10.100/roswell_19.06.10.100-1_amd64.deb -o ros.deb
RUN dpkg -i ros.deb
RUN ros install qlot
ENV PATH="/root/.roswell/bin:$PATH"

RUN mkdir fanfic2
WORKDIR fanfic2
COPY . .

CMD ["qlot", "exec", "./fanfic2.ros"]
