FROM fukamachi/sbcl


ENV PATH=${PATH}:/root/.roswell/bin
ENV CL_SOURCE_REGISTRY=/app

RUN mkdir /app
WORKDIR /app
COPY . /app

RUN ros install rove

ENTRYPOINT ros exec rove getcmd-test.asd
