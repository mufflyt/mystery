FROM docker:20.10.7-dind

RUN apk add --no-cache curl git bash

RUN curl -sSfL https://raw.githubusercontent.com/nektos/act/master/install.sh | bash

WORKDIR /github/workspace

ENTRYPOINT ["/bin/bash"]

