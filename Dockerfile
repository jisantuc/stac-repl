FROM node:14.16.0-alpine3.10

RUN mkdir -p /opt/js

COPY dist/stac-repl.js /opt/js/stac-repl.js

COPY ./package.json /opt/js

WORKDIR /opt/js

RUN npm install

ENTRYPOINT ["node"]

CMD ["/opt/js/stac-repl.js"]
