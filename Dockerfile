FROM alpine:3.18

ENV QUERY_FNAME query.minsql

RUN apk update && apk add ghc && apk add --no-cache musl-dev

WORKDIR /app

COPY *.hs ./
COPY Extra/ Extra/
COPY Evals/ Evals/
COPY Parsers/ Parsers/

RUN ghc -odir /bin/minsql -hidir /bin/minsql -o minsql Main.hs

CMD ["/bin/sh","-c","./minsql ext/$QUERY_FNAME"]

