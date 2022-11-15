FROM haskell:9.4.2
WORKDIR /app/user

COPY stack.yaml *.cabal ./

RUN stack build --dependencies-only

COPY . ./
RUN stack install

CMD /root/.local/bin/backend
EXPOSE 3000
