FROM haskell:9.2.4
WORKDIR /app/user

COPY stack.yaml *.cabal ./

RUN stack build --dependencies-only

COPY . ./
RUN stack install

CMD /root/.local/bin/acorneroftheweb
EXPOSE 3000
