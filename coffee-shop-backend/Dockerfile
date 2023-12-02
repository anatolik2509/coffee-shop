FROM haskell:9.4.7

WORKDIR /app

COPY package.yaml .
COPY stack.yaml .
COPY coffee-shop-backend.cabal .

RUN stack build --only-dependencies

COPY . .

RUN stack build

CMD $(stack path --local-install-root)/coffee-shop-backend-exe
