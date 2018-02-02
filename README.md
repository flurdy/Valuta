# Valuta

### A currency rates lookup service

* Includes normal (FIAT) currencies
* Includes crypto currencies


## Technology

* [Bootstrap](http://getbootstrap.com)
* [Play Framework](https://playframework.com)
* [Scala](http://scala-lang.org)
* [Redis](https://redis.io)


* [Github](https://github.com)
* [Docker](https://docker.com)
* [CircleCI](https://circleci.com)
* [Heroku](https://heroku.com)

## Try Valuta

* [valuta.flurdy.io](https://valuta.flurdy.io)

## Run Valuta

### Prerequisite

* Java
* SBT
* Git
* Redis

### Install & run
#### Locally

* `git clone git://github.com/flurdy/valuta.git`
* `cd valuta`
* `sbt test`
* `sbt run`
* `open http://localhost:9000`

#### Docker

* `docker pull flurdy/valuta:latest`
* `docker pull redis:4.0.6-alpine`
* `docker run -d --name redis -p 6379:6379 redis:4.0.6-alpine`
* `docker run -ti --rm -p 9000:9000 flurdy/valuta:latest`

#### Heroku

* `git clone git://github.com/flurdy/valuta.git`
* `heroku create myapp`
* `git push heroku`


## Security

* Low risk

## Contribute to Valuta

* Report bugs: [github.com/flurdy/valuta/issues](https://github.com/flurdy/valuta/issues)
* Fix bugs: [github.com/flurdy/valuta/pulls](https://github.com/flurdy/valuta/pulls)


## License

Apache License 2.0

## People

* Creator: Ivar Abrahamsen ([@flurdy](https://twitter.com/flurdy))

## More information

* [github.com/flurdy/valuta](https://github.com/flurdy/valuta)
* [code.flurdy.com/project/Valuta](https://code.flurdy.com/project/Valuta)

## Contact

* [flurdy.com/contact](https://flurdy.com/contact)
