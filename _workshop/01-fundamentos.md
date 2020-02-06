---
layout: workshop
title:  "Workshop 1: Fundamentos"
workshop_date:   2020-02-07
description: "Introducción, Set Up, Creación de Aplicación, Deploy y Generadores."
---

## Instalamos ASDF
```
brew install asdf
```

## Instalamos Erlang y Elixir
```
asdf install erlang 22.2.2
asdf install elixir 1.10
iex
```

En caso de que el observer no ande, [chequear troubleshooting][before-asdf-install].

## Teoría:
* Datos, Estructuras de datos, Pattern matching.
* Idiomas propios: If, do-end, Cond, Case.
* Modulos, Funciones nombradas y guardas.
* Funciones anónimas y operador de captura.
* Recursión.

## Elixir Flashcards
* Kernel I
* Fundamentals I
* Fundamentals II

## Instalar Phoenix
Asumimos que ya está instalado node.js y postgresql.
Pero si no están, nos tomamos el tiempo de instalarlos.

```
mix local.hex
mix archive.install hex phx_new 1.4.12
```

## Configurar nueva aplicación:
```
mix phx.new yo
cd yo
```

Configuramos nuestro acceso a base de datos en `config/dev.exs`

```
mix ecto.create
mix phx.server
```

Vamos a <a href="https://127.0.0.1:4000">127.0.0.1:4000</a>

## Subir a Heroku

Creamos la aplicación en Heroku usando el Elixir buildpack:
```
heroku create --buildpack hashnuke/elixir
```

Creamos archivo `elixir_buildpack.config` y agregamos:
```
# Elixir version
elixir_version=1.10.0

# Erlang version
erlang_version=21.2.5
```

Agregamos Phoenix build pack:
```
heroku buildpacks:add https://github.com/gjaldon/heroku-buildpack-phoenix-static.git
```

En `config/prod.exs` reemplazamos:
``` elixir
url: [host: "example.com", port: 80],
```
Por:
``` elixir
http: [port: {:system, "PORT"}],
url: [scheme: "https", host: "mysterious-meadow-6277.herokuapp.com", port: 443],
force_ssl: [rewrite_on: [:x_forwarded_proto]],
```

Descomentamos de `config/prod.secret.exs` la linea:
```
ssl: true
```

En `endpoint.ex` reemplazamos:
``` elixir
websocket: true,
```
Por:
``` elixir
websocket: [timeout: 45_000],
```

Agregamos add-on de postresql para nuestra aplicación heroku:
```
heroku addons:create heroku-postgresql:hobby-dev
```

Seteamos el Pool size en Heroku:
```
heroku config:set POOL_SIZE=18
```

Generamos secret key:
```
mix phx.gen.secret
```
Y la subimos a Heroku:
```
heroku config:set SECRET_KEY_BASE=“XXXX”
```

Hacemos push a Heroku:
```
git add .
git commit -m "Use production config from Heroku ENV variables and decrease socket timeout"
git push heroku master
```

Si falla, probar lo siguiente.

1. Agregamos archivo `Procfile` con:
```
web: elixir --sname server -S mix phx.server
```

2. Agregamos `phoenix_static_buildpack.config` con:
```
node_version=8.9.0
assets_path=assets
phoenix_ex=phx
```

## Nuestra Aplicación:

- Mostrar muy por arriba estructura de aplicación (Mix.exs, routes, controller, template)

## Corremos generador de Posts
```
mix phx.gen.html Blog Post posts title:string body:string
```

- Entrar a la migración, mostrarla y mix ecto.migrate
- Agregar al router la line resources …
- Mostrar la aplicacion en `localhost:4000/posts`
- Ruta, controlador, Vistas

[before-asdf-install]: https://github.com/asdf-vm/asdf-erlang#before-asdf-install
[localhost]: "https://127.0.0.1:4000"