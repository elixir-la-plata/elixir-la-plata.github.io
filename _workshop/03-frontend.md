---
layout: workshop
title:  "Workshop 3: Frontend"
workshop_date:   2020-02-21
description: "Frontend, Plug, Endpoint, Routes, Controller, Views, Templates, Layouts."
public: false
---

## Teoría
* Procesos, receive, send, PID, self()
* Tasks
* Estado y Agent

## Elixir Cards:
* Kernel II y III
* Agent

## Antes de empezar

En `post.ex` faltó agregar el *on_delete*:
```
has_many :comments, Yo.Blog.Comment, on_delete: :delete_all
```

Vamos a cambiar los estilos. Clonar projecto:
```
git clone https://github.com/nicanor/estilo-yo.git
```
Reemplazar contenido de la carpeta `/assets/css/` y `/lib/yo_web/templates/layout/`.

Agregar class table a las tablas:
```
<table class="table">
```

Agregar class button submit-button al botón del formulario:
```
<%= submit "Save", class: "button submit-button" %>
```


-----

## Procesos

Crear un proceso:
``` elixir
spawn(fn -> 1 + 2 end)
```

Vida de un proceso:
``` elixir
pid = spawn(fn -> 1 + 2 end)
Process.alive?(pid)

self()
Process.alive?(self())
```

Cola de mensajes:
``` elixir
send self(), {:mensaje, "Hola mundo"}
send self(), {:otra_cosa, "Como va?"}

Process.info(self(), :messages)
```

Llamamos receive 3 veces (la tercera va a bloquear):
``` elixir
receive do
  {:mensaje, msg} -> msg
  {:otra_cosa, _msg} -> "No me importa"
end
```

Ejemplo más complejo:
``` elixir
send self(), {:mensaje, "Hola mundo"}
send self(), {:mensaje, "Como va?"}
send self(), {:otra_cosa, "Como va?"}
send self(), {:no_matchea, "Como va?"}
send self(), {:mensaje, "Aguante Elixir"}

Process.info(self(), :messages)

receive do
  {:mensaje, msg} -> msg
  {:otra_cosa, _msg} -> "No me importa"
end

Process.info(self(), :messages)
```

Podemos usar timeout:
``` elixir
receive do
  {:mensaje, msg} -> msg
  {:otra_cosa, _msg} -> "No me importa"
after
  1_000 -> "Pasó 1 segundo"
end
```

Linkear procesos y EXIT:
``` elixir
self()
spawn_link fn -> raise "oops" end
self()
```

### Tareas:
``` elixir
Task.start fn -> raise "oops" end
```

Async y await:
``` elixir
task = task.async(fn -> 1 + 1 end)
task = Task.await(task)
```

Estado con procesos:
``` elixir
defmodule KV do
  def start_link do
    Task.start_link(fn -> loop(%{}) end)
  end

  defp loop(map) do
    receive do
      {:get, key, caller} ->
        send caller, Map.get(map, key)
        loop(map)
      {:put, key, value} ->
        loop(Map.put(map, key, value))
    end
  end
end
```

Podemos llamar al proceso:
``` elixir
{:ok, pid} = KV.start_link()
send(pid, {:put, :hello, :world})
send(pid, {:get, :hello, self()})
flush()
```

### Agentes
``` elixir
{:ok, pid} = Agent.start_link(fn -> %{} end)
Agent.update(pid, fn map -> Map.put(map, :hello, :world) end)
Agent.get(pid, fn map -> Map.get(map, :hello) end)
```

# Plug
`%Plug.Conn` tiene información del request y el response. Correr en iex:
```
%Plug.Conn{}
```

## Endpoint

Mirada simplificada del endpoint por defecto:
``` elixir
# lib_web/endpoint.ex
defmodule YoWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :yo

  plug Plug.Static, []
  plug Plug.RequestId
  plug Plug.Telemetry, []
  plug Plug.Parsers, []
  plug Plug.MethodOverride
  plug Plug.Head
  plug Plug.Session, []
  plug YoWeb.Router
end
```

* `Plug.Static` Busca archivos estáticos en `priv/static`.
* `Plug.RequestId` genera un ID único por cada request.
* `Plug.Telemetry` agrega puntos de instrumentación para loguear el path, status code y tiempos.
* `Plug.Parsers` nos dice como parsear el Request Body.
* [`Plug.MethodOverride`](https://github.com/elixir-plug/plug/blob/v1.9.0/lib/plug/method_override.ex) usa parametro `_method` para cambiar POST por PUT, PATCH o DELETE.
* [`Plug.Head`](https://github.com/elixir-plug/plug/blob/v1.9.0/lib/plug/head.ex) convierte solicitud `HEAD` a solicitud `GET`.
* `Plug.Session` maneja las cookies de sesión y las session stores.
* `YoWeb.Router` es el último paso del endpoint.

## Agregamos un Plug
``` elixir
defp unauthorize!(conn, _) do
  conn
  |> Plug.Conn.resp(401, "No autorizado!")
  |> halt()
end

plug :unauthorize!
```

También podemos pasarle parametros:
``` elixir
defp unauthorize!(conn, options) do
  message = options[:message] || "No autorizado!"

  conn
  |> Plug.Conn.resp(401, message)
  |> halt()
end

plug :unauthorize!, message: "Paso mensaje por parámetro"
```

Y podemos escribirlo como módulo. Creamos la carpeta `lib_web/plugs/` y agregamos el archivo `unauthorized.ex`:
``` elixir
defmodule YoWeb.Plugs.Unauthorized do
  import Plug.Conn

  def init(default), do: default

  def call(conn, options) do
    message = options[:message] || "No autorizado"

    conn
    |> Plug.Conn.resp(401, message)
    |> halt()
  end
end
```

Y lo llamamos desde el endpoint:
``` elixir
plug YoWeb.Plugs.Unauthorized, message: "Ahora uso un Módulo"
```

## Rutas

La linea `resources "/posts", PostController` equivale a:
``` elixir
get "/posts", PostController, :index
get "/posts/:id/edit", PostController, :edit
get "/posts/new", PostController, :new
get "/posts/:id", PostController, :show
post "/posts", PostController, :create
put "/posts/:id", PostController, :update
patch "/posts/:id", PostController, :update
delete "/posts/:id", PostController, :delete
```

Corremos `mix phx.routes`:
```
mix phx.routes
```

Se pueden nestear resources:
``` elixir
resources "/posts", PostController do
  resources "/comments", CommentController
end
```

Plugs del router:
* `plug :accepts` define el formato de solicitud aceptado.
* `plug :fetch_session` carga `conn` con los datos de sesión.
* `plug :fetch_flash` carga `conn` con mensajes flash seteados.
* `plug :protect_from_forgery` protege de cross site forgery.
* `plug :put_secure_browser_headers` también protege de cross site forgery.

## Controladores

En Rails:
``` ruby
class PostsController < ApplicationController
  def show @post = Post.find(params[:id])
    # render "show.html"
  end
end
```

En Phoenix:
``` elixir
defmodule YoWeb.PostController do
  use YoWeb, :controller

  def show(conn, %{"id" => id}) do
    post = Blog.get_post!(id)
    render(conn, "show.html", post: post)
  end
end
```

Agregamos plug a controlador:
``` elixir
plug YoWeb.Plugs.Unauthorized, message: "Desde el controlador"
```

## Views
Las vistas se encargan de renderizar los templates.
``` elixir
defmodule YoWeb.PostView do
  use YoWeb, :view

  def render("index.html", _assigns) do
    "Hola Mundo"
  end
end
```

```
Phoenix.View.render(YoWeb.PageView, "index.html", %{})
```

## Templates:

Agregar a `show.html.erb`:
``` elixir
<%= "Puedo embeber código Elixir" %>
```

En `Post.View` agregar:
``` elixir
def saludo do
  "Hola Mundo"
end
```

Y en `show.html.erb` agregar:
``` elixir
<%= saludo %>
```

Agregar comentarios en `show.html.erb`:
``` elixir
<h2>Comments</h2>
<ul>
  <%= for comment <- @post.comments do %>
    <li><%= comment.body %></li>
  <% end %>
</ul>
```

## API
Agregamos rutas para api:
``` elixir
scope "/api", YoWeb.Api do
  pipe_through :api
  resources "/posts", PostController, only: [:show, :index]
end
```

Agregamos nuevo controlador `controllers/api/post_controller.ex`:
``` elixir
defmodule YoWeb.Api.PostController do
  use YoWeb, :controller
  alias Yo.Blog

  def index(conn, _params) do
    posts = Blog.list_posts()
    render(conn, "index.json", posts: posts)
  end

  def show(conn, %{"id" => id}) do
    post = Blog.get_post!(id)
    render(conn, "show.json", post: post)
  end
end
```

Agregamos nueva vista `controllers/api/post_controller.ex`:
``` elixir
defmodule YoWeb.Api.PostView do
  use YoWeb, :view

  def render("index.json", %{posts: posts}) do
    render_many(posts, YoWeb.Api.PostView, "show.json")
  end

  def render("show.json", %{post: post}) do
    %{id: post.id, title: post.title, body: :body}
  end
end
```
