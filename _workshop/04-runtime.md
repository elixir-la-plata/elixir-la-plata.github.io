---
layout: workshop
title:  "Workshop 4: Runtime"
workshop_date:   2020-03-06
description: "Arboles de supervición, GenServers, Aplicaciones, Tolerancia a fallos, behaviours, protocols, formularios y debugging."
public: false
---

## Con las técnicas aprendidas creamos un contador simple
``` elixir
defmodule Counter do
  def start_link do
    Task.start_link(fn -> loop(0) end)
  end

  defp loop(state) do
    receive do
      {:get, caller} ->
        send caller, state
        loop(state)

      :inc ->
        new_state = state + 1
        loop(new_state)
    end
  end
end
```

Y lo puedo llamar:
```
{:ok, pid} = Counter.start_link()

send(pid, {:get, self()})
value = receive do: (x -> x)

send(pid, :inc)

send(pid, {:get, self()})
value = receive do: (x -> x)
```

## Implementamos como Genserver
``` elixir
defmodule Counter do
  use GenServer

  def init(value) do
    {:ok, value}
  end

  def handle_call(:get, _, state) do
    {:reply, state, state}
  end

  def handle_cast(:inc, state) do
    {:noreply, state + 1}
  end
end
```

Y lo llamamos:
``` elixir
# Start the server
{:ok, pid} = GenServer.start_link(Counter, 0)

GenServer.call(pid, :get)
GenServer.cast(pid, :inc)
GenServer.call(pid, :get)
```

## Cliente y Servidor

Pero la idea es abstraernos de que es un GenServer.
Implementamos un lado Cliente y un lado Servidor:
``` elixir
defmodule Counter do
  use GenServer

  # Cliente

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, 0, opts)
  end

  def get(pid) do
    GenServer.call(pid, :get)
  end

  def inc(pid) do
    GenServer.cast(pid, :inc)
  end

  # Servidor

  def init(value) do
    {:ok, value}
  end

  def handle_call(:get, _, state) do
    {:reply, state, state}
  end

  def handle_cast(:inc, state) do
    {:noreply, state + 1}
  end
end
```

Y lo llamamos:
```
{:ok, pid} = Counter.start_link()

Counter.get(pid)
Counter.inc(pid)
Counter.get(pid)
```

Los procesos pueden ser nombrados:
``` elixir
GenServer.start_link(Counter, 0, name: :contador)

Process.whereis(:contador)
```

Si no queremos usar PID podemos darle un nombre local:
``` elixir
Counter.start_link(name: :counter)
Counter.get(:counter)
Counter.inc(:counter)
Counter.get(:counter)
```

## Repaso procesos
Los procesos están aislados entre sí:
``` elixir
spawn(fn ->
  spawn(fn ->
    Process.sleep(1000)
    IO.puts("Internal process finished!")
  end)
  raise("Something went wrong!")
end)
```

Pero también se pueden linkear:
``` elixir
spawn(fn ->
  spawn_link(fn ->
    Process.sleep(1000)
    IO.puts("Internal process finished!")
  end)
  raise("Something went wrong!")
end)
```
Al linkearlos, si uno falla, manda una señal de salida (EXIT) al otro proceso.
Todos los procesos mueren.

Se pueden atrapar las señales se salida (trapping exit):
``` elixir
spawn(fn ->
  Process.flag(:trap_exit, true)

  spawn_link(fn -> raise("Hubo un error") end)

  receive do
    msg -> IO.inspect(msg)
  end
end)
```

## Supervisión
Antes de ver supervición, vamos a agregar nuestro contador al código de nuestra aplicación Phoenix.

Agregamos el código al archivo `lib/blog/counter.ex`.
Y nombramos al módulo `Yo.Blog.Counter`.
Y agregamos al archivo `.iex.exs` el `alias Yo.Blog.Counter`.

Abrimos `iex -S mix1` y corremos:
``` elixir
children = [{Counter, [name: :counter]}]
Supervisor.start_link(children, strategy: :one_for_one)
```

Y luego comprobamos que el proceso Contador está corriendo:
```
pid = Process.whereis(:counter)

Counter.get(:counter)
Counter.inc(:counter)
Counter.get(:counter)
```

Matamos al proceso, y comprobamos que el supervisor lo revivió en el estado original:
```
Process.exit(pid, :kill)

pid = Process.whereis(:counter)

Counter.get(:counter)
```

Corremos un supervisor con dos contadores de hijo:
``` elixir
children = [
  Supervisor.child_spec({Counter, [name: :counter1]}, id: :counter1),
  Supervisor.child_spec({Counter, [name: :counter2]}, id: :counter2)
]

Supervisor.start_link(children, strategy: :one_for_one)
```

Y comprobamos que los podemos llamar y que al matar uno, el otro sigue vivo:
``` elixir
pid1 = Process.whereis(:counter1)
pid2 = Process.whereis(:counter2)
Process.exit(pid1, :kill)
Process.whereis(:counter1)
Process.whereis(:counter2)
```

Tarea para casa:
Hacer lo mismo pero con estrategias `:one_for_all` y `:rest_for_one`.

Vamos al archivo `application.ex`:
Y agregamos nuestro Contador en el árbol de supervisión:
``` elixir
children = [
  Yo.Repo,
  YoWeb.Endpoint,
  {Yo.Blog.Counter, name: :counter}
]
```

Corremos `iex -S mix` y lo probamos:
```
Counter.get(:counter)
Counter.inc(:counter)
```

Agregamos en nuestro contexto `blog.ex`:
```
  alias Yo.Blog.Counter

  def get_counter() do
    Counter.get(:counter)
  end

  def increment_counter() do
    Counter.inc(:counter)
  end
```

Creamos un nuevo Plug para contar en `yo_web/plugs/count.ex`:
``` elixir
defmodule YoWeb.Plugs.Count do
  import Plug.Conn
  alias Yo.Blog

  def init(default), do: default

  def call(conn, options) do
    Blog.increment_counter()
    new_count = Blog.get_counter()

    Plug.Conn.assign(conn, :counter, new_count)
  end
end
```

Agregamos el plug en el pipeline `browser`, en `router.ex`:
```
pipeline :browser do
  # ...
  plug YoWeb.Plugs.Count
end
```

Y finalmente dentro del header, en `templates/layouts/app.html.eex` agregamos:
```
Views: <%= @conn.assigns.counter %>
```

Visitamos la página con `mix phx.server` y vemos los resultados.




## Mover todo a Admin

1. Creamos scope de admin y movemos rutas de post:
```
scope "/admin", YoWeb.Admin, as: :admin do
  pipe_through :browser
  resources "/posts", PostController
end
```

2. Movemos `post_controller.ex` a `admin/post_controller.ex` y le cambiamos el nombre a `YoWeb.Admin.PostController`.

3. Movemos `post_view.ex` a `admin/post_view.ex` y le cambiamos el nombre a `YoWeb.Admin.PostView`.

4. Movemos carpeta `templates/post` a `templates/admin/post`

5. Reemplazamos todas las instancias de `Router.post_path` por `Router.admin_post_path`.

6. Nuevo Layout para Admin (otro color de fondo, o que diga admin, no sé.)


## Crear parte pública de posts.
1. Agregamos ruta pública para posts (index y show)
2. Agregamos controller publico con index y show
3. Agregamos vista publica
4. Agregamos template de index
5. Agregamos template de show

## Agregar soporte de markdown
1. Agregar dependencia y compilar.
2. Modificar `show.html.eex` para mostrar markdown
3. Bajamos seeds de url: (crear gist)
4. Lo corremos

## Agregamos formulario de creacion de Comentarios.
- Agregar formulario
- Agregar ruta
- Agregar controlador con create.
- Mostrar como funciona.

## Agregar contador de vistas publicas
- Agregar contador que creamos hoy.
- Agregarlo para que arranque con la aplicación.
- Agregar plug para contar visitas.
- Agregar plug a public post controller.
- Mostrar cantidad de visitas en app layout.

## Si hay tiempo, creamos aplicación Nueva
```
mix new contador --sup
```
