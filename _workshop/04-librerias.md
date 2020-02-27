---
layout: workshop
title:  "Workshop 4: Librerias"
workshop_date:   2020-03-06
description: "Manejo de dependencias, Autenticación, terminando aplicación, releases."
public: false
---

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

## Supervisión.
``` elixir
children = []
Supervisor.start_link(children, strategy: :one_for_one)
```

## Creamos aplicación Nueva
```
mix new contador --sup
```

## Creamos GenServer de Contador
``` elixir
defmodule Yo.Blog.Counter do
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
{:ok, pid} = GenServer.start_link(Yo.Blog.Counter, 0)

GenServer.call(pid, :get)
GenServer.cast(pid, :inc)
GenServer.call(pid, :get)
```

Pero la idea es abstraernos de que es un GenServer.
Implementamos un lado Cliente y un lado Servidor:
``` elixir
defmodule Yo.Blog.Counter do
  use GenServer

  # Cliente

  def start_link(_) do
    GenServer.start_link(__MODULE__, 0)
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
alias Yo.Blog.Counter

{:ok, pid} = Counter.start_link(0)

Counter.get(pid)
Counter.inc(pid)
Counter.get(pid)
```

Si no queremos usar PID podemos darle un nombre:
```
  def start_link(_) do
    GenServer.start_link(__MODULE__, 0, name: __MODULE__)
  end

  def get() do
    GenServer.call(__MODULE__, :get)
  end

  def inc() do
    GenServer.cast(__MODULE__, :inc)
  end
```

Y lo llamamos:
```
Counter.start_link(0)
Counter.get()
Counter.inc()
Counter.get()
```

Mencionar similitudes y diferencias con objetos.

Lo podemos meter en el árbol de supervisión:
```
children = [
  Yo.Repo,
  YoWeb.Endpoint,
  Yo.Blog.Counter
]
```

## Elixir Flashcards
* GenServer y otras.

## Autenticación
Agregamos dependencia
Hacemos modelo Usuario.
Pensar más.


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

## Si hay tiempo, que no creo, agregar autenticación:
- Github?
- Debemos crear un modelo nuevo? O hacemos algo más simple?
