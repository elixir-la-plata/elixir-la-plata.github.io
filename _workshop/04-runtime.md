---
layout: workshop
title:  "Workshop 4: Runtime"
workshop_date:   2020-03-06
description: "Arboles de supervición, GenServers, Aplicaciones, Tolerancia a fallos, behaviours, protocols, formularios y debugging."
public: false
---

## Repaso procesos

Con las técnicas aprendidas creamos un contador simple
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
``` elixir
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
``` elixir
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
``` elixir
pid = Process.whereis(:counter)

Counter.get(:counter)
Counter.inc(:counter)
Counter.get(:counter)
```

Matamos al proceso, y comprobamos que el supervisor lo revivió en el estado original:
``` elixir
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
``` elixir
Counter.get(:counter)
Counter.inc(:counter)
```

Agregamos en nuestro contexto `blog.ex`:
``` elixir
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
``` elixir
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

---------

## Agregamos formulario de creacion de Comentarios.
- Agregar formulario
- Agregar ruta
- Agregar controlador con create.
- Mostrar como funciona.
- Explicar protocolos

## Si hay tiempo, creamos aplicación Nueva
```
mix new contador --sup
```

-----------

## Cosas que no dimos en el curso:

### Streams
Son enumerables Lazy que se pueden componer:
``` elixir
1..10000 |> Stream.map(&(&1 * &1)) |> Enum.take(10)
```

Útiles para trabajar con el concepto de infinito.
También útiles para trabajar con archivos grandes sin ocupar toda la memoria:
``` elixir
"./my_file.txt"
|> File.stream!
|> Stream.map(&String.strip/1)
|> Stream.with_index
|> Stream.map(fn {line, i} -> "#{i}: #{line}" end)
|> Enum.take(1)
|> IO.inspect()
```

## With
A veces no es siempre fácil escribir el código con Pipes.
Para estos casos se puede usar `with`:
```
with {:ok, data} <- Reader.read(socket),
     {:ok, command} <- Parser.parse(data),
     do: Server.run(command)
```
## Typespecs
Elixir es dinámico, así que no tiene chequeo de tipos en tiempo de compilación.
Pueden agregar especificaciones de tipo con la anotación `spec`:
```
@spec round(number) :: integer
def round(number) do
  :erlang.round(number)
end
```
Y una herramienta como Dialyzer va a usarlos para analizar el código.

## Doctests
Las anotaciones `@doc` se usan para documentar las funciones.
Si se le agregan ejemplos con esta notación se pueden correr en los tests:
```
@doc ~S"""
  Parses the given `line` into a command.

  ## Examples

      iex> KVServer.Command.parse("CREATE shopping\r\n")
      {:ok, {:create, "shopping"}}

"""
def parse(_line) do
  :not_implemented
end
```

## Intercompatibilidad con Erlang
Pueden usar librerías de Erlang desde Elixir.
Los módulos de Erlang se escriben como átomos:
```
:ets.new(:a_name, [])

:mnesia.create_schema([node()])

:observer.start

:queue.new

:rand.uniform()
```

## Canales, Presence y Phoenix Live View
Phoenix tiene un excelente soporte para websockets.

* Con Canales podemos crear chats en tiempo real.
* Con Presence podemos saber quien está conectado en tiempo real.
* Con Phoenix Live View podemos crear aplicaciones super ricas sin escribir una sóla linea de javascript.

## Nerves
Herramienta para construir sistemas embebidos (Raspberries, etc.).
