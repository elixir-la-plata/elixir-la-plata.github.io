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

### 1. Agregar la nueva ruta:

`POST /post/:id/comments`

```elixir
    resources "/posts", PostController do
      resources "/comments", CommentController, only: [:create]
    end
```


### 2. Agregamos el formulario

```html
# lib/yo_web/templates/comment/form.html.eex
<%= form_for @comment_changeset, @action, fn f -> %>
  <%= if @comment_changeset.action do %>
    <div class="alert alert-danger">
      <p>Oops, something went wrong! Please check the errors below.</p>
    </div>
  <% end %>

  <%= label f, :body %>
  <%= text_input f, :body %>
  <%= error_tag f, :body %>

  <%= hidden_input f, :post_id, value: @post.id %>

  <div>
    <%= submit "Save" %>
  </div>
<% end %>
```


### 3. Modificar el template del post `show.html.eex` con el formulario nuevo y el
listado

```html
<hr>
<%= for comment <- @post.comments do %>
  <p>
    <div class=alert-info><%= comment.body %></div>
  </p>
<% end %>

<h3> New comment <h3>
<%= render YoWeb.CommentView, "form.html",
  Map.put(assigns, :action, Routes.post_comment_path(@conn, :create, 1)) %>

```


### 4. Agregar el controlador `CommentController` con el método `create`

```elixir
defmodule YoWeb.CommentController do
  use YoWeb, :controller

  alias Yo.Blog
  alias Yo.Blog.Post

  def create(conn, %{"comment" => comment_params, "post_id" => post_id}) do
    post = Blog.get_post!(post_id)

    case Blog.create_comment(comment_params) do
      {:ok, comment} ->
        conn
        |> put_flash(:info, "Comment created successfully.")
        |> redirect(to: Routes.post_path(conn, :show, post_id))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, YoWeb.PostView, "show.html", post: post, comment_changeset: changeset)
    end
  end
end
```


### 5. Implementamos los métodos necesarios en nuestra contexto

```elixir
 @doc """
 Returns an `%Ecto.Changeset{}` for traking comment changes.

 ## Examples

 iex> change_comment(comment)
 %Ecto.Changeset{source: %Comment{}}
 """
 def change_comment(%Comment{} = comment) do
   Comment.changeset(comment, %{})
 end
 ```


 ```elixir
 @doc """
 Creates a comment belongs_to a post.

 ## Examples

     iex> create_comment(%{field: value})
     {:ok, %Post{}}

     iex> create_comment(%{field: bad_value})
     {:error, %Ecto.Changeset{}}

 """
 def create_comment(attrs \\ %{}) do
   %Comment{}
   |> Comment.changeset(attrs)
   |> Repo.insert()
 end
```


### 6. Creamos el Modulo `CommentView` para usar el render

```elixir
defmodule YoWeb.CommentView do
  use YoWeb, :view
end
```

## Protocols

- Medio para lograr polimorfismo en Elixir
- Los protocolos son especificamente para cuando quiere cambiar el
comportamiento dependiendo del tipo de un dato


### Ejemplo `String.Chars`

```elixir
iex> to_string(5)
"5"
iex> to_string(12.4)
"12.4"
iex> to_string("foo")
"foo"
```


### Para una tupla?

```elixir
to_string({:foo})
** (Protocol.UndefinedError) protocol String.Chars not implemented for {:foo}
    (elixir) lib/string/chars.ex:3: String.Chars.impl_for!/1
    (elixir) lib/string/chars.ex:17: String.Chars.to_string/1
```


### Implementemos para tupla


```elixir
defimpl String.Chars, for: Tuple do
  def to_string(tuple) do
    interior =
      tuple
      |> Tuple.to_list()
      |> Enum.map(&Kernel.to_string/1)
      |> Enum.join(", ")

    "{#{interior}}"
  end
end
```

```elixir
iex> to_string({3.14, "apple", :pie})
"{3.14, apple, pie}"
```


### Implementemos un Protocolo

```elixir
defprotocol AsAtom do
  def to_atom(data)
end

defimpl AsAtom, for: Atom do
  def to_atom(atom), do: atom
end

defimpl AsAtom, for: BitString do
  defdelegate to_atom(string), to: String
end

defimpl AsAtom, for: List do
  defdelegate to_atom(list), to: List
end

defimpl AsAtom, for: Map do
  def to_atom(map), do: List.first(Map.keys(map))
end
```


```elixir
iex> import AsAtom
AsAtom
iex> to_atom("string")
:string
iex> to_atom(:an_atom)
:an_atom
iex> to_atom([1, 2])
:"\x01\x02"
iex> to_atom(%{foo: "bar"})
:foo
```

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

## Programación distribuída
Es uno de los puntos fuertes de Erlang y Elixir.
Son problemas complejos. Elixir provee las abstracciones adecuadas.

## Palabras finales
La web está evolucionando. La web moderna es altamente conectada y necesita sistemas en tiempo real.
Necesita incluir dispositivos conectados.
Necesita sistemas de alta disponibilidad, escalables, tolerantes a fallos y distribuidos. Necesita sistemas que puedan aprovechar al máximo la capacidad de procesamiento de las computadoras.

Erlang, Elixir y Phoenix fueron creados para resolver este tipo de problemas.
