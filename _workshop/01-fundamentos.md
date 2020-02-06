---
layout: workshop
title:  "Workshop 1: Fundamentos"
workshop_date:   2020-02-07
description: "Introducción, Set Up, Creación de Aplicación, Deploy y Generadores."
public: true
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

## Basic Data Types
```elixir
iex> 1          # integer
iex> 1.0        # float
iex> true       # boolean
iex> :atom      # atom / symbol
iex> "elixir"   # string
```

Tuple
-----

```elixir
iex> tuple = {1, :two, "three"}  
iex> elem(tuple, 0)
1
iex> elem(tuple, 2) 
"three"
iex> put_elem(tuple, 0, 'uno')
{'uno', :two, "three"}
iex> tuple_size(tuple)
3
```


Tuple module 
------------

```ruby
Tuple.append(tuple, value)
# Inserts an element at the end of a tuple.
Tuple.delete_at(tuple, index)
# Removes an element from a tuple.
Tuple.duplicate(data, size)
# Creates a new tuple.
Tuple.insert_at(tuple, index, value)
# Inserts an element into a tuple.
Tuple.to_list(tuple)
# Converts a tuple to a list.
```

Doc: [Tuple](https://hexdocs.pm/elixir/Tuple.html)


List
----

```elixir
iex> [1, "two", 3, :four]
[1, "two", 3, :four]
iex> [1, 2, 3] ++ [4, 5, 6]
[1, 2, 3, 4, 5, 6]
iex> [1, true, 2, false, 3, true] -- [true, false]
[1, 2, 3, true]
iex> [1, "two", :three, 4] ++ ["more numbers"]
[1, "two", :three, 4, "more numbers"]
iex> hd([1, 2, 3])
1
iex> tl([1, 2, 3])
[2,3]
iex> 2 in [1,2,3]
true
```


```elixir
iex> [0 | [1, "two", 3, :four]]
[0, 1, "two", 3, :four]
iex> [1 | [2 | [3 | []]]]
[1, 2, 3]

iex> list = [1, 2, 3]
iex> [0 | list] # fast
[0, 1, 2, 3]
iex> list ++ [4] # slow
[1, 2, 3, 4]
```


List module
-----------

```elixir
List.delete(list, element)
# Deletes the given element from the list.
# Returns a new list without the element.
List.delete_at(list, index)
# Produces a new list by removing the value at the specified index.
List.first(list)
# Returns the first element in list or nil if list is empty.
List.flatten(list)
# Flattens the given list of nested lists.
```

Doc: [`List`](https://hexdocs.pm/elixir/List.html)


Keyword
-------

```elixir
iex> [{:first_number, 1}, {:second_number, 2}]
[first_number: 1, second_number: 2]

iex> [first_number: 1, second_number: 2]
[first_number: 1, second_number: 2]
```


Keyword module
--------------

```elixir
Keyword.fetch(keywords, key)
# Fetches the value for a specific key and returns it in a tuple.
Keyword.fetch!(keywords, key)
# Fetches the value for specific key.
Keyword.keys(keywords)
# Returns all keys from the keyword list.
Keyword.values(keywords)
# Returns all values from the keyword list.
Keyword.split(keywords, keys)
# Takes all entries corresponding to the given keys and
# extracts them into a separate keyword list.
Keyword.take(keywords, keys)
# Takes all entries corresponding to the given keys and
# returns them in a new keyword list.
```

Doc: [`Keyword`](https://hexdocs.pm/elixir/Keyword.html)


Map
---

```elixir
iex> %{"one" => :two, 3 => "four"}
%{3 => "four", "one" => :two}
iex> map = %{a: 1, b: 2}
iex> map[:b]
2
iex> map["non_existing_key"]
nil
iex> map.a
1
iex> map.non_existing_key
** (KeyError) key :non_existing_key not found in: %{a: 1, b: 2}
```


Map module
----------

```elixir
Map.drop(map, keys)
# Drops the given keys from map.
Map.equal?(map1, map2)
# Checks if two maps are equal.
Map.from_struct(struct)
# Converts a struct to map.
Map.fet(map, key, default \\ nil)
# Gets the value for a specific key in map.
Map.has_key?(map, key)
# Returns whether the given key exists in the given map.
```

Doc: [`Map`](https://hexdocs.pm/elixir/Map.html)


MapSet module
-------------

```elixir
iex> my_set = MapSet.new([1, 2, 3, 4, 5])
iex> MapSet.put(my_set, 6)
MapSet<[1, 2, 3, 4, 5, 6]>
iex> MapSet.delete(my_set, 6)
MapSet<[1, 2, 3, 4, 5]>
```

```elixir
MapSet.difference(map_set1, map_set2)
# Returns a set that is map_set1 without the members of map_set2.
MapSet.disjoint?(map_set1, map_set2)
# Checks if map_set1 and map_set2 have no members in common.
MapSet.intersection(map_set, map_set)
# Returns a set containing only members that map_set1 and 
# map_set2 have in common.
MapSet.subset?(map_set1, map_set2)
# Checks if map_set1's members are all contained in map_set2.
MapSet.union(map_set1, map_set2)
# Returns a set containing all members of map_set1 and map_set2.
```

Doc: [`MapSet`](https://hexdocs.pm/elixir/MapSet.html)


Struct
------

```elixir
iex> defmodule User do
...>   defstruct name: "John", age: 27
...> end

iex> %User{}
%User{age: 27, name: "John"}
iex> %User{name: "Jane"}
%User{age: 27, name: "Jane"}

iex> %User{oops: :field}
** (KeyError) key :oops not found in: %User{age: 27, name: "John"}
```


Enumerable and Enum
-------------------

The protocol [`Enumerable`](https://hexdocs.pm/elixir/Enumerable.html) is
implemented by `Range`, `List` and `Map`. So many function to work with these
are found in the [`Enum`](https://hexdocs.pm/elixir/Enum.html) module.

Pattern Matching
================


Match Operator
--------------

Símbolo "="

```elixir 
iex(1)> x = 2
2
iex(2)> 2 = x
2
iex(3)> 1 = x
** (MatchError) no match of right hand side value: 2
    (stdlib) erl_eval.erl:453: :erl_eval.expr/5
    (iex) lib/iex/evaluator.ex:257: IEx.Evaluator.handle_eval/5
    (iex) lib/iex/evaluator.ex:237: IEx.Evaluator.do_eval/3
    (iex) lib/iex/evaluator.ex:215: IEx.Evaluator.eval/3
    (iex) lib/iex/evaluator.ex:103: IEx.Evaluator.loop/1
    (iex) lib/iex/evaluator.ex:27: IEx.Evaluator.init/4
```


With tuples
-----------

```elixir
iex> {a, b, c} = {:hello, "world", 42}
{:hello, "world", 42}
iex> a
:hello
iex> b
"world"

iex> {:ok, result} = {:ok, 13}
{:ok, 13}
iex> result
13
```


With Lists
----------

```elixir
iex(1)> [head | tail] = [1,2,3,4]
[1, 2, 3, 4]
iex(2)> head
1
iex(3)> tail
[2, 3, 4]
```


With Structs
------------

```elixir
iex> %User{name: "Fede", age: age} = %User{name: "Fede", age: 30}
%User{age: 30, name: "Fede"}

iex> %{name: "Fede", age: age} = %User{name: "Fede", age: 30}
%User{age: 30, name: "Fede"}

iex> %User{name: "Fede", age: age} = %{name: "Fede", age: 30}
** (MatchError) no match of right hand side value: %{age: 30, name: "Fede"}
    (stdlib) erl_eval.erl:453: :erl_eval.expr/5
    (iex) lib/iex/evaluator.ex:257: IEx.Evaluator.handle_eval/5
    (iex) lib/iex/evaluator.ex:237: IEx.Evaluator.do_eval/3
    (iex) lib/iex/evaluator.ex:215: IEx.Evaluator.eval/3
    (iex) lib/iex/evaluator.ex:103: IEx.Evaluator.loop/1
    (iex) lib/iex/evaluator.ex:27: IEx.Evaluator.init/4
```


Pin Operator
------------

```elixir
iex> x = 1
1
iex> ^x = 2
** (MatchError) no match of right hand side value: 2
iex> {y, ^x} = {2, 1}
{2, 1}
iex> y
2
iex> {y, ^x} = {2, 2}
** (MatchError) no match of right hand side value: {2, 2}
```


Underscore
----------

```elixir
iex> [head | _] = [1, 2, 3]
[1, 2, 3]
iex> head
1

iex> _
** (CompileError) iex:1: invalid use of _. "_" represents a
  value to be ignored in a pattern and cannot be used in expressions
```


Real Example
------------

```elixir
response = HTTP.patch(url, body, headers, [])
case response do
  {:ok, %{status_code: 200, body: body}} ->
    body
  {:ok, %{status_code: 404} = resp} ->
    "Error: #{resp.status_code} Not found!"
  {:error, %{reason: reason}} ->
    "Error: #{reason}"
end
```

### If
``` elixir
if x > 0 do
  "Positivo"
else
  "No positivo"
end
```

### Cond
``` elixir
cond do
  x == 0 -> "Cero"
  x > 0  -> "Positivo"
  true   -> "Negativo"
end
```

### Case
``` elixir
case workday do
  0 -> "Lunes"
  1 -> "Martes"
  2 -> "Miércoles"
  3 -> "Jueves"
  4 -> "Viernes"
  _ -> "#{workday} es inválido"
end
```

### Modulos y funciones
``` elixir
defmodule Math do
  def sum(a, b) do
    a + b
  end
end
```

### Multiples definiciones
``` elixir
defmodule Math do
  def zero?(0) do
    true
  end
  
  def zero?(x) do
    false
  end
end
```

### Con guardas
``` elixir
defmodule Math do
  def zero?(0) do
    true
  end
  
  def zero?(x) when is_integer(x) do
    false
  end
end
```

### Funciones anonimas
``` elixir
inc = fn (a) -> a + 1 end
inc.(2) #=> 3
Enum.map([1,2,3], inc)
#=> [2,3,4]
```

### Operador de captura
``` elixir
defmodule Math do
  def inc(x), do: x + 1
end

Enum.map([1,2,3], &Math.inc/1)
#=> [2,3,4]
```

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