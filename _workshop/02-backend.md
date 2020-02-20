---
layout: workshop
title:  "Workshop 2: Backend"
workshop_date:   2020-02-14
description: "Backend, Ecto, Repo, Query, Schema, Changeset, Relations."
public: true
---

## Teoría
* Pipe Operator
* Strings, Charlist y Binarios
* Alias, Import, Use

### Pipeline Operator

#### Bash example

```bash
ls | grep "Do"
```

#### Elixir without pipeline

```elixir
string            = "hello, world!"
words             = String.split(string, " ")
capitalized_words = Enum.map(words, &String.capitalize/1)
Enum.join(capitalized_words, " ")
```

#### Elixir with pipeline

```elixir
"hello, world!"
  |> String.split(" ")
  |> Enum.map(&String.capitalize/1)
  |> Enum.join
```

### String

#### Strings are a sequence of bytes

```elixir
iex> string = <<104,101,108,108,111>>
"hello"
iex> "hello" <> <<0>>
<<104, 101, 108, 108, 111, 0>>
```


### Charlist

```elixir
iex> 'hełło'
[104, 101, 322, 322, 111]
iex> "hełło" <> <<0>>
<<104, 101, 197, 130, 197, 130, 111, 0>>
```

`322` is the Unicode codepoint for ł but it is
encoded in UTF-8 as the two bytes `197`, `130`.


### Code point

You can get a character’s code point by using `?`

```elixir
iex> ?Z
90
```

### Back to strings

```elixir
iex> string = "\u0061\u0301"
"á"
iex> String.codepoints string
["a", "́"]
iex> String.graphemes string
["á"]
```


### Basic functions

```elixir
iex> String.length "Hello"
5
iex> String.replace("Hello", "e", "a")
"Hallo"
iex> String.duplicate("Oh my ", 3)
"Oh my Oh my Oh my "
iex> String.split("Hello World", " ")
["Hello", "World"]
```

## Alias / require / import / use

Note: The special forms `alias`, `use`, `import`, and `require` are ways of accessing functions or macros outside of the current module. The forms `alias` and `import` are used to be able to refer to functions without having to use their fully qualified names. The form `use` is used to add functionality to the current module by running a macro from another module. When macros are used from an external module, `require` is needed to make the macros available to the compiler at compile time.


### Alias

Used to shorten the references to a specific module.

```elixir
# Fully qualified User struct
%Application.User{}

# alias is used to shorten the fully qualified name
alias Application.User, as: User

# After aliasing
%User{}

# alias without `:as` option will automatically use the last
# part of the module name after the last period
alias Application.User
# is the same as
alias Application.User, as: User
```


### Import

Imports specific functions into the current module so they can be called without using their module name.
```elixir
# Without import functions need to be called
# by their full name including the module
"ABC" = String.upcase("abc")

# Import a single function with the form
# import Module, only: [function_name: arity]
import String, only: [upcase: 1]

# upcase can now be used without the module name
"ABC" = upcase("abc")

# Imports all functions in the String module.
# It is recommend to use only option above to
# only import the functions you need.
import String
```


### Require

Makes a macro from an external module available to the compiler at compile time.

```elixir
defmodule Hello do
  # Example macro to add say_hello function to the module
  defmacro hello_macro do
    quote do
      def say_hello do
        IO.puts "Hello"
      end
    end
  end
end

```


```elixir
defmodule MyModule do
  # Without require here results in the following error:
  # (CompileError) iex:37: you must require Hello before invoking the macro Hello.hello_macro/0
  require Hello
  Hello.hello_macro
end
# Prints Hello
MyModule.say_hello
```


### Use

Adds functionality to the current module by calling another module’s `__using__`  macro.

```elixir
defmodule Hello do
  defmacro __using__(_opts) do
    quote do
      def say_hello do
        IO.puts "Hello"
      end
    end
  end
end

defmodule MyModule do
  use Hello
end
# prints "Hello"
MyModule.say_hello
```


### Multi alias/import/require/use

```elixir
alias MyApp.{Foo, Bar, Baz}
```


## Elixir Flashcards
* Kernel II
* Enum I
* Enum II

## Ecto

### Creamos migración para Comentarios

Vemos tareas disponibles de Ecto:
```
mix ecto
```

Vemos documentación de tarea de generación de migración:
```
mix help ecto.gen.migration
```

Creamos nueva migración
```
mix ecto.gen.migration add_comments
```

Entramos al archivo nuevo y agregamos `body` y `post_id` (Correjido):
``` elixir
create table(:comments) do
  add :body, :string
  add :post_id, references("posts", on_delete: :delete_all)

  timestamps()
end
```

Corremos migración:
```
mix ecto.migrate
```

### Corremos queries en iex

``` elixir
import Ecto.Query

query = from p in "posts", select: p.id
Repo.all(query)

query = from p in "posts", select: %{id: p.id, title: p.title, body: p.body}
Repo.all(query)

query = from p in "posts", join: c in "comments", on: c.post_id == p.id, select: [p.id, c.id]
Repo.all(query)
```

Ejemplo con Pipe: (los parentesis son necesarios)
```
(
"posts"
|> order_by([:title])
|> select([:title, :body])
|> Repo.all()
)
```

### Ecto.Schema

``` elixir
defmodule Yo.Blog.Post do
  use Ecto.Schema

  schema "posts" do
    field :body, :string
    field :title, :string

    timestamps()
  end
end
```

Corremos queries usando el schema `Post`:
``` elixir
import Ecto.Query
alias Yo.Blog.Post

Repo.all(Post)

query = from p in Post, where: p.id < 5
Repo.all(query)
```

### Creamos Schema de Comentarios
``` elixir
defmodule Yo.Blog.Comment do
  use Ecto.Schema
  alias Yo.Blog.Post

  schema "comments" do
    field :body, :string
    belongs_to :post, Post

    timestamps()
  end
end
```

Y en post
``` elixir
has_many :comments, Yo.Blog.Comment
```

La macro schema nos define un struct:
```
%Post{}
```

## Corremos queries en iex
``` elixir
import Ecto.Query
alias Yo.Repo
alias Yo.Blog.Post
alias Yo.Blog.Comment

Repo.all(Post)

Repo.all(Comment)

query = from p in Post, join: c in Comment, on: c.post_id == p.id
Repo.all(query)

query = from [p, c] in query, select: {p.title, c.body}
Repo.all(query)
```

Creamos archivo `.iex.exs`:
``` elixir
import Ecto.Query
alias Yo.Repo
alias Yo.Blog.Post
alias Yo.Blog.Comment
```

Insertamos comentario sin post
``` elixir
Repo.insert(%Comment{body: "Probando"})
# Ups! Necesitamos validaciones!
```

### Ecto.Changeset

Vamos a correr uno por vez:
``` elixir
import Ecto.Changeset

post = %Post{title: "titulo"}

# Sin nada
cast(post, %{}, [:title, :body])

# Con titulo
cast(post, %{title: "nuevo titulo"}, [:title, :body])

# Con titulo y body
changeset = cast(post, %{title: "nuevo titulo", body: "nuevo body"}, [:title, :body])

changeset.valid?

changeset.errors


# Con validación
(
  changeset = cast(post, %{}, [:title, :body])
  |> validate_required([:title, :body])
)

changeset.valid?

changeset.errors

IO.puts(inspect(cast(post, %{}, [:title, :body]), structs: false, pretty: true))
```

En `comment.ex` agregamos:
``` elixir
def changeset(comment, attrs) do
  comment
  |> cast(attrs, [:post_id, :body])
  |> validate_required([:post_id, :body])
end
```

Probamos insertar otro comentario sin post usando el changeset
``` elixir
changeset = Comment.changeset(%Comment{}, %{body: "Otro comentario"})

changeset.valid?

changeset.errors

Repo.insert(changeset)

# Ahora no me lo permite :)
```

En `post.ex` agregamos:
``` elixir
def changeset(post, attrs) do
  post
  |> cast(attrs, [:title, :body])
  |> validate_required([:title, :body])
end
```

### Queremos que los posts tengan títulos únicos.

Creamos una nueva migración:
```
mix ecto.gen.migration add_unique_index_to_post_title
```

Agregamos index único al titulo de Posts:
``` elixir
defmodule Yo.Repo.Migrations.AddUniqueIndexToPostTitle do
  use Ecto.Migration

  def change do
    create unique_index("posts", [:title])
  end
end
```

Vamos a iex e intentamos insertar el Post 2 veces
``` elixir
changeset = Post.changeset(%Post{},%{title: "a", body: "asa"})
Repo.insert(changeset)
Repo.insert(changeset)
```

Ahora agregamos una restricción:
``` elixir
|> unique_constraint(:title)
```

### Contexto

Modificamos `get_post!` para que nos traiga los comentarios.
``` elixir
  def get_post!(id) do
    Repo.get!(Post, id) |> Repo.preload([:comments])
  end
```

Si quiero que venga en una sola query tengo que ser explícito con los joins.
``` elixir
def get_post!(id) do
  Repo.one!(
    from p in Post,
      where: p.id == ^id,
      left_join: c in assoc(p, :comments),
      preload: [comments: c]
  )
end
```

### Corremos tests

```
mix test
```

Modificamos test:
``` elixir
test "get_post!/1 returns the post with given id" do
  post = post_fixture()
  loaded_post = Blog.get_post!(post.id)

  assert post.title == loaded_post.title
  assert post.body == loaded_post.body
  assert loaded_post.body == []
end
```
