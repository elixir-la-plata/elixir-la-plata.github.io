---
layout: workshop
title:  "Workshop 2: Backend"
workshop_date:   2020-02-14
description: "Backend, Ecto, Repo, Query, Schema, Changeset, Relations."
public: false
---

(Work in progress)

## Teoría
* Pipe Operator
* Strings, Charlist y Binarios
* Alias, Import, Use

## Elixir Flashcards
* Kernel II
* Enum I
* Enum II

## Ecto

* Ecto Modulos
* Ecto Repo
* Ecto Query (algunos ejemplos) -> Será muy difícil hacer una prueba en iex?

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

Entramos al archivo nuevo y agregamos `body` y `post_id`:
``` elixir
create table(:comments) do
  add :body, :string
  add :post_id, references("posts"), on_delete: :delete_all

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
Post
|> order_by([p], [p.title, p.body])
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

En `comment.ex` agregamos:
``` elixir
def changeset(post, attrs) do
  post
  |> cast(attrs, [:post_id, :body])
  |> validate_required([:post_id, :body])
end
```

Probamos insertar otro comentario sin post
``` elixir
Repo.insert(%Comment{body: "Otro comentario"})
# Ahora no me lo permite :)
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
```
changeset = Post.changeset(%Post{},%{title: "a", body: "asa"})
Repo.insert(changeset)
Repo.insert(changeset)
```

Ahora agregamos una restricción:
```
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
def get_post(id) do
  Repo.all(
    from p in Post,
      join: c in assoc(p, :comments),
      preload: [comments: c]
  )
end
```
