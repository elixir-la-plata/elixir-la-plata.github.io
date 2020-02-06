---
layout: workshop
title:  "Workshop 2: Backend"
workshop_date:   2020-02-14
description: "Backend, Ecto, Repo, Query, Schema, Changeset, Relations."
public: false
---

## Borrador
En proceso.

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

## Migration

* Corremos mix ecto, para ver tareas disponibles.
* Corremos  mix help ecto.gen.migration para ver documentación de migraciones
* Corremos  mix ecto.gen.migration add_comments
* Vemos archivo, agregamos código:

``` elixir
create table(:comments) do
  add :body, :string
  add :post_id, references("posts"), on_delete: :delete_all

  timestamps()
end
```

* Corremos mix ecto.migrate

## Schema

```
import Ecto.Query
alias Yo.Post
from p in "posts”, select: p.id
```

```
from p in "posts", select: %{id: p.id, title: p.title, body: p.body}
```

```
q = from p in “posts”, join: c in "comments", on: c.post_id == p.id, select: [p.id, c.id]
```

## Schema
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

## Creamos Schema de Comentarios
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

## Hacemos queries en iex
```
import Ecto.Query
alias Yo.Repo
alias Yo.Blog.Post
alias Yo.Blog.Comment

Repo.all(%Post{})

Repo.all(%Comment{})

query = from p in Post, join: c in Comment, on: c.post_id == p.id

Repo.all(query)

query = from [p, c] in query, select: {p.title, c.body}

Repo.all(query)

```

## .iex.exs
```
import Ecto.Query
alias Yo.Repo
alias Yo.Blog.Post
alias Yo.Blog.Comment
```

## Insertamos un Comment sin Post
```
Repo.insert(%Comment{body: "Probando"})
```
Explicamos que eso está mal.

## Changeset

En Comment
``` elixir
 def changeset(post, attrs) do
    post
    |> cast(attrs, [:post_id, :body])
    |> validate_required([:post_id, :body])
  end
```

Queremos que los posts tengan titulos unicos.
Create migration
```
mix ecto.gen.migration add_unique_index_to_post_title
```

Add unique index to Titulo de Posts
``` elixir
defmodule Yo.Repo.Migrations.AddUniqueIndexToPostTitle do
  use Ecto.Migration

  def change do
    create unique_index("posts", [:title])
  end
end
```

Go to iex and try to insert post twice
```
changeset = Post.changeset(%Post{},%{title: "a", body: "asa"})
Repo.insert(changeset)
Repo.insert(changeset)
```

Agregamos unique constraint
```
|> unique_constraint(:title)
```

Contexto, modificamos get_post! para que nos traiga los comentarios.
Preload.
```
  def get_post!(id), do: Repo.get!(Post, id) |> Repo.preload([:comments])
```

Si quiero que venga en una sola query tengo que ser explicito con los joins.
```
def get_post(id) do
  Repo.all(
    from p in Post,
      join: c in assoc(p, :comments),
      preload: [comments: c]
  )
end
  ```

* Testing
