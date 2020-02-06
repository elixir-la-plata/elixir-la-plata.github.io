---
layout: workshop
title:  "Workshop 2: Backend"
workshop_date:   2020-02-14
description: "Backend, Ecto, Repo, Query, Schema, Changeset, Relations."
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

* Corremos import Ecto.Query
* Corremos alias Yo.Post
* Corremos from p in “posts”, select: p.id
* Corremos from p in “posts”, select: %{id: p.id, title: p.title, body: p.body}
* Corremos Yo.Repo.all(Post)
* Corremos q = from p in “posts”, join: c in “comments” on: c.post_id == p.id, select: [p.id, c.id]
* Explicamos Schema
* Creamos Schema de Comentarios

```
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

* Hacemos queries con Schema

* Insertamos un Comment sin Post, y explicamos que eso está mal.

Yo.Repo.insert(%Yo.Blog.Comment{body: "Probando"})


* Explicamos changeset
* Hacemos update con changeset (agregamos alguna validación)
* Quizás algún constraint.
* Quizás agregamos indice a la base de datos.
* Agregamos metodos al contexto. Insert, Destroy, Show y agregar comments a get_post!
* Testing
