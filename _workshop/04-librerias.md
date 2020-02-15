---
layout: workshop
title:  "Workshop 4: Librerias"
workshop_date:   2020-02-21
description: "Manejo de dependencias, Autenticación, terminando aplicación, releases."
public: false
---

## Borrador
En proceso.

## Teoría
* GenServer

## Elixir Flashcards
* GenServer

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


## Crear parte publica de posts.

