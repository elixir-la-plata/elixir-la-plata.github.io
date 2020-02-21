---
layout: workshop
title:  "Workshop 4: Librerias"
workshop_date:   2020-03-06
description: "Manejo de dependencias, Autenticación, terminando aplicación, releases."
public: false
---

## Borrador
En proceso.

## Teoría
* Aplicaciones y supervición.
* GenServer (Contador)

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
