---
layout: workshop
title:  "Workshop 3: Frontend"
workshop_date:   2020-02-21
description: "Frontend, Plug, Endpoint, Routes, Controller, Views, Templates, Layouts."
public: false
---

## Borrador
En proceso.

## Teoría
* Procesos, receive, send, PID, self()
* Tasks
* Estado y Agent

## Elixir Cards:
* Kernel III
* Enum III
* Fundamentals III

## Plug
* Plug
* Plug.Conn
* Cómo crear un plug (función y módulo)
* Cómo atravieza Plug toda la aplicación

## Rutas
* Pipelines
* Scope
* Resources

## Controladores y vistas.
* Comparar con Rails
* Explicar acción por acción

## Parte práctica:
* Creamos Post público (index y show) Controladores, Vistas y Templates.

## Si hay tiempo
* Webpack y Layout - Agregar CSS y poner linda la página.
* Creamos una API

-----

Procesos
Explicar procesos en general

Explicar spawn:
```
spawn(fn -> 1 + 2 end)
```

Explicar vida de un proceso:
pid = spawn(fn -> 1 + 2 end)
Process.alive?(pid)

Explicar self()
self()
Process.alive?(self())

Explicar send y cola de mensajes:
send self(), {:mensaje, "Hola mundo"}
send self(), {:otra_cosa, "Como va?"}

Process.info(self(), :messages)

Explicar receive (llamarlo 3 veces, la tercera va a bloquear):
receive do
  {:mensaje, msg} -> msg
  {:otra_cosa, _msg} -> "No me importa"
end

Explicar send y receive con ejemplo mas complejo:
send self(), {:mensaje, "Hola mundo"}
send self(), {:mensaje, "Como va?"}
send self(), {:otra_cosa, "Como va?"}
send self(), {:no_matchea, "Como va?"}
send self(), {:mensaje, "Aguante Elixir"}

Process.info(self(), :messages)

receive do
  {:mensaje, msg} -> msg
  {:otra_cosa, _msg} -> "No me importa"
end

Process.info(self(), :messages)

Explicar receive con timeout:
receive do
  {:mensaje, msg} -> msg
  {:otra_cosa, _msg} -> "No me importa"
after
  1_000 -> "Pasó 1 segundo"
end

Explicar spawn_link, EXIT:
self()
spawn_link fn -> raise "oops" end
self()


Explicar brevemente Task:
Task.start fn -> raise "oops" end


task = task.async(fn -> 1 + 1 end)
task = Task.await(task)

Explicar state + KV
…

Explicar Agent?
…


Plug
Explicar que es Plug
