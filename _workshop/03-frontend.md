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

## Procesos

Crear un proceso:
``` elixir
spawn(fn -> 1 + 2 end)
```

Vida de un proceso:
``` elixir
pid = spawn(fn -> 1 + 2 end)
Process.alive?(pid)

self()
Process.alive?(self())
```

Cola de mensajes:
``` elixir
send self(), {:mensaje, "Hola mundo"}
send self(), {:otra_cosa, "Como va?"}

Process.info(self(), :messages)
```

Llamamos receive 3 veces (la tercera va a bloquear):
``` elixir
receive do
  {:mensaje, msg} -> msg
  {:otra_cosa, _msg} -> "No me importa"
end
```

Ejemplo más complejo:
``` elixir
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
```

Podemos usar timeout:
``` elixir
receive do
  {:mensaje, msg} -> msg
  {:otra_cosa, _msg} -> "No me importa"
after
  1_000 -> "Pasó 1 segundo"
end
```

Linkear procesos y EXIT:
``` elixir
self()
spawn_link fn -> raise "oops" end
self()
```

### Tareas:
``` elixir
Task.start fn -> raise "oops" end
```

Async y await:
``` elixir
task = task.async(fn -> 1 + 1 end)
task = Task.await(task)
```

Estado con procesos:
``` elixir
defmodule KV do
  def start_link do
    Task.start_link(fn -> loop(%{}) end)
  end

  defp loop(map) do
    receive do
      {:get, key, caller} ->
        send caller, Map.get(map, key)
        loop(map)
      {:put, key, value} ->
        loop(Map.put(map, key, value))
    end
  end
end
```

Podemos llamar al proceso:
``` elixir
{:ok, pid} = KV.start_link()
send(pid, {:put, :hello, :world})
send(pid, {:get, :hello, self()})
flush()
```

### Agentes
``` elixir
{:ok, pid} = Agent.start_link(fn -> %{} end)
Agent.update(pid, fn map -> Map.put(map, :hello, :world) end)
Agent.get(pid, fn map -> Map.get(map, :hello) end)
```

## Plug
```

```
