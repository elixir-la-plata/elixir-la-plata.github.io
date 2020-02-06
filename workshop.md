---
layout: page
title: Workshop Elixir
permalink: /workshop/
---
<img src="/assets/workshops-icon.png" class="imsg-responsive">

 {% for workshop in site.workshop %}
  <article class="workshop">

    <span class="post-meta">{{ workshop.workshop_date | date: "%b %-d, %Y" }}</span>

    <h2>
      <a href="{{ workshop.url }}">
        {{ workshop.title }}
      </a>
      <small>{{ workshop.description | markdownify }}</small>
    </h2>

  </article>
  {% endfor %}