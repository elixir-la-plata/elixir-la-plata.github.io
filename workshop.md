---
layout: page
title: Workshop
permalink: /workshop/
---
<img src="/assets/workshops-icon.png" class="imsg-responsive">

 {% for workshop in site.workshop %}
  <article style="margin-bottom:2rem;" class="workshop">

    <span class="post-meta">{{ workshop.workshop_date | date: "%b %-d, %Y" }}</span>

    <h2>
      {% if workshop.public %}
        <a href="{{ workshop.url }}">
          {{ workshop.title }}
        </a>
      {% else %}
        {{ workshop.title }} <span style="font-size:50%; color: #666;">(pronto)</span>
      {% endif %}
      <small>{{ workshop.description | markdownify }}</small>
    </h2>

  </article>
  {% endfor %}