---
layout: home

hero:
  name: "Proteus"
  text: "Protobuf library for Scala"
  tagline: Protobuf goes Scala-first
  image:
    src: /proteus.svg
    alt: Proteus
  actions:
    - theme: brand
      text: Getting Started
      link: /getting-started
    - theme: alt
      text: GitHub
      link: https://github.com/ghostdogpr/proteus
    - theme: alt
      text: Watch the talk
      link: https://youtu.be/zTq9gdImhpk

features:
  - title: Code-First
    details: Generate Protobuf codecs directly from your Scala code. No code generation or sbt plugin required.
  - title: Schema Generation
    details: Generate .proto files to document your schema and efficiently track changes. Various customizations are available.
  - title: Declarative gRPC Services
    details: Define gRPC services in Scala. Generate clients and servers from them using direct style, Future, ZIO, fs2, Ox or Kyo backends.
  - title: Schema Diffing
    details: Detect breaking changes between .proto schemas with the proteus-diff CLI. Standalone native binary, no JVM required.
    link: /proteus-diff
    linkText: Learn more
---

<div style="max-width: 960px; margin: 4rem auto 0; padding: 0 24px;">
  <h2 style="text-align: center; font-size: 1.75rem; margin-bottom: 1.5rem;">Watch the talk</h2>
  <div style="position: relative; padding-bottom: 56.25%; height: 0; overflow: hidden; border-radius: 12px;">
    <iframe
      style="position: absolute; top: 0; left: 0; width: 100%; height: 100%; border: 0;"
      src="https://www.youtube-nocookie.com/embed/zTq9gdImhpk"
      title="Proteus: Protobuf goes Scala-first"
      allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share"
      allowfullscreen
    ></iframe>
  </div>
</div>

