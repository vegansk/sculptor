Sculptor
========

Description
-----------

Sculptor is the tool to create types from the domain specific languages. For now it 
supports xml schemas as DSL.

The types written in DSL can be generated as scala case classes or typescript interfaces with
[io-ts](https://github.com/gcanti/io-ts) definitions. Scala types can be armed with json (circe)
and xml (kantan.xpath) codecs.

The scala and typescript generators itselfs are made as sbt plugins. For the usage, see sbt tests
in `sbt-scalagen` and `sbt-tsgen` projects.

Setup project
-------------

To setup git hooks, run:

```
rm -rf .git/hooks && \
ln -s ../bin/hooks .git/hooks
```
