============
jmespath.clj
============

JMESPath (pronounced "jaymz path") allows you to declaratively specify how to
extract elements from a JSON document. *jmespath.clj* allows you to use
JMESPath in Clojure applications with PHP Clojure data structures.

Installation
------------

Add the following to your project's ``project.clj`` file.

.. code-block:: clojure

    [jmespath "0.1.0"]

Usage
-----

.. code-block:: clojure

    (ns hello-world
      (:require jmespath.core :as jmespath))

    (jmespath.parse "foo.bar.*.baz")

The official `JMESPath documentation <http://jmespath.readthedocs.org/en/latest/>`_
includes a formal grammar, specification, as well as links to other JMESPath
implementations.
