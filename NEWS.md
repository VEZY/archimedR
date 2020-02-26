# archimedR 0.3.0

* Add the java path as an optional argument to `run_archimed()` for users using an old version of Java, or the Open JDK (not compatible with ARCHIMED yet);

* Improve `read_ops()`: now more resilient to the variety of OPS inputs + chaining is optional + computation of plor area from boundaries + explicit error if boundaries are missing (not mandatory for AMAPStudio but it is for ARCHIMED);

* Add `write_ops()`

# archimedR 0.2.0

* Add args to `run_archimed()` to add the possibility to give arguments to the ARCHIMED app.

# archimedR 0.1.0

* Added a `NEWS.md` file to track changes to the package.
