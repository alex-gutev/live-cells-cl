# Live Cells CL

> [!CAUTION]
> This software is still in beta, and will likely experience rapid
> changes before the first official release.

Live-Cells-CL is a library that adds reactive programming to lisp. The
functionality of the library is ported from [Live
Cells](https://livecell.gutev.dev/) for Dart.

## Examples

> [!NOTE]
> This section contains examples demonstrating the main features of
> the library. Head to the
> [documentation](https://gutev.dev/live-cells-cl),
> for a short tutorial.

Cells (reactive containers for data) are defined using `DEFCELL`

```lisp
(defcell first-name "John")
(defcell last-name "Smith")
```

Cells can be defined using arbitrary expressions that reference other
cells:

```lisp
(defcell full-name
  (format nil "~a ~a" first-name last-name))
```
     
Cells are observed with `LIVE`:

```lisp
(live
  (format t "Hello ~a~%" full-name))
```

Changing the value of a cell, using `SETF`, results in the *live
block* being run:

For example changing the values of ``FIRST-NAME`` and ``LAST-NAME``:

```lisp
(setf first-name "Jane")
(setf last-name "Doe")
```

results in the following being printed:

```
Hello Jane Smith
Hello Jane Doe
```

If you'd like to learn more head over to the
[documentation](https://gutev.dev/live-cells-cl).
