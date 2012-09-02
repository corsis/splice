# Cross-platform socket-to-socket splicing

`splice` is a Haskell library that implements a cross platform API
to splice sockets together. On Linux, the `splice(2)` system call is
used which allows zero-copy transfer between file descriptors.

[Homepage][main page].

# Installation

It's just a `cabal install` away on [Hackage][]:

```bash
$ cabal install splice
```

# Join in

File bugs in the GitHub [issue tracker][].

Master [git repository][gh]:

* `git clone https://github.com/corsis/splice.git`

# Authors

See [AUTHORS.txt](https://raw.github.com/corsis/splice/master/AUTHORS.txt).

# License

BSD3. See `LICENSE.txt` for terms of copyright and redistribution.

[main page]: http://corsis.github.com/splice
[issue tracker]: http://github.com/corsis/splice/issues
[gh]: http://github.com/corsis/splice
[Hackage]: http://hackage.haskell.org/package/splice
