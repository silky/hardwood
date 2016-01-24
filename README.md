# hardwood
mud game client made in haskell

Most modern MUD clients provide scripting through Lua and pattern matching through regular expression matching. Having recently begun working with the [attoparsec](https://hackage.haskell.org/package/attoparsec) library, I thought it might be fun to create a MUD client that uses parser combinators instead of regular expressions. Additionally, this project will serve as a learning opportunity in working with the [conduit](https://hackage.haskell.org/package/conduit) package.

## current features
* parses Telnet and ANSI messages

## planned features

* support for Iron Realms' Generic Mud Client Protocol (GMCP)
* building triggers using parser combinators as opposed to regular expressions
* GTK-based user interface
* colored text output
* automatic logging

## building

    $ git clone https://github.com/narrative/hardwood
    $ stack build
