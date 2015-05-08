Prospect is a status reporter for laptops, written in Haskell.

If you know what [dzen](https://github.com/robm/dzen/) does, the concept should be very clear. Prospect responds to certain events and output a string line to `stdout`.

## Why reinvent the wheel?

Yes, there are better alternatives such as [conky](http://conky.sourceforge.net/).

I used to use [xmonad](http://xmonad.org/) and eventually switched to [compiz](http://compiz.org/) for hardware rendering. So it was just bare compiz, and I was happy for a while, until recently I realized that there were things I manually look up too often that I really should have them just constantly displayed. Those were power status, time, and sound volume.

I wanted something as simple as possible, and the way dzen worked seemed promising. I thought I must be able to write a simple script myself that does exactly what I want without any unnecessary overhead.

### But this software doesn't look simple at all.

While writing this software, I became immersed into the joy of programming too much that I forgot the original purpose and went on to reckless coding. I deeply regret. Next time I'll use Python and ...

## Usage

In the repo directory:

```sh
$ cabal build
$ ./dist/build/prospect/prospect | dzen
```

## Todo

- Avoid the `createProcess (shell "command")` evil.
- Mitigate `ByteString` (strict) concatenation overhead.
