Prospect is a status reporter for laptops, written in Haskell.

The assumption is that you're using something bare-metal, like xmonad or a [compiz-only](https://askubuntu.com/questions/174806/standalone-of-compiz-and-google-chrome-with-borders) environment like me. If you know what [conky](http://conky.sourceforge.net/) and [dzen](https://github.com/robm/dzen/) does, the concept should be clear. Using dzen2, Prospect displays

- Power (battery) status
- Date and time
- Sound volume

It also displays warning when the battery is discharging and remaining power is lower than 50%.

## Why reinvent the wheel?

Yes, there are better alternatives. I just thought I'd write one for myself since I wouldn't need a full-featured status reporter when those three things were all I wanted.

## But this software does not look simple at all.

While writing I became too immersed into the joy of programming that I forgot the original purpose and went on to reckless coding. I deeply regret. This is software sin. Stone me.

## Usage

In the repo directory:

```sh
$ cabal configure && cabal build
$ ./dist/build/prospect/prospect
```

Now `./dist/build/prospect-touch/prospect-touch` will toggle the bar.

## Todo

- Avoid the `createProcess (shell "command")` evil.
- Respond to volume changing events. (Or at least have a "update" command interface.)
- `threadDelay` may take more than 60 seconds.
