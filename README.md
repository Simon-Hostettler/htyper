# htyper

htyper is a typing test for terminals written in Haskell using Brick.

![recording](resources/htypervid.m4v)

# Installation

First make sure you have stack installed. You can install it [here](https://www.haskell.org/ghcup/).
Then clone the repo:

```
$ git clone https://github.com/Simon-Hostettler/htyper.git
```

And install htyper:

```
$ cd htyper
$ stack install
```

This will create a copy of the executable in `~/.local/bin` . If you want to run htyper from any directory, you should add this to your `PATH`.
This was only tested on Linux, I can't guarantee that installation on another OS will work.

htyper uses the DECSCUSR escape sequence to change the cursor shape, which might not be supported by every terminal.

# Usage

```
Usage: htyper [-q|--quote ARG] [-l|--line_length ARG] [-n|--num_words ARG]
Available options:
  -m,--mode ARG            possible arguments: quote, random, timed
  -t,--time ARG            How long the test should run, only affects timed mode
                           (default: 30)
  -l,--line_length ARG     Number of words to display per line (default: 10)
  -n,--num_words ARG       Number of Words to randomly select (default: 50)
  -h,--help                Show this help text

```

Running htyper will create the file `~/.config/htyper/htyper.conf`, which contains the following settings:

| Variable | Type (Range) | Description |
| --- | --- | --- |
|`fgcolor`| Hexadecimal Int (000000 -> ffffff) | The color of the test text. |
|`cursorshape`| Int (0 -> 6) | The displayed text cursor. 0-1 = blinking block, 2 = steady block, 3 = blinking underline, 4 = steady underline, 5 = blinking bar, 6 = steady bar.|
|`numcommonwords`| Int (0 -> 1000) | The amount of words to use from the 1000 most common list (Used in Random and Timed Mode). |

You can change these lines as you want, but setting them to a value outside of the defined range might have unexpected results.

If you want to change the wordlist, you can edit `textfiles/1000us.text`, or you can add quotes to `textfiles/quotes.txt` delimited by a `^_^`.

# Suggestions

If you have any ideas or suggestions just open an issue or create a pull request.
