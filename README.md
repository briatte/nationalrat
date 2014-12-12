This repository contains code to build cosponsorship networks from bills passed in the [lower/national chamber](http://www.parlament.gv.at/) of the Austrian Parliament.

- [interactive demo](http://briatte.org/nationalrat)
- [static plots](http://briatte.org/nationalrat/plots.html)

# HOWTO

Replicate by running `make.r` in R.

The `data.r` script downloads information on bills and sponsors. There is not much data to download in both cases, so the loops should execute pretty quickly. All photos should download fine.

The `build.r` script then assembles the edge lists and plots the networks, with the help of a few routines coded into `functions.r`. Adjust the `plot`, `gexf` and `mode` parameters to skip the plots or to change the node placement algorithm.

The networks are very sparse (0.01 < _d_ < 0.03) and show an odd case of dissortative mixing: due to the "grand coalition" formed by the SPÖ and the ÖVP in the post-Haider era, members of the two parties cosponsor with each other as if they formed a single entity.

# DATA

## Bills

- `legislature` -- legislature id (coded in Roman numbers)
- `date` -- date of introduction of the bill (yyyy-mm-dd)
- `title` -- title
- `ref` -- unique identifier
- `url` -- bill URL
- `sponsors` -- bill sponsors, using their numeric ids

## Sponsors

The sponsors data has one row per legislature in which the sponsor sat, which allows to keep track of party transitions like the move from FPÖ to BZÖ for several MPs after the 1999 legislative election (Haider).

- `id` -- numeric id, preceded by "id_"; points to the profile URL
- `name` -- full name, with titles like "Dr." or "Mag."
- `legislature` -- legislature of activity
- `party` -- political party
- `mandate` -- semicolon-separated mandate years, used to compute the `nyears` seniority variable
- `sex` -- gender (F/M), imputed from title ("Abgeordnete/r")
- `born` -- year of birth (stored as character)
- `photo` -- photo URL, a variation of the profile URL stored in `id`
