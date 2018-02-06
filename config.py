#!/usr/bin/env python3
# -*- coding: utf-8 -*-
r"""
Life's pathetic, have fun ("▔□▔)/hi~♡ Nasy.

Excited without bugs::

    |             *         *
    |                  .                .
    |           .
    |     *                      ,
    |                   .
    |
    |                               *
    |          |\___/|
    |          )    -(             .              '
    |         =\  -  /=
    |           )===(       *
    |          /   - \
    |          |-    |
    |         /   -   \     0.|.0
    |  NASY___\__( (__/_____(\=/)__+1s____________
    |  ______|____) )______|______|______|______|_
    |  ___|______( (____|______|______|______|____
    |  ______|____\_|______|______|______|______|_
    |  ___|______|______|______|______|______|____
    |  ______|______|______|______|______|______|_
    |  ___|______|______|______|______|______|____

* author: Nasy
* date: Jan 2, 2018
* email: echo bmFzeXh4QGdtYWlsLmNvbQo= | base64 -D
* file: config.py
* license: MIT

The configurations.

Copyright © 2017 by Nasy. All Rights Reserved.
"""
import pendulum

config = dict(
    # Your Blog's Setting
    title = "Nasy Land",
    description = "Nasy 的花园，栽花、养鱼以及闲聊的地方～",
    author = "Nasy",
    bpath = "blog",
    fsuffix = "org",
    copyright = f"Copyright © {pendulum.now().year} Nasy",
    google_ana = "UA-102577027-1",

    # Hash setting
    method = "xxhash",
    seed = "Nasy"
)
