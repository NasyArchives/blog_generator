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
* file: const_tools.py
* license: MIT

Copyright © 2017 by Nasy. All Rights Reserved.
"""
from typing import NamedTuple

# Config types
CF_Blog = NamedTuple(
    "CF_Blog", [
        ("title", str),
        ("description", str),
        ("author", str),
        ("path", str),
        ("suffix", str),
    ]
)
CF_Hash = NamedTuple("CF_Hash", [
    ("method", str),
    ("seed", int),
])
CF = NamedTuple("CF", [
    ("blog", CF_Blog),
    ("hash", CF_Hash),
])


def generate_config(
        title: str = "Nasy Land",
        description: str = "Nasy 的花园，栽花、养鱼以及闲聊d的d地方～",
        author: str = "Nasy",
        bpath: str = "blog",
        fsuffix: str = "org",
        method: str = "xxhash",
        seed: str = "Nasy"
) -> CF:
    """Generate the config."""
    return CF(
        blog = CF_Blog(
            title = title,
            description = description,
            author = author,
            path = bpath,
            suffix = fsuffix,
        ),
        hash = CF_Hash(method = method, seed = sum(map(ord, seed)))
    )
