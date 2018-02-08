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
* date: Feb 01, 2018
* email: echo bmFzeXh4QGdtYWlsLmNvbQo= | base64 -D
* file: utils/utils.py
* license: MIT

Copyright © 2017 by Nasy. All Rights Reserved.
"""
import hashlib
import pathlib
from functools import lru_cache
from typing import Dict, List, NamedTuple, Set, Tuple, Union

import pendulum
import xxhash

from config import config

# Path types
PH = Union[str, pathlib.Path]
BI = Dict[str, Dict[str, Union[str, Set[str]]]]

# Blog link types
BL = NamedTuple(
    "BL", [
        ("time", str),
        ("summary", str),
        ("url", str),
        ("title", str),
        ("author", str),
        ("tags", Tuple[str, ...]),
        ("categories", Tuple[str, ...]),
    ]
)

# Config types
CF_Blog = NamedTuple(
    "CF_Blog", [
        ("title", str),
        ("description", str),
        ("author", str),
        ("path", str),
        ("suffix", str),
        ("copyright", str),
        ("google_ana", str),
        ("cname", str),
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

CONFIG = CF(
    blog = CF_Blog(
        title = config.get("title", "Nasy Land"),
        description = config.get("description", "Nasy 的花园，栽花、养鱼以及闲聊d的d地方～"),
        author = config.get("author", "Nasy"),
        path = config.get("bpath", "blog"),
        suffix = config.get("fsuffix", "org"),
        copyright = config.get(
            "copyright", f"Copyright © {pendulum.now().year} Nasy"
        ),
        google_ana = config.get("google_ana"),
        cname = config.get("cname"),
    ),
    hash = CF_Hash(
        method = config.get("method", "xxhash"),
        seed = sum(map(ord, config.get("seed", "Nasy")))
    )
)

CFW = Dict[str, Union[str, List[BL], Dict[str, int], Set[str]]]


@lru_cache(maxsize = 65536)
def bhash(
        content: str,
        method: str = CONFIG.hash.method,
        seed: int = CONFIG.hash.seed
) -> str:
    """Hash the content of pathfile."""
    if not method:
        method = "xxhash"

    if method == "sha1":
        hstr = hashlib.sha1(content.encode()).hexdigest()
    elif method == "sha256":
        hstr = hashlib.sha256(content.encode()).hexdigest()
    elif method == "md5":
        hstr = hashlib.md5(content.encode()).hexdigest()
    else:
        hstr = xxhash.xxh64(content, seed = seed).hexdigest()

    return hstr
