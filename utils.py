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
* file: utils.py
* license: MIT

Useful utilities.

Including:

    * function: bhash
    * class: Render

Copyright © 2017 by Nasy. All Rights Reserved.
"""

import hashlib
from functools import lru_cache

import xxhash

from config import CONFIG

try:
    import ujson as json
except ImportError:
    import json  # type: ignore


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


class Render:
    """A renderer of blog.

    What must be prioritized is that we have to read in our blog files.
        The question to be considered is whether the document to be read in has
        been rendered or modified already? To decide on this, we need to hash
        everything into a place that we can read every time. Here we can use
        ~xxhash~ since it is an extremely fast non-cryptographic hash
        algorithm. After that, we can read the text happily and hash it at the
        same time to avoid re-reading, or just ignore this file.

    The second step, equalling worthy to do, is getting headers from our blog
        files. That is because, for a blog, tags or categories is significant
        to give the reader a clear picture of what he wants. In our opinions,
        `{"date", "author", "tags", "categories", "title", "update"}` is
        essential.

    Next step is rendering blog source file to `HTML` with emacs org-mode,
        which can significantly reduce our burden because we are lazy. In this
        rendering process, there is only one place to note, that is, do not
        render unchanged files yet.

    And then, it is the time to read the information from `HTML`, which
        contains the main part of our blog, that we can simply use the
        packages, `bs4` and `lxml`, to parse the `HTML`. At the same time, we
        have to edit it to fit out website.

    Last, but not the least, we need to merge the content, from the `HTML`
        files, and the information, from the `.org` files, into a `Dict/json`
        which is used to return to the `server.py` to do next step.
    """

    def __init__(self) -> None:
        """Initilized Render."""
        pass
