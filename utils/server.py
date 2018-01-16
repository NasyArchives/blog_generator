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
* date: Jan 12, 2018
* email: echo bmFzeXh4QGdtYWlsLmNvbQo= | base64 -D
* file: utils/server.py
* license: MIT

"Well, for some time, I wondered if it was needed a server, I had already
    rendered the .org files to HTML, to response the requests real-time.
    Eventually, I found that the server was not used for the real-time
    responding, it was used to the live-reload the web pages, which made you
    debugging efficiently." -- Nasy

Copyright © 2017 by Nasy. All Rights Reserved.
"""
import os
from urllib.parse import quote

from sanic import Sanic

from utils.render import Render


class Server:
    """The static blog server."""

    def _add_bstatic(self) -> None:
        """Add blogs' html static files."""
        self.app.static("/", "./public/index.html")
        for path in self.render.bpaths:
            upath = quote(path)
            print(path, upath, sep = " --> ")
            self.app.static(upath, f"./public/{path}")
            self.app.static(upath, f"./public/{path}/index.html")

            self.app.static(f"{upath}.html", f"public/{path}")
            self.app.static(f"{upath}.html", f"public/{path}/index.html")

        for tag in self.render.ctags:
            upath = quote(f"/tags/{tag}")
            print(tag, upath, sep = " --> ")
            self.app.static(upath, f"./public/tags/{tag}")
            self.app.static(upath, f"./public/tags/{tag}/index.html")

    def __init__(self, rforce: bool = False) -> None:
        """Initilizing the server."""
        self.app = Sanic("Nasy Land")
        self.render = Render(force = rforce)
        self.render._to_html()

        self._add_bstatic()

        for root, dirs, files in os.walk("public"):
            if files:
                upath = quote(root.replace("public/", "/"))
                for f in files:
                    self.app.static(f"{upath}/{f}", f"{root}/{f}")
                    print(upath, f"{root}/{f}", sep = " --> ")


def main() -> None:
    """Yooo, here is the main function."""
    pass


if __name__ == "__main__":
    main()
