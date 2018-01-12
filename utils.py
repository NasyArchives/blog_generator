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
import logging
import re
import subprocess
from functools import lru_cache
from pathlib import Path
from typing import Dict, Optional, Set

import bs4
import pendulum

import xxhash
from config import CONFIG
from const_tools import BI, PH
from jinja2 import Environment, FileSystemLoader
from ruamel.yaml import YAML

yaml = YAML(typ = "safe")


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

    * STEP ONE
    What must be prioritized is that we have to read in our blog files.
        The question to be considered is whether the document to be read in has
        been rendered or modified already? To decide on this, we need to hash
        everything into a place that we can read every time. Here we can use
        ~xxhash~ since it is an extremely fast non-cryptographic hash
        algorithm. After that, we can read the text happily and hash it at the
        same time to avoid re-reading, or just ignore this file.

    * STEP TWO
    The second step, equalling worthy to do, is getting headers from our blog
        files. That is because, for a blog, tags or categories is significant
        to give the reader a clear picture of what he wants. In our opinions,
        `{"date", "author", "tags", "categories", "title", "update"}` is
        essential.

    * STEP THREE
    Next step is rendering blog source file to `HTML` with emacs org-mode,
        which can significantly reduce our burden because we are lazy. In this
        rendering process, there is only one place to note, that is, do not
        render unchanged files yet.

    * STEP FOUR
    And then, it is the time to read the information from `HTML`, which
        contains the main part of our blog, that we can simply use the
        packages, `bs4` and `lxml`, to parse the `HTML`. At the same time, we
        have to edit it to fit out website.

    * STEP FIVE
    Last, but not the least, we need to merge the content, from the `HTML`
        files, and the information, from the `.org` files, into a `Dict/yml`
        which is used to return to the `server.py` to do next step.
    """

    @staticmethod
    def get_blog_file_hash(
            path: PH = Path("./.stores/bfhash.yml"),
    ) -> Dict[str, str]:
        """Get blog files hash."""
        path = Path(path)
        if path.exists():
            bfhash = yaml.load(path)
        else:
            bfhash = {}
        return bfhash

    @staticmethod
    def get_render_file(
            bfhash: Dict[str, str],
            hstore: PH = Path("./.stores/bfhash.yml"),
            force: bool = False
    ) -> Set[Path]:
        """Get files that will be readered.

        STEP ONE.

        Contain three steps:
            + Get the render-file path
            + Determine whether the source file has already rendered or not
            + Determine whether the source file has modified or not
        """
        hstore = Path(hstore)
        bfiles = Path().glob(f"./{CONFIG.blog.path}/*.org")
        res = set()
        for path in bfiles:
            with path.open() as f:
                logging.info(f"checking hash:{path.as_posix()}")
                hsh = bhash(f.read())
                phsh = bhash(path.stem)
                red = False if bfhash.get(phsh) == hsh else True
                bfhash[phsh] = hsh
                if force or red:
                    res.add(path)

        # pylint: disable=E1101
        # pylint bug
        # https://github.com/PyCQA/pylint/issues/1660
        if not hstore.parent.exists():
            # pylint: enable=E1101
            hstore.parent.mkdir()

        yaml.dump(bfhash, hstore)

        return res

    @staticmethod
    def get_info_from_org(paths: Set[Path]) -> BI:
        """Get information from org files.

        STEP TWO.
        """
        res = {}  # type: BI
        NEEDED = {"date", "author", "tags", "categories", "title", "update"}
        for path in paths:
            logging.info(f"reading from org: {path}")
            with path.open() as f:
                n = 0
                bhashp = bhash(path.stem)
                res.setdefault(bhashp, {})["author"] = CONFIG.blog.author
                res[bhashp]["date"] = pendulum.create(
                    pendulum.now().year,
                    pendulum.now().month,
                    pendulum.now().day
                ).ctime()
                res[bhashp]["title"] = path.stem
                for ii in {"tags", "categories"}:
                    res[bhashp][ii] = set()
                for line in f:
                    if line[0] == "\n" or line[0:2] != "#+" or ":" not in line:
                        n += 1
                        continue
                    if line[0] == "*" or n > 1:
                        break

                    key, value = re.split(
                        r":\s*",
                        line.replace("\n", "").replace("#+", "")
                    )
                    key = key.lower()
                    if key in {"date", "update"}:
                        if value:
                            value = pendulum.parse(
                                value.replace("<", "").replace(">", "")
                            ).ctime()
                        else:
                            value = pendulum.create(
                                pendulum.now().year,
                                pendulum.now().month,
                                pendulum.now().day
                            ).ctime()
                    if key in {"tags", "categories"}:
                        value = set(re.split(r",\s*", value))
                        res[bhashp][key] |= value
                        continue
                    if key in NEEDED:
                        res[bhashp][key] = value
                res[bhashp].setdefault("update", res[bhashp]["date"])

        return res

    @staticmethod
    def org_to_html(paths: Set[Path]) -> Set[Path]:
        """Render org to html using emacs.

        STEP THREE.
        """
        if subprocess.Popen(
                "emacsclient -q --socket-name=org_to_html "
                "-e '(message \"hi\")'",
                shell = True,
                stdout = subprocess.PIPE,
                stderr = subprocess.PIPE,
        ).wait():
            logging.warning("No emacs daemon, create one.")
            if not subprocess.Popen(
                    "emacs --no-desktop --daemon=org_to_html",
                    shell = True,
                    stdout = subprocess.PIPE,
                    stderr = subprocess.PIPE,
            ).wait():
                logging.info("Success start emacs daemon org_to_html")
            else:
                raise RuntimeError("Failed to start emacs daemon!")

        res = set()
        for p in paths:
            logging.debug(f"exporting to html: {p}")
            subprocess.Popen(
                "emacsclient --socket-name=org_to_html "
                f"-e '(progn (find-file \"{p}\") "
                "(setq org-export-preserve-breaks nil)"
                "(setq org-export-with-emphasize t)"
                "(setq org-export-with-special-strings t)"
                "(setq org-export-with-sub-superscripts t)"
                "(setq org-export-headline-levels 4)"
                "(setq org-export-with-latex t)"
                "(setq org-export-with-fixed-width t)"
                "(setq org-export-with-section-numbers nil) "
                "(setq org-export-with-toc t)"
                "(setq org-export-with-tables t)"
                "(org-html-export-to-html))' ",
                shell = True,
                stdout = subprocess.PIPE,
                stderr = subprocess.PIPE,
            ).wait()
            res.add(p.with_suffix(".html"))

        return res

    @staticmethod
    def get_html_content(hpaths: Set[Path]) -> BI:
        """Get information from `HTML`.

        STEP FOUR.
        """
        res = {}  # type: BI
        for p in hpaths:
            bhashp = bhash(p.stem)
            if p.is_dir() or not p.exists():
                continue
            with p.open() as f:
                logging.info(f"reading html: {p}")
                content = bs4.BeautifulSoup(f.read(),
                                            "lxml").select("#content")[0]
                content_table = content.select("#table-of-contents")[0]

                # Edit content.
                content = content.replace_with(content_table)

                for pre in content.select("pre"):
                    # Highlight.js needs the src in `pre code` Tag.
                    tag_code = bs4.BeautifulSoup("", "lxml").new_tag("code")
                    tag_code.string, pre.string = pre.text, tag_code.text
                    for c in pre["class"]:
                        tmp = re.findall(r"src-(\w+)", c)
                        for src in tmp:
                            if src:
                                tag_code["class"] = src
                                break
                    pre.append(tag_code)

                # Edit content_table.
                content_table = content_table.select(
                    "#text-table-of-contents"
                )[0]
                content_table.name = "nav"

                # Remove html file
                p.unlink()
                logging.debug(f"removed html: {p}")

            res.setdefault(bhashp, {})["content"] = str(content)
            res[bhashp]["content_table"] = str(content_table)

        return res

    @property
    def bfhash(self) -> Dict[str, str]:
        """Get blog files hash."""
        return self.get_blog_file_hash(self.hstore)

    @property
    def bfiles(self) -> Set[Path]:
        """Get files that will be readered."""
        return self.get_render_file(self.bfhash, self.hstore, self.force)

    @property
    def oinfo(self) -> BI:
        """Get information from org files."""
        return self.get_info_from_org(self.bfiles)

    @property
    def hfiles(self) -> Set[Path]:
        """Render org to html using emacs."""
        return self.org_to_html(self.bfiles)

    @property
    def hinfo(self) -> BI:
        """Get information from `HTML`."""
        return self.get_html_content(self.hfiles)

    def __init__(
            self,
            force: bool = False,
            hstore: PH = Path("./.stores/bfhash.yml"),
            bstore: PH = Path("./.stores/blogs.yml")
    ) -> None:
        """Initilizing Render."""
        self.force = force
        self.hstore = Path(hstore)
        self.bstore = Path(bstore)

        self.blogs = {}  # type: BI
        self.load_saved_blogs()
        self.update_blogs()
        self.jinjaenv = Environment(
            loader = FileSystemLoader("./theme/nasyland/")
        )

    def load_saved_blogs(self) -> None:
        """Load saved blogs' content."""
        if self.bstore.exists():
            self.blogs.update(yaml.load(self.bstore))

    def save_blog(self, path: Optional[PH] = None,
                  replace: bool = False) -> None:
        """Save blogs' content."""
        if path:
            bstore = Path(path)
            if replace:
                self.bstore = bstore
        else:
            bstore = self.bstore

        yaml.dump(self.blogs, bstore)

    def to_html(self) -> None:
        """Render the all blogs to htmls."""
        for _, blog in self.blogs.items():
            logging.info(f"Rendering {blog['title']}")
            template = self.jinjaenv.get_template("layout.html")
            htmls = template.render(blog, description = CONFIG.blog)
            with open(f"public/{blog['title']}.html", "w") as f:
                f.write(htmls)

    def update_blogs(self) -> None:
        """Update blogs content."""
        oinfo = self.oinfo
        hinfo = self.hinfo
        if oinfo.keys() == hinfo.keys():
            for k in oinfo:
                self.blogs[k] = {**oinfo[k], **hinfo[k]}
        self.save_blog()


if __name__ == '__main__':
    logging.basicConfig(
        level = logging.DEBUG,
        format = "%(levelname)s\t[%(asctime)s]\t%(message)s",
        datefmt = "%x %X"
    )
    render = Render(True)
    render.to_html()
