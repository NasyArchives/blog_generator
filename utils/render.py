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
* file: utils/render.py
* license: MIT

Useful utilities.

Including:

    * function: bhash
    * class: Render

Copyright © 2017 by Nasy. All Rights Reserved.
"""
import logging
import os
import re
import shutil
import subprocess
from collections import Counter
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple

import bs4
import htmlmin
import pendulum
import requests as req
from jinja2 import Environment, FileSystemLoader
from ruamel.yaml import YAML

from .summary import Summary
from .utils import BI, BL, CFW, CONFIG, PH, bhash

assert CFW

yaml = YAML(typ = "safe")
NWORD = set((
    "1234567890qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM"
    "+-=_[]{}.,。，？?$()（）【】「」『』!！！～~*/&%\"\'“”‘’\n\t<>``\\;:；："
    "@#%^&|"
))
NWORD.add("")


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
            if force:
                res.add(path)
                continue
            with path.open() as f:
                logging.info(f"checking hash:{path.as_posix()}")
                hsh = bhash(f.read())
                phsh = bhash(path.stem)
                red = False if bfhash.get(phsh) == hsh else True
                bfhash[phsh] = hsh
                if red:
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
        NEEDED = {
            "date", "author", "tags", "categories", "title", "update",
            "language"
        }
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
                res[bhashp]["language"] = "chinese"
                for ii in {"tags", "categories"}:
                    res[bhashp][ii] = set([f"no {ii}"])
                for line in f:
                    if line[0] == "\n" or line[0:2] != "#+" or ":" not in line:
                        n += 1
                        continue
                    if line[0] == "*" or n > 1:
                        break

                    key, *values = re.split(
                        r":\s*",
                        line.replace("\n", "").replace("#+", "")
                    )
                    value = ":".join(values)
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
                        svalue = set(re.split(r",\s*", value))
                        res[bhashp][key] = svalue
                        continue
                    if key in NEEDED:
                        res[bhashp][key] = value
                        if key == "language":
                            res[bhashp][key] = "chinese" if value == "cn" else "english"
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
                "(setq org-export-with-toc 3)"
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
                content_table.attrs["class"] = content_table.attrs.pop("id")

                # Remove html file
                p.unlink()
                logging.debug(f"removed html: {p}")

            # TODO: Need help.
            res.setdefault(bhashp, {})["content"] = str(content).replace(
                "<table", "<div class='table_container'><table"
            ).replace("/table>", "/table></div>")

            res[bhashp]["content_table"] = str(content_table)

            # wordcounter
            ccount = Counter(content.text)
            res[bhashp]["wordcount"] = str(
                sum(ccount.values()) -
                sum([ccount[i] for i in ccount if i in NWORD])
            )

        return res

    @staticmethod
    def render_stylus(
            path: PH = "./theme/nasyland/styles",
            spath: PH = "./public/stylesheet"
    ) -> None:
        """Render stylus file to css."""
        path = Path(path)
        spath = Path(spath)
        spath.mkdir(parents = True, exist_ok = True)

        subprocess.Popen(
            (
                f"cd {path} && stylus -c style.styl "
                f"-o ../../../{spath}/main.css"
            ),
            shell = True,
        ).wait()
        subprocess.Popen(
            (
                f"cd {path} && stylus -w -c style.styl "
                f"-o ../../../{spath}/main.css"
            ),
            shell = True,
        )

    @staticmethod
    def move_static() -> None:
        """Move static file to public."""
        public = Path("public")
        for dst in Path("theme/nasyland/").glob("*"):
            if dst.stem in {"lib", "images"}:
                if public.joinpath(dst.stem).exists():
                    later = []
                    for fs in public.joinpath(dst.stem).rglob("*"):
                        if fs.is_dir():
                            later.append(fs)
                            continue
                        fs.unlink()
                    for dirs in later[::-1]:
                        dirs.rmdir()
                    public.joinpath(dst.stem).rmdir()
                shutil.copytree(dst, public.joinpath(dst.stem))

    @staticmethod
    def _others() -> Dict[str, str]:
        """Get others."""
        logging.info("Getting mathjax")
        try:
            content = bs4.BeautifulSoup(
                req.get("https://www.mathjax.org/").content, "lxml"
            )
            mathjax = content.select("#gettingstarted .snippet code")[0].text
        except req.exceptions.ProxyError:
            mathjax = (
                "<script src='https://cdnjs.cloudflare.com/ajax/libs"
                "/mathjax/2.7.2/MathJax.js?"
                "config=TeX-MML-AM_CHTML'></script>"
            )
        return {"mathjax": mathjax}

    @staticmethod
    def _tags_filter(blinks: List[BL], ctag: str) -> List[BL]:
        """Filter tags."""
        res = []
        for blink in blinks:
            if ctag in blink.tags:
                res.append(blink)
        return res

    @staticmethod
    def _archives_filter(blinks: List[BL],
                         apath: str) -> List[Tuple[str, List[BL]]]:
        """Filter tags."""
        bls = []
        res = []
        if apath:
            for blink in blinks:
                if apath in blink.url:
                    bls.append(blink)
        else:
            bls = blinks.copy()
        bls.sort(key = lambda x: x.url.split("/")[1])
        nbls = []
        old_year = bls[0].url.split("/")[1]
        for bl in bls:
            year = bl.url.split("/")[1]
            if year == old_year:
                nbls.append(bl)
            else:
                res.append((old_year, nbls))
                old_year = year
                nbls = [bl]
        res.append((old_year, nbls))
        res.reverse()
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
    def bpaths(self) -> Set[str]:
        """Get all of the blog paths."""
        return set((str(blog["path"]) for blog in self.blogs.values()))

    @property
    def blinks(self) -> List[BL]:
        """Get a blink for the blog renderer."""
        blinks = set()
        for blog in self.blogs.values():
            date = pendulum.parse(blog["date"])
            blinks.add(
                BL(
                    time = date.__str__(),
                    summary = Summary(
                        str(blog["content"]), str(blog["language"])
                    ).get(),
                    url = f"/{str(blog['path'])}",
                    title = str(blog["title"]),
                    author = str(blog["author"]),
                    tags = tuple(blog["tags"]),
                    categories = tuple(blog["categories"])
                )
            )
        res = sorted(blinks, key = lambda x: x.time, reverse = True)
        for i, e in enumerate(res):
            for key, blog in self.blogs.items():
                if e.title == blog["title"]:
                    self.blogs[key]["prev"] = (res[i - 1].url if i else "")
                    self.blogs[key]["next"] = (
                        res[i + 1].url if i + 1 < len(res) else ""
                    )

        return res

    @property
    def ctags(self) -> Dict[str, int]:
        """Get tags from blogs."""
        ctags = dict()  # type: Dict[str, int]
        for blog in self.blogs.values():
            for tag in set(blog["tags"]):
                ctags[tag] = ctags.setdefault(tag, 0) + 1

        return ctags

    @property
    def bapaths(self) -> Set[str]:
        """Get all of the blog archives paths."""
        bapath = set()  # type: Set[str]
        bapath.add("")
        for path in self.bpaths:
            year, month, day, _ = path.split("/")
            bapath.add(year)
            bapath.add(f"{year}/{month}")
            bapath.add(f"{year}/{month}/{day}")
        return bapath

    @property
    def _oinfo(self) -> BI:
        """Get information from org files."""
        return self.get_info_from_org(self.bfiles)

    @property
    def _hfiles(self) -> Set[Path]:
        """Render org to html using emacs."""
        return self.org_to_html(self.bfiles)

    @property
    def _hinfo(self) -> BI:
        """Get information from `HTML`."""
        return self.get_html_content(self._hfiles)

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
        self.jinjaenv = Environment(
            loader = FileSystemLoader("./theme/nasyland/layout")
        )
        self.jinjaenv.filters["tags_filter"] = self._tags_filter
        self.jinjaenv.filters["archives_filter"] = self._archives_filter

        self.blogs = {}  # type: BI
        self.others = {}  # type: Dict[str, str]
        self.load_saved_blogs()
        self.update_blogs()

    def _to_html(self) -> None:
        """Render the all blogs to htmls."""
        # -------------
        # move statics
        self.move_static()

        # -------------
        # render stylus
        self.render_stylus()

        # -------------
        # render index
        logging.info(f"Rendering index.html")

        index = self.jinjaenv.get_template("index.html")
        iconfig = {}  # type: CFW
        iconfig.update(self.others)
        iconfig.update(CONFIG.blog._asdict())
        iconfig["blinks"] = self.blinks
        iconfig["cctags"] = self.ctags

        htmls = htmlmin.minify(index.render(iconfig))

        os.makedirs(f"public/", exist_ok = True)
        with open(f"public/index.html", "w") as f:
            f.write(htmls)

        # -------------
        # render blogs
        for key, blog in self.blogs.items():

            logging.info(f"Rendering {blog['title']}")

            template = self.jinjaenv.get_template("blog.html")

            bconfig = {}  # type: CFW
            bconfig.update(CONFIG.blog._asdict())
            bconfig.update(blog)
            bconfig.update(self.others)
            bconfig["blinks"] = self.blinks

            htmls = htmlmin.minify(template.render(bconfig))

            os.makedirs(f"public/{blog['path']}", exist_ok = True)
            with open(f"public/{blog['path']}/index.html", "w") as f:
                f.write(htmls)

        # -------------
        # render tags
        for ctag in self.ctags:
            logging.info(f"Rendering tag: {ctag}")

            template = self.jinjaenv.get_template("tag.html")

            tconfig = {}  # type: CFW
            tconfig["ctag"] = ctag
            tconfig.update(self.others)
            tconfig.update(CONFIG.blog._asdict())
            tconfig["blinks"] = self.blinks

            htmls = htmlmin.minify(template.render(tconfig))

            os.makedirs(f"public/tags/{ctag}", exist_ok = True)
            with open(f"public/tags/{ctag}/index.html", "w") as f:
                f.write(htmls)

        # -------------
        # render archives
        for apath in self.bapaths:
            logging.info(f"Rendering archives: {apath}")

            template = self.jinjaenv.get_template("archive.html")

            aconfig = {}  # type: CFW
            aconfig["apath"] = apath
            aconfig.update(self.others)
            aconfig.update(CONFIG.blog._asdict())
            aconfig["blinks"] = self.blinks

            htmls = htmlmin.minify(template.render(aconfig))

            os.makedirs(f"public/archives/{apath}", exist_ok = True)
            with open(f"public/archives/{apath}/index.html", "w") as f:
                f.write(htmls)

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

    def update_blogs(self) -> None:
        """Update blogs content."""
        oinfo = self._oinfo
        hinfo = self._hinfo
        if oinfo.keys() == hinfo.keys():
            for k in oinfo:
                self.blogs[k] = {**oinfo[k], **hinfo[k]}
                date = pendulum.parse(oinfo[k]["date"])
                self.blogs[k]["path"] = (
                    f"{date.year}/{date.month}/"
                    f"{date.day}/{oinfo[k]['title']}"
                )
        self.save_blog()
        self.others = self._others()


if __name__ == '__main__':
    logging.basicConfig(
        level = logging.DEBUG,
        format = "%(levelname)s\t[%(asctime)s]\t%(message)s",
        datefmt = "%x %X"
    )
    render = Render(True)
    render._to_html()
