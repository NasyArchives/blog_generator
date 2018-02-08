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
* date: Feb 02, 2018
* email: echo bmFzeXh4QGdtYWlsLmNvbQo= | base64 -D
* file: utils/summary.py
* license: MIT

Copyright © 2017 by Nasy. All Rights Reserved.
"""
#TODO(Nasy): Toooooooooooooo Slooooooooooooooooow
from typing import List

from sumy.nlp.stemmers import Stemmer
from sumy.nlp.tokenizers import Tokenizer
from sumy.parsers.html import HtmlParser
from sumy.summarizers.lex_rank import LexRankSummarizer
from sumy.utils import get_stop_words

assert List


class Summary:
    """Summary of the blog."""

    def __init__(self, text: str, language: str = "chinese") -> None:
        """Initilize the Summary."""
        self.parser = None  # type: HtmlParser
        stemmer = Stemmer(language)
        self.summarizer = LexRankSummarizer(stemmer)
        self.summarizer.stop_words = get_stop_words(language)
        self.sentences = []  # type: List[str]
        self(text)

    def _run(self) -> None:
        """Run to find sentences of document."""
        self.sentences = []
        for sentence in self.summarizer(self.parser.document, 20):
            if "「" not in str(sentence) and "」" not in str(sentence):
                self.sentences.append(sentence)

    def __call__(self, text: str, language: str = "chinese") -> str:
        """Set text strings."""
        self.parser = HtmlParser(text, Tokenizer(language))
        return self.get()

    def get(self, n: int = 4) -> str:
        """Get n sentences."""
        self._run()
        summary = "".join([
            "<li class=\"blog_link_summary_list\">"
            f"<i class=\"fas fa-circle-notch fa-sm\"></i><p>{i}</p>"
            "</li>" for i in self.sentences[:n]
        ])
        if summary:
            return ("<ul>" + summary + "</ul>")
        else:
            return ""
