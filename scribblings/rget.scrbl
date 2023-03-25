#lang scribble/manual
@require[@for-label[rget
                    (except-in racket/base #%module-begin read-syntax)]]

@title{rget}
@author{hin}

@defmodule[rget #:reader]

A HTTP-GET client written in racket.

This module should be used through a @code[#:lang "reader rget"] header.
