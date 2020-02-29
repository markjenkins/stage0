#!/usr/bin/env python3

# Copyright (C) 2020 Mark Jenkins <mark@markjenkins.ca>
# This file is part of stage0.
#
# stage0 is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# stage0 is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with stage0.  If not, see <http://www.gnu.org/licenses/>.

from os.path import dirname, join as path_join

# The following globals, class and function definitions are copy-pasted
# from M1.py in https://github.com/markjenkins/knightpies
#
# Please don't include any other code in this section. See the comment
# marking the section end.
#
# From a code reading perspective, we recommend skipping this section
# and jumping to the real heart and soul of the disassembler
#
# These global variable definitions were copy-pasted from M1.py in
# https://github.com/markjenkins/knightpies
# revision e10fbd920ae4cf7b4b29c60986d0bab9993aef84
#
# This redundancy can be cleaned up once knightpies reaches maturity and is
# merged into the stage0 project
#
# Doing fancy things like a git submodule and manipulating sys.path
# was not considered worth it for this small amount of borrowed code
TOK_TYPE_MACRO, TOK_TYPE_ATOM, TOK_TYPE_STR, TOK_TYPE_DATA, \
    TOK_TYPE_COMMENT, TOK_TYPE_NEWLINE = range(6)
TOK_TYPE, TOK_EXPR, TOK_FILENAME, TOK_LINENUM = range(4)
MACRO_NAME, MACRO_VALUE = 0, 1

# This exception definition was copy-pasted from M1.py in
# https://github.com/markjenkins/knightpies
# revision e10fbd920ae4cf7b4b29c60986d0bab9993aef84
#
# This redundancy can be cleaned up once knightpies reaches maturity and is
# merged into the stage0 project
#
# Doing fancy things like a git submodule and manipulating sys.path
# was not considered worth it for this small amount of borrowed code
class MultipleDefinitionsException(Exception):
    pass

# This function was copy-pasted from M1.py in
# https://github.com/markjenkins/knightpies
# revision e10fbd920ae4cf7b4b29c60986d0bab9993aef84
#
# Python 2.2.0 compatability via COMPAT_TRUE which M1.py
# imports from pythoncompat.py were replaced with the True constant
# available from Python 2.2.1 onward.
#
# This redundancy can be cleaned up once knightpies reaches maturity and is
# merged into the stage0 project
#
# Doing fancy things like a git submodule and manipulating sys.path
# was not considered worth it for this small amount of borrowed code
def read_atom(first_char, f):
    buf = first_char
    while True:
        c = f.read(1)
        if c in ('', "\n", "\t", " "):
            break
        else:
            buf += c
    return buf, c

# This function was copy-pasted from M1.py in
# https://github.com/markjenkins/knightpies
# revision e10fbd920ae4cf7b4b29c60986d0bab9993aef84
#
# Python 2.2.0 compatability via COMPAT_TRUE which M1.py
# imports from pythoncompat.py were replaced with the True constant
# available from Python 2.2.1 onward.
#
# This redundancy can be cleaned up once knightpies reaches maturity and is
# merged into the stage0 project
#
# Doing fancy things like a git submodule and manipulating sys.path
# was not considered worth it for this small amount of borrowed code
def read_until_newline_or_EOF(f):
    comment_buffer = ''
    while True:
        c = f.read(1)
        if c == '' or c=='\n' or c=='\r':
            return c, comment_buffer
        else:
            comment_buffer += c

# This function was copy-pasted from M1.py in
# https://github.com/markjenkins/knightpies
# revision e10fbd920ae4cf7b4b29c60986d0bab9993aef84
#
# Python 2.2.0 compatability via COMPAT_TRUE and COMPAT_FALSE which M1.py
# imports from pythoncompat.py were replaced with True and False constants
# available from Python 2.2.1 onward.
#
# This redundancy can be cleaned up once knightpies reaches maturity and is
# merged into the stage0 project
#
# Doing fancy things like a git submodule and manipulating sys.path
# was not considered worth it for this small amount of borrowed code
def tokenize_file(f):
    line_num = 1
    string_char, string_buf = None, None
    while True:
        c = f.read(1)
        if c=='':
            if string_char != None:
                raise Exception("unmatched %s quote in %s line %s",
                                string_char, f.name, line_num)
            break
        # look for being in string stage first, as these are not
        # interupted by newline or comments
        elif (string_char != None):
            if string_char == c:
                if string_char == '"':
                    yield (TOK_TYPE_STR, string_buf, f.name, line_num)
                elif string_char == "'":
                    yield (TOK_TYPE_DATA, string_buf, f.name, line_num)
                else:
                    assert False # we should never reach here
                string_char, string_buf = None, None
            else:
                string_buf += c
        elif c == '#' or c == ';':
            c, comment = read_until_newline_or_EOF(f)
            yield (TOK_TYPE_COMMENT, comment, f.name, line_num)
            if c!= '':
                yield (TOK_TYPE_NEWLINE, '\n', f.name, line_num)
                line_num+=1
            else:
                break
        elif (string_char == None) and (c == '"' or c == "'"):
            string_char = c
            string_buf  = ''
        elif c == '\n':
            yield (TOK_TYPE_NEWLINE, '\n', f.name, line_num)
            line_num+=1
        elif c == ' ' or c == '\t':
            pass
        else:
            atom, trailing_char = read_atom(c, f)
            yield (TOK_TYPE_ATOM, atom, f.name, line_num)
            if trailing_char == '':
                break
            elif trailing_char == '\n':
                yield (TOK_TYPE_NEWLINE, '\n', f.name, line_num)
                line_num+=1

    yield (TOK_TYPE_NEWLINE, '\n', f.name, line_num)

# This function was copy-pasted from M1.py in
# https://github.com/markjenkins/knightpies
# revision e10fbd920ae4cf7b4b29c60986d0bab9993aef84
#
# Python 2.2.0 compatability via COMPAT_TRUE which M1.py
# imports from pythoncompat.py were replaced with the True constant
# available from Python 2.2.1 onward.
#
# This redundancy can be cleaned up once knightpies reaches maturity and is
# merged into the stage0 project
#
# Doing fancy things like a git submodule and manipulating sys.path
# was not considered worth it for this small amount of borrowed code
def upgrade_token_stream_to_include_macro(input_tokens):
    input_tokens_iter = iter(input_tokens)
    while True:
        try:
            tok = next(input_tokens_iter)
        except StopIteration:
            break

        tok_type, tok_expr, tok_filename, tok_linenum = tok
        # if we have a DEFINE atom we're going to yield a TOK_TYPE_MACRO
        # based on the next two tokens
        if tok_type == TOK_TYPE_ATOM and tok_expr == "DEFINE":
            # look ahead to token after DEFINE
            try:
                macro_name_tok = next(input_tokens_iter)
            except StopIteration:
                raise Exception(
                    "%s ended with uncompleted DEFINE" % tok_filename
                )

            # enforce next token after DEFINE atom must be an atom,
            # not newline or string
            if (macro_name_tok[TOK_TYPE] == TOK_TYPE_STR or
                macro_name_tok[TOK_TYPE] == TOK_TYPE_DATA ):
                raise Exception(
                    "Using a string for macro name %s not supported "
                    "line %s from %s" % (
                        tok_expr, tok_linenum, tok_filename) )
            elif macro_name_tok[TOK_TYPE] == TOK_TYPE_NEWLINE:
                raise Exception(
                    "You can not have a newline in a DEFINE "
                    "line %s from %s" % (
                        tok_expr, tok_linenum, tok_filename) )
            assert macro_name_tok[TOK_TYPE] == TOK_TYPE_ATOM

            # look ahead to second token after DEFINE
            try:
                macro_value_tok = next(input_tokens_iter)
            except StopIteration:
                raise Exception(
                    "%s ended with uncompleted DEFINE" % tok_filename
                )

            # enforce second token after DEFINE atom must be atom or string
            if macro_value_tok[TOK_TYPE] == TOK_TYPE_NEWLINE:
                raise Exception(
                    "You can not have a newline in a DEFINE "
                    "line %s from %s" % (
                        tok_expr, tok_linenum, tok_filename) )

            # make a macro type token which has a two element tuple
            # of name token and value token as the TOK_EXPR component
            yield (
                TOK_TYPE_MACRO,
                (macro_name_tok, macro_value_tok),
                tok_filename, tok_linenum
            )
        # else any atom token that's not DEFINE and two tokens after it
        # or any str or newline token, we just pass it through
        else:
            yield tok

# This function was copy-pasted from M1.py in
# https://github.com/markjenkins/knightpies
# revision e10fbd920ae4cf7b4b29c60986d0bab9993aef84
#
# This redundancy can be cleaned up once knightpies reaches maturity and is
# merged into the stage0 project
#
# Doing fancy things like a git submodule and manipulating sys.path
# was not considered worth it for this small amount of borrowed code
def get_macros_defined_and_add_to_sym_table(f, symbols=None):
    # start a new dictionary if one wasn't provided, putting this in the
    # function definition would cause there to be one dictionary at build time
    if symbols == None:
        symbols = {}

    for tok in upgrade_token_stream_to_include_macro(tokenize_file(f)):
        if tok[TOK_TYPE] == TOK_TYPE_MACRO:
            tok_type, tok_expr, tok_filename, tok_linenum = tok
            macro_name = tok_expr[MACRO_NAME][TOK_EXPR]
            if macro_name in symbols:
                raise MultipleDefinitionsException(
                    "DEFINE %s on line %s of %s is a duplicate definition"
                    % (macro_name, tok_linenum, tok_filename) )
            symbols[macro_name] = tok_expr[MACRO_VALUE]
    return symbols

# END globals, classes, and functions imported from knightpies M1.py

# Everything below is the unique code of disasm.py

def filter_M1_py_symbol_table_to_simple_dict(symbols):
    return {
        macro_name: macro_detailed_definition[TOK_EXPR]
        for macro_name, macro_detailed_definition in symbols.items()
    }

def get_macro_definitions_from_file(definitions_file):
    with open(definitions_file) as f:
        symbols = get_macros_defined_and_add_to_sym_table(f)
    return filter_M1_py_symbol_table_to_simple_dict(symbols)

def get_stage0_knight_defs_filename():
    return path_join(dirname(__file__), 'defs')

if __name__ == "__main__":
    STAGE0_KNIGHT_DEFS = get_stage0_knight_defs_filename()
    print(get_macro_definitions_from_file(STAGE0_KNIGHT_DEFS))
