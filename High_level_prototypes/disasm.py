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

from __future__ import division

from os.path import dirname, join as path_join
from binascii import hexlify
from sys import argv, stdout
from collections import deque
from itertools import count

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

NUM_REGISTERS = 16

KNIGHT_REGISTER_SYMBOLS = {
    'R%d' % i: i
    for i in range(NUM_REGISTERS)
    }

(INSTRUCT_NYBLES_AFT_PREFIX,
 INSTRUCT_NUM_REG_OPERANDS,
 INSTRUCT_IMMEDIATE_NYBLE_LEN,
 INSTRUCT_SHARED_PREFIX_LOOKUP,
) = range(4)

INSTRUCTION_STRUCTURE = {
    '01': (2, 4, None), # 4 OP Integer Group
    '05': (3, 3, None), # 3 OP Integer Group
    '09': (4, 2, None), # 2 OP Integer Group
    '0D': (5, 1, None), # 1 OP Group
    'E1': (4, 2, 4),    # 2 OP Immediate Group
    'E0': (5, 1, 4),    # 1 OP Immediate Group
    '3C': (2, 0, 4),    # 0 OP Immediate Group
    '42': (6, 0, None), # HALCODE Group
    '00': (6, 0, None), # 0 OP Group '00' prefix
    'FF': (6, 0, None), # 0 OP Group 'FF' prefix
    }

INSTRUCTION_PREFIX_LEN = 2
if __debug__:
    for key in INSTRUCTION_STRUCTURE.keys():
        assert( len(key) == INSTRUCTION_PREFIX_LEN )

class InvalidInstructionDefinitionException(Exception):
    def __init__(self, instruct_name, instruct_hex, msg):
        Exception.__init__(self,
            "definition for %s, %s %s" % (
                instruct_name, instruct_hex, msg))

class LookaheadBuffer(object):
    def __init__(self, iterator):
        self.__iterator = iterator
        self.__buffer = deque()

        # anyone LookaheadBuffer internal calling next() on self.__iterator
        # is responsible for catching StopIteration and setting this
        # So far this is __next__, grow_buffer, and grow_by_predicate
        self.__hit_end = False

    def hit_end(self):
        """If we hit StopIteration on the underlying iterator.
        But this doesn't mean the buffer is empty, callers should
        check unless it's clear from the context, even then, documenting
        LookaheaddBuffer.__len__()==0 is a good idea
        """
        # anyone LookaheadBuffer internal calling next() on self.__iterator
        # is responsible for catching StopIteration and setting this
        # So far this is __next__, grow_buffer, and grow_by_predicate
        return self.__hit_end

    def grow_buffer(self, n):
        if len(self.__buffer) >= n:
            return True
        else:
            while len(self.__buffer) < n:
                try:
                    self.__buffer.append( next(self.__iterator) )
                except StopIteration:
                    self.__hit_end = True
                    break
            return len(self.__buffer) >= n

    def grow_by_predicate(self, predicate, n=None,
                          raise_if_not_clearfirst=True):
        """Grow the existing buffer by up to n amount as long as predicate
        is true on the elements. Or leave out n to just rely on the predicate.

        The item that fails the predicate will be the last in the buffer
        if any. (n or the underlying iterable running out could leave you
        with one that passes the predicate)

        Default assumption is you've already cleared the buffer first so that
        everything after this call matches your predicate. For your
        protection raise_if_not_clearfirst defaults to True, meaning
        you'll get an exception if the buffer isn't clear.

        If you're aware of elments already in the buffer and want to
        add more matching your predicate, set raise_if_not_clearfirst=False
        to say you know what you're doing.

        Returns a two element tuple(
            pred_last, # the last bool evaluation of predicate
                       # None if hit_end==True

            i,         # the number of elements added that matched the predicate
                       # 0 <= i <= n
                       # i==n implies hit_end==True or pred_last==True
        )

        You probably also want to check LookaheadBuffer.hit_end()
        as that should mean the predicate passed until the end and the
        last element of the buffer passes the predicate
        (though i < n would also be consistent this this)
        """
        # the reason we do is the assumption the caller will normally
        # have dealt with the buffer contents first and would be making
        # a mistake if for some reason clear and would be surprised
        # the front of buffer contents don't match the predicate
        if raise_if_not_clearfirst and len(self)>0:
            raise Exception(
                "grow_by_predicate called with content in buffer "
                "but raise_if_not_clearfirst flag set"
            )
        # these are only used in assert expressions, do don't
        # worry about them if we're in debug mode where assertions do nothing
        if __debug__:
            def i_n_assertion():
                return 0 <= i <= n
            def i_eq_n_assertion():
                return n!=i or self.hit_end() or pred_last
            def i_n_assertions():
                return i_n_assertion() and i_eq_n_assertion()

        for i in (count(0) if n==None else range(n)):
            try:
                next_in = next(self.__iterator)
                self.__buffer.append( next_in )
            except StopIteration:
                self.__hit_end  = True
                pred_last = None
                assert i_n_assertions()
                return pred_last, i

            if not predicate(next_in):
                pred_last = False
                assert i_n_assertions()
                return pred_last, i
        # only if the loop completes, n been exhausted and the predicate
        # passed on every element
        else:
            i+=1
            assert( i==n )
            pred_last = True
            assert i_n_assertions()
            return pred_last, i

    def next_n(self, n=1, grow=True, raise_if_too_small=True):
        if grow:
            self.grow_buffer(n)
        elif raise_if_too_small and len(self.__buffer) < n:
            raise Exception(
                "LookaheadBuffer too small, growth not allowed "
                "and error checking enabled")

        # important that we not return an iterator because
        # the user may subsequently call return_iterables_to_front()
        return tuple(
            self.__buffer.popleft()
            for i in range( min(n, len(self.__buffer) ) ) )

    def __iter__(self):
        # important that we make this based on a copy in case the user calls
        # return_iterables_to_front()
        return iter(self.as_tuple())

    def as_tuple(self):
        return tuple(self.__buffer)

    def clear(self, as_iter=False, as_tuple=False):
        if as_iter:
            old_buffer = self.__buffer
            self.__buffer = deque()
            return iter(old_buffer)
        elif as_tuple:
            return_tuple = tuple(self.__buffer)
            self.__buffer.clear()
            return return_tuple
        else:
            self.__buffer.clear()

    def __next__(self):
        # support python's builtin next()
        #
        # raises StopIteration if there is nothing left or a default value
        # if one additional argument is provided
        # checking for len(args) == 1 ensures we add at most one extra
        # argumet to python's builtin next()
        #
        # we do it this way instead of self.__buffer.popleft() to ensure
        # StopIteration is thrown
        #
        # cool fact, callers (who are using builtin next() ) can
        # provide a default value
        try:
            next_val = next(iter(self.next_n(n=1, grow=True)))
        except StopIteration:
            self.__hit_end = True
            raise # re raise same StopIteration
        else:
            return next_val

    def __len__(self):
        return len(self.__buffer)

    def return_iterables_to_front(self, *iters):
        for iterable in reversed(iters):
            self.__buffer.extendleft(reversed(iterable))

    def peek(self):
        if len(self.__buffer)==0:
            raise Exception(
                "peek() called when buffer is empty, you should check len()")
        next_val = next(iter(self.__buffer))
        return next_val

def num_nybles_from_immediate(lookup_struct):
    return (0 if None == lookup_struct[INSTRUCT_IMMEDIATE_NYBLE_LEN]
            else lookup_struct[INSTRUCT_IMMEDIATE_NYBLE_LEN])

def num_nybles_from_register_operands_and_immediate(lookup_struct):
    return (
        lookup_struct[INSTRUCT_NUM_REG_OPERANDS] +
        num_nybles_from_immediate(lookup_struct)
    ) # end addition expression

def get_instruction_opcode_len_after_prefix(lookup_struct):
    return lookup_struct[INSTRUCT_NYBLES_AFT_PREFIX]

def get_instruction_opcode_len(instruct_struct):
    return (
        INSTRUCTION_PREFIX_LEN +
        get_instruction_opcode_len_after_prefix(instruct_struct)
        )

def smallest_instruction_nybles(instruct_struct):
    return min(
        (get_instruction_opcode_len(instructfamily) +
         num_nybles_from_register_operands_and_immediate(instructfamily)
        )
        for prefix, instructfamily in instruct_struct.items() )

def filter_M1_py_symbol_table_to_simple_dict(symbols):
    return {
        macro_name: macro_detailed_definition[TOK_EXPR]
        for macro_name, macro_detailed_definition in symbols.items()
    }

def filter_unwanted_symbols(symbols, unwanted):
    return {
        key: value
        for key, value in symbols.items()
        if key not in unwanted
        }

def get_macro_definitions_from_file(definitions_file):
    with open(definitions_file) as f:
        symbols = get_macros_defined_and_add_to_sym_table(f)
    return filter_M1_py_symbol_table_to_simple_dict(symbols)

def get_knight_instruction_definititions_from_file(definitions_file):
    filtered_symbols = filter_unwanted_symbols(
        get_macro_definitions_from_file(definitions_file),
        set( (tuple(KNIGHT_REGISTER_SYMBOLS.keys()) + ('NULL',)) )
        )
    return {
        key: value.upper() # ensure instruction definitions are upper case hex
        for key, value in filtered_symbols.items()
        }

def remove_prefix_from_instruct_hex(instruct_hex):
    return instruct_hex[INSTRUCTION_PREFIX_LEN:]

def expand_instruct_struct_define_if_valid(
        prefix,
        instruct_struct_define,
        pairs_for_this_prefix):
    # validity check, nybles after prefix must be the right length
    for instruct_hex, instruct_name in pairs_for_this_prefix:
        instruct_hex_after_prefix = \
            remove_prefix_from_instruct_hex(instruct_hex)
        nybles_after_prefix = instruct_struct_define[
            INSTRUCT_NYBLES_AFT_PREFIX]
        if len(instruct_hex_after_prefix) != nybles_after_prefix:
            raise InvalidInstructionDefinitionException(
                instruct_name, instruct_hex,
                "does not have %d nybles after prefix (%s) had %d" % (
                    nybles_after_prefix,
                    instruct_hex_after_prefix,
                    len(instruct_hex_after_prefix))
                )
    return ( # start of tuple appending expression
        instruct_struct_define + # tuple appending operator
        ( # start of singleton tuple
            {
                remove_prefix_from_instruct_hex(instruct_hex):
                instruct_name
                for instruct_hex, instruct_name in pairs_for_this_prefix
            }
            ,) # end singleton tuple
    ) # end tuple appending expression

def get_knight_instruction_structure_from_file(
        definitions_file, strict_size_assert=False):
    symbols = get_knight_instruction_definititions_from_file(definitions_file)
    for symname, symvalue in symbols.items():
        if symvalue[0:INSTRUCTION_PREFIX_LEN] not in INSTRUCTION_STRUCTURE:
            raise InvalidInstructionDefinitionException(
                symname, symvalue, "has unknown prefix")

    instruction_pairs_per_prefix = {
        prefix: [
            (symvalue, symname)
            for symname, symvalue in symbols.items()
            if symvalue.startswith(prefix)
        ] # list comprehension
        for prefix in INSTRUCTION_STRUCTURE.keys()
        }

    finished_instruction_struct = {
        prefix:
        expand_instruct_struct_define_if_valid(
            prefix,
            instruct_struct_define,
            instruction_pairs_per_prefix[prefix]
        ) # expand_instruct_struct_define_if_valid
        for prefix, instruct_struct_define in INSTRUCTION_STRUCTURE.items()
        }
    if strict_size_assert:
        assert(
            smallest_instruction_nybles(finished_instruction_struct) ==
            max( len(x) for x in symbols.values()) )
    else:
        assert(
            smallest_instruction_nybles(finished_instruction_struct) <=
            max( len(x) for x in symbols.values()) )

    return finished_instruction_struct

NY_ANNO_IS_DATA, NY_ANNO_ADDRESS, NY_ANNO_FIRST_NYBLE = range(3)

def annotate_nyble_as_data(nyble_annotations):
    return (nyble_annotations[0:NY_ANNO_IS_DATA] +
            (True, ) + # NY_ANNO_IS_DATA
            nyble_annotations[NY_ANNO_IS_DATA+1:] )

def multiple_annotated_nybles_as_data(annotated_nybles):
    return (
        (x, annotate_nyble_as_data(y))
        for x,y in annotated_nybles
        )

def annotated_nyble_is_data(ny_annotated):
    nyble, annotations = ny_annotated
    return annotations[NY_ANNO_IS_DATA]

# this is a helper called by replace_instructions_in_hex_nyble_stream
# all asserts assume that context
def construct_annotated_instruction(
        instruction_structure,
        instruction_prefix,
        remainder_of_opcode_hex,
        operand_nybles_annotated,
        first_nyble_annotations,
):

    assert( instruction_prefix in instruction_structure )
    instruct_table = instruction_structure[instruction_prefix]

    remainder_opcode_table = instruct_table[INSTRUCT_SHARED_PREFIX_LOOKUP]
    assert(  remainder_of_opcode_hex in remainder_opcode_table )
    opcode_name = remainder_opcode_table[remainder_of_opcode_hex]
    opcode_fullhex = instruction_prefix + remainder_of_opcode_hex# string append

    immediate_len = num_nybles_from_immediate(instruct_table)
    if immediate_len>0:
        # negatives in python slices allow for getting last n
        # so first example here gets last n
        # and second example here is everything up to last n
        immediate_nybles = operand_nybles_annotated[-immediate_len:]
        immediate_nybles_string = ''.join(
            immediate_hex_nyble
            for immediate_hex_nyble, immediate_annotation in immediate_nybles)
        immediate_unsigned_value = int(immediate_nybles_string, 16)
        immediate_string = "0x%.4X" % immediate_unsigned_value
        reg_operand_nybles = operand_nybles_annotated[:-immediate_len]
    else:
        immediate_nybles = ()
        immediate_unsigned_value = None
        immediate_string = ''
        reg_operand_nybles = operand_nybles_annotated

    register_operands_string = ' '.join(
        "R%d" % int(operand_in_hex,16)
        for operand_in_hex, operand_annotations in reg_operand_nybles
        )

    full_instruction_string = (# start expression for string
        "%s %s %s" % (opcode_name, register_operands_string, immediate_string)
    ).strip() # strip() covers case of immediate_string==''

    return (full_instruction_string,
            (False, # NY_ANNO_IS_DATA, it's not data its an instruction!
             first_nyble_annotations[NY_ANNO_ADDRESS], # NY_ANNO_ADDRESS
             True, # NY_ANNO_FIRST_NYBLE, instructions start on byte boundary
            )
    )

def replace_instructions_in_hex_nyble_stream(
        hex_nyble_stream, instruction_structure):
    # any annotated nybles we pull from hex_nyble_stream might get
    # tossed into this lookahead buffer [first in first out /FIFO with
    # append() and popleft() ] if it turns out that oops, they were not what
    # we thought they were
    lookahead_buffer = LookaheadBuffer(hex_nyble_stream)

    minimal_instruction_size = smallest_instruction_nybles(
        instruction_structure)
    assert( INSTRUCTION_PREFIX_LEN < minimal_instruction_size )

    while True:
        if not lookahead_buffer.grow_buffer(minimal_instruction_size):
            assert( len(lookahead_buffer) < minimal_instruction_size )
            yield from multiple_annotated_nybles_as_data(
                lookahead_buffer.clear(as_iter=True))
            break

        prefix_nybles_w_annotations = lookahead_buffer.next_n(
            INSTRUCTION_PREFIX_LEN, grow=False)

        # if any of the prefix nybles are marked as data, they're both
        # treated as data
        if any( ny_annotations[NY_ANNO_IS_DATA]
                for nydata, ny_annotations in prefix_nybles_w_annotations ):
            yield from multiple_annotated_nybles_as_data(
                prefix_nybles_w_annotations)
        else:
            instruction_prefix = \
                ''.join(
                    nyble
                    for nyble, nyble_annotations in prefix_nybles_w_annotations
                ).upper()
            if instruction_prefix not in instruction_structure:
                yield from multiple_annotated_nybles_as_data(
                    prefix_nybles_w_annotations)
            else:
                instruction_struc_table = instruction_structure[
                    instruction_prefix]

                rest_of_opcode_len = get_instruction_opcode_len_after_prefix(
                    instruction_struc_table)
                operand_len = num_nybles_from_register_operands_and_immediate(
                    instruction_struc_table)

                result = lookahead_buffer.grow_buffer(
                    rest_of_opcode_len + operand_len)

                # if we hit end of file, we treat the two nyble prefix
                # as data we'll go back to the top of the while loop
                # the nybles still available will be in lookahead_buffer
                if not result:
                    yield from multiple_annotated_nybles_as_data(
                        prefix_nybles_w_annotations)
                else:
                    rest_of_opcode_nybles = lookahead_buffer.next_n(
                        rest_of_opcode_len, grow=False)

                    rest_of_opcode_nybles_hex = ''.join(
                        content
                        for content, additional_nyble_annotations in
                        rest_of_opcode_nybles
                        ).upper()
                    remaining_nybles_lookup_table = instruction_struc_table[
                        INSTRUCT_SHARED_PREFIX_LOOKUP]
                    # if the rest of the opcode isn't recognizable
                    if (rest_of_opcode_nybles_hex not in
                        remaining_nybles_lookup_table):
                        # treat the prefix as data
                        yield from multiple_annotated_nybles_as_data(
                            prefix_nybles_w_annotations)
                        # put the rest of opcode nybles back into the
                        # front of our lookahead buffer to be consumed by
                        # next iteration of while True
                        lookahead_buffer.return_iterables_to_front(
                            rest_of_opcode_nybles)
                    else:
                        # no need to grow the buffer to match this read
                        # or to check operand_len nybles are available as we
                        # already did a grow_buffer operation with
                        # both the remainder of opcode length + operand length
                        # and we checked the result
                        operand_nybles_consumed = lookahead_buffer.next_n(
                            operand_len, grow=False)
                        yield construct_annotated_instruction(
                            instruction_structure,
                            instruction_prefix,
                            rest_of_opcode_nybles_hex,
                            operand_nybles_consumed,
                            first_nyble_annotations =
                              prefix_nybles_w_annotations[0][1] )

def consolidate_data_into_chunks_in_hex_nyble_stream(
        hex_nyble_stream, n=4):
    if (n % 2)!=0:
        raise Exception(
            "consolidate_data_into_chunks_in_hex_nyble_stream can only do it "
            "in multiples of 2, e.g. two nybles per byte")

    lookahead_buffer = LookaheadBuffer(hex_nyble_stream)

    MAX_DATA_NYBLES_PER_LINE = n

    while len(lookahead_buffer)>0 or not lookahead_buffer.hit_end():
        # anything left over in the lookahead buffer is not data
        # because we didn't handle below after calling grow_by_predicate
        if len(lookahead_buffer)>0:
            assert( len(lookahead_buffer) == 1 ) # no reason for many
            # next item in buffer is some kind of
            # assembler plain text instead of data.
            # pass it through
            yield next(lookahead_buffer)
            assert( len(lookahead_buffer) == 0 )
            continue

        pred_last, num_data = lookahead_buffer.grow_by_predicate(
            annotated_nyble_is_data,
            MAX_DATA_NYBLES_PER_LINE)
        if num_data > 0: # if we found some data
            first_data_nyble_annotated = lookahead_buffer.peek()
            first_data_nyble, first_data_nyble_annotations = \
                first_data_nyble_annotated

            # put it in single quotes as one chunk
            yield (
                "'%s'" % ''.join( # no characters between data
                    nyble
                    for nyble, annotations in lookahead_buffer.next_n(
                            num_data, grow=False)
                ), # join
                first_data_nyble_annotations
            ) # tuple

            # hitting the end means we didn't stop because the predicate
            # failed, it means we ran out of data at the end of the file
            if lookahead_buffer.hit_end():
                assert num_data < MAX_DATA_NYBLES_PER_LINE
                assert pred_last==None
                break # technically the while loop variant has this covered
            else:
                # if we didn't hit the end, either we loaded enough
                # data or the predicate stopped us, either way, go back
                # to the top of the while loop to process more
                assert ( num_data==MAX_DATA_NYBLES_PER_LINE or
                         ( not pred_last and len(lookahead_buffer)==1 )
                ) # assert expression
                continue # redundant, we're headed back to top of while loop

        # num_data must be 0
        #
        # if we also hit the end it means we're at end of file,
        # because that means grow_by_predicate wasn't stopped by the
        # predicate
        elif lookahead_buffer.hit_end():
            assert num_data==0 # implied by num_data > 0 testing failing
            assert len(lookahead_buffer)==0
            break # redundant because the while loop invarient covers this
        # but if we're not at the end
        elif not pred_last:
            assert len(lookahead_buffer)==1
            # we'll handle the one item at the top of the loop

def binary_to_annotated_hex(binary_fileobj):
    # nyble is int when iterating over bytes from hexlify, hence chr(nyble)
    for i, nyble in enumerate(hexlify(binary_fileobj.read())):
        yield (chr(nyble).upper(),
               (False,    # NY_ANNO_IS_DATA
                i//2,     # NY_ANNO_ADDRESS
                (i%2==0), # NY_ANNO_FIRST_NYBLE
               ) # annotation tuple
        ) # outer tuple

def dissassemble_knight_binary(
        binary_fileobj,
        output_fileobj,
        definitions_file=None,
        ):
    builtin_definitions = definitions_file==None

    if builtin_definitions:
        definitions_file = get_stage0_knight_defs_filename()
    instruction_structure = get_knight_instruction_structure_from_file(
        definitions_file, strict_size_assert=builtin_definitions)

    # we know the smallest
    if builtin_definitions:
        assert( 8 == smallest_instruction_nybles(instruction_structure))

    nyble_stream = binary_to_annotated_hex(binary_fileobj)

    after_instruction_replacement_stream = \
        replace_instructions_in_hex_nyble_stream(
            nyble_stream,
            instruction_structure
        ) # replace_instructions_in_hex_nyble_stream

    MAX_DATA_NYBLES_PER_LINE = 4 # configurable in a future version
    final_stream = consolidate_data_into_chunks_in_hex_nyble_stream(
        after_instruction_replacement_stream,
        n=MAX_DATA_NYBLES_PER_LINE)

    for content, annotations in final_stream:
        output_fileobj.write(content)
        output_fileobj.write("\n")

def get_stage0_knight_defs_filename():
    return path_join(dirname(__file__), 'defs')

if __name__ == "__main__":
    with open(argv[1], 'rb') as f:
        dissassemble_knight_binary(f, stdout)
