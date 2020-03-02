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
        self.iterator = iterator
        self.buffer = deque()

    def grow_buffer(self, n):
        if len(self.buffer) >= n:
            return True
        else:
            while len(self.buffer) < n:
                try:
                    self.buffer.append( next(self.iterator) )
                except StopIteration:
                    break
            return len(self.buffer) >= n

    def next_n(self, n=1, grow=True):
        if grow:
            self.grow_buffer(n)
        for i in range( min(n, len(self.buffer) ) ):
            try:
                yield self.buffer.popleft()
            except IndexError:
                break

    def __next__(self):
        # support python's builtin next()
        #
        # raises StopIeteration if there is nothing left or a default value
        # if one additional argument is provided
        # checking for len(args) == 1 ensures we add at most one extra
        # argumet to python's builtin next()
        #
        # we do it this way instead of self.buffer.popleft() to ensure
        # StopIteration is thrown
        #
        # cool fact, callers (who are using builtin next() ) can
        # provide a default value
        return next(iter(self.next_n(n=1, grow=True)))

    def __len__(self):
        return len(self.buffer)

    def return_iterables_to_front(self, *iters):
        for iterable in reversed(iters):
            self.buffer.extendleft(reversed(iterable))

def num_nybles_from_immediate(lookup_struct):
    return (0 if None == lookup_struct[INSTRUCT_IMMEDIATE_NYBLE_LEN]
            else lookup_struct[INSTRUCT_IMMEDIATE_NYBLE_LEN])

def num_nybles_from_register_operands_and_immediate(lookup_struct):
    return (
        lookup_struct[INSTRUCT_NUM_REG_OPERANDS] +
        num_nybles_from_immediate(lookup_struct)
    ) # end addition expression

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

def get_knight_instruction_structure_from_file(definitions_file):
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

    return {
        prefix:
        expand_instruct_struct_define_if_valid(
            prefix,
            instruct_struct_define,
            instruction_pairs_per_prefix[prefix]
        ) # expand_instruct_struct_define_if_valid
        for prefix, instruct_struct_define in INSTRUCTION_STRUCTURE.items()
        }

NY_ANNO_IS_DATA, NY_ANNO_ADDRESS, NY_ANNO_FIRST_NYBLE = range(3)

def annotate_nyble_as_data(nyble_annotations):
    return (nyble_annotations[0:NY_ANNO_IS_DATA] +
            (True, ) + # NY_ANNO_IS_DATA
            nyble_annotations[NY_ANNO_IS_DATA+1:] )

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

    # the act of getting the next annotated nyble is first to look at the
    # lookahead_buffer for ones we oops on, otherwise pull from hex_nyble_stream
    # which could raise StopIteration if we reach end of file / stream
    def get_next_nyble():
        return next(lookahead_buffer)

    def try_to_consume_n_nybles(n):
        success = lookahead_buffer.grow_buffer(n)
        if success:
            return True, tuple(lookahead_buffer.next_n(n, grow=False))
        else:
            return False, None

    while True:
        try:
            (nyble, nyble_annotations) = get_next_nyble()
        except StopIteration:
            break # while True

        if nyble_annotations[NY_ANNO_IS_DATA]:
            yield (nyble, nyble_annotations)
        else:
            def return_first_nyble_as_data():
                return (nyble, annotate_nyble_as_data(nyble_annotations) )
            try:
                second_nyble, second_nyble_annotations = get_next_nyble()
            except StopIteration:
                yield return_first_nyble_as_data()
                break # while True

            def return_first_and_second_nyble_as_data():
                """provides a two element tuple with the first nyble
re-annotated as data and the second nyble re-annotated as data
recommended use:
yield from return_first_and_second_nyble_as_data()
which is equivilent to
first, second = return_first_and_second_nyble_as_data()
yield first
yield second"""
                return (
                    return_first_nyble_as_data(),
                    (second_nyble,
                     annotate_nyble_as_data(second_nyble_annotations))
                    ) # tuple

            instruction_prefix = (nyble + second_nyble).upper()
            if instruction_prefix not in instruction_structure:
                yield from return_first_and_second_nyble_as_data()
            else:
                instruction_struc_table = instruction_structure[
                    instruction_prefix]

                result, additional_nybles = try_to_consume_n_nybles(
                    instruction_struc_table[INSTRUCT_NYBLES_AFT_PREFIX]
                ) # try_to_consume_n_nybles
                # if we hit end of file, we treat the two nyble prefix
                # as data we'll go back to the top of the while loop
                # try_to_consume_n_nybles will have already put
                # the ones it did consume into lookahead_buffer
                if not result:
                    yield from return_first_and_second_nyble_as_data()
                else:
                    additional_nybles_hex = ''.join(
                        content
                        for content, additional_nyble_annotations in
                        additional_nybles
                        ).upper()
                    remaining_nybles_lookup_table = instruction_struc_table[
                        INSTRUCT_SHARED_PREFIX_LOOKUP]
                    # if the additional nybles
                    if (additional_nybles_hex not in
                        remaining_nybles_lookup_table):
                        yield from return_first_and_second_nyble_as_data()
                        # put the additional nybles back into our lookahead
                        # buffer to be consumed by next iteration of while True
                        lookahead_buffer.return_iterables_to_front(
                            additional_nybles)
                    else:
                        result, operand_nybles_consumed = \
                            try_to_consume_n_nybles(
                                num_nybles_from_register_operands_and_immediate(
                                    instruction_struc_table)
                            ) # try_to_consume_n_nybles
                        if not result:
                            yield from return_first_and_second_nyble_as_data()
                            # put the additional nybles back into our lookahead
                            # buffer to be consumed by next iteration of
                            # while True
                            lookahead_buffer.return_iterables_to_front(
                                additional_nybles,
                                operand_nybles_consumed)
                        else:
                            yield construct_annotated_instruction(
                                instruction_structure,
                                instruction_prefix,
                                additional_nybles_hex,
                                operand_nybles_consumed,
                                first_nyble_annotations=nyble_annotations)

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
    if definitions_file==None:
        definitions_file = get_stage0_knight_defs_filename()
    instruction_structure = get_knight_instruction_structure_from_file(
        definitions_file)

    AFTER_DATA_CHARS = "'\n"
    last_was_data = False
    last_was_instruct = False
    for content, annotations in replace_instructions_in_hex_nyble_stream(
            binary_to_annotated_hex(binary_fileobj),
            instruction_structure
            ):
        if annotations[NY_ANNO_IS_DATA]:
            if ( ( ( annotations[NY_ANNO_ADDRESS]) % 4 == 0 and
                   annotations[NY_ANNO_FIRST_NYBLE] ) or last_was_instruct):
                if annotations[NY_ANNO_ADDRESS]>0 and not last_was_instruct:
                    output_fileobj.write(AFTER_DATA_CHARS)
                output_fileobj.write( "'" )
            output_fileobj.write( content )
            last_was_data = True
            last_was_instruct = False
        else: # if not data, it's an instruction!
            if last_was_data:
                output_fileobj.write(AFTER_DATA_CHARS)

            output_fileobj.write( content )
            output_fileobj.write( "\n" )
            last_was_data = False
            last_was_instruct = True

    if last_was_data:
        output_fileobj.write(AFTER_DATA_CHARS)

def get_stage0_knight_defs_filename():
    return path_join(dirname(__file__), 'defs')

if __name__ == "__main__":
    with open(argv[1], 'rb') as f:
        dissassemble_knight_binary(f, stdout)
