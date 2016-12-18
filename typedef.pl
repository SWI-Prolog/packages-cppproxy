/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2011, University of Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(typedef,
          [ current_type/2,             % :Type, -Expanded
            (type)/1,                   % Catch calls

            op(1199, fx, type)
          ]).

:- meta_predicate
    current_type(:, -).

                 /*******************************
                 *          EXPANSION           *
                 *******************************/

type(_) :-
    throw(error(context_error(directive), _)).

expand_type_term(:- type TypeDecl, Clauses) :-
    phrase(type_clauses(TypeDecl), Clauses).

type_clauses(Name=Alias) -->
    [ 'type alias'(Name, Alias) ].
type_clauses(Name->Decl) -->
    { or_to_list(Decl, List)
    },
    [ 'type def'(Name, List)
    ].

or_to_list(A|B0, [A|B]) :-
    !,
    or_to_list(B0, B).
or_to_list(A, [A]).

:- multifile
    user:term_expansion/2.

user:term_expansion((:- type TypeDecl), Clauses) :-
    expand_type_term((:- type TypeDecl), Clauses).


                 /*******************************
                 *            QUERY             *
                 *******************************/

%       current_type(:Name, -Expanded)
%
%       Do alias expansion of the  type   Name  and  return the expanded
%       types. Expanded is a list of allowed types (polymorphism).

current_type(NameSpec, Expanded) :-
    strip_module(NameSpec, Module, Name),
    current_type(Module, Name, Expanded).

current_type(Module, Name, Expanded) :-
    current_predicate(_, Module:'type alias'(_, _)),
    Module:'type alias'(Name, Alias),
    !,
    current_type(Module, Alias, Expanded).
current_type(_, Primitive, [Primitive]) :-
    primitive(Primitive), 
    !.
current_type(Module, Name, Expanded) :-
    Module:'type def'(Name, Expanded).


%       primitive(?Type)
%
%       True if type is a primitive type

primitive(integer).
primitive(atom).
primitive(float).
primitive(var).
