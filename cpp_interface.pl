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

:- module(cpp_interface,
          [ current_cpp_callable/1,     % :Goal
            current_cpp_callable/3,     % ?Module, ?Goal, ?Options
            current_cpp_type/3,         % ?Module, ?CPPClass, ?Functor

            (cpp_callable)/1,           % Define for errors
            (cpp_type)/1,                       % Define for errors

            op(1199, fx, cpp_type),
            op(1015, fx, cpp_callable),
            op(100,  xf, non_det),
            op(100,  xf, det)
          ]).

:- meta_predicate
    current_cpp_callable(:).

%       :- cpp_callable Goal1, Goal2, ...
%
%       Declare Goal1, ... to  be  callable   from  C++.  Callable goals
%       consist of a head, specifying the argument types and optional an
%       " = Options", where  Options  is   a  list  of  options. Defined
%       options are:
%
%               * one
%               Predicate always succeeds.  This is the default.  Such
%               predicates are mapped to void C++ functions.
%
%               * zero_or_one
%               Predicate succeeds or fails.
%
%               * zero_or_more
%               Predicate is non-deterministic.
%
%               * as(Name)
%               Name to give to the C++ function or Query class.

expand_callable((A,B)) -->
    !,
    expand_callable(A),
    expand_callable(B).
expand_callable(Head = Attrs) -->
    [ 'cpp callable'(Head, Attrs)
    ].
expand_callable(Head) -->
    [ 'cpp callable'(Head, [one])
    ].

expand_type((A,B)) -->
    !,
    expand_type(A),
    expand_type(B).
expand_type(CPP=Prolog) -->
    [ 'cpp type'(CPP, Prolog)
    ].


cpp_callable(_) :-
    throw(error(context_error(directive), _)).
cpp_type(_) :-
    throw(error(context_error(directive), _)).

:- multifile
    user:term_expansion/2.

user:term_expansion((:- cpp_callable Spec), Clauses) :-
    phrase(expand_callable(Spec), Clauses).
user:term_expansion((:- cpp_type Spec), Clauses) :-
    phrase(expand_type(Spec), Clauses).

current_cpp_callable(Spec) :-
    strip_module(Spec, Module, Goal),
    current_cpp_callable(Module, Goal, _).
current_cpp_callable(Module, Goal, Attributes) :-
    predicate_property(Module:'cpp callable'(_,_), interpreted),
    Module:'cpp callable'(Goal, Attributes).

current_cpp_type(Module, CPP, Prolog) :-
    predicate_property(Module:'cpp type'(_,_), interpreted),
    Module:'cpp type'(CPP, Prolog).
