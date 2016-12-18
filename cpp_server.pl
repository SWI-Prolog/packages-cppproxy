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

:- module(cpp_server,
          [ cpp_server/1                % +Options
          ]).
:- use_module(library(socket)).
:- use_module(library(debug)).
:- use_module(library(shlib)).
:- use_module(cpp_interface).
:- use_module(typedef).

%:- debug(cpp(_)).


                 /*******************************
                 *        BASIC READ/WRITE      *
                 *******************************/

:- use_foreign_library(foreign(serialize)).


                 /*******************************
                 *             SERVER           *
                 *******************************/

%       cpp_server(+Options)
%
%       Run a CPP server in a seperate thread

cpp_server(Options) :-
    option(port(Port), Options, 4224),
    tcp_socket(Socket),
    tcp_setopt(Socket, reuseaddr),
    tcp_bind(Socket, Port),
    tcp_listen(Socket, 5),
    thread_create(cpp_accept(Socket, Options), _,
                  [ alias(cpp_accept),
                    local(32),
                    global(32),
                    trail(32)
                  ]).

cpp_accept(Socket, Options) :-
    repeat,
    tcp_accept(Socket, Slave, Peer),
    debug(cpp(connection), 'Connect from ~p', [Peer]),
    thread_create(cpp_server(Slave, Options), _,
                  [ detached(true)
                  ]),
    fail.

cpp_server(Socket, _Options) :-
    tcp_open_socket(Socket, Read, Write),
    repeat,
        (   catch(at_end_of_stream(Read), _, true)
        ->  !,
            close(Read),
            close(Write)
        ;   handle_call(Read, Write, false),
            fail
        ).


                 /*******************************
                 *            CALL-IO           *
                 *******************************/

%       handle_call(+In, +Out, +Q)
%
%       Handle a call from a remote server, reading from In and writing
%       to Out.

handle_call(In, Out, Q) :-
    catch(handle_call2(In, Out, Q), E,
          reply(communication_error(E), Out)).

handle_call2(In, Out, Q) :-
    read_query_open(Q, In),
    read_goal(In, Module, Goal, Types, Attributes),
    det_goal(Attributes, Det),
    debug(cpp(call), 'call: ~p', [Goal]),
    (   catch(Module:Goal, Error, true),
        (   nonvar(Error)
        ->  debug(cpp(call), 'error: ~p --> ~p', [Goal, Error]),
            reply(error(Error), Out), !
        ;   debug(cpp(call), 'exit: ~p', [Goal]),
            reply(true(Module, Goal, Types, Det), Out),
            read_cont(Det, In, Out, Cont),
            Cont == done, !
        )
    ;   debug(cpp(call), 'fail', []),
        reply(false, Out)
    ).

%       read_query_open(+Done, +In)
%
%       Queries start with a 'q' byte. `Done' indicates whether this has
%       been read already. If true we skip this step. Otherwise we throw
%       an error of the next byte is not 'q'.

read_query_open(true, _) :- !.
read_query_open(false, In) :-
    get_byte(In, Byte),
    Byte == 0'q, 
    !.         % '
read_query_open(_, _) :-
    throw(error(communication_error(out_of_sync), _)).

det_goal(Attributes, Det) :-
    (   memberchk(zero_or_more, Attributes)
    ->  Det = false
    ;   Det = true
    ).

%       read_goal(+In, -Module, -Goal, -Types, -Attributes)
%
%       Read the next goal to execute from the input.  Goals are written
%       on the stream as
%
%               <module><name><arity><input arguments>
%
%       Types specifies the type vector to be   used for the goal. It is
%       returned as we need it after  executing   the  goal to write the
%       output arguments.

read_goal(In, Module, Goal, Types, Attributes) :-
    read_atom(In, Module),
    read_atom(In, NameOrAlias),
    read_int32(In, Arity),
    (   functor(Types, NameOrAlias, Arity),
        current_cpp_callable(Module, Types, Attributes)
    ->  Name = NameOrAlias,
        debug(cpp(call), 'pred: ~p:~p/~d', [Module, Name, Arity])
    ;   current_cpp_callable(Module, Types, Attributes),
        memberchk(as(NameOrAlias), Attributes)
    ->  functor(Types, Name, Arity),
        debug(cpp(call), 'pred: ~p:~p/~d as ~p',
              [Module, Name, Arity, NameOrAlias])
    ),
    functor(Goal, Name, Arity),
    input_args(0, Arity, In, Module, Types, Goal).

input_args(Arity, Arity, _, _, _, _) :- !.
input_args(I0, Arity, In, Module, Types, Goal) :-
    I is I0 + 1,
    arg(I, Types, Type),
    arg(I, Goal, Arg),
    input_arg(Type, In, Module, Arg),
    input_args(I, Arity, In, Module, Types, Goal).


%       read_arg(+ArgSpec, +In, +Module, -Arg)
%
%       Read an argument.  Only +Type arguments must be filled.

input_arg(+Type, In, Module, Arg) :-
    !,
    read_type(Type, In, Module, Arg).
input_arg(-_, _, _, _).


%       read_cont(+Det, +In, +Out, -Continuation)
%
%       See whether the client wants another answer or is satisfied.  If
%       the reply is non-deterministic, inserting `q' starts a new query
%       (like entering a break from the toplevel).

read_cont(true, _, _, done) :- !.
read_cont(Det, In, Out, Cont) :-
    get_byte(In, Byte),
    (   Byte == 0'!
    ->  Cont = done
    ;   Byte == 0';
    ->  Cont = more
    ;   Byte == 0'q
    ->  handle_call(In, Out, true),
        read_cont(Det, In, Out, Cont)
    ).


                 /*******************************
                 *        LOW-LEVEL READ        *
                 *******************************/

%       read_type(+Type, +In, +Module, -Term)
%
%       Read data of given type from the input stream In.

read_type(Type, In, Module, Term) :-
    debug(cpp(read), 'Reading type ~w', [Type]),
    current_type(Module:Type, Types),
    debug(cpp(read), 'Reading type ~w --> ~w', [Type, Types]),
    get_byte(In, C0),
    debug(cpp(read), 'C0 = ~c', [C0]),
    read_type2(C0, Types, In, Module, Term),
    debug(cpp(read), 'read_type() --> ~p', [Term]).

read_type2(0'i, Types, In, _, Int) :-
    !,
    must_include_type(integer, Types),
    read_int32(In, Int).
read_type2(0'f, Types, In, _, Int) :-
    !,
    must_include_type(float, Types),
    read_float(In, Int).
read_type2(0'a, Types, In, _, Atom) :-
    !,
    must_include_type(atom, Types),
    read_atom(In, Atom).
read_type2(0'c, Types, In, Module, Term) :-
    read_atom(In, Name),
    read_int32(In, Arity),
    functor(TermSpec, Name, Arity),
    must_include_type(TermSpec, Types),
    functor(Term, Name, Arity),
    read_args(0, Arity, In, Module, TermSpec, Term).

read_args(Arity, Arity, _, _, _, _) :- !.
read_args(I0, Arity, In, Module, Types, Goal) :-
    I is I0 + 1,
    arg(I, Types, Type),
    arg(I, Goal, Arg),
    read_type(Type, In, Module, Arg),
    read_args(I, Arity, In, Module, Types, Goal).

must_include_type(Type, Types) :-
    memberchk(Type, Types), 
    !.
must_include_type(Type, Types) :-
    throw(error(type_error(Types, Type), _)).


                 /*******************************
                 *             REPLIES          *
                 *******************************/

reply(false, Out) :-
    debug(cpp(reply), 'Reply: fail', []),
    write(Out, 'f'),
    flush_output(Out).
reply(true(Module, Goal, Types, Det), Out) :-
    write_det(Det, Out),
    functor(Goal, Name, Arity),
    functor(Types, Name, Arity),
    output_args(0, Arity, Out, Module, Types, Goal),
    flush_output(Out).
reply(error(Term), Out) :-
    message_to_string(Term, Msg),
    debug(cpp(reply), 'Reply: error: ~w', [Msg]),
    write(Out, 'e'),
    write_atom(Out, Msg),
    flush_output(Out).
reply(communication_error(Term), Out) :-
    message_to_string(Term, Msg),
    debug(cpp(reply), 'Reply: system error: ~w', [Msg]),
    write(Out, 'E'),
    write_atom(Out, Msg),
    flush_output(Out).

write_det(true, Out) :-
    write(Out, 'l').                % no more answers
write_det(false, Out) :-
    write(Out, 'm').                % more answers left


output_args(Arity, Arity, _, _, _, _) :- !.
output_args(I0, Arity, In, Module, Types, Goal) :-
    I is I0 + 1,
    arg(I, Types, Type),
    arg(I, Goal, Arg),
    output_arg(Type, In, Module, Arg),
    output_args(I, Arity, In, Module, Types, Goal).

%       output_arg(+ArgSpec, +Out, +Module, +Arg)
%
%       Read an argument.  Only +Type arguments must be filled.

output_arg(-Type, Out, Module, Arg) :-
    !,
    write_type(Type, Out, Module, Arg).
output_arg(+_, _, _, _).


write_type(Type, Out, Module, Term) :-
    current_type(Module:Type, Types),
    write_type2(Types, Out, Module, Term).

write_type2(Types, Out, _, Int) :-
    integer(Int),                  % TBD: auto conversion
    !,
    debug(cpp(write), 'Write int: ~D', [Int]),
    must_include_type(integer, Types),
    put_byte(Out, 0'i),
    write_int32(Out, Int).
write_type2(Types, Out, _, Float) :-
    float(Float),                  % TBD: auto conversion
    !,
    debug(cpp(write), 'Write float: ~f', [Float]),
    must_include_type(float, Types),
    put_byte(Out, 0'f),
    write_float(Out, Float).
write_type2(Types, Out, _, Atom) :-
    atom(Atom),
    !,
    debug(cpp(write), 'Write atom: ~p', [Atom]),
    must_include_type(atom, Types),
    put_byte(Out, 0'a),
    write_atom(Out, Atom).
write_type2(Types, Out, Module, Term) :-
    compound(Term),
    !,
    functor(Term, Name, Arity),
    debug(cpp(write), 'Write compound: ~w/~d', [Name, Arity]),
    functor(TermSpec, Name, Arity),
    must_include_type(TermSpec, Types),
    put_byte(Out, 0'c),
    write_atom(Out, Name),
    write_int32(Out, Arity),
    write_args(0, Arity, Out, Module, TermSpec, Term).

write_args(Arity, Arity, _, _, _, _) :- !.
write_args(I0, Arity, Out, Module, Types, Goal) :-
    I is I0 + 1,
    arg(I, Types, Type),
    arg(I, Goal, Arg),
    write_type(Type, Out, Module, Arg),
    write_args(I, Arity, Out, Module, Types, Goal).
