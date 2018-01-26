-module(example_record).

-export([a/0]).

-record(a,
        {any  %% multi-line is legal
         :: %% This won't get a line
         any(),                 %% The top type, the set of all Erlang terms
         none :: none(),                %% The bottom type, contains no terms
         pid :: pid(),
         port :: port(),
         reference :: reference(),
         nil :: [],                    %% nil
         nil2 = [] :: [],                    %% nil
         atom :: atom(),
         atom_raw :: foo,
         atom_union :: foo | bar, %% union
         bitstring_empty :: <<>>,
         bitstring_count :: <<_:1>>,
         bitstring_rep_count :: <<_:_*2>>,
         bitstring_count_rep_count :: <<_:1, _:_*2>>,
         float :: float(),
         fun_empty :: fun(),                  %% any function
         fun_any_arity :: fun((...) -> any()),     %% any arity, returning Type
         fun_product_empty :: fun(() -> none()), %% the () becomes 'product'
         fun_product1 :: fun((pid()) -> port()), %% I don't know why an argument list is called a product
         fun_product2 :: fun((pid(), maybe_improper_list()) -> port()),
         integer1 :: integer(),
         integer2 = 1 :: integer(),
         integer3 :: 1,                    %% ..., -1, 0, 1, ... 42 ...
         integer4 :: -1..1,    %% specifies an integer range
         list1 :: list(), %% [any()]
         list2 :: list(reference()),                           %% Proper list ([]-terminated)
         list3 :: maybe_improper_list([], atom()),    %% Type1=contents, Type2=termination
         list4 :: nonempty_improper_list(foo, <<>>), %% Type1 and Type2 as above
         list5 :: nonempty_list(<<_:1>>),                  %% Proper non-empty list
         list6 :: [_],                  %% Proper non-empty list
         list7 :: [any()],                  %% Proper non-empty list
         list8 = [1, b, "c", <<"d">>, [2.0, {}]],                  %% Proper non-empty list
         map1 :: map(),                                 %% denotes a map of any size
         map2 :: #{},                                   %% denotes the empty map
         map3 :: #{float() := fun()},
         map4 :: #{integer() => list()},
         map5 :: #{map() := tuple()},
         map6 :: #{term() := any(), binary() => bitstring()},
         tuple1 :: tuple(),                             %% denotes a tuple of any size
         tuple2 :: {},
         tuple3 :: {boolean()},
         tuple4 :: {boolean(), byte()},
         union :: char() | nil(),
         %% user_defined :: %% TODO          %% described in Type Declarations of User-Defined Types
         term :: term(),	%% any()
         binary :: binary(), %% :: <<_:_*8>>,
         bitstring :: bitstring(), %% <<_:_*1>>,
         boolean :: boolean(), %% 'false' | 'true'
         byte :: byte(), %% 0..255
         char ::	char(), %% 0..16#10ffff
         number :: number(),	%% integer() | float()
         maybe_improper_list :: maybe_improper_list(), %% maybe_improper_list(any(), any())
         nonempty_list :: nonempty_list(), %% nonempty_list(any())
         string :: string(), %% [char()]
         nonempty_string :: nonempty_string(), %% [char(),...]
         iodata :: iodata(), %% iolist() | binary()
         iolist :: iolist(), %% maybe_improper_list(byte() | binary() | iolist(), binary() | [])
         function :: function(), %% fun()
         module :: module(), %% atom()
         mfa :: mfa(), %% {module(),atom(),arity()}
         arity :: arity(), %% 0..255
         identifier :: identifier(), %% pid() | port() | reference()
         node :: node(), %% atom()
         timeout :: timeout(), %% 'infinity' | non_neg_integer()
         no_return :: no_return(), %% none()
         non_neg_integer :: non_neg_integer(), %% 0..
         pos_integer :: pos_integer(), %% 1..
         neg_integer :: neg_integer(), %% ..-1
         nonempty_improper_list :: nonempty_improper_list(tuple(), byte()), %% nonempty_improper_list(Type1, Type2)
         nonempty_maybe_improper_list1 :: nonempty_maybe_improper_list(), %% :: nonempty_maybe_improper_list(any(), any())
         nonempty_maybe_improper_list2 :: nonempty_maybe_improper_list(byte(), char())}).

a() ->
    _ = #a{},
    A = #a{integer1 = 1},
    B = A#a.integer1,
    {(hd([A]))#a.integer1, B}.
