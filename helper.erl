-module(helper).
-export([
  prepare/1,
  mutate/1,
  read_file/1
  ]).

%abrir ets
%cargar ets con clases
% para cada token
%    classify_token
%    find_gen
%    mutate


%   statistics(runtime),
%   statistics(wall_clock),

classify_token(ClassMap,Token={[{<<"class">>,TokenClass}|_]}) ->
  [{TokenClass,ClassItem}]=lookup_class(ClassMap,TokenClass),
  {[{<<"name">>,TokenClass},{<<"type">>,Type},_]}=ClassItem,
  {TokenClass,Type,Token,ClassItem}.

   
mutate({ ClassName, <<"inmutable">>, Token={[{<<"class">>,ClassName},{<<"value">>,Value},Info]} , Class}) ->
   [Value];
mutate({
       ClassName,<<"asymmetric">>, 
       Token={[{<<"class">>,ClassName}|[{<<"value">>,Value}|[_Info]]]},
       Class={[
         {<<"name">>,ClassName},
         {<<"type">>,<<"asymmetric">>},
         {<<"genes">>,Genes}
       ]}
   }) ->
  find_gen(Value,Genes,<<"asymmetric">> ).

  %% symetric - > Value, [{[{<<"gene">>,Value},{<<"genePool">>,Genes}]}], <<"symmetric">>

find_gen(Value, [{[{<<"gene">>,Value},{<<"genePool">>,GenePool}]}], <<"symmetric">>)->
  lists:delete(Value,GenePool);
find_gen(Value, [{[{<<"gene">>,Value},{<<"genePool">>,GenePool}]}], <<"asymmetric">>)->
  GenePool;
find_gen(Value, [{[{<<"gene">>,Value},{<<"genePool">>,GenePool}]}|_TheRest],<<"asymmetric">>)->
  GenePool;
find_gen(Value1, [{[{<<"gene">>,Value2},_]}| TheRest ],Type) when Value1 =/= Value2 ->
    find_gen(Value1,TheRest,Type).
  
load_one_class(Ets,Class={[{<<"name">>,Name}|_]}) ->
  ets:insert(Ets, {Name,Class}).

load_classes(_ClassMap,[]) ->
  true;
load_classes(ClassMap,[Class| Classes]) ->
  load_one_class(ClassMap,Class),
  load_classes(ClassMap,Classes).

lookup_class(ClassMap,Class) ->
  ets:lookup(ClassMap,Class).

prepare(Ets) ->
  ets:new(Ets, [set]).
 
read(Line) ->
  json_eep:json_to_term(Line).
  
get_all_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), hd(lists:reverse(Accum));
        Line -> get_all_lines(Device, [Line|Accum])
    end.

read_file(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    get_all_lines(Device, []).

