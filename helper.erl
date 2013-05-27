-module(helper).
-export([
  prepare/1,
  mutate_token/1,
  read_file/1
  ]).

%abrir ets
%cargar ets con clases
% para cada token
%    classify_token
%    find_gen
%    mutate_token


%   statistics(runtime),
%   statistics(wall_clock),

classify_token(ClassMap,Token={[{<<"class">>,TokenClass}|_]}) ->
  [{TokenClass,ClassItem}]=lookup_class(ClassMap,TokenClass),
  {[{<<"name">>,TokenClass},{<<"type">>,Type},_]}=ClassItem,
  {TokenClass,Type,Token,ClassItem}.

% mutate(ClassMap, [], Accum) ->
% 
% mutate(ClassMap, [Mutations | TheRest], Accum) ->
% 
% accumulate every single token
% when a list of tokens is found, fire a single mutation for every one

generate_the_rest([],Accum) ->
   lists:reverse(Accum);

generate_the_rest([{Value,_Pool }|[]],Accum) ->
   generate_the_rest([],[Value|Accum]);
   
generate_the_rest([{Value,_Pool }|TheRest],Accum) ->
   generate_the_rest(TheRest,[Value|Accum]).
  

% generate([],Accum) ->
%    lists:reverse(Accum);
% 
% generate_the_rest([{Value,_Pool }|[]],Accum) ->
%    generate_the_rest([],[Value|Accum]);
   
generate([{Value,[] }|TheRest],Accum,MetaAccum) ->
   generate(TheRest,[Value|Accum],MetaAccum);
generate([{Value,Pool }|TheRest],Accum,MetaAccum) ->
   foreach mutation in pool, 
   generate_the_rest(TheRest,[Value|Accum],MetaAccum);
   
   

   
mutate_token({ ClassName, <<"inmutable">>, _Token={[{<<"class">>,ClassName},{<<"value">>,Value},_Info]} , _Class}) ->
   {Value,[]};
mutate_token({
       ClassName,<<"asymmetric">>, 
       _Token={[{<<"class">>,ClassName}|[{<<"value">>,Value}|[_Info]]]},
       _Class={[
         {<<"name">>,ClassName},
         {<<"type">>,<<"asymmetric">>},
         {<<"genes">>,Genes}
       ]}
   }) ->
  {Value,find_gen(Value,Genes,<<"asymmetric">> )};
mutate_token({
       ClassName,<<"symmetric">>, 
       _Token={[{<<"class">>,ClassName}|[{<<"value">>,Value}|[_Info]]]},
       _Class={[
         {<<"name">>,ClassName},
         {<<"type">>,<<"symmetric">>},
         {<<"genePool">>,Genes}
       ]}
   }) ->
  {Value,find_gen(Value, [{[{<<"gene">>,Value},{<<"genePool">>,Genes}]}], <<"symmetric">>)}.

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

