-module(mutator).
-export([
  mutate_source/1,
  print/1
]).


%   statistics(runtime),
%   statistics(wall_clock),
print(Filename)->
    io:format("~s~n", [mutate_source(Filename)]).

mutate_source(Filename) ->
    {ok, Source} = file:read_file(Filename),
   
    {[{<<"classes">>,Classes},{<<"tokens">>, SourceTokens}]} =  mutator:json_to_term(Source),
    ClassMap = mutator:prepare(classes),
    mutator:load_classes(ClassMap, Classes),
    ClassifiedTokens = mutator:classify_tokens(ClassMap,SourceTokens),
    MutatedTokens = mutator:mutate_tokens(ClassifiedTokens),
    Mutations = mutator:generate(MutatedTokens),
    mutator:term_to_json(Mutations).

mutate_tokens(Tokens) ->
  [mutate_token(Token) || Token <- Tokens].
  
classify_tokens(ClassMap,Tokens)->
   [classify_token(ClassMap, Token) || Token <-Tokens].


classify_token(ClassMap,Token={[{<<"class">>,TokenClass}|_]}) ->
  [{TokenClass,ClassItem}]=lookup_class(ClassMap,TokenClass),
  {[{<<"name">>,TokenClass},{<<"type">>,Type},_]}=ClassItem,
  {TokenClass,Type,Token,ClassItem}.

generate_the_rest(Tokens) ->
   generate_the_rest(Tokens,[]).

generate_the_rest([],Accum) ->
   lists:reverse(Accum);
generate_the_rest([{Value,Info,_Pool }|[]],Accum) ->
   generate_the_rest([],[{Value,Info}|Accum]);
generate_the_rest([{Value,Info,_Pool }|TheRest],Accum) ->
   generate_the_rest(TheRest,[{Value,Info}|Accum]).

depth(L) ->
  depth(L,0).
  
depth([H|_],Accum)->
   depth(H,Accum + 1);
depth([],Accum)->
   Accum +1;
depth(_,Accum) ->
   Accum.

fix([[[]]]) ->
   [];
fix([[]]) ->   
  [];
fix([]) ->  
  [];
fix(Tokens)->
   Levels = depth(Tokens),
   case Levels of
     0->[[Tokens]];
     1->[Tokens];
     2->Tokens;
     3->[R]=Tokens,
        R
   end.

% normalizar([L = [[_|_]|_]]) -> L;
%   normalizar(L) -> L.

generate(Tokens) ->
   fix(generate(Tokens,[])).
%  normalizar(generate(Tokens,[])).

generate([],[]) ->
   [];
generate([],Accum) ->
   lists:reverse(Accum);
generate([{Token,Info,[]}],Accum) ->  
   generate([],[{Token,Info}|Accum]);
generate([{Token,Info,[]}| TheRest],Accum) ->  
   generate(TheRest,[{Token,Info}|Accum]);
generate([{Value,Info,Pool }|TheRest],Accum) ->
   [ generate(TheRest,[{Value,Info} |Accum]) | [generate_the_rest(TheRest,[{Gen,Info}|Accum]) || Gen <- Pool]].
   
mutate_token({ ClassName, <<"inmutable">>, _Token={[{<<"class">>,ClassName},{<<"value">>,Value},Info]} , _Class}) ->
   {Value,Info,[]};
mutate_token({
       ClassName,<<"asymmetric">>, 
       _Token={[{<<"class">>,ClassName}|[{<<"value">>,Value}|[Info]]]},
       _Class={[
         {<<"name">>,ClassName},
         {<<"type">>,<<"asymmetric">>},
         {<<"genes">>,Genes}
       ]}
   }) ->
  {Value,Info,find_gen(Value,Genes,<<"asymmetric">> )};
mutate_token({
       ClassName,<<"symmetric">>, 
       _Token={[{<<"class">>,ClassName}|[{<<"value">>,Value}|[Info]]]},
       _Class={[
         {<<"name">>,ClassName},
         {<<"type">>,<<"symmetric">>},
         {<<"genePool">>,Genes}
       ]}
   }) ->
  {Value,Info,find_gen(Value, [{[{<<"gene">>,Value},{<<"genePool">>,Genes}]}], <<"symmetric">>)}.

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
 
json_to_term(Line) ->
  json_eep:json_to_term(binary:bin_to_list(Line)).
 
term_to_json(Mutations) ->
  json_eep:term_to_json(fix_tokens(Mutations)).

fix_tokens(Tokens) ->
  [ repack_file(File) || File <- Tokens].

repack_file(File) ->  
  [ repack_token( Token  ) || Token <- File].
  
repack_token({Value, Info}) ->
  {[{<<"value">>,Value},Info]}.

  
% get_all_lines(Device, Accum) ->
%     case io:get_line(Device, "") of
%         eof  -> file:close(Device), hd(lists:reverse(Accum));
%         Line -> get_all_lines(Device, [Line|Accum])
%     end.
% 
% read_file(FileName) ->
%     {ok, Device} = file:open(FileName, [read]),
%     get_all_lines(Device, []).

