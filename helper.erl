-module(helper).
-export([
  prepare/1,
  mutate/1,
  readlines/1
  ]).

%abrir dets
%cargar dets con clases
% para cada token
%    classify_token
%    find_gen
%    mutate


%   statistics(runtime),
%   statistics(wall_clock),


% mutate_test() ->
%    [{Classes,_},{Tokens,_}] = mutate("test/oneline.json"),
%    ?_assert(Classes =:= "classes"),
%    ?_assert(Tokens =:= "tokens").

%load_classes_test()->
%  TestClasses = epp_json_to_term(...),
%  load_classes(TestClasses),
%  [{...}] = dets:lookup(ClassFile, "comparisson")
% it shoud take the full json taken from the file and load the classes in   
% 
%

%   ClassFile = classfile.dets,
%   dets:open_file(ClassFile, [{type, set}]),
%   dets:delete_all_objects(ClassFile),
%   dets:insert(ClassFile, {group_administrators,GroupAdministratorsRaw}),
%   [{group_administrators, GroupAdministratorsRaw }] = dets:lookup(ClassFile ,group_administrators ),

%'{"classes":[{"name":"clone","type":"asymmetric","genes":{"gene":"clone","genePool":["="]}},{"name":"flow","type":"asymmetric","genes":{"gene":"exit","genePool":[""]}},{"name":"arithmetic","type":"symmetric","genePool":["&","\/","-","%","*","|","+","^"]},{"name":"bitwise","type":"symmetric","genePool":[">>","<<"]},{"name":"typeCasting","type":"symmetric","genePool":["boolean","integer","double","object","string"]},{"name":"logical","type":"symmetric","genePool":["&&","||","and","or","xor"]},{"name":"incrementing","type":"symmetric","genePool":["++","--"]},{"name":"comparisson","type":"symmetric","genePool":["==",">=","===","!=","!==","<="]},{"name":"accessControl","type":"symmetric","genePool":["public","private","protected"]},{"name":"bitwiseAssignment","type":"symmetric","pool":["<<=",">>=","^="]},{"name":"assignment","type":"symmetric","pool":["&=",".=","\/=","-=","%=","*=","|=","+="]}],"tokens":[{"class":"inmutable","value":"<?php ","info":1},{"class":"inmutable","value":"$a","info":1} 
%,{"class":"assignment","value":"=","info":0},{"class":"inmutable","value":"1","info":1},{"class":"inmutable","value":";","info":0},{"class":"inmutable","value":"$a","info":1},{"class":"assignment","value":"=","info":0},{"class":"inmutable","value":"$a","info":1},{"class":"arithmetic","value":"+","info":0},{"class":"inmutable","value":"1","info":1},{"class":"inmutable","value":";","info":0}]}';
 

%{"classes":[{"name":"clone","type":"asymmetric","genes":{"gene":"clone","genePool":["="]}},{"name":"flow","type":"asymmetric","genes":{"gene":"exit","genePool":[""]}}



% helper:mutate({[{<<"class">>,<<"inmutable">>},
%                                     {<<"value">>,<<"?a">>},
%                                     {<<"info">>,1}]},
%                                   {[{<<"name">>,<<"string">>},
%                                     {<<"type">>,<<"inmutable">>}]}) (helper.erl,

% helper:mutate({[{<<"class">>,<<"inmutable">>},
%                                     {<<"value">>,<<"<?php ">>},
%                                     {<<"info">>,1}]},Class) 

%mutate({[]},{[{<<"name">>,<<"string">>},{<<"type">>,<<"inmutable">>}]})
%mutate({[]},{[{<<"name">>,<<"string">>},{<<"type">>,<<"inmutable">>}]})

%{[{<<"name">>,TokenClass},{<<"type">>,Type},_]}
  
classify_token(ClassMap,Token={[{<<"class">>,TokenClass}|_]}) ->
  [{TokenClass,ClassItem}]=lookup_class(ClassMap,TokenClass),
  {[{<<"name">>,TokenClass},{<<"type">>,Type},_]}=ClassItem,
  {TokenClass,Type,Token,ClassItem}.

   
mutate({ ClassName, <<"inmutable">>, Token={[{<<"class">>,ClassName},{<<"value">>,Value},Info]} , Class}) ->
   [Value];
mutate({
       ClassName, <<"asymmetric">>, 
       Token={[{<<"class">>,ClassName}|[{<<"value">>,Value}|[_Info]]]},
       Class={[
         {<<"name">>,ClassName},
         {<<"type">>,<<"asymmetric">>},
         {<<"genes">>,Genes}
       ]}
   }) ->
  find_gen(Value,Genes).

find_gen(Value, [{[{<<"gene">>,Value},{<<"genePool">>,GenePool}]}])->
  GenePool;
find_gen(Value, [{[{<<"gene">>,Value},{<<"genePool">>,GenePool}]}|_TheRest])->
  GenePool;
find_gen(Value1, [{[{<<"gene">>,Value2},_]}| TheRest ]) when Value1 =/= Value2 ->
    find_gen(Value1,TheRest).
  
 
  
% mutate({ ClassName, <<"asymmetric">>, Token={[{<<"class">>,ClassName},{<<"value">>,Value},Info]} ,Class}) ->
%  {[{<<"name">>,<<"clone">>},
%   {<<"type">>,<<"asymmetric">>},
%   {<<"genes">>,
%    {[{<<"gene">>,<<"clone">>},{<<"genePool">>,GenePool}]}}
%  ]}=Class,
%    GenePool.
   
% %        %Token=
% % mutate({[{<<"class">>,<<"string">>},{<<"value">>,Value},_]},_Class) ->
% %    [Value].

% mutate({[{<<"class">>,TokenClass},{<<"value">>,Value},_]},_Class) ->
%    [Value].

%  {[{<<"class">>,<<"inmutable">>},
%   {<<"value">>,<<"<?php ">>},
%   {<<"info">>,1}]}.
   
load_one_class(Ets,Class={[{<<"name">>,Name}|_]}) ->
  ets:insert(Ets, {Name,Class}).

load_classes(_ClassMap,[]) ->
  true;
load_classes(ClassMap,[Class| Classes]) ->
  load_one_class(ClassMap,Class),
  load_classes(ClassMap,Classes).

lookup_class(ClassMap,Class) ->
  ets:lookup(ClassMap,Class).

%% TESTED OK  
  
prepare(Ets) ->
  ets:new(Ets, [set]).
  
% THIS IS THE REALITY...   
% {[{<<"classes">>,Classes},_]}=json_eep:json_to_term(helper:readlines("test/code.json")).
   
read(Line) ->
  json_eep:json_to_term(Line).
  
get_all_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), hd(lists:reverse(Accum));
        Line -> get_all_lines(Device, [Line|Accum])
    end.

readlines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    get_all_lines(Device, []).

