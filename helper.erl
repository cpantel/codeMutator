-module(helper).
-include_lib("eunit/include/eunit.hrl").
-export([readlines/1, read/1]).

%abrir dets
%cargar dets con clases
%mutar tokens(


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
 

             
load_one_class_test() ->
  ClassEts = prepare(classes),
  load_one_class(ClassEts,{"key","value"}),
  [{"key",ClassItem}] = ets:lookup(ClassEts,"key"),
  ?_assert("value" =:= ClassItem).

load_one_class(Ets,Class) ->
  ets:insert(Ets, Class).


load_classes_testX() ->
   ClassEts = prepare(classes),
   load_classes(ClassEts, [{"name","clone"},{"type","asymmetric"},{"genes",{"gene","clone","genePool",["="]}}] ).
   %[{<<"clone">>,ClassItem}] = ets:lookup(ClassEts,<<"clone">>).
  
% load_classes_empty_test() ->
%   ClassEts = prepare(classes),
%   load_classes(ClassEts,[]),
%   {badmatch,[]} = ets:lookup(ClassEts,"key").
  %?_assert("" =:= ClassItem). 
  
load_classes(_ClassEts,[]) ->
  true;
load_classes(ClassEts,[Class| Classes]) ->
  load_one_class(ClassEts,Class),
  load_classes(ClassEts,Classes).
  
  
prepare(Ets) ->
  ets:new(Ets, [set]).
  
% borrable
read_test() ->
   Buffer = read("[1,3.14,{\"key\":\"value\"}]"),
   ?_assert(Buffer=:=[1,3.14,{[{<<"key">>,<<"value">>}]}]),
   Buffer2 = read("\"classes\":[{\"name\":\"clone\",\"type\":\"asymmetric\",\"genes\":{\"gene\":\"clone\",\"genePool\":[\"=\"]}},{\"name\":\"flow\",\"type\":\"asymmetric\",\"genes\":{\"gene\":\"exit\",\"genePool\":[\"\"]}}").
   
% THIS IS THE REALITY...   
% {[{<<"classes">>,Classes},_]}=json_eep:json_to_term(helper:readlines("test/code.json")).

fixtureGiveMeOneClass() ->
[{[{<<"name">>,<<"clone">>},
   {<<"type">>,<<"asymmetric">>},
   {<<"genes">>,
    {[{<<"gene">>,<<"clone">>},{<<"genePool">>,[<<"=">>]}]}}]}].


fixtureGiveMeManyClasses() ->
[{[{<<"name">>,<<"clone">>},
   {<<"type">>,<<"asymmetric">>},
   {<<"genes">>,
    {[{<<"gene">>,<<"clone">>},{<<"genePool">>,[<<"=">>]}]}}]},
 {[{<<"name">>,<<"flow">>},
   {<<"type">>,<<"asymmetric">>},
   {<<"genes">>,
    {[{<<"gene">>,<<"exit">>},{<<"genePool">>,[<<>>]}]}}]},
 {[{<<"name">>,<<"arithmetic">>},
   {<<"type">>,<<"symmetric">>},
   {<<"genePool">>,
    [<<"&">>,<<"/">>,<<"-">>,<<"%">>,<<"*">>,<<"|">>,<<"+">>,
     <<"^">>]}]},
 {[{<<"name">>,<<"bitwise">>},
   {<<"type">>,<<"symmetric">>},
   {<<"genePool">>,[<<">>">>,<<"<<">>]}]},
 {[{<<"name">>,<<"typeCasting">>},
   {<<"type">>,<<"symmetric">>},
   {<<"genePool">>,
    [<<"boolean">>,<<"integer">>,<<"double">>,<<"object">>,
     <<"string">>]}]},
 {[{<<"name">>,<<"logical">>},
   {<<"type">>,<<"symmetric">>},
   {<<"genePool">>,
    [<<"&&">>,<<"||">>,<<"and">>,<<"or">>,<<"xor">>]}]},
 {[{<<"name">>,<<"incrementing">>},
   {<<"type">>,<<"symmetric">>},
   {<<"genePool">>,[<<"++">>,<<"--">>]}]},
 {[{<<"name">>,<<"comparisson">>},
   {<<"type">>,<<"symmetric">>},
   {<<"genePool">>,
    [<<"==">>,<<">=">>,<<"===">>,<<"!=">>,<<"!==">>,<<"<=">>]}]},
 {[{<<"name">>,<<"accessControl">>},
   {<<"type">>,<<"symmetric">>},
   {<<"genePool">>,
    [<<"public">>,<<"private">>,<<"protected">>]}]},
 {[{<<"name">>,<<"bitwiseAssignment">>},
   {<<"type">>,<<"symmetric">>},
   {<<"pool">>,[<<"<<=">>,<<">>=">>,<<"^=">>]}]},
 {[{<<"name">>,<<"assignment">>},
   {<<"type">>,<<"symmetric">>},
   {<<"pool">>,
    [<<"&=">>,<<".=">>,<<"/=">>,<<"-=">>,<<"%=">>,<<"*=">>,
     <<"|=">>,<<"+=">>]}]}].

     
fixtureGiveMeOneToken() ->
[{[{<<"class">>,<<"inmutable">>},
   {<<"value">>,<<"<?php ">>},
   {<<"info">>,1}]}].
     
fixtureGiveMeManyTokens() ->
[{[{<<"class">>,<<"inmutable">>},
   {<<"value">>,<<"<?php ">>},
   {<<"info">>,1}]},
 {[{<<"class">>,<<"inmutable">>},
   {<<"value">>,<<"$a">>},
   {<<"info">>,1}]},
 {[{<<"class">>,<<"assignment">>},
   {<<"value">>,<<"=">>},
   {<<"info">>,0}]},
 {[{<<"class">>,<<"inmutable">>},
   {<<"value">>,<<"1">>},
   {<<"info">>,1}]},
 {[{<<"class">>,<<"inmutable">>},
   {<<"value">>,<<";">>},
   {<<"info">>,0}]},
 {[{<<"class">>,<<"inmutable">>},
   {<<"value">>,<<"$a">>},
   {<<"info">>,1}]},
 {[{<<"class">>,<<"assignment">>},
   {<<"value">>,<<"=">>},
   {<<"info">>,0}]},
 {[{<<"class">>,<<"inmutable">>},
   {<<"value">>,<<"$a">>},
   {<<"info">>,1}]},
 {[{<<"class">>,<<"arithmetic">>},
   {<<"value">>,<<"+">>},
   {<<"info">>,0}]},
 {[{<<"class">>,<<"inmutable">>},
   {<<"value">>,<<"1">>},
   {<<"info">>,1}]},
 {[{<<"class">>,<<"inmutable">>},
   {<<"value">>,<<";">>},
   {<<"info">>,0}]}].

     
     
read(Line) ->
  json_eep:json_to_term(Line).


  
%% TESTED OK  
  
readlines_test_() ->
   [
    ?_assertException(error, _,readlines("test/void.json")),
    ?_assert(readlines("test/simple.json")=:="[{\"key1\":\"value1\"},{\"key2\":\"value2\"}]")
   ].
   
get_all_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), hd(lists:reverse(Accum));
        Line -> get_all_lines(Device, [Line|Accum])
    end.

readlines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    get_all_lines(Device, []).

