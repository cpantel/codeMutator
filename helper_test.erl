-module(helper_test).
-include_lib("eunit/include/eunit.hrl").

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
%  [{...}] = dhelper:find_class(ClassFile, "comparisson")
% it shoud take the full json taken from the file and load the classes in   
% 
%

%   ClassFile = classfile.dets,
%   dets:open_file(ClassFile, [{type, set}]),
%   dets:delete_all_objects(ClassFile),
%   dets:insert(ClassFile, {group_administrators,GroupAdministratorsRaw}),
%   [{group_administrators, GroupAdministratorsRaw }] = dhelper:find_class(ClassFile ,group_administrators ),

%'{"classes":[{"name":"clone","type":"asymmetric","genes":{"gene":"clone","genePool":["="]}},{"name":"flow","type":"asymmetric","genes":{"gene":"exit","genePool":[""]}},{"name":"arithmetic","type":"symmetric","genePool":["&","\/","-","%","*","|","+","^"]},{"name":"bitwise","type":"symmetric","genePool":[">>","<<"]},{"name":"typeCasting","type":"symmetric","genePool":["boolean","integer","double","object","string"]},{"name":"logical","type":"symmetric","genePool":["&&","||","and","or","xor"]},{"name":"incrementing","type":"symmetric","genePool":["++","--"]},{"name":"comparisson","type":"symmetric","genePool":["==",">=","===","!=","!==","<="]},{"name":"accessControl","type":"symmetric","genePool":["public","private","protected"]},{"name":"bitwiseAssignment","type":"symmetric","pool":["<<=",">>=","^="]},{"name":"assignment","type":"symmetric","pool":["&=",".=","\/=","-=","%=","*=","|=","+="]}],"tokens":[{"class":"inmutable","value":"<?php ","info":1},{"class":"inmutable","value":"$a","info":1} 
%,{"class":"assignment","value":"=","info":0},{"class":"inmutable","value":"1","info":1},{"class":"inmutable","value":";","info":0},{"class":"inmutable","value":"$a","info":1},{"class":"assignment","value":"=","info":0},{"class":"inmutable","value":"$a","info":1},{"class":"arithmetic","value":"+","info":0},{"class":"inmutable","value":"1","info":1},{"class":"inmutable","value":";","info":0}]}';
 

%{"classes":[{"name":"clone","type":"asymmetric","genes":{"gene":"clone","genePool":["="]}},{"name":"flow","type":"asymmetric","genes":{"gene":"exit","genePool":[""]}}
 
find_class_test() ->
  ClassEts = helper:prepare(classes),
  helper:load_classes(ClassEts, fixtureGiveMeManyClasses()),
  ClassItem = helper:find_class(ClassEts,<<"clone">>),
  ?_assert(fixtureGiveMeCloneClass() =:= ClassItem).

  
load_one_class_test() ->
  ClassEts = helper:prepare(classes),
  helper:load_one_class(ClassEts,fixtureGiveMeCloneClass()),
  [{<<"clone">>,ClassItem}] = helper:find_class(ClassEts,<<"clone">>),
  ?_assert(fixtureGiveMeCloneClass() =:= ClassItem).

load_classes_test() ->
  ClassEts = helper:prepare(classes),
  helper:load_classes(ClassEts, fixtureGiveMeManyClasses()),
  [{<<"clone">>,ClassItem1}] = helper:find_class(ClassEts,<<"clone">>),
  ?_assert(fixtureGiveMeCloneClass() =:= ClassItem1),
  [{<<"accessControl">>,ClassItem2}] = helper:find_class(ClassEts,<<"accessControl">>),
  ?_assert(fixtureGiveMeAccessControlClass() =:= ClassItem2).

  
load_classes_empty_test() ->
  ClassEts = helper:prepare(classes),
  helper:load_classes(ClassEts,[]),
  ?_assert(helper:find_class(ClassEts,<<"key">>) =:= []). 
  
  
readlines_test_() ->
   [
    ?_assertException(error, _,helper:readlines("test/void.json")),
    ?_assert(helper:readlines("test/simple.json")=:="[{\"key1\":\"value1\"},{\"key2\":\"value2\"}]")
   ].
   

% borrable
read_test() ->
   Buffer = helper:read("[1,3.14,{\"key\":\"value\"}]"),
   ?_assert(Buffer=:=[1,3.14,{[{<<"key">>,<<"value">>}]}]).%,
   %Buffer2 = read("\"classes\":[{\"name\":\"clone\",\"type\":\"asymmetric\",\"genes\":{\"gene\":\"clone\",\"genePool\":[\"=\"]}},{\"name\":\"flow\",\"type\":\"asymmetric\",\"genes\":{\"gene\":\"exit\",\"genePool\":[\"\"]}}").
   
% THIS IS THE REALITY...   
% {[{<<"classes">>,Classes},_]}=json_eep:json_to_term(helper:readlines("test/code.json")).

fixtureGiveMeCloneClass() ->
{[
  {<<"name">>,<<"clone">>},
  {<<"type">>,<<"asymmetric">>},
  {<<"genes">>,{[
      {<<"gene">>,<<"clone">>},
      {<<"genePool">>,[<<"=">>]}
     ]}
   }
   ]
   }.

fixtureGiveMeAccessControlClass() ->
{[{<<"name">>,<<"accessControl">>},
   {<<"type">>,<<"symmetric">>},
   {<<"genePool">>,
    [<<"public">>,<<"private">>,<<"protected">>]}]}.


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
{[{<<"class">>,<<"inmutable">>},
   {<<"value">>,<<"<?php ">>},
   {<<"info">>,1}]}.
     
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
   

