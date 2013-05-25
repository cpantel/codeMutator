-module(helper_test).
-include_lib("eunit/include/eunit.hrl").

%abrir dets
%cargar dets con clases
%mutar tokens(


%   statistics(runtime),
%   statistics(wall_clock),


% mutate_test() ->
%    [{Classes,_},{Tokens,_}] = mutate("test/oneline.json"),
%    ?assert(Classes =:= "classes"),
%    ?assert(Tokens =:= "tokens").

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

%'{"classes":[{"name":"clone","type":"asymmetric","genes":{"gene":"clone","genePool":["="]}},{"name":"flow","type":"asymmetric","genes":{"gene":"exit","genePool":[""]}},{"name":"arithmetic","type":"symmetric","genePool":["&","\/","-","%","*","|","+","^"]},{"name":"bitwise","type":"symmetric","genePool":[">>","<<"]},{"name":"typeCasting","type":"symmetric","genePool":["boolean","integer","double","object","string"]},{"name":"logical","type":"symmetric","genePool":["&&","||","and","or","xor"]},{"name":"incrementing","type":"symmetric","genePool":["++","--"]},{"name":"comparisson","type":"symmetric","genePool":["==",">=","===","!=","!==","<="]},{"name":"accessControl","type":"symmetric","genePool":["public","private","protected"]},{"name":"bitwiseAssignment","type":"symmetric","pool":["<<=",">>=","^="]},{"name":"assignment","type":"symmetric","pool":["&=",".=","\/=","-=","%=","*=","|=","+="]}],"tokens":[{"class":"string","value":"<?php ","info":1},{"class":"string","value":"$a","info":1} 
%,{"class":"assignment","value":"=","info":0},{"class":"string","value":"1","info":1},{"class":"string","value":";","info":0},{"class":"string","value":"$a","info":1},{"class":"assignment","value":"=","info":0},{"class":"string","value":"$a","info":1},{"class":"arithmetic","value":"+","info":0},{"class":"string","value":"1","info":1},{"class":"string","value":";","info":0}]}';
 

%{"classes":[{"name":"clone","type":"asymmetric","genes":{"gene":"clone","genePool":["="]}},{"name":"flow","type":"asymmetric","genes":{"gene":"exit","genePool":[""]}}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
find_class_test() ->
  ClassEts = helper:prepare(classes),
  helper:load_classes(ClassEts, fixtureGiveMeManyClasses()),
  [{<<"clone">>,ClassItem}] = helper:find_class(ClassEts,<<"clone">>),
  ?assert(ClassItem =:= fixtureGiveMeCloneClass()).

  
load_one_class_test() ->
  ClassEts = helper:prepare(classes),
  helper:load_one_class(ClassEts,fixtureGiveMeCloneClass()),
  [{<<"clone">>,ClassItem}] = helper:find_class(ClassEts,<<"clone">>),
  ?assert(fixtureGiveMeCloneClass() =:= ClassItem).

load_classes_test() ->
  ClassEts = helper:prepare(classes),
  helper:load_classes(ClassEts, fixtureGiveMeManyClasses()),
  [{<<"clone">>,ClassItem1}] = helper:find_class(ClassEts,<<"clone">>),
  ?assert(fixtureGiveMeCloneClass() =:= ClassItem1),
  [{<<"accessControl">>,ClassItem2}] = helper:find_class(ClassEts,<<"accessControl">>),
  ?assert(fixtureGiveMeAccessControlClass() =:= ClassItem2).

  
load_classes_empty_test() ->
  ClassEts = helper:prepare(classes),
  helper:load_classes(ClassEts,[]),
  ?assert(helper:find_class(ClassEts,<<"key">>) =:= []). 
  
% % % readlines_test_() ->
% % %     ?assert(helper:readlines("test/simple.json")=:="[{\"key1\":\"value1\"},{\"key2\":\"value2\"}]").
% % 
% % 
% % % readlines_fail_test_() ->
% % %   ?assertException(error, _,helper:readlines("test/void.json")).


% 
% % borrable
read_test() ->
   Buffer = helper:read("[1,3.14,{\"key\":\"value\"}]"),
   ?assert(Buffer=:=[1,3.14,{[{<<"key">>,<<"value">>}]}]).%,
   %Buffer2 = read("\"classes\":[{\"name\":\"clone\",\"type\":\"asymmetric\",\"genes\":{\"gene\":\"clone\",\"genePool\":[\"=\"]}},{\"name\":\"flow\",\"type\":\"asymmetric\",\"genes\":{\"gene\":\"exit\",\"genePool\":[\"\"]}}").
% 

% {<<"string">>,<<"inmutable">>,
%  {[{<<"class">>,<<"string">>},
%    {<<"value">>,<<"<?php ">>},
%    {<<"info">>,1}]},
%  {[{<<"name">>,<<"string">>},
%    {<<"type">>,<<"inmutable">>},
%    {<<"genes">>,
%     {[{<<"gene">>,<<"clone">>},{<<"genePool">>,[<<"=">>]}]}}]}}

classify_token_test() ->
  ClassEts = helper:prepare(classes),
  helper:load_classes(ClassEts, fixtureGiveMeManyClasses()),
  Token = fixtureGiveMeOneInmutableToken(),
  Class = fixtureGiveMeStringClass(),
  ?assert({<<"string">>,<<"inmutable">>,Token,Class} =:= helper:classify_token(ClassEts,Token)).

mutate_empty_token_test() ->
   ?assert([] =:= helper:mutate([],fixtureGiveMeStringClass())).

mutate_one_inmutable_token_test() ->
   ?assert([<<"<?php ">>] =:= helper:mutate(fixtureGiveMeOneInmutableToken(),fixtureGiveMeStringClass())).

mutate_another_inmutable_token_test() ->
   ?assert([<<"$a">>] =:= helper:mutate(fixtureGiveMeAnotherInmutableToken(),fixtureGiveMeStringClass())).
   
% mutate_one_mutable_asymetric_token_test() ->
%     ?assert([<<"=">>] =:= helper:mutate(fixtureGiveMeAnotherInmutableToken(),fixtureGiveMeCloneClass())).


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

fixtureGiveMeStringClass() ->
{[{<<"name">>,<<"string">>},
   {<<"type">>,<<"inmutable">>}
   ]}.

    
fixtureGiveMeManyClasses() ->
[{[{<<"name">>,<<"string">>},
   {<<"type">>,<<"inmutable">>}
   ]},
 {[{<<"name">>,<<"clone">>},
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

     
fixtureGiveMeOneInmutableToken() ->
{[{<<"class">>,<<"string">>},
   {<<"value">>,<<"<?php ">>},
   {<<"info">>,1}]}.

fixtureGiveMeAnotherInmutableToken() ->
{[{<<"class">>,<<"string">>},
   {<<"value">>,<<"$a">>},
   {<<"info">>,1}]}.

fixtureGiveMeOneCloneToken() ->
{[{<<"class">>,<<"clone">>},
   {<<"value">>,<<"clone">>},
   {<<"info">>,1}]}.

fixtureGiveMeManyTokens() ->
[{[{<<"class">>,<<"string">>},
   {<<"value">>,<<"<?php ">>},
   {<<"info">>,1}]},
 {[{<<"class">>,<<"string">>},
   {<<"value">>,<<"$a">>},
   {<<"info">>,1}]},
 {[{<<"class">>,<<"assignment">>},
   {<<"value">>,<<"=">>},
   {<<"info">>,0}]},
 {[{<<"class">>,<<"string">>},
   {<<"value">>,<<"1">>},
   {<<"info">>,1}]},
 {[{<<"class">>,<<"string">>},
   {<<"value">>,<<";">>},
   {<<"info">>,0}]},
 {[{<<"class">>,<<"string">>},
   {<<"value">>,<<"$a">>},
   {<<"info">>,1}]},
 {[{<<"class">>,<<"assignment">>},
   {<<"value">>,<<"=">>},
   {<<"info">>,0}]},
 {[{<<"class">>,<<"string">>},
   {<<"value">>,<<"$a">>},
   {<<"info">>,1}]},
 {[{<<"class">>,<<"arithmetic">>},
   {<<"value">>,<<"+">>},
   {<<"info">>,0}]},
 {[{<<"class">>,<<"string">>},
   {<<"value">>,<<"1">>},
   {<<"info">>,1}]},
 {[{<<"class">>,<<"string">>},
   {<<"value">>,<<";">>},
   {<<"info">>,0}]}].
   

