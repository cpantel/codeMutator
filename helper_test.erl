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
%  [{...}] = dhelper:lookup_class(ClassFile, "comparisson")
% it shoud take the full json taken from the file and load the classes in   
% 
%

%'{"classes":[{"name":"clone","type":"asymmetric","genes":{"gene":"clone","genePool":["="]}},{"name":"flow","type":"asymmetric","genes":{"gene":"exit","genePool":[""]}},{"name":"arithmetic","type":"symmetric","genePool":["&","\/","-","%","*","|","+","^"]},{"name":"bitwise","type":"symmetric","genePool":[">>","<<"]},{"name":"typeCasting","type":"symmetric","genePool":["boolean","integer","double","object","string"]},{"name":"logical","type":"symmetric","genePool":["&&","||","and","or","xor"]},{"name":"incrementing","type":"symmetric","genePool":["++","--"]},{"name":"comparisson","type":"symmetric","genePool":["==",">=","===","!=","!==","<="]},{"name":"accessControl","type":"symmetric","genePool":["public","private","protected"]},{"name":"bitwiseAssignment","type":"symmetric","pool":["<<=",">>=","^="]},{"name":"assignment","type":"symmetric","pool":["&=",".=","\/=","-=","%=","*=","|=","+="]}],"tokens":[{"class":"string","value":"<?php ","info":1},{"class":"string","value":"$a","info":1} 
%,{"class":"assignment","value":"=","info":0},{"class":"string","value":"1","info":1},{"class":"string","value":";","info":0},{"class":"string","value":"$a","info":1},{"class":"assignment","value":"=","info":0},{"class":"string","value":"$a","info":1},{"class":"arithmetic","value":"+","info":0},{"class":"string","value":"1","info":1},{"class":"string","value":";","info":0}]}';
 

%{"classes":[{"name":"clone","type":"asymmetric","genes":{"gene":"clone","genePool":["="]}},{"name":"flow","type":"asymmetric","genes":{"gene":"exit","genePool":[""]}}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
lookup_class_test() ->
  ClassMap = helper:prepare(classes),
  helper:load_classes(ClassMap, fixtureGiveMeManyClasses()),
  [{<<"clone">>,ClassItem}] = helper:lookup_class(ClassMap,<<"clone">>),
  ?assert(ClassItem =:= fixtureGiveMeCloneClass()).

  
load_one_class_test() ->
  ClassMap = helper:prepare(classes),
  helper:load_one_class(ClassMap,fixtureGiveMeCloneClass()),
  [{<<"clone">>,ClassItem}] = helper:lookup_class(ClassMap,<<"clone">>),
  ?assert(fixtureGiveMeCloneClass() =:= ClassItem).

load_classes_test() ->
  ClassMap = helper:prepare(classes),
  helper:load_classes(ClassMap, fixtureGiveMeManyClasses()),
  [{<<"clone">>,ClassItem1}] = helper:lookup_class(ClassMap,<<"clone">>),
  ?assert(fixtureGiveMeCloneClass() =:= ClassItem1),
  [{<<"accessControl">>,ClassItem2}] = helper:lookup_class(ClassMap,<<"accessControl">>),
  ?assert(fixtureGiveMeAccessControlClass() =:= ClassItem2).

  
load_classes_empty_test() ->
  ClassMap = helper:prepare(classes),
  helper:load_classes(ClassMap,[]),
  ?assert(helper:lookup_class(ClassMap,<<"key">>) =:= []). 
  
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
  ClassMap = helper:prepare(classes),
  helper:load_classes(ClassMap, fixtureGiveMeManyClasses()),
  Token = fixtureGiveMeOneStringToken(),
  Class = fixtureGiveMeStringClass(),
  ?assert({<<"string">>,<<"inmutable">>,Token,Class} =:= helper:classify_token(ClassMap,Token)).


mutate_one_string_token_test() ->
    Token = fixtureGiveMeOneStringToken(),
    Class = fixtureGiveMeStringClass(),
    ?assert([<<"<?php ">>] =:= helper:mutate({<<"string">>,<<"inmutable">>,Token,Class})).
 
mutate_another_inmutable_token_test() ->
    Token = fixtureGiveMeAnotherStringToken(),
    Class = fixtureGiveMeStringClass(),
    ?assert([<<"$a">>] =:= helper:mutate({<<"string">>,<<"inmutable">>,Token,Class})).

mutate_one_symmetric_token_test() ->
    Token = fixtureGiveMeCloneToken(),
    Class = fixtureGiveMeCloneClass(),
    ?assert([<<"=">>] =:= helper:mutate({<<"clone">>,<<"asymmetric">>,Token,Class})).
    
% % % mutate_another_symmetric_token_test() ->
% % %     Token = fixtureGiveMeFlowToken(),
% % %     Class = fixtureGiveMeFlowClass(),
% % %     ?assert([<<"=">>] =:= helper:mutate({<<"flow">>,<<"asymmetric">>,Token,Class})).

find_gen_test() ->
    Genes = fixtureGiveMeCloneGenes(),
    Gen = helper:find_gen(<<"clone">>,Genes),
    Expected = [<<"=">>],
    ?assert(Expected =:= Gen).

find_another_gen_test() ->
    Genes = fixtureGiveMeFlowGenes(),
    Gen = helper:find_gen(<<"exit">>,Genes),
    Expected = [<<"">>],
    ?assert(Expected =:= Gen).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                 CLASS FIXTURES 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fixtureGiveMeCloneGenes() ->
{[
  {<<"name">>,<<"clone">>},
  {<<"type">>,<<"asymmetric">>},
  {<<"genes">>,Genes}
  ]} = fixtureGiveMeCloneClass(),
   Genes.


   
   
fixtureGiveMeFlowGenes() ->
{[
  {<<"name">>,<<"flow">>},
  {<<"type">>,<<"asymmetric">>},
  {<<"genes">>,Genes}
 ]} = fixtureGiveMeFlowClass(),
   Genes.

   
%                        {[{<<"name">>,<<"flow">>},
%                       {<<"type">>,<<"asymmetric">>},
%                       {<<"genes">>,
%                        {[{<<"gene">>,<<"exit">>},{<<"genePool">>,[<<>>]}]},
%                        {[{<<"gene">>,<<"break">>},
%                          {<<"genePool">>,
%                           [<<>>,<<"continue">>,<<"exit">>,<<"return">>]}]},
%                        {[{<<"gene">>,<<"return">>},
%                          {<<"genePool">>,
%                           [<<>>,<<"break">>,<<"continue">>,<<"exit">>]}]},
%                        {[{<<"gene">>,<<"continue">>},
%                          {<<"genePool">>,
%                           [<<>>,<<"break">>,<<"exit">>,<<"return">>]}]}}]}
   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                 CLASS FIXTURES 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fixtureGiveMeCloneClass() ->
{[
  {<<"name">>,<<"clone">>},
  {<<"type">>,<<"asymmetric">>},
  {<<"genes">>,
    [{[{<<"gene">>,<<"clone">>}, {<<"genePool">>,[<<"=">>]}]}]
  }]}.

fixtureGiveMeFlowClass() ->
{[
  {<<"name">>,<<"flow">>},
  {<<"type">>,<<"asymmetric">>},
  {<<"genes">>,[
    {[{<<"gene">>,<<"exit">>},{<<"genePool">>,[<<"">>]}]},
    {[{<<"gene">>,<<"break">>},{<<"genePool">>,[<<"">>,<<"continue">>,<<"exit">>,<<"return">>]}]},
    {[{<<"gene">>,<<"return">>},{<<"genePool">>,[<<"">>,<<"break">>,<<"continue">>,<<"exit">>]}]},
    {[{<<"gene">>,<<"continue">>},{<<"genePool">>,[<<"">>,<<"break">>,<<"exit">>,<<"return">>]}]}
  ]}]}.

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
   {<<"genes">>,[
      {[{<<"gene">>,<<"clone">>},{<<"genePool">>,[<<"=">>]}]}
    ]
   }
  ]},
    
 {[{<<"name">>,<<"flow">>},
   {<<"type">>,<<"asymmetric">>},
   {<<"genes">>,[
      {[{<<"gene">>,<<"break">>},{<<"genePool">>,[<<"">>,<<"continue">>,<<"exit">>,<<"return">>]}]},
      {[{<<"gene">>,<<"continue">>},{<<"genePool">>,[<<"">>,<<"break">>,<<"exit">>,<<"return">>]}]},
      {[{<<"gene">>,<<"exit">>},{<<"genePool">>,[<<"">>]}]},
      {[{<<"gene">>,<<"return">>},{<<"genePool">>,[<<"">>,<<"break">>,<<"continue">>,<<"exit">>]}]}
    ]
   }
  ]},
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
   {<<"genePool">>,[<<"<<=">>,<<">>=">>,<<"^=">>]}]},
 {[{<<"name">>,<<"assignment">>},
   {<<"type">>,<<"symmetric">>},
   {<<"genePool">>,
    [<<"&=">>,<<".=">>,<<"/=">>,<<"-=">>,<<"%=">>,<<"*=">>,
     <<"|=">>,<<"+=">>]}]}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                 TOKEN FIXTURES 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     
fixtureGiveMeOneStringToken() ->
{[{<<"class">>,<<"string">>},
   {<<"value">>,<<"<?php ">>},
   {<<"info">>,1}]}.

fixtureGiveMeAnotherStringToken() ->
{[{<<"class">>,<<"string">>},
   {<<"value">>,<<"$a">>},
   {<<"info">>,1}]}.

fixtureGiveMeCloneToken() ->
{[{<<"class">>,<<"clone">>},
   {<<"value">>,<<"clone">>},
   {<<"info">>,1}]}.

fixtureGiveMeFlowToken() ->
{[{<<"class">>,<<"flow">>},
   {<<"value">>,<<"exit">>},
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
   

