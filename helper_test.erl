-module(helper_test).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lookup_class_test() ->
  ClassMap = helper:prepare(classes),
  helper:load_classes(ClassMap, fixtureGiveMeAllClasses()),
  [{<<"clone">>,ClassItem}] = helper:lookup_class(ClassMap,<<"clone">>),
  ?assert(ClassItem =:= fixtureGiveMeCloneClass()).

lookup_class_unknown_test() ->
  ClassMap = helper:prepare(classes),
  helper:load_classes(ClassMap, fixtureGiveMeAllClasses()),
  ?assert([] =:= helper:lookup_class(ClassMap,<<"xxxx">>)).
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load_one_class_test() ->
  ClassMap = helper:prepare(classes),
  helper:load_one_class(ClassMap,fixtureGiveMeCloneClass()),
  [{<<"clone">>,ClassItem}] = helper:lookup_class(ClassMap,<<"clone">>),
  ?assert(fixtureGiveMeCloneClass() =:= ClassItem).

  
load_classes_test() ->
  ClassMap = helper:prepare(classes),
  helper:load_classes(ClassMap, fixtureGiveMeAllClasses()),
  [{<<"clone">>,ClassItem1}] = helper:lookup_class(ClassMap,<<"clone">>),
  ?assert(fixtureGiveMeCloneClass() =:= ClassItem1),
  [{<<"accessControl">>,ClassItem2}] = helper:lookup_class(ClassMap,<<"accessControl">>),
  ?assert(fixtureGiveMeAccessControlClass() =:= ClassItem2).

  
load_classes_empty_test() ->
  ClassMap = helper:prepare(classes),
  helper:load_classes(ClassMap,[]),
  ?assert(helper:lookup_class(ClassMap,<<"key">>) =:= []). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% % borrable
read_test() ->
   Buffer = helper:read("[1,3.14,{\"key\":\"value\"}]"),
   ?assert(Buffer=:=[1,3.14,{[{<<"key">>,<<"value">>}]}]).%,

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
classify_token_test() ->
  ClassMap = helper:prepare(classes),
  helper:load_classes(ClassMap, fixtureGiveMeAllClasses()),
  InmutableToken = fixtureGiveMeOneStringToken(),
  InmutableClass = fixtureGiveMeStringClass(),
  AsymmetricToken = fixtureGiveMeCloneToken(),
  AsymmetricClass = fixtureGiveMeCloneClass(),
  SymmetricToken = fixtureGiveMeAssignmentToken(),
  SymmetricClass =fixtureGiveMeAssignmentClass(),
  [
     ?assert({<<"string">>,<<"inmutable">>,InmutableToken,InmutableClass} =:= helper:classify_token(ClassMap,InmutableToken)),
     ?assert({<<"clone">>,<<"asymmetric">>,AsymmetricToken,AsymmetricClass} =:= helper:classify_token(ClassMap,AsymmetricToken)),
     ?assert({<<"assignment">>,<<"symmetric">>,SymmetricToken,SymmetricClass} =:= helper:classify_token(ClassMap,SymmetricToken))
  ].

classify_token_notoken_test() ->
  ClassMap = helper:prepare(classes),
  helper:load_classes(ClassMap, fixtureGiveMeAllClasses()),
  ?_assertException(error, _,helper:classify_token(ClassMap,[])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mutate_token_one_string_test() ->
    Token = fixtureGiveMeOneStringToken(),
    Class = fixtureGiveMeStringClass(),
    ?assert({<<"<?php ">>,[]} =:= helper:mutate_token({<<"string">>,<<"inmutable">>,Token,Class})).
 
mutate_token_another_inmutable_test() ->
    Token = fixtureGiveMeAnotherStringToken(),
    Class = fixtureGiveMeStringClass(),
    ?assert({<<"$a">>,[]} =:= helper:mutate_token({<<"string">>,<<"inmutable">>,Token,Class})).

mutate_token_one_asymmetric_test() ->
    Token = fixtureGiveMeCloneToken(),
    Class = fixtureGiveMeCloneClass(),
    ?assert({<<"clone">>,[<<"=">>]} =:= helper:mutate_token({<<"clone">>,<<"asymmetric">>,Token,Class})).
    
mutate_token_another_asymmetric_test() ->
    Token = fixtureGiveMeExitToken(),
    Class = fixtureGiveMeFlowClass(),
    ?assert({<<"exit">>,[<<"">>]} =:= helper:mutate_token({<<"flow">>,<<"asymmetric">>,Token,Class})).

mutate_token_one_symmetric_test() ->
    Token = fixtureGiveMeAssignmentToken(),
    Class = fixtureGiveMeAssignmentClass(),
    Expected = {<<"=">>,[<<"&=">>,<<".=">>,<<"/=">>,<<"-=">>,<<"%=">>,<<"*=">>,<<"|=">>,<<"+=">>]},
    ?assert(Expected =:= helper:mutate_token({<<"assignment">>,<<"symmetric">>,Token,Class})).
    
mutate_token_bad_string_type_test() ->
    Token = fixtureGiveMeOneStringToken(),
    Class = fixtureGiveMeStringClass(),
    ?_assertException(error, _,helper:mutate_token({<<"string">>,<<"symmetric">>,Token,Class})).
   
mutate_token_bad_clone_type_test() ->
    Token = fixtureGiveMeCloneToken(),
    Class = fixtureGiveMeCloneClass(),
    ?_assertException(error, _,helper:mutate_token({<<"clone">>,<<"asymmetric">>,Token,Class})).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_gen_asymmetric_test() ->
    Genes = fixtureGiveMeCloneGenes(),
    Gen = helper:find_gen(<<"clone">>,Genes, <<"asymmetric">>),
    Expected = [<<"=">>],
    ?assert(Expected =:= Gen).

find_another_gen_asymmetric_test() ->
    Genes = fixtureGiveMeFlowGenes(),
    Gen = helper:find_gen(<<"exit">>,Genes, <<"asymmetric">>),
    Expected = [<<"">>],
    ?assert(Expected =:= Gen).
    
find_gen_symmetric_test() ->
    Genes = fixtureGiveMeAssignmentGenes(),
    Gen = helper:find_gen(<<"=">>,[{[{<<"gene">>,<<"=">>},{<<"genePool">>,Genes}]}], <<"symmetric">>),
    Expected = [<<"&=">>,<<".=">>,<<"/=">>,<<"-=">>,<<"%=">>,<<"*=">>,<<"|=">>,<<"+=">>],
    ?assert(Expected =:= Gen).
    
find_gen_another_symmetric_test() ->
    Genes = fixtureGiveMeAssignmentGenes(),
    Gen = helper:find_gen(<<"&=">>,[{[{<<"gene">>,<<"&=">>},{<<"genePool">>,Genes}]}], <<"symmetric">>),
    Expected = [<<".=">>,<<"/=">>,<<"-=">>,<<"=">>,<<"%=">>,<<"*=">>,<<"|=">>,<<"+=">>],
    ?assert(Expected =:= Gen).
    
find_gen_symmetric_another_class_test() ->
    Genes = fixtureGiveMeAccessControlGenes(),
    Gen = helper:find_gen(<<"private">>,[{[{<<"gene">>,<<"private">>},{<<"genePool">>,Genes}]}], <<"symmetric">>),
    Expected = [<<"public">>,<<"protected">>],
    ?assert(Expected =:= Gen).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_file_fail_test_() ->
  ?_assertException(error, _,helper:read_file("test/void.json")).

read_file_test_() ->
  ?_assert("[{\"key1\":\"value1\"},{\"key2\":\"value2\"}]" =:= helper:read_file("test/simple.json")).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                 GEN FIXTURES 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fixtureGiveMeAccessControlGenes() ->
{[
  {<<"name">>,<<"accessControl">>},
  {<<"type">>,<<"symmetric">>},
  {<<"genePool">>,Genes}
 ]} = fixtureGiveMeAccessControlClass(),
   Genes.

fixtureGiveMeAssignmentGenes() ->
{[
  {<<"name">>,<<"assignment">>},
  {<<"type">>,<<"symmetric">>},
  {<<"genePool">>,Genes}
 ]} = fixtureGiveMeAssignmentClass(),
   Genes.

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
    {[{<<"gene">>,<<"exit">>},
      {<<"genePool">>,[<<"">>]}]},
    {[{<<"gene">>,<<"break">>},
      {<<"genePool">>,[<<"">>,<<"continue">>,<<"exit">>,<<"return">>]}]},
    {[{<<"gene">>,<<"return">>},
      {<<"genePool">>,[<<"">>,<<"break">>,<<"continue">>,<<"exit">>]}]},
    {[{<<"gene">>,<<"continue">>},
      {<<"genePool">>,[<<"">>,<<"break">>,<<"exit">>,<<"return">>]}]}
  ]}]}.

fixtureGiveMeAccessControlClass() ->
{[{<<"name">>,<<"accessControl">>},
   {<<"type">>,<<"symmetric">>},
   {<<"genePool">>,
    [<<"public">>,<<"private">>,<<"protected">>]}]}.

fixtureGiveMeStringClass() ->
{[{<<"name">>,<<"string">>},
   {<<"type">>,<<"inmutable">>},
   {<<"genes">>,[]}
   ]}.

fixtureGiveMeAssignmentClass() ->
{[{<<"name">>,<<"assignment">>},
   {<<"type">>,<<"symmetric">>},
   {<<"genePool">>,
    [<<"&=">>,<<".=">>,<<"/=">>,<<"-=">>,<<"=">>,<<"%=">>,<<"*=">>,
     <<"|=">>,<<"+=">>]}]}.
     
fixtureGiveMeAllClasses() ->
[{[{<<"name">>,<<"string">>},
   {<<"type">>,<<"inmutable">>},
   {<<"genes">>,[]}
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
      {[{<<"gene">>,<<"break">>},
        {<<"genePool">>,[<<"">>,<<"continue">>,<<"exit">>,<<"return">>]}]},
      {[{<<"gene">>,<<"continue">>},
        {<<"genePool">>,[<<"">>,<<"break">>,<<"exit">>,<<"return">>]}]},
      {[{<<"gene">>,<<"exit">>},
        {<<"genePool">>,[<<"">>]}]},
      {[{<<"gene">>,<<"return">>},
        {<<"genePool">>,[<<"">>,<<"break">>,<<"continue">>,<<"exit">>]}]}
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
    [<<"&=">>,<<".=">>,<<"/=">>,<<"-=">>,<<"=">>,<<"%=">>,<<"*=">>,
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

fixtureGiveMeExitToken() ->
{[{<<"class">>,<<"flow">>},
   {<<"value">>,<<"exit">>},
   {<<"info">>,1}]}.

fixtureGiveMeAssignmentToken()->   
 {[{<<"class">>,<<"assignment">>},
   {<<"value">>,<<"=">>},
   {<<"info">>,0}]}.
   
fixtureGiveMeCloneProgram() ->
[{[{<<"class">>,<<"string">>},
   {<<"value">>,<<"<?php ">>},
   {<<"info">>,1}]},
 {[{<<"class">>,<<"string">>},
   {<<"value">>,<<"$a">>},
   {<<"info">>,1}]},
 {[{<<"class">>,<<"assignment">>},
   {<<"value">>,<<"=">>},
   {<<"info">>,0}]},
 {[{<<"class">>,<<"clone">>},
   {<<"value">>,<<"clone">>},
   {<<"info">>,1}]},
 {[{<<"class">>,<<"string">>},
   {<<"value">>,<<"(">>},
   {<<"info">>,0}]},
 {[{<<"class">>,<<"string">>},
   {<<"value">>,<<"$b">>},
   {<<"info">>,1}]},
 {[{<<"class">>,<<"string">>},
   {<<"value">>,<<")">>},
   {<<"info">>,0}]},
 {[{<<"class">>,<<"string">>},
   {<<"value">>,<<";">>},
   {<<"info">>,0}]}].

   
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
