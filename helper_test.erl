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
generate_the_rest_last_test() ->
    ?assert([] =:= helper:generate_the_rest([])).

generate_the_rest_one_token_test() ->
    ?assert([<<"$a">>] =:= helper:generate_the_rest([{<<"$a">>,[]}])).

generate_the_rest_two_tokens_test() ->
    ?assert([<<"$a">>,<<"$b">>] =:= helper:generate_the_rest([{<<"$a">>,[]},{<<"$b">>,[]}])).

generate_the_rest_three_tokens_test() ->
    ?assert([<<"$a">>,<<"$b">>,<<"$c">>] =:=
       helper:generate_the_rest([{<<"$a">>,[]},{<<"$b">>,[]},{<<"$c">>,[]}])).

generate_the_rest_two_tokens_and_accum_test() ->
    Expected = [<<"$a">>,<<"$b">>,<<"$c">>,<<"$d">>],
    Tokens = [{<<"$c">>,[]},{<<"$d">>,[]}],
    Accum = [<<"$b">>,<<"$a">>],
    ?assert( Expected =:= helper:generate_the_rest(Tokens,Accum)).
    
generate_the_rest_with_mutable_tokens_and_accum_test() ->
    Expected = [<<"$a">>,<<"$b">>,<<"$c">>,<<"$d">>],
    Tokens = [{<<"$c">>,[<<"$x">>,<<"$x">>]},{<<"$d">>,[<<"$x">>,<<"$x">>]}],
    Accum = [<<"$b">>,<<"$a">>],
    ?assert( Expected =:= helper:generate_the_rest(Tokens,Accum)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_with_accum_empty_test()->
      Accum = [],
      ?assert([] =:= helper:generate([],Accum)).

generate_with_empty_accum_one_inmutable_token_test()->
   Accum =[],
   [
    ?assert([<<"$a">>] =:= helper:generate( [ {<<"$a">>,[]} ],Accum ) ),
    ?assert([<<"$b">>] =:= helper:generate([{<<"$b">>,[]}],Accum))
   ].

generate_with_accum_many_inmutable_token_test()->
   Accum1 =[<<"$a">>],
   Accum2 =[<<"$b">>,<<"$a">>],
   [
    ?assert([<<"$a">>,<<"$b">>] =:= helper:generate([{<<"$b">>,[]}],Accum1)),
    ?assert([<<"$a">>,<<"$b">>,<<"$c">>] =:= helper:generate([{<<"$c">>,[]}],Accum2))
   ].

generate_with_accum_two_inmutable_token_test()->
   Accum = [],
   [
    ?assert([<<"$a">>,<<"$b">>] =:= helper:generate([{<<"$a">>,[]},{<<"$b">>,[]}],Accum))
   ].

generate_many_inmutable_token_test()->
   [
    ?assert([[<<"$a">>,<<"$b">>]] =:= helper:generate([{<<"$a">>,[]},{<<"$b">>,[]}])),
    ?assert([[<<"$a">>,<<"$b">>,<<"$c">>]] =:= helper:generate([{<<"$a">>,[]},{<<"$b">>,[]},{<<"$c">>,[]}]))
   ].

generate_empty_test()->
    ?assert([[]] =:= helper:generate([])).

generate_test() ->
    ?assert([[<<"clone">>],[<<"=">>]] =:= helper:generate([{<<"clone">>,[<<"=">>]}])).

generate_test2() ->
    Expected = [[<<"clone">>,<<"$a">>],[<<"=">>,<<"$a">>]],
    Tokens = [{<<"clone">>,[<<"=">>]},{<<"$a">>,[]}],
    ?assert(Expected =:= helper:generate( Tokens )).

generate_test3() ->
    Expected = [[<<"clone">>,<<"$a">>,<<"$b">>],[<<"=">>,<<"$a">>,<<"$b">>]],
    Tokens = [{<<"clone">>,[<<"=">>]},{<<"$a">>,[]},{<<"$b">>,[]}],
    ?assert(Expected =:= helper:generate( Tokens )).
    
generate_test4() ->
    Expected = [[<<"$a">>,<<"clone">>,<<"$b">>],[<<"$a">>,<<"=">>,<<"$b">>]],
    Tokens = [{<<"$a">>,[]},{<<"clone">>,[<<"=">>]},{<<"$b">>,[]}],
    ?assert(Expected =:= helper:generate( Tokens )). 
generate_test5() ->
    Expected = [
       [<<"$a">>,<<"mutable">>,<<"$b">>],
       [<<"$a">>,<<"mutation1">>,<<"$b">>],
       [<<"$a">>,<<"mutation2">>,<<"$b">>]
    ],
    Tokens = [{<<"$a">>,[]},{<<"mutable">>,[<<"mutation1">>,<<"mutation2">>]},{<<"$b">>,[]}],
    ?assert(Expected =:= helper:generate( Tokens )).
    
% generate_one_inmutable_token_test()->
%    [
%     ?assert([<<"$a">>] =:= helper:generate([{<<"$a">>,[]}])),
%     ?assert([<<"$b">>] =:= helper:generate([{<<"$b">>,[]}]))
%    ].
%     
% generate_many_inmutable_token_test()->
%    [
%     ?assert([<<"$a">>,<<"$b">>] =:= helper:generate([{<<"$a">>,[]},{<<"$b">>,[]}]))
%    ].
    
    
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                 FULL PROGRAM FIXTURES 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

   
fixtureGiveMeSimpleProgram() ->
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
