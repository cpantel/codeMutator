-module(mutator_private_test).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Source = read json source 
% [ Classes,SourceTokens ]= convert to terms(Source)
% load (Classes)
% ClassifiedTokens = classify_every_token(SourceTokens)
% MutatedTokens = mutate every token(ClassifiedTokens)
% Mutations = generate(MutatedTokens)
% save terms_to_json(Mutations)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
full_without_tokens_test()->
    ClassMap = mutator:prepare(classes),
    {ok, Source} = file:read_file("test/fixtures/classes.php.json"),
    {[{<<"classes">>,Classes},{<<"tokens">>, SourceTokens}]} =  mutator:json_to_term(Source),
    [ FirstClass| [SecondClass| _ ] ] = Classes,

    [
      ?assert([] =:= SourceTokens),
      ?assert(fixtures:giveMeCloneClass() =:= FirstClass),
      ?assert(fixtures:giveMeStringClass() =:= SecondClass)
    ].
    

helloworld_test()->
    Expected = "[[{\"value\":\"<?php \",\"info\":1},{\"value\":\"print\",\"info\":1},{\"value\":\"(\",\"info\":0},{\"value\":\"\\\"Hello, world!\\\"\",\"info\":1},{\"value\":\")\",\"info\":0},{\"value\":\";\",\"info\":0}]]",
    
    MutationsJson = mutator:mutate_source("test/fixtures/helloworld.php.json"),
    
    ?assert(Expected =:= MutationsJson).
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
classify_tokens_test() ->
  ClassMap = mutator:prepare(classes),
  mutator:load_classes(ClassMap, fixtures:giveMeAllClasses()),
  Tokens = [ fixtures:giveMeOneStringToken(),fixtures:giveMeCloneToken() ], 
  Expected = [    
    {<<"string">>,<<"inmutable">>,fixtures:giveMeOneStringToken(),fixtures:giveMeStringClass()},
    {<<"clone">>,<<"asymmetric">>,fixtures:giveMeCloneToken(),fixtures:giveMeCloneClass()}
  ],
     
  ?assert(Expected =:= mutator:classify_tokens(ClassMap,Tokens)). 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
mutate_tokens_test() ->
  ClassifiedTokens = [ 
    {<<"string">>,<<"inmutable">>,fixtures:giveMeOneStringToken(),fixtures:giveMeStringClass()},
    {<<"clone">>,<<"asymmetric">>,fixtures:giveMeCloneToken(),fixtures:giveMeCloneClass()}
  ],
  Expected = [{<<"<?php ">>,{<<"info">>,1},[]}, {<<"clone">>,{<<"info">>,1},[<<"=">>]}],
     
  ?assert(Expected =:= mutator:mutate_tokens(ClassifiedTokens)). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
classify_token_test() ->
  ClassMap = mutator:prepare(classes),
  mutator:load_classes(ClassMap, fixtures:giveMeAllClasses()),
  InmutableToken = fixtures:giveMeOneStringToken(),
  InmutableClass = fixtures:giveMeStringClass(),
  AsymmetricToken = fixtures:giveMeCloneToken(),
  AsymmetricClass = fixtures:giveMeCloneClass(),
  SymmetricToken = fixtures:giveMeAssignmentToken(),
  SymmetricClass =fixtures:giveMeAssignmentClass(),
  [
     ?assert({<<"string">>,<<"inmutable">>,InmutableToken,InmutableClass} =:= mutator:classify_token(ClassMap,InmutableToken)),
     ?assert({<<"clone">>,<<"asymmetric">>,AsymmetricToken,AsymmetricClass} =:= mutator:classify_token(ClassMap,AsymmetricToken)),
     ?assert({<<"assignment">>,<<"symmetric">>,SymmetricToken,SymmetricClass} =:= mutator:classify_token(ClassMap,SymmetricToken))
  ].

classify_token_notoken_test() ->
  ClassMap = mutator:prepare(classes),
  mutator:load_classes(ClassMap, fixtures:giveMeAllClasses()),
  ?_assertException(error, _,mutator:classify_token(ClassMap,[])).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lookup_class_test() ->
  ClassMap = mutator:prepare(classes),
  mutator:load_classes(ClassMap, fixtures:giveMeAllClasses()),
  [{<<"clone">>,ClassItem}] = mutator:lookup_class(ClassMap,<<"clone">>),
  ?assert(ClassItem =:= fixtures:giveMeCloneClass()).

lookup_class_unknown_test() ->
  ClassMap = mutator:prepare(classes),
  mutator:load_classes(ClassMap, fixtures:giveMeAllClasses()),
  ?assert([] =:= mutator:lookup_class(ClassMap,<<"xxxx">>)).
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load_one_class_test() ->
  ClassMap = mutator:prepare(classes),
  mutator:load_one_class(ClassMap,fixtures:giveMeCloneClass()),
  [{<<"clone">>,ClassItem}] = mutator:lookup_class(ClassMap,<<"clone">>),
  ?assert(fixtures:giveMeCloneClass() =:= ClassItem).

  
load_classes_test() ->
  ClassMap = mutator:prepare(classes),
  mutator:load_classes(ClassMap, fixtures:giveMeAllClasses()),
  [{<<"clone">>,ClassItem1}] = mutator:lookup_class(ClassMap,<<"clone">>),
  ?assert(fixtures:giveMeCloneClass() =:= ClassItem1),
  [{<<"accessControl">>,ClassItem2}] = mutator:lookup_class(ClassMap,<<"accessControl">>),
  ?assert(fixtures:giveMeAccessControlClass() =:= ClassItem2).

  
load_classes_empty_test() ->
  ClassMap = mutator:prepare(classes),
  mutator:load_classes(ClassMap,[]),
  ?assert(mutator:lookup_class(ClassMap,<<"key">>) =:= []). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% % borrable
% json_to_term_test() ->
%    Buffer = mutator:json_to_term("[1,3.14,{\"key\":\"value\"}]"),
%    ?assert(Buffer=:=[1,3.14,{[{<<"key">>,<<"value">>}]}]).%,

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_the_rest_last_test() ->
    ?assert([] =:= mutator:generate_the_rest([])).

generate_the_rest_one_token_test() ->
    ?assert([{<<"inmutable1">>,{<<"info">>,1}}] =:= mutator:generate_the_rest([{<<"inmutable1">>,{<<"info">>,1},[]}])).

generate_the_rest_two_tokens_test() ->
    ?assert([{<<"inmutable1">>,{<<"info">>,1}},{<<"inmutable2">>,{<<"info">>,1}}] =:= mutator:generate_the_rest([{<<"inmutable1">>,{<<"info">>,1},[]},{<<"inmutable2">>,{<<"info">>,1},[]}])).

generate_the_rest_three_tokens_test() ->
    Expected = [
      {<<"inmutable1">>,{<<"info">>,1}},
      {<<"inmutable2">>,{<<"info">>,1}},
      {<<"inmutable3">>,{<<"info">>,1}}
    ], 
    Tokens = [
      {<<"inmutable1">>,{<<"info">>,1},[]},
      {<<"inmutable2">>,{<<"info">>,1},[]},
      {<<"inmutable3">>,{<<"info">>,1},[]}
    ],
    ?assert(Expected=:= mutator:generate_the_rest(Tokens)).

generate_the_rest_two_tokens_and_accum_test() ->
    Expected = [
      {<<"inmutable1">>,{<<"info">>,1}},
      {<<"inmutable2">>,{<<"info">>,1}},
      {<<"inmutable3">>,{<<"info">>,1}},
      {<<"inmutable4">>,{<<"info">>,1}}
    ],
    Tokens = [
       {<<"inmutable3">>,{<<"info">>,1},[]},
       {<<"inmutable4">>,{<<"info">>,1},[]}
     ],
    Accum = [
       {<<"inmutable2">>,{<<"info">>,1}},
       {<<"inmutable1">>,{<<"info">>,1}}
    ],
    ?assert( Expected =:= mutator:generate_the_rest(Tokens,Accum)).
    
generate_the_rest_with_mutable_tokens_and_accum_test() ->
    Expected = [
      {<<"inmutable1">>,{<<"info">>,1}},
      {<<"inmutable2">>,{<<"info">>,1}},
      {<<"mutable1">>,{<<"info">>,1}},
      {<<"mutable2">>,{<<"info">>,1}}
    ],
    Tokens = [
      {<<"mutable1">>,{<<"info">>,1},[<<"$mutation1">>,<<"$mutation2">>]},
      {<<"mutable2">>,{<<"info">>,1},[<<"$mutation3">>,<<"$mutation4">>]}
    ],
    Accum = [
      {<<"inmutable2">>,{<<"info">>,1}},
      {<<"inmutable1">>,{<<"info">>,1}}
    ],
    ?assert( Expected =:= mutator:generate_the_rest(Tokens,Accum)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fix_empty_test()->
    ?assert([] =:= mutator:fix([])).
    
fix_double_empty_test()->
    ?assert([] =:= mutator:fix([[]])).
    
fix_inmutable_test()->
   [
   ?assert([[a]] =:= mutator:fix([[a]])),
   ?assert([[a],[b]] =:= mutator:fix([[a],[b]])),
   ?assert([[a],[b],[c]] =:= mutator:fix([[a],[b],[c]]))
   ].
    
fix_mutated_test()->
   [
   ?assert( [[a]        ] =:= mutator:fix( [ [ [a]         ] ] )),
   ?assert( [[a],[b]    ] =:= mutator:fix( [ [ [a],[b]     ] ] )),
   ?assert( [[a],[b],[c]] =:= mutator:fix( [ [ [a],[b],[c] ] ] ))
   ].

fix_inmutable_mutated_test() ->
   [
   ?assert([[{a}]] =:= mutator:fix([[[{a}]]])),
   ?assert([[{a}],[{b}]] =:= mutator:fix([[[{a}],[{b}]]]))
   ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
depth_test() ->
  [
    ?assert( 1 =:= mutator:depth([])),
    ?assert( 2 =:= mutator:depth([[]])),
    ?assert( 3 =:= mutator:depth([[[]]])),
    ?assert(2 =:= mutator:depth([[a]])),
    ?assert(2 =:= mutator:depth([[a],[b]])) ,
    ?assert(2 =:= mutator:depth([[a],[b],[c]])),
    ?assert(3 =:= mutator:depth([[[a]]])),
    ?assert(3 =:= mutator:depth([[[a],[b]]])) ,
    ?assert(3 =:= mutator:depth([[[a],[b],[c]]])),
    ?assert(1 =:= mutator:depth([{<<"inmutable1">>,{<<"info">>,1}},{<<"inmutable2">>,{<<"info">>,1}}]) ),
    ?assert(2 =:= mutator:depth([
      [{<<"inmutable1">>,{<<"info">>,1}},{<<"inmutable2">>,{<<"info">>,1}}]
    ] ) ),
    ?assert(2 =:= mutator:depth([
      [{<<"inmutable1">>,{<<"info">>,1}},{<<"inmutable2">>,{<<"info">>,1}}],
      [{<<"inmutable1">>,{<<"info">>,1}},{<<"inmutable2">>,{<<"info">>,1}}]
    ] ) ),
    ?assert(2 =:= mutator:depth([
      [{<<"inmutable1">>,{<<"info">>,1}},{<<"inmutable2">>,{<<"info">>,1}}],
      [{<<"inmutable1">>,{<<"info">>,1}},{<<"inmutable2">>,{<<"info">>,1}}],
      [{<<"inmutable1">>,{<<"info">>,1}},{<<"inmutable2">>,{<<"info">>,1}}]
    ] ) ),
    ?assert(3 =:= mutator:depth([[
      [{<<"inmutable1">>,{<<"info">>,1}},{<<"inmutable2">>,{<<"info">>,1}}]
    ]] ) ),
    ?assert(3 =:= mutator:depth([[
      [{<<"inmutable1">>,{<<"info">>,1}},{<<"inmutable2">>,{<<"info">>,1}}],
      [{<<"inmutable1">>,{<<"info">>,1}},{<<"inmutable2">>,{<<"info">>,1}}]
    ]] ) ),
    ?assert(3 =:= mutator:depth([[
      [{<<"inmutable1">>,{<<"info">>,1}},{<<"inmutable2">>,{<<"info">>,1}}],
      [{<<"inmutable1">>,{<<"info">>,1}},{<<"inmutable2">>,{<<"info">>,1}}],
      [{<<"inmutable1">>,{<<"info">>,1}},{<<"inmutable2">>,{<<"info">>,1}}]
    ]] ) )
   ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_2_empty_test()->
      Accum = [],
      ?assert([] =:= mutator:generate([],Accum)).

generate_2_empty_accum_one_inmutable_token_test()->
   Accum =[],
   [
    ?assert([{<<"inmutable1">>,{<<"info">>,1}}] =:= mutator:generate( [ {<<"inmutable1">>,{<<"info">>,1},[]} ],Accum ) ),
    ?assert([{<<"inmutable2">>,{<<"info">>,1}}] =:= mutator:generate([{<<"inmutable2">>,{<<"info">>,1},[]}],Accum))
   ].

generate_2_many_inmutable_token_test()->
   Accum1 =[{<<"inmutable1">>,{<<"info">>,1}}],
   Accum2 =[{<<"inmutable2">>,{<<"info">>,1}},{<<"inmutable1">>,{<<"info">>,1}}],
   [
    ?assert([{<<"inmutable1">>,{<<"info">>,1}},{<<"inmutable2">>,{<<"info">>,1}}] =:= mutator:generate([{<<"inmutable2">>,{<<"info">>,1},[]}],Accum1)),
    ?assert([{<<"inmutable1">>,{<<"info">>,1}},{<<"inmutable2">>,{<<"info">>,1}},{<<"inmutable3">>,{<<"info">>,1}}] =:= mutator:generate([{<<"inmutable3">>,{<<"info">>,1},[]}],Accum2))
   ].

generate_2_two_inmutable_token_test()->
   Accum = [],
   [
    ?assert([{<<"inmutable1">>,{<<"info">>,1}},{<<"inmutable2">>,{<<"info">>,1}}] 
       =:= mutator:generate([{<<"inmutable1">>,{<<"info">>,1},[]},{<<"inmutable2">>,{<<"info">>,1},[]}],Accum))
   ].

generate_many1_inmutable_token_test()->
  
    ?assert(
       [[{<<"inmutable1">>,{<<"info">>,1}},{<<"inmutable2">>,{<<"info">>,1}}]]
     =:= 
       mutator:generate([{<<"inmutable1">>,{<<"info">>,1},[]},{<<"inmutable2">>,{<<"info">>,1},[]}])
    ).

  
generate_many2_inmutable_token_test()->
    ?assert(
      [[{<<"inmutable1">>,{<<"info">>,1}},{<<"inmutable2">>,{<<"info">>,1}},{<<"inmutable3">>,{<<"info">>,1}}]]
    =:= 
      mutator:generate([{<<"inmutable1">>,{<<"info">>,1},[]},{<<"inmutable2">>,{<<"info">>,1},[]},{<<"inmutable3">>,{<<"info">>,1},[]}])
    ).

  
generate_empty_test()->
    ?assert([] =:= mutator:generate([])).

generate_mutable_test() ->
    ?assert( [[{<<"clone">>,{<<"info">>,1}}],[{<<"=">>,{<<"info">>,1}}]]
       =:= mutator:generate([{<<"clone">>,{<<"info">>,1},[<<"=">>]}])).

generate_mutable_mixed_test() ->
    Expected = [[{<<"clone">>,{<<"info">>,1}},{<<"inmutable1">>,{<<"info">>,1}}], [{<<"=">>,{<<"info">>,1}},  {<<"inmutable1">>,{<<"info">>,1}}]],
    Tokens = [{<<"clone">>,{<<"info">>,1},[<<"=">>]},{<<"inmutable1">>,{<<"info">>,1},[]}],
    ?assert(Expected =:= mutator:generate( Tokens )).

generate_a_test() ->
    Expected = [[{<<"clone">>,{<<"info">>,1}},{<<"inmutable1">>,{<<"info">>,1}},{<<"inmutable2">>,{<<"info">>,1}}],[{<<"=">>,{<<"info">>,1}},{<<"inmutable1">>,{<<"info">>,1}},{<<"inmutable2">>,{<<"info">>,1}}]],
    Tokens = [{<<"clone">>,{<<"info">>,1},[<<"=">>]},{<<"inmutable1">>,{<<"info">>,1},[]},{<<"inmutable2">>,{<<"info">>,1},[]}],
    ?assert(Expected =:= mutator:generate( Tokens )).

generate_b_test() ->
    Expected = [[{<<"inmutable1">>,{<<"info">>,1}},{<<"clone">>,{<<"info">>,1}},{<<"inmutable2">>,{<<"info">>,1}}],[{<<"inmutable1">>,{<<"info">>,1}},{<<"=">>,{<<"info">>,1}},{<<"inmutable2">>,{<<"info">>,1}}]],
    Tokens = [{<<"inmutable1">>,{<<"info">>,1},[]},{<<"clone">>,{<<"info">>,1},[<<"=">>]},{<<"inmutable2">>,{<<"info">>,1},[]}],
    ?assert(Expected =:= mutator:generate( Tokens )). 
generate_c_test() ->
    Expected = [
       [{<<"inmutable1">>,{<<"info">>,1}},{<<"mutable">>,{<<"info">>,1}},{<<"inmutable2">>,{<<"info">>,1}}],
       [{<<"inmutable1">>,{<<"info">>,1}},{<<"mutation1">>,{<<"info">>,1}},{<<"inmutable2">>,{<<"info">>,1}}],
       [{<<"inmutable1">>,{<<"info">>,1}},{<<"mutation2">>,{<<"info">>,1}},{<<"inmutable2">>,{<<"info">>,1}}]
    ],
    Tokens = [{<<"inmutable1">>,{<<"info">>,1},[]},{<<"mutable">>,{<<"info">>,1},[<<"mutation1">>,<<"mutation2">>]},{<<"inmutable2">>,{<<"info">>,1},[]}],
    ?assert(Expected =:= mutator:generate( Tokens )).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mutate_token_one_string_test() ->
    Token = fixtures:giveMeOneStringToken(),
    Class = fixtures:giveMeStringClass(),
    ?assert({<<"<?php ">>,{<<"info">>,1},[]} =:= mutator:mutate_token({<<"string">>,<<"inmutable">>,Token,Class})).
 
mutate_token_another_inmutable_test() ->
    Token = fixtures:giveMeAnotherStringToken(),
    Class = fixtures:giveMeStringClass(),
    ?assert({<<"$a">>,{<<"info">>,1},[]} =:= mutator:mutate_token({<<"string">>,<<"inmutable">>,Token,Class})).

mutate_token_one_asymmetric_test() ->
    Token = fixtures:giveMeCloneToken(),
    Class = fixtures:giveMeCloneClass(),
    ?assert({<<"clone">>,{<<"info">>,1},[<<"=">>]} =:= mutator:mutate_token({<<"clone">>,<<"asymmetric">>,Token,Class})).
    
mutate_token_another_asymmetric_test() ->
    Token = fixtures:giveMeExitToken(),
    Class = fixtures:giveMeFlowClass(),
    ?assert({<<"exit">>,{<<"info">>,1},[<<"">>]} =:= mutator:mutate_token({<<"flow">>,<<"asymmetric">>,Token,Class})).

mutate_token_one_symmetric_test() ->
    Token = fixtures:giveMeAssignmentToken(),
    Class = fixtures:giveMeAssignmentClass(),
    Expected = {<<"=">>,{<<"info">>,0},[<<"&=">>,<<".=">>,<<"/=">>,<<"-=">>,<<"%=">>,<<"*=">>,<<"|=">>,<<"+=">>]},
    ?assert(Expected =:= mutator:mutate_token({<<"assignment">>,<<"symmetric">>,Token,Class})).
    
mutate_token_bad_string_type_test() ->
    Token = fixtures:giveMeOneStringToken(),
    Class = fixtures:giveMeStringClass(),
    ?_assertException(error, _,mutator:mutate_token({<<"string">>,<<"symmetric">>,Token,Class})).
   
mutate_token_bad_clone_type_test() ->
    Token = fixtures:giveMeCloneToken(),
    Class = fixtures:giveMeCloneClass(),
    ?_assertException(error, _,mutator:mutate_token({<<"clone">>,<<"asymmetric">>,Token,Class})).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_gen_asymmetric_test() ->
    Genes = fixtures:giveMeCloneGenes(),
    Gen = mutator:find_gen(<<"clone">>,Genes, <<"asymmetric">>),
    Expected = [<<"=">>],
    ?assert(Expected =:= Gen).

find_another_gen_asymmetric_test() ->
    Genes = fixtures:giveMeFlowGenes(),
    Gen = mutator:find_gen(<<"exit">>,Genes, <<"asymmetric">>),
    Expected = [<<"">>],
    ?assert(Expected =:= Gen).
    
find_gen_symmetric_test() ->
    Genes = fixtures:giveMeAssignmentGenes(),
    Gen = mutator:find_gen(<<"=">>,[{[{<<"gene">>,<<"=">>},{<<"genePool">>,Genes}]}], <<"symmetric">>),
    Expected = [<<"&=">>,<<".=">>,<<"/=">>,<<"-=">>,<<"%=">>,<<"*=">>,<<"|=">>,<<"+=">>],
    ?assert(Expected =:= Gen).
    
find_gen_another_symmetric_test() ->
    Genes = fixtures:giveMeAssignmentGenes(),
    Gen = mutator:find_gen(<<"&=">>,[{[{<<"gene">>,<<"&=">>},{<<"genePool">>,Genes}]}], <<"symmetric">>),
    Expected = [<<".=">>,<<"/=">>,<<"-=">>,<<"=">>,<<"%=">>,<<"*=">>,<<"|=">>,<<"+=">>],
    ?assert(Expected =:= Gen).
    
find_gen_symmetric_another_class_test() ->
    Genes = fixtures:giveMeAccessControlGenes(),
    Gen = mutator:find_gen(<<"private">>,[{[{<<"gene">>,<<"private">>},{<<"genePool">>,Genes}]}], <<"symmetric">>),
    Expected = [<<"public">>,<<"protected">>],
    ?assert(Expected =:= Gen).
