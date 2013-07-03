-module(fixtures).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                 GEN FIXTURES 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
giveMeAccessControlGenes() ->
{[
  {<<"name">>,<<"accessControl">>},
  {<<"type">>,<<"symmetric">>},
  {<<"genePool">>,Genes}
 ]} = giveMeAccessControlClass(),
   Genes.

giveMeAssignmentGenes() ->
{[
  {<<"name">>,<<"assignment">>},
  {<<"type">>,<<"symmetric">>},
  {<<"genePool">>,Genes}
 ]} = giveMeAssignmentClass(),
   Genes.

giveMeCloneGenes() ->
{[
  {<<"name">>,<<"clone">>},
  {<<"type">>,<<"asymmetric">>},
  {<<"genes">>,Genes}
  ]} = giveMeCloneClass(),
   Genes.

giveMeFlowGenes() ->
{[
  {<<"name">>,<<"flow">>},
  {<<"type">>,<<"asymmetric">>},
  {<<"genes">>,Genes}
 ]} = giveMeFlowClass(),
   Genes.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                 CLASS FIXTURES 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
giveMeCloneClass() ->
{[
  {<<"name">>,<<"clone">>},
  {<<"type">>,<<"asymmetric">>},
  {<<"genes">>,
    [{[{<<"gene">>,<<"clone">>}, {<<"genePool">>,[<<"=">>]}]}]
  }]}.

giveMeFlowClass() ->
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

giveMeAccessControlClass() ->
{[{<<"name">>,<<"accessControl">>},
   {<<"type">>,<<"symmetric">>},
   {<<"genePool">>,
    [<<"public">>,<<"private">>,<<"protected">>]}]}.

giveMeStringClass() ->
{[{<<"name">>,<<"string">>},
   {<<"type">>,<<"inmutable">>},
   {<<"genes">>,[]}
   ]}.

giveMeAssignmentClass() ->
{[{<<"name">>,<<"assignment">>},
   {<<"type">>,<<"symmetric">>},
   {<<"genePool">>,
    [<<"&=">>,<<".=">>,<<"/=">>,<<"-=">>,<<"=">>,<<"%=">>,<<"*=">>,
     <<"|=">>,<<"+=">>]}]}.

giveMeAllClasses() ->
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
     
giveMeOneStringToken() ->
{[{<<"class">>,<<"string">>},
   {<<"value">>,<<"<?php ">>},
   {<<"info">>,1}]}.

giveMeAnotherStringToken() ->
{[{<<"class">>,<<"string">>},
   {<<"value">>,<<"$a">>},
   {<<"info">>,1}]}.

giveMeCloneToken() ->
{[{<<"class">>,<<"clone">>},
   {<<"value">>,<<"clone">>},
   {<<"info">>,1}]}.

giveMeExitToken() ->
{[{<<"class">>,<<"flow">>},
   {<<"value">>,<<"exit">>},
   {<<"info">>,1}]}.

giveMeAssignmentToken()->   
 {[{<<"class">>,<<"assignment">>},
   {<<"value">>,<<"=">>},
   {<<"info">>,0}]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                 FULL PROGRAM FIXTURES 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
giveMeCloneProgram() ->
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

   
giveMeSimpleProgram() ->
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
   {<<"value">>,<<"inmutable1">>},
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

   