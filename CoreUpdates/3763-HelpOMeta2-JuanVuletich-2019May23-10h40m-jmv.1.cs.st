'From Cuis 5.0 of 7 November 2016 [latest update: #3763] on 23 May 2019 at 10:45:10 am'!

!CompiledMethod methodsFor: 'ometa2preload' stamp: 'jmv 5/23/2019 10:44:49'!
createMethodNode
	"Creates the parse tree that represents self"
	| aClass source |
	aClass := self methodClass.
	source := self
		getSourceFor: (self selector ifNil: [ self defaultSelector ])
		in: aClass.
	"OMeta2 (and maybe others) could do source code transformations that mean #methodNodeFor: could fail."
	^ (aClass methodNodeFor: source) ifNil: [ self decompile ]! !

