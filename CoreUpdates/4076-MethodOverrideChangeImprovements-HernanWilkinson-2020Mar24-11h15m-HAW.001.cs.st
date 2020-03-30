'From Cuis 5.0 [latest update: #4070] on 24 March 2020 at 11:24:31 am'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'HAW 3/24/2020 11:20:25'!
printCategoryChunk: category on: aFileStream withStamp: changeStamp priorMethod: priorMethod 

	^self 
		printCategoryChunk: category 
		on: aFileStream 
		withStamp: changeStamp 
		priorMethod: priorMethod 
		overridesMethod: nil ! !

!ClassDescription methodsFor: 'private' stamp: 'HAW 3/24/2020 11:16:12'!
logMethodSource: aText forMethodWithNode: aCompiledMethodWithNode inCategory: category withStamp: changeStamp notifying: requestor
	| priorMethodOrNil overridenMethodOrNil |
	
	priorMethodOrNil := self compiledMethodAt: aCompiledMethodWithNode selector ifAbsent: nil.
	overridenMethodOrNil := self superclass ifNotNil: [ :aSuperclass | 
		aSuperclass lookupSelector: aCompiledMethodWithNode selector ].
	
	aCompiledMethodWithNode method 
		putSource: aText asString
		fromParseNode: aCompiledMethodWithNode node
		class: self 
		category: category 
		withStamp: changeStamp 
		inFile: 2 
		priorMethod: priorMethodOrNil 
		overridesMethod: overridenMethodOrNil.! !

!methodRemoval: CompiledMethod #putSource:fromParseNode:class:category:withStamp:inFile:priorMethod: stamp: 'HAW 3/24/2020 11:21:04'!
CompiledMethod removeSelector: #putSource:fromParseNode:class:category:withStamp:inFile:priorMethod:!
