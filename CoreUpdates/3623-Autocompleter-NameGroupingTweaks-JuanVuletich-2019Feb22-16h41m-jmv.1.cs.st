'From Cuis 5.0 of 7 November 2016 [latest update: #3622] on 22 February 2019 at 4:50:06 pm'!

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'jmv 2/22/2019 16:49:15'!
computeIdentifierEntries
	"Use an aux Set to avoid duplicates, but keep the order given."
	| entriesSet lastTitle |
	
	entriesSet _ Set new.
	lastTitle _ nil.
	entries _ Array streamContents: [ :strm |
		parser namesBeginningWith: prefix do: [ :identifier :kindOfIdentifierTitle |
			(entriesSet includes: identifier) ifFalse: [
				kindOfIdentifierTitle = lastTitle ifFalse: [
					strm nextPut: kindOfIdentifierTitle.
					lastTitle _ kindOfIdentifierTitle ].
				entriesSet  add: identifier.
				strm nextPut: identifier ]]].
	entriesSet size = 1 ifTrue: [
		entries _ Array with: entriesSet anyOne ]! !


!SHParserST80 methodsFor: 'indentifier iteration' stamp: 'jmv 2/22/2019 16:41:34'!
blockArgNamesDo: aBlock
	"Iterate over block argument names valid at current blockDepth"

	| title |
	title _ '-- block arguments'.
	blockDepth to: 1 by: -1 do: [ :level |
		arguments at: level ifPresent: [ :args | args do: [ :name | aBlock value: name value: title ] ]]! !

!SHParserST80 methodsFor: 'indentifier iteration' stamp: 'jmv 2/22/2019 16:41:41'!
blockTempNamesDo: aBlock
	"Iterate over block temp names valid at current blockDepth"

	| title |
	title _ '-- block variables'.
	blockDepth to: 1 by: -1 do: [ :level |
		temporaries at: level ifPresent: [ :args | args do: [ :name | aBlock value: name value: title ] ]]! !

!SHParserST80 methodsFor: 'indentifier iteration' stamp: 'jmv 2/22/2019 16:41:48'!
classVarNamesDo: aBlock

	| title |
	title _ '-- class variables'.
	classOrMetaClass isBehavior 
		ifTrue: [
			classOrMetaClass theNonMetaClass withAllSuperclasses do: [ :c |
				c classPool keysDo: [ :name | aBlock value: name value: title ] ]]! !

!SHParserST80 methodsFor: 'indentifier iteration' stamp: 'jmv 2/22/2019 16:42:08'!
instVarNamesDo: aBlock

	| title |
	title _ '-- instance variables'.
	instanceVariables do: [ :name | aBlock value: name value: title ]! !

!SHParserST80 methodsFor: 'indentifier iteration' stamp: 'jmv 2/22/2019 16:42:17'!
methodArgNamesDo: aBlock
	"Iterate over method argument names"

	| title |
	title _ '-- method arguments'.
	^arguments at: 0  ifPresent: [ :args | args do: [ :name | aBlock value: name value: title ] ]! !

!SHParserST80 methodsFor: 'indentifier iteration' stamp: 'jmv 2/22/2019 16:42:24'!
methodTempNamesDo: aBlock
	"Iterate over method temporary names"

	| title |
	title _ '-- method variables'.
	^temporaries at: 0 ifPresent: [ :args | args do: [ :name | aBlock value: name value: title ] ]! !

!SHParserST80 methodsFor: 'indentifier iteration' stamp: 'jmv 2/22/2019 16:42:33'!
namesBeginningWith: aString do: aBlock
	"Evaluate aBlock for all available names that start with aString"

	self nonGlobalNamesDo: [ :name :kindOfIdentifierTitle |
		(name beginsWith: aString) ifTrue: [ aBlock value: name value: kindOfIdentifierTitle ]].
	self namesBeginningWith: aString do: [ :name | aBlock value: name value: '-- classes' ] in: Smalltalk classNames.
	self namesBeginningWith: aString do: [ :name | aBlock value: name value: '-- globals' ] in: Smalltalk nonClassNames! !

!SHParserST80 methodsFor: 'indentifier iteration' stamp: 'jmv 2/22/2019 16:43:00'!
nonGlobalNamesDo: aBlock
	"Evaluate aBlock over all available names, except for globals"

	self
		blockArgNamesDo: aBlock;
		blockTempNamesDo: aBlock;
		methodArgNamesDo: aBlock;
		methodTempNamesDo: aBlock;
		instVarNamesDo: aBlock;
		classVarNamesDo: aBlock;
		poolConstantNamesDo: aBlock;
		workspaceNamesDo: aBlock.
	self
		reservedNames do: [ :name |
			aBlock value: name value: '-- pseudovariables'  ]! !

!SHParserST80 methodsFor: 'indentifier iteration' stamp: 'jmv 2/22/2019 16:43:31'!
poolConstantNamesDo: aBlock

	| title |
	title _ '-- pool variables'.
	classOrMetaClass isBehavior 
		ifTrue: [
			classOrMetaClass theNonMetaClass withAllSuperclasses do: [ :c |
				c sharedPools do: [ :pool |
					pool bindingsDo: [ :assoc | aBlock value: assoc key value: title ]]]]! !

!SHParserST80 methodsFor: 'indentifier iteration' stamp: 'jmv 2/22/2019 16:43:38'!
workspaceNamesDo: aBlock

	| title |
	title _ '-- workspace variables'.
	workspace ifNotNil: [
		workspace bindingNamesDo: [ :name | aBlock value: name value: title ] ]! !

