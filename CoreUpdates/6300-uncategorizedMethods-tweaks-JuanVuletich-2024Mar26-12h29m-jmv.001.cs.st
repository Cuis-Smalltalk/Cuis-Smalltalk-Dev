'From Cuis6.3 [latest update: #6298] on 26 March 2024 at 12:31:34 pm'!

!ClassDescription methodsFor: 'accessing method dictionary' stamp: 'jmv 3/26/2024 11:02:13'!
selectorsInCategory: aSymbol 
	"Answer a list of the method selectors of the receiver that are in category named aName"
	
	| aColl |
	aColl := Set withAll: ((aSymbol isNil or: [aSymbol == ClassOrganizer allCategory])
			ifTrue: [self organization allMethodSelectors]
			ifFalse: [self organization listAtCategoryNamed: aSymbol]).
	^ aColl asArray sort! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 3/26/2024 11:01:53'!
fileOutCategory: aSymbol on: aFileStream moveSource: moveSource toFile: fileIndex 
	"File a description of the receiver's category, aString, onto aFileStream. If 
	moveSource, is true, then set the method source pointer to the new file position.
	Note when this method is called with moveSource=true, it is condensing the
	.sources file, and should only write one preamble per method category."

	| selectors |

	aFileStream newLine.
	selectors := (aSymbol isNil or: [aSymbol == ClassOrganizer allCategory])
				ifTrue: [ self organization allMethodSelectors ]
				ifFalse: [ self organization listAtCategoryNamed: aSymbol ].

	"Overridden to preserve author stamps in sources file regardless"
	selectors do: [:sel |
		self printMethodChunk: sel 
			withPreamble: true
			on: aFileStream 
			moveSource: moveSource 
			toFile: fileIndex].
	^ self! !

!ClassDescription methodsFor: 'method dictionary' stamp: 'jmv 3/26/2024 11:01:25'!
allMethodsInCategory: aSymbol
	"Answer a list of all the method categories of the receiver and all its superclasses"

	| aColl |
	aColl := OrderedCollection new.
	self withAllSuperclasses do: [ :aClass |
		aColl addAll:
			((aSymbol isNil or: [aSymbol == ClassOrganizer allCategory])
				ifTrue:
					[aClass organization allMethodSelectors]
				ifFalse:
					[aClass organization listAtCategoryNamed: aSymbol])].
	^ aColl asSet sorted

"
Morph allMethodsInCategory: #initialization
Morph allMethodsInCategory: ClassOrganizer allCategory :: size
Morph allMethodsInCategory: nil :: size
"! !

