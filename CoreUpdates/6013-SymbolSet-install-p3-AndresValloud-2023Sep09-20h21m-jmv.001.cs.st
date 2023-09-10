'From Cuis 6.0 [latest update: #6011] on 9 September 2023 at 8:35:18 pm'!

!Symbol class methodsFor: 'access' stamp: 'sqr 12/30/2021 07:14:59'!
allSymbols
	"Answer all interned symbols"

	^Array streamContents:
		[:s | Symbols do: [:each | s nextPut: each]]! !

!Symbol class methodsFor: 'class initialization' stamp: 'sqr 12/30/2021 07:13:05'!
allSymbolTablesDo: aBlock

	Symbols do: aBlock! !

!Symbol class methodsFor: 'class initialization' stamp: 'sqr 12/30/2021 07:13:16'!
allSymbolTablesDo: aBlock after: aSymbol

	Symbols do: aBlock after: aSymbol! !

!Symbol class methodsFor: 'instance creation' stamp: 'sqr 12/30/2021 07:13:25'!
lookup: aStringOrSymbol

	^Symbols lookup: aStringOrSymbol! !


!SystemDictionary methodsFor: 'shrinking' stamp: 'sqr 12/30/2021 07:44:35'!
presumedSentMessages
	| sent |
	"
	In addition to those here, if it is desired to preserve some methods from deletion, see #nominallyUnsent:
	Smalltalk presumedSentMessages
	"

	"The following should be preserved for doIts, etc"
	sent _ IdentitySet new.
	#( rehashWithoutBecome
		browseAllSelect:  lastRemoval
		vScrollBarValue: hScrollBarValue: 
		to: removeClassNamed:
		dragon: hilberts: mandala: web test3 factorial tinyBenchmarks benchFib
		newDepth: restoreAfter: zapAllMethods obsoleteClasses
		removeAllUnSentMessages abandonSources removeUnreferencedKeys
		zapOrganization condenseSources condenseChanges browseObsoleteReferences
		subclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		methodsFor:stamp: methodsFor:stamp:prior: instanceVariableNames:
		startTimerEventLoop unusedClasses allClasses
		unimplemented
		reduceCuis
		variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		variableByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		variableWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		weakSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		printSpaceAnalysis:on:) do: [ :sel |
			sent add: sel].
	"The following may be sent by perform: in dispatchOnChar..."
	Editor withAllSubclassesDo: [ :c |
		c shortcuts asSet do: [ :sel | sent add: sel ].
		c cmdShortcuts asSet do: [ :sel | sent add: sel ]].
	#(beReadOnlyBinding beReadWriteBinding) do: [ :sel |
		sent add: sel].
	AppLauncher appSelector ifNotNil: [ :sel |
		sent add: sel ].
	^ sent! !

!methodRemoval: Symbol class #compactSymbolTable stamp: 'sqr 9/9/2023 20:27:05'!
Symbol class removeSelector: #compactSymbolTable!
