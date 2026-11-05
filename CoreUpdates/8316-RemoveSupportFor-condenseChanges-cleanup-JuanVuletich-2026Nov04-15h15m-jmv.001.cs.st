'From Cuis7.9 [latest update: #8313] on 4 November 2026 at 3:42:19 pm'!

!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 4/Nov/2026, 6:21:07 pm (UTC)'!
presumedSentMessages
	| sent |
	"
	In addition to those here, if it is desired to preserve some methods from deletion, see #nominallyUnsent:
	Smalltalk presumedSentMessages
	"

	"The following should be preserved for doIts, etc"
	sent := IdentitySet new.
	#( rehashWithoutBecome
		browseAllSelect:  lastRemoval
		vScrollBarValue: hScrollBarValue: 
		to: removeClassNamed:
		dragon: hilberts: mandala: web test3 factorial tinyBenchmarks benchFib
		newDepth: restoreAfter: zapAllMethods obsoleteClasses
		removeAllUnSentMessages abandonSources removeUnreferencedKeys
		zapOrganization condenseSources browseObsoleteReferences
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
		c keyboardActions asSet do: [ :sel | sent add: sel ].
		c new defaultMenuSpec do: [ :spec |
			spec size >= 5 ifTrue: [
				spec fifth isSymbol ifTrue: [ sent add: spec fifth ]]]].
	#(beReadOnlyBinding beReadWriteBinding) do: [ :sel |
		sent add: sel].
	AppLauncher appSelector ifNotNil: [ :sel |
		sent add: sel ].
	^ sent! !

!methodRemoval: RemoteString #fileStream stamp: 'jmv 4/Nov/2026, 6:15:03 pm (UTC)'!
RemoteString removeSelector: #fileStream!
!methodRemoval: RemoteString #sourceFileNumber stamp: 'jmv 4/Nov/2026, 6:26:11 pm (UTC)'!
RemoteString removeSelector: #sourceFileNumber!
!methodRemoval: SystemDictionary #condenseChanges stamp: 'jmv 4/Nov/2026, 6:23:15 pm (UTC)'!
SystemDictionary removeSelector: #condenseChanges!
!methodRemoval: ClassDescription #putClassCommentToCondensedChangesFile: stamp: 'jmv 4/Nov/2026, 6:25:33 pm (UTC)'!
ClassDescription removeSelector: #putClassCommentToCondensedChangesFile:!
!methodRemoval: ClassDescription #moveChangesTo: stamp: 'jmv 4/Nov/2026, 6:25:59 pm (UTC)'!
ClassDescription removeSelector: #moveChangesTo:!
