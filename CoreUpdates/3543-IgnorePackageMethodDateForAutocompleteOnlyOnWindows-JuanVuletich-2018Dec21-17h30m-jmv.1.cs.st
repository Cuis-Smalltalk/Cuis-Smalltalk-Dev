'From Cuis 5.0 of 7 November 2016 [latest update: #3542] on 21 December 2018 at 5:34:41 pm'!

!SmalltalkCompleter class methodsFor: 'class initialization' stamp: 'jmv 12/21/2018 17:33:20'!
addSelector: aSymbol method: aCompiledMethod allImplemented: implemented

	| sentValue value |
	self protected: [
		value _ (ChangeSet notInstallOrTestRun or: [Smalltalk platformName ~= 'Win32'])
			ifTrue: [
				"Expensive and not worth doing in Windows with antivirus active, when installing large packages"
				aCompiledMethod dateSortingValue ]
			ifFalse: [ 0 ].
		Selectors at: aSymbol put: (value  max: (Selectors at: aSymbol ifAbsent: [0])).
		aCompiledMethod messages do: [ :sentMsg |
			((Selectors includesKey: sentMsg) or: [ 
				implemented
					ifNotNil: [ implemented includes: sentMsg ]
					ifNil: [ Smalltalk isThereAnImplementorOf: sentMsg ]])
						ifTrue: [
							sentValue _ value max: (Selectors at: sentMsg ifAbsent: [0]).
							Selectors at: sentMsg put: sentValue ]]]! !

