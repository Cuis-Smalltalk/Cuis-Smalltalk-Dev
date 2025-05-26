'From Cuis7.3 [latest update: #7236] on 24 May 2025 at 7:47:19 pm'!

!SmalltalkCompleter class methodsFor: 'class initialization' stamp: 'jmv 5/24/2025 19:46:38'!
addSelector: aSymbol method: aCompiledMethod allImplemented: implemented

	| thisMethodSortingValue sentMessages |
	thisMethodSortingValue := (ChangeSet notInstallOrTestRun or: [Smalltalk platformName ~= 'Win32'])
		ifTrue: [
			"Expensive and not worth doing in Windows with antivirus active, when installing large packages"
			aCompiledMethod dateSortingValue ]
		ifFalse: [ 0 ].
	sentMessages := aCompiledMethod messages.
	self protected: [
		thisMethodSortingValue > (Selectors at: aSymbol ifAbsent: [0]) ifTrue: 
			[ Selectors at: aSymbol put: thisMethodSortingValue ].
		sentMessages do: [ :sentMsg |
			thisMethodSortingValue > (Selectors at: sentMsg ifAbsent: [0]) ifTrue: 
				[ Selectors at: sentMsg put: thisMethodSortingValue ]]].! !

