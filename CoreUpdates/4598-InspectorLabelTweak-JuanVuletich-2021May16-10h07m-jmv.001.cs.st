'From Cuis 5.0 [latest update: #4597] on 16 May 2021 at 10:08:16 am'!

!Inspector methodsFor: 'user interface support' stamp: 'jmv 5/14/2021 11:38:18'!
displayLabel

	| label |
	object isObject ifFalse: [^ 'Inspect: ', self objectClass name].
	label := [object printStringLimitedTo: 64]
		on: UnhandledError
		do: [:ex | ex return: self objectClass printString, ' (printing failed)'].
	"Note this is ambiguous: what happens with (Array with: Array)?"
	(label includesSubString: self objectClass name) ifTrue: [^'Inspect: ', label].
	^ 'Inspect: ', self objectClass name, ': ', label! !

