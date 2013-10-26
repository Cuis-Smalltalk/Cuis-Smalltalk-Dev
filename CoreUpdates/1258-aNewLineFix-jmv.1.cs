'From Cuis 4.0 of 3 April 2012 [latest update: #1257] on 16 April 2012 at 7:01:41 pm'!

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'jmv 4/16/2012 19:01'!
checkForPreamble: chunk
	"As we don't support preambles and postscripts in Packages, assume any preamble or postscript belongs in the BaseSystem.
	Note: In packages, replace preamble by prerequisites, and postscript by class initialize methods."

	| changeSet newPreamble newPostscript |
	(chunk beginsWith: '"Change Set:')
		ifTrue: [
			changeSet _ ChangeSet changeSetForBaseSystem.
			newPreamble _ changeSet preambleString
				ifNil: [ chunk ]
				ifNotNil: [ :oldPreamble |
					oldPreamble, '.', String newLineString, chunk ].
			changeSet preambleString: newPreamble.
			'Preamble added to ChangeSet ', changeSet name.
			].
	(chunk beginsWith: '"Postscript:')
		ifTrue: [
			changeSet _ ChangeSet changeSetForBaseSystem.
			newPostscript _ changeSet postscriptString
				ifNil: [ chunk ]
				ifNotNil: [ :oldPostscript |
					oldPostscript, '.', String newLineString, chunk ].
			changeSet postscriptString: newPostscript.
			'Postscript added to ChangeSet ', changeSet name.
			].
							
! !

