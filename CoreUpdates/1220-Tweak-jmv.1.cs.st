'From Cuis 4.0 of 16 November 2011 [latest update: #1144] on 22 March 2012 at 11:55:58 am'!

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'jmv 3/22/2012 11:16'!
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
					oldPreamble, '.', String newLine, chunk ].
			changeSet preambleString: newPreamble.
			'Preamble added to ChangeSet ', changeSet name.
			].
	(chunk beginsWith: '"Postscript:')
		ifTrue: [
			changeSet _ ChangeSet changeSetForBaseSystem.
			newPostscript _ changeSet postscriptString
				ifNil: [ chunk ]
				ifNotNil: [ :oldPostscript |
					oldPostscript, '.', String newLine, chunk ].
			changeSet postscriptString: newPostscript.
			'Postscript added to ChangeSet ', changeSet name.
			].
							
! !

