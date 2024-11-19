'From Cuis7.1 [latest update: #6817] on 18 November 2024 at 10:03:23 am'!

!SystemDictionary methodsFor: 'startup - recover user changes' stamp: 'jmv 11/18/2024 10:02:00'!
warnAndOfferLastUserChanges
"
Smalltalk warnAndOfferLastUserChanges
"
 	| userChanges |
	PreviousUserChangesName ifNil: [
		self notify: 
			self currentChangesName asFullFileEntry pathName, ' is not clean.', String newLineString, String newLineString,
			'- Cuis may already be running (and running it twice may lead to .changes file corruption).', String newLineString,
			'		Suggestion: Exit this session and resume work on the other, already running, one.', String newLineString, String newLineString,
			'- Cuis may have crashed in a previous run.', String newLineString,
			'		However the appropriate User Changes file could not be automatically determined.', String newLineString, String newLineString,
			'- The .changes file may have been accidentally modified.', String newLineString,
			'		Suggestion: Exit this session and recover it using the `git checkout` command.',String newLineString, String newLineString,
			'Proceed with caution.' .
		^ self ].
		
	userChanges := DirectoryEntry userChangesDirectory // PreviousUserChangesName.
	self notify: 
		self currentChangesName asFullFileEntry pathName, ' is not clean.', String newLineString, String newLineString,
		'- Cuis may already be running (and running it twice may lead to .changes file corruption).', String newLineString,
		'		Suggestion: Exit this session and resume work on the other, already running, one.', String newLineString, String newLineString,
		'- Cuis may have crashed in a previous run.', String newLineString,
		'		Proceed to review and recover code from last saved User Changes file:', String newLineString,
		'		', userChanges pathName, String newLineString, String newLineString,
		'- The .changes file may have been accidentally modified.', String newLineString,
		'		Suggestion: Exit this session and recover it using the `git checkout` command.',String newLineString, String newLineString,
		'Proceed with caution.' .
	ChangeList browseCondensedChanges: userChanges.! !

