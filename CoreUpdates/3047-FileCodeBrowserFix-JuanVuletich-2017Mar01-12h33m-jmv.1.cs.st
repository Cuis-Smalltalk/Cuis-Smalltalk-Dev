'From Cuis 5.0 of 7 November 2016 [latest update: #3046] on 1 March 2017 at 12:34:07 pm'!

!CodeFile methodsFor: 'change record types' stamp: 'jmv 3/1/2017 12:31:23'!
doIt: chgRec
	"See senders of #doIt "
	| string |
	string := chgRec string.
	
	"Method classification spec"
	(string beginsWith: '(''') ifTrue: [
		^ doIts add: chgRec ].
		
	"Just for compatibility with Squeak, as Cuis always adds the #classDefinition change type marker in the files."
	('*ubclass:*instanceVariableNames:*classVariableNames:*poolDictionaries:*category:*'
		match: string) ifTrue:[^self classDefinition: string with: chgRec].

	"Just for compatibility with Squeak, as Cuis always adds the #classDefinition change type marker in the files."
	('* class*instanceVariableNames:*'
		match: string) ifTrue:[^self metaClassDefinition: string with: chgRec].

	"Just for compatibility with Squeak, as Cuis always adds the #methodRemoval: (or similar) change type marker in the files."
	('* removeSelector: *'
		match: string) ifTrue:[^self removedMethod: string with: chgRec].

	"Just for compatibility with Squeak, as Cuis always adds the #classComment change type marker in the files."
	('* comment:*'
		match: string) ifTrue:[^self msgClassComment: string with: chgRec].

	"Don't add these to a CodeFile. They will be added on save if needed."
	('* initialize'
		match: string) ifTrue:[^self]. "Initialization is done based on class>>initialize"

	('''From *'
		match: string) ifTrue:[^self possibleSystemSource: chgRec].
	doIts add: chgRec.! !

