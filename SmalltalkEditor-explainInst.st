'From Cuis 4.2 of 25 July 2013 [latest update: #2820] on 13 June 2016 at 8:18:33.069191 pm'!

!SmalltalkEditor methodsFor: 'explain' stamp: 'dhn 6/13/2016 18:21'!
explainInst: string 
	"Is string an instance variable of this class?"
	| classes cls provider |
	provider _ self codeProvider.
	(provider respondsTo: #selectedClassOrMetaClass) ifTrue: [
		cls _ provider selectedClassOrMetaClass].
	cls ifNil: [^ nil].	  "no class known"
	classes _ (Array with: cls)
				, cls allSuperclasses.
	classes _ classes detect: [:each | (each instVarNames
			detect: [:name | name = string] ifNone: nil)
			notNil] ifNone: [^nil].
	classes _ classes printString.
	^ String streamContents: [:str |
		str
			nextPutAll: '"is an instance variable of the receiver; defined in class ';
			nextPutAll: classes, '"\' withNewLines;
			nextPutAll: 'Smalltalk browseAllAccessesTo: ''';
			nextPutAll: string;
			nextPutAll: ''' from: ';
			nextPutAll: classes;
			nextPut: $.]! !
