'From Cuis 5.0 [latest update: #4176] on 24 May 2020 at 8:25:32 pm'!

!SmalltalkCompleter methodsFor: 'entries - private ' stamp: 'jmv 5/24/2020 20:25:23'!
selectedClassOrMetaClassIn: specificModel

	(specificModel is: #CodeProvider) ifTrue: [ ^ specificModel selectedClassOrMetaClass ].
	
	"I can not use #selectedClassOrMetaClass becuase it changes with the selection but when compiling to evaluate it assumes object as receiver - Hernan"
	^ (specificModel isKindOf: Inspector) ifTrue: [ specificModel objectClass ] ! !

