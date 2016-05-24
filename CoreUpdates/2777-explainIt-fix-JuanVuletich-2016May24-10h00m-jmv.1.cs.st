'From Cuis 4.2 of 25 July 2013 [latest update: #2775] on 24 May 2016 at 10:10:57.777812 am'!

!SmalltalkEditor methodsFor: 'explain' stamp: 'len 5/24/2016 10:07'!
explainClass: symbol 
	"Is symbol a class variable or a pool variable?"
	| provider class reply classes |
	provider _ self codeProvider.
	(provider respondsTo: #selectedClassOrMetaClass)
		ifFalse: [^ nil].
	(class _ provider selectedClassOrMetaClass) ifNil: [^ nil].
	"no class is selected"
	(class isKindOf: Metaclass)
		ifTrue: [class _ class soleInstance].
	classes _ (Array with: class)
				, class allSuperclasses.
	"class variables"
	reply _ classes detect: [:each | (each classVarNames detect: [:name | symbol = name]
					ifNone: nil)
					notNil]
				ifNone: nil.
	reply ifNotNil: [
		^ '"is a class variable, defined in class ' , reply printString , '"\' withNewLines , 
		'Smalltalk browseAllCallsOn: (' , reply printString , ' classPool associationAt: #' , symbol , ').'].
	"pool variables"
	classes do: [:each | (each sharedPools
			detect: [:pool | (pool includesKey: symbol)
					and: 
						[reply _ pool.
						true]]
			ifNone: nil)
			notNil].
	reply
		ifNil: [(Undeclared includesKey: symbol)
				ifTrue: [
					^ '"is an undeclared variable.' , '"\' withNewLines , 
					'Smalltalk browseAllCallsOn: (Undeclared associationAt: #' , symbol , ').']]
		ifNotNil: 
			[classes _ WriteStream on: Array new.
			Smalltalk
				allBehaviorsDo: [:each | (each sharedPools
						detect: 
							[:pool | 
							pool == reply]
						ifNone: nil)
						notNil ifTrue: [classes nextPut: each]].
			"Perhaps not print whole list of classes if too long. (unlikely)"
			^ '"is a pool variable from the pool ' , (Smalltalk keyAtIdentityValue: reply) asString , 
			', which is used by the following classes ' , classes contents printString , '"\' withNewLines , 
			'Smalltalk browseAllCallsOn: (' , (Smalltalk keyAtIdentityValue: reply) asString , 
			' bindingOf: #' , symbol , ').'].
	^ nil! !

!SmalltalkEditor methodsFor: 'explain' stamp: 'len 5/24/2016 10:10'!
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
	^ '"is an instance variable of the receiver; defined in class ' , classes , 
		'"\' withNewLines , 'Smalltalk browseAllAccessesTo: ''' , string , ''' from: ', classes, '.'! !

