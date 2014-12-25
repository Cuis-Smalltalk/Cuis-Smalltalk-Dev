'From Cuis 4.2 of 25 July 2013 [latest update: #2131] on 12 December 2014 at 5:43:44.584997 pm'!

!SystemChangeNotifier class methodsFor: 'class initialization' stamp: 'jmv 12/12/2014 17:22'!
initialize
	"
	SystemChangeNotifier initialize
	"
	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: Smalltalk.
	SystemChangeNotifier uniqueInstance
		when: #classRemoved send: #classRemoved:fromCategory: to: Smalltalk;
		when: #evaluated send: #evaluated:context: to: Smalltalk;
		when: #methodRemoved send: #methodRemoved:selector:inProtocol:class: to: Smalltalk! !

!methodRemoval: SystemDictionary class #startUp!
SystemDictionary class removeSelector: #startUp!
!methodRemoval: SystemDictionary #shutDown!
SystemDictionary removeSelector: #shutDown!
SystemChangeNotifier initialize!
