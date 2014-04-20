'From Cuis 4.2 of 25 July 2013 [latest update: #1977] on 19 April 2014 at 6:43:58.706749 pm'!

!SmalltalkCompleter class methodsFor: 'system change notifications' stamp: 'jmv 4/19/2014 18:38'!
classRemoved: aClass fromCategory: aCategoryName

	self protected: [
		aClass selectorsDo: [ :selector |
			(Smalltalk isThereAnImplementorOf: selector exceptFor: { aClass . aClass class }) ifFalse: [
				Selectors removeKey: selector ifAbsent: nil ]].
		aClass class selectorsDo: [ :selector |
			(Smalltalk isThereAnImplementorOf: selector exceptFor: { aClass . aClass class }) ifFalse: [
				Selectors removeKey: selector ifAbsent: nil ]]]! !


!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 4/19/2014 18:43'!
isThereAnImplementorOf: aSelector exceptFor: aCollectionOfBehaviors
	"Answer whether if there is at least some other implementor of the selector besides aBehavior."

	self allBehaviorsDo: [ :class |
		((aCollectionOfBehaviors includes: class) not and: [class includesSelector: aSelector])
			ifTrue: [^ true]].
	^ false
"
Smalltalk isThereAnImplementorOf: #contents.
Smalltalk isThereAnImplementorOf: #nobodyImplementsThis.
"! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
SmalltalkCompleter initialize!

