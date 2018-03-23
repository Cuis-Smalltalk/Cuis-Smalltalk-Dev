'From Cuis 5.0 of 7 November 2016 [latest update: #3277] on 22 March 2018 at 3:07:52 pm'!

!SystemDictionary methodsFor: 'class names' stamp: 'HAW 3/22/2018 15:07:34'!
forgetClass: aClass logged: aBool 
	"Delete the class, aClass, from the system.
	Note that this doesn't do everything required to dispose of a class - to do that use Class>>removeFromSystem."

	| classCategory |
	
	"I have to keep the cateogory becuase it is nil after removing the class -Hernan"
	classCategory _ aClass category.
	
	SystemOrganization removeElement: aClass name.
	aBool ifTrue: [SystemChangeNotifier uniqueInstance classRemoved: aClass fromCategory: classCategory].		
	self removeFromStartUpList: aClass.
	self removeFromShutDownList: aClass.
	self removeKey: aClass name ifAbsent: nil.
	self flushClassNameCache! !

