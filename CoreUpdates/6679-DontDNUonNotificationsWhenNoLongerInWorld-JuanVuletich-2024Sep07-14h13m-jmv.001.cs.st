'From Cuis7.1 [latest update: #6677] on 7 September 2024 at 2:22:33 pm'!

!CodeWindow methodsFor: 'updating' stamp: 'jmv 9/7/2024 14:18:00'!
classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName
	"Do nothing here. Subclasses should implement if necessary"! !

!CodeWindow methodsFor: 'updating' stamp: 'jmv 9/7/2024 14:19:13'!
updateListsAndCode
	"All code windows receive this message on any code change in the system.
	Process it only once, for the benefit of installing large packages!!"

	owner ifNotNil: [
		(self hasProperty: #updateListsAndCode) ifFalse: [
			self setProperty: #updateListsAndCode toValue: true.
			self whenUIinSafeState: [
				self removeProperty: #updateListsAndCode.
				owner ifNotNil: [ self updateListsAndCodeNow ]]]].! !

!CodeWindow methodsFor: 'initialization' stamp: 'jmv 9/7/2024 14:21:56'!
model: anObject
	"Set my model and make me me a dependent of the given object.
	Note: The implementors of the sent messages must check if they are still 'alive'. The usual
	way is asking `owner ifNotNil: [ do stuff ]`.
	The reason is that the receiver (i.e. the CodeWindow) may be already closed, and
	without a model, although not yet garbage collected, when the message gets dispatched"

	super model: anObject.

	"Avoid double registration"
	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: self.
	SystemChangeNotifier uniqueInstance
		when: #classAdded send: #classAdded: to: self;
		when: #classCommented send: #updateListsAndCode to: self;
		when: #classDefinitionChanged send: #updateListsAndCode to: self;
		when: #classRecategorized send: #updateListsAndCode to: self;
		when: #classRemoved send: #updateListsAndCode to: self;
		when: #classRenamed send: #classRenamed:from:to:inCategory: to: self;
		when: #classReorganized send: #updateListsAndCode to: self;
		when: #methodAddedInProtocol send: #updateListsAndCode to: self;
		when: #methodChanged send: #updateListsAndCode to: self;
		when: #methodRemoved send: #updateListsAndCode to: self;
		when: #selectorRecategorized send: #updateListsAndCode to: self.! !


!BrowserWindow methodsFor: 'updating' stamp: 'jmv 9/7/2024 14:15:24'!
classAdded: addedClass

	owner ifNotNil: [
		model selectedSystemCategory = addedClass category 
			ifTrue: [ model changed: #classList ]].! !

!BrowserWindow methodsFor: 'updating' stamp: 'jmv 9/7/2024 14:18:31'!
classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName

	owner ifNotNil: [
		self canDiscardEdits ifTrue: [
			model classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName ]].! !

