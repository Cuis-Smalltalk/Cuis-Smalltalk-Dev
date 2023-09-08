'From Cuis 6.0 [latest update: #5995] on 5 September 2023 at 9:00:00 am'!

!ClassDescription methodsFor: 'printing' stamp: 'jmv 9/4/2023 18:58:11'!
printNameOn: aStream
	aStream nextPutAll: 'a subclass of '.
	superclass notNil
		ifTrue: [ superclass printNameOn: aStream ]
		ifFalse: [ aStream nextPutAll: 'nil' ]! !


!Class methodsFor: 'printing' stamp: 'jmv 9/4/2023 18:51:34'!
printNameOn: aStream
	name notNil
		ifTrue: [ aStream nextPutAll: name ]
		ifFalse: [ super printNameOn: aStream ]! !


!Metaclass methodsFor: 'printing' stamp: 'jmv 9/4/2023 19:05:02'!
printNameOn: aStream
	thisClass notNil
		ifTrue: [
			thisClass printNameOn: aStream.
			aStream nextPutAll: ' class']
		ifFalse: [ aStream nextPutAll: 'a Metaclass']! !


!ContextPart methodsFor: 'printing' stamp: 'jmv 9/4/2023 18:56:01'!
printOn: aStream 
	| selector class mclass |
	self method
		ifNil: [^ super printOn: aStream].
	class := self receiver class.
	mclass := self methodClass.
	selector := self selector ifNil: [ self method defaultSelector].
	class printNameOn: aStream.
	mclass == class 
		ifFalse: [
			aStream nextPut: $(.
			mclass printNameOn: aStream.
			aStream nextPut: $)].
	aStream nextPutAll: '>>'.
	aStream nextPutAll: selector.
	selector == #doesNotUnderstand: ifTrue: [
		aStream space.
		(self tempAt: 1) selector printOn: aStream.
	].
! !

