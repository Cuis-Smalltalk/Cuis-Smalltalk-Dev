'From Cuis 4.1 of 12 December 2012 [latest update: #1726] on 14 June 2013 at 9:41:45.08061 pm'!

!CodePackage methodsFor: 'saving' stamp: 'jmv 6/14/2013 20:23'!
writeMethodsOf: aClass on: aStream

	| core |
	core _ self coreMethodsForClass: aClass.
	core sort: [ :a :b |
		a methodSymbol < b methodSymbol ].
	core do: [ :methodReference |
		methodReference isValid ifTrue: [
			self writeMethod: methodReference on: aStream ]]! !


!CodePackage methodsFor: 'saving' stamp: 'jmv 6/14/2013 20:24'!
write: classes methodsOn: aStream

	self
		sortedExtensionMethodsDo:  [ :methodReference |
			methodReference isValid ifTrue: [
				self writeMethod: methodReference on: aStream ]]
		displayingProgress: 'Saving extension methods...'.
	classes
		do: [ :class |
			self writeMethodsOf: class on: aStream.
			self writeMethodsOf: class class on: aStream ]
		displayingProgress: 'Saving methods...'! !

