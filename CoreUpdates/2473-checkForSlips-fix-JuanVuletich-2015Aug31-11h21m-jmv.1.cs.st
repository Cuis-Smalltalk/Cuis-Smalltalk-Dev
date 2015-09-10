'From Cuis 4.2 of 25 July 2013 [latest update: #2463] on 31 August 2015 at 12:41:24.745872 pm'!

!MethodReference class methodsFor: 'instance creation' stamp: 'jmv 8/31/2015 12:40'!
method: aCompiledMethod
	^self class: aCompiledMethod methodClass selector: aCompiledMethod selector! !


!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 8/31/2015 12:40'!
checkForSlips
	"Return a collection of method refs with possible debugging code in them."
	| slips |
	slips _ OrderedCollection new.
	self changedClasses do: [ :aClass |
		(self methodChangesAtClass: aClass name) associationsDo: [ :mAssoc |  | method |
			(#(remove addedThenRemoved movedToOtherPackage) includes: mAssoc value) ifFalse: [
				method _ aClass compiledMethodAt: mAssoc key ifAbsent: nil.
				method ifNotNil: [
					method hasReportableSlip
						ifTrue: [slips add: (MethodReference method: method)]]]]].
	^ slips! !

